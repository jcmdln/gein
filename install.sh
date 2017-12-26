#!/usr/bin/env sh
#
# Copyright (C) 2017 Azryn Software Labs contact@azryn.org
# This software is licensed under the Azryn Software Labs Public License
# of version 1.1.0 or later. You should have received a copy of the Azryn
# Software Labs Public License along with this program. If not, please
# see https://apl.azryn.org/ for a copy.
#

## Azryn
BaseUrl="https://raw.githubusercontent.com/Azryn/AzrynOS/master"
CPUCores="$(grep -c ^processor /proc/cpuinfo)"
GrubTarget="/dev/sda"
Hostname="azryn"
Locale="en_US.UTF-8 UTF-8"
RootPwd="azryn"
SwapSize="2G"
TimeZone="America/New_York"
VideoCards="intel"

## Kernel
KernelVersion="4.14"
KernelConfig="$Base_Url/usr/src/linux/$KernelVersion.config"

## Portage
MakeConf="$BaseUrl/etc/portage/make.conf"
PackageAcceptKeywords="$BaseUrl/etc/portage/package.accept_keywords"
PackageEnv="$BaseUrl/etc/portage/package.env"
PackageLicense="$BaseUrl/etc/portage/package.license"
PackageUse="$BaseUrl/etc/portage/package.use"

## Stage3
S3Arch="amd64"
S3Url="http://distfiles.gentoo.org/releases/$S3Arch/autobuilds"
[ -x "$(command -v curl)" ] && \
S3Tgt="$(curl -s $S3Url/latest-stage3-$S3Arch.txt|tail -1|awk '{print $1}')"


### Passes #######################################

BOOTSTRAP() {
    echo "Please ensure that you have performed the following: "
    echo "  - Edited the environment variables at the top of this script."
    echo "  - Partitioned and mounted your disk(s)."
    read -ep "Proceed with installation? [Y/N]: " Proceed
    if echo $Proceed | grep -iq "^n"; then exit; fi

    ## If this script isn't in /mnt/gentoo/azryn, cp it now
    [ ! -e /mnt/gentoo/azryn ] && cp $0 /mnt/gentoo/

    echo "azryn: Ensure we are in /mnt/gentoo..."
    cd /mnt/gentoo

    echo "azryn: Setting system time via ntpd..."
    ntpd -q -g

    echo "azryn: Downloading and extracting Stage3 tarball..."
    wget -q $S3Url/$S3Tgt
    tar -xjpf stage3-*.tar.bz2 --xattrs --numeric-owner

    echo "azryn: Mounting hardware devices..."
    mount -t proc /proc /mnt/gentoo/proc
    mount --rbind /sys  /mnt/gentoo/sys
    mount --make-rslave /mnt/gentoo/sys
    mount --rbind /dev  /mnt/gentoo/dev
    mount --make-rslave /mnt/gentoo/dev

    echo "azryn: Setting up swapfile..."
    SwapFile="/mnt/gentoo/swapfile"
    if [ ! -e $SwapFile ]; then
        fallocate -l $SwapSize $SwapFile && chmod 0600 $SwapFile
        mkswap $SwapFile && swapon $SwapFile
        echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "azryn: Downloading portage configuration files..."
    [ ! -z $MakeConf ] && \
        wget -q $MakeConf \
             -O /mnt/gentoo/etc/portage/make.conf
    [ ! -z $PackageAcceptKeywords ] && \
        wget -q $PackageAcceptKeywords \
             -O /mnt/gentoo/etc/portage/package.accept_keywords
    [ ! -z $PackageEnv ] && \
        wget -q $PackageEnv \
             -O /mnt/gentoo/etc/portage/package.env
    [ ! -z $PackageLicense ] && \
        wget -q $PackageLicense \
             -O /mnt/gentoo/etc/portage/package.license
    [ ! -z $PackageUse ] && \
        rm -rf /mnt/gentoo/etc/portage/package.use && \
        wget -q $PackageUse \
             -O /mnt/gentoo/etc/portage/package.use

    echo "azryn: Setting up Portage mirrors..."
    mkdir -vp /mnt/gentoo/etc/portage/repos.conf
    wget -q $BaseUrl/etc/portage/repos.conf/gentoo.conf \
         -O /etc/portage/repos.conf/gentoo.conf
    cp -vL /etc/resolv.conf /mnt/gentoo/etc/

    echo "azryn: Chroot'ing into /mnt/gentoo..."
    chroot /mnt/gentoo /usr/bin/env -i \
           HOME="/root" TERM="$TERM" PS1="[chroot \u@\h \w]$" \
           PATH="/bin:/sbin:/usr/bin:/usr/sbin" \
           /bin/bash --login
}

MINIMAL() {
    echo "azryn: Setting CPU cores and GPU type..."
    sed -i "s/Video_Cards/$VideoCards/g; s/Make_Opts/-j$CPUCores/g" \
        /etc/portage/make.conf

    echo "azryn: Syncing Portage and selecting profile..."
    emerge -q --sync
    eselect profile list
    echo "azryn: Hint: choose the latest 'default/linux/amd64/xx.x'"
    read -ep "Which profile? : " TargetProfile
    [ -z $TargetProfile ] && TargetProfile="1"
    eselect profile set $TargetProfile
    emerge -quDN @world

    echo "azryn: Setting timezone..."
    echo $TimeZone > /etc/timezone
    emerge -q --config sys-libs/timezone-data

    echo "azryn: Setting locale..."
    echo $Locale > /etc/locale.gen
    locale-gen && locale -a && eselect locale list
    read -ep "Target locale: " TargetLocale
    eselect locale set $TargetLocale
    env-update && source /etc/profile && export PS1="[chroot \u@\h \w]$"

    echo "azryn: Emerge/install Linux kernel and modules..."
    emerge -q \
           sys-kernel/gentoo-sources \
           sys-kernel/linux-firmware \
           sys-apps/pciutils \
           sys-kernel/genkernel \
           net-misc/connman \
           sys-boot/grub:2

    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "azryn: emerging intel-microcode"
        emerge -q intel-microcode
    fi

    echo "azryn: Configuring Linux kernel..."
    cd /usr/src/linux
    if [ -z $KernelConfig ]; then
        make defconfig && make menuconfig
    else
        wget $KernelConfig -O /usr/src/linux/.config
    fi

    echo "azryn: Compiling Linux kernel, modules, and initramfs..."
    make -j$CPUCores && make modules_install && make install
    genkernel --install initramfs
    cd /

    echo "azryn: Install netifrc..."
    emerge -q --noreplace net-misc/netifrc

    echo "azryn: Add connman to OpenRC..."
    rc-update add connman default

    echo "azryn: Setting hostname..."
    echo "hostname=$Hostname" > /etc/conf.d/hostname

    echo "azryn: Installing Grub..."
    grub-install $GrubTarget
    grub-mkconfig -o /boot/grub/grub.cfg

    echo "azryn: Adding bash configuration..."
    wget -q $BaseUrl/etc/bash/bashrc -O /etc/bash/bashrc

    echo "azryn: Adding profile configuration..."
    wget -q $BaseUrl/etc/profile -O /etc/profile
    wget -q $BaseUrl/etc/profile.d/alias.sh -O /etc/profile.d/alias.sh
    wget -q $BaseUrl/etc/profile.d/azryn.sh -O /etc/profile.d/azryn.sh
    wget -q $BaseUrl/etc/profile.d/environment.sh \
         -O /etc/profile.d/environment.sh

    echo "azryn: Adding sudo, vim, tmux, and htop..."
    emerge -q \
           app-admin/sudo \
           app-editors/vim \
           app-misc/tmux \
           dev-vcs/git \
           sys-process/htop

    echo "azryn: Adding userland configurations..."
    wget -q $BaseUrl/etc/sudoers   -O /etc/sudoers
    wget -q $BaseUrl/etc/tmux.conf -O /etc/tmux.conf
    wget -q $BaseUrl/etc/vimrc     -O /etc/vimrc

    echo "azryn: Setting root password..."
    echo "root:$RootPwd" | chpasswd
}

DESKTOP() {
    echo "azryn: Installing Xorg drivers..."
    emerge -q x11-base/xorg-drivers
    env-update && source /etc/profile && export PS1="[chroot \u@\h \w]$"

    echo "azryn: Installing base desktop packages..."
    emerge -q \
           app-admin/eclean-kernel \
           app-editors/emacs \
           app-laptop/laptop-mode-tools \
           app-portage/gentoolkit \
           app-text/aspell \
           kde-frameworks/breeze-icons \
           kde-plasma/breeze-gtk \
           media-fonts/noto \
           media-libs/alsa-lib \
           media-sound/alsa-utils \
           media-video/mpv \
           net-misc/youtube-dl \
           sys-apps/mlocate \
           x11-apps/xbacklight \
           x11-apps/xrandr \
           x11-apps/xset \
           x11-misc/wmctrl \
           x11-misc/xclip \
           x11-misc/xdotool

    echo "azryn: Add laptop_mode to OpenRC..."
    rc-update add laptop_mode default

    echo "azryn: Adding userland configuration files..."
    wget -q $BaseUrl/etc/Xresources       -O /etc/Xresources
    wget -q $BaseUrl/etc/emacs/default.el -O /etc/emacs/default.el
    wget -q $BaseUrl/etc/i3/config        -O /etc/i3/config
    wget -q $BaseUrl/etc/xinitrc          -O /etc/xinitrc
}

I3WM() {
    echo "azryn: Installing i3wm desktop..."
    emerge -q \
           lxde-base/lxappearance \
           x11-misc/dmenu \
           x11-misc/i3lock \
           x11-misc/i3status \
           x11-terms/gnome-terminal \
           x11-wm/i3
}

LXQT() {
    echo "azryn: Installing LXQT desktop..."
    emerge -q \
           kde-plasma/breeze \
           kde-plasma/breeze-grub \
           kde-plasma/kwin \
           kde-plasma/sddm-kcm \
           lxqt-base/lxqt-meta \
           x11-terms/qterminal

    echo "azryn: Set SDDM as the display manager"
    sed -i 's/DISPLAYMANAGER="xdm"/DISPLAYMANAGER="sddm"/g' \
        /etc/conf.d/xdm
    sed -i 's/startlxqt/"ck-launch-session dbus-launch startlxqt"/g' \
        /usr/share/xsessions/lxqt.desktop
    rc-update add xdm default
    rc-update add dbus default
}

CLEANUP() {
    echo "azryn: Updating @world and removing unused packages..."
    emerge -quDN @world
    emerge -q --depclean
    emerge -quD --changed-use @world
    eclean --deep distfiles
    eclean --deep packages

    echo "azryn: Removing stage3 tarball..."
    rm -rf /stage3*.tar.bz2
}


### Execution ####################################

shopt -s nocasematch
case $1 in
    bootstrap)
        BOOTSTRAP
        ;;

    minimal)
        MINIMAL
        ;;

    i3wm)
        MINIMAL
        DESKTOP
        I3WM
        ;;

    lxqt)
        MINIMAL
        DESKTOP
        LXQT
        ;;

    cleanup)
        CLEANUP
        ;;

    *)
        echo "AzrynOS: Linux-based derivative of Gentoo"
        echo "  help         Shows help output"
        echo ""
        echo "Pre-install tasks:"
        echo "  bootstrap    Bootstrap the stage3 tarball"
        echo ""
        echo "Installation options:"
        echo "  minimal      Install minimal Gentoo"
        echo "  i3wm         Install Gentoo and i3wm desktop"
        echo "  lxqt         Install Gentoo and LXQT desktop"
        echo ""
        echo "Post-install tasks:"
        echo "  cleanup      Remove junk created during install"
esac
shopt -u nocasematch
