#!/usr/bin/env sh
#
# Copyright (C) 2017 Azryn Software Labs contact@azryn.org
# This software is licensed under the Azryn Software Labs Public License
# of version 1.1.0 or later. You should have received a copy of the Azryn
# Software Labs Public License along with this program. If not, please
# see https://apl.azryn.org/ for a copy.
#
##################################################
#
# This script does automate the install but you will need to adjust the
# variables in 'Setup' to suit your desired install. The installation
# process will not proceed unless 'PartitionBoot' and 'VideoCards' are
# set though some example values have been provided. If you are unsure
# how to proceed, consult the Gentoo Handbook.
#
# === WARNINGS ===
# - Your root password is set to the value of $Hostname for simplicity.
#   Change your root password with 'passwd' after the MINIMAL install.
#
# - Be sure to set $PartitionBoot to your boot target, though the
#   default value of '/dev/sda' will install to the master boot record.
#
# - By default, $AutoKernel is set to 'true' which means that the kernel
#   will be built using 'make defconfig'. If you want to run
#   'make defconfig; make menuconfig' then set $AutoKernel to 'false'.
#   You may also supply your own URL to $KernelConfig while setting
#   $AutoKernel to 'false' to use a pre-built kernel config. And example
#   kernel config is provided though commented out.
#
##################################################

### Setup ########################################

## Azryn
CPUCores="$(grep -c ^processor /proc/cpuinfo)"
Hostname="azryn"
Locale="en_US.UTF-8 UTF-8"
#PartitionBoot="/dev/sda"
Source="https://raw.githubusercontent.com/Azryn/AzrynOS/master"
SwapSize="2G"
TimeZone="America/New_York"
#VideoCards="i965 intel"
#VideoCards="amdgpu radeonsi"
#VideoCards="nouveau nvidia"
#VideoCards="virtualbox vmware"

## Kernel
AutoKernel="true"
#KernelConfig="$Source/usr/src/linux/x.x.config"

## Portage
MakeConf="$Source/etc/portage/make.conf"
PackageAcceptKeywords="$Source/etc/portage/package.accept_keywords"
PackageEnv="$Source/etc/portage/package.env"
PackageLicense="$Source/etc/portage/package.license"
PackageUse="$Source/etc/portage/package.use"

## Stage3
S3Arch="amd64"
S3Src="http://distfiles.gentoo.org/releases/$S3Arch/autobuilds"
[ -x "$(command -v curl)" ] && \
    S3Cur="$(curl -s $S3Src/latest-stage3-$S3Arch.txt|tail -1|awk '{print $1}')"
Stage="$S3Src/$S3Cur"


### Passes #######################################

BOOTSTRAP() {
    echo "Please ensure that you have performed the following: "
    echo "  - Edited the environment variables at the top of this script."
    echo "  - Partitioned and mounted your disk(s)."
    read -ep "Proceed with installation? [Y/N]: " Proceed
    if echo $Proceed | grep -iq "^n"; then exit; fi

    if [ -z $VideoCards ] || [ -z $PartitionBoot ]; then
        echo "azryn: You didn't read $0 and adjust the variables! Exiting..."
        exit
    fi

    echo "azryn: Ensuring we are in /mnt/gentoo..."
    [ ! -e /mnt/gentoo/$(basename $0) ] && \
        cp $0 /mnt/gentoo/ && \
        cd /mnt/gentoo

    echo "azryn: Setting system time via ntpd..."
    [ -x "$(command -v ntpd)" ] && ntpd -q -g

    echo "azryn: Downloading and extracting Stage3 tarball..."
    if [ ! -z $S3Cur ]; then
        wget -q $Stage
        tar -xpf stage3-* --xattrs --numeric-owner
    else
        echo "azryn: 'S3Tgt' is not set! Is cURL missing? Exiting..."
        exit
    fi

    echo "azryn: Mounting hardware devices..."
    HW="proc sys dev"
    for target in $HW; do
        if [ -e /mnt/gentoo/$target ]; then
            case $target in
                proc) mount -t proc /proc /mnt/gentoo/proc;;
                sys ) mount --rbind /sys  /mnt/gentoo/sys
                      mount --make-rslave /mnt/gentoo/sys;;
                dev ) mount --rbind /dev  /mnt/gentoo/dev
                      mount --make-rslave /mnt/gentoo/dev;;
                *) echo "azryn: $target: Improper hardware device"
                   exit
            esac
        else
            echo "azryn: $target unable to be mounted! Exiting..."
            exit
        fi
    done

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
    #mkdir -vp /mnt/gentoo/etc/portage/repos.conf
    #wget -q $Source/etc/portage/repos.conf/gentoo.conf \
    #     -O /mnt/gentoo/etc/portage/repos.conf/gentoo.conf
    cp -vL /etc/resolv.conf /mnt/gentoo/etc/

    echo "azryn: Chroot'ing into /mnt/gentoo..."
    chroot /mnt/gentoo /usr/bin/env -i \
           HOME="/root" TERM="$TERM" PS1="[chroot \u@\h \w]$" \
           PATH="/usr/local/sbin/:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/bin" \
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
           net-misc/dhcpcd \
           sys-boot/grub:2

    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "azryn: emerging intel-microcode"
        emerge -q intel-microcode
    fi

    echo "azryn: Configuring Linux kernel..."
    cd /usr/src/linux
    if [ "$AutoKernel" = "true" ] && [ -z $KernelConfig ]; then
        make defconfig
    elif [ "$AutoKernel" = "false" ]; then
        if [ ! -z $KernelConfig ]; then
            wget -q $KernelConfig -O /usr/src/linux/.config
        else
            make defconfig
        fi
        make menuconfig
    fi

    echo "azryn: Compiling Linux kernel, modules, and initramfs..."
    make -j$CPUCores && make modules_install && make install
    genkernel --install initramfs
    cd /

    echo "azryn: Add connman to OpenRC..."
    rc-update add connman default

    echo "azryn: Setting hostname..."
    echo "hostname=$Hostname" > /etc/conf.d/hostname

    echo "azryn: Installing Grub to $PartitionBoot..."
    grub-install $PartitionBoot
    grub-mkconfig -o /boot/grub/grub.cfg

    echo "azryn: Adding sudo, vim, tmux, and htop..."
    emerge -q \
           app-admin/sudo \
           app-editors/vim \
           app-misc/tmux \
           dev-vcs/git \
           sys-process/htop

    echo "azryn: Adding userland configurations..."
    CfgFiles="
      /etc/bash/bashrc
      /etc/profile
      /etc/profile.d/alias.sh
      /etc/profile.d/azryn.sh
      /etc/profile.d/env.sh
      /etc/sudoers
      /etc/tmux.conf
      /etc/vimrc
    "
    for cfg in $CfgFiles; do
        wget -q $Source/$cfg -O $cfg
    done

    echo "azryn: Setting root password..."
    [ -x $(command -v chpasswd) ] && \
        echo "root:$Hostname" | chpasswd
}

DESKTOP() {
    echo "azryn: Installing Xorg drivers..."
    emerge -q x11-base/xorg-drivers
    env-update && source /etc/profile && export PS1="[chroot \u@\h \w]$"

    echo "azryn: Installing desktop packages..."
    emerge -q \
           app-admin/eclean-kernel \
           app-laptop/laptop-mode-tools \
           app-portage/gentoolkit \
           app-text/aspell \
           media-fonts/noto \
           media-gfx/scrot \
           media-libs/alsa-lib \
           media-sound/alsa-utils \
           media-sound/pavucontrol \
           media-video/mpv \
           net-misc/youtube-dl \
           sys-apps/mlocate \
           x11-apps/xbacklight \
           x11-apps/xset \
           x11-apps/xsetroot \
           x11-misc/arandr \
           x11-misc/dmenu \
           x11-misc/i3lock \
           x11-misc/i3status \
           x11-misc/xclip \
           x11-terms/gnome-terminal \
           x11-wm/i3

    echo "azryn: Add laptop_mode to OpenRC..."
    rc-update add laptop_mode default

    echo "azryn: Adding userland configuration files..."
    [ ! -d /etc/emacs ] && mkdir -p /etc/emacs
    CfgFiles="
      /etc/Xresources
      /etc/emacs/default.el
      /etc/i3/config
      /etc/xinitrc
    "
    for cfg in $CfgFiles; do
        wget -q $Source/$cfg -O $cfg
    done
}

CLEANUP() {
    echo "azryn: Updating @world and removing unused packages..."
    emerge -quDN @world
    emerge -q --depclean
    emerge -quD --changed-use @world
    eclean --deep distfiles

    echo "azryn: Removing stage3 tarball..."
    rm -rf /stage3*
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
        echo ""
        echo "Post-install tasks:"
        echo "  cleanup      Remove junk created during install"
esac
shopt -u nocasematch
