#!/usr/bin/env sh
#
# Copyright (C) 2017 Johnathan C Maudlin <jcmdln@gmail.com>
# This software is licensed under the Azryn Software Labs Public License
# of version 1.1.0 or later. You should have received a copy of the
# Azryn Software Labs Public License along with this program. If not,
# please see https://apl.azryn.org/ for a copy.
#
# This script does automate the install but you will need to adjust the
# variables in 'Setup' to suit your desired install. The installation
# process will not proceed unless 'PartitionBoot' and 'VideoCards' are
# set though some example values have been provided. If you are unsure
# how to proceed, consult the Gentoo Handbook.
#


# This script heavily relies on downloading configuration files from the
# main repository. If you have cloned gein and made your own changes
# to the configuration files, you may change the $Source URL as needed.

Source="https://raw.githubusercontent.com/jcmdln/gein/master"


# This section defines the basic variables needed in order to complete
# the installation. Your root password is set to the value of $Hostname
# for simplicity. Change your root password with 'passwd' after the
# installation completes. Be sure to set $PartitionBoot to your intended
# Grub destination. The default value will install to the master boot
# record (MBR) of '/dev/sda'. You will also need to uncomment the line
# pertaining to your GPU and modify it as needed.

CPUCores="$(grep -c ^processor /proc/cpuinfo)"
Hostname="gein"
Locale="en_US.UTF-8 UTF-8"
#PartitionBoot="/dev/sda"
SwapSize="2G"
TimeZone="America/New_York"
#VideoCards="i965 intel"
#VideoCards="amdgpu radeonsi"
#VideoCards="nouveau nvidia"
#VideoCards="virtualbox vmware"


# By default, $AutoKernel is set to 'true' which means that the kernel
# will be built using 'make defconfig'. If you want to run
# 'make defconfig; make menuconfig' then set $AutoKernel to 'false'. You
# may also supply your own URL to $KernelConfig while setting
# $AutoKernel to 'false' to use a pre-built kernel config. An example
# kernel config is provided though commented out.

AutoKernel="true"
#KernelConfig="$Source/usr/src/linux/x.x.config"


# Much work has been done to simplify or in most cases fully automate
# interacting with Portage though this section simply creates easily
# referenced variables that may be called later in this script. At the
# time of writing, the Gentoo stage3 no longer includes 'git' which
# prevents using the GitHub mirror. Please leave this commented for now.

MakeConf="$Source/etc/portage/make.conf"
PackageAcceptKeywords="$Source/etc/portage/package.accept_keywords"
PackageEnv="$Source/etc/portage/package.env"
PackageLicense="$Source/etc/portage/package.license"
PackageUse="$Source/etc/portage/package.use"
#ReposConf="$Source/etc/portage/repos.conf/gentoo.conf"


# This section exists to automate identifying and downloading the latest
# stage3 archive under the condition that cURL is present. This is not
# an issue when using the Gentoo installation CD's though prevents
# errors when executing MINIMAL() or DESKTOP() due to cURL missing
# after completing the BOOTSTRAP().

S3Arch="amd64"
S3Src="http://distfiles.gentoo.org/releases/$S3Arch/autobuilds"
S3Txt="curl -s $S3Src/latest-stage3-$S3Arch.txt"
[ -x "$(command -v curl)" ] && \
    S3Cur="$($S3Txt|tail -1|awk '{print $1}')" && \
    Stage3="$S3Src/$S3Cur"


# Bootstrapping a Gentoo stage3 archive is a fairly quick process though
# we must also ensure this script will be accessible from the chroot as
# well as other housekeeping tasks. The Portage configuration files are
# setup in this section.

BOOTSTRAP() {
    echo "Please ensure that you have performed the following: "
    echo "  - Edited the environment variables at the top of this script."
    echo "  - Partitioned and mounted your disk(s)."
    read -ep "Proceed with installation? [Y/N]: " Proceed
    if echo $Proceed | grep -iq "^y"; then
        echo "gein: Proceeding with installation..."
    else
        echo "gein: Exiting as requested..."
        exit
    fi

    if [ -z $VideoCards ] || [ -z $PartitionBoot ]; then
        echo "gein: You didn't read $0 and adjust the variables! Exiting..."
        exit
    fi

    echo "gein: Ensuring we are in /mnt/gentoo..."
    [ ! -e /mnt/gentoo/$(basename $0) ] && \
        cp $0 /mnt/gentoo/ && \
        cd /mnt/gentoo

    echo "gein: Setting system time via ntpd..."
    [ -x "$(command -v ntpd)" ] && ntpd -q -g

    echo "gein: Downloading and extracting Stage3 tarball..."
    if [ ! -z $Stage3 ]; then
        wget -q $Stage3
        tar -xpf stage3-* --xattrs --numeric-owner
        rm -rf stage3-*
    else
        echo "gein: 'S3Tgt' is not set! Is cURL missing? Exiting..."
        exit
    fi

    echo "gein: Mounting hardware devices..."
    HW="proc sys dev"
    for target in $HW; do
        if [ -e /mnt/gentoo/$target ]; then
            case $target in
                proc) mount -t proc /proc /mnt/gentoo/proc;;
                sys ) mount --rbind /sys  /mnt/gentoo/sys
                      mount --make-rslave /mnt/gentoo/sys;;
                dev ) mount --rbind /dev  /mnt/gentoo/dev
                      mount --make-rslave /mnt/gentoo/dev;;
                *) echo "gein: $target: Improper hardware device"
                   exit
            esac
        else
            echo "gein: $target unable to be mounted! Exiting..."
            exit
        fi
    done

    echo "gein: Setting up swapfile..."
    SwapFile="/mnt/gentoo/swapfile"
    if [ ! -e $SwapFile ]; then
        fallocate -l $SwapSize $SwapFile && chmod 0600 $SwapFile
        mkswap $SwapFile && swapon $SwapFile
        echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "gein: Copying '/etc/resolv.conf'..."
    cp -L /etc/resolv.conf /mnt/gentoo/etc/

    echo "gein: Downloading portage configuration files..."
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
    [ ! -z $ReposConf ] && \
        mkdir -p /mnt/gentoo/etc/portage/repos.conf && \
        wget -q $ReposConf \
             -O /mnt/gentoo/etc/portage/repos.conf/gentoo.conf

    echo "gein: Chroot'ing into /mnt/gentoo..."
    chroot /mnt/gentoo /usr/bin/env -i \
           HOME="/root" TERM="$TERM" PS1="[chroot \u@\h \w]$" \
           PATH="/usr/local/sbin/:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/bin" \
           /bin/bash --login
}


# In this section we will update various configuration files, select the
# desired profile, compile the kernel, and install some basic packages.

MINIMAL() {
    echo "gein: Setting CPU cores and GPU type..."
    sed -i "s/Video_Cards/$VideoCards/g; s/Make_Opts/-j$CPUCores/g" \
        /etc/portage/make.conf

    echo "gein: Syncing Portage and selecting profile..."
    emerge -q --sync
    eselect profile list
    echo "gein: Hint: choose the latest 'default/linux/amd64/xx.x'"
    read -ep "Which profile?: " TargetProfile
    [ -z $TargetProfile ] && TargetProfile="1"
    eselect profile set $TargetProfile
    emerge -vuDN --quiet-build @world

    echo "gein: Setting timezone..."
    echo $TimeZone > /etc/timezone
    emerge -v --quiet-build --config sys-libs/timezone-data

    echo "gein: Setting locale..."
    echo $Locale > /etc/locale.gen
    locale-gen && locale -a && eselect locale list
    read -ep "Target locale: " TargetLocale
    eselect locale set $TargetLocale
    env-update && source /etc/profile && export PS1="[chroot \u@\h \w]$"

    echo "gein: Emerging base packages..."
    emerge \
        -v --quiet-build \
        app-admin/sudo \
        app-editors/vim \
        app-misc/tmux \
        dev-util/bcc \
        dev-vcs/git \
        net-misc/connman \
        net-misc/dhcpcd \
        sys-apps/pciutils \
        sys-boot/grub:2 \
        sys-kernel/gentoo-sources \
        sys-kernel/linux-firmware \
        sys-process/htop \
        virtual/cron

    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "gein: emerging intel-microcode"
        emerge -v --quiet-build intel-microcode
    fi

    echo "gein: Configuring Linux kernel..."
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

    echo "gein: Compiling Linux kernel, modules, and initramfs..."
    make -j$CPUCores && make modules_install && make install
    cd /

    echo "gein: Adding services to OpenRC..."
    rc-update add dhcpcd default
    rc-update add connman default
    rc-update add cronie default

    echo "gein: Setting hostname..."
    echo "hostname=$Hostname" > /etc/conf.d/hostname

    echo "gein: Installing Grub to $PartitionBoot..."
    grub-install $PartitionBoot
    grub-mkconfig -o /boot/grub/grub.cfg

    echo "gein: Adding userland configurations..."
    CfgFiles="
      /etc/bash/bashrc
      /etc/profile
      /etc/profile.d/alias.sh
      /etc/profile.d/gein.sh
      /etc/profile.d/env.sh
      /etc/sudoers
      /etc/tmux.conf
      /etc/vimrc
    "
    for cfg in $CfgFiles; do
        wget -q $Source/$cfg -O $cfg
    done

    echo "gein: Setting root password..."
    [ -x $(command -v chpasswd) ] && \
        echo root:$Hostname | chpasswd
}


# In this section we will install the chosen xorg-driver and packages
# for i3wm as the desktop of choice. Some additional packages are added
# as a convenience.

DESKTOP() {
    echo "gein: Installing Xorg drivers..."
    emerge -v --quiet-build x11-base/xorg-drivers
    env-update && source /etc/profile && export PS1="[chroot \u@\h \w]$"

    echo "gein: Installing desktop packages..."
    emerge \
        -v --quiet-build \
        app-editors/emacs \
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

    echo "gein: Add laptop_mode to OpenRC..."
    rc-update add laptop_mode default

    echo "gein: Adding userland configuration files..."
    CfgFiles="
      /etc/Xresources
      /etc/emacs/default.el
      /etc/i3/config
      /etc/i3status.conf
      /etc/xinitrc
    "
    [ ! -d /etc/emacs ] && mkdir -p /etc/emacs
    for cfg in $CfgFiles; do
        wget -q $Source/$cfg -O $cfg
    done
}

# This section is for completing tasks after the installation is
# complete. The user will have a complete system already installed and
# may skip these steps if desired.
POSTINSTALL() {
    read -ep "gein: Setup a standard user? [Y/N]: " SetupUser
    if echo $SetupUser | grep -iq "^y"; then
        read -ep "Username: " Username
        read -ep "Password: " Password
        useradd -m -G wheel,audio,video -s /bin/bash $Username
        echo $Username:$Password | chpasswd
    fi
}

# This is the CLI that controls what will be run. Keep in mind that the
# BOOTSTRAP() needs to be run to completion before running the MINIMAL()
# or DESKTOP() passes.

shopt -s nocasematch
case $1 in
    -b|bootstrap)
        BOOTSTRAP
        ;;

    -m|minimal)
        MINIMAL
        POSTINSTALL
        ;;

    -d|desktop)
        MINIMAL
        DESKTOP
        POSTINSTALL
        ;;

    *)
        echo "gein: Linux-based derivative of Gentoo"
        echo "  -h help         Shows this output"
        echo "  -b bootstrap    Bootstrap the stage3 tarball"
        echo ""
        echo "Post-bootstrap:"
        echo "  -m minimal      Perform a basic Gentoo installation"
        echo "  -d desktop      Install a complete gein desktop"
esac
shopt -u nocasematch
