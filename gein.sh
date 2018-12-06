#!/usr/bin/env sh
#
#
#
##


# This section describes variables that will define the resulting
# system. '$Hostname', '$Locale', and '$TimeZone' are mostly obvious in
# terms of their use, though '$VideoCards' is used as the value of
# 'VIDEO_CARDS' within '/etc/portage/make.conf'.

cpu_cores="$(grep -c ^processor /proc/cpuinfo)"
hostname="gein"
locale="en_US.UTF-8 UTF-8"
timezone="America/New_York"
video_cards="false"


# This section contains aliases for commands which are used throughout
# the script. By default commands have their output and prompts
# suppressed, though should you want to review or debug this script you
# may want to adjust these as desired.

curl="curl -sSf"
emerge="emerge -v --quiet-build"
emerge_sync="emerge -q --sync"
make="make -s -j$cpu_cores"
mkdir="mkdir -p"
rm="rm"
wget="wget -q"


# This section describes two variables: '$AutoKernel'
#

kernel_autobuild="true"
#kernel_config=""


#
#

#partition_boot="/dev/sda1"
#partition_uefi="/dev/sda2"
#partition_root="/dev/sda3"
#partition_home="/dev/sda4"
#partition_swap="/dev/sda5"


#
#

case "$(uname -m)" in
    i486|i586)
        cpu_dir="x86"
        cpu_arch="i486";;
    i686|x86|x86_32)
        cpu_dir="x86"
        cpu_arch="i686";;
    amd64|x86_64)
        cpu_dir="amd64"
        cpu_arch="amd64";;

    *)
        echo "gein: error: your architecture has not been defined yet" \
             "Submit an issue with the output of 'uname -m'" | fold -s
        echo "gein: Exiting..." \
            | fold -s
        exit 1
esac


#
#

config_source="https://gein.io"

CONFIG() {
    config_files="
        /etc/portage/make.conf

        /etc/portage/package.accept_keywords/development
        /etc/portage/package.accept_keywords/lxqt
        /etc/portage/package.accept_keywords/media
        /etc/portage/package.accept_keywords/system

        /etc/portage/package.env
        /etc/portage/package.license

        /etc/portage/package.use/global
        /etc/portage/package.use/local
        /etc/portage/package.use/multilib

        /etc/portage/sets/gein-base
        /etc/portage/sets/gein-i3wm
        /etc/portage/sets/gein-lxqt
        /etc/portage/sets/gein-laptop

        /usr/local/sbin/gpkg
        /usr/local/sbin/kbuild
     "

    config_dirs="
        /etc/portage/package.accept_keywords
        /etc/portage/package.use
        /etc/portage/sets
     "

    for dir in $config_dirs; do
        if [ ! -d "$dir" ]; then
            $rm "$dir"
        fi

        if [ ! -e "$dir" ]; then
            $mkdir "$dir"
        fi
    done

    unset config_dirs dir

    for file in $config_files; do
        $wget "$config_source/$file" -O "$file"
    done

    unset config_files file

    config_complete="true"
}


#
#

PREREQUISITES() {
    if [ -z "$partition_boot" ] || [ -z "$video_cards" ]; then
        echo "gein: error: required variables are unset!" | fold -s
        echo "gein: please ensure you have partitioned and mounted" \
             "your disks, as well as updated the variables associated" \
             "with the required partitions. You must also declare" \
             "your VideoCard. Please see gein.sh for instructions." \
            | fold -s
        echo "gein: Exiting..." | fold -s
        exit 1
    fi

    prerequisites_complete="true"
}


#
#

PARTITION() {
    if [ ! -e /mnt/gentoo ]; then
        echo "gein: error: '/mnt/gentoo' does not exist!" | fold -s
        echo "gein: '/mnt/gentoo' is referred to later in this script," \
             "and is required to continue. Please ensure your mounted" \
             "partitions are correct." | fold -s
        echo "gein: Exiting..." | fold -s
        exit 1
    fi

    partition_complete="true"
}


#
#

BOOTSTRAP() {
    echo "gein: Ensuring we are in /mnt/gentoo..."
    if [ ! -e /mnt/gentoo/$(basename "$0") ]; then
        cp "$0" /mnt/gentoo/
        cd /mnt/gentoo
    fi

    echo "gein: Setting system time via ntpd..." | fold -s
    if [ -x "$(command -v ntpd)" ]; then
        ntpd -q -g
    fi

    echo "gein: Downloading and extracting Stage3 tarball..."
    if [ -x "$(command -v curl)" ]; then
        stage3_source="http://distfiles.gentoo.org/releases/$cpu_dir/autobuilds"
        stage3_release="curl -s $stage3_source/latest-stage3-$cpu_arch.txt"
        stage3_current="$($stage3_release|tail -1|awk '{print $1}')"

        $wget "$stage3_source/$stage3_current"
        tar -xpf stage3-* --xattrs --numeric-owner
        rm -rf stage3-*

        unset stage3_source stage3_release stage3_current
    else
        echo "gein: error: curl not present!"
        echo "gein: Exiting..."
        exit 1
    fi

    echo "gein: Mounting hardware devices..."
    hardware_mountpoints="proc sys dev"
    for target in $hardware_mountpoints; do
        if [ -e /mnt/gentoo/"$target" ]; then
            case "$target" in
                proc) mount -t proc /proc /mnt/gentoo/proc ;;
                sys ) mount --rbind /sys  /mnt/gentoo/sys
                      mount --make-rslave /mnt/gentoo/sys ;;
                dev ) mount --rbind /dev  /mnt/gentoo/dev
                      mount --make-rslave /mnt/gentoo/dev ;;
                *) echo "gein: $target: Improper hardware device" \
                         | fold -s
                   exit
            esac
        else
            echo "gein: $target unable to be mounted! Exiting..." \
                | fold -s
            exit
        fi
    done

    unset hardware_mountpoints target

    if [ ! -e "$partition_swap" ]; then
        echo "gein: Setting up swapfile..." | fold -s
        mkswap "$partition_swap"
        swapon "$partition_swap"
        echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "gein: Copying '/etc/resolv.conf'..." | fold -s
    cp -L /etc/resolv.conf /mnt/gentoo/etc/

    echo "gein: Chroot'ing into /mnt/gentoo..." | fold -s
    chroot \
        /mnt/gentoo /usr/bin/env -i \
        HOME="/root" \
        TERM="$TERM" \
        PS1="[chroot \u@\h \W]$ " \
        PATH="/usr/local/sbin/:/usr/local/bin:/usr/sbin" \
        PATH="$PATH:/usr/bin:/sbin:/bin:/opt/bin" \
        MANPATH="/usr/man:/usr/share/man:/usr/local/man" \
        MANPATH="$MANPATH:/usr/local/share/man" \
        /bin/bash --login

    bootstrap_complete="true"
}


#
#

INSTALL() {
    CONFIG

    echo "gein: Setting CPU cores and GPU type..." | fold -s
    sed -i "s/Video_Cards/$VideoCards/g; s/Make_Opts/-j$CPUCores/g" \
        /etc/portage/make.conf

    echo "gein: Syncing Portage and selecting profile..." | fold -s
    $emerge_sync
    eselect profile list | grep -Evi "dev|exp"

    echo "gein: Choose the latest stable release" | fold -s
    profile_target=""
    while [ -z "$profile_target" ]; do
        read -p "Which profile?: " profile_target
    done
    eselect profile set "$profile_target"
    $emerge -uDN @world

    echo "gein: Setting timezone..." | fold -s
    echo "$timezone" > /etc/timezone
    $emerge --config sys-libs/timezone-data

    echo "gein: Setting locale..." | fold -s
    echo "$Locale" > /etc/locale.gen && locale-gen
    L="$(echo $Locale | awk -F '[-]' '{print $1}')"
    LL="$(eselect locale list|grep -i $L|awk -F '[][]' '{print $2}')"
    eselect locale set "$LL"
    env-update && source /etc/profile
    export PS1="[chroot \u@\h \W]$ "

    echo "gein: Emerging base system packages..." | fold -s
    $emerge @gein-base
    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "gein: Emerging intel-microcode"
        $emerge intel-microcode
    fi

    echo "gein: Configuring Linux kernel..."
    cd /usr/src/linux
    if [ "$kernel_autobuild" = "true" ]; then
        if [ -z "$kernel_config" ]; then
            make defconfig
        else
            $wget "$kernel_config" -O /usr/src/linux/.config
        fi
    elif [ "$kernel_autobuild" = "false" ]; then
        if [ -z "$kernel_config" ]; then
            make defconfig
            make menuconfig
        else
            $wget "$kernel_config" -O /usr/src/linux/.config
            make menuconfig
        fi
    else
        echo "gein: Error: AutoKernel isn't true or false. Exiting..."
    fi

    echo "gein: Compiling Linux kernel and modules..."
    $make && $make modules && \
        $make install && $make modules install && \
        $make distclean
    cd /

    echo "gein: Adding services to OpenRC..."
    rc-update add dhcpcd default
    rc-update add cronie default

    echo "gein: Setting hostname..."
    echo "hostname=$hostname" > /etc/conf.d/hostname

    echo "gein: Installing Grub to $partition_boot..."
    grub-install "$partition_boot"
    grub-mkconfig -o /boot/grub/grub.cfg

    install_complete="true"
}


#
#

POSTINSTALL() {
    echo "gein: Setting root password..."
    passwd

    # echo "gein: Creating 'power' group"
    # groupadd power
    #   poweroff reboot shutdown

    read -p "gein: Setup a standard user? [Y/N]: " user_setup
    if echo "$user_setup" | grep -iq "^y"; then
        echo "gein: Creating user account"
        read -p "Username: " username
        useradd -m -G wheel,audio,video \
                -s /bin/bash "$username"
        passwd "$username"
    fi

    echo "gein: Installation complete."

    postinstall_complete="true"
}


#
#

case $1 in
    -p|--partition)
        PREREQUISITES
        PARTITION
        ;;

    -b|--bootstrap)
        PREREQUISITES
        BOOTSTRAP
        ;;

    -i|--install)
        PREREQUISITES
        INSTALL
        POSTINSTALL
        ;;

    *)
        echo "gein: Linux-based derivative of Gentoo"
        echo "  -p, partition    Partition and mount disk(s)"
        echo "  -b. bootstrap    Bootstrap the stage3 tarball"
        echo "  -i, install      Install Gentoo"
esac
