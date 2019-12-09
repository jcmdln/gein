#!/usr/bin/env sh
#
# gein.sh - Gentoo minimal installation script
#
#
## License
#
# Copyright (c) 2018, 2019 Johnathan C. Maudlin
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#
#
## About
#
# While this script does assist with installing Gentoo, it is NOT a
# replacement for reading and understanding the Gentoo Handbook for
# your system.  See https://gentoo.org/get-started/ for information on
# getting started with Gentoo.
#
# This file uses inline documentation whenever possible to preserve
# context, situational or otherwise. Please read the entire file before
# continuing to ensure you are aware of how this script functions, as
# undesired behaviors for your use-case may exist.
#
# This script does NOT automatically partition your disk(s). You MUST
# partition and mount your disk(s) before running this script.
#
# This file uses a modified 0-clause MIT license. If you did not
# receive a copy of the license, please visit
# https://jcmdln.github.io/gein/License.md for a copy.
#
#
## Conventions
#
#   Functions
#
#   Variables
#
#       Global Variables
#
#       Local Variables
#
###


# This section describes variables that will define the resulting
# system.
#
#   GEIN_CONFIG_URL
#
#       Missing description
#
#   GEIN_HOSTNAME
#
#       Missing description
#
#   GEIN_LOCALE
#
#       Missing description
#
#   GEIN_NCPUS
#
#       The number of CPUs reported as available.
#
#   GEIN_VIDEO_CARDS
#
#       The value of VIDEO_CARDS to use in '/etc/portage/make.conf'.
#
#   GEIN_TIMEZONE
#
#       Missing description

GEIN_CONFIG_URL="https://raw.githubusercontent.com/jcmdln/gein/master"
GEIN_HOSTNAME="gein"
GEIN_LOCALE="en_US.UTF-8 UTF-8"
GEIN_NCPUS="$(grep -c ^processor /proc/cpuinfo)"
GEIN_TIMEZONE="America/New_York"
GEIN_VIDEO_CARDS="false"


# This section contains aliases for commands which are used throughout
# the script.  By default, commands have their output and prompts
# suppressed when possible.  Should you want to review or debug this
# script, you may want to adjust these as desired.

alias curl="curl -sSf"
alias emerge="emerge -v --quiet-build"
alias emerge_sync="emerge -q --sync"
alias make="make -s -j$GEIN_NCPUS"
alias mkdir="mkdir -p"
alias rm="rm"
alias wget="wget -q"


# This section describes how the kernel will be built, and whether the
# user will be prompted to configure their kernel. The two mentioned
# variables may be used together to suit a variety of use-cases.
#
#   GEIN_KERNEL_AUTOBUILD
#
#     When 'true', this implies that the user does not want to perform
#     any manual configuration of the kernel, regardless of whether a
#     configuration file was provided. '$ make menuconfig' will NOT be
#     run before compiling the kernel.
#
#     When any value OTHER than 'true', this implies that the use DOES
#     want to perform manual configuration on the kernel, regardless
#     of whether a configuration file was provided. '$ make menuconfig'
#     will be run before compiling the kernel.
#
#   GEIN_KERNEL_CONFIG
#
#     The URL location of a kernel configuration file.  If this variable
#     is unset, no attempt to download a configuration file will be
#     made, instead using the kernel defconfig.

GEIN_KERNEL_AUTOBUILD="true"
#GEIN_KERNEL_CONFIG=""


# TODO: I would like this section to be where a user defines their
# partition schema specifically for configuring GRUB2.
#
#   GEIN_PARTITION_BOOT
#
#       Missing description
#
#   GEIN_PARTITION_UEFI
#
#       Missing description
#
#   GEIN_PARTITION_SWAP
#
#       Missing description

#GEIN_PARTITION_BOOT=""
#GEIN_PARTITION_UEFI=""
#GEIN_PARTITION_SWAP=""


# This section determines the current system architecture, which later
# is used to download the correct Stage 3 archive.  Depending on the
# architecture, there may be implications in which directory we should
# retrieve our Stage 3 from.
#
# TODO: use qemu/kvm to define and test against more achitectures

case "$(uname -m)" in
    # TODO: document _what_ and _why_
    i486|i586)
        GEIN_CPU_DIR="x86"
        GEIN_CPU_ARCH="i486"
    ;;

    # TODO: document _what_ and _why_
    i686|x86|x86_32)
        GEIN_CPU_DIR="x86"
        GEIN_CPU_ARCH="i686"
    ;;

    # TODO: document _what_ and _why_
    amd64|x86_64)
        GEIN_CPU_DIR="amd64"
        GEIN_CPU_ARCH="amd64"
    ;;

    *)
        echo "gein: error: your architecture has not been defined." \
             "Submit an issue with the output of 'uname -m'." \
             | fold -s
        echo "gein: Exiting..."
        exit 1
esac


# TODO: document this section
#
#   GEIN_CONFIG_URL
#
#       Missing description

GEIN_CONFIG_URL="https://raw.githubusercontent.com/jcmdln/gein/master"


# TODO: document this section
#
#

PREREQUISITES() {
    if [ -z "$GEIN_PARTITION_BOOT" ] || [ -z "$GEIN_VIDEO_CARDS" ]; then
        echo "gein: error: required variables are unset!" | fold -s
        echo "gein: please ensure you have partitioned and mounted" \
             "your disks, as well as updated the variables associated" \
             "with the required partitions. You must also declare" \
             "your VideoCard.  Please see gein.sh for instructions." \
            | fold -s
        echo "gein: Exiting..." | fold -s
        exit 1
    fi

    if [ ! -e /mnt/gentoo ]; then
        echo "gein: error: '/mnt/gentoo' does not exist!" | fold -s
        echo "gein: '/mnt/gentoo' is referred to later in this script," \
             "and is required to continue. Please ensure your mounted" \
             "partitions are correct." | fold -s
        echo "gein: Exiting..." | fold -s
        exit 1
    fi

    # TODO: check that all required packages/utilities are available
    # in case this script isn't run from the Gentoo installation CD.

    export GEIN_COMPLETED_STAGES="$GEIN_COMPLETED_STAGES PREREQUISITES"
}


# TODO: document this section
#
#

BOOTSTRAP() {
    echo "gein: ensure script is available in /mnt/gentoo... "
    if [ ! -e /mnt/gentoo/$(basename "$0") ]; then
        cp "$0" /mnt/gentoo/
        cd /mnt/gentoo
    fi

    echo "gein: correct system time via ntpd... "
    if [ -x "$(command -v ntpd)" ]; then
        ntpd -q -g
    else
        echo "gein: error: ntpd is not available!"
        echo "gein: exiting..."
        exit 1
    fi

    echo "gein: Downloading and extracting Stage3 tarball..."
    if [ -x "$(command -v curl)" ]; then
        stage3_source="http://distfiles.gentoo.org/releases/$GEIN_CPU_DIR/autobuilds"
        stage3_release="curl -s $stage3_source/latest-stage3-$GEIN_CPU_ARCH.txt"
        stage3_current="$($stage3_release | tail -1 | awk '{print $1}')"

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
    for hw_mountpoint in "proc sys dev"; do
        if [ -e /mnt/gentoo/"$hw_mountpoint" ]; then
            case "$hw_mountpoint" in
                proc)
                    mount -t proc /proc /mnt/gentoo/proc
                ;;

                sys)
                    mount --rbind /sys /mnt/gentoo/sys
                    mount --make-rslave /mnt/gentoo/sys
                ;;

                dev)
                    mount --rbind /dev /mnt/gentoo/dev
                    mount --make-rslave /mnt/gentoo/dev
                ;;

                *) echo "gein: error: $hw_mountpoint: Improper hardware device"
                   exit 1
            esac
        else
            echo "gein: error: $hw_mountpoint unable to be mounted! Exiting..."
            exit 1
        fi
    done; unset hw_mountpoint

    if [ ! -e "$GEIN_PARTITION_SWAP" ]; then
        echo "gein: Setting up swapfile..."
        mkswap "$GEIN_PARTITION_SWAP"
        swapon "$GEIN_PARTITION_SWAP"
        echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "gein: Copying '/etc/resolv.conf'..." | fold -s
    cp -L /etc/resolv.conf /mnt/gentoo/etc/

    echo "gein: Chroot'ing into /mnt/gentoo..." | fold -s
    chroot \
        /mnt/gentoo /usr/bin/env -i \
        HOME="/root" \
        TERM="$TERM" \
        PS1="\[\e];\u@\h: \w\a\][\u@\h \W]\$ " \
        PATH="/sbin:/usr/sbin:/opt/sbin:/usr/local/sbin/" \
        PATH="$PATH:/bin:/usr/bin:/opt/bin:/usr/local/bin" \
        PATH="$PATH:$HOME/bin:$HOME/.local/bin" \
        MANPATH="/usr/man:/usr/share/man:/usr/local/man:/usr/local/share/man" \
        MANPATH="$MANPATH:$HOME/man:$HOME/.local/man:$HOME/.local/share/man" \
        /bin/bash --login

    export GEIN_COMPLETED_STAGES="$GEIN_COMPLETED_STAGES BOOTSTRAP"
}


#
#

INSTALL() {
    config_dirs="
        /etc/portage/package.accept_keywords
        /etc/portage/package.use
        /etc/portage/sets
     "

    for config_dir in $config_dirs; do
        if [ ! -d "$config_dir" ]; then
            rm "$config_dir"
        fi

        if [ ! -e "$config_dir" ]; then
            mkdir "$config_dir"
        fi
    done; unset config_dirs config_dir

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

    for config_file in $config_files; do
        wget "$GEIN_CONFIG_URL/$config_file" -O "$config_file"
    done; unset config_files config_file

    echo "gein: Setting CPU cores and GPU type..."
    sed -i "s/GEIN_VIDEO_CARDS/$GEIN_VIDEO_CARDS/g; s/GEIN_NCPUS/-j$GEIN_NCPUS/g" \
        /etc/portage/make.conf

    echo "gein: Syncing Portage and selecting profile..."
    emerge_sync
    eselect profile list | grep -Evi "dev|exp"

    echo "gein: Choose the latest stable release"
    profile_target=""
    while [ -z "$profile_target" ]; do
        read -p "Which profile?: " profile_target
    done
    eselect profile set "$profile_target"
    emerge -uDN @world

    echo "gein: Setting timezone..." | fold -s
    echo "$GEIN_TIMEZONE" > /etc/timezone
    emerge --config sys-libs/timezone-data

    echo "gein: Setting locale..." | fold -s
    echo "$GEIN_LOCALE" > /etc/locale.gen &&
    locale-gen
    L="$(echo $GEIN_LOCALE | awk -F '[-]' '{print $1}')"
    LL="$(eselect locale list | grep -i $L | awk -F '[][]' '{print $2}')"
    eselect locale set "$LL"
    env-update &&
    source /etc/profile
    export PS1="\[\e];\u@\h: \w\a\][\u@\h \W]\$ "

    echo "gein: Emerging base system packages..." | fold -s
    emerge @gein-base
    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "gein: Emerging intel-microcode"
        emerge intel-microcode
    fi

    echo "gein: Configuring Linux kernel..."
    cd /usr/src/linux
    if [ "$GEIN_KERNEL_AUTOBUILD" = "true" ]; then
        if [ -z "$GEIN_KERNEL_CONFIG" ]; then
            make defconfig
        else
            wget "$GEIN_KERNEL_CONFIG" -O /usr/src/linux/.config
        fi
    elif [ "$GEIN_KERNEL_AUTOBUILD" = "false" ]; then
        if [ -z "$GEIN_KERNEL_CONFIG" ]; then
            make defconfig
            make menuconfig
        else
            wget "$GEIN_KERNEL_CONFIG" -O /usr/src/linux/.config
            make menuconfig
        fi
    else
        echo "gein: Error: AutoKernel isn't true or false. Exiting..."
    fi

    echo "gein: Compiling Linux kernel and modules..."
    make &&
    make modules &&
    make install &&
    make modules install &&
    make distclean &&
    cd /

    echo "gein: Adding services to OpenRC..."
    rc-update add dhcpcd default
    rc-update add cronie default

    echo "gein: Setting hostname..."
    echo "hostname=$GEIN_HOSTNAME" > /etc/conf.d/hostname

    echo "gein: Installing Grub to $GEIN_PARTITION_BOOT..."
    grub-install "$GEIN_PARTITION_BOOT"
    grub-mkconfig -o /boot/grub/grub.cfg

    echo "gein: Setting root password..."
    passwd

    # echo "gein: Creating 'power' group"
    # groupadd power
    #   poweroff reboot shutdown

    read -p "gein: Setup a standard user? [Y/N]: " setup_user
    if echo "$setup_user" | grep -iq "^y"; then
        echo "gein: Creating user account"
        read -p "Username: " username
        useradd -m -G wheel,audio,video \
                -s /bin/bash "$username"
        passwd "$username"
    fi

    echo "gein: Installation complete."

    export GEIN_COMPLETED_STAGES="$GEIN_COMPLETED_STAGES INSTALL"
}


#
#

case $1 in
    -b|--bootstrap)
        PREREQUISITES
        BOOTSTRAP
    ;;

    -i|--install)
        PREREQUISITES
        INSTALL
    ;;

    *)
        echo "gein: Gentoo minimal installation script"
        echo "  -b. bootstrap    Bootstrap the stage3 tarball"
        echo "  -i, install      Install Gentoo"
esac
