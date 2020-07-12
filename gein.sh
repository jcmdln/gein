#!/usr/bin/env bash
# gein.sh - Gentoo installation script
#
## License
#
# Copyright (c) 2018-2020 Johnathan C. Maudlin
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
## About
#
# While this script does assist with installing Gentoo, it is NOT a
# replacement for reading and understanding the Gentoo Handbook for
# your system.  See https://gentoo.org/get-started/ for information on
# getting started with Gentoo.
#
# This file uses inline documentation whenever possible to preserve
# context, situational or otherwise.  Please read the entire file
# before continuing to ensure you are aware of how this script
# functions, as undesired behaviors for your use-case may exist.
#
# This script does NOT automatically partition your disk(s).  You MUST
# partition and mount your disk(s) before running this script.
#
##

set -e -o pipefail
alias make="make -s -j $(grep -c ^processor /proc/cpuinfo)"

# Width-respecting print
function print() {
    echo "$@" | fold -s -w ${COLUMNS:-$(stty size|awk '{print $2}')}
}

# This section describes variables that will define the resulting
# system.  Some are specific to this script, though others should look
# familiar, aside from their prefix.
#
#   GEIN_CONFIG_URL
#
#       The base URL where the source of this repository (or your fork)
#       is available.  This allows this script to download all needed
#       configuration files rather than you having to retrieve them
#       manually.
#
#   GEIN_HOSTNAME
#
#       The hostname to set on the resulting system after the
#       installation has completed.
#
#   GEIN_LOCALE
#
#       The LOCALE to set based on your preference.  All language
#       defaults such as this default to some variant of en_US.UTF-8
#       for simplicity of design.
#
#   GEIN_VIDEO_CARDS
#
#       The value of VIDEO_CARDS that will be set in
#       '/etc/portage/make.conf', which may either be left blank or be
#       configured as mentioned in the following url:
#
#       https://wiki.gentoo.org/wiki//etc/portage/make.conf#VIDEO_CARDS
#
#       You most likely should use "amdpu", "intel", or "nvidia".
#
#   GEIN_TIMEZONE
#
#       The timezone to set on the resulting system after the
#       installation has completed.  This is set to America/New_York
#       by default for simplicity of design.

GEIN_CONFIG_URL="https://raw.githubusercontent.com/jcmdln/gein/master"
GEIN_HOSTNAME="gein"
GEIN_LOCALE="en_US.UTF-8 UTF-8"
GEIN_TIMEZONE="America/New_York"
GEIN_VIDEO_CARDS=""


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

GEIN_KERNEL_AUTOBUILD="true"


# TODO: I would like this section to be where a user defines their
# partition schema, specifically for configuring GRUB2.
#
#   GEIN_PARTITION_BOOT
#
#       Missing description
#
#   GEIN_PARTITION_SWAP
#
#       Missing description
#
#   GEIN_PARTITION_SWAPFILE_SIZE
#
#       The size of the swapfile to create.  This is only used if
#       GEIN_PARTITION_SWAP is set to "/swapfile", and defaults to 4G.

GEIN_PARTITION_BOOT=""
GEIN_PARTITION_SWAP="/swapfile"
GEIN_PARTITION_SWAPFILE_SIZE="4G"

# This section determines the current system architecture, which later
# is used to download the correct Stage 3 archive.  Depending on the
# architecture, there may be implications in which directory we should
# retrieve our Stage 3 from.

case "$(uname -m)" in
    i486|i586)
        GEIN_CPU_DIR="x86"
        GEIN_CPU_ARCH="i486"
    ;;

    i686|x86|x86_32)
        GEIN_CPU_DIR="x86"
        GEIN_CPU_ARCH="i686"
    ;;

    amd64|x86_64)
        GEIN_CPU_DIR="amd64"
        GEIN_CPU_ARCH="amd64"
    ;;

    *)
        print "gein: error: your architecture is not yet defined." \
            "Submit an issue with the output of 'uname -m' to" \
            "https://github.com/jcmdln/gein so it may be reviewed"
        print "gein: Exiting..."
        exit 1
esac


# TODO: document this section
#
#

PREREQUISITES() {
    if [ -v "$GEIN_PARTITION_BOOT" ]; then
        print "gein: error: GEIN_PARTITION_BOOT is not set!"
        print "gein: please ensure you have partitioned and mounted" \
            "your disks, as well as updated the variables associated" \
            "with the required partitions."
        print "gein: Exiting..."
        exit 1
    fi

    if [ ! -e /mnt/gentoo ]; then
        print "gein: error: '/mnt/gentoo' does not exist!"
        print "gein: '/mnt/gentoo' is required for us to build" \
            "Gentoo, as it is referred to later in this script." \
            "Please ensure your mounted partitions are correct."
        print "gein: Exiting..."
        exit 1
    fi
}


# TODO: document this section
#
#

BOOTSTRAP() {
    print "gein: ensure script is available in /mnt/gentoo... "
    if [ ! -e /mnt/gentoo/gein.sh ]; then
        cp gein.sh /mnt/gentoo/
        cd /mnt/gentoo
    fi

    print "gein: correct system time via ntpd... "
    if [ -n "$(command -v ntpd)" ]; then
        ntpd -q -g
    else
        print "gein: warning: ntpd not found!"
        print "gein: assuming system time is correct"
    fi

    print "gein: Downloading and extracting Stage3 tarball..."
    if [ -n "$(command -v curl)" ]; then
        stage3_src="http://distfiles.gentoo.org/releases/$GEIN_CPU_DIR/autobuilds"
        stage3_rel="curl -sSf $stage3_src/latest-stage3-$GEIN_CPU_ARCH.txt"
        stage3_ver="$($stage3_rel | tail -1 | awk '{print $1}')"
        wget -q "$stage3_src/$stage3_ver"
        tar -xpf stage3-* --xattrs --numeric-owner -C /mnt/gentoo
        rm -rf "$PWD/stage3-*"

        unset stage3_src stage3_rel stage3_ver
    else
        print "gein: error: curl not present!"
        print "gein: Exiting..."
        exit 1
    fi

    print "gein: Downloading portage configuration files..."
    config_dirs="
        /etc/portage/sets
    "

    for config_dir in $config_dirs; do
        if [ ! -e "/mnt/gentoo/$config_dir" ]; then
            mkdir -p "/mnt/gentoo/$config_dir"
        else
            [ ! -d "/mnt/gentoo/$config_dir" ] &&
                rm "/mnt/gentoo/$config_dir"
        fi
    done; unset config_dirs config_dir

    config_files="
        /etc/portage/make.conf
        /etc/portage/package.accept_keywords
        /etc/portage/package.license
        /etc/portage/package.use
        /etc/portage/sets/gein-base
        /etc/portage/sets/gein-workstation
        /usr/local/sbin/gpkg
        /usr/local/sbin/kbuild
    "

    for config_file in $config_files; do
        [ -e "/mnt/gentoo/$config_file" ] && rm -rf "/mnt/gentoo/$config_file"
        wget -q "$GEIN_CONFIG_URL/$config_file" -O "/mnt/gentoo/$config_file"
    done; unset config_files config_file

    print "gein: Updating make.conf..."
    sed -i "
        s/^MAKEOPTS.*$/MAKEOPTS\=\"-j$(grep -c ^processor /proc/cpuinfo)\"/;
        s/^VIDEO_CARDS.*$/VIDEO_CARDS=\"$GEIN_VIDEO_CARDS\"/;
    " /mnt/gentoo/etc/portage/make.conf

    print "gein: Mounting hardware devices..."
    for hw_mountpoint in $(echo proc sys dev); do
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

                *)
                    print "gein: error: $hw_mountpoint: Improper " \
                    "hardware device"
                    exit 1
            esac
        else
            print "gein: error: $hw_mountpoint unable to be mounted!" \
            "Exiting..."
            exit 1
        fi
    done; unset hw_mountpoint

    print "gein: Setting up swapfile..."
    if [ -v "$GEIN_PARTITION_SWAP" ]; then
        if [ "$GEIN_PARTITION_SWAP" = "/swapfile" ]; then
            fallocate -l "/mnt/gentoo/$GEIN_PARTITION_SWAP_SIZE"
        fi

        mkswap "/mnt/gentoo/$GEIN_PARTITION_SWAP"
        swapon "/mnt/gentoo/$GEIN_PARTITION_SWAP"
        echo "$GEIN_PARTITION_SWAP none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    print "gein: Copying '/etc/resolv.conf'..."
    cp -L /etc/resolv.conf /mnt/gentoo/etc/

    print "gein: Chroot'ing into /mnt/gentoo..."
    chroot \
        /mnt/gentoo /usr/bin/env -i \
        HOME="/root" \
        MANPATH="/usr/man:/usr/share/man:/usr/local/man:/usr/local/share/man" \
        MANPATH="$MANPATH:$HOME/man:$HOME/.local/man:$HOME/.local/share/man" \
        PS1="\[\e];\u@\h: \w\a\][\u@\h \W]\$ " \
        PATH="/sbin:/usr/sbin:/opt/sbin:/usr/local/sbin/:/bin:/usr/bin" \
        PATH="$PATH:/opt/bin:/usr/local/bin:$HOME/bin:$HOME/.local/bin" \
        TERM="$TERM" \
        /bin/bash --login
}


# TODO: document this section
#
#

INSTALL() {
    print "gein: Syncing Portage..."
    emerge -q --sync

    print "gein: Selecting a profile..."
    eselect profile list | grep -Evi "dev|exp"
    profile_target=""
    while [ -z "$profile_target" ]; do
        read -p "Which profile?: " profile_target
    done
    eselect profile set "$profile_target"

    print "gein: Updating @world"
    emerge -uvDN --quiet-build @world
    print "gein: Setting timezone..."
    print "$GEIN_TIMEZONE" > /etc/timezone
    emerge --config sys-libs/timezone-data

    print "gein: Setting locale..."
    print "$GEIN_LOCALE" > /etc/locale.gen &&
    locale-gen
    L="$(print $GEIN_LOCALE | awk -F '[-]' '{print $1}')"
    LL="$(eselect locale list | grep -i $L | awk -F '[][]' '{print $2}')"
    eselect locale set "$LL"

    print "gein: Updating environment..."
    env-update &&
    source /etc/profile
    export PS1="\[\e];\u@\h: \w\a\][\u@\h \W]\$ "

    print "gein: Emerging base system packages..."
    emerge -v --quiet-build @gein-base
    if grep -Rqi 'intel' /proc/cpuinfo; then
        print "gein: Emerging intel-microcode"
        emerge -v --quiet-build intel-microcode
    fi

    print "gein: Configuring Linux kernel..."
    cd /usr/src/linux
    if [ "$GEIN_KERNEL_AUTOBUILD" = "true" ]; then
        make defconfig
    elif [ "$GEIN_KERNEL_AUTOBUILD" = "false" ]; then
        make defconfig
        make menuconfig
    else
        print "gein: Error: AutoKernel isn't true or false. Exiting..."
    fi

    print "gein: Compiling Linux kernel and modules..."
    make &&
    make modules &&
    make install &&
    make modules install &&
    make distclean &&
    cd /

    print "gein: Adding services to OpenRC..."
    rc-update add dhcpcd default
    rc-update add cronie default

    print "gein: Setting hostname..."
    print "hostname=$GEIN_HOSTNAME" > /etc/conf.d/hostname

    print "gein: Installing Grub to $GEIN_PARTITION_BOOT..."
    grub-install "$GEIN_PARTITION_BOOT"
    grub-mkconfig -o /boot/grub/grub.cfg

    print "gein: Setting root password..."
    passwd

    read -p "gein: Setup a standard user? [Y/N]: " setup_user
    if print "$setup_user" | grep -iq "^y"; then
        print "gein: Creating user account"
        read -p "Username: " username
        useradd -m -G wheel,audio,video -s /bin/bash "$username"
        passwd "$username"
    fi
}


# TODO: document this section
#
#

case $1 in
    -b|--bootstrap)
        PREREQUISITES
        BOOTSTRAP
    ;;

    -i|--install)
        INSTALL
        print "gein: Installation complete."
    ;;

    *)
        print "gein: Gentoo minimal installation script"
        print "  -b. --bootstrap    Bootstrap the stage3 tarball"
        print "  -i, --install      Install Gentoo"
esac
