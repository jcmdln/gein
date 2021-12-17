#!/usr/bin/env bash
# gein.sh - GEntoo INstaller
#
# Copyright (c) 2021 Johnathan C. Maudlin
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
### Using
#
# Here's a _very_ simple example of running this script on a Gentoo LiveCD:
#
#    $ fdisk /dev/sda
#    $ mkfs.xfs /dev/sda1
#    $ mount /dev/sda1 /mnt/gentoo
#    $ curl -LO https://raw.githubusercontent.com/jcmdln/gein/master/gein.sh
#    $ sh gein.sh
#    gein: Gentoo minimal installation script
#      -b. --bootstrap    Bootstrap the stage3 tarball
#      -i, --install      Install Gentoo
#    $ GEIN_PARTITION_BOOT="/dev/sda" sh ./gein.sh --bootstrap
#    $ sh ./gein.sh --install
#
### Environment Variables
#
# The following environment variables are to be used when bootstrapping though
# only GEIN_PARTITION_BOOT is required. In all other cases the defaults should
# be within reason.
#
# GEIN_CONFIG_URL
#
#     The base URL where the source of this repository (or your fork) is
#     available. This allows this script to download all needed config files
#     rather than you having to retrieve them manually.
#
# GEIN_HOSTNAME
#
#     The hostname to set on the resulting system after the installation has
#     completed.
#
# GEIN_LOCALE
#
#     The LOCALE to set based on your preference. All language defaults such as
#     this default to some variant of en_US.UTF-8 for simplicity of design.
#
# GEIN_PARTITION_BOOT (required)
#
#     The partition to do grub things to. This is blank by default and must be
#     set, or gein will exit with an error.
#
# GEIN_PARTITION_SWAP
#
#     The partition used for swap. The default is to use a swapfile so that you
#     can change this easily later.
#
# GEIN_PARTITION_SWAPFILE_SIZE
#
#     The size of the swapfile to create. This is only used if
#     GEIN_PARTITION_SWAP is set to "/swapfile", and defaults to 4G.
#
# GEIN_TIMEZONE
#
#     The timezone to set on the resulting system after the installation has
#     completed. This is set to America/New_York by default.
#
# GEIN_VIDEO_CARDS
#
#     The value of VIDEO_CARDS that will be set in '/etc/portage/make.conf',
#     which may either be left blank or be configured as mentioned in the
#     following url:
#
#         https://wiki.gentoo.org/wiki/etc/portage/make.conf#VIDEO_CARDS
#
#     You most likely should use "amdpu", "intel", or "nvidia".
#
##

set -e -o pipefail

GEIN_CONFIG_URL="${GEIN_CONFIG_URL:-https://raw.githubusercontent.com/jcmdln/gein/master}"
GEIN_HOSTNAME="${GEIN_HOSTNAME:-gein}"
GEIN_LOCALE="${GEIN_LOCALE:-en_US.UTF-8 UTF-8}"
GEIN_MAKEOPTS="${GEIN_MAKEOPTS:--s -j$(grep -c ^processor /proc/cpuinfo)}"
GEIN_PARTITION_BOOT="${GEIN_PARTITION_BOOT}"
GEIN_PARTITION_SWAP="${GEIN_PARTITION_SWAP:-/swapfile}"
GEIN_PARTITION_SWAPFILE_SIZE="${GEIN_PARTITION_SWAPFILE_SIZE:-4G}"
GEIN_TIMEZONE="${GEIN_TIMEZONE:-America/New_York}"
GEIN_VIDEO_CARDS="${GEIN_VIDEO_CARDS}"

log() {
    local columns="${COLUMNS:-$(stty size|awk '{print $2}')}"
    echo "gein: $@" | fold -s -w $columns
}

log_err() {
    log "error: $@" >&2
    log "Exiting..."
    return 1
}

log_warn() {
    log "warning: $@" >&2
}


gein_prerequisites() {
    local cpuarch="$(uname -m)"

    log "Determining if we can install on \"$cpuarch\"..."
    case "$cpuarch" in
        i486|i586)
            GEIN_CPU_DIR="x86"
            GEIN_CPU_ARCH="i486"
            log "found $cpuarch which maps to 'i486'"
            ;;

        i686|x86|x86_32)
            GEIN_CPU_DIR="x86"
            GEIN_CPU_ARCH="i686"
            log "found $cpuarch which maps to 'i686'"
            ;;

        amd64|x86_64)
            GEIN_CPU_DIR="amd64"
            GEIN_CPU_ARCH="amd64"
            log "found $cpuarch which maps to 'amd64'"
            ;;

        *)
            log_err "cpu arch \"$cpuarch\" isn't automated yet!"
    esac

    if [ -v "$GEIN_PARTITION_BOOT" ]; then
        log_err \
            "environment variable 'GEIN_PARTITION_BOOT' is not set!" \
            "Please ensure you have partitioned and mounted your disks" \
            "your disks, as well as updated the variables associated" \
            "with the required partitions."
    fi

    if [ ! -e /mnt/gentoo ]; then
        log_err \
            "'/mnt/gentoo' does not exist! '/mnt/gentoo' is" \
            "required for us to build Gentoo, as it is referred to later" \
            "in this script. Please ensure your mounted partitions are" \
            "correct."
    fi

    log "Checking if curl is installed..."
    if [ -z "$(command -v curl)" ]; then
        log_err "curl not installed!"
        exit 1
    fi

    log "Correcting system time using ntpd..."
    if [ -n "$(command -v ntpd)" ]; then
        ntpd -q -g
    else
        log_warn "ntpd not found! Assuming system time is correct"
    fi

    if [ ! -e /mnt/gentoo/gein.sh ]; then
        log "Copying 'gein.sh' to /mnt/gentoo... "
        cp gein.sh /mnt/gentoo/
    fi
}


gein_bootstrap() {
    cd /mnt/gentoo

    log "Downloading and extracting Stage3 tarball..."
    local stage3_src="https://distfiles.gentoo.org/releases/$GEIN_CPU_DIR/autobuilds"
    local stage3_target="$stage3_src/latest-stage3-amd64-openrc.txt"

    log "Determining the latest Stage3 version..."
    local stage3_path="$(curl -sf $stage3_target | awk 'END{print $1}')"
    local stage3_file="$(echo $stage3_path | awk -F[/' '] '{print $2}')"
    local stage3_url="$stage3_src/$stage3_path"

    if [ -f "/mnt/gentoo/$stage3_file" ]; then
        log "Latest stage3 tarball is already present"
    else
        log "Downloading \"$stage3_url\"..."
        curl -sfL "$stage3_url" -o "/mnt/gentoo/$stage3_file"
        tar -xpf "$stage3_file" --xattrs --numeric-owner -C /mnt/gentoo
        rm -f "./$stage3_file"
    fi

    log "Downloading portage configuration files..."
    config_dirs="
        /etc/portage/sets
    "

    for config_dir in $config_dirs; do
        if [ ! -e "/mnt/gentoo/$config_dir" ]; then
            mkdir -p "/mnt/gentoo/$config_dir"
        else
            if [ ! -d "/mnt/gentoo/$config_dir" ]; then
                rm "/mnt/gentoo/$config_dir"
            fi
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
        if [ -e "/mnt/gentoo/$config_file" ]; then
            rm -rf "/mnt/gentoo/$config_file"
        fi
        curl -sf "$GEIN_CONFIG_URL/$config_file" -o "/mnt/gentoo/$config_file"
    done; unset config_files config_file

    log "Updating make.conf..."
    sed -i "
        s/^MAKEOPTS=.*$/MAKEOPTS=\"-j$GEIN_MAKEOPTS\"/;
        s/^VIDEO_CARDS=.*$/VIDEO_CARDS=\"$GEIN_VIDEO_CARDS\"/;
    " /mnt/gentoo/etc/portage/make.conf

    log "Mounting hardware devices..."
    for hw_mount in $(echo proc sys dev); do
        if [ ! -e /mnt/gentoo/"$hw_mount" ]; then
            log_err "$hw_mount: unable to be mounted!"
        fi

        case "$hw_mount" in
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
                log_err "$hw_mount: Improper hardware device"
        esac
    done; unset hw_mounts hw_mount

    log "Setting up swapfile..."
    local swap_target="/mnt/gentoo/$GEIN_PARTITION_SWAP"
    if [ -n "$GEIN_PARTITION_SWAP" ]; then
        if [ "$GEIN_PARTITION_SWAP" = "/swapfile" ]; then
            fallocate -l $GEIN_PARTITION_SWAP_SIZE $swap_target
            chmod 0600 /mnt/gentoo/swapfile
        fi

        mkswap $swap_target
        swapon $swap_target
        echo "$GEIN_PARTITION_SWAP none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    log "Copying '/etc/resolv.conf'..."
    echo "nameserver 1.1.1.1" > /mnt/gentoo/etc/resolv.conf

    log "Chroot'ing into /mnt/gentoo..."
    chroot \
        /mnt/gentoo /usr/bin/env -i \
        HOME="/root" \
        GEIN_CONFIG_URL="$GEIN_CONFIG_URL" \
        GEIN_HOSTNAME="$GEIN_HOSTNAME" \
        GEIN_LOCALE="$GEIN_LOCALE" \
        GEIN_MAKEOPTS="$GEIN_MAKEOPTS" \
        GEIN_TIMEZONE="$GEIN_TIMEZONE" \
        GEIN_VIDEO_CARDS="$GEIN_VIDEO_CARDS" \
        GEIN_KERNEL_AUTOBUILD="$GEIN_KERNEL_AUTOBUILD" \
        GEIN_PARTITION_BOOT="$GEIN_PARTITION_BOOT" \
        GEIN_PARTITION_SWAP="$GEIN_PARTITION_SWAP" \
        GEIN_PARTITION_SWAPFILE_SIZE="$GEIN_PARTITION_SWAPFILE_SIZE" \
        /bin/bash --login
}


gein_install() {
    log "Setting root password..."
    passwd

    read -p "gein: Setup a standard user? [Y/N]: " setup_user
    if echo "$setup_user" | grep -iq "^y"; then
        log "Creating user account"
        read -p "Username: " username
        useradd -m -G wheel,audio,video -s /bin/bash "$username"
        passwd "$username"
    fi

    log "Syncing Portage..."
    emerge -q --sync

    log "Selecting a profile..."
    eselect profile list | grep -Evi "dev|exp"
    profile_target=""
    while [ -z "$profile_target" ]; do
        read -p "Which profile?: " profile_target
    done
    eselect profile set "$profile_target"

    log "Updating @world"
    emerge -uDN --quiet-build --verbose @world

    log "Setting timezone..."
    echo "$GEIN_TIMEZONE" > /etc/timezone
    emerge --config --quiet-build --verbose sys-libs/timezone-data

    log "Setting locale..."
    echo "$GEIN_LOCALE" > /etc/locale.gen
    locale-gen
    L="$(echo $GEIN_LOCALE | awk -F '[-]' '{print $1}')"
    LL="$(eselect locale list | grep -i $L | awk -F '[][]' '{print $2}')"
    eselect locale set "$LL"

    log "Updating environment..."
    env-update
    source /etc/profile

    log "Emerging base system packages..."
    $emerge @gein-base
    if grep -Rqi 'intel' /proc/cpuinfo; then
        log "Emerging intel-microcode"
        $emerge intel-microcode
    fi

    log "Adding services to OpenRC..."
    rc-update add dhcpcd default
    rc-update add cronie default

    log "Setting hostname..."
    echo "hostname=$GEIN_HOSTNAME" > /etc/conf.d/hostname

    log "Installing Grub to $GEIN_PARTITION_BOOT..."
    grub-install "$GEIN_PARTITION_BOOT"
    grub-mkconfig -o /boot/grub/grub.cfg

    log "Installation complete."
}


case $1 in
    -b|--bootstrap)
        gein_prerequisites
        gein_bootstrap
        ;;

    -i|--install)
        gein_install
        ;;

    *)
        echo -e \
             "Gentoo minimal installation script\n" \
             "  -b. --bootstrap    Bootstrap the stage3 tarball" \
             "  -i, --install      Install Gentoo"
esac
