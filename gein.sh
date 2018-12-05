#!/usr/bin/env sh
#
#
#
##


# $Source is the protocol and domain that will be the top-level reference
# when retrieving configuration files.

Source="https://gein.io"


# This section defines some command aliases that will be used later on,
# and is primarily used as a mechanism to inhibit or control output in a
# way that can be easily updated or changed if needed.

Emerge="emerge -v --quiet-build"
EmergeSync="emerge -q --sync"
Make="make -s -j$CPUCores"
Mkdir="mkdir -p"
Rm="rm"
Wget="wget -q"


# The following variables will be called later on to perform the
# following:
#
#  * $Hostname is used to set the hostname.
#  * $Locale is your language and encoding of choice
#  * $TimeZone is the target within '/usr/share/zoneinfo' of your region

Hostname="gein"
Locale="en_US.UTF-8 UTF-8"
TimeZone="America/New_York"


# The 'PartitionBoot' variable *MUST* be set to proceed with the
# installation. The example uses '/dev/sda' which will later install
# GRUB to the MBR rather than a partition. Change this to your desired
# '/boot' partition.

#PartitionBoot="/dev/sda1"
#PartitionUefi="/dev/sda2"
#PartitionRoot="/dev/sda3"
#PartitionHome="/dev/sda4"
#PartitionSwap="/dev/sda5"


# The 'VideoCards' variable *MUST* be set to proceed with the
# installation. Several examples have been provided based on brand or
# target, though if you have any specific needs then set this variable
# accordingly. If you don't need any video support, then set
# 'VideoCards' to false like the last example.

#VideoCards="i915 i965 intel"
#VideoCards="amdgpu radeonsi"
#VideoCards="nouveau nvidia"
#VideoCards="virtualbox vmware"
#VideoCards="false"


## Portage
#
# Leave 'CPUCores' as-is. It will count the number of available cores
# which will be used during this script and set in the 'make.conf'.
# Changing this to 'Cores + 1', despite this being suggested in many
# corners of the web, is not a good idea and will actually increase the
# total time needed to compile.

case "$(uname -m)" in
    i486|i586)
        CPUDir="x86"
        CPUArch="i486";;
    i686|x86|x86_32)
        CPUDir="x86"
        CPUArch="i686";;
    amd64|x86_64)
        CPUDir="amd64"
        CPUArch="amd64";;

    *)
        echo "gein: error: your architecture has not been defined yet" \
	    | fold -s
        echo "gein: Submit an issue with the output of 'uname -m'" \
	    | fold -s
        exit 1
esac

CPUCores="$(grep -c ^processor /proc/cpuinfo)"


## Gentoo Stage3
#
# This section exists to automate identifying and downloading the latest
# stage3 archive under the condition that cURL is present. This is not
# an issue when using the Gentoo installation CD's though prevents
# errors when executing MINIMAL() or DESKTOP() due to cURL missing
# after completing the BOOTSTRAP().

S3_Source="http://distfiles.gentoo.org/releases/$CPUDir/autobuilds"

if [ -x "$(command -v curl)" ]; then
    S3_Release="curl -s $S3_Source/latest-stage3-$CPUArch.txt"
    S3_Current="$($S3_Release|tail -1|awk '{print $1}')"
    Stage3="$S3_Source/$S3_Current"
else
    echo "gein: error: curl not present!"
    echo "gein: Exiting..."
    exit 1
fi


# By default, 'AutoKernel' is set to 'true' which means that the kernel
# will be built using 'make defconfig'. If you want to run
# 'make defconfig; make menuconfig' then set $AutoKernel to 'false'. You
# may also supply your own URL to $KernelConfig while setting
# 'AutoKernel' to 'false' to use a pre-built kernel config. An example
# kernel config is provided though commented out as it may quickly become
# irrelevant or dangerous to use.

AutoKernel="true"
#KernelConfig=""


# `gein` relies on downloading configuration files from the main
# repository. Here we will create the 'Source' variable which points to
# the main repository, and 'Config' is the list of all configuration
# files that will be installed.

ConfigSource="$Source"

CONFIG() {
    # This is a list of all config files that need to be downloaded and
    # moved into place.
    Configs="
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

    # Files/Folders that need to be removed and re-created. Sometimes
    # 'package.use' is a file, sometimes it's a folder. We need it to
    # be a folder so we'll remove it outright and create what we need.
    # Sure, this is wasting a little time, I know.
    ConfigFolders="
        /etc/portage/package.accept_keywords
        /etc/portage/package.use
        /etc/portage/sets
    "

    for Folder in $ConfigFolders; do
        if [ ! -d "$Folder" ]; then
	    $Rm "$Folder"
	fi

        if [ ! -e "$Folder" ]; then
	    $Mkdir "$Folder"
	fi
    done

    for File in $Configs; do
        $Wget "$Source/$File" -O "$File"
    done
}


#
#

PREREQUISITES() {
    if [ -z "$PartitionBoot" ] || [ -z "$VideoCards" ]; then
        echo "gein: error: required variables are unset!" | fold -s
	echo "gein: please ensure you have partitioned and mounted" \
	     "your disks, as well as updated the variables associated" \
	     "with the required partitions. You must also declare" \
	     "your VideoCard. Please see gein.sh for instructions." \
	    | fold -s
        echo "gein: Exiting..." | fold -s
        exit 1
    fi
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
}


# Bootstrapping a Gentoo stage3 archive is a fairly quick process though
# we must also ensure this script will be accessible from the chroot as
# well as other housekeeping tasks. The Portage configuration files are
# setup in this section.

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
    if [ -n "$Stage3" ]; then
        $Wget "$Stage3"
        tar -xpf stage3-* --xattrs --numeric-owner
        rm -rf stage3-*
    else
        echo "gein: 'Stage3' variable is not set! Is cURL missing?" | fold -s
        echo "gein: Exiting..." | fold -s
        exit
    fi

    echo "gein: Mounting hardware devices..."
    HW="proc sys dev"
    for target in $HW; do
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

    if [ ! -e "$SwapFile" ]; then
        echo "gein: Setting up swapfile..." | fold -s
        mkswap "$PartitionSwap"
        swapon "$PartitionSwap"
        echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "gein: Copying '/etc/resolv.conf'..." | fold -s
    cp -L /etc/resolv.conf /mnt/gentoo/etc/

    echo "gein: Chroot'ing into /mnt/gentoo..." | fold -s
    chroot /mnt/gentoo /usr/bin/env -i \
           HOME="/root" TERM="$TERM" PS1="[chroot \u@\h \W]$ " \
           PATH="/usr/local/sbin/:/usr/local/bin:/usr/sbin" \
           PATH="$PATH:/usr/bin:/sbin:/bin:/opt/bin" \
           MANPATH="/usr/man:/usr/share/man:/usr/local/man" \
           MANPATH="$MANPATH:/usr/local/share/man" \
           /bin/bash --login
}


# In this section we will update various configuration files, select the
# desired profile, compile the kernel, and install some basic packages.

MINIMAL() {
    CONFIG

    echo "gein: Setting CPU cores and GPU type..." | fold -s
    sed -i "s/Video_Cards/$VideoCards/g; s/Make_Opts/-j$CPUCores/g" \
        /etc/portage/make.conf

    echo "gein: Syncing Portage and selecting profile..." | fold -s
    $EmergeSync
    eselect profile list | grep -Evi "dev|exp"

    echo "gein: Choose the latest stable release" | fold -s
    TargetProfile=""
    while [ -z "$TargetProfile" ]; do
        read -p "Which profile?: " TargetProfile
    done
    eselect profile set "$TargetProfile"
    $Emerge -uDN @world

    echo "gein: Setting timezone..." | fold -s
    echo "$TimeZone" > /etc/timezone
    $Emerge --config sys-libs/timezone-data

    echo "gein: Setting locale..." | fold -s
    echo "$Locale" > /etc/locale.gen
    locale-gen
    L="$(echo $Locale | awk -F '[-]' '{print $1}')"
    LSet="$(eselect locale list|grep -i $L|awk -F '[][]' '{print $2}')"
    eselect locale set "$LSet"
    env-update && source /etc/profile
    export PS1="[chroot \u@\h \W]$ "

    echo "gein: Emerging base system packages..." | fold -s
    $Emerge @gein-base

    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "gein: Emerging intel-microcode"
        $Emerge intel-microcode
    fi

    echo "gein: Configuring Linux kernel..."
    cd /usr/src/linux
    if [ "$AutoKernel" = "true" ]; then
        if [ -z "$KernelConfig" ]; then
            make defconfig
        else
            $Wget "$KernelConfig" -O /usr/src/linux/.config
        fi
    elif [ "$AutoKernel" = "false" ]; then
        if [ -z "$KernelConfig" ]; then
            make defconfig
            make menuconfig
        else
            $Wget "$KernelConfig" -O /usr/src/linux/.config
            make menuconfig
        fi
    else
        echo "gein: Error: AutoKernel isn't true or false. Exiting..."
    fi

    echo "gein: Compiling Linux kernel and modules..."
    $Make && $Make modules && $Make install && $Make modules install
    $Make distclean
    cd /

    echo "gein: Adding services to OpenRC..."
    rc-update add dhcpcd default
    rc-update add cronie default

    echo "gein: Setting hostname..."
    echo "hostname=$Hostname" > /etc/conf.d/hostname

    echo "gein: Installing Grub to $PartitionBoot..."
    grub-install "$PartitionBoot"
    grub-mkconfig -o /boot/grub/grub.cfg
}


# This section is for completing tasks after the installation is
# complete. The user will have a complete system already installed and
# may skip these steps if desired.

POSTINSTALL() {
    echo "gein: Setting root password..."
    passwd

    # echo "gein: Creating 'power' group"
    # groupadd power
    #   poweroff reboot shutdown

    read -p "gein: Setup a standard user? [Y/N]: " SetupUser
    if echo "$SetupUser" | grep -iq "^y"; then
        echo "gein: Creating user account"
        read -p "Username: " Username
        useradd -m -G wheel,audio,video \
                -s /bin/bash "$Username"
        passwd $Username
    fi

    echo "gein: Installation complete."
}


# This is the CLI that controls what will be run. Keep in mind that the
# BOOTSTRAP() needs to be run to completion before running the MINIMAL()
# or DESKTOP() passes.

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
	MINIMAL
	POSTINSTALL
        ;;

    *)
        echo "gein: Linux-based derivative of Gentoo"
        echo "  -p, partition    Partition and mount disk(s)"
        echo "  -b. bootstrap    Bootstrap the stage3 tarball"
        echo "  -i, install      Install Gentoo"
esac
