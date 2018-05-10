#!/usr/bin/env sh
#
# Copyright (C) 2017, 2018
# * Johnathan C Maudlin <jcmdln@gmail.com>
#
# This software is licensed under the Azryn Software Labs Public License
# of version 1.1.0 or later. You should have received a copy of the
# Azryn Software Labs Public License along with this program. If not,
# please see https://apl.azryn.org/ for a copy.
#


## Grub installation path
#
# The 'PartitionBoot' variable *MUST* be set to proceed with the
# installation. The example uses '/dev/sda' which will later install
# GRUB to the MBR rather than a partition. Change this to your desired
# '/boot' partition.

#PartitionBoot="/dev/sda"


## GPU/VGA drivers
#
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


## Configuration
#
# This script relies on downloading configuration files from the main
# repository. Here we will create the 'Source' variable which points to
# the main repository, and 'Config' is the list of all configuration
# files that will be installed.

Source="https://raw.githubusercontent.com/jcmdln/gein/master"

CONFIG() {
    # This is a list of all config files that need to be downloaded and
    # moved into place.
    Configs="
        /etc/portage/make.conf
        /etc/portage/package.accept_keywords
        /etc/portage/package.env
        /etc/portage/package.license
        /etc/portage/package.use/defaults
        /etc/portage/package.use/multilib
        /etc/portage/package.use/packages

        /etc/portage/sets/gein-base
        /etc/portage/sets/gein-i3wm
        /etc/portage/sets/gein-lxqt
        /etc/portage/sets/gein-laptop

        /etc/profile
        /etc/profile.d/alias.sh
        /etc/profile.d/defaults.sh
        /etc/profile.d/golang.sh
        /etc/profile.d/racket.sh

        /etc/Xresources
        /etc/bash/bashrc
        /etc/emacs/default.el
        /etc/i3/config
        /etc/i3status.conf
        /etc/tmux.conf
        /etc/vim/vimrc
        /etc/xinitrc
        /etc/zsh/zshrc

        /usr/local/gein/gpkg
        /usr/local/gein/kbuild
        /usr/local/gein/srandr
    "

    # Files/Folders that need to be removed and re-created. Sometimes
    # 'package.use' is a file, sometimes it's a folder. We need it to
    # be a folder so we'll remove it outright and create what we need.
    # Sure, this is wasting a little time, I know.
    ConfigFolders="
       /etc/bash
       /etc/emacs
       /etc/i3
       /etc/portage/package.use
       /etc/portage/sets
       /etc/profile.d
       /etc/zshrc
       /usr/local/gein
    "

    for Folder in $ConfigFolders; do
        [ -d "$Folder" ]  && rm    -rf "$Folder"
        [ ! -d "$Folder" ] && mkdir -p  "$Folder"
    done

    for File in $Configs; do
        wget -q "$Source/$File" -O "$File"
    done
}


## System
#
# The 'Hostname' is used to set the 'Hostname'.
#
# Change 'Locale' to your language and encoding of choice as needed.
#
# A 2G 'SwapSize' seems to be plenty, even for compiling chromium or
# firefox. Make this larger if needed.
#
# Change 'TimeZone' as needed. 'ls /usr/share/zoneinfo' for your region
# and so on.
#
# By default, 'AutoKernel' is set to 'true' which means that the kernel
# will be built using 'make defconfig'. If you want to run
# 'make defconfig; make menuconfig' then set $AutoKernel to 'false'. You
# may also supply your own URL to $KernelConfig while setting
# 'AutoKernel' to 'false' to use a pre-built kernel config. An example
# kernel config is provided though commented out.

Hostname="gein"
Locale="en_US.UTF-8 UTF-8"
SwapSize="2G"
TimeZone="America/New_York"

AutoKernel="true"
KernelConfig="$Source/usr/src/linux/4.16.config"


## Portage
#
# Leave 'CPUCores' as-is. It will count the number of available cores
# which will be used during this script and set in the 'make.conf'.
# Changing this to 'Cores + 1', despite this being suggested in many
# corners of the web, is not a good idea and will actually increase the
# total time needed to compile.
#

case "$(uname -m)" in
    i486|i586)
        CPUArch="i486";;
    i686|x86|x86_32)
        CPUArch="i686";;
    amd64|x86_64)
        CPUArch="amd64";;

    *)
        echo "gein: CPU arch has not been defined yet"
        echo "gein: Submit an issue with the output of 'uname -m'"
        exit
esac

CPUCores="$(grep -c ^processor /proc/cpuinfo)"


## Command Aliases
#
# This section defines some command aliases that will be used later on,
# and is primarily used as a mechanism to inhibit or control output in a
# way that can be easily updated or changed if needed.

Emerge="emerge -v --quiet-build"
Make="make -s -j$CPUCores"
Wget="wget -q"


## Gentoo Stage3
#
# This section exists to automate identifying and downloading the latest
# stage3 archive under the condition that cURL is present. This is not
# an issue when using the Gentoo installation CD's though prevents
# errors when executing MINIMAL() or DESKTOP() due to cURL missing
# after completing the BOOTSTRAP().

S3_Source="http://distfiles.gentoo.org/releases/$CPUArch/autobuilds"
S3_Release="curl -s $S3_Source/latest-stage3-$CPUArch.txt"

if [ -x "$(command -v curl)" ]; then
    S3_Current="$($S3_Release|tail -1|awk '{print $1}')"
    Stage3="$S3_Source/$S3_Current"
fi


# Bootstrapping a Gentoo stage3 archive is a fairly quick process though
# we must also ensure this script will be accessible from the chroot as
# well as other housekeeping tasks. The Portage configuration files are
# setup in this section.

BOOTSTRAP() {
    echo "Please ensure that you have performed the following: "
    echo "  - Edited the environment variables at the top of this script."
    echo "  - Partitioned and mounted your disk(s)."

    read -p "Proceed with installation? [Y/N]: " Proceed
    if echo "$Proceed" | grep -iq "^y"; then
        echo "gein: Proceeding with installation..."
    else
        echo "gein: Exiting..."
        exit
    fi

    if [ -z "$VideoCards" ] || [ -z "$PartitionBoot" ]; then
        echo "gein: You didn't read $0 and adjust the variables!"
        echo "gein: Exiting..."
        exit
    fi

    echo "gein: Ensuring we are in /mnt/gentoo..."
    if [ ! -e /mnt/gentoo/$(basename "$0") ]; then
        cp "$0" /mnt/gentoo/
        cd /mnt/gentoo
    fi

    echo "gein: Setting system time via ntpd..."
    if [ -x "$(command -v ntpd)" ]; then
        ntpd -q -g
    fi

    echo "gein: Downloading and extracting Stage3 tarball..."
    if [ -n "$Stage3" ]; then
        $Wget "$Stage3"
        tar -xpf stage3-* --xattrs --numeric-owner
        rm -rf stage3-*
    else
        echo "gein: 'Stage3' variable is not set! Is cURL missing?"
        echo "gein: Exiting..."
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
                *) echo "gein: $target: Improper hardware device"
                   exit
            esac
        else
            echo "gein: $target unable to be mounted! Exiting..."
            exit
        fi
    done

    SwapFile="/mnt/gentoo/swapfile"
    if [ ! -e "$SwapFile" ]; then
        echo "gein: Setting up swapfile..."
        fallocate -l "$SwapSize" "$SwapFile"
        chmod 0600 "$SwapFile"
        mkswap "$SwapFile"
        swapon "$SwapFile"
        echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "gein: Copying '/etc/resolv.conf'..."
    cp -L /etc/resolv.conf /mnt/gentoo/etc/

    echo "gein: Chroot'ing into /mnt/gentoo..."
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

    echo "gein: Setting CPU cores and GPU type..."
    sed -i "s/Video_Cards/$VideoCards/g; s/Make_Opts/-j$CPUCores/g" \
        /etc/portage/make.conf

    echo "gein: Syncing Portage and selecting profile..."
    emerge -q --sync
    eselect profile list | grep -Evi "dev|exp"

    echo "gein: choose the latest stable release"
    TargetProfile=""
    while [ -z "$TargetProfile" ]; do
        read -p "Which profile?: " TargetProfile
    done
    eselect profile set "$TargetProfile"
    $Emerge -uDN @world

    echo "gein: Setting timezone..."
    echo "$TimeZone" > /etc/timezone
    $Emerge --config sys-libs/timezone-data

    echo "gein: Setting locale..."
    echo "$Locale" > /etc/locale.gen
    locale-gen
    L=$(echo $Locale | awk -F '[-]' '{print $1}')
    LSet=$(eselect locale list|grep -i $L|awk -F '[][]' '{print $2}')
    eselect locale set $LSet
    env-update && source /etc/profile
    export PS1="[chroot \u@\h \W]$ "

    echo "gein: Emerging base system packages..."
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


# In this section we will install the chosen xorg-driver and packages
# for i3wm as the desktop of choice. Some additional packages are added
# as a convenience.

DESKTOP() {
    echo "gein: Enabling X in '/etc/portage/package.use/defaults'..."
    sed -i '2,$s/^# //g' /etc/portage/package.use/defaults

    echo "gein: Installing Xorg drivers..."
    $Emerge x11-base/xorg-drivers
    env-update && source /etc/profile
    export PS1="[chroot \u@\h \W]$ "

    echo "gein: Installing desktop packages..."
    $Emerge "$DesktopChoice"
    rc-update add consolekit default

    read -p "gein: Install laptop packages? [Y/N]: " SetupUser
    if echo "$SetupUser" | grep -iq "^y"; then
        $Emerge @gein-laptop
        rc-update add laptop_mode default
    fi
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
    bootstrap)
        BOOTSTRAP
        ;;

    minimal)
        MINIMAL && POSTINSTALL
        ;;

    desktop)
        if [ "$VideoCards" == 'false' ]; then
            echo "gein: VideoCards is false, though this requires xorg drivers"
            echo "gein: Exiting..."
            exit
        fi

        case $2 in
            i3wm)
                DesktopChoice="@gein-i3wm"
                MINIMAL && DESKTOP && POSTINSTALL
                ;;

            lxqt)
                DesktopChoice="@gein-lxqt"
                MINIMAL && DESKTOP

                echo "azryn: Set SDDM as the display manager"
                sed -i 's/DISPLAYMANAGER="xdm"/DISPLAYMANAGER="sddm"/g' \
                    /etc/conf.d/xdm
                sed -i 's/startlxqt/"ck-launch-session dbus-launch startlxqt"/g' \
                    /usr/share/xsessions/lxqt.desktop
                rc-update add xdm default
                rc-update add dbus default

                POSTINSTALL
                ;;

            *)
                echo "gein: desktop: Available options"
                echo "  i3wm         i3wm desktop"
                echo "  lxqt         LXQT desktop"
        esac
        ;;

    *)
        echo "gein: Linux-based derivative of Gentoo"
        echo "  bootstrap    Bootstrap the stage3 tarball"
        echo ""
        echo "Post-bootstrap:"
        echo "  minimal      Headless installation"
        echo "  desktop      Desktop installation"
esac
