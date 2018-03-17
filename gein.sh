#/bin/env sh
#
# Copyright (C) 2017, 2018
# * Johnathan C Maudlin <jcmdln@gmail.com>
#
# This software is licensed under the Azryn Software Labs Public License
# of version 1.1.0 or later. You should have received a copy of the
# Azryn Software Labs Public License along with this program. If not,
# please see https://apl.azryn.org/ for a copy.
#
# This script does automate the install but you will need to adjust the
# variables in the following section to suit your desired install. The
# installation process will not proceed unless 'PartitionBoot' and
# 'VideoCards' are set though some example values have been provided. If
# you are unsure how to proceed, consult the Gentoo Handbook.
#


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
#VideoCards="i915 i965 intel"
#VideoCards="amdgpu radeonsi"
#VideoCards="nouveau nvidia"
#VideoCards="virtualbox vmware"


# This section defines some command aliases that will be used later on,
# and is primarily used as a mechanism to inhibit or control output in a
# way that can be easily updated if needed.

Emerge="emerge -v --quiet-build"
Make="make -s -j$CPUCores"
Wget="wget -q"


# This script relies on downloading configuration files from the main
# repository. Here we will create the $Source variable to simplify
# future sections.

Source="https://raw.githubusercontent.com/jcmdln/gein/master"


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
# prevents using the GitHub mirror. Please leave this commented unless
# you plan to emerge git ahead of time.

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

S3_Arch="amd64"
S3_Source="http://distfiles.gentoo.org/releases/$S3_Arch/autobuilds"
S3_Release="curl -s $S3_Source/latest-stage3-$S3_Arch.txt"
[ -x "$(command -v curl)" ] &&
    S3_Current="$($S3_Release|tail -1|awk '{print $1}')" &&
    Stage3="$S3_Source/$S3_Current"


# Bootstrapping a Gentoo stage3 archive is a fairly quick process though
# we must also ensure this script will be accessible from the chroot as
# well as other housekeeping tasks. The Portage configuration files are
# setup in this section.

BOOTSTRAP() {
    echo "Please ensure that you have performed the following: "
    echo "  - Edited the environment variables at the top of this script."
    echo "  - Partitioned and mounted your disk(s)."

    read -ep "Proceed with installation? [Y/N]: " Proceed
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
    [ ! -e /mnt/gentoo/$(basename "$0") ] &&
        cp "$0" /mnt/gentoo/ &&
        cd /mnt/gentoo

    echo "gein: Setting system time via ntpd..."
    [ -x "$(command -v ntpd)" ] &&
	ntpd -q -g

    echo "gein: Downloading and extracting Stage3 tarball..."
    if [ -n "$Stage3" ]; then
        $Wget "$Stage3" &&
            tar -xpf stage3-* --xattrs --numeric-owner &&
            rm -rf stage3-*
    else
        echo "gein: 'S3Tgt' is not set! Is cURL missing? Exiting..."
        exit
    fi

    echo "gein: Mounting hardware devices..."
    HW="proc sys dev"
    for target in $HW; do
        if [ -e /mnt/gentoo/"$target" ]; then
            case "$target" in
                proc)
		    mount -t proc /proc /mnt/gentoo/proc
		    ;;

                sys )
		    mount --rbind /sys  /mnt/gentoo/sys &&
			mount --make-rslave /mnt/gentoo/sys
		    ;;

                dev )
		    mount --rbind /dev  /mnt/gentoo/dev &&
			mount --make-rslave /mnt/gentoo/dev
		    ;;

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
	echo "gein: Setting up swapfile..." &&
            fallocate -l "$SwapSize" "$SwapFile" &&
            chmod 0600 "$SwapFile" &&
            mkswap "$SwapFile" &&
	    swapon "$SwapFile" &&
            echo "/swapfile none swap sw 0 0" >> /mnt/gentoo/etc/fstab
    fi

    echo "gein: Copying '/etc/resolv.conf'..." &&
	cp -L /etc/resolv.conf /mnt/gentoo/etc/

    echo "gein: Downloading Portage configuration files..."
    [ -n "$MakeConf" ] &&
        $Wget "$MakeConf" \
              -O /mnt/gentoo/etc/portage/make.conf
    [ -n "$PackageAcceptKeywords" ] &&
        $Wget "$PackageAcceptKeywords" \
              -O /mnt/gentoo/etc/portage/package.accept_keywords
    [ -n "$PackageEnv" ] &&
        $Wget "$PackageEnv" \
              -O /mnt/gentoo/etc/portage/package.env
    [ -n "$PackageLicense" ] &&
        $Wget "$PackageLicense" \
              -O /mnt/gentoo/etc/portage/package.license
    [ -n "$PackageUse" ] &&
        rm -rf /mnt/gentoo/etc/portage/package.use &&
        $Wget "$PackageUse" \
              -O /mnt/gentoo/etc/portage/package.use
    [ -n "$ReposConf" ] &&
        mkdir -p /mnt/gentoo/etc/portage/repos.conf &&
        $Wget "$ReposConf" \
              -O /mnt/gentoo/etc/portage/repos.conf/gentoo.conf

    echo "gein: Downloading gein Portage package sets..."
    mkdir -p /mnt/gentoo/etc/portage/sets
    PortageSets="
        /etc/portage/sets/gein-base
        /etc/portage/sets/gein-i3wm
        /etc/portage/sets/gein-laptop
        /etc/portage/sets/gein-lxqt
        /etc/portage/sets/gein-steam
    "
    for Set in $PortageSets; do
        $Wget "$Source"/"$Set" -O /mnt/gentoo/"$Set" || \
	    echo "gein: Download failed. Exiting..." && exit
    done

    echo "gein: Chroot'ing into /mnt/gentoo..." &&
	chroot /mnt/gentoo /usr/bin/env -i \
               HOME="/root" TERM="$TERM" PS1="[chroot \u@\h \W]$ " \
               PATH="/usr/local/sbin/:/usr/local/bin:/usr/sbin:/usr/bin" \
               PATH="$PATH:/sbin:/bin:/opt/bin" \
               MANPATH="/usr/man:/usr/share/man:/usr/local/man" \
               MANPATH="$MANPATH:/usr/local/share/man" \
               /bin/bash --login
}


# In this section we will update various configuration files, select the
# desired profile, compile the kernel, and install some basic packages.

MINIMAL() {
    echo "gein: Setting CPU cores and GPU type..." &&
	sed -i "s/Video_Cards/$VideoCards/g; s/Make_Opts/-j$CPUCores/g" \
            /etc/portage/make.conf

    echo "gein: Syncing Portage and selecting profile..." &&
	emerge -q --sync &&
	eselect profile list &&
	echo "gein: Hint: choose the latest 'default/linux/amd64/xx.x'" &&
	TargetProfile="" &&
	while [ -z "$TargetProfile" ]; do
	    read -ep "Which profile?: " TargetProfile
	done &&
	eselect profile set "$TargetProfile" &&
	$Emerge -uDN @world

    echo "gein: Setting timezone..." &&
	echo "$TimeZone" > /etc/timezone &&
	$Emerge --config sys-libs/timezone-data

    echo "gein: Setting locale..." &&
	echo "$Locale" > /etc/locale.gen &&
	locale-gen && locale -a &&
	LocaleMain=$(echo $Locale | awk -F '[-]' '{print $1}') &&
	LocaleSet=$(eselect locale list | grep -i $LocaleMain| \
			awk -F '[][]' '{print $2}') &&
	eselect locale set $LocaleSet &&
	env-update && source /etc/profile &&
	export PS1="[chroot \u@\h \W]$ "

    echo "gein: Emerging minimal packages..." &&
	$Emerge @gein-base

    if grep -Rqi 'intel' /proc/cpuinfo; then
        echo "gein: emerging intel-microcode" &&
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
            make defconfig &&
		make menuconfig
        else
            $Wget "$KernelConfig" -O /usr/src/linux/.config &&
		make menuconfig
        fi
    else
        echo "gein: Error: AutoKernel isn't true or false. Exiting..."
    fi

    echo "gein: Compiling Linux kernel, modules, and initramfs..." &&
	$Make && $Make modules &&
	$Make install && $Make modules install &&
	$Make distclean &&
	cd /

    echo "gein: Adding services to OpenRC..." &&
	rc-update add dhcpcd default &&
	rc-update add cronie default

    echo "gein: Setting hostname..." &&
	echo "hostname=$Hostname" > /etc/conf.d/hostname

    echo "gein: Installing Grub to $PartitionBoot..." &&
	grub-install "$PartitionBoot" &&
	grub-mkconfig -o /boot/grub/grub.cfg

    echo "gein: Adding userland configurations..."
    CfgFiles="
        /etc/bash/bashrc
        /etc/profile
        /etc/profile.d/alias.sh
        /etc/profile.d/defaults.sh
        /etc/profile.d/gein.sh
        /etc/profile.d/golang.sh
        /etc/profile.d/kernel.sh
        /etc/profile.d/racket.sh
    "
    for cfg in $CfgFiles; do
        $Wget "$Source"/"$cfg" -O "$cfg"
    done

    echo "gein: Setting root password..."
    [ -x $(command -v chpasswd) ] && \
        echo root:"$Hostname" | chpasswd
}


# In this section we will install the chosen xorg-driver and packages
# for i3wm as the desktop of choice. Some additional packages are added
# as a convenience.

DESKTOP() {
    echo "gein: Installing Xorg drivers..." &&
	$Emerge x11-base/xorg-drivers &&
	env-update && source /etc/profile &&
	export PS1="[chroot \u@\h \W]$ "

    echo "gein: Installing desktop packages..." &&
	$Emerge @gein-base "$DesktopChoice"

    if [ -n "$DesktopConfig" ]; then
        echo "gein: Adding configuration files..."
        for cfg in "$DesktopConfig"; do
            $Wget "$Source"/"$cfg" -O "$cfg"
        done
    fi
}

LAPTOP() {
    echo "gein: Installing laptop packages..." &&
	$Emerge @gein-laptop

    echo "azryn: Add laptop_mode to OpenRC..." &&
	rc-update add laptop_mode default
}


# This section is for completing tasks after the installation is
# complete. The user will have a complete system already installed and
# may skip these steps if desired.

POSTINSTALL() {
    # echo "gein: Creating 'power' group"
    # groupadd power
    #   poweroff reboot shutdown

    read -ep "gein: Install laptop packages? [Y/N]: " SetupUser
    if echo "$SetupUser" | grep -iq "^y"; then
        $Emerge @gein-laptop
    fi

    read -ep "gein: Setup a standard user? [Y/N]: " SetupUser
    if echo "$SetupUser" | grep -iq "^y"; then
	echo "gein: Creating user account" &&
	    read -ep "Username: " Username &&
            read -ep "Password: " Password &&
            useradd -m -G wheel,audio,video,power \
		    -s /bin/bash "$Username" &&
            echo "$Username":"$Password" | chpasswd
    fi

    echo "gein: Installation complete."
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
        MINIMAL && POSTINSTALL
        ;;

    -d|desktop)
        case $2 in
            i3wm)
                DesktopChoice="@gein-i3wm"
                DesktopConfig="
                    /etc/i3status.conf
                    /etc/xinitrc
                    /etc/Xresources
                    /etc/i3/config
                "
                MINIMAL && DESKTOP && POSTINSTALL
                ;;

            lxqt)
                DesktopChoice="@gein-lxqt"
                MINIMAL && DESKTOP

                echo "azryn: Set SDDM as the display manager" &&
                    sed -i 's/DISPLAYMANAGER="xdm"/DISPLAYMANAGER="sddm"/g' \
			/etc/conf.d/xdm &&
                    sed -i 's/startl|xqt/"ck-launch-session dbus-launch startlxqt"/g' \
			/usr/share/xsessions/lxqt.desktop &&
                    rc-update add xdm default &&
                    rc-update add dbus default

                POSTINSTALL
                ;;

            *)
                echo "gein: $2 not an available desktop"
                echo ""
                echo "Available desktops:"
                echo "  i3wm             A complete i3wm desktop"
                echo "  lxqt             A complete LXQT desktop"
        esac
        ;;

    *)
        echo "gein: Linux-based derivative of Gentoo"
        echo "  -h, help         Shows this output"
        echo "  -b, bootstrap    Bootstrap the stage3 tarball"
        echo ""
        echo "Post-bootstrap:"
        echo "  -m, minimal      Perform a basic Gentoo installation"
        echo "  -d, desktop      Install a gein desktop"
        echo "    i3wm           A complete i3wm desktop"
        echo "    lxqt           A complete LXQT desktop"
esac
shopt -u nocasematch
