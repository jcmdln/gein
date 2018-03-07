# /etc/profile.d/kbuild.sh

kbuild() {
    KMake="make -s -j$(grep -c ^processor /proc/cpuinfo)"
    cd /usr/src/linux

    make menuconfig &&
	$KMake         && $KMake modules &&
	$KMake install && $KMake modules install &&
	grub-mkconfig -o /boot/grub/grub.cfg &&
	emerge -av --quiet-build @module-rebuild

    unset KMake
}
