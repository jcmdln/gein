# /etc/profile.d/kbuild.sh

kbuild() {
    if [ $EUID -ne 0 ]; then
        if [ -e $(command -v sudo) ]; then
            SU="sudo"
        else
            SU="su -c"
        fi
    fi

    KMake="make -s -j$(grep -c ^processor /proc/cpuinfo)"
    cd /usr/src/linux &&
        $SU make menuconfig &&
        $SU $KMake         &&
        $SU $KMake modules &&
        $SU $KMake install &&
        $SU $KMake modules install &&
        $SU grub-mkconfig -o /boot/grub/grub.cfg &&
        $SU emerge -av --quiet-build @module-rebuild

    unset KMake
}
