# /etc/profile.d/kernel.sh

KMake="make -j$(grep -c ^processor /proc/cpuinfo)"

kernel-rebuild() {
    printf "\n\n#####\n### Building Kernel\n#####\n\n"
    $KMake &&

    printf "\n\n#####\n### Building Kernel Modules\n#####\n\n"
    $KMake modules &&

    printf "\n\n#####\n### Installing Kernel\n#####\n\n"
    $KMake install &&

    printf "\n\n#####\n### Installing Kernel Modules\n#####\n\n"
    $KMake modules install &&

    printf "\n\n#####\n### Generating Grub Config\n#####\n\n"
    grub-mkconfig -o /boot/grub/grub.cfg &&

    printf "\n\n#####\n### Rebuilding Emerged Kernel Modules\n#####\n\n"
    emerge -aq @module-rebuild
}

unset KMake
