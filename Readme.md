`gein` (GEntoo INstaller) is a script for automating the installation of
my ideal Gentoo Linux system. This repository contains the following:

* `gein.sh`, the installation script
* `etc/`, configuration files
* `usr/`, kernel configurations


## Warnings

* Using `gein` is not a replacement for reading the Gentoo Handbook. If
  anything is not understood, please consult the Gentoo Handbook for
  your system before submitting an issue.
* Be sure to review this repository in it's entirety, including the
  warnings at the top of `gein.sh`.
* You must manually partition your disks and mount the partitions.
* No web browser, office suite, or other applications are included by
  default.


## FAQ

### "How much about Gentoo will I need to know to use gein?"
I would suggest reading the Gentoo Handbook and every file in this
project before proceeding.

### "I keep getting boot failures after installing in a VirtualBox VM?"
Remove the virtual disk drive from the boot order and restart.

### "The display is lagging when using LXQT on my Nvidia GPU?"
Run `eselect opengl set nvidia` as root.


## Installing

You may use this script as-is by performing the following steps:

1. Download the latest Gentoo install cd for your architecture
Note: only amd64 is currently defined. Create an issue with the output
of `uname -a` for it to be added to the script.

2. Write the ISO to a disk or USB drive and boot from it.

    $ dd if=~/Downloads/install-*.iso of=/dev/sdX
    $ reboot

3. Partition and mount your disks
Note: this example assumes that you will be using a single partition and
later installing GRUB to the MBR.

    $ fdisk /dev/sdX
    $ mount /dev/sdX1 /mnt/gentoo

4. Download and run `gein.sh`

    $ wget https://raw.githubusercontent.com/jcmdln/gein/master/gein.sh
    $ sh ./gein.sh
    gein: Linux-based derivative of Gentoo
      -h, help         Shows this output
      -b, bootstrap    Bootstrap the stage3 tarball

    Post-bootstrap:
      -m, minimal      Perform a basic Gentoo installation
      -d, desktop      Install a gein desktop
        i3wm           A complete i3wm desktop
        lxqt           A complete LXQT desktop

5. Uncomment and set the `PartitionBoot` & `VideoCards` variables
Both MUST be set or `gein.sh` will exit. If you don't need any video
support, then set 'VideoCards' to false.

    PartitionBoot="/dev/sda"
    ...
    VideoCards="i915 i965 intel"

6. Start the bootstrap

    $ sh ./gein.sh bootstrap

Upon completion, the script will chroot into `/mnt/gentoo` so we may
proceed to the next step.

7. Install the desired target

    $ sh ./gein.sh desktop i3wm

8. Wait about 1 to 4 hours
You will be prompted as few times as possible, and as close to the
beginning or end of the process. You will be prompted to:
  * Choose your Portage profile (Hint: the one with `*` at the end)
    * You could choose another profile, which is why I don't set it
      automatically. I only test the latest stable profile, which at the
      time of writing is:

        [12]  default/linux/amd64/17.0 (stable) *

  * If performing a desktop install, you will be asked if you would like
    to install desktop packages.
  * Set your `root` password
  * Setup a standard user


## Customizing

If you would like to create your own ideal Gentoo system using this
script, you may do so by:

1. Forking https://github.com/jcmdln/gein
2. Modify the configuration files as desired
3. Update the `Source` URL in `gein.sh`
4. Update the `CONFIG()` function to represent your configuration
