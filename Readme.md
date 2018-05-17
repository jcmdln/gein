`gein` is a Gentoo Installation Framework in the form of a repository
and can be used as a model for you to automate your ideal Gentoo
Installation. This repository contains the following:

* `gein.sh`
  * The installation script
  * This is the only script that performs any installation tasks.
* `etc/`
  * Configuration files
  * I've placed my own personal configurations here to suit my needs,
    though feel free to use, replace, or remove them.
* `usr/local/gein`
  * This folder contains some helpful scripts
  * `gpkg`
    * emerge front-end for managing packages
    * Not needed at all. Think of this as a set of aliases in the form
      of a command.
  * `kbuild`
    * Helps with rebuilding the kernel after downloading new source,
      though basically is a wrapper for switching to the directory
      and other menial tasks.
* `usr/src`
  * Kernel configurations
  * These are meant to be somewhat bloated so that they work on most
    systems, though I do my best to stay up to date with security
    releases and optimizations.

If you discover issues with any file in this repository, please do file
an issue or submit a pull request. While I mainly created this
repository to simplify installing my own flavor of Gentoo, I would like
to remain somewhat agnostic when possible.


## Warnings

* `gein` is not a Gentoo-based Linux distribution. This is simply a set
  of files that augment the Stage3 tarball and a script to orchestrate
  the installation of Gentoo.
* `gein` is not a panacea. If you do not review the files in this
  repository then chances are that you will experience behavior that you
  might not prefer. Be sure to review this repository in it's entirety,
  including the warnings at the top of `gein.sh`.
* `gein` will not partition and mount your disks. You must do so with the
  tools provided by your installation media, whether that be the Gentoo
  minimal install CD or some graphical LiveCD, before running `gein.sh`.
* `gein` is not intended to provide an opinionated package set by
  default. I have attempted to keep `/etc/portage/sets` as minimalist as
  possible though I do have a number of packages that suit my own
  tastes.


## FAQ

* "How much about Gentoo will I need to know to use `gein`?"
  * Very little to get Gentoo installed, but you'll need to understand
    how Gentoo and Portage work to maintain the system.
  * As long as you can follow this Readme, then you can use `gein`
    without much thought to install Gentoo. Because I have configured
    Portage for my own needs, this may cause pain later on, such as when
    attempting to install a package that I haven't defined USE flags for.
    If you aren't familiar with Portage then you will gradually find
    yourself further an further at odds with the system unless you read
    the Gentoo Handbook for your system.
* "I keep getting boot failures after installing in a VirtualBox VM?"
  * Remove the virtual disk drive from the boot order and restart.
* "The display is lagging when using LXQT on my Nvidia GPU?"
  * Run `eselect opengl set nvidia` as root.


## Installing

You may use this script as-is by performing the following steps:

1. Download the Gentoo minimal install CD for your architecture. `gein`
is able to run from a graphical LiveCD if you would prefer to entertain
yourself while installing Gentoo.

2. Write the ISO to a disk or USB drive, reboot, and boot from it.

        $ dd if=~/Downloads/install-*.iso of=/dev/sdX
        $ reboot

3. Partition and mount your disks. Note that this example assumes that
you will be using a single partition and later installing GRUB to the
MBR. If you aren't using the Gentoo installation CD, you will need to
run `mkdir /mnt/gentoo` before performing these steps.

        $ fdisk /dev/sdX
        $ mkfs.ext4 /dev/sdX
        $ mount /dev/sdX1 /mnt/gentoo

4. Download and run `gein.sh`:

        $ wget https://gein.io/gein.sh
        $ sh gein.sh
        gein: Linux-based derivative of Gentoo
          bootstrap    Bootstrap the stage3 tarball

        Post-bootstrap:
          minimal      Headless installation
          desktop      Desktop installation

5. You will now need to modify `gein.sh`. In order to proceed with the
installation, you must have the variables`PartitionBoot` and `VideoCards`
set or `gein.sh` will exit. If you don't need any video support, then set
`VideoCards` to false. Below is an example of a minimal (X-less) install,
though examples are provided within `gein.sh`.

        PartitionBoot="/dev/sda"
        VideoCards="false"

6. Now that we've satisfied the conditions I coded into `gein.sh`, we may
now start the bootstrap process. The script will download the latest
Stage3 archive for the detected architecture and do some housekeeping.
The boostrap process finishes, which lastly will chroot into
`/mnt/gentoo` so we may proceed to the next step.

        $ sh ./gein.sh bootstrap

7. Install the desired target.

        $ sh ./gein.sh minimal

8. The installation itself takes quite some time, with GCC taking the
longest to compile at about 1 hour and 30 minutes on a 2013 laptop. If
you decided to use a LiveCD then kill some time while you wait. You won't
be prompted until after the installation has completed, and these final
prompts are for installing laptop packages, setting the root password,
and creating a user account.

9. Enjoy your new Gentoo installation!


## Customizing

If you would like to create your own ideal Gentoo system using this
script, you may do so by:

1. Forking https://github.com/jcmdln/gein
2. Modify the configuration files as desired
3. Update the `Source` URL in `gein.sh`
4. Update the `CONFIG()` function to represent your configuration
