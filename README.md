gein
===================
**GEntoo INstaller**

[![star this repo](http://githubbadges.com/star.svg?user=jcmdln&repo=gein&style=flat)](https://github.com/jcmdln/gein)
[![fork this repo](http://githubbadges.com/fork.svg?user=jcmdln&repo=gein&style=flat)](https://github.com/jcmdln/gein/fork)

`gein` is a Gentoo Installation Framework in the form of a repository
and can be used as a model for you to automate your ideal Gentoo
Installation.  This repository contains the following:

If you discover issues with any file in this repository, please file an
issue or submit a pull request.  While I mainly created this repository
to simplify installing my own flavor of Gentoo, I would like to remain
somewhat agnostic when possible.


Warnings
-------------------

* `gein` is not a Gentoo-based Linux distribution. This is simply a set
of files that augment the Stage3 tarball and a script to orchestrate
the installation of Gentoo.
* `gein` is not a panacea. If you do not review the files in this
repository then chances are that you will experience behavior that you
might not prefer.  Be sure to review this repository in it's entirety,
including the warnings at the top of `gein.sh`.
* `gein` will not partition and mount your disks. You must do so with the
tools provided by your installation media, whether that be the Gentoo
minimal install CD or some graphical LiveCD, before running `gein.sh`.


FAQ
-------------------

### "How much about Gentoo will I need to know to use `gein`?"
* Very little to get Gentoo installed, but you'll need to understand
how Gentoo and Portage work to maintain the system. As long as you
can follow this Readme, then you can use `gein` without much thought.
* Because I have configured Portage for my own needs, this may cause
pain later on, such as when attempting to install a package that I
haven't defined USE flags for. If you aren't familiar with Portage
then you will gradually find yourself further and further at odds
with the system unless you read the Gentoo Handbook for your system.

### "I keep getting boot failures after installing in a VirtualBox VM?"
* Remove the virtual disk drive from the boot order and restart.

### "The display is lagging when using LXQT on my Nvidia GPU?"
* Run `eselect opengl set nvidia` as root.


Usage
-------------------
1. Prepare a disk or USB drive

    `gein` should work from almost any live installation media, though
    Fedora Workstation or Ubuntu Desktop will most likely be best if
    you wish to continue using the system while you wait for the
    installation to complete.

2. Partition and mount your disks.

    Note that this example assumes that you will be using a single
    partition and later installing GRUB to the MBR. If you aren't using
    the Gentoo installation CD, you will need to run `mkdir /mnt/gentoo`
    before performing these steps:

    ```sh
    $ fdisk /dev/sdX
    $ mkfs.xfs /dev/sdX
    $ mount /dev/sdX1 /mnt/gentoo
    ```

3. Download `gein.sh`:

    You should be able to run the script from anywhere, but in general
    it may be best to `sudo -i` and `cd ~/` so that you are a
    privileged user to begin with.

    ```sh
    $ wget https://raw.githubusercontent.com/jcmdln/gein/master/gein.sh
    $ sh gein.sh
    gein: Gentoo minimal installation script
    -b. --bootstrap    Bootstrap the stage3 tarball
    -i, --install      Install Gentoo
    ```

4. Modify `gein.sh`

    You will now need to modify `gein.sh`. In order to proceed with the
    installation, you must have the variable `GEIN_PARTITION_BOOT` set
    or `gein.sh` will complain at you.

    ```sh
    GEIN_PARTITION_BOOT="/dev/sda"
    ```

    Be sure to read the warning messages in `gein.sh` if you haven't
    already.

5. Bootstrap the system

    The script will download the latest Stage3 archive for the detected
    architecture and do some housekeeping.

    ```sh
    $ sh ./gein.sh --bootstrap
    ```

    Once the boostrap process finishes, you will be in a chroot within
    `/mnt/gentoo` where you may proceed to the next step.

6. Install the desired target.

    ```sh
    $ sh ./gein.sh install
    ```

7. Wait

    If you used a LiveCD then kill some time while you wait. You won't
    be prompted until after the installation has completed.  These
    final prompts are for setting the root password and optionally
    creating a user.


    The installation itself takes quite some time, with GCC taking the
    longest to compile at about 1 hour and 30 minutes on a 2013 laptop.

8. Enjoy your new Gentoo installation!


Customizing
-------------------
If you would like to create your own ideal Gentoo system using this
script, you may do so by:

1. Forking https://github.com/jcmdln/gein
2. Update the `GEIN_CONFIG_URL` URL in `gein.sh`
3. Modify the configuration files as desired
4. Update the `BOOTSTRAP()` function to represent your configuration


Contributing
-------------------
I'm very interested in feedback regarding the contents of this repo. If
you have any feedback, suggestions, or contributions please file an
issue or pull request.
