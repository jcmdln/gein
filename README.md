`gein` is a collection of files driven by a shell script that (somewhat)
automates installing Gentoo.


Warnings
-------------------
* `gein` is not a panacea. If you don't review all of the files in this
repository then you will experience unexpected behavior.
* `gein` will not partition and mount your disks. You must do so with the tools
provided by your installation media, whether that be the Gentoo minimal install
CD or some graphical LiveCD, before running `gein.sh`.
* Because I have configured Portage for my own needs, this may cause
pain later on, such as when attempting to install a package that I
haven't defined USE flags for. If you aren't familiar with Portage
then you will gradually find yourself further and further at odds
with the system unless you read the Gentoo Handbook.


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
    $ fdisk /dev/sda
    $ mkfs.xfs /dev/sda1
    $ mount /dev/sda1 /mnt/gentoo
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

4. Bootstrap the system

    Now we can bootstrap the system:

    ```sh
    $ GEIN_PARTITION_BOOT="/dev/sda" sh ./gein.sh --bootstrap
    ```

    Once the boostrap process finishes, you will be in a chroot within
    `/mnt/gentoo` where you may proceed to the next step. Your new chroot has
    all the `GEIN_*` environment variables as you defined them.

5. Install the desired target

    ```sh
    $ sh ./gein.sh --install
    ```

    At the start of the installation you'll be prompted to create a root
    password and pick your profile.

6. Wait

    If you used a LiveCD then kill some time while you wait. Enjoy your new
    Gentoo installation!


Contributing
-------------------
I'm very interested in feedback regarding the contents of this repo. If
you have any feedback, suggestions, or contributions please file an
issue or pull request.
