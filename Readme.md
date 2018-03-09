`gein` is a script which installs Gentoo Linux. The primary goal is to
automate the installation of my own ideal Gentoo setup though the
current design allows for others to fork or submit PR's should it be
desired. Keep in mind that this project is NOT a replacement for reading
the Gentoo Handbook.

You may review the source code at https://github.com/jcmdln/gein for
more information if needed.


## FAQ
- "How much about Gentoo will I need to know to use gein?"
  - I would suggest reading the Gentoo Handbook and every file in this
    project before proceeding.
- "I keep getting boot failures after installing in a VirtualBox VM?"
  - Remove the virtual disk drive from the boot order and restart.
- "The display is lagging when using LXQT on my Nvidia GPU?"
  - Run `eselect opengl set nvidia` as root.


## Warnings
- Read the warnings at the top of `gein.sh` FIRST!
- You must manually partition and mount your disks.
  - Consult your Gentoo Handbook.
- gein does not install a web browser or office suite.


## Getting Started
1. Download a Gentoo LiveCD from https://www.gentoo.org/downloads/

2. Write the ISO to a USB drive:

        $ dd if=~/Downloads/<iso> of=/dev/<usb>

3. Boot from the USB drive on the target device


## Installing
1. Partition and mount your disk(s)
2. Download and run `gein.sh`:

        $ wget http://gein.jcmdln.net/gein.sh
        $ sh gein
        gein: Linux-based derivative of Gentoo
          -h help         Shows this output
          -b bootstrap    Bootstrap the stage3 tarball

        Post-bootstrap:
          -m minimal      Perform a basic Gentoo installation
          -d desktop      Install a complete gein desktop

3. Read the information at the top of `install.sh` and modify the
variables as needed:

        $ vi install.sh

4. Start the bootstrap:

        $ sh install.sh bootstrap

5. Install your desired variant:

        $ sh install.sh desktop

Enjoy your Gentoo installation!
