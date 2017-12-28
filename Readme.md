`AzrynOS` is a Linux-based derivative of Gentoo. The primary goal is to
automate the installation of my own ideal Gentoo setup though the
current design allows for others to fork or submit PR's should it be
desired. Keep in mind that this project is NOT a replacement for reading
the Gentoo Handbook. Sabayon tries to do this and I don't want to repeat
this line of thinking.

I'm aware that Gentoo is a highly personal system and much of what I've
added may be undesired. Despite this I think that this project may serve
as a subjectively decent example of getting started with Gentoo.


## FAQ
- "How much about Gentoo will I need to know to use AzrynOS?"
  - I would suggest reading the Gentoo Handbook and every file in this
    project before proceeding.
- "How long does this take to install?"
  - About 1 to 4 hours for the minimal install.
  - About 4 to 24 hours for the complete installation.
- "I keep getting boot failures after installing in a VirtualBox VM?"
  - Remove the virtual disk drive from the boot order and restart.
- "The display is lagging when using LXQT on my Nvidia GPU?"
  - Run `eselect opengl set nvidia` as root.


## Warnings
- AzrynOS is not production-ready, proceed with caution.
- You must manually partition and mount your disks.
  - Consult your Gentoo Handbook.
- Read the warnings at the top of `install.sh` FIRST!
  - Your root password is set to the value of $Hostname for simplicity.
    Change your root password with 'passwd' after the MINIMAL install.
  - Be sure to uncomment the appropriate `VIDEO_CARDS` line and modify
    as needed
  - By default, $AutoKernel is set to 'true' which means that the kernel
    will be built using 'make defconfig'. If you want to run
    'make defconfig; make menuconfig' then set $AutoKernel to 'false'.
    You may also supply your own URL to $KernelConfig while setting
    $AutoKernel to 'false' to use a pre-built kernel config. And example
    kernel config is provided though commented out.


## Getting Started
1. Download a Gentoo LiveCD from https://www.gentoo.org/downloads/
2. Write the ISO to a USB drive:
```
# THIS IS AN EXAMPLE!
$ dd if=~/Downloads/<iso> of=/dev/<usb>
```
3. Boot from the USB drive on the target device


## Installing
1. Partition and mount your disk(s):
```
# THIS IS AN EXAMPLE!
# Make a single partition
$ fdisk /dev/sda

# Enter these in the fdisk prompt:
o
n
enter
enter
enter
w

# Partition as EXT4
$ mkfs.ext4 /dev/sda1

# Mount to /mnt/gentoo
$ mount /dev/sda1 /mnt/gentoo
```
2. Download and run `install.sh`:
```
$ wget http://os.aswl.org/install.sh
$ sh install.sh
AzrynOS: Linux-based derivative of Gentoo
  help         Shows help output

Pre-install tasks:
  bootstrap    Bootstrap the stage3 tarball

Installation options:
  minimal      Install minimal Gentoo
  i3wm         Install Gentoo and i3wm desktop
  lxqt         Install Gentoo and LXQT desktop

Post-install tasks:
  cleanup      Remove junk created during install
```
3. Read the information at the top of `install.sh` and modify the
variables:
```
$ vi install.sh
# or
$ nano install.sh
```
4. Start the bootstrap:
```$ sh install.sh bootstrap```
5. Install your desired variant:
```$ sh install lxqt```
6. (Optional) Cleanup:
```$ sh install.sh cleanup```

Enjoy your AzrynOS installation!


## Post-install

### Adding an administrative user
```
$ useradd -m -g wheel -G audio,network,video -s /bin/bash username
$ passwd username
```

### Administering the system
`azryn` is meant to act as a simplification script for performing
administrative tasks such as package and configuration management:
```
$ azryn
Available options:
  cleanup,  -c    Remove unneeded packages
  install,  -i    Install a package
  reconfig, -R    Get latest configuration files
  remove,   -r    Safely remove a package
  sync,     -s    Sync portage
  update,   -u    Update @world without rebuild
  upgrade,  -U    Update @system and @world with rebuild
```

### Installing a Web Browser
AzrynOS does not ship with a web browser unless you count w3m or eww
within Emacs. I would suggest using either Firefox or Chromium though
these packages take some time to compile on older hardware. You may
resort to installing google-chrome which is a binary package if desired.

### Installing an Office Suite
Maybe install the libreoffice-bin binary? Depends on your needs.
