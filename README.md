## Installing Gentoo using `gein`
The installation process is almost entirely automated, though will
require user input for many sections to simplify the script as well as
make it more versatile.

### Note: There has been a rather drastic rewrite and this file has not been updated to reflect this.

0) Read the entire script, I may do things you won't like or want.
1) Download and boot the Gentoo livecd of your choosing.
2) Configure and mount your partitions.
3) Download `gein` into `/mnt/gentoo`.
```
cd /mnt/gentoo
wget https://raw.githubusercontent.com/jcmdln/gein/master/gein
```

4) Run `gein`.
```
# Initialize/Bootstrap the Gentoo installation.
sh /mnt/gentoo/gein -i

# From the new chroot, install the Gentoo base.
sh gein -b
```

From here you can either reboot into the base system, or install
additional utilities via the following options.

```
# Install Desktop packages
sh gein -d

# Install Laptop packages
sh gein -l

# Install Server packages
sh gein -s

# If you want to trim the total size of the installation, you may run
# the following to "safely" remove non-vital cruft.
sh gein -c
```

## Caveats
- I've automated the creation of a 2G swapfile during the installation
  process, so remove this file when the installation is complete and 
  update `/etc/fstab` to use your desired swap partition or simply 
  don't make a swap partition.
- I'm lazy and haven't fully automated installing Grub, so if you're 
  using UEFI then read the script and adjust `Boot_Grub()` to your
  liking.
- The total size of this installation, even after running `gein -c`, 
  is still rather large at roughly 1.5gb or so.
- There may be other lingering issues, please report them if possible.
