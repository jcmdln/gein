## Installing Gentoo using `gein`
The installation process is almost entirely automated, though will
require user input for many sections to simplify the script as well as
make it more versatile.

```
Available options:
  bootstrap (-b), minimal (-m), desktop (-d), laptop (-l), server (-s), virtual (-v)
```

0) Read the entire script, I may do things you won't like or want.
1) Download and boot the Gentoo livecd of your choosing.
2) Configure and mount your partitions.
3) Download `gein` into `/mnt/gentoo`.
```
cd /mnt/gentoo
wget https://raw.githubusercontent.com/jcmdln/gein/master/gein
```

4) Bootstrap the Gentoo installation.
```
sh /mnt/gentoo/gein -b

# Once this command completes, you should be in your new chroot
```

5) From the new chroot, install your desired version
```
# Minimal install (only bare minimum packages)
sh gein -m

# Desktop install (a full desktop with baked-in configs)
sh gein -d

# Laptop install (add power monitoring, wifi)
sh gein -d

# Server install (WIP)
sh gein -s

# Virtual install (minimal install with support for qemu/vbox/vmware)
sh gein -v
```
