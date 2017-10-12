# gein - GEntoo INstaller

```
$ sh gein
gein - GEntoo INstaller

  -h help       Suggests to read the script and Gentoo documentation

Required first step:
  -b bootstrap  Bootstrap the stage3 tarball

Post-bootstrap options:
  -m minimal    Only install required gentoo packages
  -d desktop    Install required and desktop packages
  -l laptop     Install required, desktop, and laptop packages
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
# Once this command completes, you should be in your new chroot
sh /mnt/gentoo/gein -b
```

5) From the new chroot, install your desired version
```
# Minimal install (only bare minimum packages)
sh gein -m

# Desktop install (a full desktop with baked-in configs)
sh gein -d

# Laptop install (add power monitoring, wifi)
sh gein -l
```
