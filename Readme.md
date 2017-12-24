AzrynOS is a Linux distribution that uses Gentoo to provide an i3wm desktop.
You may install a basic Gentoo system and proceed to installing the complete
version of AzrynOS if desired:

```
# Run the following from a Gentoo LiveCD
$ wget -q http://os.aswl.org -O /mnt/gentoo/azryn
$ sh azryn
AzrynOS - i3wm powered Gentoo derivative
  help         Shows help output

Run in the order listed:
  bootstrap    Bootstrap the stage3 tarball
  minimal      Very basic Gentoo install
  complete     Full AzrynOS installation
  cleanup      Remove junk created during install
```


## Warnings
- AzrynOS is not production-ready, proceed with caution.
- This installation relies on the Gentoo Live CD
  - In the future, there will be a custom Live CD and/or USB image.
- There is no automatic partitioning process, you will need to manually
  partition your disks and mounting them before proceeding.
  - Later this will be slightly improved by using partition labels.
- Read the entire script and thumb through the configuration files.
  - Some environment variables at the top of the script will need to be
    modified. Read the Gentoo Handbook for your intended architecture if
    anything is unclear.


## FAQ
- How much about Gentoo will I need to know to use AzrynOS?
  - If you have a little bit of Linux experience and can read shell scripts
    along with documentation it should be fairly straightforward.
- How long does this take to install?
  - About 1-4 hours for the minimal install depending on the hardware it's
    being installed to, and expect to spend 4-24 hours for the complete
    installation.
- I keep getting boot failures after installing in a VirtualBox VM?
  - Yea... You have to remove the virtual optical drive from the boot order
    manually in the settings of your VM. Afterwards it should work without
    issue.
