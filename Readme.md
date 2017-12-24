`AzrynOS` is a Linux-based distribution using Gentoo and i3wm. The
primary goal is to automate the installation of my own ideal Gentoo
setup though the current design allows for others to fork or submit PR's
should it be desired. Keep in mind that this project is NOT a
replacement for reading the Gentoo Handbook. Sabayon tries to do this
and I don't want to repeat this line of thinking.

I'm aware that Gentoo is a highly personal system and much of what I've
added may be undesired. Despite this I think that this project may serve
as a subjectively decent example of getting started with Gentoo.

The installation process is handled in four steps as shown in the output
of the `azryn` script:
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

If you want to fork this script or start with a basic Gentoo
installation, simply stop after running the `minimal` pass and do as you
wish. The `azryn` script is meant to be modular and allow configuration,
so if you notice any issues please feel free to submit a PR.


## Warnings
- AzrynOS is not production-ready, proceed with caution.
- You must manually partition and mount your disks.
  - Consult your Gentoo Handbook.


## FAQ
- "How much about Gentoo will I need to know to use AzrynOS?"
  - I would suggest reading the Gentoo Handbook and every file in this
    project before proceeding.
- "How long does this take to install?"
  - About 1 to 4 hours for the minimal install.
    - Most time-consuming packages are GCC 6.4 and Linux.
  - About 4 to 24 hours for the complete installation.
    - Most time-consuming packages are Mesa and Chromium.
- "I keep getting boot failures after installing in a VirtualBox VM?"
  - Remove the virtual disk drive from the boot order and restart.
