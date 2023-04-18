# void-pi

Void Linux installer implemented in GNU Prolog.

Last tested | ISO                                                                                | Result
----------- | ---------------------------------------------------------------------------------- | ------
2023-03-02  | [void-live-x86_64-20221001-base.iso](https://repo-default.voidlinux.org/live/current/void-live-x86_64-20221001-base.iso) | PASS
2023-03-02  | [void-live-x86_64-musl-20221001-base.iso](https://repo-default.voidlinux.org/live/current/void-live-x86_64-musl-20221001-base.iso) | PASS
2023-03-02  | [void-live-i686-20221001-base.iso](https://repo-default.voidlinux.org/live/current/void-live-i686-20221001-base.iso) | N/A
2023-03-03  | [void-live-i686-20210930.iso](https://repo-default.voidlinux.org/live/20210930/void-live-i686-20210930.iso) | PASS

## Description

### Overview

void-pi is a Void Linux installer similar to [void-installer](https://docs.voidlinux.org/installation/live-images/guide.html).

It extends void-installer in several ways:
- provides predefined templates for [BTRFS](https://en.wikipedia.org/wiki/Btrfs), [LVM](https://en.wikipedia.org/wiki/Logical_volume_management), and [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup).
- supports [rEFInd](https://rodsbooks.com/refind/) and [Limine](https://limine-bootloader.org/) boot managers

void-pi works on Void with Intel or AMD x86 CPU. It wasn't tested with ARM CPUs.

### Features

- completely emulates void-installer.
- install from Void ISO or network.
- predefined templates for BTRFS, LVM, and LUKS.
- TUI dynamically changes depending on selected template.
- uses `/mnt` for chroot by default.
- allows to use an alternative rootdir via `--rootdir` command line argument.
- all settings can be saved in a file and loaded on startup. File name is controlled via `--config` command line argument.
- passwords are never saved in files even temporarily.
- Bootloaders
    - uses GRUB, rEFIind, or Limine as a bootloader.
    - GRUB supports btrfs, ext2, ext3, ext4, and xfs file systems.
    - rEFIind supports btrfs, ext2, ext3, and ext4 file systems.
    - rEFIind is configured to use kernel auto detection.
    - Limine supports ext2, ext3, and ext4 file systems.
    - if a bootloader doesn't support a file system, then installer will create an ext4 `/boot` partition.
- LUKS
    - LUKS can be used with GRUB.
    - whole system on LUKS, including encrypted `/boot`

### Default settings

All default settings can be changed via `Common Attrs` sub-menu.

- installation source: local
- host name: voidpp
- user name: void
- keymap: us
- locale: en_US.UTF-8
- timezone: America/New_York
- LUKS mapping name: cryptroot
- MBR size: 2M
- ESP size: [550M][550M]
- Boot partition size: 1G

### Filesystem

#### BIOS boot

- `/dev/sdX1` is the BIOS boot sector (size: 2M)
- `/dev/sdX2` is the EFI system partition (size: [550M][550M])
- `/dev/sdX3` is the root filesystem (size: remainder)

#### UEFI

- `/dev/sdX1` is the EFI system partition (size: [550M][550M])
- `/dev/sdX2` is the root filesystem (size: remainder)

#### BTRFS layout

void-pi creates the following Btrfs subvolumes with a [flat layout][flat layout]:

Subvolume name    | Mounting point    | Mount options
---               | ---               | ---
`@`               | `/`               |
`@home`           | `/home`           | `nodev,nosuid`
`@opt`            | `/opt`            | `nodev`
`@srv`            | `/srv`            | `nodev,noexec,nosuid` + [nodatacow][nodatacow]
`@var`            | `/var`            | `nodev,noexec,nosuid`
`@var-cache-xbps` | `/var/cache/xbps` | `nodev,noexec,nosuid`
`@var-lib-ex`     | `/var/lib/ex`     | `nodev,noexec,nosuid` + nodatacow
`@var-log`        | `/var/log`        | `nodev,noexec,nosuid` + nodatacow
`@var-opt`        | `/var/opt`        | `nodev,noexec,nosuid`
`@var-spool`      | `/var/spool`      | `nodev,noexec,nosuid` + nodatacow
`@var-tmp`        | `/var/tmp`        | `nodev,noexec,nosuid` + nodatacow
`@snapshots`      | `/.snapshots`     | `nodev,noexec,nosuid` + nodatacow

#### F2FS options

- `extra_attr,inode_checksum,sb_checksum,compression,encrypt`

## How to run

### Run precompiled executable.
```sh
sudo xbps-install -Suy xbps wget
wget https://github.com/sdbtools/void-pi/releases/latest/download/void-pi.x86_64.tgz
tar -xzf void-pi.x86_64.tgz
./void-pi
```

### Run as script.
```sh
sudo xbps-install gprolog git
git clone https://github.com/sdbtools/void-pi.git
cd void-pi
./void-pi.pl
# or
gprolog --consult-file void-pi.pl
```

### Compile Prolog code locally and run it.

```sh
sudo xbps-install gprolog gcc git
git clone https://github.com/sdbtools/void-pi.git
cd void-pi
gplc --min-size void-pi.pl
./void-pi
```
## Dependencies

Name      | Provides                | Included in Void ISO?
---       | ---                     | ---
bash      | `set -o pipefail` | Y
dialog    | ncurses user input menu | Y
gptfdisk  | GPT disk partitioning with sgdisk | N
lz4       | Extremely Fast Compression algorithm | N

## Licensing

This software is released under the GNU GPLv2 license.

## Credits

- [Void Linux](https://voidlinux.org/)
- [Voidvault](https://github.com/atweiden/voidvault)
 
[550M]: https://wiki.archlinux.org/index.php/EFI_system_partition#Create_the_partition
[flat layout]: https://btrfs.wiki.kernel.org/index.php/SysadminGuide#Layout
[nodatacow]: https://wiki.archlinux.org/index.php/Btrfs#Disabling_CoW

