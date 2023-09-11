# void-pi

Void Linux installer implemented in GNU Prolog.

Last tested | ISO                                                                                | Result
----------- | ---------------------------------------------------------------------------------- | ------
2023-08-23  | [void-live-x86_64-20221001-base.iso](https://repo-default.voidlinux.org/live/current/void-live-x86_64-20221001-base.iso) | PASS
2023-06-29  | [void-live-x86_64-musl-20221001-base.iso](https://repo-default.voidlinux.org/live/current/void-live-x86_64-musl-20221001-base.iso) | PASS
2023-07-25  | [void-live-i686-20230628-base.iso](https://repo-default.voidlinux.org/live/current/void-live-i686-20230628-base.iso) | PASS
2023-06-29  | [void-live-i686-20221001-base.iso](https://repo-default.voidlinux.org/live/current/void-live-i686-20221001-base.iso) | N/A
2023-06-29  | [void-live-i686-20210930.iso](https://repo-default.voidlinux.org/live/20210930/void-live-i686-20210930.iso) | PASS
2023-08-14  | [hrmpf-x86_64-6.1.3_1-20230105.iso ](https://github.com/leahneukirchen/hrmpf/releases/download/v20230105/hrmpf-x86_64-6.1.3_1-20230105.iso) | PASS
2023-08-22  | [void-live-lxqt-unofficial-x86_64-6.3.13_1-20230821.iso](https://voidbuilds.xyz/download/void-live-lxqt-unofficial-x86_64-6.3.13_1-20230821.iso) | PASS

## Description

### Overview

void-pi is a Void Linux installer similar to [void-installer](https://docs.voidlinux.org/installation/live-images/guide.html).

It extends void-installer in several ways:
- provides predefined templates for [BTRFS](https://en.wikipedia.org/wiki/Btrfs), [LVM](https://en.wikipedia.org/wiki/Logical_volume_management), and [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup).
- supports [rEFInd](https://rodsbooks.com/refind/), [Limine](https://limine-bootloader.org/), [Syslinux](https://wiki.syslinux.org/wiki/index.php?title=The_Syslinux_Project), and [Gummiboot](https://www.freedesktop.org/wiki/Software/systemd/systemd-boot/) boot managers
- supports EFISTUB boot loader
- supports multi-device configurations.
- installs and configures additional boot manager and file system related software.

void-pi works on Void with Intel or AMD x86 CPU. It wasn't tested with ARM CPUs.

### Features

- completely emulates `void-installer`.
- install from Void ISO or network.
- predefined templates for BTRFS, LVM, and LUKS.
- selected devices will be automatically cleaned up.
- TUI dynamically changes depending on selected template.
- uses `/mnt` for chroot by default.
- allows to use an alternative rootdir via `--rootdir` command line argument.
- all settings can be saved in a file and loaded on startup. File name is controlled via `--config` command line argument.
- passwords are never saved in files even temporarily.
- max password length is limited to 1024 charaters.
- Boot managers
    - uses GRUB, rEFInd, Limine, Syslnux, or Gummiboot as a boot manager.
    - uses EFISTUB as a boot loader.
    - GRUB supports fat, btrfs, ext2, ext3, ext4, and xfs file systems.
    - rEFInd supports fat, btrfs, ext2, ext3, and ext4 file systems.
    - rEFInd is configured to use kernel auto detection.
    - Limine supports fat, ext2, ext3, and ext4 file systems.
    - Syslinux supports fat, ext2, ext3, and ext4 file systems.
    - EFISTUB and Gummiboot are enabled with UEFI and support fat, ext2, ext3, ext4, f2fs, and xfs file systems. (btrfs is not supported at this time)
    - if a boot manager doesn't support a file system (or LVM, or LUKS), then installer will create an ext4 `/boot` partition.
- LUKS
    - LUKS can be used with GRUB, rEFInd, Limine, and Syslinux.
    - In case of GRUB whole system is located on LUKS, including encrypted `/boot`. LUKS1 is used because GRUB2 doesn't support LUKS2.
    - In case of rEFInd and Limine installer will create an unencrypted ext4 `/boot` partition. LUKS2 is used.
- Syslinux
    - In case of UEFI the kernel and initramfs files are located in the EFI system partition (aka ESP), as Syslinux does not (currently) have the ability to access files outside its own partition.
    - IA32 (32-bit) is currently unsupported.
- EFISTUB
    - The kernel and initramfs files are located in the EFI system partition (aka ESP).
    - btrfs is not supported.
- Gummiboot
    - The kernel and initramfs files are located in the EFI system partition (aka ESP).
    - btrfs is not supported.
- Multi-device support
    - Multi-device configurations are available with BTRFS and LVM.
- Additional software
    - [snooze](https://github.com/leahneukirchen/snooze) - run a command at a particular time.
    - [btrbk](https://github.com/digint/btrbk) - Tool for creating snapshots and remote backups of btrfs subvolumes.
    - [grub-btrfs](https://github.com/Antynea/grub-btrfs) - improves the grub bootloader by adding a btrfs snapshots sub-menu, allowing the user to boot into snapshots.

### Templates

- Manual configuration of everything
- GPT. Basic
- GPT. LVM
- GPT. LVM. LUKS
- GPT. LUKS. One device
- GPT. LUKS. LVM. One device

### Default settings

All default settings can be changed via `Common Attrs` sub-menu.

- installation source: local
- host name: voidpp
- user name: void
- keymap: us
- locale: en_US.UTF-8
- timezone: America/New_York
- LUKS mapping name: crypt
- MBR size: 1M
- ESP size: [550M][550M]
- Boot partition size: 1G
- btrbk: `/mnt/btr_pool`
    - snapshots are created for @, @opt, @var, @srv, and @home subvolumes.
    - btrbk is scheduled to run every day

### Filesystem

#### BIOS boot

- Limine and Syslinux
    - `/dev/sdX1` is the root filesystem (size: whole device)
- Grub
    - `/dev/sdX1` is the BIOS boot sector (size: 1M)
    - `/dev/sdX2` is the root filesystem (size: remainder)

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

### Manually run btrbk

- `sudo btrbk run`

### Manually run grub-btrfs

- `grub-mkconfig -o /boot/grub/grub.cfg`

## Dependencies

Name       | Provides                | Included in Void ISO?
---        | ---                     | ---
dialog     | ncurses user input menu | Y
gptfdisk   | GPT disk partitioning with sgdisk | N
lz4        | Extremely Fast Compression algorithm | N
snooze     | cron replacement | N
btrbk      | Tool for creating snapshots | N
grub-btrfs | Add a btrfs snapshots sub-menu to GRUB | N

## Licensing

This software is released under the GNU GPLv2 license.

## Credits

- [Void Linux](https://voidlinux.org/)
- [Voidvault](https://github.com/atweiden/voidvault)
 
[550M]: https://unix.stackexchange.com/a/722439
[flat layout]: https://btrfs.wiki.kernel.org/index.php/SysadminGuide#Layout
[nodatacow]: https://wiki.archlinux.org/index.php/Btrfs#Disabling_CoW

