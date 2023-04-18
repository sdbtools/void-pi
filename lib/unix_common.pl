% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

ux_user_root :-
	os_shell_number('id -u', 0).

% sgdisk -L
% sfdisk --list-types
part_typecode(bios_boot, 'EF02').
part_typecode(efi_system, 'EF00').
part_typecode(solaris_root, 'BF00').
part_typecode(solaris_boot, 'BF01').
part_typecode(swap, '8200').
part_typecode(linux, '8300').
part_typecode(linux_luks, '8309').
part_typecode(linux_lvm, '8e00').
part_typecode(linux_raid, 'fd00').

fs_info(btrfs, 'Oracle\'s Btrfs').
fs_info(ext2, 'Linux ext2 (no journaling)').
fs_info(ext3, 'Linux ext3 (journal)').
fs_info(ext4, 'Linux ext4 (journal)').
fs_info(f2fs, 'Flash-Friendly Filesystem').
fs_info(swap, 'Linux swap').
fs_info(vfat, 'FAT32').
fs_info(xfs, 'SGI\'s XFS').
fs_info(zfs, 'OpenZFS').

