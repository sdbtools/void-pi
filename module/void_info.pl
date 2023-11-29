% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

version :-
	writenl('version 0.17.0').

fs_info(bcachefs, 'Linux Bcachefs (experimental)').
fs_info(btrfs, 'Oracle\'s Btrfs').
fs_info(ext2, 'Linux ext2 (no journaling)').
fs_info(ext3, 'Linux ext3 (journal)').
fs_info(ext4, 'Linux ext4 (journal)').
fs_info(f2fs, 'Flash-Friendly Filesystem').
fs_info(swap, 'Linux swap').
fs_info(vfat, 'FAT32').
fs_info(xfs, 'SGI\'s XFS').
fs_info(zfs, 'OpenZFS').

source_dep_module('void-live', filesystem(bcachefs), ['bcachefs-tools', lz4]).
source_dep_module('void-live', filesystem(zfs), [zfs, lz4]).
source_dep_module('void-live', filesystem(f2fs), [lz4]).
source_dep_module('void-live', template(_), [gptfdisk]).
source_dep_module('void-live', template(gpt_luks), [lz4]).
% source_dep_module('void-live', template(gpt_luks_lvm), [lz4, curl]).
source_dep_module('void-live', template(gpt_luks_lvm), [lz4]).
source_dep_module('void-live', template(gpt_lvm_luks), [lz4]).
source_dep_module('void-live', template(gpt_raid), [mdadm]).
source_dep_module('void-live', inst_method(rootfs), [xz]).

source_dep_module(hrmpf, filesystem(zfs), [lz4]).
source_dep_module(hrmpf, filesystem(f2fs), [lz4]).
source_dep_module(hrmpf, template(gpt_luks), [lz4]).
source_dep_module(hrmpf, template(gpt_luks_lvm), [lz4]).
source_dep_module(hrmpf, template(gpt_lvm_luks), [lz4]).

