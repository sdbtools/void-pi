% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man btrfs
% man 5 btrfs
% man mkfs.btrfs
% ls /sys/fs/btrfs/features/
% btrfs_feat(name, [compat, safe, default], descr), "default" means "enabled".
% kv - kernel version.
prop_info(btrfs_feat, feat3s('--features', ','), [
	  prop_feat('mixed-bg', off, attr([compat=kv(2, 6, 37), safe=kv(2, 6, 37)], 'mixed data and metadata block groups (compat=2.6.37, safe=2.6.37)'))
	, prop_feat(quota, off, attr([compat=kv(3, 4, 0)], 'quota support (qgroups) (compat=3.4)'))
	, prop_feat(extref, on, attr([compat=kv(3, 7, 0), safe=kv(3, 12, 0), default=kv(3, 12, 0)], 'increased hardlink limit per file to 65536 (compat=3.7, safe=3.12, default=3.12)'))
	, prop_feat(raid56, off, attr([compat=kv(3, 9, 0)], 'raid56 extended format (compat=3.9)'))
	, prop_feat('skinny-metadata', on, attr([compat=kv(3, 10, 0), safe=kv(3, 18, 0), default=kv(3, 18, 0)], 'reduced-size metadata extent refs (compat=3.10, safe=3.18, default=3.18)'))
	, prop_feat('no-holes', on, attr([compat=kv(3, 14, 0), safe=kv(4, 0, 0), default=kv(5, 15, 0)], 'no explicit hole extents for files (compat=3.14, safe=4.0, default=5.15)'))
	, prop_feat('free-space-tree', on, attr([compat=kv(4, 5, 0), safe=kv(4, 9, 0), default=kv(5, 15, 0)], 'free space tree (space_cache=v2) (compat=4.5, safe=4.9, default=5.15)'))
	, prop_feat(raid1c34, off, attr([compat=kv(5, 5, 0)], 'RAID1 with 3 or 4 copies (compat=5.5)'))
	, prop_feat(zoned, off, attr([compat=kv(5, 12, 0)], 'support zoned devices (compat=5.12)'))
	, prop_feat('block-group-tree', off, attr([compat=kv(6, 1, 0)], 'block group tree to reduce mount time (compat=6.1)'))
	, prop_feat('raid-stripe-tree', off, attr([compat=kv(6, 7, 0)], ''))
	, prop_feat(squota, off, attr([compat=kv(6, 7, 0)], ''))
	]).

prop_info(btrfs_rw, opt4s(' '), [
	  opt4('byte-count', int, 0, '--byte-count')
	, opt4(checksum, enum([crc32c, xxhash, sha256, blake2]), crc32c, '--checksum')
	, opt4(data, enum([raid0, raid1, raid1c3, raid1c4, raid5, raid6, raid10, single, dup]), single, '--data')
	, opt4(metadata, enum([raid0, raid1, raid1c3, raid1c4, raid5, raid6, raid10, single, dup]), raid1, '--metadata')
	, opt4(mixed, enable, no, '--mixed')
	, opt4(nodesize, size, 16384, '--nodesize')
	, opt4(sectorsize, size, 16384, '--sectorsize')
	, opt4(label, str, '', '--label')
	, opt4(nodiscard, enable, no, '--nodiscard')
	, opt4(shrink, enable, no, '--shrink')
	, opt4(force, enable, no, '--force')
	, opt4(uuid, guid, '', '--uuid')
	]).

