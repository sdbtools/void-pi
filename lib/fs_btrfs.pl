% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man btrfs
% man 5 btrfs
% man mkfs.btrfs
% ls /sys/fs/btrfs/features/
% btrfs_feat(name, [compat, safe, default], descr), "default" means "enabled".
% kv - kernel version.
prop_info(btrfs_feat, feat4s('--features', ',', pref1('^')), [
	  prop_feat4('mixed-bg', off, std, attr([compat=kv(2, 6, 37), safe=kv(2, 6, 37)], 'mixed data and metadata block groups (compat=2.6.37, safe=2.6.37)'))
	, prop_feat4(quota, off, std, attr([compat=kv(3, 4, 0)], 'quota support (qgroups) (compat=3.4)'))
	, prop_feat4(extref, on, std, attr([compat=kv(3, 7, 0), safe=kv(3, 12, 0), default=kv(3, 12, 0)], 'increased hardlink limit per file to 65536 (compat=3.7, safe=3.12, default=3.12)'))
	, prop_feat4(raid56, off, std, attr([compat=kv(3, 9, 0)], 'raid56 extended format (compat=3.9)'))
	, prop_feat4('skinny-metadata', on, std, attr([compat=kv(3, 10, 0), safe=kv(3, 18, 0), default=kv(3, 18, 0)], 'reduced-size metadata extent refs (compat=3.10, safe=3.18, default=3.18)'))
	, prop_feat4('no-holes', on, std, attr([compat=kv(3, 14, 0), safe=kv(4, 0, 0), default=kv(5, 15, 0)], 'no explicit hole extents for files (compat=3.14, safe=4.0, default=5.15)'))
	, prop_feat4('free-space-tree', on, std, attr([compat=kv(4, 5, 0), safe=kv(4, 9, 0), default=kv(5, 15, 0)], 'free space tree (space_cache=v2) (compat=4.5, safe=4.9, default=5.15)'))
	, prop_feat4(raid1c34, off, std, attr([compat=kv(5, 5, 0)], 'RAID1 with 3 or 4 copies (compat=5.5)'))
	, prop_feat4(zoned, off, std, attr([compat=kv(5, 12, 0)], 'support zoned devices (compat=5.12)'))
	, prop_feat4('block-group-tree', off, std, attr([compat=kv(6, 1, 0)], 'block group tree to reduce mount time (compat=6.1)'))
	, prop_feat4('raid-stripe-tree', off, std, attr([compat=kv(6, 7, 0)], ''))
	, prop_feat4(squota, off, std, attr([compat=kv(6, 7, 0)], ''))
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

prop_info(mnt_btrfs_feat, feat4s('--options', ',', on_off), [
	  prop_feat4(acl, on, pref1(no), none)
	, prop_feat4(autodefrag, off, pref1(no), none)
	, prop_feat4(compress, off, std, none)
	, prop_feat4('compress-force', off, std, none)
	, prop_feat4(barrier, on, pref1(no), none)
	, prop_feat4(check_int, off, std, none)
	, prop_feat4(check_int_data, off, std, none)
	, prop_feat4(clear_cache, off, std, none)
	, prop_feat4(datacow, on, pref1(no), none)
	, prop_feat4(datasum, on, pref1(no), none)
	, prop_feat4(degraded, off, std, none)
	, prop_feat4(discard, off, pref1(no), none)
	, prop_feat4(enospc_debug, off, pref1(no), none)
	, prop_feat4(flushoncommit, off, pref1(no), none)
	, prop_feat4(nologreplay, off, pref1(no), none)
	, prop_feat4(norecovery, off, pref1(no), none)
	, prop_feat4(rescan_uuid_tree, off, pref1(no), none)
	, prop_feat4(skip_balance, off, pref1(no), none)
	, prop_feat4(space_cache, off, pref1(no), none)
	, prop_feat4(ssd, off, pref1(no), none)
	, prop_feat4(ssd_spread, off, pref1(no), none)
	, prop_feat4(treelog, on, pref1(no), none)
	, prop_feat4(usebackuproot, off, pref1(no), none)
	, prop_feat4(user_subvol_rm_allowed, off, pref1(no), none)
	]).

prop_info(mnt_btrfs_opt, opt3s('--options', ','), [
	  opt3(check_int_print_mask, int, 0)
	, opt3(commit, int, 30)
	, opt3(compress, enum([zlib, lzo, zstd, no]), zlib)
	, opt3('compress-force', enum([zlib, lzo, zstd, no]), zlib)
	, opt3(device, str, '')
	, opt3(discard, enum([sync, async]), async)
	, opt3(fatal_errors, enum([bug, panic]), bug)
	, opt3(fragment, enum([off, data, metadata, all]), off)
	, opt3(max_inline, int, 2048)
	, opt3(metadata_ratio, int, 0)
	, opt3(rescue, enum([usebackuproot, nologreplay, ignorebadroots, ibadroots, ignoredatacsums, idatacsums, all]), all)
	, opt3(space_cache, enum([v1, v2]), v2)
	, opt3(subvol, str, '')
	, opt3(subvolid, int, 0)
	, opt3(thread_pool, int, 0)
	]).

