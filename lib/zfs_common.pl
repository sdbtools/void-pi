% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

zpool_list(L) :-
	os_shell_lines_codes('zpool list -Hp 2>/dev/null', CL),
	CL \= [],
	maplist(zpool_list_convert, CL, L),
	true.

zpool_list_convert(L, zp(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)) :-
	split_list_ne(L, "\t", SL),
	maplist(codes_atom, SL, [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11]),
	true.

zfs_load_key(PSWD) :-
	popen('zfs load-key -L prompt zroot', write, WS),
	write(WS, PSWD), % no nl(WS) should be here.
	close(WS),
	true.

% zpool_prop(name, format)
zpool_prop_ro(allocated, int).
zpool_prop_ro(bcloneratio, real).
zpool_prop_ro(bclonesaved, int).
zpool_prop_ro(bcloneused, int).
zpool_prop_ro(capacity, real).
zpool_prop_ro(expandsize, int).
zpool_prop_ro(fragmentation, int).
zpool_prop_ro(free, int).
zpool_prop_ro(freeing, int).
zpool_prop_ro(guid, guid).
zpool_prop_ro(health, enum('ONLINE', 'DEGRADED', 'FAULTED', 'OFFLINE', 'REMOVED', 'UNAVAIL')).
zpool_prop_ro(leaked, int).
zpool_prop_ro(load_guid, guid).
zpool_prop_ro(size, int).

% zpool_prop(name, format, def_value)
zpool_prop_rw(altroot, path, '').
zpool_prop_rw(readonly, enum(on, off), off).
zpool_prop_rw(ashift, int, 0).
zpool_prop_rw(autoexpand, enum(on, off), off).
zpool_prop_rw(autoreplace, enum(on, off), off).
zpool_prop_rw(autotrim, enum(on, off), off).
zpool_prop_rw(bootfs, fmt(bootfs), none).
zpool_prop_rw(cachefile, fmt(cachefile), '').
zpool_prop_rw(comment, str, '').
zpool_prop_rw(compatibility, fmt(compatibility), '').
zpool_prop_rw(dedupditto, int, 0).
zpool_prop_rw(delegation, enum(on, off), off).
zpool_prop_rw(failmode, enum(wait, continue, panic), wait).
zpool_prop_rw(listsnapshots, enum(on, off), off).
zpool_prop_rw(multihost, enum(on, off), off).
zpool_prop_rw(version, int, 0).

% zpool_feat(name, depends_list)
zpool_feat(allocation_classes, []).
zpool_feat(async_destroy, []).
zpool_feat(blake3, [extensible_dataset]).
zpool_feat(block_cloning, []).
zpool_feat(bookmarks, [extensible_dataset]).
zpool_feat(bookmark_v2, [bookmark, extensible_dataset]).
zpool_feat(bookmark_written, [bookmark, extensible_dataset, bookmark_v2]).
zpool_feat(device_rebuild, []).
zpool_feat(device_removal, []).
zpool_feat(draid, []).
zpool_feat(edonr, [extensible_dataset]).
zpool_feat(embedded_data, []).
zpool_feat(empty_bpobj, []).
zpool_feat(enabled_txg, []).
zpool_feat(encryption, [bookmark_v2, extensible_dataset]).
zpool_feat(extensible_dataset, []).
zpool_feat(filesystem_limits, [extensible_dataset]).
zpool_feat(head_errlog, []).
zpool_feat(hole_birth, [enabled_txg]).
zpool_feat(large_blocks, [extensible_dataset]).
zpool_feat(large_dnode, [extensible_dataset]).
zpool_feat(livelist, []).
zpool_feat(log_spacemap, [spacemap_v2]).
zpool_feat(lz4_compress, []).
zpool_feat(multi_vdev_crash_dump, []).
zpool_feat(obsolete_counts, [device_removal]).
zpool_feat(project_quota, [extensible_dataset]).
zpool_feat(redaction_bookmarks, [bookmarks, extensible_dataset]).
zpool_feat(redacted_datasets, [extensible_dataset]).
zpool_feat(resilver_defer, []).
zpool_feat(sha512, [extensible_dataset]).
zpool_feat(skein, [extensible_dataset]).
zpool_feat(spacemap_histogram, []).
zpool_feat(spacemap_v2, []).
zpool_feat(userobj_accounting, [extensible_dataset]).
zpool_feat(vdev_zaps_v2, []).
zpool_feat(zilsaxattr, [extensible_dataset]).
zpool_feat(zpool_checkpoint, []).
zpool_feat(zstd_compress, [extensible_dataset]).

% zfs_prop(name, format)
% man zfsprops
zfs_prop_ro(available, none).
zfs_prop_ro(compressratio, none).
zfs_prop_ro(createtxg, none).
zfs_prop_ro(creation, none).
zfs_prop_ro(clones, none).
zfs_prop_ro(defer_destroy, none).
zfs_prop_ro(encryptionroot, none).
zfs_prop_ro(filesystem_count, none).
zfs_prop_ro(keystatus, none).
zfs_prop_ro(guid, guid).
zfs_prop_ro(logicalreferenced, none).
zfs_prop_ro(logicalused, none).
zfs_prop_ro(mounted, none).
zfs_prop_ro(objsetid, none).
zfs_prop_ro(origin, none).
zfs_prop_ro(receive_resume_token, none).
zfs_prop_ro(redact_snaps, none).
zfs_prop_ro(referenced, none).
zfs_prop_ro(refcompressratio, none).
zfs_prop_ro(snapshot_count, none).
zfs_prop_ro(type, none).
zfs_prop_ro(used, none).
zfs_prop_ro(usedbychildren, none).
zfs_prop_ro(usedbydataset, none).
zfs_prop_ro(usedbyrefreservation, none).
zfs_prop_ro(usedbysnapshots, none).
zfs_prop_ro(userrefs, none).
zfs_prop_ro(snapshots_changed, none).
zfs_prop_ro(volblocksize, none).
zfs_prop_ro(written, none).

zfs_prop_ro(fmt(usedby), none).
zfs_prop_ro(fmt('userused@user'), none).
zfs_prop_ro(fmt('userobjused@user'), none).
zfs_prop_ro(fmt('groupused@group'), none).
zfs_prop_ro(fmt('groupobjused@group'), none).
zfs_prop_ro(fmt('projectused@project'), none).
zfs_prop_ro(fmt('projectobjused@project'), none).
zfs_prop_ro(fmt('written@snapshot'), none).

% zfs_prop_rw(name, format, def_value)
zfs_prop_rw(aclinherit, enum(discard, noallow, restricted, passthrough, 'passthrough-x'), none).
zfs_prop_rw(aclmode, enum(discard, groupmask, passthrough, restricted), none).
zfs_prop_rw(acltype, enum(off, nfsv4, posix), off).
zfs_prop_rw(atime, enum(on, off), none).
zfs_prop_rw(canmount, enum(on, off, noauto), none).
zfs_prop_rw(checksum, enum(on, off, fletcher2, fletcher4, sha256, noparity, sha512, skein, edonr, blake3), none).
zfs_prop_rw(compression, enum(on, off, gzip, 'gzip-1', lz4, lzjb, zle, zstd, 'zstd-1', 'zstd-fast', 'zstd-fast-1'), none).
zfs_prop_rw(context, enum(none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level'), none).
zfs_prop_rw(fscontext, enum(none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level'), none).
zfs_prop_rw(defcontext, enum(none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level'), none).
zfs_prop_rw(rootcontext, enum(none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level'), none).
zfs_prop_rw(copies, enum(1, 2, 3), none).
zfs_prop_rw(devices, enum(on, off), none).
zfs_prop_rw(dedup, enum(off, on, verify, sha256, sha512, skein, edonr, blake3), off).
zfs_prop_rw(dnodesize, enum(legacy, auto, '1k', '2k', '4k', '8k', '16k'), legacy).
zfs_prop_rw(encryption, enum(off, on, 'aes-128-ccm', 'aes-192-ccm', 'aes-256-ccm', 'aes-128-gcm', 'aes-192-gcm', 'aes-256-gcm'), off).
zfs_prop_rw(keyformat, enum(raw, hex, passphrase), none).
zfs_prop_rw(keylocation, enum(prompt, 'file:///absolute/file/path', 'https://address', 'http://address'), none).
zfs_prop_rw(pbkdf2iters, int, none).
zfs_prop_rw(exec, enum(on, off), on).
zfs_prop_rw(filesystem_limit, enum(int, none), none).
zfs_prop_rw(special_small_blocks, int, none).
zfs_prop_rw(mountpoint, enum(path, none, legacy), none).
zfs_prop_rw(nbmand, enum(on, off), none).
zfs_prop_rw(overlay, enum(on, off), none).
zfs_prop_rw(primarycache, enum(all, none, metadata), all).
zfs_prop_rw(quota, enum(int, none), none).
zfs_prop_rw(snapshot_limit, enum(int, none), none).
zfs_prop_rw(readonly, enum(on, off), none).
zfs_prop_rw(recordsize, int, none).
zfs_prop_rw(redundant_metadata, enum(all, most, some, none), all).
zfs_prop_rw(refquota, enum(int, none), none).
zfs_prop_rw(refreservation, enum(size, none, auto), none).
zfs_prop_rw(relatime, enum(on, off), none).
zfs_prop_rw(reservation, enum(int, none), none).
zfs_prop_rw(secondarycache, enum(all, none, metadata), all).
zfs_prop_rw(setuid, enum(on, off), on).
zfs_prop_rw(sharesmb, enum(on, off, opts), none).
zfs_prop_rw(sharenfs, enum(on, off, opts), none).
zfs_prop_rw(logbias, enum(latency, throughput), none).
zfs_prop_rw(snapdev, enum(hidden, visible), none).
zfs_prop_rw(snapdir, enum(hidden, visible), none).
zfs_prop_rw(sync, enum(standard, always, disabled), none).
zfs_prop_rw(version, enum(int, current), none).
zfs_prop_rw(volsize, int, none).
zfs_prop_rw(volmode, enum(default, full, geom, dev, none), none).
zfs_prop_rw(vscan, enum(on, off), none).
zfs_prop_rw(xattr, enum(on, off, sa), none).
zfs_prop_rw(jailed, enum(on, off), none).
zfs_prop_rw(zoned, enum(on, off), none).

zfs_prop_rw('userquota@user', enum(int, none), none).
zfs_prop_rw('userobjquota@user', enum(int, none), none).
zfs_prop_rw('groupquota@group', enum(int, none), none).
zfs_prop_rw('groupobjquota@group', enum(int, none), none).
zfs_prop_rw('projectquota@project', enum(int, none), none).
zfs_prop_rw('projectobjquota@project', enum(int, none), none).

% The following three properties cannot be changed after the file system is
% created, and therefore, should be set when the file system is created.
zfs_prop_rw(casesensitivity, enum(sensitive, insensitive, mixed), sensitive).
zfs_prop_rw(normalization, enum(none, formC, formD, formKC, formKD), none).
zfs_prop_rw(utf8only, enum(on, off), off).

