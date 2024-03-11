% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

zpool_list(L) :-
	os_shell_lines_codes('zpool list -Hp 2>/dev/null', CL),
	CL \= [],
	maplist(zpool_list_convert, CL, L),
	true.

zpool_list_convert(L, zp(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)) :-
	split_list_ne(L, "\t", SL),
	maplist(codes_atom, SL, [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11]),
	true.

% PN - pool name.
zfs_load_key(PN, PSWD) :-
	atom_concat('zfs load-key -L prompt ', PN, CA),
	popen(CA, write, WS),
	write(WS, PSWD), % no nl(WS) should be here.
	close(WS),
	true.

prop_info_zfs_feat(off, L) :- !,
	prop_info(zpool_feat, _, L).
prop_info_zfs_feat(legacy, L) :- !,
	prop_info(zpool_feat, _, L1),
	findall(prop_feat(Tag, off, Attr), member(prop_feat(Tag, _, Attr), L1), L).
prop_info_zfs_feat(C, L) :- !,
	os_shell2_lines_nc([cat, '/usr/share/zfs/compatibility.d/' + C], "#", CL),
	prop_info(zpool_feat, _, L1),
	findall(prop_feat(Tag, E, Attr), (member(prop_feat(Tag, _, Attr), L1), (memberchk(Tag, CL) -> E = on; E = off)), L).

% prop_info_zfs(TAG, _FS, L) :-
% 	prop_info(TAG, _, L).

% prop_ro(name, format)
% man zpoolprops
prop_info(zpool_ro, prop2, [
	  prop_ro(allocated, int)
	, prop_ro(bcloneratio, real)
	, prop_ro(bclonesaved, int)
	, prop_ro(bcloneused, int)
	, prop_ro(capacity, real)
	, prop_ro(expandsize, int)
	, prop_ro(fragmentation, int)
	, prop_ro(free, int)
	, prop_ro(freeing, int)
	, prop_ro(guid, guid)
	, prop_ro(health, enum(['ONLINE', 'DEGRADED', 'FAULTED', 'OFFLINE', 'REMOVED', 'UNAVAIL']))
	, prop_ro(leaked, int)
	, prop_ro(load_guid, guid)
	, prop_ro(size, int)
	]).

% opt3(name, format, def_value)
% "-o"
prop_info(zpool_props_rw, opt3p('-o'), [
	  opt3(altroot, path, '')
	, opt3(readonly, enum([on, off]), off)
	, opt3(ashift, enum([0, 9, 10, 11, 12, 13, 14, 15, 16]), 0)
	, opt3(autoexpand, enum([on, off]), off)
	, opt3(autoreplace, enum([on, off]), off)
	, opt3(autotrim, enum([on, off]), off)
	, opt3(bootfs, combo_val(zpool_bootfs), default=unset)
	, opt3(cachefile, combo_val(zpool_cachefile), default=unset)
	, opt3(comment, str, '')
	, opt3(compatibility, fmt(zpool_compatibility), off)
	, opt3(dedupditto, int, 0)
	, opt3(delegation, enum([on, off]), off)
	, opt3(failmode, enum([wait, continue, panic]), wait)
	, opt3(listsnapshots, enum([on, off]), off)
	, opt3(multihost, enum([on, off]), off)
	, opt3(version, int, 5000)
	]).

% zpool_feat(name, depends_list)
% man zpool-features
% Feature flags implementation per OS: https://openzfs.github.io/openzfs-docs/Basic%20Concepts/Feature%20Flags.html
prop_info(zpool_feat, feat3p('-o'), [
	  prop_feat(allocation_classes, on, attr([], ''))
	, prop_feat(async_destroy, on, attr([], ''))
	, prop_feat(blake3, on, attr([extensible_dataset], ''))
	, prop_feat(block_cloning, on, attr([], ''))
	, prop_feat(bookmarks, on, attr([extensible_dataset], ''))
	, prop_feat(bookmark_v2, on, attr([bookmark, extensible_dataset], ''))
	, prop_feat(bookmark_written, on, attr([bookmark, extensible_dataset, bookmark_v2], ''))
	, prop_feat(device_rebuild, on, attr([], ''))
	, prop_feat(device_removal, on, attr([], ''))
	, prop_feat(draid, on, attr([], ''))
	, prop_feat(edonr, on, attr([extensible_dataset], ''))
	, prop_feat(embedded_data, on, attr([], ''))
	, prop_feat(empty_bpobj, on, attr([], ''))
	, prop_feat(enabled_txg, on, attr([], ''))
	, prop_feat(encryption, on, attr([bookmark_v2, extensible_dataset], ''))
	, prop_feat(extensible_dataset, on, attr([], ''))
	, prop_feat(filesystem_limits, on, attr([extensible_dataset], ''))
	, prop_feat(head_errlog, on, attr([], ''))
	, prop_feat(hole_birth, on, attr([enabled_txg], ''))
	, prop_feat(large_blocks, on, attr([extensible_dataset], ''))
	, prop_feat(large_dnode, on, attr([extensible_dataset], ''))
	, prop_feat(livelist, on, attr([], ''))
	, prop_feat(log_spacemap, on, attr([spacemap_v2], ''))
	, prop_feat(lz4_compress, on, attr([], ''))
	, prop_feat(multi_vdev_crash_dump, on, attr([], ''))
	, prop_feat(obsolete_counts, on, attr([device_removal], ''))
	, prop_feat(project_quota, on, attr([extensible_dataset], ''))
	, prop_feat(redaction_bookmarks, on, attr([bookmarks, extensible_dataset], ''))
	, prop_feat(redacted_datasets, on, attr([extensible_dataset], ''))
	, prop_feat(resilver_defer, on, attr([], ''))
	, prop_feat(sha512, on, attr([extensible_dataset], ''))
	, prop_feat(skein, on, attr([extensible_dataset], ''))
	, prop_feat(spacemap_histogram, on, attr([], ''))
	, prop_feat(spacemap_v2, on, attr([], ''))
	, prop_feat(userobj_accounting, on, attr([extensible_dataset], ''))
	, prop_feat(vdev_zaps_v2, on, attr([], ''))
	, prop_feat(zilsaxattr, on, attr([extensible_dataset], ''))
	, prop_feat(zpool_checkpoint, on, attr([], ''))
	, prop_feat(zstd_compress, on, attr([extensible_dataset], ''))
	]).

% zfs_prop(name, format)
% man zfsprops
prop_info(zfs_ro, prop2, [
	  prop_ro(available, none)
	, prop_ro(compressratio, none)
	, prop_ro(createtxg, none)
	, prop_ro(creation, none)
	, prop_ro(clones, none)
	, prop_ro(defer_destroy, none)
	, prop_ro(encryptionroot, none)
	, prop_ro(filesystem_count, none)
	, prop_ro(keystatus, none)
	, prop_ro(guid, guid)
	, prop_ro(logicalreferenced, none)
	, prop_ro(logicalused, none)
	, prop_ro(mounted, none)
	, prop_ro(objsetid, none)
	, prop_ro(origin, none)
	, prop_ro(receive_resume_token, none)
	, prop_ro(redact_snaps, none)
	, prop_ro(referenced, none)
	, prop_ro(refcompressratio, none)
	, prop_ro(snapshot_count, none)
	, prop_ro(type, none)
	, prop_ro(used, none)
	, prop_ro(usedbychildren, none)
	, prop_ro(usedbydataset, none)
	, prop_ro(usedbyrefreservation, none)
	, prop_ro(usedbysnapshots, none)
	, prop_ro(userrefs, none)
	, prop_ro(snapshots_changed, none)
	, prop_ro(volblocksize, none)
	, prop_ro(written, none)

	, prop_ro(fmt(usedby), none)
	, prop_ro(fmt('userused@user'), none)
	, prop_ro(fmt('userobjused@user'), none)
	, prop_ro(fmt('groupused@group'), none)
	, prop_ro(fmt('groupobjused@group'), none)
	, prop_ro(fmt('projectused@project'), none)
	, prop_ro(fmt('projectobjused@project'), none)
	, prop_ro(fmt('written@snapshot'), none)
	]).

% opt3(name, format, def_value)
% man zfsprops
% "-O"
% -O file-system-property=value
prop_info(zfs_rw, opt3p('-O'), [
	  opt3(aclinherit, enum([discard, noallow, restricted, passthrough, 'passthrough-x']), restricted)
	, opt3(aclmode, enum([discard, groupmask, passthrough, restricted]), discard)
	, opt3(acltype, enum([off, nfsv4, posix]), off)
	, opt3(atime, enum([on, off]), on)
	, opt3(canmount, enum([on, off, noauto]), on)
	, opt3(checksum, enum([on, off, fletcher2, fletcher4, sha256, noparity, sha512, skein, edonr, blake3]), on)
	, opt3(compression, enum([on, off, gzip, 'gzip-1', lz4, lzjb, zle, zstd, 'zstd-1', 'zstd-fast', 'zstd-fast-1']), on)
	% , opt3(context, enum([none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level']), none)
	% , opt3(fscontext, enum([none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level']), none)
	% , opt3(defcontext, enum([none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level']), none)
	% , opt3(rootcontext, enum([none, 'SELinux-User:SELinux-Role:SELinux-Type:Sensitivity-Level']), none)
	, opt3(copies, enum([1, 2, 3]), 1)
	, opt3(devices, enum([on, off]), on)
	, opt3(dedup, enum([off, on, verify, sha256, sha512, skein, edonr, blake3]), off)
	, opt3(dnodesize, enum([legacy, auto, '1k', '2k', '4k', '8k', '16k']), legacy)
	, opt3(encryption, enum([off, on, 'aes-128-ccm', 'aes-192-ccm', 'aes-256-ccm', 'aes-128-gcm', 'aes-192-gcm', 'aes-256-gcm']), off)
	, opt3(keyformat, enum([none, raw, hex, passphrase]), none)
	, opt3(keylocation, combo_val(zfs_keylocation), default=prompt)
	, opt3(pbkdf2iters, int, 350000)
	, opt3(exec, enum([on, off]), on)
	, opt3(filesystem_limit, combo_val(zfs_filesystem_limit, [opt3(default, none, none), opt3(limit, int, 1)]), default=none)
	, opt3(special_small_blocks, int, 0)
	, opt3(mountpoint, combo_val(zfs_mountpoint, [opt3(default, none, none), opt3(legacy, none, legacy), opt3(path, path, '')]), default=none)
	, opt3(nbmand, enum([on, off]), off)
	, opt3(overlay, enum([on, off]), on)
	, opt3(prefetch, enum([all, none, metadata]), all)
	, opt3(primarycache, enum([all, none, metadata]), all)
	, opt3(quota, combo_val(zfs_quota, [opt3(default, none, none), opt3(quota, int, 0)]), default=none)
	, opt3(snapshot_limit, combo_val(zfs_snapshot_limit, [opt3(default, none, none), opt3(limit, int, 0)]), default=none)
	, opt3(readonly, enum([on, off]), off)
	, opt3(recordsize, int, 512)
	, opt3(redundant_metadata, enum([all, most, some, none]), all)
	, opt3(refquota, combo_val(zfs_refquota, [opt3(default, none, none), opt3(quota, int, 0)]), default=none)
	, opt3(refreservation, combo_val(zfs_refreservation, [opt3(default, none, none), opt3(auto, none, auto), opt3(size, int, 0)]), default=none)
	, opt3(relatime, enum([on, off]), on)
	, opt3(reservation, combo_val(zfs_reservation, [opt3(default, none, none), opt3(size, int, 0)]), default=none)
	, opt3(secondarycache, enum([all, none, metadata]), all)
	, opt3(setuid, enum([on, off]), on)
	, opt3(sharesmb, combo_val(zfs_sharesmb, [opt3(default, none, on), opt3(legacy, none, off), opt3(opts, str, '')]), legacy=off)
	, opt3(sharenfs, combo_val(zfs_sharenfs, [opt3(default, none, on), opt3(legacy, none, off), opt3(opts, str, '')]), legacy=off)
	, opt3(logbias, enum([latency, throughput]), latency)
	, opt3(snapdev, enum([hidden, visible]), hidden)
	, opt3(snapdir, enum([hidden, visible]), hidden)
	, opt3(sync, enum([standard, always, disabled]), standard)
	, opt3(version, combo_val(zfs_version, [opt3(default, none, current), opt3(version, int, 0)]), default=current)
	, opt3(volsize, int, 0)
	, opt3(volmode, enum([default, full, geom, dev, none]), full)
	, opt3(volthreading, enum([on, off]), on)
	, opt3(vscan, enum([on, off]), off)
	, opt3(xattr, enum([on, off, sa]), on)
	, opt3(jailed, enum([on, off]), off)
	, opt3(zoned, enum([on, off]), off)

	% , opt3('userquota@user', enum([int, none]), none)
	% , opt3('userobjquota@user', enum([int, none]), none)
	% , opt3('groupquota@group', enum([int, none]), none)
	% , opt3('groupobjquota@group', enum([int, none]), none)
	% , opt3('projectquota@project', enum([int, none]), none)
	% , opt3('projectobjquota@project', enum([int, none]), none)

	% The following three properties cannot be changed after the file system is
	% created, and therefore, should be set when the file system is created.
	, opt3(casesensitivity, enum([sensitive, insensitive, mixed]), sensitive)
	, opt3(normalization, enum([none, formC, formD, formKC, formKD]), none)
	, opt3(utf8only, enum([on, off]), off)
	]).

prop_info(zpool_rw, opt4s(' '), [
	  opt4(force, enable, no, '-f')
	, opt4(mountpoint, str, '', '-m')
	, opt4(tname, str, '', '-t')
	]).

