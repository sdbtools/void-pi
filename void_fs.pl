% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

mkfs(RD) :-
	% tui_msgbox('mkfs', []),
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_, P, PD, _, FS, Label, _MP, create, _SZ)),
	% modprobe
	( FS = swap; FS = lvm; FS = luks1 ->
	  true
	; ( os_call2([modprobe, FS]) ->
	    true
	  ; tui_msgbox2([modprobe, FS, has, failed], []),
	    fail
	  ),
	  true
	),
	mkfs(FS, P, PD, Label, RD),
	fail.
mkfs(_).

mkfs(zfs, P, _PD, Label, RD) :- !,
	add_dquote(Label, _LQ),
	% lx_get_dev_disk_partuuid(PD, PID),
	lx_get_dev_id(P, PID),

	% Create a ZFS pool
	% tui_programbox_safe([
	tui_progressbox_safe([
		zpool,
		create,
		o(f),
		o(o, v(ashift, 12)),
		o(o, v(autotrim, on)),
		o('O', v(compression, lz4)),
		o('O', v(acltype, posixacl)),
		o('O', v(xattr, sa)),
		o('O', v(relatime, on)),
		% o('O', v(mountpoint, '/')),
		o(m, none),
		zroot,
		PID,
		'2>&1'
		], 'Creating filesystem zfs', [sz([12, 80])]
	),
	% export and re-import the pool with a temporary, alternate root path
	os_shell2([zpool, export, zroot]),
	os_shell2([zpool, import, '-N', o('R', RD), zroot]),
	create_zfs_dataset,

	% Mount the ZFS hierarchy
	os_shell2([zfs, mount, 'zroot/ROOT/void', '2>&1']),
	os_shell2([zfs, mount, '-a', '2>&1']),

	% % record the current pool configuration in a cache file that Void will use to avoid walking the entire device hierarchy to identify importable pools.
	% os_shell2([mkdir, '-p', '/mnt/etc/zfs']),
	% os_shell2([zpool, set, 'cachefile=/mnt/etc/zfs/zpool.cache', zroot]),
	!.
mkfs(lvm, _P, PD, Label, RD) :- !,
	add_dquote(Label, LQ),
	( lvm_pvcreate_unsafe(PD), !
	; tui_msgbox('lvm_pvcreate has failed', []),
	  fail
	),
	( lvm_vgcreate(LQ, [PD]), !
	; tui_msgbox('lvm_vgcreate has failed', []),
	  fail
	),
	inst_setting(lvm, lv(VG, LV, SZ)),
	inst_setting(root_fs, FS),
	lvm_lvcreate_unsafe(Label, LV, SZ),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	mkfs(FS, _P, LVM_PD, Label, RD),
	!.
mkfs(luks1, _P, PD, Label, RD) :- !,
	Type = luks1,
	inst_setting_tmp(passwd('$_luks_$'), RPWD),
	tui_infobox('Creating crypto-device.', [sz([4, 40])]),
	lx_luks_format(Type, PD, RPWD),

	inst_setting(luks, luks(Name)),
	tui_infobox('Opening crypto-device.', [sz([4, 40])]),
	lx_luks_open(Type, Name, PD, RPWD),

	inst_setting(root_fs, FS),
	atom_concat('/dev/mapper/', Name, LUKS_PD),
	mkfs(FS, _P, LUKS_PD, Label, RD),
	!.
mkfs(btrfs, _P, PD, Label, RD) :- !,
	tui_progressbox_safe(['mkfs.btrfs', o('L', dq(Label)), '-f', PD, '2>&1'], 'Creating filesystem btrfs', [sz([12, 80])]),
	% tui_programbox_safe(['mkfs.btrfs', '-f', '-L', LQ, PD, '2>&1'], 'Creating filesystem btrfs', [sz([12, 80])]),
	create_btrfs_subv(PD, RD).
mkfs(ext2, _P, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-F', PD, '2>&1'], 'Creating filesystem ext2', [sz([12, 80])]),
	true.
mkfs(ext3, _P, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-j', '-F', PD, '2>&1'], 'Creating filesystem ext3', [sz([12, 80])]),
	true.
mkfs(ext4, _P, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), o(t, ext4), '-F', PD, '2>&1'], 'Creating filesystem ext4', [sz([12, 80])]),
	% tui_programbox_safe(['mke2fs', '-F', '-t', 'ext4', PD, '2>&1'], 'Creating filesystem ext4', [sz([24, 80])]),
	true.
mkfs(f2fs, _P, PD, Label, _RD) :- !,
	% tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), '-f', PD, '2>&1'], 'Creating filesystem f2fs', [sz([12, 80])]),
	tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), o('O', 'extra_attr,inode_checksum,sb_checksum,compression,encrypt'), '-f', PD, '2>&1'], 'Creating filesystem f2fs', [sz([12, 80])]),
	true.
mkfs(vfat, _P, PD, Label, _RD) :- !,
	upper(Label, UL),
	tui_progressbox_safe(['mkfs.vfat', o('F', '32'), o('n', dq(UL)), PD, '2>&1'], 'Creating filesystem vfat', [sz([12, 80])]),
	% tui_programbox_safe(['mkfs.vfat', '-F', '32', '-n', dq(UL), PD, '2>&1'], 'Creating filesystem vfat', [sz([12, 80])]),
	true.
mkfs(xfs, _P, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mkfs.xfs', o('L', dq(Label)), '-f', '-i', 'sparse=0', PD, '2>&1'], 'Creating filesystem xfs', [sz([12, 80])]),
	true.
mkfs(swap, _P, PD, _Label, _RD) :- !,
	os_shell2_rc([swapoff, PD, '>/dev/null', '2>&1'], _),
	( os_shell2l([mkswap, PD, '2>&1']) ->
	  true
	; tui_msgbox('ERROR: failed to create swap', []),
	  fail
	),
	( os_shell2l([swapon, PD, '2>&1']) ->
	  true
	; tui_msgbox('ERROR: failed to activate swap', []),
	  fail
	),
	true.
mkfs(FS, _P, _, _, _, _RD) :- !,
	tui_msgbox2(['Unknown filesystem', FS], []),
	fail.

mount_fs(RD) :-
	get_mp_list(MPL),
	maplist(mount_mp(RD), MPL),
	true.

% Get list of mounting points in order in which they should be mounted (except of swap).
get_mp_list(MPL1) :-
	% Ignore swap partition
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	findall(MP, (inst_setting(partition, part(_D, _S1, _P1, _PT, FS, _Label, MP, _CK, _SZ)), FS \= swap), MPL0),
	sort(MPL0, MPL1),
	true.

mount_mp(RD, MP) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_, _RP, PD, _, FS, _Label, MP, _, _SZ)),
	mount_fs(FS, PD, MP, RD),
	true.

% Ignore swap partition.
mount_fs(swap, _D, _MP, _RD) :-
	!.
% Ignore empty mount point.
mount_fs(_FS, _D, '', _RD) :-
	!.
mount_fs(btrfs, D, _MP, RD) :-
	mount_btrfs(D, RD),
	!.
mount_fs(zfs, _D, _MP, _RD) :-
	% tui_msgbox2([before, zfs, mount, 'zroot/ROOT/void'], []),
	% % Mount the ZFS hierarchy
	% % os_shell2([zfs, mount, 'zroot/ROOT/void']),
	% tui_programbox_safe([zfs, mount, 'zroot/ROOT/void', '2>&1'], '', [title(' zfs mount zroot/ROOT/void '), sz([12, 80])]),
	% tui_msgbox2([after, zfs, mount, 'zroot/ROOT/void'], []),
	% os_shell2([zfs, mount, '-a']),
	% tui_msgbox2([after, zfs, mount, '-a'], []),
	!.
mount_fs(lvm, _D, MP, RD) :-
	inst_setting(lvm, lv(VG, LV, _SZ)),
	inst_setting(root_fs, FS),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	mount_fs(FS, LVM_PD, MP, RD),
	!.
mount_fs(luks1, _D, MP, RD) :-
	inst_setting(root_fs, FS),
	luks_dev_name(LUKS_PD),
	mount_fs(FS, LUKS_PD, MP, RD),
	!.
mount_fs(FS, D, MP, RD) :-
	memberchk(FS, [vfat, ext2, ext3, ext4, f2fs, xfs]),
	atom_concat(RD, MP, MP1),
	os_mkdir_p(MP1),
	( inst_setting(fs(FS, MP), mount(O))
	; O = [rw, noatime]
	),
	join_atoms(O, ',', OA),
	os_shell2([mount, '-t', FS, o(o, OA), D, MP1, '2>&1']),
	!.
mount_fs(FS, D, MP, _RD) :-
	tui_msgbox2(['mount_fs has failed.', [FS, D, MP]], [sz([6, 40])]),
	fail.

% DO NOT delete
% post_setup_fs :-
% 	get_mp_list(MPL),
% 	maplist(setup_mp, MPL),
% 	true.

% setup_mp(MP) :-
% 	inst_setting(partition, part(_, _RP, D, _, FS, _, MP, _, _SZ)),
% 	setup_mp(FS, D, MP),
% 	true.

% setup_mp(btrfs, _D, '/') :- !,
% 	( inst_setting(fs(btrfs, '/'), snap_dir(SD)) ->
% 	  os_shell2([mkdir, '-p', SD, '2>&1'])
% 	; true
% 	),
% 	true.
% setup_mp(zfs, _D, _MP) :- !,Device major number
% 	% zfs requires hostid.
% 	% os_shell2([cp, '/etc/hostid', '/mnt/etc/hostid']),
% 	tui_programbox_safe([cp, '/etc/hostid', '/mnt/etc/hostid'], '', [title(' copy hostid '), sz([12, 80])]),
% 	% record the current pool configuration in a cache file that Void will use to avoid walking the entire device hierarchy to identify importable pools.
% 	os_shell2([mkdir, '-p', '/mnt/etc/zfs']),
% 	os_shell2([zpool, set, 'cachefile=/mnt/etc/zfs/zpool.cache', zroot]),
% 	true.
setup_mp(_FS, _D, _MP).

clean_mnt(RD) :-
	os_shell2([umount, oo(recursive), RD, '2>/dev/nul']),
	fail.
% clean_mnt(_RD) :-
% 	inst_setting(bootloader_dev, dev(D, _, _)),
% 	lx_split_dev(D, _P, S),
% 	lx_part_info_disk(S, D, L),
% 	% lx_part_info_mapper(L),
% 	write_to_atom(A, L),
% 	tui_msgbox(A, []),
% 	% part_info(PD, P, PA, FS, FSS, Type), PIL,
% 	member(part_info(_, _, _, crypto_LUKS, _, _), L),
% 	% cryptsetup close cryptroot
% 	fail.
clean_mnt(_RD) :-
	% LVM
	inst_setting(bootloader_dev, dev(D, _, _)),
	lvm_pvs(PVL),
	findall(pv(PV,VG), (member(pv(PV,VG), PVL), atom_concat(D, _, PV)), VGL),
	maplist(clean_mnt_lvm_, VGL),
	fail.
clean_mnt(RD) :-
	% uses_zfs,
	zpool_list(L),
	memberchk(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,RD), L),
	tui_progressbox_safe([zpool, destroy, '-f', PN, '2>&1'], '', [title(' zpool destroy '), sz([6, 40])]),
	fail.
% lsblk -n -o KNAME,PKNAME /dev/sda1
% lsblk -J -o NAME,TYPE
% lsblk -sr /dev/mapper/cryptroot
% lsblk -pr /dev/sda
% lsblk -o NAME,TYPE,RO,PTUUID,PARTUUID
% lsblk -o KNAME,TYPE,SIZE
% lsblk -S
clean_mnt(_).

clean_mnt_lvm_(pv(PV,VG)) :-
	lvm_vgremove_unsafe(VG, PV).

run_mkfs(RD) :-
	install_zfs(RD),
	mkfs(RD),
	mount_fs(RD),
	% post_setup_fs,
	!.
run_mkfs(_) :-
	tui_msgbox('mkfs has failed.', []),
	fail.

