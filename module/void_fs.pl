% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

has_boot_part(TL) :-
	% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
	memberchk(fs5(_FS, _Label, '/boot', _DL, _CK), TL), !,
	true.

has_root_part(TL) :-
	% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
	memberchk(fs5(_FS, _Label, '/', _DL, _CK), TL), !,
	true.

has_usr_part(TL) :-
	% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
	memberchk(fs5(_FS, _Label, '/usr', _DL, _CK), TL), !,
	true.

root_pd(TL, PD) :-
	% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
	memberchk(fs5(_FS, _Labe1, '/', [PD| _], _CK), TL),
	!.
root_pd(_TL, _PD) :-
	tui_msgbox2(['root partition was not found']),
	fail.

root_fs(TL, FS) :-
	% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
	memberchk(fs5(FS, _Labe1, '/', _DL, _CK), TL).

boot_pref(TL, '') :-
	has_boot_part(TL),
	!.
boot_pref(_TL, 'boot/').

mkfs(zfs, Title, [PD, _], Label, RD) :- !,
	add_dquote(Label, _LQ),
	% lx_get_dev_disk_partuuid(PD, PID),
	lx_split_dev(PD, _Pref, P),
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
		], '', [title(Title), sz([12, 80])]
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
mkfs(btrfs, Title, DL, Label, RD) :- !,
	tui_progressbox_safe(['mkfs.btrfs', o('L', dq(Label)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	% tui_programbox_safe(['mkfs.btrfs', '-f', '-L', LQ, DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	DL = [D| _],
	create_btrfs_subv(D, RD).
mkfs(bcachefs, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mkfs.bcachefs', o('L', dq(Label)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(ext2, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-F', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(ext3, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-j', '-F', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(ext4, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), o(t, ext4), '-F', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	% tui_programbox_safe(['mke2fs', '-F', '-t', 'ext4', DL, '2>&1'], '', [title(Title), sz([24, 80])]),
	true.
mkfs(f2fs, Title, DL, Label, _RD) :- !,
	( inst_setting(fs_attr(f2fs, _), create(OL))
	; OL = [encrypt]
	),
	% tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), o('O', lc(OL)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(vfat, Title, DL, Label, _RD) :- !,
	upper(Label, UL),
	tui_progressbox_safe(['mkfs.vfat', o('F', '32'), o('n', dq(UL)), DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	% tui_programbox_safe(['mkfs.vfat', '-F', '32', '-n', dq(UL), DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(xfs, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mkfs.xfs', o('L', dq(Label)), '-f', '-i', 'sparse=0', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(swap, _Title, [PD| _], _Label, _RD) :- !,
	os_shell2_rc([swapoff, PD, '>/dev/null', '2>&1'], _),
	( os_shell2l([mkswap, PD, '2>&1']) ->
	  true
	; tui_msgbox('ERROR: failed to create swap'),
	  fail
	),
	( os_shell2l([swapon, PD, '2>&1']) ->
	  true
	; tui_msgbox('ERROR: failed to activate swap'),
	  fail
	),
	true.
mkfs(FS, _Title, _, _, _, _RD) :- !,
	tui_msgbox2(['Unknown filesystem', FS]),
	fail.

% bdev4(Name, Label, TargetDev, SourceDevList)
% vg(Name, [PhysicalVolumeList], [LogicalVolumeList])
mkbd(lvm, vg(VG, SDL, CL)) :- !,
	maplist(mk_lvm_pvcreate, SDL),
	( lvm_vgcreate(VG, SDL)
	; tui_msgbox('vgcreate has failed'),
	  fail
	), !,
	maplist(mk_lvm_lvcreate(VG), CL),
	true.
mkbd(luks, luks(Type, PD)) :- !,
	inst_setting_tmp(passwd('$_luks_$'), RPWD),
	atom_concat('Creating crypto-device ', PD, M1),
	tui_infobox(M1, [sz([4, 40])]),
	( lx_luks_format(Type, PD, RPWD)
	; tui_msgbox('luks_format has failed'),
	  fail
	), !,
	% inst_setting(luks, luks(Name)),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	atom_concat('Opening crypto-device ', PD, M2),
	tui_infobox(M2, [sz([4, 40])]),
	( lx_luks_open(Type, LUKS_PD, PD, RPWD)
	; tui_msgbox('luks_open has failed'),
	  fail
	), !,
	true.
mkbd(BD, _) :- !,
	tui_msgbox2(['Unknown block device', BD]),
	fail.

% PD - a single device or a list
mk_lvm_pvcreate(PD) :-
	% A PV can be a disk partition, whole disk, meta device, or loopback file.
	lvm_pvcreate_unsafe(PD), !.
mk_lvm_pvcreate(PD) :-
	tui_msgbox2(['lvm_pvcreate', PD, 'has failed']),
	fail.

mk_lvm_lvcreate(VG, lv(LV, SZ)) :-
	lvm_lvcreate_unsafe(VG, LV, SZ), !.
mk_lvm_lvcreate(VG, lv(LV, _SZ)) :-
	tui_msgbox2([lvcreate, VG, LV, 'has failed']),
	fail.

% Get list of mounting points in order in which they should be mounted (except of swap).
get_mp_list(TL, MPL1) :-
	% Ignore swap partition
	% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
	findall(MP, (member(fs5(FS, _Label, MP, _DL, _CK), TL), FS \= swap), MPL0),
	sort(MPL0, MPL1),
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
	% tui_msgbox2([before, zfs, mount, 'zroot/ROOT/void']),
	% % Mount the ZFS hierarchy
	% % os_shell2([zfs, mount, 'zroot/ROOT/void']),
	% tui_programbox_safe([zfs, mount, 'zroot/ROOT/void', '2>&1'], '', [title(' zfs mount zroot/ROOT/void '), sz([12, 80])]),
	% tui_msgbox2([after, zfs, mount, 'zroot/ROOT/void']),
	% os_shell2([zfs, mount, '-a']),
	% tui_msgbox2([after, zfs, mount, '-a']),
	!.
mount_fs(FS, D, MP, RD) :-
	memberchk(FS, [vfat, ext2, ext3, ext4, f2fs, xfs, bcachefs]),
	atom_concat(RD, MP, MP1),
	os_mkdir_p(MP1),
	( inst_setting(fs_attr(FS, MP), mount(OL))
	; OL = [rw, noatime]
	),
	os_shell2([mount, o(t, FS), o(o, lc(OL)), D, MP1, '2>&1']),
	!.
mount_fs(FS, D, MP, _RD) :-
	tui_msgbox2(['mount_fs has failed.', [FS, D, MP]], [sz([6, 40])]),
	fail.

umount_mnt(RD) :-
	os_shell2([umount, '--recursive', RD, '2>/dev/nul']), !.
umount_mnt(_RD).

% PV - long device name
clean_mnt_lvm_(pv(PV, VG)) :-
	lvm_pvremove_unsafe(VG, PV),
	!.
clean_mnt_lvm_(pv(PV, VG)) :-
	tui_msgbox2(['Removing of PV ', PV, 'from VG', VG, 'has failed.']),
	fail.

ensure_lvm(TL) :-
	lx_list_dev_part(PL),
	member(bdev(lvm, vg(VG, _, LVL)), TL),
	member(lv(LV, _SZ), LVL),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	member(dev_part(LVM_PD,_,_,_), PL), !,
	PL = [dev_part(D2,_,_,_)| _],
	format_to_atom(M, 'LVM device ~w (VG: ~w, LV: ~w) already exists on the device ~w', [LVM_PD, VG, LV, D2]),
	tui_msgbox(M, [title(' Ensure LVM ERROR ')]),
	fail.
ensure_lvm(_TL).

