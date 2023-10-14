% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/Dm-crypt
% https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system
% https://wiki.archlinux.org/title/Dm-crypt/System_configuration
% Stacking LVM volumes: https://access.redhat.com/articles/2106521
% https://wiki.syslinux.org/wiki/index.php?title=Filesystem#ext
% Free Software EFI/UEFI file system drivers: https://efi.akeo.ie/
%	EfiFs - EFI File System Drivers: https://github.com/pbatard/efifs

has_boot_part(TL) :-
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Label, '/boot', _DL, _CAL, _MOL, _CK), TL), !,
	true.

has_root_part(TL) :-
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Label, '/', _DL, _CAL, _MOL, _CK), TL), !,
	true.
has_root_part(TL) :-
	member(fs5_multi(btrfs, _Label, _DL, PTL, _CK), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL), !,
	true.
has_root_part(TL) :-
	member(fs5_multi(zfs, _Label, _DL, PTL, _CK), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.

has_usr_part(TL) :-
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Label, '/usr', _DL, _CAL, _MOL, _CK), TL), !,
	true.

root_pd(TL, PD) :-
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Labe1, '/', [PD| _], _CAL, _MOL, _CK), TL),
	!.
root_pd(_TL, _PD) :-
	tui_msgbox2(['root partition was not found']),
	fail.

root_fs(TL, FS) :-
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	memberchk(fs7(FS, _Labe1, '/', _DL, _CAL, _MOL, _CK), TL), !,
	true.
root_fs(TL, btrfs) :-
	member(fs5_multi(btrfs, _Label, _DL, PTL, _CK), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL), !,
	true.
root_fs(TL, zfs) :-
	member(fs5_multi(zfs, _Label, _DL, PTL, _CK), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.

boot_pref(TL, '') :-
	has_boot_part(TL),
	!.
boot_pref(_TL, 'boot/').

% DAL - default attr list.
make_fs_attr(FS, B, _DAL, OL) :-
	inst_setting(fs_attr(FS, _, B), create(AL1)), !,
	make_fs_attr_(AL1, OL),
	true.
make_fs_attr(_FS, _B, DAL, OL) :-
	make_fs_attr_(DAL, OL),
	true.

make_fs_attr_([], []) :- !.
make_fs_attr_(AL, [o('O', lc(AL))]).

mkfs(bcachefs, _B, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mkfs.bcachefs', o('L', dq(Label)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(ext2, _B, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-F', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(ext3, _B, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-j', '-F', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(ext4, B, Title, DL, Label, _RD) :- !,
	make_fs_attr(ext4, B, [], OL),
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), OL, o(t, ext4), '-F', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(f2fs, B, Title, DL, Label, _RD) :- !,
	make_fs_attr(f2fs, B, [encrypt], OL),
	tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), OL, '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(vfat, _B, Title, DL, Label, _RD) :- !,
	upper(Label, UL),
	tui_progressbox_safe(['mkfs.vfat', o('F', '32'), o('n', dq(UL)), DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(xfs, _B, Title, DL, Label, _RD) :- !,
	tui_progressbox_safe(['mkfs.xfs', o('L', dq(Label)), '-f', '-i', 'sparse=0', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	true.
mkfs(swap, _B, _Title, [PD| _], _Label, _RD) :- !,
	os_shell2_rc([swapoff, PD, '>/dev/null', '2>&1'], _),
	( os_shell2l([mkswap, PD, '2>&1'])
	; tui_msgbox('ERROR: failed to create swap'),
	  fail
	), !,
	( os_shell2l([swapon, PD, '2>&1'])
	; tui_msgbox('ERROR: failed to activate swap'),
	  fail
	), !,
	true.
mkfs(FS, _B, _Title, _, _, _, _RD) :- !,
	tui_msgbox2(['Unknown filesystem', FS]),
	fail.

mkfs_multi(zfs, Title, DL, PTL, _Label, RD) :- !,
	findall(PID, (member(PD, DL), lx_split_dev(PD, _Pref, P), lx_get_dev_id(P, PID)), PIDL),
	% tui_msgbox_w(PIDL),

	% Create a ZFS pool
	% tui_programbox_safe([
	tui_progressbox_safe([
		zpool,
		create,
		o(f),
		o(o, ashift=12),
		o(o, autotrim=on),
		% o(o, compatibility='openzfs-2.1-linux'),
		o('O', compression=lz4),
		o('O', acltype=posixacl),
		o('O', xattr=sa),
		o('O', relatime=on),
		o(m, none),
		zroot,
		PIDL,
		'2>&1'
		], '', [title(Title), sz([12, 80])]
	),
	% export and re-import the pool with a temporary, alternate root path
	os_shell2([zpool, export, zroot]),
	% os_shell2([zpool, import, '-N', o('R', RD), zroot]),
	tui_progressbox_safe([zpool, import, '-N', o('R', RD), zroot, '2>&1'], '', [title('Import pool'), sz([8, 60])]),
	mkfs_multi_zfs(PTL),
	true.
mkfs_multi(btrfs, Title, DL, PTL, Label, RD) :- !,
	tui_progressbox_safe(['mkfs.btrfs', o('L', dq(Label)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	( inst_setting(fs_attr(btrfs, '/', _), mount(AL))
	; AL = [rw, noatime]
	), !,
	DL = [D| _],
	os_call2([mount, o(o, lc(AL)), D, RD]),
	% create_btrfs_subv(RD),
	mkfs_multi_btrfs(PTL, RD),
	os_call2([umount, RD]),
	true.
mkfs_multi(FS, _Title, _DL, _PTL, _Label, _RD) :- !,
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
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	findall(MP, (member(fs7(FS, _Label, MP, _DL, _CAL, _MOL, _CK), TL), FS \= swap), MPL0),
	sort(MPL0, MPL1),
	true.

% Ignore swap partition.
mount_fs(swap, _D, _MP, _RD) :-
	!.
% Ignore empty mount point.
mount_fs(_FS, _D, '', _RD) :-
	!.
mount_fs(FS, D, MP, RD) :-
	memberchk(FS, [vfat, ext2, ext3, ext4, f2fs, xfs, bcachefs]),
	atom_concat(RD, MP, MP1),
	os_mkdir_p(MP1),
	( inst_setting(fs_attr(FS, MP, _), mount(OL))
	; OL = [rw, noatime]
	),
	os_shell2([mount, o(t, FS), o(o, lc(OL)), D, MP1, '2>&1']),
	!.
mount_fs(FS, D, MP, _RD) :-
	tui_msgbox2(['mount_fs has failed.', [FS, D, MP]], [sz([6, 40])]),
	fail.

mount_fs_multi(zfs, _D, _PTL, RD) :-
	% Mount the ZFS hierarchy
	os_call2([zfs, mount, 'zroot/ROOT/void']),
	os_call2([zfs, mount, '-a']),
	% Update device symlinks
	os_call2([udevadm, trigger]),

	% record the current pool configuration in a cache file that Void will use to avoid walking the entire device hierarchy to identify importable pools.
	os_mkdir_p(RD + '/etc/zfs'),
	os_call2([zpool, set, cachefile=concat(RD, '/etc/zfs/zpool.cache'), zroot]),
	!.
mount_fs_multi(btrfs, D, PTL, RD) :-
	mount_btrfs_muli(D, PTL, RD),
	!.
mount_fs_multi(FS, D, _PTL, _RD) :-
	tui_msgbox2(['mount_fs_multi has failed.', FS, D], [sz([6, 40])]),
	fail.

umount_mnt(RD) :-
	os_shell2([umount, '--recursive', RD, '2>/dev/nul']),
	fail.
umount_mnt(RD) :-
	zpool_list(L),
	memberchk(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,RD), L),
	% tui_progressbox_safe([zpool, export, '-f', PN, '2>&1'], '', [title(' exporting zpool '), sz([6, 40])]),
	tui_progressbox_safe([zpool, destroy, '-f', PN, '2>&1'], '', [title(' destroying zpool '), sz([6, 40])]),
	fail.
umount_mnt(_RD).

% vg(Name, [PhysicalVolumeList], [LogicalVolumeList])
ensure_lvm(TL) :-
	% Check for already tacken VG-LV pairs.
	lx_list_dev_part(PL),
	member(bdev(lvm, vg(VG, _, LVL)), TL),
	member(lv(LV, _SZ), LVL),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	member(dev_part(LVM_PD,_,_,_), PL), !,
	PL = [dev_part(D2,_,_,_)| _],
	format_to_atom(M, 'LVM device ~w (VG: ~w, LV: ~w) already exists on the device ~w', [LVM_PD, VG, LV, D2]),
	tui_msgbox(M, [title(' Ensure LVM ERROR ')]), !,
	fail.
ensure_lvm(TL) :-
	% Check for already tacken VG.
	findall(VG1, member(bdev(lvm, vg(VG1, _, _LVL)), TL), VGL),
	sort(VGL, SVGL),
	lvm_pvs(L),
	member(VG, SVGL),
	memberchk(pv(PV, VG), L),
	format_to_atom(M, 'Volume group called "~w" already exists on the device ~w', [VG, PV]),
	tui_msgbox(M, [title(' Ensure LVM ERROR ')]), !,
	fail.
ensure_lvm(_TL) :-
	% halt,
	true.

