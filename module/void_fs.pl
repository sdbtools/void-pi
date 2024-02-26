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
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Label, '/boot', _D, _COL, _MOL, _CK), TL), !,
	true.

has_root_part(TL) :-
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Label, '/', _D, _COL, _MOL, _CK), TL), !,
	true.
has_root_part(TL) :-
	member(fs5_multi(btrfs, _Label, _DL, PTL, _CK, _B, _E), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL), !,
	true.
has_root_part(TL) :-
	member(fs5_multi(zfs, _Label, _DL, PTL, _CK, _B, _E), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.

has_usr_part(TL) :-
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Label, '/usr', _D, _COL, _MOL, _CK), TL), !,
	true.

root_pd(TL, PD) :-
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs7(_FS, _Labe1, '/', PD, _COL, _MOL, _CK), TL),
	!.
root_pd(TL, PD) :-
	member(fs5_multi(btrfs, _Label, [PD|_], PTL, _CK, _B, _E), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL),
	!.
root_pd(TL, PD) :-
	member(fs5_multi(zfs, _Label, [PD|_], PTL, _CK, _B, _E), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.
root_pd(_TL, _PD) :-
	tui_msgbox2(['root partition was not found']),
	fail.

root_fs(TL, FS) :-
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs7(FS, _Labe1, '/', _D, _COL, _MOL, _CK), TL), !,
	true.
root_fs(TL, btrfs) :-
	member(fs5_multi(btrfs, _Label, _DL, PTL, _CK, _B, _E), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL), !,
	true.
root_fs(TL, zfs) :-
	member(fs5_multi(zfs, _Label, _DL, PTL, _CK, _B, _E), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.

boot_pref(TL, '') :-
	has_boot_part(TL),
	!.
boot_pref(_TL, 'boot/').

get_mkfs_attrs([], []) :- !.
get_mkfs_attrs(COL, [o('O', lc(COL))]).

mkfs(swap, PD, _COL, _Label) :- !,
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
mkfs(FS, D, COL, Label) :-
	get_mkfs_attrs(COL, OL),
	mkfs_cl(FS, D, OL, Label, CL),
	format_to_atom(Title, ' Creating filesystem ~w ', [FS]),
	tui_progressbox_safe(CL, '', [title(Title), sz([12, 80])]),
	true.
mkfs(FS, _, _, _) :- !,
	tui_msgbox2(['Unknown filesystem', FS]),
	fail.

mkfs_cl(ext2, D, OL, Label, CL) :- !,
	CL = ['mke2fs', o('L', dq(Label)), OL, '-F', D, '2>&1'],
	true.
mkfs_cl(ext3, D, OL, Label, CL) :- !,
	CL = ['mke2fs', o('L', dq(Label)), OL, '-j', '-F', D, '2>&1'],
	true.
mkfs_cl(ext4, D, OL, Label, CL) :- !,
	CL = ['mke2fs', o('L', dq(Label)), OL, o(t, ext4), '-F', D, '2>&1'],
	true.
mkfs_cl(f2fs, D, OL, Label, CL) :- !,
	CL = ['mkfs.f2fs', o(l, dq(Label)), OL, '-f', D, '2>&1'],
	true.
mkfs_cl(vfat, D, OL, Label, CL) :- !,
	upper(Label, UL),
	CL = ['mkfs.vfat', o('F', '32'), o('n', dq(UL)), OL, D, '2>&1'],
	true.
mkfs_cl(xfs, D, OL, Label, CL) :- !,
	CL = ['mkfs.xfs', o('L', dq(Label)), OL, '-f', '-i', 'sparse=0', D, '2>&1'],
	true.
mkfs_cl(bcachefs, D, OL, Label, CL) :- !,
	CL = ['mkfs.bcachefs', o('L', dq(Label)), OL, '-f', D, '2>&1'],
	true.

mkfs_multi(zfs, Title, TL, DL, PTL, _Label, B, E, RD) :- !,
	zfs_zpool_create(Title, TL, DL, PTL, B, E, RD),
	true.
mkfs_multi(btrfs, Title, _TL, DL, PTL, Label, _B, _E, RD) :- !,
	btrfs_mkfs(Title, DL, PTL, Label, RD),
	true.
mkfs_multi(FS, _Title, _TL, _DL, _PTL, _Label, _B, _E, _RD) :- !,
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
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	findall(MP, (member(fs7(FS, _Label, MP, _D, _COL, _MOL, _CK), TL), FS \= swap), MPL0),
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
	zfs_mount_muli(RD),
	!.
mount_fs_multi(btrfs, D, PTL, RD) :-
	btrfs_mount_muli(D, PTL, RD),
	!.
mount_fs_multi(FS, D, _PTL, _RD) :-
	tui_msgbox2(['mount_fs_multi has failed.', FS, D], [sz([6, 40])]),
	fail.

umount_mnt(RD) :-
	os_shell2([umount, '--recursive', RD, '2>/dev/nul']),
	fail.
umount_mnt(RD) :-
	zfs_destroy_pool_rd(RD),
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

get_boot_part(TL, PD) :-
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	member(fs7(_FS, _Label, MP, PD, _, _, _CK1), TL),
	( MP = '/boot'; MP = (/) ), !,
	% memberchk(p4(_, bd1([PD| _]), _CK2, _SZ), TL),
	!.
get_boot_part(TL, PD) :-
	member(fs5_multi(FS, _Label, [PD|_], PTL, _CK1, _B, _E), TL),
	get_boot_part_1(FS, PTL),
	!.

get_boot_part_1(zfs, PTL) :- !,
	member(dataset(_, MP, _), PTL),
	( MP = '/boot'; MP = (/) ), !,
	true.
get_boot_part_1(btrfs, PTL) :- !,
	member(subv(_, MP, _, _), PTL),
	( MP = '/boot'; MP = (/) ), !,
	true.

