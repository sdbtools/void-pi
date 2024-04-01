% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/Dm-crypt
% https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system
% https://wiki.archlinux.org/title/Dm-crypt/System_configuration
% Stacking LVM volumes: https://access.redhat.com/articles/2106521
% https://wiki.syslinux.org/wiki/index.php?title=Filesystem#ext
% Free Software EFI/UEFI file system drivers: https://efi.akeo.ie/
% EfiFs - EFI File System Drivers: https://github.com/pbatard/efifs

has_boot_part(TL) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs6(_FS, '/boot', _D, _COL, _MOL, _CK), TL), !,
	true.

has_root_part(TL) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs6(_FS, '/', _D, _COL, _MOL, _CK), TL), !,
	true.
has_root_part(TL) :-
	member(fs5_multi(btrfs, _COL, _DL, PTL, _CK), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL), !,
	true.
has_root_part(TL) :-
	member(fs5_multi(zfs, _COL, _DL, PTL, _CK), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.

has_usr_part(TL) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs6(_FS, '/usr', _D, _COL, _MOL, _CK), TL), !,
	true.

root_pd(TL, PD) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs6(_FS, '/', PD, _COL, _MOL, _CK), TL),
	!.
root_pd(TL, PD) :-
	member(fs5_multi(btrfs, _COL, [PD|_], PTL, _CK), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL),
	!.
root_pd(TL, PD) :-
	member(fs5_multi(zfs, _COL, [PD|_], PTL, _CK), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.
root_pd(_TL, _PD) :-
	tui_msgbox2(['root partition was not found']),
	fail.

root_fs(TL, FS) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	memberchk(fs6(FS, '/', _D, _COL, _MOL, _CK), TL), !,
	true.
root_fs(TL, btrfs) :-
	member(fs5_multi(btrfs, _COL, _DL, PTL, _CK), TL),
	memberchk(subv(_Name, '/', _MOL, _), PTL), !,
	true.
root_fs(TL, zfs) :-
	member(fs5_multi(zfs, _COL, _DL, PTL, _CK), TL),
	memberchk(dataset(_, '/', _), PTL), !,
	true.

get_boot_part(TL, PD) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	member(fs6(_FS, MP, PD, _, _, _CK1), TL),
	( MP = '/boot'; MP = (/) ), !,
	% memberchk(p4(_, bd1([PD| _]), _CK2, _SZ), TL),
	!.
get_boot_part(TL, PD) :-
	member(fs5_multi(FS, _COL, [PD|_], PTL, _CK1), TL),
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

boot_pref(TL, '') :-
	has_boot_part(TL),
	!.
boot_pref(_TL, 'boot/').

get_mkfs_attrs(COL, L) :-
	findall(LM, (member(CO, COL), get_mkfs_attrs0(CO, L0), member(LM, L0)), L),
	true.

get_mkfs_attrs0(attr(TAG, OL), L) :-
	prop_info(TAG, Format, FL),
	get_mkfs_attrs1(Format, TAG, FL, OL, L),
	true.

get_mkfs_attrs1(feat4p(Opt, _Fmt), zpool_feat, _FL, OL, COL) :- !, % "p" stands for "prefix"
	findall(CO, (member(F=V, OL), make_zfs_feat(V, F, O), member(CO, [Opt, O])), COL),
	true.
get_mkfs_attrs1(feat4p(Opt, Fmt), _TAG, FL, OL, COL) :- !, % "p" stands for "prefix"
	findall(CO, (member(F=V, OL), memberchk(prop_feat4(F, _, LFmt, _), FL), make_fs_feat(LFmt, Fmt, F, V, O), member(CO, [Opt, O])), COL),
	true.
get_mkfs_attrs1(feat4s(Opt, Sep, Fmt), _TAG, FL, OL, [Opt, l(COL, Sep)]) :- !, % "s" stands for "separator"
	findall(O, (member(F=V, OL), memberchk(prop_feat4(F, _, LFmt, _), FL), make_fs_feat(LFmt, Fmt, F, V, O)), COL),
	true.
get_mkfs_attrs1(opt3s(Opt, Sep), _TAG, FL, OL, [Opt, l(COL, Sep)]) :- !, % "s" stands for "separator"
	findall(O, (member(P=V, OL), make_opt3_val(FL, P, V, O)), COL),
	true.
get_mkfs_attrs1(opt3p(Opt), _TAG, FL, OL, COL) :- !, % "p" stands for "prefix"
	findall(CO, (member(P=V, OL), make_opt3_val(FL, P, V, OV), member(CO, [Opt, OV])), COL),
	true.
get_mkfs_attrs1(opt4s(Sep), _TAG, FL, OL, COL) :- !,
	findall(O, (member(P=V, OL), make_opt4_val(FL, P, V, Sep, O)), COL),
	true.
get_mkfs_attrs1(Format, _TAG, _FL, _OL, _COL) :-
	format_to_atom(A, 'Invalid formatter: ~w', [Format]),
	tui_msgbox(A, [title(' ERROR ')]),
	fail.

% make_fs_feat(LocalFormat, Format, V, F, FV).
make_fs_feat(std, Fmt, F, V, FV) :- !,
	make_fs_feat0(V, Fmt, F, FV).
make_fs_feat(LFmt, _Fmt, F, V, FV) :-
	make_fs_feat0(V, LFmt, F, FV).

make_fs_feat0(off, on_off, _F, '') :- !,
	fail.
make_fs_feat0(off, pref1(P), F, FV) :- !,
	atom_concat(P, F, FV).
make_fs_feat0(off, v2(_ON, OFF), _F, OFF) :- !.
make_fs_feat0(off, Fmt, _F, '') :- !,
	format_to_atom(A, 'Invalid formatter: ~w', [Fmt]),
	tui_msgbox(A, [title(' ERROR ')]),
	fail.
make_fs_feat0(on, v2(ON, _OFF), _F, ON) :- !.
make_fs_feat0(_, _Fmt, F, F) :- !.

make_zfs_feat(V, F, FV) :- !,
	( V = on ->
	  VA = enabled
	; VA = disabled
	),
	format_to_atom(FV, 'feature@~w=~w', [F, VA]).

make_opt4_val(PL, P, V, Sep, PVL) :-
	memberchk(opt4(P, Fmt, _, AN), PL),
	make_opt4_val0(Fmt, AN, V, Sep, PVL),
	true.

make_opt4_val0(enable, AN, _V, _Sep, AN) :- !.
make_opt4_val0(Fmt, AN, V, Sep, vs(AN, Sep, OV)) :-
	make_opt_dq(Fmt, V, OV).

make_opt3_val(PL, P, V, OV) :-
	( memberchk(opt3(P, Fmt, _), PL)
	; memberchk(opt3a(P, Fmt, _, _), PL)
	), !,
	make_opt3_val0(P, V, Fmt, OV),
	true.

make_opt3_val0(keylocation, =(file, V), _Fmt, keylocation = dq(AV)) :- !,
	atom_concat('file://', V, AV),
	true.
make_opt3_val0(P, =(_, V), Fmt, P = DQV) :- !,
	make_opt_dq(Fmt, V, DQV),
	true.
make_opt3_val0(P, V, Fmt, P = DQV) :-
	make_opt_dq(Fmt, V, DQV),
	true.

make_opt_dq(Fmt, V, dq(V)) :-
	opt_require_dq(Fmt, V), !.
make_opt_dq(_Fmt, V, V).

opt_require_dq(Fmt, V) :-
	memberchk(Fmt, [str, file, path]),
	sub_atom(V, _Before, _Length, _After, ' '), !,
	true.

mkfs(swap, PD, _COL) :- !,
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
mkfs(FS, D, COL) :-
	get_mkfs_attrs(COL, OL),
	mkfs_cl(FS, D, OL, CL),
	format_to_atom(Title, ' Creating filesystem ~w ', [FS]),
	tui_progressbox_safe(CL, '', [title(Title), sz([12, 80])]),
	!.
mkfs(FS, _, _) :- !,
	tui_msgbox2(['Couldn\'t create filesystem', FS]),
	fail.

mkfs_cl(ext2, D, OL, ['mke2fs', OL, D, '2>&1']) :- !.
mkfs_cl(ext3, D, OL, ['mke2fs', OL, D, '2>&1']) :- !.
mkfs_cl(ext4, D, OL, ['mke2fs', OL, D, '2>&1']) :- !.
mkfs_cl(f2fs, D, OL, CL) :- !,
	CL = ['mkfs.f2fs', OL, D, '2>&1'],
	true.
mkfs_cl(vfat, D, OL, CL) :- !,
	CL = ['mkfs.vfat', OL, D, '2>&1'],
	true.
mkfs_cl(xfs, D, OL, CL) :- !,
	CL = ['mkfs.xfs', OL, D, '2>&1'],
	true.
mkfs_cl(bcachefs, D, OL, CL) :- !,
	CL = ['mkfs.bcachefs', OL, D, '2>&1'],
	true.
mkfs_cl(nilfs2, D, OL, CL) :- !,
	CL = ['mkfs.nilfs2', OL, D, '2>&1'],
	true.

mkfs_multi(zfs, Title, TL, DL, PTL, COL, RD) :- !,
	get_bootloader(TL, B),
	% tui_msgbox_w(OL, [title(mkfs_multi)]),
	( zfs_pool_encryption_col(COL) ->
	  E = true
	; E = false
	),
	PN = zroot,
	zfs_zpool_create(E, Title, B, PN, DL, PTL, COL, RD),
	true.
mkfs_multi(btrfs, Title, _TL, DL, PTL, COL, RD) :- !,
	get_mkfs_attrs(COL, OL),
	btrfs_mkfs(Title, DL, PTL, OL, RD),
	true.
mkfs_multi(FS, _Title, _TL, _DL, _PTL, _COL, _RD) :- !,
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
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	findall(MP, (member(fs6(FS, MP, _D, _COL, _MOL, _CK), TL), FS \= swap), MPL0),
	sort(MPL0, MPL1),
	true.

% Ignore swap partition.
mount_fs(FS, _D, _MP, _RD) :-
	memberchk(FS, [proc, tmpfs, swap]), !.
% Ignore empty mount point.
mount_fs(_FS, _D, '', _RD) :-
	!.
mount_fs(FS, D, MP, RD) :-
	memberchk(FS, [vfat, ext2, ext3, ext4, f2fs, xfs, bcachefs]),
	atom_concat(RD, MP, MP1),
	os_mkdir_p(MP1),
	CL = [mount, o(t, FS), D, MP1, '2>&1'],
	% os_shell2(CL),
	tui_shell2_safe(CL),
	!.
mount_fs(FS, D, MP, _RD) :-
	tui_msgbox2(['mount_fs has failed.', [FS, D, MP]], [sz([6, 40])]),
	fail.

mount_fs_multi(zfs, _D, _PTL, RD) :-
	PN = zroot,
	zfs_mount_multi(PN, RD),
	!.
mount_fs_multi(btrfs, D, PTL, RD) :-
	btrfs_mount_multi(D, PTL, RD),
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
	% Check for already taken VG-LV pairs.
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
	% Check for already taken VG.
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

