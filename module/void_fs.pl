% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

has_boot_part :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(_FS, _Label, '/boot', _BD1)), !,
	true.

has_root_part :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(_FS, _Label, '/', _BD1)), !,
	true.

has_efi_system_part :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(vfat, _Label, '/boot/efi', _BD1)), !,
	true.

has_usr_part :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(_FS, _Label, '/usr', _BD1)), !,
	true.

root_pd(PD) :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(_FS, _Labe1, '/', bd1([PD| _]))),
	% inst_setting(fs, fs4(_FS, _Labe1, '/', bd1(L))),
	% append(_, [PD, D], L),
	!.
root_pd(_PD) :-
	tui_msgbox2(['root partition was not found']),
	fail.

boot_pref('') :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(_Name, _Labe1, '/boot', _BD1)),
	!.
boot_pref('boot/').

mkfs(RD) :-
	% tui_msgbox('mkfs'),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(FS, Label, _MP, bd1([PD| _]))),
	% modprobe
	( FS = swap; FS = lvm; FS = luks1 ->
	  true
	; ( os_call2([modprobe, FS]) ->
	    true
	  ; tui_msgbox2([modprobe, FS, has, failed]),
	    fail
	  ),
	  true
	),
	mkfs(FS, PD, Label, RD),
	fail.
mkfs(_).

mkfs(zfs, PD, Label, RD) :- !,
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
mkfs(btrfs, PD, Label, RD) :- !,
	tui_progressbox_safe(['mkfs.btrfs', o('L', dq(Label)), '-f', PD, '2>&1'], 'Creating filesystem btrfs', [sz([12, 80])]),
	% tui_programbox_safe(['mkfs.btrfs', '-f', '-L', LQ, PD, '2>&1'], 'Creating filesystem btrfs', [sz([12, 80])]),
	create_btrfs_subv(PD, RD).
mkfs(ext2, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-F', PD, '2>&1'], 'Creating filesystem ext2', [sz([12, 80])]),
	true.
mkfs(ext3, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), '-j', '-F', PD, '2>&1'], 'Creating filesystem ext3', [sz([12, 80])]),
	true.
mkfs(ext4, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mke2fs', o('L', dq(Label)), o(t, ext4), '-F', PD, '2>&1'], 'Creating filesystem ext4', [sz([12, 80])]),
	% tui_programbox_safe(['mke2fs', '-F', '-t', 'ext4', PD, '2>&1'], 'Creating filesystem ext4', [sz([24, 80])]),
	true.
mkfs(f2fs, PD, Label, _RD) :- !,
	( inst_setting(fs_attr(f2fs, _), create(OL))
	; OL = [encrypt]
	),
	% tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), '-f', PD, '2>&1'], 'Creating filesystem f2fs', [sz([12, 80])]),
	tui_progressbox_safe(['mkfs.f2fs', o(l, dq(Label)), o('O', lc(OL)), '-f', PD, '2>&1'], 'Creating filesystem f2fs', [sz([12, 80])]),
	true.
mkfs(vfat, PD, Label, _RD) :- !,
	upper(Label, UL),
	tui_progressbox_safe(['mkfs.vfat', o('F', '32'), o('n', dq(UL)), PD, '2>&1'], 'Creating filesystem vfat', [sz([12, 80])]),
	% tui_programbox_safe(['mkfs.vfat', '-F', '32', '-n', dq(UL), PD, '2>&1'], 'Creating filesystem vfat', [sz([12, 80])]),
	true.
mkfs(xfs, PD, Label, _RD) :- !,
	tui_progressbox_safe(['mkfs.xfs', o('L', dq(Label)), '-f', '-i', 'sparse=0', PD, '2>&1'], 'Creating filesystem xfs', [sz([12, 80])]),
	true.
mkfs(swap, PD, _Label, _RD) :- !,
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
mkfs(FS, _, _, _, _RD) :- !,
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
	tui_infobox('Creating crypto-device.', [sz([4, 40])]),
	( lx_luks_format(Type, PD, RPWD)
	; tui_msgbox('luks_format has failed'),
	  fail
	), !,
	inst_setting(luks, luks(Name)),
	tui_infobox('Opening crypto-device.', [sz([4, 40])]),
	( lx_luks_open(Type, Name, PD, RPWD)
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

mount_fs(RD) :-
	get_mp_list(MPL),
	maplist(mount_mp(RD), MPL),
	true.

% Get list of mounting points in order in which they should be mounted (except of swap).
get_mp_list(MPL1) :-
	% Ignore swap partition
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	findall(MP, (inst_setting(fs, fs4(FS, _Label, MP, _BD1)), FS \= swap), MPL0),
	sort(MPL0, MPL1),
	true.

mount_mp(RD, MP) :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(FS, _Label, MP, bd1([PD| _]))),
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
	% tui_msgbox2([before, zfs, mount, 'zroot/ROOT/void']),
	% % Mount the ZFS hierarchy
	% % os_shell2([zfs, mount, 'zroot/ROOT/void']),
	% tui_programbox_safe([zfs, mount, 'zroot/ROOT/void', '2>&1'], '', [title(' zfs mount zroot/ROOT/void '), sz([12, 80])]),
	% tui_msgbox2([after, zfs, mount, 'zroot/ROOT/void']),
	% os_shell2([zfs, mount, '-a']),
	% tui_msgbox2([after, zfs, mount, '-a']),
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
	( inst_setting(fs_attr(FS, MP), mount(OL))
	; OL = [rw, noatime]
	),
	os_shell2([mount, o(t, FS), o(o, lc(OL)), D, MP1, '2>&1']),
	!.
mount_fs(FS, D, MP, _RD) :-
	tui_msgbox2(['mount_fs has failed.', [FS, D, MP]], [sz([6, 40])]),
	fail.

clean_mnt(RD) :-
	os_shell2([umount, oo(recursive), RD, '2>/dev/nul']),
	fail.
clean_mnt(_RD) :-
	% LVM
	inst_setting(bootloader_dev, dev3(D, _, _)),
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

