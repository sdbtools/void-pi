% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% template_info(name, descr, except_fs).
template_info(manual, 'Manual configuration of everything', []).
template_info(gpt_basic, 'GPT', [swap, vfat]).
template_info(gpt_lvm, 'GPT. LVM', [swap, vfat]).
template_info(gpt_lvm_luks1, 'GPT. LVM. LUKS1', [swap, vfat]).
template_info(gpt_luks1, 'GPT. LUKS1. One device', [swap, vfat]).
template_info(gpt_luks1_lvm, 'GPT. LUKS1. LVM. One device', [swap, vfat]).
template_info(gpt_raid, 'GPT. RAID. One device', [swap]).
template_info(gpt_zfsbootmenu, 'GPT. ZFS. One device', []).

setup_fs_template :-
	% subv(name, mount_point, mount_attrs, cow)
	assertz(inst_setting(btrfs, subv('@', mp('/'), [rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	% assertz(inst_setting(btrfs, subv('@home', mp('/home'), [nosuid, nodev, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@home', mp('/home'), [nosuid, nodev, rw, noatime, space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@opt', mp('/opt'), [nodev, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@srv', mp('/srv'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var', mp('/var'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@var-cache-xbps', mp('/var/cache/xbps'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@var-lib-ex', mp('/var/lib/ex'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-log', mp('/var/log'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-opt', mp('/var/opt'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@var-spool', mp('/var/spool'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-tmp', mp('/var/tmp'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@snapshots', mp('/.snapshots'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),

	% dataset(name, mount_point, mount_attrs)
	assertz(inst_setting(zfs, dataset('ROOT', 'none', ['canmount=off']))),
	assertz(inst_setting(zfs, dataset('ROOT/void', '/', ['canmount=noauto', 'atime=off']))),
	% assertz(inst_setting(zfs, dataset('ROOT/void', '/', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('home', '/home', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('opt', '/opt', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('srv', '/srv', ['atime=off']))),
	% assertz(inst_setting(zfs, dataset('usr', '/usr', ['canmount=off', 'atime=off']))),
	% assertz(inst_setting(zfs, dataset('usr-local', '/usr/local', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('tmp', '/tmp', ['com.sun:auto-snapshot=false', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var', '/var', ['canmount=off', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var-lib', '/var/lib', ['canmount=off', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var-cache-xbps', '/var/cache/xbps', ['com.sun:auto-snapshot=false', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var-lib-ex', '/var/lib/ex', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-log', '/var/log', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-opt', '/var/opt', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-spool', '/var/spool', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-tmp', '/var/tmp', ['com.sun:auto-snapshot=false', 'atime=off']))),
	% os_shell2([zfs, create, '-o', 'mountpoint=/var/tmp', 'zroot/var-tmp']),

	true.

% OT - old template
% NT - new template
% NB - new bootloader.
switch_template(OT, OT, NB) :- !,
	inst_setting(template(OT), TL),
	memberchk(bootloader(OB, _BD), TL),
	( OB = NB
	; make_cmd_list(manual, NB, L),
	  retractall(inst_setting(template(OT), _)),
	  assertz(inst_setting(template(OT), L))
	),
	!.
switch_template(OT, NT, NB) :-
	make_cmd_list(NT, NB, L),
	retractall(inst_setting(template(OT), _)),
	assertz(inst_setting(template(NT), L)),
	true.

fs2parttype(zfs, solaris_root).
fs2parttype(_, linux).

% need_boot_part(TemplateType, BootLoader, FileSystem).
need_boot_part(TT, B, _FS) :-
	member(B, [rEFInd, limine]),
	member(TT, [gpt_lvm, gpt_lvm_luks1, gpt_luks1, gpt_luks1_lvm]),
	!.
need_boot_part(_TT, B, FS) :-
	% bootloader_info(bootloade, supported_fs, supported_template).
	bootloader_info(B, FSL, _),
	\+ member(FS, FSL),
	!.

% B - bootloader.
make_cmd_list(manual, B, [bootloader(B, _)]) :- !.
make_cmd_list(gpt_zfsbootmenu, B, [bootloader(B, _)]) :- !,
	tui_msgbox('not implemented yet'),
	true.
make_cmd_list(TT, B, [bootloader(B, DEV3)| L]) :- !,
	menu_dev7_combo(TT, LN1, SN1, DL1),
	menu_root_fs(TT),
	% Put boot device first.
	part_name(SN1, 1, D1),
	partition_set_mbr(TT, B, [d4(LN1, SN1, D1, 1)| DL1], L),
	lx_make_dev3(LN1, DEV3),
	true.

% Select devices to use and a boot device.
menu_dev7_combo(TT, LN1, SN1, DL1) :-
	% multi-device templates.
	memberchk(TT, [gpt_basic, gpt_lvm, gpt_lvm_luks1]), !,
	menu_dev7_checklist_light(' Select device(s) to use ', DL0),
	menu_dev71_menu(' Select boot device ', DL0, DEV71),
	lx_dev7_to_ldn_sdn(DEV71, LN1, SN1),
	findall(d4(LN2, SN2, D2, 1), (member(DEV72, DL0), lx_dev7_to_ldn_sdn(DEV72, LN2, SN2), LN2 \= LN1, part_name(SN2, 1, D2)), DL1),
	true.
menu_dev7_combo(_TT, LN1, SN1, []) :-
	inst_setting(dev7, available(DL0)),
	menu_dev71_menu(' Select boot device ', DL0, DEV71),
	lx_dev7_to_ldn_sdn(DEV71, LN1, SN1),
	true.

enable_template(TT, B) :-
	make_cmd_list(TT, B, L),
	retractall(inst_setting(template(_), _)),
	assertz(inst_setting(template(TT), L)),
	true.

% p4(PartType, device, create/keep, size)
% fs4(FileSystem, Label, MountPoint, [device_list])
% N - partition number.
% B - bootloader.
% P - partition.
% vg(Name, [PhysicalVolumeList], [LogicalVolumeList])
partition_set_mbr(TT, B, P, L) :-
	inst_setting(system(efi), _), !,
	partition_set_efi(TT, B, P, L).
partition_set_mbr(TT, B, [d4(D, SN, _SDN, N)| T], [p4(bios_boot, bd1([PD, D]), keep, MBR_SZ)| L]) :-
	part_name(D, N, PD),
	inst_setting(mbr_size, MBR_SZ),
	N1 is N + 1,
	part_name(SN, N1, SD1),
	partition_set_efi(TT, B, [d4(D, SN, SD1, N1)| T], L).

partition_set_efi(TT, B, [d4(D, SN, _SDN, N)| T], [p4(efi_system, bd1([PD, D]), create, ESP_SZ), fs4(vfat, efi, '/boot/efi', [PD])| L]) :-
	part_name(D, N, PD),
	inst_setting(esp_size, ESP_SZ),
	N1 is N + 1,
	part_name(SN, N1, SD1),
	partition_set_boot(TT, B, [d4(D, SN, SD1, N1)| T], L).

partition_set_boot(TT, B, [d4(D, SN, _SDN, N)| T], [p4(linux, bd1([PD, D]), create, BOOT_SZ), fs4(ext4, boot, '/boot', [PD])| L]) :-
	inst_setting(fs_info, info('/', FS)),
	need_boot_part(TT, B, FS), !,
	part_name(D, N, PD),
	inst_setting(boot_size, BOOT_SZ),
	N1 is N + 1,
	part_name(SN, N1, SD1),
	partition_set_template(TT, [d4(D, SN, SD1, N1)| T], L).
partition_set_boot(TT, _B, P, L) :-
	partition_set_template(TT, P, L).

partition_set_template(gpt_lvm, DL, L) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	% menu_d4_checklist_light(' Select device(s) to use with LVM ', DL, DL0),
	% DL0 \= [],
	% maplist(d4_to_p4_pd(linux_lvm), DL0, P4L, PDL),
	maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	inst_setting(fs_info, info('/', FS)),
	append(P4L, [bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), fs4(FS, void, '/', [LVM_PD])], L),
	true.

partition_set_template(gpt_lvm_luks1, DL, L) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	format_to_atom(LVM_PD_SHORT, '~w-~w', [VG, LV]),
	luks_dev_name(LVM_PD_SHORT, LUKS_PD),
	inst_setting(fs_info, info('/', FS)),
	append(P4L, [bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), bdev(luks, luks(luks1, LVM_PD)), fs4(FS, void, '/', [LUKS_PD])], L),
	true.

partition_set_template(gpt_luks1, [d4(D, _SN, SDN, N)| _T], [p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(luks1, PD)), fs4(FS, void, '/', [LUKS_PD])]) :- !,
	part_name(D, N, PD),
	luks_dev_name(SDN, LUKS_PD),
	inst_setting(fs_info, info('/', FS)),
	true.
% !!! DO NOT delete !!!
% multi-device support.
% partition_set_template(gpt_luks1, DL, L) :- !,
% 	DL = [D41| _T],
% 	d4_to_luks_pd(D41, LUKS_PD),
% 	% menu_d4_checklist_light(' Select device(s) to use with LUKS ', DL, DL0),
% 	% DL0 \= [],
% 	% maplist(d4_to_luks_bdev, DL0, P4L, BDEVL),
% 	maplist(d4_to_luks_bdev, DL, P4L, BDEVL),
% 	inst_setting(fs_info, info('/', FS)),
% 	flatten([P4L, BDEVL, fs4(FS, void, '/', [LUKS_PD])], L),
% 	true.
partition_set_template(gpt_luks1_lvm, [d4(D, _SN, SDN, N)| _T], [p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(luks1, PD)), bdev(lvm, vg(VG, [LUKS_PD], [lv(LV, SZ)])), fs4(FS, void, '/', [LVM_PD])]) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	part_name(D, N, PD),
	luks_dev_name(SDN, LUKS_PD),
	inst_setting(fs_info, info('/', FS)),
	true.
% !!! DO NOT delete !!!
% multi-device support.
% partition_set_template(gpt_luks1_lvm, DL, L) :- !,
% 	maplist(d4_to_luks_bdev, DL, P4L, BDEVL),
% 	inst_setting(lvm, lv(VG, LV, SZ)),
% 	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
% 	maplist(d4_to_luks_pd, DL, PDL),
% 	inst_setting(fs_info, info('/', FS)),
% 	flatten([P4L, BDEVL, bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), fs4(FS, void, '/', [LVM_PD])], L),
% 	true.
partition_set_template(_, DL, L) :-
	inst_setting(fs_info, info('/', FS)),
	fs_to_p4l_pdl(FS, DL, P4L, PDL),
	append(P4L, [fs4(FS, void, '/', PDL)], L),
	true.

fs_to_p4l_pdl(btrfs, DL, P4L, PDL) :- !,
	fs2parttype(btrfs, PT),
	maplist(d4_to_p4_pd(PT), DL, P4L, PDL),
	true.
fs_to_p4l_pdl(FS, [H| _T], [P4], [PD]) :-
	fs2parttype(FS, PT),
	d4_to_p4_pd(PT, H, P4, PD),
	true.

d4_to_p4_pd(PT, d4(D, _SN, _SDN, N), p4(PT, bd1([PD, D]), create, ''), PD) :-
	part_name(D, N, PD),
	true.

d4_to_luks_bdev(d4(D, _SN, _SDN, N), p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(luks1, PD))) :-
	part_name(D, N, PD),
	true.

d4_to_luks_pd(d4(_D, _SN, SDN, _N), LUKS_PD) :-
	luks_dev_name(SDN, LUKS_PD).

part_name(D, N, PD) :-
	atom_concat('/dev/nvme', _, D), !,
	format_to_atom(PD, '~wp~d', [D, N]).
part_name(D, N, PD) :-
	format_to_atom(PD, '~w~d', [D, N]).

