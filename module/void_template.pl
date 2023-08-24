% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% template_info(name, descr, except_fs).
template_info(manual, 'Manual configuration of everything', []).
template_info(gpt_wizard, 'GPT. Wizard', [swap, vfat]).
template_info(gpt_basic, 'GPT. Basic', [swap, vfat]).
template_info(gpt_lvm, 'GPT. LVM', [swap, vfat]).
template_info(gpt_lvm_luks, 'GPT. LVM. LUKS', [swap, vfat]).
template_info(gpt_luks, 'GPT. LUKS. One device', [swap, vfat]).
template_info(gpt_luks_lvm, 'GPT. LUKS. LVM. One device', [swap, vfat]).
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
% OB - old bootloader.
% NB - new bootloader.
switch_template(OT, OT, OB, OB) :- !.
switch_template(OT, NT, _OB, NB) :-
	make_cmd_list(NT, NB, L),
	retractall(inst_setting(template(OT), _)),
	assertz(inst_setting(template(NT), L)),
	!.

fs2parttype(zfs, solaris_root).
fs2parttype(_, linux_data).

% need_boot_part(TemplateType, BootLoader, FileSystem).
need_boot_part(TT, B, _FS) :-
	member(B, [rEFInd, limine, syslinux, gummiboot]),
	member(TT, [gpt_lvm, gpt_lvm_luks, gpt_luks, gpt_luks_lvm]),
	!.
need_boot_part(_TT, B, FS) :-
	% bootloader_info(bootloade, supported_fs, supported_template, _).
	bootloader_info(B, FSL, _, _),
	\+ member(FS, FSL),
	!.

% B - bootloader.
make_cmd_list(manual, B, [bootloader(B)]) :- !.
make_cmd_list(gpt_zfsbootmenu, B, [bootloader(B)]) :- !,
	tui_msgbox('not implemented yet'),
	true.
make_cmd_list(gpt_wizard, B, [bootloader(B)| L]) :- !,
	menu_dev7_checklist_used_light(' Select device(s) to use ', DEV7L),
	menu_wiz_action(DEV7L, L),
	true.
make_cmd_list(TT, B, [bootloader(B), bootloader_dev(DEV3)| L]) :- !,
	menu_dev7_combo(TT, DEV7L),
	menu_root_fs(TT, B, FS),
	maplist(menu_dev7_to_d4, DEV7L, D4L),
	partition_set_mbr(TT, B, FS, D4L, L),
	D4L = [d4(LN1, _SN1, _D1, _)| _],
	lx_make_dev3(LN1, DEV3),
	true.

% Select devices to use and a boot device.
% Put boot device first.
menu_dev7_combo(TT, [DEV71| DEV7L]) :-
	% multi-device templates.
	% memberchk(TT, [gpt_basic, gpt_wizard, gpt_lvm, gpt_lvm_luks, gpt_luks_lvm]), !,
	memberchk(TT, [gpt_basic, gpt_wizard, gpt_lvm, gpt_lvm_luks]), !,
	menu_dev7_checklist_used_light(' Select device(s) to use ', DL0),
	menu_dev71_menu(' Select boot device ', DL0, DEV71),
	delete(DL0, DEV71, DEV7L),
	true.
menu_dev7_combo(_TT, [DEV71]) :-
	inst_setting(dev7, available(DL0)),
	menu_dev71_menu_used(' Select boot device ', DL0, DEV71),
	true.

% Select devices to use and a boot device.
% Put boot device first.
menu_dev4_combo(TT, [D4| D4L1]) :-
	% multi-device templates.
	memberchk(TT, [gpt_basic, gpt_wizard, gpt_lvm, gpt_lvm_luks]), !,
	menu_dev7_checklist_used_light(' Select device(s) to use ', DL0),
	maplist(menu_dev7_to_d4, DL0, D4L),
	menu_dev4_boot_dev(D4L, D4),
	delete(D4L, D4, D4L1),
	true.
menu_dev4_combo(_TT, [D4]) :-
	inst_setting(dev7, available(DL0)),
	maplist(menu_dev7_to_d4, DL0, D4L),
	menu_dev4_boot_dev(D4L, D4),
	true.

menu_dev4_boot_dev(D4L, D4) :-
	menu_d41_menu(' Select boot device ', D4L, SD),
	menu_sdn_to_d4(D4L, SD, D4),
	true.

menu_dev7_to_d4(DEV7, d4(LN, SN, D, 1)) :-
	lx_dev7_to_ldn_sdn(DEV7, LN, SN),
	lx_part_name(SN, 1, D),
	true.

enable_template(TT, B) :-
	make_cmd_list(TT, B, L),
	retractall(inst_setting(template(_), _)),
	assertz(inst_setting(template(TT), L)),
	true.

% p4(PartType, device, create/keep, size)
% fs5(FileSystem, Label, MountPoint, [device_list], create/keep)
% TT - template type
% N - partition number.
% B - bootloader.
% P - partition.
% vg(Name, [PhysicalVolumeList], [LogicalVolumeList])
partition_set_mbr(TT, B, FS, P, L) :-
	inst_setting(system(efi), _), !,
	partition_set_efi(TT, B, FS, P, L).
partition_set_mbr(TT, B, FS, P, L) :-
	memberchk(B, [syslinux, limine]), !,
	partition_set_boot(TT, B, FS, P, L).
partition_set_mbr(TT, B, FS, [d4(D, SN, _SDN, N)| T], [p4(sys_bios_boot, bd1([PD, D]), create, MBR_SZ)| L]) :-
	% No filesystem in this case.
	lx_part_name(D, N, PD),
	inst_setting(mbr_size, MBR_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	partition_set_boot(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).

% mount EFI to /boot instead of /boot/efi
partition_set_efi(TT, B, FS, [d4(D, SN, _SDN, N)| T], [p4(sys_efi, bd1([PD, D]), create, ESP_SZ), fs5(vfat, efi, '/boot', [PD], create)| L]) :-
	bootloader_boot_efi(BL),
	memberchk(B, BL), !,
	lx_part_name(D, N, PD),
	inst_setting(esp_size, ESP_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	% !!! skip partition_set_boot
	partition_set_template(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).
partition_set_efi(TT, B, FS, [d4(D, SN, _SDN, N)| T], [p4(sys_efi, bd1([PD, D]), create, ESP_SZ), fs5(vfat, efi, '/boot/efi', [PD], create)| L]) :-
	lx_part_name(D, N, PD),
	inst_setting(esp_size, ESP_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	partition_set_boot(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).

partition_set_boot(TT, B, FS, [d4(D, SN, _SDN, N)| T], [p4(linux_data, bd1([PD, D]), create, BOOT_SZ), fs5(ext4, boot, '/boot', [PD], create)| L]) :-
	need_boot_part(TT, B, FS), !,
	lx_part_name(D, N, PD),
	inst_setting(boot_size, BOOT_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	partition_set_template(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).
partition_set_boot(TT, B, FS, P, L) :-
	partition_set_template(TT, B, FS, P, L).

partition_set_template(gpt_lvm, B, FS, DL, L) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	% menu_d4_checklist_light(' Select device(s) to use with LVM ', DL, DL0),
	% DL0 \= [],
	% maplist(d4_to_p4_pd(linux_lvm), DL0, P4L, PDL),
	maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	menu_soft_soft(B, FS, [], SL),
	append(P4L, [bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), fs5(FS, void, '/', [LVM_PD], create)| SL], L),
	true.
partition_set_template(gpt_lvm_luks, B, FS, DL, L) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	format_to_atom(LVM_PD_SHORT, '~w-~w', [VG, LV]),
	luks_dev_name(LVM_PD_SHORT, LUKS_PD),
	get_luks_type(B, LUKS_T),
	menu_soft_soft(B, FS, [], SL),
	append(P4L, [bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), bdev(luks, luks(LUKS_T, LVM_PD)), fs5(FS, void, '/', [LUKS_PD], create)| SL], L),
	true.
partition_set_template(gpt_luks, B, FS, [d4(D, _SN, SDN, N)| _T], [p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(LUKS_T, PD)), fs5(FS, void, '/', [LUKS_PD], create)| SL]) :- !,
	lx_part_name(D, N, PD),
	luks_dev_name(SDN, LUKS_PD),
	get_luks_type(B, LUKS_T),
	menu_soft_soft(B, FS, [], SL),
	true.
% !!! DO NOT delete !!!
% multi-device support.
% partition_set_template(gpt_luks, B, FS, DL, L) :- !,
% 	DL = [D41| _T],
% 	d4_to_luks_pd(D41, LUKS_PD),
% 	% menu_d4_checklist_light(' Select device(s) to use with LUKS ', DL, DL0),
% 	% DL0 \= [],
% 	% maplist(d4_to_luks_bdev(LUKS_T), DL0, P4L, BDEVL),
% 	get_luks_type(B, LUKS_T),
% 	maplist(d4_to_luks_bdev(LUKS_T), DL, P4L, BDEVL),
% 	flatten([P4L, BDEVL, fs5(FS, void, '/', [LUKS_PD], create)], L),
% 	true.
partition_set_template(gpt_luks_lvm, B, FS, [d4(D, _SN, SDN, N)| _T], [p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(LUKS_T, PD)), bdev(lvm, vg(VG, [LUKS_PD], [lv(LV, SZ)])), fs5(FS, void, '/', [LVM_PD], create)| SL]) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	lx_part_name(D, N, PD),
	luks_dev_name(SDN, LUKS_PD),
	get_luks_type(B, LUKS_T),
	menu_soft_soft(B, FS, [], SL),
	true.
% !!! DO NOT delete !!!
% multi-device support.
% partition_set_template(gpt_luks_lvm, B, FS, DL, L) :- !,
% 	get_luks_type(B, LUKS_T),
% 	maplist(d4_to_luks_bdev(LUKS_T), DL, P4L, BDEVL),
% 	inst_setting(lvm, lv(VG, LV, SZ)),
% 	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
% 	maplist(d4_to_luks_pd, DL, PDL),
% 	flatten([P4L, BDEVL, bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), fs5(FS, void, '/', [LVM_PD], create)], L),
% 	true.
partition_set_template(_, B, FS, DL, L) :-
	fs_to_p4l_pdl(FS, DL, P4L, PDL),
	menu_soft_soft(B, FS, [], SL),
	append(P4L, [fs5(FS, void, '/', PDL, create)| SL], L),
	true.

get_luks_type(grub2, luks1) :- !.
get_luks_type(_B, luks2).

% DCL - device combo list.
menu_wiz_action([], []) :- !.
menu_wiz_action(DCL, L) :-
	dialog_msg(menu, MENULABEL),
	M = [
		  make_lvm_vg
		, make_luks
		, make_part
		, bootloader_dev
	],
	maplist(menu_tag, M, ML),
	% tui_msgbox(ookk),
	tui_menu_tag(ML, MENULABEL, [title(' Select Action ')], Tag),
	action_info(A, Tag, _),
	wiz_cmd(A, DCL, L),
	true.

% DCL - device combo list.
% DC: dev7, p4, luks, lvm_vg
wiz_cmd(make_lvm_vg, DCL, [bdev(lvm, vg(VG, _PDL, [lv(LV, SZ)]))| L]) :- !,
	menu_dev_combo_checklist2(' Select VG Device(s) ', DCL, [], VGL),
	tui_inputbox('Volume Group Name:', '', [], VG),
	LV = void,
	SZ = '',
	% maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	subtract(DCL, VGL, DCL2),
	menu_wiz_action(DCL2, L),
	true.
wiz_cmd(make_luks, DCL, L) :- !,
	% LD - luks device
	menu_dev_combo_menu(' Select LUKS Device ', DCL, none, LD),
	% LDN - luks device name
	tui_inputbox('LUKS Dev Name:', '', [], LDN),
	menu_password_for('LUKS', luks(LDN)),
	format_to_atom(LUKS_PD, '/dev/mapper/~w', [LDN]),
	delete(DCL, LD, DCL2),
	menu_wiz_action([luks(LUKS_PD, LDN)| DCL2], L),
	true.
wiz_cmd(make_part, DCL, [p4(linux_luks, bd1([_PD, _D]), create, '')| L]) :- !,
	menu_dev_combo_menu(' Select Partition Device ', DCL, none, PD),
	delete(DCL, PD, DCL2),
	menu_wiz_action([d4(_D, _SN, _SDN, _N)| DCL2], L),
	true.
wiz_cmd(bootloader_dev, DCL, [bootloader_dev(DEV3)| L]) :- !,
	menu_dev_combo_menu(' Select Bootloader Device ', DCL, none, PD),
	lx_make_dev3(PD, DEV3),
	menu_wiz_action(DCL, L),
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
	lx_part_name(D, N, PD),
	true.

d4_to_luks_bdev(LUKS_T, d4(D, _SN, _SDN, N), p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(LUKS_T, PD))) :-
	lx_part_name(D, N, PD),
	true.

d4_to_luks_pd(d4(_D, _SN, SDN, _N), LUKS_PD) :-
	luks_dev_name(SDN, LUKS_PD).

