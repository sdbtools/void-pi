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

part_tmpl_info(root, 'Single root partition').
part_tmpl_info(root_home, 'Root + home partitions').

part_tmpl_info_btrfs(root, 'Single root subvolume').
part_tmpl_info_btrfs(root_home, 'Root + home subvolumes').
part_tmpl_info_btrfs(max, 'Max complexity').

setup_fs_template :-
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

	% Partition + file system.
	% pfs(name, mount_point, size)
	assertz(inst_setting(part_tmpl(root), [pfs(root, '/', '')])),
	assertz(inst_setting(part_tmpl(root_home), [pfs(root, '/', '20G'), pfs(home, '/home', '')])),

	% subv(name, mount_point, mount_attrs, cow)
	assertz(inst_setting(part_tmpl_btrfs(root), [
		  subv('@', '/', [rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@snapshots', '/.snapshots', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		])),
	assertz(inst_setting(part_tmpl_btrfs(root_home), [
		  subv('@', '/', [rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@home', '/home', [nosuid, nodev, rw, noatime, space_cache=v2], cow)
		, subv('@snapshots', '/.snapshots', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		])),
	assertz(inst_setting(part_tmpl_btrfs(max), [
		  subv('@', '/', [rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@home', '/home', [nosuid, nodev, rw, noatime, space_cache=v2], cow)
		, subv('@opt', '/opt', [nodev, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@srv', '/srv', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		, subv('@var', '/var', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@var-cache-xbps', '/var/cache/xbps', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@var-lib-ex', '/var/lib/ex', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		, subv('@var-log', '/var/log', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		, subv('@var-opt', '/var/opt', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow)
		, subv('@var-spool', '/var/spool', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		, subv('@var-tmp', '/var/tmp', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		, subv('@snapshots', '/.snapshots', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow)
		])),

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
switch_template(_OT, _NT, _OB, _NB) :-
	tui_msgbox('Switching of template has failed.', [title(' ERROR ')]),
	fail.

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
% make_cmd_list(manual, B, [state(bios_efi), state(bootloader), bootloader(B)]) :- !.
make_cmd_list(manual, B, [bootloader(B)]) :- !.
% make_cmd_list(gpt_zfsbootmenu, B, [state(bios_efi), state(bootloader), bootloader(B)]) :- !,
make_cmd_list(gpt_zfsbootmenu, B, [bootloader(B)]) :- !,
	tui_msgbox('not implemented yet'),
	true.
% make_cmd_list(gpt_wizard, B, [state(bios_efi), state(bootloader), bootloader(B)| L]) :- !,
make_cmd_list(gpt_wizard, B, [bootloader(B)| L]) :- !,
	menu_dev7_checklist_used_light(' Select device(s) to use ', DEV7L),
	menu_wiz_action(DEV7L, L),
	true.
% make_cmd_list(TT, B, [state(bios_efi), state(bootloader), bootloader(B), state(bootloader_dev), bootloader_dev(DEV3)| L]) :- !,
make_cmd_list(TT, B, [bootloader(B), bootloader_dev(DEV3)| L]) :- !,
	menu_dev7_combo(TT, DEV7L),
	menu_root_fs(TT, B, FS),
	maplist(menu_dev7_to_d4, DEV7L, D4L),
	partition_set_mbr(TT, B, FS, D4L, L),
	D4L = [d4(LN1, _SN1, _D1, _)| _],
	lx_make_dev3(LN1, DEV3),
	true.

enable_template(TT, B) :-
	make_cmd_list(TT, B, L),
	retractall(inst_setting(template(_), _)),
	assertz(inst_setting(template(TT), L)),
	true.

% p4(PartType, device, create/keep, size)
% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
% TT - template type
% N - partition number.
% B - bootloader.
% DL - available device list.
% vg(Name, [PhysicalVolumeList], [LogicalVolumeList])
partition_set_mbr(TT, B, FS, DL, L) :-
	inst_setting(system(bios), _), !,
	partition_set_mbr_1(TT, B, FS, DL, L).
partition_set_mbr(TT, B, FS, DL, L) :-
	partition_set_efi(TT, B, FS, DL, L).

partition_set_mbr_1(TT, B, FS, DL, L) :-
	memberchk(B, [syslinux, limine]), !,
	partition_set_efi(TT, B, FS, DL, L).
partition_set_mbr_1(TT, B, FS, [d4(D, SN, _SDN, N)| T], [
		p4(sys_bios_boot, bd1([PD, D]), create, MBR_SZ)| L]
		) :-
	% No filesystem in this case.
	lx_part_name(D, N, PD),
	inst_setting(mbr_size, MBR_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	partition_set_efi(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).

partition_set_efi(TT, B, FS, DL, L) :-
	inst_setting(system(efi), _), !,
	partition_set_efi_1(TT, B, FS, DL, L).
partition_set_efi(TT, B, FS, DL, L) :-
	partition_set_boot(TT, B, FS, DL, L).

% mount EFI to /boot instead of /boot/efi
partition_set_efi_1(TT, B, FS, [d4(D, SN, _SDN, N)| T], [
		p4(sys_efi, bd1([PD, D]), create, ESP_SZ),
		fs7(vfat, efi, MP, [PD], [], MOL, create)| L]
		) :-
	bootloader_boot_efi(BL),
	memberchk(B, BL), !,
	MP = '/boot',
	get_mol(FS, MP, MOL),
	lx_part_name(D, N, PD),
	inst_setting(esp_size, ESP_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	% !!! skip partition_set_boot
	partition_set_template(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).
partition_set_efi_1(TT, B, FS, [d4(D, SN, _SDN, N)| T], [
		p4(sys_efi, bd1([PD, D]), create, ESP_SZ),
		fs7(vfat, efi, MP, [PD], [], MOL, create)| L]
		) :-
	MP = '/boot/efi',
	get_mol(FS, MP, MOL),
	lx_part_name(D, N, PD),
	inst_setting(esp_size, ESP_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	partition_set_boot(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).

partition_set_boot(TT, B, FS, [d4(D, SN, _SDN, N)| T], [
		p4(linux_data, bd1([PD, D]), create, BOOT_SZ),
		fs7(ext4, boot, MP, [PD], [], MOL, create)| L]
		) :-
	need_boot_part(TT, B, FS), !,
	MP = '/boot',
	get_mol(FS, MP, MOL),
	lx_part_name(D, N, PD),
	inst_setting(boot_size, BOOT_SZ),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	partition_set_template(TT, B, FS, [d4(D, SN, SD1, N1)| T], L).
partition_set_boot(TT, B, FS, DL, L) :-
	partition_set_template(TT, B, FS, DL, L).

partition_set_template(gpt_lvm, B, FS, DL, L) :- !,
	maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	fs_to_lvl_fsl(FS, root, B, PDL, L0),
	menu_soft(B, FS, [], SL),
	append(P4L, [state(root_fs, ctx_rfs(lvm, FS, B, DL))| L0], L1),
	append(L1, SL, L),
	true.
partition_set_template(gpt_lvm_luks, B, FS, DL, L) :- !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	format_to_atom(LVM_PD_SHORT, '~w-~w', [VG, LV]),
	luks_dev_name(LVM_PD_SHORT, LUKS_PD),
	get_luks_type(B, LUKS_T),
	fs_to_fsl_d(FS, root, B, LUKS_PD, FSL),
	menu_soft(B, FS, [], SL),
	append(P4L, [
		bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])),
		bdev(luks, luks(LUKS_T, LVM_PD)),
		state(root_fs, ctx_rfs(one, FS, B, DL))| FSL],
		L0),
	append(L0, SL, L),
	true.
partition_set_template(gpt_luks, B, FS, DL, L) :- !,
	DL = [d4(D, _SN, SDN, N)| _T],
	lx_part_name(D, N, PD),
	luks_dev_name(SDN, LUKS_PD),
	get_luks_type(B, LUKS_T),
	fs_to_fsl_d(FS, root, B, LUKS_PD, FSL),
	menu_soft(B, FS, [], SL),
	append([
		p4(linux_luks, bd1([PD, D]), create, ''),
		bdev(luks, luks(LUKS_T, PD)),
		state(root_fs, ctx_rfs(one, FS, B, DL))| FSL],
		SL,
		L),
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
% 	flatten([P4L, BDEVL, fs7(FS, void, '/', [LUKS_PD], [], [], create)], L),
% 	true.
partition_set_template(gpt_luks_lvm, B, FS, DL, [
		p4(linux_luks, bd1([PD, D]), create, ''),
		bdev(luks, luks(LUKS_T, PD)),
		state(root_fs, ctx_rfs(lvm, FS, B, DL))| L]
		) :- !,
	DL = [d4(D, _SN, SDN, N)| _T],
	lx_part_name(D, N, PD),
	luks_dev_name(SDN, LUKS_PD),
	get_luks_type(B, LUKS_T),
	fs_to_lvl_fsl(FS, root, B, [LUKS_PD], L0),
	menu_soft(B, FS, [], SL),
	append(L0, SL, L),
	true.
% !!! DO NOT delete !!!
% multi-device support.
% partition_set_template(gpt_luks_lvm, B, FS, DL, L) :- !,
% 	get_luks_type(B, LUKS_T),
% 	maplist(d4_to_luks_bdev(LUKS_T), DL, P4L, BDEVL),
% 	inst_setting(lvm, lv(VG, LV, SZ)),
% 	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
% 	maplist(d4_to_luks_pd, DL, PDL),
% 	flatten([P4L, BDEVL, bdev(lvm, vg(VG, PDL, [lv(LV, SZ)])), fs7(FS, void, '/', [LVM_PD], [], [], create)], L),
% 	true.
partition_set_template(_, B, FS, DL, [state(root_fs, ctx_rfs(dev, FS, B, DL))|L]) :-
	fs_to_p4l_fsl(FS, root, B, DL, L0),
	menu_soft(B, FS, [], SL),
	append(L0, SL, L),
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
		, make_part_wiz
		, bootloader_dev
	],
	maplist(menu_tag, M, ML),
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
wiz_cmd(make_part_wiz, DCL, [p4(linux_luks, bd1([_PD, _D]), create, '')| L]) :- !,
	menu_dev_combo_menu(' Select Partition Device ', DCL, none, PD),
	delete(DCL, PD, DCL2),
	menu_wiz_action([d4(_D, _SN, _SDN, _N)| DCL2], L),
	true.
wiz_cmd(bootloader_dev, DCL, [bootloader_dev(DEV3)| L]) :- !,
	menu_dev_combo_menu(' Select Bootloader Device ', DCL, none, PD),
	lx_make_dev3(PD, DEV3),
	menu_wiz_action(DCL, L),
	true.

fs_to_p4l_fsl(btrfs, OTN, B, DL, L) :- !,
	menu_part_tmpl_btrfs(OTN, NTN),
	fs_to_p4l_fsl_5(btrfs, NTN, B, DL, L),
	true.
fs_to_p4l_fsl(FS, OTN, B, DL, L) :-
	menu_part_tmpl(OTN, NTN),
	fs_to_p4l_fsl_5(FS, NTN, B, DL, L),
	true.

fs_to_p4l_fsl_5(btrfs, TN, B, DL, L) :- !,
	inst_setting(part_tmpl_btrfs(TN), PTL),
	fs2parttype(btrfs, PT),
	maplist(d4_to_p4_pd(PT), DL, P4L, PDL),
	append(
		[state(make_part_tmpl, ctx_part(dev, B, btrfs, TN, DL))|P4L],
		[fs5_multi(btrfs, void, PDL, PTL, create)],
		L),
	true.
fs_to_p4l_fsl_5(FS, TN, B, DL, L) :-
	inst_setting(part_tmpl(TN), PTL),
	fs2parttype(FS, PT),
	DL = [D4| _T],
	part_tmpl_to_p4_fs(PTL, FS, PT, D4, P4L, FSL),
	append(
		[state(make_part_tmpl, ctx_part(dev, B, FS, TN, DL))|P4L],
		FSL,
		L),
	true.

fs_to_fsl_d(btrfs, OTN, B, D, L) :- !,
	menu_part_tmpl_btrfs(OTN, NTN),
	fs_to_fsl_d_5(btrfs, NTN, B, D, L),
	true.
fs_to_fsl_d(FS, OTN, B, D, L) :-
	fs_to_fsl_d_5(FS, OTN, B, D, L),
	true.

fs_to_fsl_d_5(btrfs, TN, B, D, L) :- !,
	inst_setting(part_tmpl_btrfs(TN), PTL),
	L = [
		  state(make_part_tmpl , ctx_part(one, B, btrfs, TN, D))
		, fs5_multi(btrfs, void, [D], PTL, create)
	],
	true.
fs_to_fsl_d_5(FS, _TN, _B, D, L) :-
	get_mol(FS, '/', MOL),
	L = [fs7(FS, void, '/', [D], [], MOL, create)],
	true.

fs_to_lvl_fsl(btrfs, OTN, B, PDL, L) :- !,
	menu_part_tmpl_btrfs(OTN, NTN),
	fs_to_lvl_fsl_5(btrfs, NTN, B, PDL, L),
	true.
fs_to_lvl_fsl(FS, OTN, B, PDL, L) :-
	menu_part_tmpl(OTN, NTN),
	fs_to_lvl_fsl_5(FS, NTN, B, PDL, L),
	true.

fs_to_lvl_fsl_5(btrfs, TN, B, PDL, L) :- !,
	inst_setting(part_tmpl_btrfs(TN), PTL),
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	L = [
		  state(make_part_tmpl
		, ctx_part(lvm, B, btrfs, TN, PDL))
		, bdev(lvm, vg(VG, PDL, [lv(LV, SZ)]))
		, fs5_multi(btrfs, void, [LVM_PD], PTL, create)
	],
	true.
fs_to_lvl_fsl_5(FS, TN, B, PDL, L) :-
	inst_setting(part_tmpl(TN), PTL),
	inst_setting(lvm, lv(VG, _LV, _SZ)),
	part_tmpl_to_lv_fs(PTL, FS, VG, LVL, FSL),
	L = [
		  state(make_part_tmpl
		, ctx_part(lvm, B, FS, TN, PDL))
		, bdev(lvm, vg(VG, PDL, LVL))
		| FSL
	],
	true.

fs_to_fsl(PTT, btrfs, OTN, B, DL, L) :- !,
	menu_part_tmpl_btrfs(OTN, NTN),
	fs_to_fsl_6(PTT, btrfs, NTN, B, DL, L),
	true.
fs_to_fsl(PTT, FS, OTN, B, DL, L) :- !,
	menu_part_tmpl(OTN, NTN),
	fs_to_fsl_6(PTT, FS, NTN, B, DL, L),
	true.

fs_to_fsl_6(dev, FS, TN, B, DL, L) :- !,
	fs_to_p4l_fsl_5(FS, TN, B, DL, L).
fs_to_fsl_6(lvm, FS, TN, B, DL, L) :- !,
	fs_to_lvl_fsl_5(FS, TN, B, DL, L).
fs_to_fsl_6(one, FS, TN, B, DL, L) :- !,
	fs_to_fsl_d_5(FS, TN, B, DL, L).
fs_to_fsl_6(PTT, _FS, _TN, _B, _DL, _L) :- !,
	tui_msgbox2(['fs_to_6. Invalid key', PTT], [title(' ERROR ')]),
	fail.

part_tmpl_to_p4_fs([pfs(Label, MP, SZ)|T], FS, PT, d4(D, SN, _SDN, N), [p4(PT, bd1([PD, D]), create, SZ)|P4L], [fs7(FS, Label, MP, [PD], [], MOL, create)|FSL]) :- !,
	get_mol(FS, MP, MOL),
	lx_part_name(D, N, PD),
	N1 is N + 1,
	lx_part_name(SN, N1, SD1),
	part_tmpl_to_p4_fs(T, FS, PT, d4(D, SN, SD1, N1), P4L, FSL),
	true.
part_tmpl_to_p4_fs([], _FS, _PT, _D, [], []).

part_tmpl_to_lv_fs([pfs(Label, MP, SZ)|T], FS, VG, [lv(Label, SZ)|LVL], [fs7(FS, Label, MP, [LVM_PD], [], MOL, create)|FSL]) :- !,
	get_mol(FS, MP, MOL),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, Label]),
	part_tmpl_to_lv_fs(T, FS, VG, LVL, FSL),
	true.
part_tmpl_to_lv_fs([], _FS, _VG, [], []).

d4_to_p4_pd(PT, d4(D, _SN, _SDN, N), p4(PT, bd1([PD, D]), create, ''), PD) :-
	lx_part_name(D, N, PD),
	true.

d4_to_luks_bdev(LUKS_T, d4(D, _SN, _SDN, N), p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(LUKS_T, PD))) :-
	lx_part_name(D, N, PD),
	true.

d4_to_luks_pd(d4(_D, _SN, SDN, _N), LUKS_PD) :-
	luks_dev_name(SDN, LUKS_PD).

