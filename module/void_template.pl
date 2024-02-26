% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% template_info(name, descr, except_fs).
template_info(manual, 'Manual configuration of everything', []).
template_info(gpt_wizard, 'GPT. Wizard', [swap, vfat]).
template_info(gpt_basic, 'GPT. Basic', [swap, vfat]).
template_info(gpt_lvm, 'GPT. LVM', [swap, vfat, zfs]).
template_info(gpt_lvm_luks, 'GPT. LVM. LUKS', [swap, vfat, zfs]).
template_info(gpt_luks, 'GPT. LUKS. One device', [swap, vfat, zfs]).
template_info(gpt_luks_lvm, 'GPT. LUKS. LVM. One device', [swap, vfat, zfs]).
template_info(gpt_raid, 'GPT. RAID. One device', [swap]).

part_tmpl_info(root, 'Single root partition').
part_tmpl_info(root_home, 'Root + home partitions').

part_tmpl_info_btrfs(root, 'Single root subvolume').
part_tmpl_info_btrfs(root_home, 'Root + home subvolumes').
part_tmpl_info_btrfs(max, 'Max complexity').

part_tmpl_info_zfs(root, 'Single root dataset').
part_tmpl_info_zfs(root_home, 'Root + home datasets').
part_tmpl_info_zfs(max, 'Max complexity').

part_tmpl_info(zfs, Name, Descr) :- !,
	part_tmpl_info_zfs(Name, Descr).
part_tmpl_info(btrfs, Name, Descr) :- !,
	part_tmpl_info_btrfs(Name, Descr).
part_tmpl_info(_FS, Name, Descr) :-
	part_tmpl_info(Name, Descr).

% Partition + file system.
% pfs(name, mount_point, size)
predef_part_tmpl(root, [pfs(root, '/', '')]).
predef_part_tmpl(root_home, [pfs(root, '/', ROOT_SZ), pfs(home, '/home', '')]) :-
	inst_setting(root_size, ROOT_SZ).

setup_fs_template :-
	% dataset(name, mount_point, mount_attrs)
	assertz(inst_setting(part_tmpl_zfs(root), [
		  dataset('/ROOT', 'none', [canmount=off])
		, dataset('/ROOT/void', '/', [canmount=noauto, atime=off])
		% , dataset('ROOT/void', '/', [atime=off])
		])),
	assertz(inst_setting(part_tmpl_zfs(root_home), [
		  dataset('/ROOT', 'none', [canmount=off])
		, dataset('/ROOT/void', '/', [canmount=noauto, atime=off])
		% , dataset('ROOT/void', '/', [atime=off])
		, dataset('/home', '/home', [atime=off])
		])),
	assertz(inst_setting(part_tmpl_zfs(max), [
		  dataset('/ROOT', 'none', [canmount=off])
		, dataset('/ROOT/void', '/', [canmount=noauto, atime=off])
		% , dataset('ROOT/void', '/', [atime=off])
		, dataset('/home', '/home', [atime=off])
		, dataset('/opt', '/opt', [atime=off])
		, dataset('/srv', '/srv', [atime=off])
		% , dataset('/usr', '/usr', [canmount=off, atime=off])
		% , dataset('/usr-local', '/usr/local', [atime=off])
		, dataset('/tmp', '/tmp', ['com.sun:auto-snapshot'=false, atime=off])
		, dataset('/var', '/var', [canmount=off, atime=off])
		, dataset('/var-lib', '/var/lib', [canmount=off, atime=off])
		, dataset('/var-cache-xbps', '/var/cache/xbps', ['com.sun:auto-snapshot'=false, atime=off])
		, dataset('/var-lib-ex', '/var/lib/ex', [atime=off])
		, dataset('/var-log', '/var/log', [atime=off])
		, dataset('/var-opt', '/var/opt', [atime=off])
		, dataset('/var-spool', '/var/spool', [atime=off])
		, dataset('/var-tmp', '/var/tmp', ['com.sun:auto-snapshot'=false, atime=off])
		])),

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

part_tmpl(zfs, Name, L) :- !,
	inst_setting(part_tmpl_zfs(Name), L).
part_tmpl(btrfs, Name, L) :- !,
	inst_setting(part_tmpl_btrfs(Name), L).
part_tmpl(_FS, Name, L) :-
	predef_part_tmpl(Name, L).

% OT - old template
% NT - new template
% OB - old bootloader.
% NB - new bootloader.
switch_template(OT, OT, OB, OB) :- !.
switch_template(OT, NT, _OB, NB) :-
	% make_cmd_list(NT, NB, NTL),
	phrase(gen_cmd_list(NT, NB), NTL),
	retractall(inst_setting(template(OT), _)),
	assertz(inst_setting(template(NT), NTL)),
	!.
switch_template(_OT, _NT, _OB, _NB) :-
	tui_msgbox('Switching of template has failed.', [title(' ERROR ')]),
	fail.

fs2parttype(zfs, solaris_root).
fs2parttype(_, linux_data).

% need_boot_part(TemplateType, BootLoader, FileSystem, Encrypt).
need_boot_part(TT, B, _FS, _E) :-
	memberchk(B, [rEFInd, limine, syslinux, gummiboot]),
	memberchk(TT, [gpt_lvm, gpt_lvm_luks, gpt_luks, gpt_luks_lvm]),
	!.
% need_boot_part(_TT, grub2, zfs, true) :- !.
need_boot_part(_TT, B, FS, _E) :-
	% bootloader_info(bootloade, supported_fs, supported_template, except_fs).
	bootloader_info(B, FSL, _, _),
	\+ memberchk(FS, FSL),
	!.

gen_cmd_list(TT, B) -->
	[
	  state(bootloader, ctx_bl(B)),
	  bootloader(B),
	  state(template, ctx_tmpl(B, TT))
	],
	gen_cmd_list_tmpl(TT, [], B).

% OUDL - old used/boot dev7 list.
% OFS - old file system.
gen_cmd_list_tmpl(manual, _OUDL, _B) -->
	({ inst_setting(dev7, available([D])) } ->
	   [state(used_d7, ctx_used([D]))]
	;  [state(used_d7, ctx_used([]))]
	).
gen_cmd_list_tmpl(gpt_wizard, OUDL, _B) -->
	{ menu_dev71_checklist(' Select device(s) to use ', OUDL, DEV7L) },
	[state(used_d7, ctx_used(DEV7L))],
	gen_wiz_action(DEV7L).
gen_cmd_list_tmpl(TT, OUDL, B) -->
	gen_cmd_list4(TT, OUDL, B).

enable_template(TT, B) :-
	% inst_setting(dev7, available(AL)),
	% make_cmd_list(TT, B, L),
	phrase(gen_cmd_list(TT, B), L),
	retractall(inst_setting(template(_), _)),
	assertz(inst_setting(template(TT), L)),
	true.

% O7L - old list.
gen_cmd_list4(TT, O7L, B) -->
	{ menu_dev7_use(TT, O7L, D7L) },
	[ state(used_d7, ctx_used(D7L)) ],
	gen_bld(TT, B, O7L, D7L).

gen_bld(TT, B, O7L, D7L) -->
	{ menu_dev7_boot_dev(D7L, O7L, DEV71)
	  , delete(D7L, DEV71, D7L1)
	  , DEV7L = [DEV71| D7L1]
	  , maplist(conv_dev7_to_d4, DEV7L, D4L)
	},
	[ state(bootloader_dev, ctx_bld7(B, DEV71, D7L))
	, bootloader_dev7(DEV71)
	],
	gen_root_fs(TT, ext4, B, D4L).

gen_root_fs(TT, OFS, B, D4L) -->
	{ menu_select_fs(TT, B, OFS, NFS) },
	gen_mbr(TT, NFS, B, D4L).

gen_mbr(TT, FS, B, D4L) -->
	{
	  ( B = zfsBootMenu, inst_setting(system(bios), _) ->
	    B1 = syslinux
	  ; B1 = B
	  )
	},
	gen_mbr_1(TT, FS, B1, D4L).

gen_mbr_1(TT, FS, B, D4L) -->
	{ inst_setting(system(bios), _), ! },
	gen_mbr_2(TT, FS, B, D4L).
gen_mbr_1(TT, FS, B, D4L) -->
	gen_efi(TT, FS, B, D4L).

gen_mbr_2(TT, FS, B, D4L) -->
	{ memberchk(B, [syslinux, limine]) },
	gen_efi(TT, FS, B, D4L).
gen_mbr_2(TT, FS, B, [OD4| T]) -->
	{ inst_setting(mbr_size, MBR_SZ) },
	gen_p4(sys_bios_boot, MBR_SZ, OD4, _PD, ND4),
	gen_efi(TT, FS, B, [ND4| T]).

gen_efi(TT, FS, B, D4L) -->
	{ inst_setting(system(efi), _), ! },
	gen_efi_1(TT, FS, B, D4L).
gen_efi(TT, FS, B, D4L) -->
	gen_boot(TT, FS, B, D4L).

gen_efi_1(TT, FS, B, [OD4| T]) -->
	{
	  bootloader_boot_efi(BL),
	  memberchk(B, BL), !,
	  % inst_setting(esp_size, ESP_SZ)
	  % EFI partition of BOOT_SZ size.
	  inst_setting(boot_size, BOOT_SZ)
	},
	gen_p4_fs7(B, sys_efi, vfat, '/boot', BOOT_SZ, efi, OD4, ND4),
	gen_tmpl(TT, FS, B, [ND4| T]).
gen_efi_1(TT, FS, B, [OD4| T]) -->
	{ inst_setting(esp_size, ESP_SZ) },
	gen_p4_fs7(B, sys_efi, vfat, '/boot/efi', ESP_SZ, efi, OD4, ND4),
	gen_boot(TT, FS, B, [ND4| T]).

gen_boot(TT, FS, B, [OD4| T]) -->
	{
	  menu_zfs_encr(FS, B, E),
	  need_boot_part(TT, B, FS, E), !,
	  inst_setting(boot_size, BOOT_SZ),
	  % bootloader_info(bootloade, supported_fs, supported_template, except_fs).
	  bootloader_info(B, FSL, _, _),
	  ( memberchk(ext4, FSL) ->
	    BFS = ext4
	  ; BFS = vfat
	  )
	},
	gen_p4_fs7(B, linux_data, BFS, '/boot', BOOT_SZ, boot, OD4, ND4),
	gen_tmpl(TT, FS, B, [ND4| T]).
gen_boot(TT, FS, B, D4L) --> gen_tmpl(TT, FS, B, D4L).

gen_p4(PT, SZ, d4(D, SN, _SDN, N), PD, d4(D, SN, SD1, N1)) -->
	{
	  % No filesystem in this case.
	  lx_part_name(D, N, PD),
	  N1 is N + 1,
	  lx_part_name(SN, N1, SD1)
	},
	[p4(PT, bd1([PD, D]), create, SZ)].

gen_fs7(B, FS, MP, Label, PD) -->
	{ get_col_mol(B, FS, MP, COL, MOL) },
	[ fs7(FS, Label, MP, PD, COL, MOL, create) ].

gen_p4_fs7(B, PT, FS, MP, SZ, Label, OD4, ND4) -->
	gen_p4(PT, SZ, OD4, PD, ND4),
	gen_fs7(B, FS, MP, Label, PD).

gen_tmpl(gpt_lvm, FS, B, D4L) -->
	{ maplist(d4_to_p4_pd(linux_lvm), D4L, P4L, PDL) },
	P4L,
	gen_st_root_fs(gpt_lvm, FS, B),
	gen_tmpl_lvm_init(FS, root, B, PDL, D4L),
	gen_soft(FS, B).
gen_tmpl(gpt_lvm_luks, FS, B, D4L) -->
	{
	  inst_setting(lvm, lv(VG, LV, SZ)),
	  format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	  maplist(d4_to_p4_pd(linux_lvm), D4L, P4L, PDL),
	  format_to_atom(LVM_PD_SHORT, '~w-~w', [VG, LV]),
	  luks_dev_name(LVM_PD_SHORT, LUKS_PD),
	  get_luks_type(B, LUKS_T)
	},
	P4L,
	[
	  bdev(lvm, vg(VG, PDL, [lv(LV, SZ)]))
	, bdev(luks, luks(LUKS_T, LVM_PD))
	],
	gen_st_root_fs(gpt_lvm_luks, FS, B),
	gen_tmpl_one_init(FS, root, B, LUKS_PD, D4L),
	gen_soft(FS, B).
gen_tmpl(gpt_luks, FS, B, D4L) -->
	{
	  D4L = [d4(D, _SN, SDN, N)| _T],
	  lx_part_name(D, N, PD),
	  luks_dev_name(SDN, LUKS_PD),
	  get_luks_type(B, LUKS_T)
	},
	[
	  p4(linux_luks, bd1([PD, D]), create, '')
	, bdev(luks, luks(LUKS_T, PD))
	],
	gen_st_root_fs(gpt_luks, FS, B),
	gen_tmpl_one_init(FS, root, B, LUKS_PD, D4L),
	gen_soft(FS, B).
gen_tmpl(gpt_luks_lvm, FS, B, D4L) -->
	{
	  D4L = [d4(D, _SN, SDN, N)| _T],
	  lx_part_name(D, N, PD),
	  luks_dev_name(SDN, LUKS_PD),
	  get_luks_type(B, LUKS_T)
	},
	[
	  p4(linux_luks, bd1([PD, D]), create, '')
	, bdev(luks, luks(LUKS_T, PD))
	],
	gen_st_root_fs(gpt_luks_lvm, FS, B),
	gen_tmpl_lvm_init(FS, root, B, [LUKS_PD], D4L),
	gen_soft(FS, B).
gen_tmpl(gpt_basic, FS, B, D4L) -->
	gen_st_root_fs(gpt_basic, FS, B),
	gen_tmpl_dev_init(FS, root, B, D4L),
	gen_soft(FS, B).

% Bootloader supports only one filesystem.
gen_st_root_fs(_TT, _FS, B) -->
	{ bootloader_info(B, [_], _, _) }.
gen_st_root_fs(TT, FS, B) -->
	{ tmpl_to_grp(TT, PTT) },
	[state(root_fs, ctx_rfs(PTT, FS, B, TT))].

gen_tmpl_lvm(FS, PTN, B, PDL) -->
	{
	  memberchk(FS, [zfs, btrfs]), !,
	  part_tmpl(FS, PTN, PTL),
	  get_enc_attr(FS, B, E),
	  inst_setting(lvm, lv(VG, LV, SZ)),
	  format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV])
	},
	[ bdev(lvm, vg(VG, PDL, [lv(LV, SZ)]))
	, fs5_multi(FS, void, [LVM_PD], PTL, create, B, E)
	].
gen_tmpl_lvm(FS, PTN, B, PDL) -->
	{
	  part_tmpl(FS, PTN, PTL),
	  inst_setting(lvm, lv(VG, _LV, _SZ)),
	  part_tmpl_to_lv(PTL, LVL)
	},
	[ bdev(lvm, vg(VG, PDL, LVL)) ],
	gen_tmpl_to_fs(PTL, B, FS, VG).

gen_tmpl_lvm_init(FS, OPTN, B, PDL, D4L) -->
	{ menu_part_tmpl(FS, OPTN, NPTN) },
	[state(make_part_tmpl, ctx_part(lvm, B, FS, NPTN, D4L))],
	gen_tmpl_lvm(FS, NPTN, B, PDL).

gen_tmpl_one(FS, PTN, B, D) -->
	{
	  memberchk(FS, [zfs, btrfs]), !,
	  part_tmpl(FS, PTN, PTL),
	  get_enc_attr(FS, B, E)
	},
	[ fs5_multi(FS, void, [D], PTL, create, B, E) ].
gen_tmpl_one(FS, _PTN, B, D) -->
	{ MP = (/) },
	gen_fs7(B, FS, MP, void, D).

gen_tmpl_one_init(FS, OPTN, B, D, D4L) -->
	{ memberchk(FS, [zfs, btrfs]) -> 
	  menu_part_tmpl(FS, OPTN, NPTN)
	; NPTN = root
	},
	[state(make_part_tmpl, ctx_part(one, B, FS, NPTN, D4L))],
	gen_tmpl_one(FS, NPTN, B, D).

gen_tmpl_dev(FS, PTN, B, D4L) -->
	{
	  memberchk(FS, [zfs, btrfs]), !,
	  part_tmpl(FS, PTN, PTL),
	  get_enc_attr(FS, B, E),
	  fs2parttype(FS, PT),
	  maplist(d4_to_p4_pd(PT), D4L, P4L, PDL)
	},
	P4L,
	[fs5_multi(FS, void, PDL, PTL, create, B, E)].
gen_tmpl_dev(FS, PTN, B, D4L) -->
	{
	  part_tmpl(FS, PTN, PTL),
	  fs2parttype(FS, PT),
	  D4L = [D4| _T]
	},
	gen_tmpl_to_p4_fs(PTL, B, FS, PT, D4).

gen_tmpl_dev_init(FS, OPTN, B, D4L) -->
	{ menu_part_tmpl(FS, OPTN, NPTN) },
	[state(make_part_tmpl, ctx_part(dev, B, FS, NPTN, D4L))],
	gen_tmpl_dev(FS, NPTN, B, D4L).

gen_tmpl_to_fs([pfs(Label, MP, _SZ)|T], B, FS, VG) -->
	{ format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, Label]) },
	gen_fs7(B, FS, MP, Label, LVM_PD),
	gen_tmpl_to_fs(T, B, FS, VG).
gen_tmpl_to_fs([], _B, _FS, _VG) --> [].

gen_soft(FS, B) -->
	{ findall(S, soft_info(S, B, FS, _DepL, _Descr), SL) },
	gen_soft2(SL, FS, B, []).

gen_soft2([], FS, B, _IL) -->
	[state(soft, ctx_soft(FS, B, []))].
gen_soft2(SL, FS, B, IL) -->
    {
	  findall(SD, (member(S, SL), menu_soft_(S, SD)), AL),
	  dialog_msg(checklist, LABEL),
	  tui_checklist_tag2(AL, IL, LABEL, [title(' Select Software ')], OL1),
	  findall(soft(S), member(S, OL1), OL2)
	},
	[state(soft, ctx_soft(FS, B, OL1))],
	OL2.

gen_tmpl_to_p4_fs([pfs(Label, MP, SZ)|T], B, FS, PT, OD4) -->
	gen_p4_fs7(B, PT, FS, MP, SZ, Label, OD4, ND4),
	gen_tmpl_to_p4_fs(T, B, FS, PT, ND4).
gen_tmpl_to_p4_fs([], _B, _FS, _PT, _D) --> [].

tmpl_to_grp(gpt_basic, dev) :- !.
tmpl_to_grp(gpt_lvm, lvm) :- !.
tmpl_to_grp(gpt_luks_lvm, lvm) :- !.
tmpl_to_grp(gpt_luks, one) :- !.
tmpl_to_grp(gpt_lvm_luks, one) :- !.

get_luks_type(grub2, luks1) :- !.
get_luks_type(_B, luks2).

gen_wiz_action([]) --> [].
gen_wiz_action(DCL) -->
	{
	  dialog_msg(menu, MENULABEL),
	  M = [
	  	  make_lvm_vg
	  	, make_luks
	  	, make_part_wiz
	  	, bootloader_dev
	  ],
	  maplist(menu_tag, M, ML),
	  tui_menu_tag(ML, MENULABEL, [title(' Select Action ')], Tag),
	  action_info(A, Tag, _)
	},
	gen_wiz_cmd(A, DCL).

% DCL - device combo list.
% DC: dev7, p4, luks, lvm_vg
gen_wiz_cmd(make_lvm_vg, DCL) -->
	[bdev(lvm, vg(VG, _PDL, [lv(LV, SZ)]))],
	{
	  menu_dev_combo_checklist2(' Select VG Device(s) ', DCL, [], VGL),
	  tui_inputbox('Volume Group Name:', '', [], VG),
	  LV = void,
	  SZ = '',
	  % maplist(d4_to_p4_pd(linux_lvm), DL, P4L, PDL),
	  subtract(DCL, VGL, DCL2)
	},
	gen_wiz_action(DCL2).
gen_wiz_cmd(make_luks, DCL) -->
	{
	  % CD - combo device
	  menu_dev_combo_menu(' Select LUKS Device ', DCL, none, CD),
	  % LDN - luks device name
	  tui_inputbox('LUKS Dev Name:', '', [], LDN),
	  menu_password_for('LUKS', luks(LDN)),
	  format_to_atom(LUKS_PD, '/dev/mapper/~w', [LDN]),
	  delete(DCL, CD, DCL2)
	},
	gen_wiz_action([luks(LUKS_PD, LDN)| DCL2]).
gen_wiz_cmd(make_part_wiz, DCL) -->
	[p4(linux_luks, bd1([_PD, _D]), create, '')],
	{
	  % CD - combo device
	  menu_dev_combo_menu(' Select Partition Device ', DCL, none, CD),
	  delete(DCL, CD, DCL2)
	},
	gen_wiz_action([d4(_D, _SN, _SDN, _N)| DCL2]).
gen_wiz_cmd(bootloader_dev, DCL) -->
	% lx_make_dev3(CD, DEV3),
	[bootloader_dev7(_DEV7)],
	{
	  % CD - combo device
	  menu_dev_combo_menu(' Select Bootloader Device ', DCL, none, _CD)
	},
	gen_wiz_action(DCL).

part_tmpl_to_lv(IL, OL) :-
	findall(lv(Label, SZ), member(pfs(Label, _MP, SZ), IL), OL).

d4_to_p4_pd(PT, d4(D, _SN, _SDN, N), p4(PT, bd1([PD, D]), create, ''), PD) :-
	lx_part_name(D, N, PD),
	true.

