% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

dialog_msg(menu, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.').
dialog_msg(list, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.').
dialog_msg(radiolist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.').
dialog_msg(checklist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.').
dialog_msg(form, 'Use UP and DOWN arrows (or Ctrl/N, Ctrl/P) to move between fields. Use TAB to move between windows.').

action_info(common_settings, 'Common Attrs', 'Common settings').
action_info(template, 'Template', 'Predefined configurations').
action_info(review, 'Review', 'Show current settings').
action_info(filesystem, 'Filesystem', 'Configure filesystems and mount points').
action_info(bootloader_dev, 'Bootloader Dev', 'Select disk to install bootloader').
action_info(bootloader, 'Bootloader', 'Set bootloader application').
action_info(save, 'Save', 'Save settings on disk').
action_info(install, 'Install', 'Start installation').
action_info(root_fs, 'Root FS', 'Set root file system').
action_info(mnt_opts, 'Mount Options', 'Set Mount options').
action_info(fs_settings, 'FS Settings', 'Set FS features and options').
action_info(make_lvm_vg, 'VG', 'Create LVM volume group').
action_info(make_lvm_lv, 'LV', 'Create LVM logical volume').
action_info(make_luks, 'LUKS', 'Create LUKS device').
action_info(make_part_wiz, 'Partition Wizard', 'Create partition').
action_info(make_part_tmpl, 'Partition Template', 'Select partition template').
action_info(make_part_manually, 'Partition', 'Manually partition disk(s)').
action_info(part_select, 'Select Part', 'Select partition(s) to use').
action_info(part_use, 'Partitions', 'Partitions to use during installation').
action_info(bios_efi, 'BIOS/EFI', 'Support either BIOS or EFI, or both').
action_info(soft, 'Software', 'Select software to install').
action_info(used_d7, 'Used devices', 'Select device(s) to use').

% action_info_common(tag, short_name, long_name, data_type)
action_info_common(hostonly, 'Host-only', 'Install only what is needed for booting the local host', bool).
action_info_common(keymap, 'Keyboard', 'Set system keyboard', bool).
action_info_common(network, 'Network', 'Set up the network', bool).
action_info_common(source, 'Source', 'Set source installation', bool).
action_info_common(hostname, 'Hostname', 'Set system hostname', string).
action_info_common(locale, 'Locale', 'Set system locale', bool).
action_info_common(timezone, 'Timezone', 'Set system time zone', bool).
action_info_common(root_passwd, 'Root Password', 'Set system root password', passwd).
action_info_common(user_passwd, 'User Password', 'Set user password', passwd).
action_info_common(luks_passwd, 'LUKS Password', 'Set LUKS password', passwd).
action_info_common(useraccount, 'User Account', 'Set primary user name and password', bool).
action_info_common(luks_info, 'LUKS Mapping Name', 'Set LUKS mapping name', bool).
action_info_common(mbr_size, 'MBR Size', 'Set MBR size', int).
action_info_common(esp_size, 'ESP Size', 'Set EFI system partition size', int).
action_info_common(boot_size, 'Boot Size', 'Set boot partition size', int).
action_info_common(root_size, 'Root Size', 'Set root partition size', int).
action_info_common(lvm_info, 'LVM Info', 'Set LVM info', bool).

boot_info(bios, 'Old BIOS boot method').
boot_info(efi, 'New EFI boot method').

setting_value(_TL, bios_efi, VL) :- !,
	findall(V, (member(V, [bios, efi]), inst_setting(system(V), _)), VL).
setting_value(TL, part_use, V1) :- !,
	findall(PD, member(p4(_PT, bd1([PD| _]), _CK, _SZ), TL), VL),
	write_to_atom(V1, VL).
setting_value(_TL, root_passwd, '********') :-
	inst_setting_tmp(passwd(root), _), !.
setting_value(_TL, user_passwd, '********') :-
	inst_setting(useraccount, user(UN, _, _)),
	inst_setting_tmp(passwd(UN), _), !.
setting_value(_TL, luks_passwd, '********') :-
	inst_setting_tmp(passwd('$_luks_$'), _), !.
setting_value(_TL, lvm_info, V1) :- !,
	inst_setting(lvm, lv(VG, LV, _SZ)),
	format_to_atom(V1, 'VG: ~w, LV: ~w', [VG, LV]).
setting_value(_TL, luks_info, N) :- !,
	inst_setting(luks, luks(N)).
setting_value(TL, bootloader, B) :- !,
	( get_bootloader(TL, B)
	; B = 'not set'
	), !.
setting_value(TL, bootloader_dev, D) :- !,
	( get_bootloader_dev7(TL, DEV7) ->
	  lx_dev7_to_ldn(DEV7, D)
	; D = 'not set'
	).
setting_value(TL, root_fs, FS) :- !,
	( root_fs(TL, FS)
	; FS = 'not set'
	), !.
setting_value(_TL, S, V1) :-
	inst_setting(S, V), !,
	write_to_atom(V1, V).
setting_value(_TL, _, 'not set').

inst_method_tag(1, local, 'Local', 'Packages from live ISO image').
inst_method_tag(2, net, 'Network', 'Base system only, downloaded from official repository').
% inst_method_tag(3, rootfs, 'Rootfs', 'Packages from rootfs ISO image').

on_inst_method(net) :-
	\+ inst_setting(network, _),
	menu_network, !.
on_inst_method(_).

menu_pkg_inst_method :-
	dialog_msg(radiolist, RADIOLABEL),
	findall([Tag, Label], inst_method_tag(_, _, Tag, Label), L1),
	inst_setting(source, OM),
	inst_method_tag(_, OM, OMT, _),
	tui_radiolist_tag2(L1, OMT, RADIOLABEL, [title(' Select installation source ')], MT),
	inst_method_tag(_, A, MT, _),

	on_inst_method(A),
	retractall(inst_setting(source, _)),
	assertz(inst_setting(source, A)).

menu_review_opt(_TT, _TL, S) :-
	S = [
		  bios_efi
		, part_use
		, root_fs
		, bootloader
		, bootloader_dev
	].
menu_review_opt(TT, TL, S) :-
	menu_common_opt(TT, TL, S).

menu_review(TT, TL) :-
	findall(S0, (menu_review_opt(TT, TL, SL0), member(S0, SL0)), S),
	maplist(menu_tag_common(TL), S, SL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [no-cancel, ok-label('Return'), title(' Current settings ')], _Tag),
	true.

% OL - old list (previously selected)
% NL - new list
menu_soft(B, FS, OL, NL) :-
	findall(S, soft_info(S, B, FS, _DepL, _Descr), SL),
	( SL = [] ->
	  NL = []
	; findall(SD, (member(S, SL), menu_soft_(S, SD)), AL),
	  dialog_msg(checklist, LABEL),
	  tui_checklist_tag2(AL, OL, LABEL, [title(' Select Software ')], NL)
	),
	true.

menu_soft_(S, [S, Descr]) :-
	soft_info(S, _B, _FS, _DepL, Descr), !,
	true.

% C - command.
menu_edit_main(C, OTL) :-
	% menu_edit_main_4(OTL, C, [], OTL, NTL),
	phrase(gen_edit(OTL, C, [], no_fu, OTL), NTL),
	% tui_msgbox_w(NTL, [sz(max)]),
	memberchk(state(template, ctx_tmpl(_B, NT)), NTL),
	retractall(inst_setting(template(_), _)),
	assertz(inst_setting(template(NT), NTL)),
	!.

gen_edit([state(bootloader, CTX), bootloader(OB)|T], bootloader, VL, FU, OTL) -->
	{
	  CTX = ctx_bl(OB),
	  % Similar to menu_bootloader(TT, TL).
	  findall(B, (menu_bootloader_(L0), member(B, L0)), BL),
	  dialog_msg(radiolist, LABEL),
	  tui_radiolist_tag2(BL, OB, LABEL, [no-tags, title(' Select a bootloader ')], NB)
	},
	[ state(bootloader, ctx_bl(NB))
	, bootloader(NB)
	],
	( { OB = NB } ->
	  T
	; gen_edit(T, template, [bl(NB)|VL], FU, OTL)
	).
gen_edit([state(template, CTX)|T], template, VL, FU, OTL) -->
	{
	  CTX = ctx_tmpl(B, OT),
	  ( memberchk(bl(VB), VL); VB = B ), !,
	  menu_select_template(VB, OT, NT)
	},
	[state(template, ctx_tmpl(VB, NT))],
	( { FU = no_fu, OT = NT } ->
	  T
	; {
		st_used_d7(OTL, UL)
		% , st_root_fs(OTL, OFS)
	  },
	  gen_cmd_list_tmpl(NT, UL, [bl(VB)])
	).
gen_edit([state(used_d7, CTX)|T], used_d7, VL, FU, OTL) -->
	{
	  CTX = ctx_used(OL),
	  menu_dev71_checklist(' Select device(s) to use ', OL, NL)
	},
	[state(used_d7, ctx_used(NL))],
	( { FU = no_fu, OL = NL, \+ memberchk(tmpl(_), VL) } ->
	  T
	; gen_edit(T, bootloader_dev, [used(NL)|VL], fu, OTL)
	).
gen_edit([state(bootloader_dev, CTX), bootloader_dev7(OBD)|T], bootloader_dev, VL, FU, OTL) -->
	{
	  CTX = ctx_bld7(B, OBD, D7L),
	  ( memberchk(bl(VB), VL); VB = B ), !,
	  ( memberchk(used(VD7L), VL); VD7L = D7L ), !,
	  menu_dev7_boot_dev(VD7L, [OBD], NBD),
	  maplist(conv_dev7_to_d4, VD7L, VD4L0), !,
	  conv_dev7_to_d4(NBD, D4),
	  delete(VD4L0, D4, DL0),
	  VD4L = [D4|DL0]
	},
	[ state(bootloader_dev, ctx_bld7(VB, NBD, VD7L))
	, bootloader_dev7(NBD)
	],
	( { FU = no_fu, OBD = NBD, D7L = VD7L } ->
	  T
	% ; gen_edit(T, root_fs, [d4l(VD4L), bld7(NBD)|VL], fu, OTL)
	; {
		memberchk(tmpl(TT), VL),
		st_root_fs(OTL, OFS)
	  }
	, gen_root_fs(OFS, [d4l(VD4L), tmpl(TT), bl(VB)])
	).

% !!! gen_edit chain is broken here !!!

gen_edit([state(root_fs, CTX)|T], root_fs, VL, FU, OTL) -->
	{
	  CTX = ctx_rfs(PTT, OFS),
	  memberchk(bl(VB), VL),
	  memberchk(tmpl(VTT), VL),
	  menu_select_fs(VTT, VB, OFS, NFS)
	},
	[state(root_fs, ctx_rfs(PTT, NFS))],
	( { FU = no_fu, OFS = NFS } ->
	  T
	; gen_edit(T, make_part_tmpl, [fs(NFS)|VL], FU, OTL)
	).
gen_edit([state(make_part_tmpl, CTX), state(fs_settings, ctx_fs_settings(_FS)), state(mnt_opts, ctx_mnt_opts)|T], make_part_tmpl, VL, FU, _OTL) -->
	{
	  CTX = ctx_part4(PTT, FS, OPTN, D4L),
	  memberchk(bl(VB), VL),
	  ( memberchk(fs(VFS), VL); VFS = FS ), !,
	  ( memberchk(d4l(VD4L), VL); VD4L = D4L ), !,
	  % !!! PTT can change !!!
	  ( PTT = one, \+ memberchk(VFS, [zfs, btrfs]) ->
	    % Skip partition template menu in case of one non-partitionable device.
	    NPTN = OPTN
	  ; menu_part_tmpl(VFS, OPTN, NPTN)
	  )
	},
	[state(make_part_tmpl, ctx_part4(PTT, VFS, NPTN, VD4L)), state(fs_settings, ctx_fs_settings(VFS)), state(mnt_opts, ctx_mnt_opts)],
	( { FU = no_fu, VFS = FS, VD4L = D4L, OPTN = NPTN } ->
	  T
	; gen_tmpl_ptt(PTT, VFS, NPTN, VB, VD4L),
	  {
		% Clean fs_settings_main menu up.
		retractall(tui_setting_tmp(menu_list(fs_settings_main), _)),
		% Clean mnt_opts_main menu up.
		retractall(tui_setting_tmp(menu_list(mnt_opts_main), _)),
		st_skip_till(T, soft, SL)
	  },
	  gen_edit(SL, soft, VL, FU, SL)
	  % , gen_soft(VFS, VB)
	).
gen_edit([state(soft, ctx_soft(FS, B, OSL))|T], soft, VL, _FU, _OTL) -->
	{
	  % root_fs(TL, FS),
	  ( memberchk(bl(VB), VL); VB = B ), !,
	  ( memberchk(fs(VFS), VL); VFS = FS ), !,
	  menu_soft(VB, VFS, OSL, NSL)
	},
	[state(soft, ctx_soft(VFS, VB, NSL))],
	( { FS = VFS, OSL = NSL } ->
	  T
	; { findall(soft(S), member(S, NSL), NSL2) },
	  NSL2
	).
gen_edit([H|T], C, VL, FU, OTL) -->
	[H],
	{ st_retrieve_ctx(H, VL, NVL) },
	gen_edit(T, C, NVL, FU, OTL).
gen_edit([], _C, _VL, _FU, _OTL) --> [].

gen_tmpl_ptt(dev, FS, PTN, B, D4L) -->
	gen_tmpl_dev(FS, PTN, B, D4L).
gen_tmpl_ptt(lvm, FS, PTN, B, PDL) -->
	gen_tmpl_lvm(FS, PTN, B, PDL).
gen_tmpl_ptt(one, FS, PTN, B, D) -->
	gen_tmpl_one(FS, PTN, B, D).

menu_edit_soft(FS, L, B, NSL) :-
	findall(S, member(soft(S), L), OSL),
	menu_soft(B, FS, OSL, NSL),
	true.

menu_select_template(B, OT, NT) :-
	bootloader_info(B, _, TL0, _),
	( TL0 = [NT]
	; findall([TT0, Descr], (member(TT0, TL0), template_info(TT0, Descr, _)), TL),
	  dialog_msg(radiolist, LABEL),
	  tui_radiolist_tag2(TL, OT, LABEL, [no-tags, title(' Choose configuration ')], NT)
	), !,
	true.

menu_save :-
	tui_yesno('Save settings?', [sz([6, 40])]), !,
	open('settings.pl', write, S),
	save_settings(S),
	close(S),
	!.
menu_save.

menu_tag(A, [T,D]) :-
	action_info(A,T,D), !.
menu_tag(A, [A,A]).

menu_tag_common(TL, A, [T, V]) :-
	action_info(A, T, _), !,
	setting_value(TL, A, V).
menu_tag_common(TL, A, [T, V]) :-
	action_info_common(A, T, _, _), !,
	setting_value(TL, A, V).
menu_tag_common(_TL, A, [A,A]).

menu_network :-
	( file_exists('/var/service/NetworkManager') ->
	  test_network(nm)
	; get_net_devs
	),
	true.

menu_setting(Tag) :-
	( inst_setting(Tag, V)
	; V = 'not set'
	), !,
	action_info_common(Tag, _, Title, _),
	tui_inputbox('', V, [title(Title)], NV),
	( NV = V
	; retractall(inst_setting(Tag, _)),
	  assertz(inst_setting(Tag, NV))
	), !,
	true.

menu_common_opt(_TT, _TL, M) :-
	M = [
		  hostonly
		, keymap
		, network
		, source
		, hostname
		, locale
		, timezone
		, root_passwd
		, useraccount
		, user_passwd
	].
menu_common_opt(_TT, _TL, [esp_size]) :-
	inst_setting(system(efi), _).
menu_common_opt(_TT, _TL, [mbr_size]) :-
	inst_setting(system(bios), _).
menu_common_opt(TT, TL, [boot_size]) :-
	root_fs(TL, FS),
	get_bootloader(TL, B),
	need_boot_part(TT, B, FS).
menu_common_opt(_TT, _TL, [root_size]).
% menu_common_opt(_TT, _TL, [root_size]) :-
% 	inst_setting(part_tmpl(root_home), _).
% menu_common_opt(gpt_raid, _TL, [boot_size]).
menu_common_opt(gpt_lvm, _TL, [lvm_info]).
menu_common_opt(gpt_luks, _TL, [luks_info, luks_passwd]).
menu_common_opt(gpt_luks_lvm, _TL, [lvm_info, luks_info, luks_passwd]).
menu_common_opt(gpt_lvm_luks, _TL, [lvm_info, luks_info, luks_passwd]).

menu_common :-
	Title = ' Common installation settings ',
	menu_list_2(menu_common, [], [], [], [title(Title), cancel-label('Return'), ok-label('Edit')]),
	true.

menu_main_info(_TT, _TL, [bios_efi]).
menu_main_info(_TT, TL, [ST]) :-
	member(state(ST, _), TL),
	\+ menu_main_skip(ST, TL).
menu_main_info(manual, _TL, [bootloader_dev, make_part_manually, part_select, filesystem]).
menu_main_info(_TT, _TL, [common_settings, review, save, install]).

menu_main_skip(used_d7, _TL) :-
	% Skip used_d7 if only one device is available.
	% memberchk(state(template, ctx_tmpl(_B, manual)), TL),
	inst_setting(dev7, available([_])).
menu_main_skip(bootloader_dev, TL) :-
	% Skip bootloader_dev if only one device is used.
	st_used_d7(TL, [_]).
menu_main_skip(soft, TL) :-
	st_root_fs(TL, FS),
	st_bootloader(TL, B),
	% Skip if there is no software to install.
	findall(S, soft_info(S, B, FS, _DepL, _Descr), []).
menu_main_skip(make_part_tmpl, TL) :-
	memberchk(state(make_part_tmpl, ctx_part4(PTT, FS, _OPTN, _D4L)), TL),
	PTT = one,
	\+ memberchk(FS, [zfs, btrfs]).

menu_main :-
	Title = ' Void Linux installation menu ',
	menu_list_2(menu_main, [], [], [], [title(Title), cancel-label('Exit')]),
	true.

cmd_menu(root_fs, _TT, TL) :- !,
	menu_edit_main(root_fs, TL),
	true.
cmd_menu(mnt_opts, TT, TL) :- !,
	menu_mnt_opts(TT, TL),
	true.
cmd_menu(fs_settings, TT, TL) :- !,
	menu_fs_settings(TT, TL),
	true.
cmd_menu(common_settings, _TT, _TL) :- !,
	menu_common,
	true.
cmd_menu(template, _TT, TL) :- !,
	menu_edit_main(template, TL),
	true.
cmd_menu(bios_efi, TT, TL) :- !,
	menu_bios_efi(TT, TL),
	true.
cmd_menu(bootloader, _TT, TL) :- !,
	menu_edit_main(bootloader, TL),
	true.
cmd_menu(bootloader_dev, TT, TL) :- !,
	( TT = manual ->
	  menu_bootloader_dev(TL)
	; menu_edit_main(bootloader_dev, TL)
	),
	true.
cmd_menu(make_part_manually, _TT, _TL) :- !,
	menu_part_manually,
	true.
cmd_menu(make_part_tmpl, _TT, TL) :- !,
	menu_edit_main(make_part_tmpl, TL),
	true.
cmd_menu(used_d7, _TT, TL) :- !,
	menu_edit_main(used_d7, TL),
	true.
cmd_menu(soft, _TT, TL) :- !,
	menu_edit_main(soft, TL),
	true.
cmd_menu(part_select, _TT, TL) :- !,
	menu_part_select(TL),
	true.
cmd_menu(filesystem, _TT, TL) :- !,
	menu_filesystem(TL),
	true.
cmd_menu(review, TT, TL) :- !,
	menu_review(TT, TL),
	true.
cmd_menu(save, _TT, _TL) :- !,
	menu_save,
	true.
cmd_menu(install, TT, TL) :- !,
	run_install(TT, TL),
	true.

cmd_action(install, TT, TL) :- !,
	cmd_menu(install, TT, TL),
	true.
cmd_action(A, TT, TL) :- !,
	cmd_menu(A, TT, TL),
	true.

cmd_menu_common(hostonly) :- !,
	menu_hostonly,
	true.
cmd_menu_common(keymap) :- !,
	menu_keymap,
	true.
cmd_menu_common(network) :- !,
	menu_network,
	true.
cmd_menu_common(source) :- !,
	menu_pkg_inst_method,
	true.
cmd_menu_common(hostname) :- !,
	menu_setting(hostname),
	true.
cmd_menu_common(locale) :- !,
	menu_locale,
	true.
cmd_menu_common(timezone) :- !,
	menu_timezone,
	true.
cmd_menu_common(root_passwd) :- !,
	menu_password_user(root),
	true.
cmd_menu_common(user_passwd) :- !,
	inst_setting(useraccount, user(UL, _UN, _UGL)),
	menu_password_user(UL),
	true.
cmd_menu_common(luks_passwd) :- !,
	menu_password_luks('$_luks_$'),
	true.
cmd_menu_common(useraccount) :- !,
	menu_useraccount_info,
	true.
cmd_menu_common(lvm_info) :- !,
	menu_lvm_info,
	true.
cmd_menu_common(luks_info) :- !,
	menu_luks_info,
	true.
cmd_menu_common(mbr_size) :- !,
	menu_setting(mbr_size),
	true.
cmd_menu_common(esp_size) :- !,
	menu_setting(esp_size),
	true.
cmd_menu_common(boot_size) :- !,
	menu_setting(boot_size),
	true.
cmd_menu_common(root_size) :- !,
	menu_setting(root_size),
	true.

