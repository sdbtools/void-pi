% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

dialog_msg(menu, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(list, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(radiolist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.') :- !.
dialog_msg(checklist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.') :- !.
dialog_msg(form, 'Use UP and DOWN arrows (or Ctrl/N, Ctrl/P) to move between fields. Use TAB to move between windows.') :- !.

action_info(common_settings, 'Common Attrs', 'Common settings').
action_info(template, 'Template', 'Predefined configurations').
action_info(review, 'Review', 'Show current settings').
action_info(filesystem, 'Filesystem', 'Configure filesystems and mount points').
action_info(keymap, 'Keyboard', 'Set system keyboard').
action_info(locale, 'Locale', 'Set system locale').
action_info(timezone, 'Timezone', 'Set system time zone').
action_info(useraccount, 'User Account', 'Set primary user name and password').
action_info(luks_info, 'LUKS Mapping Name', 'Set LUKS mapping name').
action_info(bootloader_dev, 'Bootloader Dev', 'Select disk to install bootloader').
action_info(bootloader, 'Bootloader', 'Set bootloader application').
action_info(network, 'Network', 'Set up the network').
action_info(source, 'Source', 'Set source installation').
action_info(hostname, 'Hostname', 'Set system hostname').
action_info(save, extra, 'Save settings on disk').
action_info(install, 'Install', 'Start installation').
action_info(exit, cancel, 'Exit installation').
action_info(root_passwd, 'Root Password', 'Set system root password').
action_info(user_passwd, 'User Password', 'Set user password').
action_info(luks_passwd, 'LUKS Password', 'Set LUKS password').
action_info(btrfs_opt, 'Btrfs', 'Btrfs as root options').
action_info(root_fs, 'Root FS', 'Set root file system').
action_info(mbr_size, 'MBR Size', 'Set MBR size').
action_info(esp_size, 'ESP Size', 'Set EFI system partition size').
action_info(boot_size, 'Boot Size', 'Set boot partition size').
action_info(lvm_info, 'LVM Info', 'Set LVM info').
action_info(make_lvm_vg, 'VG', 'Create LVM volume group').
action_info(make_lvm_lv, 'LV', 'Create LVM logical volume').
action_info(make_luks, 'LUKS', 'Create LUKS device').
action_info(make_part_wiz, 'Partition Wizard', 'Create partition').
action_info(make_part_tmpl, 'Partition Template', 'Select partition template').
action_info(make_part_manually, 'Partition', 'Manually partition disk(s)').
action_info(part_select, 'Select Part', 'Select partition(s) to use').
action_info(part_use, 'Partitions', 'Partitions to use during installation').
action_info(bios_efi, 'BIOS/EFI', 'Support either BIOS or EFI, or both').
action_info(hostonly, 'Host-only', 'Install only what is needed for booting the local host').
action_info(soft, 'Software', 'Select software to install').

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
	( memberchk(bootloader_dev(dev3(D, _PL, _TL1)), TL)
	; D = 'not set'
	), !.
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
	maplist(menu_tag_v(TL), S, SL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [no-cancel, ok-label('Return'), title(' Current settings ')], _Tag),
	true.

% IL - input list (previously selected)
% OL - output list
menu_soft(B, FS, IL, OL) :-
	findall(S, soft_info(S, B, FS, _DepL, _Descr), SL),
	( SL = [] ->
	  OL = []
	; findall(SD, (member(S, SL), menu_soft_(S, SD)), AL),
	  dialog_msg(checklist, LABEL),
	  tui_checklist_tag2(AL, IL, LABEL, [title(' Select Software ')], OL1),
	  % tui_checklist_tag(AL, LABEL, [title(' Select Software ')], OL1),
	  findall(soft(S), member(S, OL1), OL2),
	  OL = [state(soft, ctx_soft(FS, B))|OL2]
	),
	true.

menu_soft_(S, [S, Descr]) :-
	soft_info(S, _B, _FS, _DepL, Descr), !,
	true.

% C - command.
menu_edit_main(C, OTL) :-
	menu_edit_main_3(OTL, C, NTL),
	memberchk(state(template, ctx_tmpl(_B, NT)), NTL),
	retractall(inst_setting(template(_), _)),
	assertz(inst_setting(template(NT), NTL)),
	true.

menu_edit_main_3([state(bootloader_dev, CTX)|_T], bootloader_dev, [state(bootloader_dev, ctx_bld(B, _NBD))|_L]) :- !,
	CTX = ctx_bld(B, _OBD),
	true.
menu_edit_main_3([state(template, CTX)|T], template, [state(template, ctx_tmpl(B, NT))|L]) :- !,
	CTX = ctx_tmpl(B, OT),
	menu_select_template(B, OT, NT),
	( OT = NT ->
	  L = T
	; make_cmd_list_3(NT, B, L)
	), !,
	true.
menu_edit_main_3([state(root_fs, CTX)|T], root_fs, [state(root_fs, ctx_rfs(PTT, NFS, B, DL, TT))|L]) :- !,
	CTX = ctx_rfs(PTT, OFS, B, DL, TT),
	menu_select_fs(TT, B, OFS, NFS),
	( OFS = NFS ->
	  L = T
	; ( memberchk(state(make_part_tmpl, ctx_part(_PTT, _B, _FS, TN, _DL)), T)
	  ; TN = root
	  ), !,
	  fs_to_fsl(PTT, NFS, TN, B, DL, L0),
	  menu_edit_soft(NFS, T, B, SL),
	  append(L0, SL, L)
	), !,
	true.
menu_edit_main_3([state(make_part_tmpl, CTX)|T], make_part_tmpl, OL) :- !,
	CTX = ctx_part(PTT, B, FS, OTN, DL),
	menu_part_tmpl(FS, OTN, NTN),
	( OTN = NTN ->
	  OL = [state(make_part_tmpl, CTX)|T]
	; fs_to_fsl_6(PTT, FS, NTN, B, DL, L0),
	  menu_edit_soft(FS, T, B, SL),
	  append(L0, SL, OL)
	), !,
	true.
menu_edit_main_3([state(soft, ctx_soft(FS, B))|T], soft, SL) :- !,
	% root_fs(TL, FS),
	menu_edit_soft(FS, T, B, SL),
	true.
menu_edit_main_3([H|T], C, [H|NTL]) :-
	% tui_msgbox_w(H),
	menu_edit_main_3(T, C, NTL).
menu_edit_main_3([], _C, []).

menu_edit_soft(FS, L, B, SL) :-
	findall(S, member(soft(S), L), OSL),
	menu_soft(B, FS, OSL, SL),
	true.

% OB - old bootloader.
% NB - new bootloader.
menu_template(OT, OB, NB) :-
	menu_select_template(NB, OT, NT),
	switch_template(OT, NT, OB, NB),
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
	( tui_yesno('Save settings?', [sz([6, 40])]) ->
	  open('settings.pl', write, S),
	  save_settings(S),
	  close(S)
	; true
	), !.

menu_tag(A, [T,D]) :-
	action_info(A,T,D), !.
menu_tag(A, [A,A]).

menu_tag_v(TL, A, [T, V]) :-
	action_info(A, T, _), !,
	setting_value(TL, A, V).
menu_tag_v(_TL, A, [A,A]).

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
	action_info(Tag, _, Title),
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
menu_common_opt(gpt_raid, _TL, [boot_size]).
menu_common_opt(gpt_lvm, _TL, [lvm_info]).
menu_common_opt(gpt_luks, _TL, [luks_info, luks_passwd]).
menu_common_opt(gpt_luks_lvm, _TL, [lvm_info, luks_info, luks_passwd]).
menu_common_opt(gpt_lvm_luks, _TL, [lvm_info, luks_info, luks_passwd]).

menu_common(TT, TL) :-
	findall(M0, (menu_common_opt(TT, TL, ML0), member(M0, ML0)), M),
	dialog_msg(menu, MENULABEL),
	repeat,
	inst_setting(template(TT1), TL1),
	maplist(menu_tag_v(TL1), M, ML),
	tui_menu_tag2(main_common, ML, MENULABEL, [cancel-label('Return'), title(' Common installation settings ')], Tag),
	action_info(A, Tag, _),
	cmd_action(A,TT1, TL1),
	!.

menu_main_info(_TT, _TL, [bios_efi, bootloader]).
menu_main_info(manual, _TL, [template, bootloader_dev, make_part_manually, part_select, filesystem]).
menu_main_info(_TT, TL, [ST]) :-
	member(state(ST, _), TL).
menu_main_info(_TT, _TL, [common_settings, review, install]).

menu_main :-
	dialog_msg(menu, LABEL),
	repeat,
	inst_setting(template(TT), TL),
	findall(MI, (menu_main_info(TT, TL, MIL), member(MI, MIL)), M1),

	maplist(menu_tag, M1, ML),
	tui_menu_tag2(main, ML, LABEL, [extra-button, extra-label('Save'), cancel-label('Exit'), title(' Void Linux installation menu ')], Tag),
	action_info(A, Tag, _),
	cmd_action(A, TT, TL),
	true.

cmd_menu(root_fs, _TT, TL) :- !,
	menu_edit_main(root_fs, TL),
	true.
cmd_menu(btrfs_opt, _TT, _TL) :- !,
	menu_btrfs,
	true.
cmd_menu(common_settings, TT, TL) :- !,
	menu_common(TT, TL),
	true.
cmd_menu(template, _TT, TL) :- !,
	menu_edit_main(template, TL),
	true.
cmd_menu(bios_efi, TT, TL) :- !,
	menu_bios_efi(TT, TL),
	true.
cmd_menu(hostonly, _TT, _TL) :- !,
	menu_hostonly,
	true.
cmd_menu(keymap, _TT, _TL) :- !,
	menu_keymap,
	true.
cmd_menu(network, _TT, _TL) :- !,
	menu_network,
	true.
cmd_menu(source, _TT, _TL) :- !,
	menu_pkg_inst_method,
	true.
cmd_menu(hostname, _TT, _TL) :- !,
	menu_setting(hostname),
	true.
cmd_menu(locale, _TT, _TL) :- !,
	menu_locale,
	true.
cmd_menu(timezone, _TT, _TL) :- !,
	menu_timezone,
	true.
cmd_menu(root_passwd, _TT, _TL) :- !,
	menu_password_user(root),
	true.
cmd_menu(user_passwd, _TT, _TL) :- !,
	inst_setting(useraccount, user(UL, _UN, _UGL)),
	menu_password_user(UL),
	true.
cmd_menu(luks_passwd, _TT, _TL) :- !,
	menu_password_luks('$_luks_$'),
	true.
cmd_menu(useraccount, _TT, _TL) :- !,
	menu_useraccount_info,
	true.
cmd_menu(lvm_info, _TT, _TL) :- !,
	menu_lvm_info,
	true.
cmd_menu(luks_info, _TT, _TL) :- !,
	menu_luks_info,
	true.
cmd_menu(bootloader, TT, TL) :- !,
	menu_bootloader(TT, TL),
	true.
cmd_menu(bootloader_dev, _TT, TL) :- !,
	menu_bootloader_dev(TL),
	true.
cmd_menu(make_part_manually, _TT, _TL) :- !,
	menu_part_manually,
	true.
cmd_menu(make_part_tmpl, _TT, TL) :- !,
	menu_edit_main(make_part_tmpl, TL),
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
cmd_menu(mbr_size, _TT, _TL) :- !,
	menu_setting(mbr_size),
	true.
cmd_menu(esp_size, _TT, _TL) :- !,
	menu_setting(esp_size),
	true.
cmd_menu(boot_size, _TT, _TL) :- !,
	menu_setting(boot_size),
	true.
cmd_menu(save, _TT, _TL) :- !,
	menu_save,
	true.
cmd_menu(install, TT, TL) :- !,
	run_install(TT, TL),
	true.
cmd_menu(exit, _TT, _TL) :- !,
	% tui_yesno('Exit installer?', [sz([6, 40])]),
	true.

cmd_action(install, TT, TL) :- !,
	cmd_menu(install, TT, TL),
	true.
cmd_action(exit, TT, TL) :- !,
	cmd_menu(exit, TT, TL),
	true.
cmd_action(A, TT, TL) :- !,
	cmd_menu(A, TT, TL),
	fail.

