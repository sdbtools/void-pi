% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

dialog_msg(menu, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(list, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(radiolist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.') :- !.
dialog_msg(checklist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.') :- !.
dialog_msg(form, 'Use UP and DOWN arrows (or Ctrl/N, Ctrl/P) to move between fields. Use TAB to move between windows.') :- !.

action_info(common_settings, 'Common Attrs', 'Common settings').
action_info(template, 'Template', 'Predefined configuration').
action_info(review, 'Review', 'Show current settings').
action_info(filesystem, 'Filesystem', 'Configure filesystems and mount points').
action_info(keymap, 'Keyboard', 'Set system keyboard').
action_info(locale, 'Locale', 'Set system locale').
action_info(timezone, 'Timezone', 'Set system time zone').
action_info(useraccount, 'User Account', 'Set primary user name and password').
action_info(luks_info, 'LUKS Mapping Name', 'Set LUKS Mapping Name').
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
action_info(root_fs, 'Root FS', 'Set Root File System').
action_info(mbr_size, 'MBR Size', 'Set MBR Size').
action_info(esp_size, 'ESP Size', 'Set EFI System Partition Size').
action_info(boot_size, 'Boot Size', 'Set Boot Partition Size').
action_info(lvm_info, 'LVM Info', 'Set LVM info').
action_info(make_lvm_vg, 'VG', 'Create LVM Volume Group').
action_info(make_lvm_lv, 'LV', 'Create LVM Logical Volume').
action_info(make_luks, 'LUKS', 'Create LUKS Device').
action_info(make_part, 'Partition', 'Create Partition').
action_info(make_part_manually, 'Partition', 'Manually partition disk(s)').
action_info(part_select, 'Select Part', 'Select partition(s) to use').
action_info(part_use, 'Partitions', 'Partitions to use during installation').

setting_value(part_use, V1) :- !,
	inst_setting(template(_), TL),
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, member(p4(_PT, bd1([PD| _]), _CK, _SZ), TL), VL),
	write_to_atom(V1, VL).
setting_value(root_passwd, '********') :-
	inst_setting_tmp(passwd(root), _), !.
setting_value(user_passwd, '********') :-
	inst_setting(useraccount, user(UN, _, _)),
	inst_setting_tmp(passwd(UN), _), !.
setting_value(luks_passwd, '********') :-
	inst_setting_tmp(passwd('$_luks_$'), _), !.
setting_value(lvm_info, V1) :- !,
	inst_setting(lvm, lv(VG, LV, _SZ)),
	format_to_atom(V1, 'VG: ~w, LV: ~w', [VG, LV]).
setting_value(luks_info, N) :- !,
	inst_setting(luks, luks(N)).
setting_value(bootloader, B) :- !,
	inst_setting(template(_), TL),
	( get_bootloader(TL, B)
	; B = 'not set'
	), !.
setting_value(bootloader_dev, D) :- !,
	inst_setting(template(_), TL),
	( memberchk(bootloader_dev(dev3(D, _PL, _TL1)), TL)
	; D = 'not set'
	), !.
setting_value(root_fs, FS) :- !,
	inst_setting(template(_), TL),
	% fs4(Name, Label, MountPoint, [DevList])
	( memberchk(fs4(FS, _Label, '/', _DL), TL)
	; FS = 'not set'
	), !.
setting_value(S, V1) :-
	inst_setting(S, V), !,
	write_to_atom(V1, V).
setting_value(_, 'not set').

setting_value_str(S, V) :-
	setting_value(S, V1),
	format_to_atom(V, '~w: ~w', [S, V1]).

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

% L - list of devices to select from.
menu_dev7_menu(Title, L, DEV7) :-
	maplist(menu_dev7_menu_, L, DL),
	dialog_msg(menu, LABEL),
	tui_menu_tag(DL, LABEL, [title(Title)], SDN),
	lx_sdn_to_dev7(L, SDN, DEV7),
	!.

% L - list of devices to select from.
menu_dev7_checklist(Title, L, DL) :-
	maplist(menu_dev7_menu_, L, DL0),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag(DL0, LABEL, [title(Title)], DL1),
	maplist(lx_sdn_to_dev7(L), DL1, DL),
	true.

menu_dev7_checklist_used(Title, L, NL) :-
	( inst_setting(dev7, used(OL))
	; OL = []
	), !,
	menu_dev7_checklist2(Title, L, OL, NL),
	( NL \= []
	; tui_msgbox('No device selected'),
	  fail
	),
	( OL = []
	; retract(inst_setting(dev7, used(OL)))
	),
	assertz(inst_setting(dev7, used(NL))),
	!.

% L - list of devices to select from.
% OL - old list of selected items.
% NL - new list of selected items.
menu_dev7_checklist2(Title, L, OL, NL) :-
	maplist(menu_dev7_menu_, L, DL0),
	maplist(lx_dev7_to_sdn, OL, DL1),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag2(DL0, DL1, LABEL, [title(Title)], NL1),
	maplist(lx_sdn_to_dev7(L), NL1, NL),
	true.

menu_dev_combo_checklist2(Title, L, OL, NL) :-
	maplist(menu_dev_combo_to_menu_, L, DL0),
	maplist(menu_dev_combo_to_sdn_, OL, DL1),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag2(DL0, DL1, LABEL, [title(Title)], NL1),
	maplist(menu_sdn_to_dev_combo_(L), NL1, NL),
	true.

% OV - old value
% NV - new value
menu_dev_combo_menu(Title, L, OV, NV) :-
	maplist(menu_dev_combo_to_menu_, L, DL0),
	menu_dev_combo_to_sdn_(OV, DI),
	dialog_msg(menu, LABEL),
	tui_menu_tag(DL0, LABEL, [default-item(DI), title(Title)], NV1),
	menu_sdn_to_dev_combo_(L, NV1, NV),
	true.

menu_dev7_menu_(dev7(_NAME,SNAME,disk,_RO,_RM,SIZE,SSZ), [SNAME, DIA]) :-
	format_to_atom(DIA, 'size:~w; sector size:~d', [SIZE, SSZ]),
	true.

menu_dev_combo_to_menu_(dev7(_NAME,SNAME,disk,_RO,_RM,SIZE,SSZ), [SNAME, DIA]) :- !,
	format_to_atom(DIA, 'disk size:~w; sector size:~d', [SIZE, SSZ]),
	true.
menu_dev_combo_to_menu_(lvm_vg(_LNAME,SNAME), [SNAME, 'VG']) :- !,
	true.
menu_dev_combo_to_menu_(luks(_LNAME,SNAME), [SNAME, 'LUKS']) :- !,
	true.

menu_dev_combo_to_sdn_(DEV7, SDN) :-
	lx_dev7_to_sdn(DEV7, SDN),
	!.
menu_dev_combo_to_sdn_(lvm_vg(_LNAME,SNAME), SNAME) :- !,
	true.
menu_dev_combo_to_sdn_(luks(_LNAME,SNAME), SNAME) :- !,
	true.
menu_dev_combo_to_sdn_(V, V) :- !.

menu_sdn_to_dev_combo_(L, SDN, DEV7) :-
	lx_sdn_to_dev7(L, SDN, DEV7),
	!.
menu_sdn_to_dev_combo_(L, SDN, VG) :-
	member(VG, L),
	VG = lvm_vg(_LNAME,SDN),
	!.
menu_sdn_to_dev_combo_(L, SDN, LUKS) :-
	member(LUKS, L),
	LUKS = luks(_LNAME,SDN),
	!.

menu_dev7_menu(Title, D) :-
	lx_list_dev7_disk(L),
	menu_dev7_menu(Title, L, D).

menu_dev71_menu(Title, D) :-
	lx_list_dev7_disk(L),
	menu_dev71_menu(Title, L, D).

% L - list of devices to select from.
menu_dev71_menu(_Title, [D], D) :- !.
menu_dev71_menu(Title, L, D) :-
	menu_dev7_menu(Title, L, D).

menu_dev71_menu_used(Title, L, D) :-
	menu_dev71_menu(Title, L, D),
	retractall(inst_setting(dev7, used(_))),
	assertz(inst_setting(dev7, used([D]))).

menu_dev7_checklist_used_light(Title, NL) :-
	inst_setting(dev7, available(L)),
	( L = [_] ->
	  NL = L,
	  retractall(inst_setting(dev7, used(_))),
	  assertz(inst_setting(dev7, used(NL)))
	; menu_dev7_checklist_used(Title, L, NL)
	),
	!.

% L - list of devices to select from.
menu_d4_checklist(Title, L, DL) :-
	maplist(menu_d4_to_sdn, L, DL0),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag(DL0, LABEL, [title(Title)], DL1),
	maplist(menu_sdn_to_d4(L), DL1, DL),
	true.

menu_d41_menu(_Title, [D4], D4) :- !.
menu_d41_menu(Title, L, D4) :-
	menu_d4_menu(Title, L, D4).

menu_d4_menu(Title, L, D4) :-
	maplist(menu_d4_to_sdn, L, DL),
	dialog_msg(menu, LABEL),
	tui_menu_tag(DL, LABEL, [title(Title)], SN),
	menu_sdn_to_d4(L, SN, D4),
	true.

menu_d4_to_sdn(d4(LDN,_SN,SDN,N), [SDN, A]) :-
	format_to_atom(A, '~w~d', [LDN, N]).

menu_sdn_to_d4(L, SDN, D4) :-
	member(D4, L),
	D4 = d4(_LN,_SN,SDN,_N),
	!.

menu_d4_checklist_light(_Title, [D], [D]) :- !.
menu_d4_checklist_light(Title, L, DL) :-
	menu_d4_checklist(Title, L, DL).

part2menu_tag(PIL, PD, [PD, FSS]) :-
	memberchk(part_info(bd1([PD| _]), _FS, FSS, _Type), PIL),
	true.

menu_review_opt(S) :-
	S = [
		  part_use
		, root_fs
		, keymap
		, locale
		, timezone
		, bootloader
		, bootloader_dev
		, network
		, hostname
		, source
		, root_passwd
		, useraccount
		, user_passwd
		, esp_size
	].
menu_review_opt([mbr_size]) :-
	\+ inst_setting(system(efi), _).
menu_review_opt([boot_size]) :-
	inst_setting(fs_info, info('/', FS)),
	inst_setting(template(TT), TL),
	get_bootloader(TL, B),
	need_boot_part(TT, B, FS).
menu_review_opt([boot_size]) :-
	inst_setting(template(gpt_raid), _).
menu_review_opt([lvm_info]) :-
	inst_setting(template(gpt_lvm), _).
menu_review_opt([luks_info, luks_passwd]) :-
	inst_setting(template(gpt_luks), _).
menu_review_opt([lvm_info, luks_info, luks_passwd]) :-
	inst_setting(template(gpt_luks_lvm), _).
menu_review_opt([lvm_info, luks_info, luks_passwd]) :-
	inst_setting(template(gpt_lvm_luks), _).

menu_review :-
	findall(S0, (menu_review_opt(SL0), member(S0, SL0)), S),
	maplist(menu_tag_v, S, SL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [no-cancel, ok-label('Return'), title(' Current settings ')], _Tag),
	true.

% NB - new bootloader.
menu_template(NB) :-
	inst_setting(template(OT), _),
	bootloader_info(NB, _, TL0),
	( TL0 = [NT]
	; maplist(template_to_menu, TL0, TL),
	  dialog_msg(radiolist, RADIOLABEL),
	  tui_radiolist_tag2(TL, OT, RADIOLABEL, [no-tags, title(' Choose configuration ')], NT)
	),
	switch_template(OT, NT, NB),
	true.

template_to_menu(T, [T, Descr]) :-
	template_info(T, Descr, _),
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

menu_tag_v(A, [T, V]) :-
	action_info(A, T, _), !,
	setting_value(A, V).
menu_tag_v(A, [A,A]).

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

menu_common_opt(M) :-
	M = [
		  keymap
		, network
		, source
		, hostname
		, locale
		, timezone
		, root_passwd
		, useraccount
		, user_passwd
		, esp_size
	].
menu_common_opt([mbr_size]) :-
	\+ inst_setting(system(efi), _).
menu_common_opt([boot_size]) :-
	inst_setting(fs_info, info('/', FS)),
	inst_setting(template(TT), TL),
	get_bootloader(TL, B),
	need_boot_part(TT, B, FS).
menu_common_opt([boot_size]) :-
	inst_setting(template(gpt_raid), _).
menu_common_opt([lvm_info]) :-
	inst_setting(template(gpt_lvm), _).
menu_common_opt([luks_info, luks_passwd]) :-
	inst_setting(template(gpt_luks), _).
menu_common_opt([lvm_info, luks_info, luks_passwd]) :-
	inst_setting(template(gpt_luks_lvm), _).
menu_common_opt([lvm_info, luks_info, luks_passwd]) :-
	inst_setting(template(gpt_lvm_luks), _).

menu_common :-
	findall(M0, (menu_common_opt(ML0), member(M0, ML0)), M),
	dialog_msg(menu, MENULABEL),
	repeat,
	maplist(menu_tag_v, M, ML),
	tui_menu_tag2(main_common, ML, MENULABEL, [cancel-label('Return'), title(' Common installation settings ')], Tag),
	action_info(A, Tag, _),
	inst_setting(template(_), TL),
	cmd_action(A, TL),
	!.

menu_main :-
	dialog_msg(menu, MENULABEL),
	repeat,
	M = [
		  bootloader
		, template
		, root_fs
		, bootloader_dev
		, make_part_manually
		, part_select
		, filesystem
		, common_settings
		, review
		, install
	],
	( inst_setting(template(manual), _) ->
	  subtract(M, [root_fs], M1)
	; subtract(M, [bootloader_dev, make_part_manually, part_select, filesystem], M1)
	),
	maplist(menu_tag, M1, ML),
	tui_menu_tag2(main, ML, MENULABEL, [extra-button, extra-label('Save'), cancel-label('Exit'), title(' Void Linux installation menu ')], Tag),
	action_info(A, Tag, _),
	inst_setting(template(_), TL),
	cmd_action(A, TL),
	true.

cmd_menu(root_fs, TL) :- !,
	menu_root_fs(TL),
	true.
cmd_menu(btrfs_opt, _TL) :- !,
	menu_btrfs,
	true.
cmd_menu(common_settings, _TL) :- !,
	menu_common,
	true.
cmd_menu(template, TL) :- !,
	get_bootloader(TL, B),
	menu_template(B),
	true.
cmd_menu(keymap, _TL) :- !,
	menu_keymap,
	true.
cmd_menu(network, _TL) :- !,
	menu_network,
	true.
cmd_menu(source, _TL) :- !,
	menu_pkg_inst_method,
	true.
cmd_menu(hostname, _TL) :- !,
	menu_setting(hostname),
	true.
cmd_menu(locale, _TL) :- !,
	menu_locale,
	true.
cmd_menu(timezone, _TL) :- !,
	menu_timezone,
	true.
cmd_menu(root_passwd, _TL) :- !,
	menu_password_user(root),
	true.
cmd_menu(user_passwd, _TL) :- !,
	inst_setting(useraccount, user(UL, _UN, _UGL)),
	menu_password_user(UL),
	true.
cmd_menu(luks_passwd, _TL) :- !,
	menu_password_luks('$_luks_$'),
	true.
cmd_menu(useraccount, _TL) :- !,
	menu_useraccount_info,
	true.
cmd_menu(lvm_info, _TL) :- !,
	menu_lvm_info,
	true.
cmd_menu(luks_info, _TL) :- !,
	menu_luks_info,
	true.
cmd_menu(bootloader, TL) :- !,
	menu_bootloader(TL),
	true.
cmd_menu(bootloader_dev, TL) :- !,
	menu_bootloader_dev(TL),
	true.
cmd_menu(make_part_manually, _TL) :- !,
	menu_part_manually,
	true.
cmd_menu(part_select, TL) :- !,
	menu_part_select(TL),
	true.
cmd_menu(filesystem, TL) :- !,
	menu_filesystem(TL),
	true.
cmd_menu(review, _TL) :- !,
	menu_review,
	true.
cmd_menu(mbr_size, _TL) :- !,
	menu_setting(mbr_size),
	true.
cmd_menu(esp_size, _TL) :- !,
	menu_setting(esp_size),
	true.
cmd_menu(boot_size, _TL) :- !,
	menu_setting(boot_size),
	true.
cmd_menu(save, _TL) :- !,
	menu_save,
	true.
cmd_menu(install, TL) :- !,
	run_install(TL),
	true.
cmd_menu(exit, _TL) :- !,
	% tui_yesno('Exit installer?', [sz([6, 40])]),
	true.

cmd_action(install, TL) :- !,
	cmd_menu(install, TL),
	true.
cmd_action(exit, TL) :- !,
	cmd_menu(exit, TL),
	true.
cmd_action(A, TL) :- !,
	cmd_menu(A, TL),
	fail.

