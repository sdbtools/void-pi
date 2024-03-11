% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% Select devices to use
% OL - old used/boot dev7 list.
menu_dev7_use(TT, OL, DL) :-
	% multi-device templates.
	% memberchk(TT, [gpt_basic, gpt_wizard, gpt_lvm, gpt_lvm_luks, gpt_luks_lvm]), !,
	memberchk(TT, [gpt_basic, gpt_wizard, gpt_lvm, gpt_lvm_luks]), !,
	menu_dev71_checklist(' Select device(s) to use ', OL, DL),
	true.
menu_dev7_use(_TT, _OL, DL) :-
	inst_setting(dev7, available(DL)),
	true.

% OL - old list
menu_dev7_boot_dev(DL, OL, DEV71) :-
	( OL = [DI|_]
	; DI = none
	), !,
	menu_dev71_radiolist(' Select boot device ', DL, DI, DEV71),
	true.

% It is not used at this time.
menu_dev4_boot_dev(D4L, D4) :-
	menu_d41_menu(' Select boot device ', D4L, SD),
	menu_sdn_to_d4(D4L, SD, D4),
	true.

% It is used with manual template.
menu_bootloader_dev(TL) :-
	( get_bootloader_dev7(TL, DEV7) ->
	  lx_dev7_to_sdn(DEV7, OSN)
	; OSN = none
	),
	st_used_d7(TL, L),
	menu_select_bootloader_dev7(L, OSN, NSN),
	( OSN = NSN
	; replace_bootloader_dev7(OSN, NSN, L, TL, NTL),
	  retract(inst_setting(template(TT), _)),
	  assertz(inst_setting(template(TT), NTL))
	),
	!.

% Select from dev7 list.
menu_select_bootloader_dev7(DEV7L, OSN, NSN) :-
	maplist(conv_dev7_to_menu_, DEV7L, DL),
	dialog_msg(radiolist, LABEL),
	append(DL, [[none, 'Manage bootloader otherwise']], BL1),
	tui_radiolist_tag2(BL1, OSN, LABEL, [title(' Select the disk to install the bootloader ')], NSN),
	true.

% L - list of devices to select from.
menu_dev7_menu(Title, L, DEV7) :-
	maplist(conv_dev7_to_menu_, L, DL),
	dialog_msg(menu, LABEL),
	tui_menu_tag(DL, LABEL, [title(Title)], SDN),
	lx_sdn_to_dev7(L, SDN, DEV7),
	!.

% L - list of devices to select from.
% DI - default item.
menu_dev7_radiolist(Title, L, DI, DEV7) :-
	maplist(conv_dev7_to_menu_, L, DL),
	( lx_dev7_to_sdn(DI, SDI)
	; SDI = none
	), !,
	dialog_msg(radiolist, LABEL),
	tui_radiolist_tag2(DL, SDI, LABEL, [title(Title)], Tag),
	lx_sdn_to_dev7(L, Tag, DEV7),
	true.

% L - list of devices to select from.
menu_dev7_checklist(Title, L, DL) :-
	maplist(conv_dev7_to_menu_, L, DL0),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag(DL0, LABEL, [title(Title)], DL1),
	maplist(lx_sdn_to_dev7(L), DL1, DL),
	true.

% AL - list of all dev7.
menu_dev7_checklist(Title, AL, OL, NL) :-
	menu_dev7_checklist_2(Title, AL, OL, NL),
	( NL \= []
	; tui_msgbox('No device selected'),
	  fail
	),
	!.

% L - list of devices to select from.
% OL - old list of selected items.
% NL - new list of selected items.
menu_dev7_checklist_2(Title, L, OL, NL) :-
	maplist(conv_dev7_to_menu_, L, DL0),
	( maplist(lx_dev7_to_sdn, OL, DL1)
	; DL1 = [none]
	), !,
	dialog_msg(checklist, LABEL),
	tui_checklist_tag2(DL0, DL1, LABEL, [title(Title)], NL1),
	maplist(lx_sdn_to_dev7(L), NL1, NL),
	true.

menu_dev_combo_checklist2(Title, L, OL, NL) :-
	maplist(conv_cdev_to_menu_, L, DL0),
	maplist(conv_cdev_to_sdn_, OL, DL1),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag2(DL0, DL1, LABEL, [title(Title)], NL1),
	maplist(conv_sdn_to_cdev_(L), NL1, NL),
	true.

% OV - old combo value
% NV - new combo value
menu_dev_combo_menu(Title, L, OV, NV) :-
	maplist(conv_cdev_to_menu_, L, DL0),
	conv_cdev_to_sdn_(OV, DI),
	dialog_msg(menu, LABEL),
	tui_menu_tag(DL0, LABEL, [default-item(DI), title(Title)], NV1),
	conv_sdn_to_cdev_(L, NV1, NV),
	true.

conv_dev7_to_d4(DEV7, d4(LN, SN, D, 1)) :-
	lx_dev7_to_ldn_sdn(DEV7, LN, SN),
	lx_part_name(SN, 1, D),
	true.

%
conv_dev7_to_menu_(dev7(_NAME,SNAME,disk,_RO,_RM,SIZE,SSZ), [SNAME, DIA]) :-
	format_to_atom(DIA, 'size:~w; sector size:~d', [SIZE, SSZ]),
	true.

%
conv_cdev_to_menu_(dev7(_NAME,SNAME,disk,_RO,_RM,SIZE,SSZ), [SNAME, DIA]) :- !,
	format_to_atom(DIA, 'disk size:~w; sector size:~d', [SIZE, SSZ]),
	true.
conv_cdev_to_menu_(lvm_vg(_LNAME,SNAME), [SNAME, 'VG']) :- !,
	true.
conv_cdev_to_menu_(luks(_LNAME,SNAME), [SNAME, 'LUKS']) :- !,
	true.

%
conv_cdev_to_sdn_(DEV7, SDN) :-
	lx_dev7_to_sdn(DEV7, SDN),
	!.
conv_cdev_to_sdn_(lvm_vg(_LNAME,SNAME), SNAME) :- !,
	true.
conv_cdev_to_sdn_(luks(_LNAME,SNAME), SNAME) :- !,
	true.
conv_cdev_to_sdn_(V, V) :- !.

%
conv_sdn_to_cdev_(L, SDN, DEV7) :-
	lx_sdn_to_dev7(L, SDN, DEV7),
	!.
conv_sdn_to_cdev_(L, SDN, VG) :-
	member(VG, L),
	VG = lvm_vg(_LNAME,SDN),
	!.
conv_sdn_to_cdev_(L, SDN, LUKS) :-
	member(LUKS, L),
	LUKS = luks(_LNAME,SDN),
	!.

menu_dev7_menu(Title, D) :-
	lx_list_dev7_disk(L),
	menu_dev7_menu(Title, L, D).

menu_dev7_radiolist(Title, OD, ND) :-
	lx_list_dev7_disk(L),
	menu_dev7_radiolist(Title, L, OD, ND).

menu_dev71_menu(Title, D) :-
	lx_list_dev7_disk(L),
	menu_dev71_menu(Title, L, D).

% L - list of devices to select from.
menu_dev71_menu(_Title, [D], D) :- !.
menu_dev71_menu(Title, L, D) :-
	menu_dev7_menu(Title, L, D).

% L - list of devices to select from.
menu_dev71_radiolist(_Title, [ND], _OD, ND) :- !.
menu_dev71_radiolist(Title, L, OD, ND) :-
	menu_dev7_radiolist(Title, L, OD, ND).

menu_dev71_checklist(Title, OL, NL) :-
	inst_setting(dev7, available(AL)),
	( AL = [_] ->
	  NL = AL
	; menu_dev7_checklist(Title, AL, OL, NL)
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

