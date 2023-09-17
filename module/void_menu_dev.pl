% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

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

