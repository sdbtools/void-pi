% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

menu_filesystem(TL) :-
	MT1 = ' Select the partition to edit ',
	menu_list_2(edit_fs_short, TL, [], [], [title(MT1), cancel-label('Done')]),
	!.

part2menu_tag(PIL, PD, [PD, SZ]) :-
	memberchk(dev_part(PD,_,_,SZ), PIL),
	true.

menu_fs_short(PD) :-
	dialog_msg(menu, LABEL),
	format_to_atom(TA, ' Set ~w filesystem parameters ', [PD]),
	menu_fs_short_pre(PD, IL),
	menu_list_3(file_system(PD), LABEL, [], IL, OL, [title(TA), extra-label('Accept'), ok-label('Edit')]),
	menu_fs_short_post(PD, IL, OL),
	!.

menu_fs_short_pre(PD, [
		label = Label,
		type = FS,
		mount_point = MP,
		create = FV
	]) :-
	% We HAVE to get TL here.
	inst_setting(template(_TT), TL),
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	( memberchk(fs6(FS, MP, PD, COL, _MOL, CK), TL)
	; memberchk(fs5_multi(FS, COL, [PD], _PTL, CK), TL),
	  MP = ''
	), !,
	fs_get_label(FS, COL, Label),
	( CK = create ->
	  FV = yes
	; FV = no
	),
	true.

menu_fs_short_post(_PD, IL, IL) :- !.
menu_fs_short_post(PD, _IL, OL) :-
	retract(inst_setting(template(TT), TL)),
	get_bootloader(TL, B),
	findall(E2, (member(E1, TL), replace_pd(E1, E2, PD, B, OL)), TL1),
	assertz(inst_setting(template(TT), TL1)),
	true.

% menu_fs_info(tag, name, format)
menu_fs_info(create, 'Create FS', enable) :- !.
menu_fs_info(mount_point, 'Mount point', str) :- !.
menu_fs_info(type, 'Type', fmt(select_fs)) :- !.
menu_fs_info(label, 'Label', str) :- !.

replace_element(E1, E2, E1, E2) :- !.
replace_element(_, _, E1, E1) :- !.

replace_pd(fs6(FS, _MP, PD, COL, MOL, _CK), fs6(FS, MP, PD, COL1, MOL, CK), PD, _B, OL) :-
	memberchk(type=FS, OL), !, % fs6 -> fs6. FS type hasn't changed.
	OL = [
		label=Label,
		type=FS,
		mount_point=MP,
		create=FV
	],
	( FV = yes ->
	  CK = create
	; CK = keep
	),
	fs_set_label(FS, Label, COL, COL1),
	true.
replace_pd(fs6(_FS, _MP, PD, _COL, _MOL, _CK), fs5_multi(FS, COL1, [PD], PTL, CK), PD, B, OL) :-
	memberchk(type=FS, OL),
	memberchk(FS, [zfs, btrfs]), !, % fs6 -> fs5_multi
	OL = [
		label=Label,
		type=FS,
		mount_point=_MP,
		create=FV
	],
	( FV = yes ->
	  CK = create
	; CK = keep
	),
	get_col_multi(FS, B, COL),
	fs_set_label(FS, Label, COL, COL1),
	part_tmpl(FS, root, PTL),
	true.
replace_pd(fs6(_FS, _MP, PD, _COL, _MOL, _CK), fs6(FS, MP, PD, COL1, MOL, CK), PD, B, OL) :- !,
	% fs6 -> fs6. FS type HAS changed.
	OL = [
		label=Label,
		type=FS,
		mount_point=MP,
		create=FV
	],
	( FV = yes ->
	  CK = create
	; CK = keep
	),
	get_col(FS, MP, B, COL),
	get_mol(FS, MP, MOL),
	fs_set_label(FS, Label, COL, COL1),
	true.
replace_pd(fs5_multi(FS, COL, [PD], PTL, _CK), fs5_multi(FS, COL, [PD], PTL, CK), PD, _B, OL) :-
	memberchk(type=FS, OL), !, % fs5_multi -> fs5_multi. FS type hasn't changed.
	memberchk(create=FV, OL),
	( FV = yes ->
	  CK = create
	; CK = keep
	),
	true.
replace_pd(fs5_multi(_FS, _COL, [PD], _PTL, _CK), fs5_multi(FS, COL1, [PD], PTL, CK), PD, B, OL) :-
	memberchk(type=FS, OL),
	memberchk(FS, [zfs, btrfs]), !, % fs5_multi -> fs5_multi. FS type HAS changed.
	OL = [
		label=Label,
		type=FS,
		mount_point=_MP,
		create=FV
	],
	( FV = yes ->
	  CK = create
	; CK = keep
	),
	get_col_multi(FS, B, COL),
	fs_set_label(FS, Label, COL, COL1),
	part_tmpl(FS, root, PTL),
	true.
replace_pd(fs5_multi(_FS, _COL, [PD], _PTL, _CK), fs6(FS, MP, PD, COL1, MOL, CK), PD, B, OL) :-
	% fs5_multi -> fs6
	OL = [
		label=Label,
		type=FS,
		mount_point=MP,
		create=FV
	],
	( FV = yes ->
	  CK = create
	; CK = keep
	),
	get_col(FS, MP, B, COL),
	get_mol(FS, MP, MOL),
	fs_set_label(FS, Label, COL, COL1),
	true.
replace_pd(E1, E1, _PD, _B, _OL) :- !.

fs_type_to_menu(FST, [FST, Descr]) :-
	fs_info(FST, Descr),
	true.

% It is not supposed to assertz.
% TT - template name
% OFS - old file system
% NFS - new file system
menu_select_fs(_TT, zfsBootMenu, _OFS, zfs) :- !.
menu_select_fs(TT, B, OFS, NFS) :-
	inst_setting(fs, available(FSL)),
	% template_info(name, descr, except_fs).
	template_info(TT, _Descr, EL1),
	subtract(FSL, EL1, ML1),
	% bootloader_info(bootloade, supported_fs, supported_template, except_fs).
	bootloader_info(B, _FSL, _, EL2),
	subtract(ML1, EL2, ML2),
	maplist(fs_type_to_menu, ML2, ML),
	dialog_msg(radiolist, LABEL),
	tui_radiolist_tag2(ML, OFS, LABEL, [title(' Select the filesystem type ')], NFS),
	true.

