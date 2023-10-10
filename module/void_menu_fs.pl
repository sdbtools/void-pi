% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

menu_filesystem(TL) :-
	% do not try to edit swap partition.
	findall(PD, (member(p4(PT, bd1([PD| _]), _CK, _SZ), TL), PT \= linux_swap), PL0),
	( PL0 = [] ->
	  tui_msgbox('No partitions was selected.'),
	  fail
	; sort(PL0, PL1)
	),
	lx_list_part(PIL),
	maplist(part2menu_tag(PIL), PL1, ML1),
	MT1 = ' Select the partition to edit ',
	dialog_msg(menu, MENULABEL),
	repeat,
	tui_menu_tag2(edit_fs_short, ML1, MENULABEL, [cancel-label('Done'), title(MT1)], PD),
	menu_fs_short(PD),
	!.

part2menu_tag(PIL, PD, [PD, SZ]) :-
	memberchk(dev_part(PD,_,_,SZ), PIL),
	true.

menu_fs_short(cancel) :- !,
	true.
menu_fs_short(PD) :-
	dialog_msg(menu, MENULABEL),
	make_tmp_part_rec(PD),
	repeat,
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| _], _CAL, _MOL, CK)),
	( CK = create ->
	  FV = yes
	; FV = no
	),
	ML1 = [
		[label, Label],
		[type, FS],
		[mount_point, MP],
		[create, FV]
	],
	maplist(make_menu_fs_short, ML1, ML2),
	format_to_atom(TA, ' Set ~w filesystem parameters ', [PD]),
	tui_menu_tag2(file_system, ML2, MENULABEL, [extra-button, extra-label('Accept'), ok-label('Edit'), title(TA)], Tag),
	menu_fs_info(CMD, Tag),
	menu_fs_action(CMD, PD), !,
	fail.

make_menu_fs_short([T, V], [N, V]) :-
	menu_fs_info(T, N),
	true.

menu_fs_info(create, 'Create FS') :- !.
menu_fs_info(mount_point, 'Mount point') :- !.
menu_fs_info(type, 'Type') :- !.
menu_fs_info(label, 'Label') :- !.
menu_fs_info(save, extra) :- !.
menu_fs_info(exit, cancel) :- !.

menu_fs_action(exit, _) :- !,
	true.
menu_fs_action(save, PD) :-
	make_perm_part_rec(PD), !,
	tui_msgbox('Settings are saved.'),
	true.
menu_fs_action(label, PD) :- !,
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| T], CAL, MAL, CK)), !,
	tui_inputbox('', Label, [title('Label')], A),
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	retractall(inst_setting_tmp(fs, fs7(_, _, _, [PD| _], _, _, _))),
	assertz(inst_setting_tmp(fs, fs7(FS, A, MP, [PD| T], CAL, MAL, CK))),
	fail.
menu_fs_action(type, PD) :- !,
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	inst_setting_tmp(fs, fs7(OFS, Label, MP, [PD| T], CAL, MAL, CK)), !,
	inst_setting(template(TT), TL),
	get_bootloader(TL, B),
	menu_select_fs(TT, B, OFS, NFS),
	retractall(inst_setting_tmp(fs, fs7(_, _, _, [PD| _], _, _, _))),
	assertz(inst_setting_tmp(fs, fs7(NFS, Label, MP, [PD| T], CAL, MAL, CK))),
	fail.
menu_fs_action(mount_point, PD) :- !,
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| T], CAL, MAL, CK)), !,
	tui_inputbox('', MP, [title('Mount Point')], A),
	retractall(inst_setting_tmp(fs, fs7( _, _, _, [PD| _], _, _, _))),
	assertz(inst_setting_tmp(fs, fs7(FS, Label, A, [PD| T], CAL, MAL, CK))),
	fail.
menu_fs_action(create, PD) :-
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| T], CAL, MAL, _CK)), !,
	( tui_yesno('Create file system?', [sz([6, 40])]) ->
	  FV = create
	; FV = keep
	),
	retractall(inst_setting_tmp(fs, fs7( _, _, _, [PD| _], _, _, _))),
	assertz(inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| T], CAL, MAL, FV))),
	fail.

make_tmp_part_rec(PD) :-
	% We HAVE to get TL here.
	inst_setting(template(_TT), TL),
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	memberchk(fs7(FS, Label, MP, [PD| T2], CAL, MAL, CK), TL),
	retractall(inst_setting_tmp(fs, fs7( _, _, _, [PD| _], _, _, _))),
	assertz(inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| T2], CAL, MAL, CK))),
	true.

make_perm_part_rec(PD) :-
	inst_setting(template(TT), TL),
	% fs7(Name, Label, MountPoint, [DevList], [CreateAttrList], [MountOptList], create/keep)
	inst_setting_tmp(fs, fs7(FS, Label, MP, [PD| T2], CAL, MAL, CK)),
	maplist(replace_element(fs7( _, _, _, [PD| _], _, _, _), fs7(FS, Label, MP, [PD| T2], CAL, MAL, CK)), TL, TL1),
	retract(inst_setting(template(TT), _)),
	assertz(inst_setting(template(TT), TL1)),
	true.

replace_element(E1, E2, E1, E2) :- !.
replace_element(_, _, E1, E1) :- !.

fs_type_to_menu(FST, [FST, Descr]) :-
	fs_info(FST, Descr),
	true.

% It is not supposed to assertz.
% TT - template name
% OFS - old file system
% NFS - new file system
menu_select_fs(TT, B, OFS, NFS) :-
	FSL = [
		  btrfs
		% , bcachefs
		, ext2
		, ext3
		, ext4
		, f2fs
		, swap
		, vfat
		, xfs
		% , zfs
	],
	% template_info(name, descr, except_fs).
	template_info(TT, _Descr, EL1),
	subtract(FSL, EL1, ML1),
	% bootloader_info(bootloade, supported_fs, supported_template, except_fs).
	bootloader_info(B, _FSL, _, EL2),
	subtract(ML1, EL2, ML2),
	maplist(fs_type_to_menu, ML2, ML),
	dialog_msg(radiolist, RADIOLABEL),
	tui_radiolist_tag2(ML, OFS, RADIOLABEL, [title(' Select the filesystem type ')], NFS),
	true.

% TT - template name
menu_root_fs(TT, B, NFS) :-
	retract(inst_setting(template(TT1), OTL)),
	( root_fs(OTL, OFS)
	; OFS = ext4
	), !,
	( menu_select_fs(TT, B, OFS, NFS) ->
	  maplist(replace_fs('/', NFS), OTL, NTL),
	  assertz(inst_setting(template(TT1), NTL))
	; assertz(inst_setting(template(TT1), OTL)),
	  fail
	),
	true.

% MP - mount point
% FS - file system
replace_fs(MP, FS, fs7(_, Label, MP, DL, CAL, MAL, CK), fs7(FS, Label, MP, DL, CAL, MAL, CK)) :- !.
replace_fs(_MP, _FS, E, E) :- !.

menu_root_fs_soft(TT, TL) :-
	root_fs(TL, OFS),
	get_bootloader(TL, B),
	menu_select_fs(TT, B, OFS, NFS),
	( OFS = NFS
	; findall(C, (member(C, TL), C \= soft(_)), TL1),
	  maplist(replace_fs('/', NFS), TL1, TL2),
	  menu_soft(B, NFS, [], SL),
	  append(TL2, SL, TL3),

	  retractall(inst_setting(template(TT), _)),
	  assertz(inst_setting(template(TT), TL3))
	),
	!.

