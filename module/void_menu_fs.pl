% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

menu_filesystem(TL) :-
	% do not try to edit swap partition.
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, (member(p4(PT, bd1([PD| _]), _CK, _SZ), TL), PT \= swap), PL0),
	( PL0 = [] ->
	  tui_msgbox('No partitions was selected.'),
	  fail
	; sort(PL0, PL1)
	),
	lx_list_part_info(PIL),
	maplist(part2menu_tag(PIL), PL1, ML1),
	MT1 = ' Select the partition to edit ',
	dialog_msg(menu, MENULABEL),
	repeat,
	tui_menu_tag2(edit_fs_short, ML1, MENULABEL, [cancel-label('Done'), title(MT1)], PD),
	menu_fs_short(PD),
	!.

menu_fs_short(cancel) :- !,
	true.
menu_fs_short(PD) :-
	dialog_msg(menu, MENULABEL),
	make_tmp_part_rec(PD),
	repeat,
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting_tmp(partition, part4(bd1([PD| _]), _, CK, _SZ)),
	% fs4(FileSystem, Label, MountPoint, [device_list])
	inst_setting_tmp(fs, fs4(FS, Label, MP, [PD| _])),
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
	% fs4(FileSystem, Label, MountPoint, [device_list])
	inst_setting_tmp(fs, fs4(FS, Label, MP, [PD| T])), !,
	tui_inputbox('', Label, [title('Label')], A),
	% fs4(FileSystem, Label, MountPoint, [device_list])
	retractall(inst_setting_tmp(fs, fs4(_, _, _, [PD| _]))),
	assertz(inst_setting_tmp(fs, fs4(FS, A, MP, [PD| T]))),
	fail.
menu_fs_action(type, PD) :- !,
	% fs4(FileSystem, Label, MountPoint, [device_list])
	inst_setting_tmp(fs, fs4(OFS, Label, MP, [PD| T])), !,
	inst_setting(template(TT), TL),
	get_bootloader(TL, B),
	menu_select_fs(TT, B, OFS, NFS),
	retractall(inst_setting_tmp(fs, fs4(_, _, _, [PD| _]))),
	assertz(inst_setting_tmp(fs, fs4(NFS, Label, MP, [PD| T]))),
	fail.
menu_fs_action(mount_point, PD) :- !,
	% fs4(FileSystem, Label, MountPoint, [device_list])
	inst_setting_tmp(fs, fs4(FS, Label, MP, [PD| T])), !,
	tui_inputbox('', MP, [title('Mount Point')], A),
	retractall(inst_setting_tmp(fs, fs4( _, _, _, [PD| _]))),
	assertz(inst_setting_tmp(fs, fs4(FS, Label, A, [PD| T]))),
	fail.
menu_fs_action(create, PD) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting_tmp(partition, part4(bd1([PD| T]), PT, _F, SZ)), !,
	( tui_yesno('Create file system?', [sz([6, 40])]) ->
	  FV = create
	; FV = keep
	),
	retractall(inst_setting_tmp(partition, part4(bd1([PD| _]), _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part4(bd1([PD| T]), PT, FV, SZ))),
	fail.

make_tmp_part_rec(PD) :-
	inst_setting(template(_), TL),
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	memberchk(p4(PT, bd1([PD| T1]), CK, SZ), TL),
	retractall(inst_setting_tmp(partition, part4(bd1([PD| _]), _, _, _))),
	assertz(inst_setting_tmp(partition, part4(bd1([PD| T1]), PT, CK, SZ))),
	% fs4(FileSystem, Label, MountPoint, [device_list])
	memberchk(fs4(FS, Label, MP, [PD| T2]), TL),
	retractall(inst_setting_tmp(fs, fs4( _, _, _, [PD| _]))),
	assertz(inst_setting_tmp(fs, fs4(FS, Label, MP, [PD| T2]))),
	true.

make_perm_part_rec(PD) :-
	inst_setting(template(TT), TL),
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting_tmp(partition, part4(bd1([PD| T1]), PT, CK, SZ)), !,
	maplist(replace_element(p4(_, bd1([PD| _]), _, _), p4(PT, bd1([PD| T1]), CK, SZ)), TL, TL1),
	% fs4(FileSystem, Label, MountPoint, [device_list])
	inst_setting_tmp(fs, fs4(FS, Label, MP, [PD| T2])),
	maplist(replace_element(fs4( _, _, _, [PD| _]), fs4(FS, Label, MP, [PD| T2])), TL1, TL2),
	retract(inst_setting(template(TT), _)),
	assertz(inst_setting(template(TT), TL2)),
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
replace_fs(MP, FS, fs4(_, Label, MP, DL), fs4(FS, Label, MP, DL)) :- !.
replace_fs(_MP, _FS, E, E) :- !.

menu_root_fs_soft(TT, TL) :-
	root_fs(TL, OFS),
	get_bootloader(TL, B),
	menu_select_fs(TT, B, OFS, NFS),
	( OFS = NFS
	; findall(C, (member(C, TL), C \= soft(_)), TL1),
	  maplist(replace_fs('/', NFS), TL1, TL2),
	  get_bootloader(TL2, B),
	  menu_soft_soft(B, NFS, [], SL),
	  append(TL2, SL, TL3),

	  retractall(inst_setting(template(TT), _)),
	  assertz(inst_setting(template(TT), TL3))
	),
	!.

