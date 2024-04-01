% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

menu_password_user(UL) :-
	format_to_atom(Title, ' Enter password for user ~w ', [UL]),
	menu_password0(UL, Title).

menu_password_for(For, Tag) :-
	format_to_atom(Title, ' Enter password for ~w ', [For]),
	menu_password0(Tag, Title).

menu_password0(UL, Title) :-
	dialog_msg(form, FORMLABEL),
	( inst_setting_tmp(passwd(UL), UP)
	; UP = ''
	),
	tui_passwordform_v(25, 1024, [item('Choose a password:', UP), item('Confirm your password:', UP)], FORMLABEL, [title(Title)], [P1, P2|_]),
	check_password(UL, Title, P1, P2), !,
	retractall(inst_setting_tmp(passwd(UL), _)),
	assertz(inst_setting_tmp(passwd(UL), P1)).

menu_password_luks(UL) :-
	inst_setting(useraccount, user(UD, _, _)),
	format_to_atom(Title, ' Enter LUKS password for user ~w ', [UD]),
	menu_password0(UL, Title).

check_password(UL, Title, P1, P2) :-
	P1 \= P2, !,
	tui_yesno('Passwords don\'t match. Would you like to reenter?', [sz([6, 40])]),
	menu_password0(UL, Title).
check_password(UL, Title, '', _) :- !,
	tui_yesno('Password is empty. Would you like to reenter?', [sz([6, 40])]),
	menu_password0(UL, Title).
check_password(_, _, _, _).

menu_part_soft(S) :-
	SL = [[cfdisk, 'Easy to use'], [fdisk, 'More advanced']],
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [title(' Select the software for partitioning ')], S).

menu_part_manually :-
	menu_dev7_menu(' Select the disk to partition ', DEV7),
	lx_dev7_to_ldn(DEV7, LN),
	menu_part_soft(S),
	os_call2([S, LN]),
	retract(inst_setting(template(TT), TL)),
	% Remove all selected partitions and file systems.
	findall(E, (member(E, TL), E \= p4(_, _, _, _), E \= fs6(_, _, _, _, _, _)), NTL),
	assertz(inst_setting(template(TT), NTL)),
	true.

% OPTN - old partition template name
% NPTN - new partition template name
menu_part_tmpl(FS, OPTN, NPTN) :-
	findall([TN, TD], (part_tmpl(FS, TN, _), part_tmpl_info(FS, TN, TD)), TL),
	dialog_msg(radiolist, LABEL),
	tui_radiolist_tag2(TL, OPTN, LABEL, [title(' Select partition template ')], NPTN),
	true.

menu_part_select(TL) :-
	% OPL - list of already configured partitions.
	findall(PD, member(p4(_PT, bd1([PD| _]), _CK, _SZ), TL), OPL),
	lx_list_part(PIL),
	PIL \= [], !,
	maplist(part2taglist, PIL, ML1),
	MT1 = ' Select partition(s) to use ',
	dialog_msg(checklist, LABEL),
	tui_checklist_tag2(ML1, OPL, LABEL, [title(MT1)], NPL),
	update_part_info(PIL, OPL, NPL, TL, NTL),
	retract(inst_setting(template(TN), _)),
	assertz(inst_setting(template(TN), NTL)),
	!.
menu_part_select(_TL) :-
	tui_msgbox('There are no partitions available.', [sz([6, 40])]),
	true.

part2taglist(dev_part(PD,_,_,SZ), [PD, SZ]).

update_part_info(_PIL, OPL, OPL, IL, IL) :- !.
update_part_info(PIL, OPL, NPL, IL, OL) :-
	findall(M, (member(M, IL), keep_part_(NPL, M)), IL1),
	get_bootloader(IL, B),
	add_part_(B, PIL, OPL, NPL, IL1, OL),
	true.

% KL - list of partitions to keep.
keep_part_(KL, p4(_PT, bd1([PD| _]), _CK, _SZ)) :- !,
	memberchk(PD, KL).
keep_part_(KL, fs6(_, _, PD, _, _, _)) :- !,
	memberchk(PD, KL).
keep_part_(KL, fs5_multi(_FS, _COL, [PD| _], _PTL, _CK)) :- !,
	memberchk(PD, KL).
keep_part_(_KL, _).

% SL - list of partitions to skip.
add_part_(B, PIL, SL, [PD| T], IL, OL) :-
	memberchk(PD, SL), !,
	add_part_(B, PIL, SL, T, IL, OL).
add_part_(B, PIL, SL, [PD| T], IL, [p4(PT, BD1, keep, SZ), FSX| OL]) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	% dev_part(NAME,name(SNAME,KNAME,DL),ET,SIZE)
	member(dev_part(PD, name(_SNAME, _KNAME, [DL|_]), part5(_PTTYPE, PT, _PARTUUID, _UUID, FS), SZ), PIL),
	BD1 = bd1([PD|DL]), !,
	add_part_fs(FS, B, PT, PD, FSX),
	add_part_(B, PIL, SL, T, IL, OL).
add_part_(_B, _PIL, _SL, [], L, L) :-
	true.

add_part_fs(FS, B, PT, PD, fs5_multi(FS, COL1, [PD], PTL, keep)) :- 
	memberchk(FS, [zfs, btrfs]), !,
	guess_part_info(PT, Label, _MP),
	get_col_multi(FS, B, COL),
	fs_set_label(FS, Label, COL, COL1),
	part_tmpl(FS, root, PTL),
	true.
add_part_fs(FS, B, PT, PD, fs6(FS, MP, PD, COL1, MOL, keep)) :-
	guess_part_info(PT, Label, MP),
	% menu_fs_settings0(FS, MP, B, COL),
	get_col(FS, MP, B, COL),
	get_mol(FS, MP, MOL),
	fs_set_label(FS, Label, COL, COL1),
	true.

guess_part_info(sys_efi, 'EFI', '/boot/efi') :- !.
guess_part_info(linux_data, 'ROOT', '/') :- !.
guess_part_info(_PT, '', '').

menu_bios_efi(TT, TL) :-
	findall([M, MT], (member(M, [bios, efi]), boot_info(M, MT)), ML),
	findall(M, (member(M, [bios, efi]), inst_setting(system(M), _)), OL),
	dialog_msg(checklist, LABEL),
	tui_checklist_tag2(ML, OL, LABEL, [title(' Boot via BIOS/EFI ')], NL),
	( NL \= []
	; tui_msgbox('No boot method was selected', [title(' ERROR ')]),
	  fail
	), !,
	( OL = NL
	; retractall(inst_setting(system(bios), _)),
	  retractall(inst_setting(system(efi), _)),
	  maplist(menu_bios_efi_, NL),

	  findall(B, (menu_bootloader_(L0), member(B, L0)), BL),
	  get_bootloader(TL, OB),

	  ( memberchk(OB, BL) ->
		NB = OB
	  ; BL = [NB| _],
	    tui_msgbox2(['Bootloader will be reset to', NB]),
		replace_bootloader(NB)
	  ), !,

	  ( TT = manual
	  ; tui_msgbox('Template will be reset to "Manual"'),
		set_template(manual, NB)
	  )
	),
	!.

menu_bios_efi_(bios) :- !,
	setup_sys_bios.
menu_bios_efi_(efi) :- !,
	force_sys_efi.

menu_hostonly :-
	inst_setting(hostonly, OHO),
	dialog_msg(radiolist, LABEL),
	tui_radiolist_tag2([[yes, 'Only for experts. Booting only the local host'], [no, 'Include all drivers']], OHO, LABEL, [title(' Host-only ')], NHO),
	( OHO = NHO
	; retractall(inst_setting(hostonly, _)),
	  assertz(inst_setting(hostonly, NHO))
	),
	!.

menu_keymap :-
	os_shell_lines('find /usr/share/kbd/keymaps/ -type f -iname "*.map.gz" -printf "%f\n" | sed \'s|.map.gz||g\' | sort', KML),
	dialog_msg(radiolist, LABEL),
	( inst_setting(keymap, OKM)
	; OKM = us
	),
	tui_radiolist_tag2(KML, OKM, LABEL, [no-tags, title(' Select your keymap ')], KM), !,
	retractall(inst_setting(keymap, _)),
	assertz(inst_setting(keymap, KM)).

make_lng_cntr(A1, [A1, R]) :-
	atom_concat(A2, '.UTF-8', A1),
	atom_chars(A2, LC),
	split_list_ne(LC, ['_'], LCL),
	chars_lc(LCL, LNG, CNTR),
	get_lng_name(LNG, LN),
	get_country_name(CNTR, CN),
	format_to_atom(R, '~w (~w)', [LN, CN]).

chars_lc([LC1, LC2], LNG, CNTR) :- !,
	atom_chars(LNG, LC1),
	atom_chars(CNTR, LC2).
chars_lc([LC1], LNG, '') :- !,
	atom_chars(LNG, LC1).

menu_locale :-
	os_shell_lines('grep -E \'\\.UTF-8\' /etc/default/libc-locales|awk \'{print $1}\'|sed -e \'s/^#//\'', LCL),
	maplist(make_lng_cntr, LCL, LCL1),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(locale, OLC)
	; OLC = 'en_US.UTF-8'
	),
	tui_radiolist_tag2(LCL1, OLC, RADIOLABEL, [title(' Select your locale ')], LC), !,
	retractall(inst_setting(locale, _)),
	assertz(inst_setting(locale, LC)).

menu_timezone :-
	AREAS = ['Africa', 'America', 'Antarctica', 'Arctic', 'Asia', 'Atlantic', 'Australia', 'Europe', 'Indian', 'Pacific'],

	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(timezone, OTZ)
	; OTZ = 'America/New_York'
	),
	split_tz(OTZ, A1, A2),
	tui_radiolist_tag2(AREAS, A1, RADIOLABEL, [no-tags, title(' Select area ')], A), !,

	os_shell2_lines([ls, '/usr/share/zoneinfo/' + A], TZL),

	( A1 = A ->
	  TZ1 = A2
	; TZ1 = none
	),
	tui_radiolist_tag2(TZL, TZ1, RADIOLABEL, [no-tags, title(' Select location ')], TZ), !,

	format_to_atom(ATZ, '~w/~w', [A, TZ]),
	retractall(inst_setting(timezone, _)),
	assertz(inst_setting(timezone, ATZ)).

split_tz(TZ, A1, A2) :-
	atom_codes(TZ, TZL),
	split_list_ne(TZL, "/", LL),
	maplist(codes_atom, LL, [A1, A2]),
	true.

menu_bootloader_([grub2, syslinux]) :-
	% grub2 and syslinux cannot be installed for EFI if booted in BIOS mode.
	( lx_sys_efi(_) ->
	  true
	; \+ inst_setting(system(efi), _)
	),
	true.
menu_bootloader_([limine]).
menu_bootloader_([rEFInd, gummiboot]) :-
	inst_setting(system(efi), _),
	\+ inst_setting(system(bios), _),
	true.
menu_bootloader_([zfsBootMenu]) :-
	inst_setting(system(efi), _),
	\+ inst_setting(system(bios), _),
	inst_setting(fs, available(FSL)),
	memberchk(zfs, FSL),
	true.
menu_bootloader_([efistub]) :-
	% efistub cannot be installed if booted in BIOS mode.
	lx_sys_efi(_),
	\+ inst_setting(system(bios), _),
	true.

split_grp(G, GL) :-
	atom_chars(G, GC),
	split_list_ne(GC, [':'], GCL),
	maplist(chars_atom, GCL, GL),
	true.

grp_on_off(ON, [G, _, N|_], [A, I]) :-
	( member(G, ON) ->
	  I = on
	; I = off
	),
	format_to_atom(A, '~w:~w', [G, N]).

grp_ind2name(L, N, G) :-
	nth(N, L, [G|_]).

menu_usergroups(GL3) :-
	G = [wheel, audio, video, floppy, cdrom, optical, kvm, xbuilder],
	os_shell_lines('cat /etc/group', GL),
	maplist(split_grp, GL, GL1),
	maplist(grp_on_off(G), GL1, GL2),
	dialog_msg(menu, LISTLABEL),
	tui_checklist_ind(GL2, LISTLABEL, [title(' Select group ')], SGL1),
	maplist(grp_ind2name(GL1), SGL1, GL3),
	true.

menu_useraccount_info :-
	inst_setting(useraccount, user(LN, UN, _GL)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Login name:', LN),
		item('User name:', UN)
		], FORMLABEL, [title(' User account settings ')], [LN1, UN1|_]),
	menu_password_user(LN1),
	menu_usergroups(GL),
	retractall(inst_setting(useraccount, _)),
	assertz(inst_setting(useraccount, user(LN1, UN1, GL))),
	true.

menu_lvm_info :-
	inst_setting(lvm, lv(VG, LV, SZ)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Volume Group:', VG),
		item('Logic Volume:', LV)
	], FORMLABEL, [title(' LVM settings ')], [VG1, LV1|_]),
	retractall(inst_setting(lvm, _)),
	assertz(inst_setting(lvm, lv(VG1, LV1, SZ))),
	true.

menu_luks_info :-
	inst_setting(luks, luks(Name)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Name:', Name)
	], FORMLABEL, [title(' LUKS settings ')], [NName|_]),
	retractall(inst_setting(luks, _)),
	assertz(inst_setting(luks, luks(NName))),
	true.

menu_mnt_opts(_TT, TL) :-
	st_bootloader(TL, B),
	findall(mnt_opt3(MP, FS, MOL), member(fs6(FS, MP, _PD, _COL, MOL, _CK), TL), IL1),
	% findall(mnt_opt3('/', FS, COL), (member(fs5_multi(FS, COL, _PDL, _PTL, _CK), TL), FS=btrfs), IL2),
	% append(IL1, IL2, IL),
	IL = IL1,
	menu_list_2(mnt_opts_main, ev(B, IL), IL, _OL, [title(' Mount Options '), cancel-label('Return'), ok-label('Edit')]),
	true.

menu_mnt_opts0(FS, MP, B, OMOL, NMOL) :-
	format_to_atom(Title, ' \'~w\' ~w mount options ', [MP, FS]),
	% MP is used with mnt_opts to create an unique key.
	menu_list_2(mnt_opts(FS, MP), ev(B), OMOL, NMOL, [title(Title), cancel-label('Return'), ok-label('Edit'), no-tags]),
	( OMOL = NMOL
	; retractall(inst_setting(fs_attr(FS, MP, B), mount(_))),
	  assertz(inst_setting(fs_attr(FS, MP, B), mount(NMOL)))
	), !,
	true.

menu_fs_settings0(FS, MP, B, OCOL, NCOL) :-
	format_to_atom(Title, ' \'~w\' ~w settings ', [MP, FS]),
	% get_col(FS, MP, B, OCOL),
	% MP is used with fs_settings to create an unique key.
	menu_list_2(fs_settings(FS, MP), ev(B), OCOL, NCOL, [title(Title), cancel-label('Return'), ok-label('Edit'), no-tags]),
	( OCOL = NCOL
	; retractall(inst_setting(fs_attr(FS, MP, B), create(_))),
	  assertz(inst_setting(fs_attr(FS, MP, B), create(NCOL)))
	), !,
	true.

menu_fs_settings(TT, TL) :-
	st_bootloader(TL, B),
	findall(fs_sett3(MP, FS, COL), member(fs6(FS, MP, _PD, COL, _MOL, create), TL), IL1),
	findall(fs_sett3('/', FS, COL), member(fs5_multi(FS, COL, _PDL, _PTL, create), TL), IL2),
	append(IL1, IL2, IL),
	menu_list_2(fs_settings_main, ev(B, IL), IL, OL, [title(' FS Settings '), cancel-label('Return'), ok-label('Edit')]),
	( IL = OL
	; menu_fs_settings_(IL, OL, TL, NTL),
	  retractall(inst_setting(template(TT), _)),
	  assertz(inst_setting(template(TT), NTL))
	), !,
	true.

menu_fs_settings_([fs_sett3(MP, FS, OCOL)|T], NL, OTL, NTL) :-
	memberchk(fs_sett3(MP, FS, NCOL), NL),
	OCOL \= NCOL, !,
	( memberchk(FS, [btrfs, zfs]) ->
	  replace_fs5_multi(OTL, FS, NCOL, TL)
	; replace_fs6(OTL, MP, FS, NCOL, TL)
	),
	menu_fs_settings_(T, NL, TL, NTL).
menu_fs_settings_([_|T], NL, OTL, NTL) :- !,
	menu_fs_settings_(T, NL, OTL, NTL).
menu_fs_settings_([], _OL, OTL, OTL).

% MP + FS = unique key.
replace_fs6([fs6(FS, MP, PD, _COL, MOL, create)|T], MP, FS, NCOL, [fs6(FS, MP, PD, NCOL, MOL, create)|T]) :- !.
replace_fs6([H|T], MP, FS, NCOL, [H|T1]) :- !,
	replace_fs6(T, MP, FS, NCOL, T1).
replace_fs6([], _MP, _FS, _NCOL, []).

replace_fs5_multi([fs5_multi(FS, _COL1, PDL, PTL, create)|T], FS, NCOL, [fs5_multi(FS, NCOL, PDL, PTL, create)|T]) :- !.
replace_fs5_multi([H|T], FS, NCOL, [H|T1]) :- !,
	replace_fs5_multi(T, FS, NCOL, T1).
replace_fs5_multi([], _FS, _NCOL, []).

