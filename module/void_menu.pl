% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

dialog_msg(menu, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(list, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(radiolist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.') :- !.
dialog_msg(form, 'Use UP and DOWN arrows (or Ctrl/N, Ctrl/P) to move between fields. Use TAB to move between windows.') :- !.

action_info(common_settings, 'Common Attrs', 'Common settings').
action_info(template, 'Template', 'Predefined configuration').
action_info(review, 'Review', 'Show current settings').
action_info(filesystem, 'Filesystem', 'Configure filesystems and mount points').
action_info(part_manually, 'Partition', 'Manually partition disk(s)').
action_info(part_select, 'Select Part', 'Select partition(s) to use').
action_info(keymap, 'Keyboard', 'Set system keyboard').
action_info(locale, 'Locale', 'Set system locale').
action_info(timezone, 'Timezone', 'Set system time zone').
action_info(useraccount, 'User Account', 'Set primary user name and password').
action_info(lvm_info, 'LVM Info', 'Set LVM info').
action_info(luks_info, 'LUKS Mapping Name', 'Set LUKS Mapping Name').
action_info(bootloader_dev, 'Bootloader Dev', 'Set disk to install bootloader').
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
action_info(partition, 'Partitions', 'Partitions to use during installation').
action_info(root_fs, 'Root FS', 'Set Root File System').
action_info(mbr_size, 'MBR Size', 'Set MBR Size').
action_info(esp_size, 'ESP Size', 'Set EFI System Partition Size').
action_info(boot_size, 'Boot Size', 'Set Boot Partition Size').

setting_value(partition, V1) :- !,
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, inst_setting(partition, part4(bd1([PD| _]), _, _, _)), VL),
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
setting_value(bootloader_dev, D) :- !,
	( inst_setting(bootloader_dev, dev3(D, _, _))
	; D = 'not set'
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

select_pkg_inst_method :-
	dialog_msg(radiolist, RADIOLABEL),
	findall([Tag, Label], inst_method_tag(_, _, Tag, Label), L1),
	inst_setting(source, OM),
	inst_method_tag(_, OM, OMT, _),
	tui_radiolist_tag2(L1, OMT, RADIOLABEL, [title(' Select installation source ')], MT),
	inst_method_tag(_, A, MT, _),

	on_inst_method(A),
	retractall(inst_setting(source, _)),
	assertz(inst_setting(source, A)).

menu_password(UL) :-
	format_to_atom(MP1, ' Enter password for user ~w ', [UL]),
	dialog_msg(form, FORMLABEL),
	( inst_setting_tmp(passwd(UL), UP) ->
	  true
	; UP = ''
	),
	tui_passwordform_v(25, 0, [item('Choose a password:', UP), item('Confirm your password:', UP)], FORMLABEL, [title(MP1)], [P1, P2|_]),
	check_password(UL, P1, P2), !,
	retractall(inst_setting_tmp(passwd(UL), _)),
	assertz(inst_setting_tmp(passwd(UL), P1)).

check_password(UL, P1, P2) :-
	P1 \= P2, !,
	tui_yesno('Passwords don\'t match. Would you like to reenter?', [sz([6, 40])]),
	menu_password(UL).
check_password(UL, '', _) :- !,
	tui_yesno('Password is empty. Would you like to reenter?', [sz([6, 40])]),
	menu_password(UL).
check_password(_, _, _).

menu_part_soft(S) :-
	SL = [[cfdisk, 'Easy to use'], [fdisk, 'More advanced']],
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [title(' Select the software for partitioning ')], S).

menu_part_manually :-
	menu_dev(' Select the disk to partition ', dev7(LN,_SN,_TYPE,_RO,_RM,_SIZE,_SSZ)),
	menu_part_soft(S),
	os_call2([S, LN]),
	true.

menu_part_select :-
	% OPL - list of already configured partitions.
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, inst_setting(partition, part4(bd1([PD| _]), _, _, _)), OPL),
	lx_list_part_info(PIL),
	PIL \= [], !,
	maplist(part2taglist, PIL, ML1),
	MT1 = ' Select partition(s) to use ',
	dialog_msg(menu, MENULABEL),
	tui_checklist_tag2(ML1, OPL, MENULABEL, [title(MT1)], PLO),
	update_part_info(OPL, PLO),
	!.
menu_part_select :-
	tui_msgbox('There are no partitions available.', [sz([6, 40])]),
	true.

menu_keymap :-
	os_shell_lines('find /usr/share/kbd/keymaps/ -type f -iname "*.map.gz" -printf "%f\n" | sed \'s|.map.gz||g\' | sort', KML),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(keymap, OKM) ->
	  true
	; OKM = us
	),
	tui_radiolist_tag2(KML, OKM, RADIOLABEL, [no-tags, title(' Select your keymap ')], KM), !,
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
	( inst_setting(locale, OLC) ->
	  true
	; OLC = 'en_US.UTF-8'
	),
	tui_radiolist_tag2(LCL1, OLC, RADIOLABEL, [title(' Select your locale ')], LC), !,
	retractall(inst_setting(locale, _)),
	assertz(inst_setting(locale, LC)).

menu_timezone :-
	AREAS = ['Africa', 'America', 'Antarctica', 'Arctic', 'Asia', 'Atlantic', 'Australia', 'Europe', 'Indian', 'Pacific'],

	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(timezone, OTZ) ->
	  true
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

menu_dev_(dev7(_NAME,SNAME,disk,_RO,_RM,SIZE,SSZ), [SNAME, DIA]) :-
	format_to_atom(DIA, 'size:~w; sector size:~d', [SIZE, SSZ]),
	true.

% L - list of devices to select from.
menu_dev_list(Title, L, D) :-
	maplist(menu_dev_, L, DL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(DL, MENULABEL, [title(Title)], SN),
	member(D, L),
	D = dev7(_NAME,SN,_TYPE,_RO,_RM,_SIZE,_SSZ),
	!.

menu_dev(Title, D) :-
	lx_list_dev_disk(L),
	menu_dev_list(Title, L, D).

menu_dev_light(Title, D) :-
	lx_list_dev_disk(L),
	( L = [D]
	; menu_dev_list(Title, L, D)
	), !.

menu_btrfs :-
	tui_msgbox2([not, implemented, yet]),
	true.

menu_bootloader :-
	B = [
		  grub2
		, rEFInd
		, limine
		% , zfsBootMenu
	],
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(bootloader, OB)
	; OB = none
	), !,
	tui_radiolist_tag2(B, OB, RADIOLABEL, [no-tags, title(' Select a bootloader ')], NB), !,
	( OB = NB
	; menu_template(NB),
	  retractall(inst_setting(bootloader, _)),
	  assertz(inst_setting(bootloader, NB))
	),
	!.

menu_bootloader_dev :-
	lx_list_dev_disk(L),
	maplist(menu_dev_, L, DL),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(bootloader_dev, dev3(OB, _, _))
	; OB = none
	), !,
	append(DL, [[none, 'Manage bootloader otherwise']], BL1),
	tui_radiolist_tag2(BL1, OB, RADIOLABEL, [title(' Select the disk to install the bootloader ')], D), !,
	set_bootloader_dev(D).

split_grp(G, GL) :-
	atom_chars(G, GC),
	split_list_ne(GC, [':'], GCL),
	maplist(chars_atom, GCL, GL),
	true.

grp_on_off(ON, [G, _, N|_], [A, I]) :-
	( member(G, ON)
	-> I = on
	;  I = off
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

menu_useraccount :-
	inst_setting(useraccount, user(LN, UN, _GL)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Login name:', LN),
		item('User name:', UN)
		], FORMLABEL, [title(' User account settings ')], [LN, UN|_]),
	menu_password(LN),
	menu_usergroups(GL),
	retractall(inst_setting(useraccount, _)),
	assertz(inst_setting(useraccount, user(LN, UN, GL))),
	true.

menu_lvm :-
	inst_setting(lvm, lv(VG, LV, SZ)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Volume Group:', VG),
		item('Logic Volume:', LV)
	], FORMLABEL, [title(' LVM settings ')], [VG1, LV1|_]),
	retractall(inst_setting(lvm, _)),
	assertz(inst_setting(lvm, lv(VG1, LV1, SZ))),
	true.

menu_luks :-
	inst_setting(luks, luks(Name)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Name:', Name)
	], FORMLABEL, [title(' LUKS settings ')], [NName|_]),
	retractall(inst_setting(luks, _)),
	assertz(inst_setting(luks, luks(NName))),
	true.

part2menu_tag(PIL, PD, [PD, FSS]) :-
	memberchk(part_info(bd1([PD| _]), _FS, FSS, _Type), PIL),
	true.

menu_filesystem :-
	% do not try to edit swap partition.
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, (inst_setting(partition, part4(bd1([PD| _]), PT, _F, _SZ)), PT \= swap), PL0),
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
	inst_setting_tmp(partition, part4(bd1([PD| _]), _, F, _SZ)),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting_tmp(fs, fs4(FS, Label, MP, bd1([PD| _]))),
	( F = create ->
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
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting_tmp(fs, fs4(FS, Label, MP, bd1([PD| T]))), !,
	tui_inputbox('', Label, [title('Label')], A),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	retractall(inst_setting_tmp(fs, fs4(_, _, _, bd1([PD| _])))),
	assertz(inst_setting_tmp(fs, fs4(FS, A, MP, bd1([PD| T])))),
	fail.
menu_fs_action(type, PD) :- !,
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting_tmp(fs, fs4(OFS, Label, MP, bd1([PD| T]))), !,
	menu_select_fs(OFS, NFS),
	retractall(inst_setting_tmp(fs, fs4(_, _, _, bd1([PD| _])))),
	assertz(inst_setting_tmp(fs, fs4(NFS, Label, MP, bd1([PD| T])))),
	fail.
menu_fs_action(mount_point, PD) :- !,
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting_tmp(fs, fs4(FS, Label, MP, bd1([PD| T]))), !,
	tui_inputbox('', MP, [title('Mount Point')], A),
	retractall(inst_setting_tmp(fs, fs4( _, _, _, bd1([PD| _])))),
	assertz(inst_setting_tmp(fs, fs4(FS, Label, A, bd1([PD| T])))),
	fail.
menu_fs_action(create, PD) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting_tmp(partition, part4(bd1([PD| T]), PT, _F, SZ)), !,
	( tui_yesno('Create file system?', [sz([6, 40])]) -> FV = create
	; FV = keep
	),
	retractall(inst_setting_tmp(partition, part4(bd1([PD| _]), _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part4(bd1([PD| T]), PT, FV, SZ))),
	fail.

make_tmp_part_rec(PD) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting(partition, part4(bd1([PD| T]), PT, F, SZ)),
	retractall(inst_setting_tmp(partition, part4(bd1([PD| _]), _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part4(bd1([PD| T]), PT, F, SZ))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(FS, Label, MP, bd1([PD| T]))),
	retractall(inst_setting_tmp(fs, fs4( _, _, _, bd1([PD| _])))),
	assertz(inst_setting_tmp(fs, fs4(FS, Label, MP, bd1([PD| T])))),
	true.

make_perm_part_rec(PD) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting_tmp(partition, part4(bd1([PD| T]), PT, F, SZ)), !,
	retractall(inst_setting(partition, part4(bd1([PD| _]), _, _, _SZ))),
	assertz(inst_setting(partition, part4(bd1([PD| T]), PT, F, SZ))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting_tmp(fs, fs4(FS, Label, MP, bd1([PD| T]))),
	retractall(inst_setting(fs, fs4( _, _, _, bd1([PD| _])))),
	assertz(inst_setting(fs, fs4(FS, Label, MP, bd1([PD| T])))),
	true.

fs_type_to_menu(FST, [FST, Descr]) :-
	fs_info(FST, Descr),
	true.

% OFS - old file system
% NFS - new file system
menu_select_fs(OFS, NFS) :-
	inst_setting(template, TN),
	menu_select_fs(TN, OFS, NFS),
	true.

% It is not supposed to assertz.
% TN - template name
% OFS - old file system
% NFS - new file system
menu_select_fs(TN, OFS, NFS) :-
	% bootloader_info(bootloade, supported_fs, supported_template).
	% bootloader_info(B, FSL, _),
	FSL = [
		  btrfs
		, ext2
		, ext3
		, ext4
		, f2fs
		, swap
		, vfat
		, xfs
	],
	% template_info(name, descr, except_fs).
	template_info(TN, _Descr, EL),
	subtract(FSL, EL, ML1),
	maplist(fs_type_to_menu, ML1, ML),
	dialog_msg(radiolist, RADIOLABEL),
	tui_radiolist_tag2(ML, OFS, RADIOLABEL, [title(' Select the filesystem type ')], NFS),
	true.

menu_root_fs :-
	inst_setting(template, TN),
	menu_root_fs(TN),
	true.

% B - bootloader
% TN - template name
menu_root_fs(TN) :-
	inst_setting(root_fs, OFS),
	menu_select_fs(TN, OFS, NFS),

	retractall(inst_setting(root_fs, _)),
	assertz(inst_setting(root_fs, NFS)),
	true.

menu_review_opt(S) :-
	S = [
		  partition
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
	inst_setting(root_fs, FS),
	inst_setting(bootloader, B),
	need_boot_part(B, FS).
menu_review_opt([boot_size]) :-
	inst_setting(template, gpt_raid).
menu_review_opt([lvm_info]) :-
	inst_setting(template, gpt_lvm).
menu_review_opt([luks_info, luks_passwd]) :-
	inst_setting(template, gpt_luks1).
menu_review_opt([lvm_info, luks_info, luks_passwd]) :-
	inst_setting(template, gpt_luks1_lvm).

menu_review :-
	findall(S0, (menu_review_opt(SL0), member(S0, SL0)), S),
	maplist(menu_tag_v, S, SL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [no-cancel, title(' Current settings ')], _Tag),
	true.

menu_template(B) :-
	inst_setting(template, OT),
	bootloader_info(B, _, TL0),
	( TL0 = [NT]
	; maplist(template_to_menu, TL0, TL),
	  dialog_msg(radiolist, RADIOLABEL),
	  tui_radiolist_tag2(TL, OT, RADIOLABEL, [no-tags, title(' Choose configuration ')], NT)
	),
	switch_template(OT, NT, B),
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
% menu_common_opt([btrfs_opt]) :-
% 	inst_setting(root_fs, btrfs).
menu_common_opt([mbr_size]) :-
	\+ inst_setting(system(efi), _).
menu_common_opt([boot_size]) :-
	inst_setting(root_fs, FS),
	inst_setting(bootloader, B),
	need_boot_part(B, FS).
menu_common_opt([boot_size]) :-
	inst_setting(template, gpt_raid).
menu_common_opt([lvm_info]) :-
	inst_setting(template, gpt_lvm).
menu_common_opt([luks_info, luks_passwd]) :-
	inst_setting(template, gpt_luks1).
menu_common_opt([lvm_info, luks_info, luks_passwd]) :-
	inst_setting(template, gpt_luks1_lvm).

menu_common :-
	findall(M0, (menu_common_opt(ML0), member(M0, ML0)), M),
	dialog_msg(menu, MENULABEL),
	repeat,
	maplist(menu_tag_v, M, ML),
	tui_menu_tag2(main_common, ML, MENULABEL, [cancel-label('Exit'), title(' Common installation settings ')], Tag),
	action_info(A, Tag, _),
	menu_action(A),
	!.

menu_main :-
	dialog_msg(menu, MENULABEL),
	repeat,
	M = [
		  bootloader
		, template
		, root_fs
		, bootloader_dev
		, part_manually
		, part_select
		, filesystem
		, common_settings
		, review
		, install
	],
	( inst_setting(template, manual) ->
	  subtract(M, [root_fs], M1)
	; subtract(M, [bootloader_dev, part_manually, part_select, filesystem], M1)
	),
	maplist(menu_tag, M1, ML),
	tui_menu_tag2(main, ML, MENULABEL, [extra-button, extra-label('Save'), cancel-label('Exit'), title(' Void Linux installation menu ')], Tag),
	action_info(A, Tag, _),
	menu_action(A),
	true.

cmd_menu(root_fs) :- !,
	menu_root_fs,
	true.
cmd_menu(btrfs_opt) :- !,
	menu_btrfs,
	true.
cmd_menu(common_settings) :- !,
	menu_common,
	true.
cmd_menu(template) :- !,
	inst_setting(bootloader, B),
	menu_template(B),
	true.
cmd_menu(keymap) :- !,
	menu_keymap,
	true.
cmd_menu(network) :- !,
	menu_network,
	true.
cmd_menu(source) :- !,
	select_pkg_inst_method,
	true.
cmd_menu(hostname) :- !,
	menu_setting(hostname),
	true.
cmd_menu(locale) :- !,
	menu_locale,
	true.
cmd_menu(timezone) :- !,
	menu_timezone,
	true.
cmd_menu(root_passwd) :- !,
	menu_password(root),
	true.
cmd_menu(user_passwd) :- !,
	inst_setting(useraccount, user(UL, _UN, _UGL)),
	menu_password(UL),
	true.
cmd_menu(luks_passwd) :- !,
	menu_password('$_luks_$'),
	true.
cmd_menu(useraccount) :- !,
	menu_useraccount,
	true.
cmd_menu(lvm_info) :- !,
	menu_lvm,
	true.
cmd_menu(luks_info) :- !,
	menu_luks,
	true.
cmd_menu(bootloader) :- !,
	menu_bootloader,
	true.
cmd_menu(bootloader_dev) :- !,
	menu_bootloader_dev,
	true.
cmd_menu(part_manually) :- !,
	menu_part_manually,
	true.
cmd_menu(part_select) :- !,
	menu_part_select,
	true.
cmd_menu(filesystem) :- !,
	menu_filesystem,
	true.
cmd_menu(review) :- !,
	menu_review,
	true.
cmd_menu(mbr_size) :- !,
	menu_setting(mbr_size),
	true.
cmd_menu(esp_size) :- !,
	menu_setting(esp_size),
	true.
cmd_menu(boot_size) :- !,
	menu_setting(boot_size),
	true.
cmd_menu(save) :- !,
	menu_save,
	true.
cmd_menu(install) :- !,
	run_install,
	true.
cmd_menu(exit) :- !,
	% tui_yesno('Exit installer?', [sz([6, 40])]),
	true.

menu_action(install) :- !,
	cmd_menu(install),
	true.
menu_action(exit) :- !,
	cmd_menu(exit),
	true.
menu_action(A) :- !,
	cmd_menu(A),
	fail.

