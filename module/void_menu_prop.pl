% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.
% tui_ext.pl - related.

%%%%%%%%%%
% opt3(name, format, def_value)
prop_info(zpool_bootfs, opt3s('-o', ','), [
	  opt3(default, none, unset)
	, opt3(
		'pool/dataset',
		form_v(
			' ZFS pool & dataset ',
			20, 100,
			['Pool name:', 'Dataset name:']
			),
		['', '']
		)
	]).

prop_info(zpool_cachefile, opt3s('-o', ','), [
	  opt3(default, none, unset)
	, opt3(temporary, none, none)
	, opt3(path, path, '')
	]).

prop_info(zfs_keylocation, opt3s('-O', ','), [
	  opt3(default, none, prompt)
	, opt3(file, file, '')
	]).

%%%%%%%%%%
% menu_fs_opt_info(FS, Tag, PropGrp, Name, feat/opt).
menu_fs_opt_info(zfs, feats, zpool_feat, 'Features', feat).
menu_fs_opt_info(zfs, opts, zpool_rw, 'Options', opt).
menu_fs_opt_info(zfs, zpool_opts, zpool_props_rw, 'zpool Options', opt).
menu_fs_opt_info(zfs, zfs_opts, zfs_rw, 'zfs Options', opt).

menu_fs_opt_info(btrfs, feats, btrfs_feat, 'Features', feat).
menu_fs_opt_info(btrfs, opts, btrfs_rw, 'Options', opt).

menu_fs_opt_info(f2fs, feats, f2fs_feat, 'Features', feat).
menu_fs_opt_info(f2fs, opts, f2fs_rw, 'Options', opt).

menu_fs_opt_info(nilfs2, feats, nilfs2_feat, 'Features', feat).
menu_fs_opt_info(nilfs2, opts, nilfs2_rw, 'Options', opt).

menu_fs_opt_info(exfat, opts, exfat_rw, 'Options', opt).

menu_fs_opt_info(xfs, opts, xfs_rw, 'Options', opt).
menu_fs_opt_info(xfs, b_opts, xfs_block_size_rw, 'Block Size', opt).
menu_fs_opt_info(xfs, g_opts, xfs_global_metadata_rw, 'Global Metadata', opt).
menu_fs_opt_info(xfs, d_opts, xfs_data_section_rw, 'Data Section', opt).
menu_fs_opt_info(xfs, i_opts, xfs_inode_rw, 'INode', opt).
menu_fs_opt_info(xfs, l_opts, xfs_log_rw, 'Log', opt).
menu_fs_opt_info(xfs, n_opts, xfs_naming_rw, 'Naming', opt).
menu_fs_opt_info(xfs, p_opts, xfs_protofile_rw, 'Protofile', opt).
menu_fs_opt_info(xfs, r_opts, xfs_realtime_rw, 'Realtime', opt).
menu_fs_opt_info(xfs, s_opts, xfs_sector_size_rw, 'Sector Size', opt).

menu_fs_opt_info(vfat, opts, vfat_rw, 'Options', opt).

menu_fs_opt_info(bcachefs, opts, bcachefs_rw, 'Options', opt).

menu_fs_opt_info(FS, Tag, PropGrp, Name, FP) :-
	memberchk(FS, [ext2, ext3, ext4]), !,
	prop_extfs_info(FS, N),
	menu_fs_opt_info_ext(N, Tag, PropGrp, Name, FP).

menu_fs_opt_info_ext(_N, feats, extfs_feat, 'Features', feat).
menu_fs_opt_info_ext(_N, opts, extfs_rw, 'Options', opt).
menu_fs_opt_info_ext(_N, opts_ext, extfs_cs_ext_opts, 'Extended Options', opt).
menu_fs_opt_info_ext(N, opts_jrn, extfs_cs_jrn_opts, 'Journal Options', opt) :-
	N > 2.

%%%%%%%%%%
% menu_mnt_opt_info(FS, Tag, PropGrp, Name, feat/opt).
menu_mnt_opt_info(_, cfeats, mnt_indpn_feat, 'Common Features', feat).
menu_mnt_opt_info(_, copts, mnt_indpn_opt, 'Common Options', opt).

menu_mnt_opt_info(bcachefs, feats, mnt_bcachefs_feat, 'bcachefs Features', feat).

menu_mnt_opt_info(btrfs, feats, mnt_btrfs_feat, 'btrfs Features', feat).
menu_mnt_opt_info(btrfs, opts, mnt_btrfs_opt, 'btrfs Features', opt).

menu_mnt_opt_info(f2fs, feats, mnt_f2fs_feat, 'f2fs Features', feat).
menu_mnt_opt_info(f2fs, opts, mnt_f2fs_opt, 'f2fs Options', opt).

menu_mnt_opt_info(vfat, feats, mnt_vfat_feat, 'vfat Features', feat).
menu_mnt_opt_info(vfat, opts, mnt_vfat_opt, 'vfat Options', opt).

menu_mnt_opt_info(xfs, feats, mnt_xfs_feat, 'xfs Features', feat).
menu_mnt_opt_info(xfs, opts, mnt_xfs_opt, 'xfs Options', opt).

menu_mnt_opt_info(nilfs2, feats, mnt_nilfs2_feat, 'nilfs2 Features', feat).
menu_mnt_opt_info(nilfs2, opts, mnt_nilfs2_opt, 'nilfs2 Options', opt).

menu_mnt_opt_info(exfat, opts, mnt_exfat_opt, 'exfat Options', opt).

menu_mnt_opt_info(proc, opts, mnt_proc_opt, 'proc Options', opt).

menu_mnt_opt_info(tmpfs, opts, mnt_tmpfs_opt, 'tmpfs Options', opt).

menu_mnt_opt_info(FS, Tag, PropGrp, Name, FP) :-
	memberchk(FS, [ext2, ext3, ext4]), !,
	menu_mnt_opt_info_ext(Tag, PropGrp, Name, FP),
	true.

menu_mnt_opt_info_ext(feats, mnt_extfs_feat, 'extX Features', feat).
menu_mnt_opt_info_ext(opts, mnt_extfs_opt, 'extX Options', opt).

%%%%%%%%%%
menu_list_on_init(mnt_opts_main, ev(_B, FSL), _IL, L) :- !,
	findall(MP, member(mnt_opt3(MP, _FS, _MOL), FSL), L).

menu_list_on_pre(mnt_opts_main, ev(_B, FSL), _LV, _VL, L) :- !,
	findall([MP, FS], member(mnt_opt3(MP, FS, _MOL), FSL), L).

menu_list_on_post(mnt_opts_main, get, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(mnt_opts_main, set, ev(B, _FSL), _LV, IVL, Tag, OVL) :- !,
	MP = Tag,
	memberchk(mnt_opt3(MP, FS, OMOL), IVL),
	menu_mnt_opts0(FS, MP, B, OMOL, NMOL),
	( OMOL = NMOL ->
	  OVL = IVL
	; % Replace mnt_opt3.
	  findall(mnt_opt3(MP1, FS1, L2), (member(mnt_opt3(MP1, FS1, L1), IVL), (MP1 = MP, FS1 = FS -> L2 = NMOL; L2 = L1)), OVL)
	),
	true.

%%%%%%%%%%
menu_list_on_init(mnt_opts(FS, _MP), _EV, _IL, L) :- !,
	findall(Tag, menu_mnt_opt_info(FS, Tag, _PropGrp, _Name, _FP), L),
	true.

% AL - "all" list.
menu_list_on_pre(mnt_opts(FS, _MP), _EV, AL, _VL, OL) :- !,
	findall([Tag, Name], (member(Tag, AL), menu_mnt_opt_info(FS, Tag, _PropGrp, Name, _FP)), OL),
	true.

menu_list_on_post(mnt_opts(_FS, _MP), get, _EV, _LV, VL, _Tag, VL) :- !.

% VL - value list.
menu_list_on_post(mnt_opts(FS, MP), set, ev(B), _LV, IVL, Tag, OVL) :- !,
	menu_mnt_opt_info(FS, Tag, PropGrp, Name, FP), !,
	format_to_atom(AN, ' ~w ', [Name]),
	( memberchk(attr(PropGrp, MOL), IVL)
	; MOL = []
	), !,
	run_fs_opt_menu(FP, MP, FS, PropGrp, AN, ev(B, IVL), MOL, _OL),
	OVL = IVL,
	( COL = OL ->
	  % Keep old value
	  OVL = IVL
	; COL = [] ->
	  % Add
	  OVL = [attr(PropGrp, OL)| IVL]
	; OL = [] ->
	  % Remove
	  findall(attr(PG, L), (member(attr(PG, L), IVL), PG \= PropGrp), OVL)
	; % Replace
	  findall(attr(PG, L1), (member(attr(PG, L), IVL), (PG = PropGrp -> L1 = OL; L1 = L)), OVL)
	),
	true.

%%%%%%%%%%
menu_list_on_init(fs_settings_main, ev(_B, FSL), _IL, L) :- !,
	findall(MP, member(fs_sett3(MP, _FS, _COL), FSL), L).

menu_list_on_pre(fs_settings_main, ev(_B, FSL), _LV, _VL, L) :- !,
	findall([MP, FS], member(fs_sett3(MP, FS, _COL), FSL), L).

menu_list_on_post(fs_settings_main, get, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(fs_settings_main, set, ev(B, _FSL), _LV, IVL, Tag, OVL) :- !,
	MP = Tag,
	memberchk(fs_sett3(MP, FS, OCOL), IVL),
	menu_fs_settings0(FS, MP, B, OCOL, NCOL),
	( OCOL = NCOL ->
	  OVL = IVL
	; % Replace fs_sett3.
	  findall(fs_sett3(MP1, FS1, L2), (member(fs_sett3(MP1, FS1, L1), IVL), (MP1 = MP, FS1 = FS -> L2 = NCOL; L2 = L1)), OVL)
	),
	true.

%%%%%%%%%%
menu_list_on_init(fs_settings(FS, _MP), _EV, _IL, L) :- !,
	findall(Tag, menu_fs_opt_info(FS, Tag, _PropGrp, _Name, _FP), L),
	true.

% AL - "all" list.
menu_list_on_pre(fs_settings(FS, _MP), _EV, AL, _VL, OL) :- !,
	findall([Tag, Name], (member(Tag, AL), menu_fs_opt_info(FS, Tag, _PropGrp, Name, _FP)), OL),
	true.

menu_list_on_post(fs_settings(_FS, _MP), get, _EV, _LV, VL, _Tag, VL) :- !.

% VL - value list.
menu_list_on_post(fs_settings(FS, MP), set, ev(B), _LV, IVL, Tag, OVL) :- !,
	menu_fs_opt_info(FS, Tag, PropGrp, Name, FP), !,
	format_to_atom(AN, ' ~w ', [Name]),
	( memberchk(attr(PropGrp, COL), IVL)
	; COL = []
	), !,
	run_fs_opt_menu(FP, MP, FS, PropGrp, AN, ev(B, IVL), COL, OL),
	( COL = OL ->
	  % Keep old value
	  OVL = IVL
	; COL = [] ->
	  % Add
	  OVL = [attr(PropGrp, OL)| IVL]
	; OL = [] ->
	  % Remove
	  findall(attr(PG, L), (member(attr(PG, L), IVL), PG \= PropGrp), OVL)
	; % Replace
	  findall(attr(PG, L1), (member(attr(PG, L), IVL), (PG = PropGrp -> L1 = OL; L1 = L)), OVL)
	),
	true.

% MP is used to create an unique key.
run_fs_opt_menu(feat, MP, FS, PropGrp, Name, EV, COL, OL) :- !,
	checklist_2(prop_fs(PropGrp, MP, FS), Name, EV, COL, OL),
	true.
run_fs_opt_menu(opt, MP, FS, PropGrp, Name, EV, COL, OL) :- !,
	dialog_msg(menu, Label),
	menu_list_3(prop_fs(PropGrp, MP, FS), Label, EV, COL, OL, [title(Name), extra-label('Accept'), ok-label('Edit')]),
	true.

%%%%%%%%%%
% AL - "all" list.
menu_list_on_init(prop_fs(TAG, _MP, FS), _EV, _IL, AL) :- !,
	( memberchk(FS, [ext2, ext3, ext4]) ->
	  prop_info_extfs(TAG, FS, AL)
	; prop_info(TAG, _, AL)
	),
	true.

% AL - "all" list.
% VL - value list.
menu_list_on_pre(prop_fs(_TAG, _MP, _FS), _EV, AL, VL, OL) :- !,
	findall([P, V0], (member(ALE, AL), menu_get_tv(ALE, P, DV), menu_show_value_prop_fs(P, DV, VL, V0)), OL).

menu_list_on_post(prop_fs(_TAG, _MP, _FS), get, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(prop_fs(_TAG, _MP, _FS), cancel, _EV, _LV, VL, _Tag, VL) :- !.

% AL - "all" list.
% VL - value list.
menu_list_on_post(prop_fs(TAG, _MP, FS), set, EV, AL, VL, P, OL2) :- !,
	( prop_list_handle(AL, P, VL, OV, NV) ->
	  prop_list_set(AL, VL, P, NV, OL1),
	  prop_fs_on_change(FS, TAG, P, OV, NV, AL, EV, OL1, OL2)
	; OL2 = VL
	),
	true.

% prop_fs_on_change(+FS, +TAG, +P, +OV, +NV, +AL, +EV, +IL, -OL).
% prop_fs_on_change(FS, TAG, P, OV, NV, AL, EV, IL, OL).
prop_fs_on_change(zfs, zfs_rw, encryption, off, _NV, AL, ev(B, _COL), IL, OL) :- !,
	% OV was "off"
	prop_list_set(AL, IL, keyformat, passphrase, OL1),
	( zfs_has_boot_part(B) ->
	  V = =(default, prompt)
	; V = =(file, '/etc/zfs/zroot.key')
	),
	prop_list_set(AL, OL1, keylocation, V, OL),
	true.
prop_fs_on_change(zfs, zfs_rw, encryption, _OV, off, AL, _EV, IL, OL) :- !,
	% NV is "off"
	prop_list_set(AL, IL, keyformat, none, OL1),
	prop_list_set(AL, OL1, keylocation, default=prompt, OL), % "prompt" is a default value
	true.
prop_fs_on_change(_FS, _TAG, _P, _OV, _NV, _LV, _EV, IL, IL).

%%%%%%%%%%
% !!! combo_val is based on opt3.
menu_list_on_init(combo_val(TAG), P=V, IL, []) :- !,
	Tag = combo_val(TAG),
	( retract(tui_setting_tmp(menu_list(Tag), v(_, VL)))
	; VL = IL
	), !,
	opt3_replace(VL, P, V, OL),
	assertz(tui_setting_tmp(menu_list(Tag), v(P, OL))),
	true.

menu_list_on_pre(combo_val(_TAG), _EV, _LV, VL, PL) :- !,
	findall([P0, V0], (member(opt3(P0, _VF, V), VL), menu_show_value(P0, V, V0)), PL),
	true.

menu_list_on_post(combo_val(_TAG), get, _EV, _LV, VL, Tag, Tag=CV) :- !,
	memberchk(opt3(Tag, _VF, CV), VL),
	% menu_retrieve_value(Tag, CV, V),
	true.

menu_list_on_post(combo_val(_TAG), cancel, _EV, _LV, VL, Tag, Tag=CV) :- !,
	memberchk(opt3(Tag, _VF, CV), VL),
	% menu_retrieve_value(Tag, CV, V),
	true.

menu_list_on_post(combo_val(_TAG), set, _EV, _LV, VL, Tag, OL1) :- !,
	memberchk(opt3(Tag, VF, OV), VL), !,
	( handle_vf(VF, Tag, '', OV, NV), NV \= OV ->
	  opt3_replace(VL, Tag, NV, OL1)
	; OL1 = VL
	),
	true.

%%%%%%%%%%
menu_list_on_init(menu_main, _EV, _IL, []) :- !.

menu_list_on_pre(menu_main, _EV, _LV, _VL, PL) :- !,
	inst_setting(template(TT), TL),
	findall(MI, (menu_main_info(TT, TL, MIL), member(MI, MIL)), M1),
	maplist(menu_tag, M1, PL),
	true.

menu_list_on_post(menu_main, get, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(menu_main, set, _EV, _LV, VL, Tag, VL) :- !,
	inst_setting(template(TT), TL),
	action_info(A, Tag, _),
	( cmd_action(A, TT, TL)
	; true
	), !,
	true.

%%%%%%%%%%
menu_list_on_init(menu_common, _EV, _IL, M) :- !,
	inst_setting(template(TT), TL),
	findall(M0, (menu_common_opt(TT, TL, ML0), member(M0, ML0)), M),
	true.

menu_list_on_pre(menu_common, _EV, AL, _VL, ML) :- !,
	inst_setting(template(_TT1), TL1),
	maplist(menu_tag_common(TL1), AL, ML),
	true.

menu_list_on_post(menu_common, get, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(menu_common, set, _EV, _LV, VL, Tag, VL) :- !,
	action_info_common(A, Tag, _, _),
	( cmd_menu_common(A)
	; true
	), !,
	true.

%%%%%%%%%%
menu_list_on_init(edit_fs_short, TL, _IL, ML1) :- !,
	% do not try to edit swap partition.
	findall(PD, (member(p4(PT, bd1([PD| _]), _CK, _SZ), TL), PT \= linux_swap), PL0),
	( PL0 = [] ->
	  tui_msgbox('No partitions was selected.'),
	  fail
	; sort(PL0, PL1)
	),
	lx_list_part(PIL),
	maplist(part2menu_tag(PIL), PL1, ML1),
	true.

menu_list_on_pre(edit_fs_short, _EV, AL, _VL, AL) :- !,
	true.

menu_list_on_post(edit_fs_short, get, _EV, _LV, VL, _Tag, VL) :- !,
	true.

menu_list_on_post(edit_fs_short, set, _EV, _LV, VL, Tag, VL) :- !,
	menu_fs_short(Tag),
	true.

menu_list_on_init(file_system(_PD), _EV, _IL, []) :- !.

menu_list_on_pre(file_system(_PD), _EV, _LV, VL, ML) :- !,
	findall([N, V], (member(P=V, VL), menu_fs_info(P, N, _)), ML),
	true.

menu_list_on_post(file_system(_PD), get, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(file_system(_PD), cancel, _EV, _LV, VL, _Tag, VL) :- !.

menu_list_on_post(file_system(_PD), set, _EV, _LV, IL, Tag, OL) :- !,
	menu_fs_info(P, Tag, Fmt),
	memberchk(P=OV, IL),
	handle_vf(Fmt, Tag, '', OV, NV),
	( OV = NV ->
	  OL = IL
	; findall(P0=V0, (member(P0=V, IL), (P0=P -> V0=NV; V0=V)), OL)
	),
	true.

%%%%%%%%%%
% AL - "all" list.
checklist_on_all(prop_fs(TAG, _MP, FS), ev(_B, COL), AL) :- !,
	( memberchk(FS, [ext2, ext3, ext4]) ->
	  prop_info_extfs(TAG, FS, AL)
	; FS = zfs ->
	  zfs_pool_compatibility_col(COL, V),
	  prop_info_zfs_feat(V, AL)
	; prop_info(TAG, _, AL)
	),
	true.

checklist_on_make(prop_fs(_TAG, _MP, _FS), _EV, AL, VL, PL) :- !,
	findall([P, P, V], (member(prop_feat4(P, DV, _Fmt, _), AL), (memberchk(P=V0, VL) -> V = V0; V = DV)), PL),
	true.

checklist_on_set(prop_fs(_TAG, _MP, _FS), _EV, AL, VL, OL1) :- !,
	% prop_fs_on_change(+FS, +TAG, +P, +OV, +NV, +AL, +EV, +IL, -OL).
	findall(P=V, (member(prop_feat4(P, DV, _Fmt, _), AL), (memberchk(P, VL) -> DV = off, V = on; DV = on, V = off)), OL1),
	true.

%%%%%%%%%%
% OV - old value.
% NV - new value.
% APL - property list
% PVL - propery value list
% prop_list_handle(+, +, +, -, -).
prop_list_handle(APL, P, PVL, OV, NV) :-
	memberchk_tfv(APL, P, VF, DV),
	( memberchk(P=V0, PVL) ->
	  OV = V0
	; OV = DV
	),
	( DV = '' ->
	  DV1 = 'empty string'
	; DV1 = DV
	),
	format_to_atom(Label, 'Default value: ~w', [DV1]),
	handle_vf(VF, P, Label, OV, NV),
	true.

form_set_val([], _, []) :- !.
form_set_val([Name|NT], [Val|VT], [item(Name, Val)|T]) :-
	form_set_val(NT, VT, T).

form_get_val([], _, []) :- !.
form_get_val([_|NT], [V|VT], [V|T]) :- !,
	form_get_val(NT, VT, T).

% OV - old value.
% NV - new value (should be atom).
handle_vf(enum(VL), Title, Label, OV, NV) :- !,
	% dialog_msg(menu, LABEL),
	tui_menu_tag2(VL, OV, Label, [no-tags, title(Title)], NV),
	true.
handle_vf(fmt(FMT), Title, Label, OV, NV) :- !,
	handle_vf_fmt(FMT, Title, Label, OV, NV),
	true.
handle_vf(enable, Title, _Label, OV, NV) :- !,
	tui_menu_tag2([no, yes], OV, 'Default value: no', [no-tags, title(Title)], NV),
	true.
handle_vf(str, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(str_upper, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV0),
	upper(NV0, NV),
	true.
handle_vf(hex, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(octal, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(file, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(path, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(guid, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(int, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], V0),
	% Validation.
	( V0 = '', NV = OV
	; number_atom(_, V0),
	  NV = V0
	; tui_msgbox('Invalid integer number'),
	  NV = OV
	), !,
	true.
handle_vf(real, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], V0),
	% Validation.
	( V0 = '', NV = OV
	; number_atom(_, V0),
	  NV = V0
	; tui_msgbox('Invalid real number'),
	  NV = OV
	), !,
	true.
handle_vf(percentage, Title, Label, OV, NV) :- !,
	tui_inputbox(Label, OV, [title(Title)], V0),
	% Validation.
	( V0 = '', NV = OV
	; number_atom(_, V0),
	  NV = V0
	; tui_msgbox('Invalid integer number'),
	  NV = OV
	), !,
	true.
handle_vf(size, Title, Label, OV, NV) :- !,
	% Should understand KB, MB, GB.
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(target, Title, Label, OV, NV) :- !,
	% Device or label.
	tui_inputbox(Label, OV, [title(Title)], NV),
	true.
handle_vf(combo_val(TAG), Title, Label, OV, NV) :- !,
	( prop_info(TAG, _, PL)
	; format_to_atom(A, 'ERROR: Invalid propery info tag name: ~w', [TAG]),
	  tui_msgbox(A),
	  fail
	), !,
	menu_list_3(combo_val(TAG), Label, OV, PL, NV, [title(Title), extra-label('Accept'), ok-label('Edit')]),
	true.
handle_vf(combo_val(TAG, PL), Title, Label, OV, NV) :- !,
	menu_list_3(combo_val(TAG), Label, OV, PL, NV, [title(Title), extra-label('Accept'), ok-label('Edit')]),
	true.
handle_vf(form_v(Title, H, Len, FL), _Title, _Label, OV, NV) :- !,
	dialog_msg(form, LABEL),
	form_set_val(FL, OV, IFL),
	tui_form_v(H, Len, IFL, LABEL, [title(Title)], VL),
	form_get_val(FL, VL, NV),
	true.
handle_vf(_, _Title, _Label, OV, OV) :- !,
	fail.

% OV - old value.
% NV - new value.
handle_vf_fmt(zpool_compatibility, Title, Label, OV, NV) :- !,
	os_shell2_lines([ls, '/usr/share/zfs/compatibility.d'], CL),
	VL = [off, legacy| CL],
	tui_menu_tag2(VL, OV, Label, [no-tags, title(Title)], NV),
	true.
handle_vf_fmt(select_fs, _Title, _Label, OV, NV) :- !,
	inst_setting(template(TT), TL),
	get_bootloader(TL, B),
	menu_select_fs(TT, B, OV, NV),
	true.
handle_vf_fmt(_FMT, _Title, _Label, OV, OV) :-
	true.

% Add/replace property/value pair.
% OL - output list
% DV - default value (term)
prop_list_set(APL, PVL, P, V, OL) :-
	findall(P0=V0, (member(LE, APL), menu_get_tv(LE, P0, DV), (P0 = P -> write_to_atom(ADV, DV), write_to_atom(AV, V), AV \= ADV, V0 = V; memberchk(P0=V0, PVL))), OL),
	true.

 attr_list_set([attr(TAG, OAL)|T], TAG, P, V, [attr(TAG, NAL)|T]) :- !,
	prop_info(TAG, _, AL),
	prop_list_set(AL, OAL, P, V, NAL),
	true.
 attr_list_set([H|T], TAG, P, V, [H|T1]) :- !,
	attr_list_set(T, TAG, P, V, T1).
 attr_list_set([], TAG, P, V, [attr(TAG, [P=V])]).

% OPL - output property list
opt3_replace(PL, P, V, OPL) :-
	findall(opt3(P0, VF, V0), (member(opt3(P0, VF, V1), PL), (P0 = P -> V0 = V; V0 = V1)), OPL).

opt4_replace(PL, P, V, OPL) :-
	findall(opt4(P0, VF, V0, CLO), (member(opt4(P0, VF, V1, CLO), PL), (P0 = P -> V0 = V; V0 = V1)), OPL).

menu_get_t(opt3(Tag, _VF, _V), Tag) :- !.
menu_get_t(opt4(Tag, _VF, _V, _), Tag) :- !.

menu_get_tv(opt3(Tag, _VF, V), Tag, V) :- !.
menu_get_tv(opt4(Tag, _VF, V, _), Tag, V) :- !.

% menu_get_tfv(opt3(Tag, VF, V), Tag, VF, V) :- !.
% menu_get_tfv(opt4(Tag, VF, V, _), Tag, VF, V) :- !.

memberchk_tv(L, Tag, V) :-
	memberchk(opt3(Tag, _VF, V), L), !.
memberchk_tv(L, Tag, V) :-
	memberchk(opt4(Tag, _VF, V, _), L), !.

memberchk_tfv(L, Tag, VF, V) :-
	memberchk(opt3(Tag, VF, V), L), !.
memberchk_tfv(L, Tag, VF, V) :-
	memberchk(opt4(Tag, VF, V, _), L), !.

menu_show_value(password, V, SV) :- !,
	( V = '' ->
	  SV = 'not set'
	; SV = '********'
	).
menu_show_value(P, V, SV) :-
	menu_retrieve_value(P, V, SV).

menu_show_value_prop_fs(P, DV, VL, V0) :-
	( memberchk(P=V1, VL) ->
	  menu_show_value_prop_fs1(V1, DV, V0)
	; V0 = ''
	),
	true.

menu_show_value_prop_fs1(=(_, V0), DV, V) :- !,
	menu_show_value_prop_fs2(V0, DV, V).
menu_show_value_prop_fs1(V0, DV, V) :- !,
	menu_show_value_prop_fs2(V0, DV, V).

menu_show_value_prop_fs2(V, V, '') :- !.
menu_show_value_prop_fs2(V, _DV, V).

menu_retrieve_value('pool/dataset', [Pool, Dataset], V) :- !,
	( Dataset = '' ->
	  V = Pool
	; format_to_atom(V, '~w/~w', [Pool, Dataset])
	),
	true.
menu_retrieve_value(_P, V, V) :-
	true.

