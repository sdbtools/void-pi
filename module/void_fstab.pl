% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% generate fstab
make_fstab(TL, RD) :-
	atom_concat(RD, '/etc/fstab', FN),
	open(FN, write, S),
	get_fstab_list(TL, MPL),
	forall(member(MP, MPL), make_fstab_(S, MP)),
	close(S),
	os_call2([chmod, '644', FN]),
	!.
make_fstab(_TL, _) :-
	tui_msgbox('Making of fstab has failed.'),
	fail.

get_fstab_attrs(COL, L) :-
	findall(LM, (member(CO, COL), get_fstab_attrs0(CO, L0), member(LM, L0)), L1),
	( L1 = [] ->
	  L = [defaults]
	; L = L1
	), !,
	true.

get_fstab_attrs0(attr(TAG, OL), L) :-
	prop_info(TAG, Format, FL),
	get_fstab_attrs1(Format, FL, OL, L),
	true.

get_fstab_attrs1(feat4s(_Opt, _Sep, Fmt), FL, OL, COL) :- !, % "s" stands for "separator"
	findall(O, (member(F=V, OL), memberchk(prop_feat4(F, _, LFmt, _), FL), make_fs_feat(LFmt, Fmt, F, V, O)), COL),
	true.
get_fstab_attrs1(opt3s(_Opt, _Sep), FL, OL, COL) :- !, % "s" stands for "separator"
	findall(O, (member(P=V, OL), make_opt3_val(FL, P, V, O)), COL),
	true.
get_fstab_attrs1(opt3p(_Opt), FL, OL, COL) :- !, % "p" stands for "prefix"
	findall(O, (member(P=V, OL), make_opt3_val(FL, P, V, O)), COL),
	true.
get_fstab_attrs1(Format, _FL, _OL, _COL) :-
	format_to_atom(A, 'Invalid formatter: ~w', [Format]),
	tui_msgbox(A, [title(' ERROR ')]),
	fail.

make_fstab_(S, fstab4(MP, FS, MOL, PD)) :-
	get_fstab_attrs(MOL, MOL1),
	write_fstab(FS, PD, MP, MOL1, S),
	true.

% For debugging.
% write_fstab(FS, D, MP, MOL, _S) :-
% 	tui_msgbox_w([write_fstab, FS, D, MP, MOL]),
% 	fail.
write_fstab(zfs, _D, _MP, _MOL, _S) :- !,
	% Do nothing
	true.
write_fstab(btrfs, D, MP, MOL, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	L = ['UUID'=U, MP, btrfs, lc(MOL), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(bcachefs, D, MP, MOL, S) :- !,
	lx_get_dev_partuuid(D, PU),
	format(S, '# ~w\n', [D]),
	L = ['PARTUUID'=PU, MP, bcachefs, lc(MOL), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(swap, D, _MP, MOL, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	L = ['UUID'=U, none, swap, lc(MOL), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(proc, _, _, MOL, S) :- !,
	write(S, '# /proc with hidepid (https://wiki.archlinux.org/index.php/Security#hidepid)'), nl(S),
	L = [proc, '/proc', proc, lc(MOL), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(tmpfs, _, _, MOL, S) :- !,
	L = [tmpfs, '/tmp', tmpfs, lc(MOL), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(efivarfs, _, _, MOL, S) :- !,
	L = [efivarfs, '/sys/firmware/efi/efivars', efivarfs, lc(MOL), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(FS, D, MP, MOL, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	fapassno(FS, MP, FSPASSNO),
	L = ['UUID'=U, MP, FS, lc(MOL), '0', FSPASSNO],
	write_fstab_line(L, S),
	true.

get_mol(FS, MP, MOL) :-
	inst_setting(fs_attr(FS, MP, _), mount(MOL)),
	!.
get_mol(_FS, _MP, []).

get_col(FS, MP, B, COL) :-
	inst_setting(fs_attr(FS, MP, B), create(COL)),
	!.
get_col(_FS, _MP, _B, []).

get_col_multi(zfs, B, COL) :- !,
	phrase(zfs_get_col(B), COL).
get_col_multi(FS, B, COL) :-
	get_col(FS, (/), B, COL).

write_fstab_line(L, S) :-
	os_wcmdl(L, '\t', S), nl(S), nl(S).

fapassno(FS, _MP, '0') :-
	member(FS, [f2fs, xfs, btrfs, bcachefs]).
fapassno(_FS, '/', '1').
fapassno(_FS, _MP, '2').

% Similar to get_mp_list but including swap.
% fstab4(MP, FS, MOL, PD)
get_fstab_list(TL, MPL3) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	findall(fstab4(MP, FS, MOL, PD), member(fs6(FS, MP, PD, _COL, MOL, _CK), TL), MPL0),
	findall(FSTAB4, get_fstab_list_multi(FS, TL, FSTAB4), MPL1),
	append(MPL0, MPL1, MPL2),
	sort(MPL2, MPL3),
	true.

get_fstab_list_multi(btrfs, TL, fstab4(MP, btrfs, MOL, PD)) :-
	member(fs5_multi(btrfs, _COL, [PD|_], PTL, _), TL),
	get_fstab_list_multi_btrfs(PTL, MP, MOL),
	true.

get_fstab_list_multi_btrfs(PTL, MP, MOL1) :-
	member(subv(SV, MP, MOL, _), PTL),
	attr_list_set(MOL, mnt_btrfs_opt, subvol, SV, MOL1),
	true.
get_fstab_list_multi_btrfs(_PTL, '/mnt/btr_pool', [attr(mnt_indpn_feat, [atime=off]), attr(mnt_btrfs_opt, [subvolid=5])]).

