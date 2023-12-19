% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

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

make_fstab_(S, fstab4(MP, FS, MOL, PD)) :-
	write_fstab(FS, PD, MP, MOL, S),
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
write_fstab(tmp, _, _, MOL, S) :- !,
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
get_mol(_FS, _MP, [rw, noatime]).

get_col(FS, MP, B, COL) :-
	inst_setting(fs_attr(FS, MP, B), create(COL)),
	!.
get_col(_FS, _MP, _B, []).

get_col_mol(B, FS, MP, COL, MOL) :-
	get_col(FS, MP, B, COL),
	get_mol(FS, MP, MOL),
	true.

write_fstab_line(L, S) :-
	os_wcmdl(L, '\t', S), nl(S), nl(S).

fapassno(FS, _MP, '0') :-
	member(FS, [f2fs, xfs, btrfs]).
fapassno(_FS, '/', '1').
fapassno(_FS, _MP, '2').

% Similar to get_mp_list but including swap.
get_fstab_list(TL, [fstab4(none, proc, PMOL, none), fstab4(none, tmp, TMOL, none)| MPL3]) :-
	get_mol(proc, _, PMOL),
	get_mol(tmp, _, TMOL),
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	findall(fstab4(MP, FS, MOL, PD), member(fs7(FS, _Label, MP, PD, _COL, MOL, _CK), TL), MPL0),
	findall(FSTAB4, get_fstab_list_multi(FS, TL, FSTAB4), MPL1),
	append(MPL0, MPL1, MPL2),
	sort(MPL2, MPL3),
	true.

get_fstab_list_multi(btrfs, TL, fstab4(MP, btrfs, MOL, PD)) :-
	member(fs5_multi(btrfs, _Label, [PD|_], PTL, _, _B, _E), TL),
	get_fstab_list_multi_btrfs(PTL, MP, MOL),
	true.

get_fstab_list_multi_btrfs(PTL, MP, [subvol=concat('/', SV)|MOL]) :-
	member(subv(SV, MP, MOL, _), PTL),
	true.
get_fstab_list_multi_btrfs(_PTL, '/mnt/btr_pool', [subvolid=5, noatime]).

