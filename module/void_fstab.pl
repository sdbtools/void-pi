% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% generate fstab
make_fstab(TL, RD) :-
	atom_concat(RD, '/etc/fstab', FN),
	open(FN, write, S),
	get_fstab_list(TL, MPL),
	maplist(make_fstab_(S), MPL),
	close(S),
	os_call2([chmod, '644', FN]),
	!.
make_fstab(_TL, _) :-
	tui_msgbox('Making of fstab has failed.'),
	fail.

make_fstab_(S, fstab(MP, FS, PD)) :-
	write_fstab(FS, PD, MP, S),
	true.

write_fstab(zfs, _D, _MP, _S) :- !,
	% Do nothing
	true.
write_fstab(btrfs, D, _MP, S) :- !,
	lx_get_dev_uuid(D, U),
	write_fstab_btrfs(D, U, S),
	% write_fstab_btrfs_snap(U, S),
	true.
write_fstab(swap, D, _MP, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	% O = [sw],
	O = [defaults],
	L = ['UUID'=U, none, swap, lc(O), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(proc, _, _, S) :- !,
	write(S, '# /proc with hidepid (https://wiki.archlinux.org/index.php/Security#hidepid)'), nl(S),
	O = [nodev, noexec, nosuid, hidepid=2, gid=proc],
	L = [proc, '/proc', proc, lc(O), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(tmp, _, _, S) :- !,
	O = [defaults, nosuid, nodev],
	L = [tmpfs, '/tmp', tmpfs, lc(O), '0 0'],
	write_fstab_line(L, S),
	true.
write_fstab(FS, D, MP, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	( inst_setting(fs_attr(FS, MP), mount(OL))
	; OL = [rw, noatime]
	),
	fapassno(FS, MP, FSPASSNO),
	L = ['UUID'=U, MP, FS, lc(OL), '0', FSPASSNO],
	write_fstab_line(L, S),
	true.

write_fstab_btrfs(D, U, S) :-
	inst_setting(btrfs, subv(SV, mp(MP), OL, _)),
	format(S, '# ~w\n', [D]),
	O = [subvol=concat('/', SV)| OL],
	L = ['UUID'=U, MP, btrfs, lc(O), '0 0'],
	write_fstab_line(L, S),
	fail.
write_fstab_btrfs(_, _, _).

write_fstab_line(L, S) :-
	os_wcmdl(L, '\t', S), nl(S), nl(S).

fapassno(FS, _MP, '0') :-
	member(FS, [f2fs, xfs, btrfs]).
fapassno(_FS, '/', '1').
fapassno(_FS, _MP, '2').

% Similar to get_mp_list but including swap.
get_fstab_list(TL, [fstab(none, proc, none), fstab(none, tmp, none)| MPL1]) :-
	% fs4(FileSystem, Label, MountPoint, [device_list])
	findall(fstab(MP, FS, PD), member(fs4(FS, _Label, MP, [PD| _]), TL), MPL0),
	sort(MPL0, MPL1),
	true.

