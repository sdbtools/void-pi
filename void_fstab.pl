% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% generate fstab
make_fstab(RD) :-
	atom_concat(RD, '/etc/fstab', FN),
	open(FN, write, S),
	get_fstab_list(MPL),
	maplist(make_fstab_(S), MPL),
	close(S),
	os_call2([chmod, '644', FN]),
	!.
make_fstab(_) :-
	tui_msgbox('Making of fstab has failed.', []),
	fail.

make_fstab_(S, fstab(MP, FS, PD)) :-
	write_fstab(FS, PD, MP, S),
	true.

write_fstab(zfs, _D, _MP, _S) :- !,
	% Do nothing
	true.
write_fstab(lvm, _D, MP, S) :- !,
	inst_setting(lvm, lv(VG, LV, _SZ)),
	inst_setting(root_fs, FS),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	write_fstab(FS, LVM_PD, MP, S),
	true.
write_fstab(luks1, _D, MP, S) :- !,
	inst_setting(root_fs, FS),
	luks_dev_name(LUKS_PD),
	write_fstab(FS, LUKS_PD, MP, S),
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
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	L = [UU, none, swap, OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(proc, _, _, S) :- !,
	write(S, '# /proc with hidepid (https://wiki.archlinux.org/index.php/Security#hidepid)'), nl(S),
	O = [nodev, noexec, nosuid, 'hidepid=2', 'gid=proc'],
	join_atoms(O, ',', OA),
	L = [proc, '/proc', proc, OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(tmp, _, _, S) :- !,
	O = [defaults, nosuid, nodev],
	join_atoms(O, ',', OA),
	L = [tmpfs, '/tmp', tmpfs, OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(FS, D, MP, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	( inst_setting(fs(FS, MP), mount(O))
	; O = [rw, noatime]
	),
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	fapassno(FS, MP, FSPASSNO),
	L = [UU, MP, FS, OA, '0', FSPASSNO],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.

fapassno(FS, _MP, '0') :-
	member(FS, [f2fs, xfs, btrfs]).
fapassno(_FS, '/', '1').
fapassno(_FS, _MP, '2').

write_fstab_btrfs(D, U, S) :-
	inst_setting(btrfs, subv(SV, mp(MP), OL, _)),
	format(S, '# ~w\n', [D]),
	% format(S, '#UUID=~w\t~w\tbtrfs\n', [U, MP]),
	atom_concat('subvol=/', SV, SO),
	O = [SO|OL],
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	L = [UU, MP, 'btrfs', OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	fail.
write_fstab_btrfs(_, _, _).

% write_fstab_btrfs_snap(U, S) :-
% 	inst_setting(fs(btrfs, '/'), snap_dir(MP)), !,
% 	format(S, '# ~w\n', [MP]),
% 	O = ['subvolid=5', noatime],
% 	join_atoms(O, ',', OA),
% 	atom_concat('UUID=', U, UU),
% 	L = [UU, MP, 'btrfs', OA, '0 0'],
% 	join_atoms(L, '\t', LA),
% 	write(S, LA), nl(S), nl(S),
% 	!.
% write_fstab_btrfs_snap(_, _).

% Similar to get_mp_list but including swap.
get_fstab_list(MPL1) :-
	% Ignore BIOS boot partition
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	findall(fstab(MP, FS, PD), (inst_setting(partition, part(_D, _P, PD, PT, FS, _Label, MP, _CK, _SZ)), PT \= bios_boot), MPL0),
	sort(MPL0, MPL1),
	true.


