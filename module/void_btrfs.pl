% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/Btrfs

mkfs_multi_btrfs(PTL, RD) :-
	forall(member(subv(S, _, _, _), PTL), (format_to_atom(SA, '~w/~w', [RD, S]), os_shell2l([btrfs, subvolume, create, SA]))),
	forall(member(subv(S, _, _, nodatacow), PTL), (format_to_atom(SA, '~w/~w', [RD, S]), os_shell2l([chattr, '-R', '+C', SA]))),
	true.

mount_btrfs_muli(BD, PTL, RD) :-
	forall(member(subv(SV, MP, OL, _), PTL), mount_btrfs_muli_1(BD, SV, MP, OL, RD)),
	os_mkdir_p(RD + '/mnt/btr_pool'),
	true.

mount_btrfs_muli_1(BD, SV, MP, OL, RD) :-
	atom_concat(RD, MP, DA),
	os_mkdir_p(DA),
	O = [subvol=concat('/', SV)| OL],
	os_call2([mount, o(o, lc(O)), BD, DA]),
	true.

