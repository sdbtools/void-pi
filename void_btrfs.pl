% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

create_btrfs_subv(RD) :-
	inst_setting(btrfs, subv(S, _, _, _)),
	format_to_atom(SA, '~w/~w', [RD, S]),
	os_shell2l([btrfs, subvolume, create, SA]),
	fail.
create_btrfs_subv(RD) :-
	% set nodatacow attr.
	inst_setting(btrfs, subv(S, _, _, nodatacow)),
	format_to_atom(SA, '~w/~w', [RD, S]),
	os_shell2l([chattr, '-R', '+C', SA]),
	fail.
create_btrfs_subv(_RD).

create_btrfs_subv(D, RD) :-
	( inst_setting(fs(btrfs, '/'), mount(AL)) ->
	  true
	; AL = [rw, noatime]
	),
	join_atoms(AL, ',', AA),
	os_call2([mount, '-o', AA, D, RD]),
	create_btrfs_subv(RD),
	os_call2([umount, RD]),
	true.

mount_btrfs(BD, RD) :-
	inst_setting(btrfs, subv(SV, mp(MP), OL, _)),
	atom_concat(RD, MP, DA),
	atom_concat('subvol=/', SV, SO),
	O = [SO|OL],
	join_atoms(O, ',', OA),
	os_mkdir_p(DA),
	os_call2([mount, '-o', OA, BD, DA]),
	fail.
mount_btrfs(_, _).

