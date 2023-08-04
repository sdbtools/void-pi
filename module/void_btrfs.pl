% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/Btrfs

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
	( inst_setting(fs_attr(btrfs, '/'), mount(AL))
	; AL = [rw, noatime]
	), !,
	os_call2([mount, o(o, lc(AL)), D, RD]),
	create_btrfs_subv(RD),
	os_call2([umount, RD]),
	true.

mount_btrfs(BD, RD) :-
	inst_setting(btrfs, subv(SV, mp(MP), OL, _)),
	atom_concat(RD, MP, DA),
	os_mkdir_p(DA),
	O = [subvol=concat('/', SV)| OL],
	os_call2([mount, o(o, lc(O)), BD, DA]),
	fail.
mount_btrfs(_, _).

