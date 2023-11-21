% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/Btrfs

btrfs_mkfs_multi(PTL, RD) :-
	forall(member(subv(S, _, _, _), PTL), (format_to_atom(SA, '~w/~w', [RD, S]), os_shell2l([btrfs, subvolume, create, SA]))),
	forall(member(subv(S, _, _, nodatacow), PTL), (format_to_atom(SA, '~w/~w', [RD, S]), os_shell2l([chattr, '-R', '+C', SA]))),
	true.

btrfs_mount_muli(BD, PTL, RD) :-
	forall(member(subv(SV, MP, OL, _), PTL), btrfs_mount_muli_1(BD, SV, MP, OL, RD)),
	os_mkdir_p(RD + '/mnt/btr_pool'),
	true.

btrfs_mount_muli_1(BD, SV, MP, OL, RD) :-
	atom_concat(RD, MP, DA),
	os_mkdir_p(DA),
	O = [subvol=concat('/', SV)| OL],
	os_call2([mount, o(o, lc(O)), BD, DA]),
	true.

btrfs_mkfs(Title, DL, PTL, Label, RD) :-
	tui_progressbox_safe(['mkfs.btrfs', o('L', dq(Label)), '-f', DL, '2>&1'], '', [title(Title), sz([12, 80])]),
	( inst_setting(fs_attr(btrfs, '/', _), mount(AL))
	; AL = [rw, noatime]
	), !,
	DL = [D| _],
	os_call2([mount, o(o, lc(AL)), D, RD]),
	% create_btrfs_subv(RD),
	btrfs_mkfs_multi(PTL, RD),
	os_call2([umount, RD]),
	true.

