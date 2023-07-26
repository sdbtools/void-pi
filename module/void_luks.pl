% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

uses_luks(TL) :-
	memberchk(bdev(luks, _), TL),
	!.

create_keyfile(ROOT_PD, RD) :-
	VK = '/boot/volume.key',
	atom_concat(RD, VK, KF),
	os_shell2([dd, v(bs, 512), v(count, 4), v(if, '/dev/urandom'), v(of, KF)]),
	% os_shell2([dd, v(bs, 1), v(count, 64), v(if, '/dev/urandom'), v(of, KF)]),

	inst_setting_tmp(passwd('$_luks_$'), RPWD),
	lx_get_dev_disk_uuid(ROOT_PD, DISK_PUUID),
	tui_infobox('Adding crypto-key.', [sz([4, 40])]),
	( lx_luks_add_keyfile(DISK_PUUID, KF, RPWD)
	; tui_msgbox('luks_add_keyfile has failed'),
	  fail
	), !,

	% os_shell2([chmod, '000', KF]),
	os_shell2([chroot, RD, chmod, '000', VK]),
	os_shell2([chroot, RD, chmod, '-R', 'g-rwx,o-rwx', '/boot']),
	true.

setup_cryptab(PDL, RD) :-
	atom_concat(RD, '/etc/crypttab', KF),
	open(KF, write, S),
	maplist(setup_cryptab_(S), PDL),
	close(S),
	true.

setup_cryptab_(S, PD) :-
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	write(S, LUKS_PD),
	write(S, ' UUID='),
	write(S, PUUID),
	write(S, ' /boot/volume.key luks'),
	nl(S),
	true.

% SDN - short device name.
luks_dev_name(SDN, LUKS_PD) :-
	inst_setting(luks, luks(Name)),
	% lx_luks_dev_name(Name, LUKS_PD),
	format_to_atom(LUKS_PD, '/dev/mapper/~w_~w', [Name, SDN]),
	true.

luks_dev_name_short(SDN, LUKS_PD) :-
	inst_setting(luks, luks(Name)),
	format_to_atom(LUKS_PD, '~w_~w', [Name, SDN]),
	true.

setup_crypt(TL, RD) :-
	findall(PD, member(bdev(luks, luks(luks1, PD)), TL), PDL),
	PDL = [PD| _],
	% Create the keyfile
	create_keyfile(PD, RD),
	% Setup crypttab
	setup_cryptab(PDL, RD),
	true.

