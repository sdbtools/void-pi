% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% Automatic LUKS unlock using keyfile on boot partition: https://unix.stackexchange.com/questions/666770/automatic-luks-unlock-using-keyfile-on-boot-partition

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
	setup_cryptab_(S, PDL, RD),
	close(S),
	true.

setup_cryptab_none(PD, RD) :-
	atom_concat(RD, '/etc/crypttab', KF),
	open(KF, write, S),
	setup_cryptab_kf_none(S, PD),
	close(S),
	true.

setup_cryptab_(S, [PD], _RD) :-
	setup_cryptab_kf(S, PD),
	true.
setup_cryptab_(S, [PD|T], RD) :-
	% Download decrypt_keyctl
	os_mkdir_p(RD + '/lib/cryptsetup/scripts'),
	% tui_progressbox_safe is causes weird problems.
	% tui_progressbox_safe([curl, 'https://raw.githubusercontent.com/gebi/keyctl_keyscript/master/decrypt_keyctl', o(o, RD + '/lib/cryptsetup/scripts/decrypt_keyctl')], '', [title(' Downloading decrypt_keyctl '), sz([6, 80])]),
	tui_progressbox_unsafe([curl, 'https://raw.githubusercontent.com/gebi/keyctl_keyscript/master/decrypt_keyctl', o(o, RD + '/lib/cryptsetup/scripts/decrypt_keyctl')], '', [title(' Downloading decrypt_keyctl '), sz([6, 80])]),
	os_shell2([chroot, RD, chmod, '711', '/lib/cryptsetup/scripts/decrypt_keyctl']),

	setup_cryptab_ks(S, 'key_1:', PD),
	maplist(setup_cryptab_ks(S, 'key_1'), T),
	true.

setup_cryptab_kf(S, PD) :-
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	format(S, '~w UUID=~w /boot/volume.key luks', [LUKS_PD, PUUID]),
	nl(S),
	true.

setup_cryptab_kf_none(S, PD) :-
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	format(S, '~w UUID=~w none luks', [LUKS_PD, PUUID]),
	nl(S),
	true.

setup_cryptab_ks(S, K, PD) :-
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	format(S, '~w UUID=~w ~w luks,keyscript=/lib/cryptsetup/scripts/decrypt_keyctl', [LUKS_PD, PUUID, K]),
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
	findall(PD, member(bdev(luks, luks(_, PD)), TL), PDL),
	PDL = [PD| _],
	% Create the keyfile
	create_keyfile(PD, RD),
	% Setup crypttab
	setup_cryptab(PDL, RD),
	true.

setup_crypt_none(TL, RD) :-
	findall(PD, member(bdev(luks, luks(_, PD)), TL), PDL),
	PDL = [PD| _],
	% Setup crypttab
	setup_cryptab_none(PD, RD),
	true.

