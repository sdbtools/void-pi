% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

uses_luks :-
	inst_setting(template, gpt_luks1), !.
uses_luks :-
	inst_setting(template, gpt_luks1_lvm), !.

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

setup_cryptab(PUUID, RD) :-
	atom_concat(RD, '/etc/crypttab', KF),
	inst_setting(luks, luks(Name)),
	open(KF, write, S),
	write(S, Name),
	write(S, ' UUID='),
	write(S, PUUID),
	write(S, ' /boot/volume.key luks'),
	nl(S),
	close(S),
	true.

luks_dev_name(LUKS_PD) :-
	inst_setting(luks, luks(Name)),
	lx_luks_dev_name(Name, LUKS_PD),
	true.

setup_crypt(RD) :-
	inst_setting(bootloader_dev, dev(BD, _, _)),
	root_pd(BD, ROOT_PD),
	lx_get_dev_uuid(ROOT_PD, PUUID),
	% Create the keyfile
	create_keyfile(ROOT_PD, RD),
	% Setup crypttab
	setup_cryptab(PUUID, RD),
	true.

