% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

is_luks(D) :-
	os_call2([cryptsetup, isLuks, D]),
	true.

lx_luks_dev_name(N, LUKS_PD) :-
	atom_concat('/dev/mapper/', N, LUKS_PD),
	true.

lx_luks_format(Type, PD, PWD) :-
	CL = [cryptsetup, '--type', Type, '--key-file=-', '--force-password', luksFormat, PD, '2>&1', '1>/dev/null'],
	os_scmdl(CL, CA),
	popen(CA, write, WS), !,
	write(WS, PWD), % no nl(WS) should be here.
	close(WS),
	true.

lx_luks_open(Type, Name, PD, PWD) :-
	CL = [cryptsetup, '--key-file=-', open, '--type', Type, PD, Name],
	os_scmdl(CL, CA),
	exec(CA, SI, SO, SE, Pid),
	write(SI, PWD), % no nl(SI) should be here.
	close(SI),
	close(SO),
	close(SE),
	wait(Pid, _RC),
	true.

lx_luks_close(Name) :-
	os_call2([cryptsetup, close, Name]),
	true.

lx_luks_add_keyfile(DISK_PUUID, KF, PWD) :-
	% CL = [cryptsetup, '--key-file=-', '--force-password', luksAddKey, DISK_PUUID, KF],
	CL = [cryptsetup, '-q', '--force-password', luksAddKey, DISK_PUUID, KF],
	os_scmdl(CL, CA),
	popen(CA, write, WS), !,
	write(WS, PWD), % no nl(WS) should be here.
	close(WS),
	true.

