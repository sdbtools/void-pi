% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

usage :-
	cmd_arg_usage_short(SAL),
	surround_atoms('[', ']', SAL, SAL1),
	join_atoms(['Usage: void-pi'| SAL1], ' ', SAL2),
	writenl(SAL2), nl,
	writenl('Void Linux installer implemented in GNU Prolog.'), nl,
	cmd_arg_usage_long(S),
	writenl(S), nl,
	writenl('Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license'),
	true.

% cmd_arg_info(alias, short, long, value_descr, descr)
% short argument is a character code.
cmd_arg_info(help, 0'h, help, '', 'Show this help and exit.').
cmd_arg_info(version, 0'v, version, '', 'Show the version and exit.').
cmd_arg_info(rootdir, 0'r, rootdir, rootdir, 'Use an alternative rootdir. Acts like xbps\'s -r flag. Default is /mnt.').
cmd_arg_info(config, 0, config, 'file_name', 'Set configuration file. Default is settings.pl.').

% first argument is an alias.
on_cmd_arg(help, _, _) :- !,
	usage,
	fail.
on_cmd_arg(version, _, _) :- !,
	version,
	fail.
on_cmd_arg(rootdir, LI, T) :- !,
	( LI = [RD|T]
	; writenl('rootdir value expected'),
	  fail
	), !,
	retractall(inst_setting(root_dir, _)),
	assertz(inst_setting(root_dir, RD)),
	true.
on_cmd_arg(config, LI, T) :- !,
	( LI = [CF|T]
	; writenl('file name expected'),
	  fail
	), !,
	retractall(inst_setting(config_file, _)),
	assertz(inst_setting(config_file, CF)),
	true.

