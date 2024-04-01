% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man 5 proc

prop_info(mnt_proc_opt, opt3s('--options', ','), [
	  opt3(hidepid, enum(0, 1, 2), 0)
	, opt3(gid, str, 0) % "str" is a correct value here.
	]).

