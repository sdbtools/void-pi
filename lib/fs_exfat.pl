% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man mkfs.exfat

prop_info(exfat_rw, opt4s(' '), [
	  opt4('boundary-align', size, '16k', '-b')
	, opt4('cluster-size', size, '4k', '-c')
	, opt4('full-format', enable, no, '-f')
	, opt4(label, str, '', '-L')
	, opt4(guid, guid, '', '-U')
	, opt4('pack-bitmap', enable, no, '--pack-bitmap')
	, opt4(quite, enable, no, '-q')
	]).

prop_info(mnt_exfat_opt, opt3s('--options', ','), [
	  opt3(umask, octal, 0)
	, opt3(dmask, octal, 0)
	, opt3(fmask, octal, 0)
	, opt3(uid, int, 0)
	, opt3(gid, int, 0)
	]).

