% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man mkfs.vfat
prop_info(vfat_rw, opt4s(' '), [
	  opt4(align, enable, no, '-a')
	, opt4(atary, enable, no, '-A')
	, opt4('sector-of-backup', int, 6, '-b')
	, opt4('check for bad blocks', enable, no, '-c')
	, opt4('drive number', hex, '0x80', '-D')
	, opt4('number-of-fats', int, 2, '-f')
	, opt4('fat-size', enum([12, 16, 32]), 12, '-F')
	% , opt4('heads/sectors-per-track', int, 0, '-g')
	, opt4('number-of-hidden-sectors', int, 0, '-h')
	, opt4('volume-id', hex, '', '-i')
	, opt4('ignore and disable safety checks', enable, no, '-I')
	, opt4('bad blocks list file', file, '', '-l')
	, opt4('message-file', file, '', '-m')
	, opt4('fat-media-type', hex, '0xF8', '-M')
	% , opt4(mbr, enum([yes, no, auto]), auto, '--mbr')
	, opt4(label, str_upper, '', '-n')
	% , opt4(codepage, str, '850', '--codepage')
	, opt4('root-dir-entries', int, 512, '-r')
	, opt4('number-of-reserved-sectors', int, 32, '-R')
	, opt4('sectors-per-cluster', enum([1, 2, 4, 8, 16, 32, 64, 128]), 2, '-s')
	, opt4('logical-sector-size', enum([512, 1024, 2048, 4096, 8192, 16384, 32768]), 512, '-S')
	, opt4(offset, int, 0, '--offset')
	, opt4(variant, enum([standard, atari]), standard, '--variant')
	, opt4(invariant, enable, no, '--invariant')
	]).

prop_info(mnt_vfat_feat, feat4s('--options', ',', on_off), [
	  prop_feat4(debug, off, std, none)
	, prop_feat4(discard, off, std, none)
	, prop_feat4(dos1xfloppy, off, std, none)
	, prop_feat4(quiet, off, std, none)
	, prop_feat4(rodir, off, std, none)
	, prop_feat4(showexec, off, std, none)
	, prop_feat4(sys_immutable, off, std, none)
	, prop_feat4(flush, off, std, none)
	, prop_feat4(usefree, off, std, none)

	, prop_feat4(uni_xlate, off, std, none)
	, prop_feat4(posix, off, std, none)
	, prop_feat4(nonumtail, off, std, none)
	, prop_feat4(utf8, off, std, none)
	]).

prop_info(mnt_vfat_opt, opt3s('--options', ','), [
	  opt3(uid, int, 0)
	, opt3(gid, int, 0)
	, opt3(umask, octal, 0)
	, opt3(dmask, octal, '0022')
	, opt3(fmask, octal, '0022')
	, opt3(allow_utime, int, 0)
	, opt3(check, enum([relaxed, normal, strict]), normal)
	, opt3(codepage, int, 437)
	, opt3(errors, enum([panic, continue, 'remount-ro']), 'remount-ro')
	, opt3(fat, enum([12, 16, 32]), 12)
	, opt3(iocharset, str, 'iso8859-1')
	, opt3(nfs, enum([stale_rw, nostale_ro]), stale_rw)
	, opt3(tz, str, '')
	, opt3(time_offset, int, 0)
	% , opt3(dotsOK, enable, yes)

	, opt3(shortname, enum([lower, win95, winnt, mixed]), mixed)
	]).

