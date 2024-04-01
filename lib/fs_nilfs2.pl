% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man mount.nilfs2
% man mkfs.nilfs2

prop_info(nilfs2_feat, feat4s('-O', ',', pref1('^')), [
	  prop_feat4(block_count, off, std, none)
	]).

prop_info(nilfs2_rw, opt4s(' '), [
	  opt4('block-size', enum([1024, 2048, 4096, 8192]), 4096, '-b')
	, opt4('blocks-per-segment', int, 2048, '-B')
	, opt4(force, enable, no, '-f')
	, opt4('check-for-bad-blocks', enable, no, '-c')
	, opt4(discard, enable, yes, '-K')
	, opt4(label, str, '', '-L')
	, opt4(quite, enable, no, '-q')
	, opt4('gc-reserved-segments', percentage, 5, '-m')
	]).

prop_info(mnt_nilfs2_feat, feat4s('--options', ',', on_off), [
	  prop_feat4(barrier, off, pref1(no), none)
	, prop_feat4(discard, off, pref1(no), none)
	, prop_feat4(nogc, off, std, none)
	, prop_feat4(norecovery, off, std, none)
	]).

prop_info(mnt_nilfs2_opt, opt3s('--options', ','), [
	  opt3(cp, int, 0)
	, opt3(errors, enum([continue, panic, 'remount-ro']), 'remount-ro')
	, opt3(pp, int, 0)
	, opt3(order, enum([relaxed, strict]), relaxed)
	]).

