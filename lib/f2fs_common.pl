% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man mkfs.f2fs
% f2fs_feat(name, depends_list)
prop_info(f2fs_feat, feat3s('-O', ','), [
	  prop_feat(encrypt, off, attr([], ''))
	, prop_feat(extra_attr, off, attr([], ''))
	, prop_feat(project_quota, off, attr([extra_attr], ''))
	, prop_feat(inode_checksum, off, attr([extra_attr], ''))
	, prop_feat(flexible_inline_xattr, off, attr([extra_attr], ''))
	, prop_feat(quota, off, attr([], ''))
	, prop_feat(inode_crtime, off, attr([extra_attr], ''))
	, prop_feat(lost_found, off, attr([], ''))
	, prop_feat(verity, off, attr([], ''))
	, prop_feat(sb_checksum, off, attr([], ''))
	, prop_feat(casefold, off, attr([], ''))
	, prop_feat(compression, off, attr([extra_attr], ''))
	]).

prop_info(f2fs_rw, opt4s(' '), [
	  opt4('heap-based-allocation', enum([0, 1]), 1, '-a')
	% , opt4('device-list', int, 0, '-c')
	, opt4('debug-level', int, 0, '-d')
	% , opt4('extension-list', int, 0, '-e')
	% , opt4('extension-list', int, 0, '-E')
	, opt4('force', enable, no, '-f')
	% , opt4('default-options', int, 0, '-g')
	, opt4('extended node bitmap', enable, no, '-i')
	, opt4('label', str, '', '-l')
	, opt4('block zoned', enable, no, '-m')
	, opt4('overprovision-ratio', percentage, 0, '-o')
	% , opt4('casefolding encoding:flags', int, 0, '-C')
	, opt4('disable checkpointing srand seed', enable, no, '-r')
	% , opt4('uid:gid"', int, 0, '-R')
	, opt4('segments per section', int, 1, '-s')
	, opt4('sparse mode', enable, no, '-S')
	, opt4('discard policy', enum([0, 1]), 1, '-t')
	, opt4('timestamp', int, -1, '-T')
	, opt4('wanted-sector-size', size, 0, '-w')
	, opt4('sections-per-zone', int, 0, '-z')
	]).

