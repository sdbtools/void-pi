% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man xfs
% man mkfs.xfs
% man xfs_admin
% man xfs_growfs

prop_info(xfs_block_size_rw, opt3s('-b', ','), [
	  opt3(size, int, 4096)
	]).

prop_info(xfs_global_metadata_rw, opt3s('-m', ','), [
	  opt3(bigtime, enum([0, 1]), 1)
	, opt3(crc, enum([0, 1]), 1)
	, opt3(finobt, enum([0, 1]), 1)
	, opt3(inobtcount, enum([0, 1]), 1)
	, opt3(uuid, guid, '')
	, opt3(rmapbt, enum([0, 1]), 1)
	, opt3(reflink, enum([0, 1]), 1)
	]).

prop_info(xfs_data_section_rw, opt3s('-d', ','), [
	  opt3(agcount, int, 1)
	, opt3(agsize, size, '16m')
	, opt3(cowextsize, int, 0)
	, opt3(name, str, '')
	, opt3(file, enum([0, 1]), 1)
	, opt3(size, size, 0)
	, opt3(sunit, int, 1)
	, opt3(su, size, 0)
	, opt3(swidth, int, 0)
	, opt3(sw, size, 0)
	, opt3(noalign, enable, no)
	, opt3(rtinherit, enum([0, 1]), 0)
	, opt3(projinherit, int, 0)
	, opt3(extszinherit, size, 0)
	, opt3(daxinherit, enum([0, 1]), 0)
	]).

prop_info(xfs_inode_rw, opt3s('-i', ','), [
	  opt3(size, size, 512)
	, opt3(perblock, int, 0)
	, opt3(maxpct, percentage, 5)
	, opt3(align, enum([0, 1]), 1)
	, opt3(attr, int, 2)
	, opt3(projid32bit, enum([0, 1]), 1)
	, opt3(sparse, enum([0, 1]), 1)
	, opt3(nrext64, int, 1)
	]).

prop_info(xfs_log_rw, opt3s('-l', ','), [
	  opt3(agnum, int, 0)
	, opt3(internal, enum([0, 1]), 1)
	, opt3(logdev, str, '')
	, opt3(size, size, 0)
	, opt3(version, int, 2)
	, opt3(sunit, int, 1)
	, opt3(su, size, 0)
	, opt3('lazy-count', enum([0, 1]), 1)
	]).

prop_info(xfs_naming_rw, opt3s('-n', ','), [
	  opt3(size, size, 4096)
	, opt3(version, enum([2, ci]), 2)
	, opt3(ftype, enum([0, 1]), 1)
	]).

prop_info(xfs_protofile_rw, opt3s('-p', ','), [
	  opt3(file, file, '')
	, opt3(slashes_are_spaces, enum([0, 1]), 0)
	]).

prop_info(xfs_realtime_rw, opt3s('-r', ','), [
	  opt3(rtdev, str, '')
	, opt3(extsize, size, '64k')
	, opt3(size, size, 0)
	, opt3(noalign, enable, no)
	]).

prop_info(xfs_sector_size_rw, opt3s('-s', ','), [
	  opt3(size, size, 512)
	]).

prop_info(xfs_rw, opt4s(' '), [
	  opt4(label, str, '', '-L')
	, opt4('no discard', enable, no, '-K')
	, opt4(quite, enable, no, '-q')
	, opt4(force, enable, no, '-f')
	]).

