% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man ext4
% man mkfs.ext4
% extfs_feat(name, fs_ver, kernel)
prop_info_extfs(extfs_feat, FS, L) :- !,
	prop_extfs_info(FS, N), !,
	prop_info(extfs_feat, _, IL),
	findall(prop_feat(F, OF, attr(N1, KV)), (member(prop_feat(F, OF, attr(N1, KV)), IL), prop_extfs_filter(N, N1)), L),
	true.
prop_info_extfs(TAG, _FS, L) :-
	prop_info(TAG, _, L).

prop_extfs_info(ext2, 2).
prop_extfs_info(ext3, 3).
prop_extfs_info(ext4, 4).

% There is a bug with findall.
prop_extfs_filter(N1, N2) :-
	N1 >= N2.

prop_info(extfs_feat, feat3s('-O', ','), [
	  prop_feat('64bit', on, attr(4, kv(2, 6, 28)))
	, prop_feat(bigalloc, off, attr(4, kv(3, 2, 0)))
	, prop_feat(casefold, off, attr(4, kv(5, 2, 0)))
	, prop_feat(dir_index, on, attr(2, kv(2, 6, 0)))
	, prop_feat(dir_nlink, on, attr(4, kv(2, 6, 28)))
	, prop_feat(ea_inode, off, attr(4, kv(4, 13, 0)))
	, prop_feat(encrypt, off, attr(4, kv(4, 1, 0)))
	, prop_feat(ext_attr, on, attr(2, kv(2, 6, 0)))
	, prop_feat(extent, on, attr(4, kv(2, 6, 28)))
	, prop_feat(extra_isize, on, attr(4, kv(2, 6, 28)))
	, prop_feat(filetype, on, attr(2, kv(2, 2, 0)))
	, prop_feat(flex_bg, on, attr(4, kv(2, 6, 28)))
	, prop_feat(has_journal, on, attr(3, kv(2, 4, 15)))
	, prop_feat(huge_file, on, attr(4, kv(2, 6, 28)))
	, prop_feat(inline_data, off, attr(4, kv(3, 8, 0)))
	% , prop_feat(journal_dev, off, attr(3, kv(2, 6, 10))) % !!! no info in man !!!
	, prop_feat(large_dir, off, attr(4, kv(4, 13, 0)))
	, prop_feat(large_file, on, attr(2, kv(2, 2, 0)))
	, prop_feat(metadata_csum, on, attr(4, kv(3, 18, 0)))
	, prop_feat(metadata_csum_seed, on, attr(4, kv(4, 4, 0)))
	, prop_feat(meta_bg, off, attr(4, kv(2, 6, 28)))
	, prop_feat(mmp, off, attr(4, kv(3, 0, 0)))
	, prop_feat(project, off, attr(4, kv(4, 5, 0)))
	, prop_feat(quota, off, attr(4, kv(3, 6, 0)))
	% , prop_feat(orphan_file, on, attr(4, '???'))
	, prop_feat(resize_inode, on, attr(2, kv(2, 6, 10)))
	, prop_feat(sparse_super, on, attr(2, kv(2, 2, 0)))
	, prop_feat(sparse_super2, off, attr(4, kv(3, 16, 0)))
	, prop_feat(stable_inodes, off, attr(4, kv(5, 5, 0)))
	, prop_feat(uninit_bg, off, attr(4, kv(2, 6, 28)))
	, prop_feat(verity, off, attr(4, kv(5, 4, 0)))
	]).

prop_info(extfs_rw, opt4s(' '), [
	  opt4('block-size', int, 4096, '-b')
	, opt4('cluster-size', int, 2048, '-C')
	, opt4('root-directory', path, '', '-d')
	, opt4('error-behavior', enum([continue, 'remount-ro', panic]), continue, '-e')
	, opt4('force', enable, no, '-F')
	, opt4('blocks-per-group', int, 256, '-g')
	, opt4('number-of-groups', int, 2048, '-G')
	, opt4('bytes-per-inode', int, 16384, '-i')
	, opt4('inode-size', int, 256, '-I')
	, opt4('ext3 journal', enable, no, '-j')
	, opt4('bad blocks list', file, '', '-l')
	, opt4('label', str, '', '-L')
	, opt4('reserved-blocks-percentage', percentage, 5, '-m')
	, opt4('last-mounted-directory', path, '', '-M')
	, opt4('number-of-inodes', int, 2048, '-N')
	, opt4('creator-os', str, '', '-o')
	, opt4('revision', int, 1, '-r')
	, opt4('write descriptors only', enable, no, '-S')
	, opt4('fs-type', enum([ext2, ext3, ext4]), ext2, '-t')
	, opt4('usage-type', enum([default, floppy, small, big, huge]), default, '-T')
	% , opt4('UUID', combo_val(extfs_uuid, [opt3(default, none, random), opt3(clear, none, clear), opt3(time, none, time), opt3(uuid, guid, '')]), random, '-U')
	, opt4('undo_file', file, '', '-z')
	]).

prop_info(extfs_cs_ext_opts, opt3s('-E', ','), [
	  opt3(encoding, str, '')
	% , opt3(encoding_flags, int, 2048)
	% , opt3(mmp_update_interval, int, 2048)
	, opt3(stride, int, 2048)
	, opt3(stripe_width, int, 2048)
	, opt3(offset, int, 2048)
	, opt3(resize, int, 2048)
	, opt3(lazy_itable_init, enum([0, 1]), 1)
	, opt3(assume_storage_prezeroed, enum([0, 1]), 0)
	, opt3(no_copy_xattrs, enable, no)
	, opt3(num_backup_sb, enum([0, 1, 2]), 0)
	, opt3(packed_meta_blocks, enum([0, 1]), 0)
	% , opt3(root_owner, int, 2048)
	, opt3(test_fs, enable, no)
	, opt3(orphan_file_size, int, 0)
	, opt3(discard, enable, no)
	, opt3(nodiscard, enable, no)
	% , opt3(quotatype, int, 2048)
	]).

prop_info(extfs_cs_jrn_opts, opt3s('-J', ','), [
	  opt3(size, size, 0)
	, opt3(fast_commit_size, size, 0)
	% , opt3(location, size, 0)
	% , opt3(device, size, 0)
	]).

