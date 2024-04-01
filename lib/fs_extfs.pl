% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man ext4
% man mkfs.ext4
% extfs_feat(name, fs_ver, kernel)
prop_info_extfs(extfs_feat, FS, L) :-
	prop_extfs_info(FS, N), !,
	prop_info(extfs_feat, _, IL),
	findall(prop_feat4(F, OF, Fmt, attr(N1, KV)), (member(prop_feat4(F, OF, Fmt, attr(N1, KV)), IL), prop_extfs_filter(N, N1)), L),
	true.
prop_info_extfs(mnt_extfs_feat, FS, L) :-
	prop_extfs_info(FS, N), !,
	prop_info(mnt_extfs_feat, _, IL),
	findall(prop_feat4(F, OF, Fmt, attr(N1)), (member(prop_feat4(F, OF, Fmt, attr(N1)), IL), prop_extfs_filter(N, N1)), L),
	true.
prop_info_extfs(mnt_extfs_opt, FS, L) :-
	prop_extfs_info(FS, N), !,
	prop_info(mnt_extfs_opt, _, IL),
	findall(opt3(F, Fmt, DV), (member(opt3a(F, Fmt, DV, attr(N1)), IL), prop_extfs_filter(N, N1)), L),
	true.
prop_info_extfs(TAG, _FS, L) :-
	prop_info(TAG, _, L).

prop_extfs_info(ext2, 2).
prop_extfs_info(ext3, 3).
prop_extfs_info(ext4, 4).

% There is a bug with findall.
prop_extfs_filter(N1, N2) :-
	N1 >= N2.

prop_info(extfs_feat, feat4s('-O', ',', pref1('^')), [
	  prop_feat4('64bit', on, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(bigalloc, off, std, attr(4, kv(3, 2, 0)))
	, prop_feat4(casefold, off, std, attr(4, kv(5, 2, 0)))
	, prop_feat4(dir_index, on, std, attr(2, kv(2, 6, 0)))
	, prop_feat4(dir_nlink, on, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(ea_inode, off, std, attr(4, kv(4, 13, 0)))
	, prop_feat4(encrypt, off, std, attr(4, kv(4, 1, 0)))
	, prop_feat4(ext_attr, on, std, attr(2, kv(2, 6, 0)))
	, prop_feat4(extent, on, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(extra_isize, on, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(filetype, on, std, attr(2, kv(2, 2, 0)))
	, prop_feat4(flex_bg, on, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(has_journal, on, std, attr(3, kv(2, 4, 15)))
	, prop_feat4(huge_file, on, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(inline_data, off, std, attr(4, kv(3, 8, 0)))
	% , prop_feat4(journal_dev, off, std, attr(3, kv(2, 6, 10))) % !!! no info in man !!!
	, prop_feat4(large_dir, off, std, attr(4, kv(4, 13, 0)))
	, prop_feat4(large_file, on, std, attr(2, kv(2, 2, 0)))
	, prop_feat4(metadata_csum, on, std, attr(4, kv(3, 18, 0)))
	, prop_feat4(metadata_csum_seed, on, std, attr(4, kv(4, 4, 0)))
	, prop_feat4(meta_bg, off, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(mmp, off, std, attr(4, kv(3, 0, 0)))
	, prop_feat4(project, off, std, attr(4, kv(4, 5, 0)))
	, prop_feat4(quota, off, std, attr(4, kv(3, 6, 0)))
	% , prop_feat4(orphan_file, on, std, attr(4, '???'))
	, prop_feat4(resize_inode, on, std, attr(2, kv(2, 6, 10)))
	, prop_feat4(sparse_super, on, std, attr(2, kv(2, 2, 0)))
	, prop_feat4(sparse_super2, off, std, attr(4, kv(3, 16, 0)))
	, prop_feat4(stable_inodes, off, std, attr(4, kv(5, 5, 0)))
	, prop_feat4(uninit_bg, off, std, attr(4, kv(2, 6, 28)))
	, prop_feat4(verity, off, std, attr(4, kv(5, 4, 0)))
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

prop_info(mnt_extfs_feat, feat4s('--options', ',', pref1('^')), [
	  prop_feat4(acl, on, pref1(no), attr(2))
	, prop_feat4(abort, off, std, attr(4))
	, prop_feat4(auto_da_alloc, off, pref1(no), attr(4))
	, prop_feat4(block_validity, off, pref1(no), attr(4))
	, prop_feat4(bsddf, off, std, attr(2))
	, prop_feat4(debug, off, std, attr(2))
	, prop_feat4(delalloc, off, pref1(no), attr(4))
	, prop_feat4(dioread_lock, on, v2(dioread_lock, dioread_nolock), attr(4))
	, prop_feat4(discard, off, pref1(no), attr(4))
	, prop_feat4(grpid, off, pref1(no), attr(2))
	, prop_feat4(grpquota, off, on_off, attr(2))
	, prop_feat4(i_version, off, std, attr(4))
	, prop_feat4(journal_async_commit, off, std, attr(4))
	, prop_feat4(journal_checksum, off, pref1(no), attr(4))
	, prop_feat4(minixdf, off, std, attr(2))
	, prop_feat4(nocheck, off, std, attr(2))
	, prop_feat4(noinit_itable, off, std, attr(4))
	, prop_feat4(nombcache, off, std, attr(4))
	, prop_feat4(norecovery, off, std, attr(3))
	, prop_feat4(nouid32, off, std, attr(2))
	, prop_feat4(oldalloc, off, v2(oldalloc, orlov), attr(2))
	, prop_feat4(prjquota, off, std, attr(4))
	, prop_feat4(quota, off, pref1(no), attr(2))
	, prop_feat4(user_xattr, off, pref1(no), attr(2))
	, prop_feat4(user_xattr, off, std, attr(3))
	, prop_feat4(usrquota, off, on_off, attr(2))
	]).

prop_info(mnt_extfs_opt, opt3s('--options', ','), [
	  opt3a(errors, enum([continue, 'remount-ro', panic]), 'remount-ro', attr(2))
	, opt3a(resgid, int, 0, attr(2))
	, opt3a(resuid, int, 0, attr(2))
	, opt3a(sb, int, 0, attr(2))
	, opt3a(journal_dev, int, 0, attr(3))
	, opt3a(journal_path, str, '', attr(3))
	, opt3a(data, enum([journal, ordered, writeback]), ordered, attr(3))
	, opt3a(data_err, enum([none, ignore, abort]), none, attr(3))
	, opt3a(barrier, enum([0, 1]), 1, attr(3))
	, opt3a(commit, int, 0, attr(3))
	, opt3a(jqfmt, enum([vfsold, vfsv0, vfsv1]), vfsv1, attr(3))
	, opt3a(usrjquota, int, 0, attr(3))
	, opt3a(grpjquota, int, 0, attr(3))
	, opt3a(inode_readahead_blks, int, 0, attr(4))
	, opt3a(stripe, int, 0, attr(4))
	, opt3a(max_batch_time, int, 15000, attr(4))
	, opt3a(min_batch_time, int, 0, attr(4))
	, opt3a(journal_ioprio, enum([0, 1, 2, 3, 4, 5, 6, 7]), 3, attr(4))
	, opt3a(init_itable, int, 1, attr(4))
	, opt3a(max_dir_size_kb, int, 0, attr(4))
	]).

