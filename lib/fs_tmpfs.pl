% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man 5 tmpfs

prop_info(mnt_tmpfs_opt, opt3s('--options', ','), [
	  opt3(size, size, 0)
	, opt3(nr_blocks, size, 0)
	, opt3(nr_inodes, size, 0)
	, opt3(mode, int, 0)
	, opt3(gid, int, 0)
	, opt3(uid, int, 0)
	, opt3(huge, enum([never, always, within_size, advise, deny, force]), never)
	, opt3(mpol, enum([
		default,
		% prefer:node,
		% bind:nodelist,
		interleave,
		% interleave:nodelist,
		local
		]), default)
	]).

