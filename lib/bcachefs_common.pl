% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% man bcachefs
prop_info(bcachefs_rw, opt4s('='), [
	  opt4(acl, enable, no, '--acl') % , attr('Enable POSIX acls')
	, opt4(background_compression, enum([none, lz4, gzip, zstd]), none, '--background_compression')
	, opt4(background_target, target, '', '--background_target') % Device or label to move data to in the background
	, opt4(block_size, size, 0, '--block_size') % block size, in bytes (e.g. 4k)
	, opt4(btree_node_size, size, '256k', '--btree_node_size') % Btree node size, default 256k
	, opt4(compression, enum([none, lz4, gzip, zstd]), none, '--compression') % Set compression type (default: none).
	, opt4(data_checksum, enum([none, crc32c, crc64, xxhash]), crc32c, '--data_checksum') % Set data checksum type (default: crc32c).
	, opt4(data_replicas, int, 1, '--data_replicas') % Number of data replicas
	, opt4(data_replicas_required, int, 1, '--data_replicas_required')
	, opt4(encoded_extent_max, size, 0, '--encoded_extent_max') % Maximum size of checksummed/compressed extents
	, opt4(encrypted, enable, no, '--encrypted') % attr('Enable whole filesystem encryption (chacha20/poly1305); passphrase will be prompted for.')
	, opt4(erasure_code, enable, no, '--erasure_code') % , attr('Enable erasure coding (DO NOT USE YET)')
	, opt4(errors, enum([continue, ro, panic]), panic, '--errors') % Action to take on filesystem error
	, opt4(foreground_target, target, '', '--foreground_target') % Device or label for foreground writes
	, opt4(gc_reserve_bytes, percentage, 10, '--gc_reserve_bytes') % Amount of disk space to reserve for copygc. This takes precedence over gc_reserve_percent if set
	, opt4(gc_reserve_percent, percentage, 10, '--gc_reserve_percent') % Percentage of disk space to reserve for copygc
	, opt4(grpquota, enable, no, '--grpquota') % , attr('Enable group quotas')
	, opt4(inodes_32bit, enable, no, '--inodes_32bit') % , attr('Constrain inode numbers to 32 bits')
	, opt4(inodes_use_key_cache, enable, no, '--inodes_use_key_cache') % , attr('Use the btree key cache for the inodes btree')
	, opt4(journal_transaction_names, enable, no, '--journal_transaction_names') % , attr('Log transaction function names in journal')
	, opt4(label, str, '', '--fs_label')
	, opt4(metadata_checksum, enum([none, crc32c, crc64, xxhash]), crc32c, '--metadata_checksum') % Set metadata checksum type (default: crc32c).
	, opt4(metadata_replicas, int, 1, '--metadata_replicas') % Number of metadata replicas
	, opt4(metadata_replicas_required, int, 1, '--metadata_replicas_required')
	, opt4(metadata_target, target, '', '--metadata_target') % Device or label for metadata writes
	, opt4(no_passphrase, enable, no, '--no_passphrase') % attr('Don\'t encrypt master encryption key')
	, opt4(nocow, enable, no, '--nocow') % , attr('Nocow mode: Writes will be done in place when possible. Snapshots and reflink will still caused writes to be COW. This flag implicitly disables data checksumming, compression and encryption.')
	, opt4(prjquota, enable, no, '--prjquota') % , attr('Enable project quotas')
	, opt4(promote_target, target, '', '--promote_target') % Device or label to promote data to on read
	, opt4(replicas, int, 1, '--replicas') % Sets both data and metadata replicas
	, opt4(root_reserve_percent, percentage, 10, '--root_reserve_percent') % Percentage of disk space to reserve for superuser
	, opt4(shared_inode_numbers, enable, no, '--shared_inode_numbers') % , attr('Shared new inode numbers by CPU id')
	, opt4(str_hash, enum([crc32c, crc64, siphash]), crc32c, '--str_hash') % Hash function for directory entries and xattrs
	, opt4(superblock_size, size, 0, '--superblock_size')
	, opt4(usrquota, enable, no, '--usrquota') % , attr('Enable user quotas')
	, opt4(uuid, guid, '', '--uuid')
	, opt4(wide_macs, enable, no, '--wide_macs') % , attr('Store full 128bits of cryptographic MACS, instead of 80')

	% Device-specific
	, opt4(bucket, size, 0, '--bucket') % Specifies the bucket size; must be greater than the btree node size
	, opt4(dev_label, str, '', '--label')
	, opt4(discard, enable, no, '--discard') % , attr('Enable discard/TRIM support')
	, opt4(durability, int, 1, '--durability') % Data written to this device will be considered to have already been replicated n times
	, opt4(force, enable, no, '--force')
	, opt4(fs_size, size, 0, '--fs_size') % Create the filesystem using size bytes on the subsequent device.
	]).


