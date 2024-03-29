% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% Installing Void on a ZFS Root - https://docs.voidlinux.org/installation/guides/zfs.html
% https://openzfs.github.io/openzfs-docs/man/master/8/zpool.8.html#ENVIRONMENT_VARIABLES

zfs_col(TL, COL) :-
	memberchk(fs5_multi(zfs, COL, _DL, _PTL, _CK), TL).

uses_zfs(TL) :-
	zfs_col(TL, _COL),
	true.

zfs_uses_encr(TL) :-
	zfs_col(TL, COL),
	zfs_pool_encryption_col(COL),
	!.

zfs_pool_keylocation_file(TL, FN) :-
	zfs_col(TL, COL),
	zfs_pool_keylocation_file_col(COL, FN).

% Do not mount the newly created file system.
zfs_mkfs_cmd(MP, _DS, _AL, _PN, [zfs, create, '-u', o(o, mountpoint=MP)]).
zfs_mkfs_cmd(_MP, _DS, AL, _PN, OL) :-
	findall(o(o, A), member(A, AL), OL).
zfs_mkfs_cmd(_MP, DS, _AL, PN, [concat(PN, DS)]).

zfs_mkfs_multi(PN, PTL) :-
	forall(member(dataset(DS, MP, AL), PTL), (findall(C, (zfs_mkfs_cmd(MP, DS, AL, PN, CL), member(C, CL)), CMD), os_shell2(CMD))),
	true.

zfs_install(TL, _RD) :-
	uses_zfs(TL),
	% os_call2([zgenhostid, '-f']), % Void won't boot with hostid generated by zgenhostid.
	lx_gen_hostid(''),
	!.
zfs_install(_TL, _).

zfs_passphrase(PASS, FN, RD) :-
	atom_concat(RD, FN, PFN),
	open(PFN, write, S),
	write(S, PASS),
	close(S),
	os_call2([chmod, '000', PFN]),
	true.

zfs_zpool_destroy_all :-
	zpool_list(L),
	member(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,_A11), L),
	% tui_progressbox_safe([zpool, export, '-f', PN, '2>&1'], '', [title(' exporting zpool '), sz([6, 40])]),
	tui_progressbox_safe([zpool, destroy, '-f', PN, '2>&1'], '', [title(' destroying zpool '), sz([6, 40])]),
	fail.
zfs_zpool_destroy_all.

zfs_pool_encryption_col(COL) :-
	memberchk(attr(zfs_rw, AL), COL),
	memberchk(encryption=V, AL),
	V \= off.

zfs_pool_keylocation_file_col(COL, FN) :-
	memberchk(attr(zfs_rw, AL), COL),
	memberchk(keylocation= =(file, FN), AL),
	true.

zfs_pool_compatibility_col(COL, V) :-
	memberchk(attr(zpool_props_rw, AL), COL),
	( memberchk(compatibility=V, AL)
	; V = off
	), !.

% "has_boot_part" replacement. We do not check for templates.
zfs_has_boot_part(B) :-
	% bootloader_info(bootloade, supported_fs, supported_template, except_fs).
	bootloader_info(B, FSL, _, _),
	\+ memberchk(zfs, FSL).

zfs_pool_base --> [
	  force=yes
	, mountpoint=none
	].

zfs_pool_props --> [
	  ashift=12
	, autotrim=on
	].

zfs_pool_props(grub2) --> [
	compatibility=grub2
	].
zfs_pool_props(_) --> [].

zfs_pool_feats --> [
	  compression=lz4
	, acltype=posixacl
	, xattr=sa % vastly improves the performance of extended attributes
	, relatime=on % is a middle ground between classic POSIX atime behavior (with its significant performance impact) and atime=off (which provides the best performance by completely disabling atime updates).
	, normalization=formD % eliminates some corner cases relating to UTF-8 filename normalization
	].

zfs_pool_feats(grub2) --> [].
zfs_pool_feats(_) --> [
	dnodesize=auto
	].

zfs_get_col1(B) -->
	zfs_pool_props,
	zfs_pool_props(B).

zfs_get_col2(B) -->
	zfs_pool_feats,
	zfs_pool_feats(B).

zfs_get_col(B) -->
	({ phrase(zfs_get_col1(B), PL), PL \= [], ! } -> [attr(zpool_props_rw, PL)]; []),
	({ phrase(zfs_get_col2(B), FL), FL \= [], ! } -> [attr(zfs_rw, FL)]; []),
	({ phrase(zfs_pool_base, BL), BL \= [], ! } -> [attr(zpool_rw, BL)]; []).

% PN - pool name.
zfs_make_zpool_create_cmd(PN, DL, COL, OL) :-
	findall(PID, (member(PD, DL), (atom_concat('/dev/mapper', _, PD) -> PID = PD; lx_get_dev_disk_id(PD, PID))), PIDL),
	append([zpool, create| COL], [PN, PIDL, '2>&1'], OL),
	true.

zfs_zpool_export_import(PN, RD) :-
	% export and re-import the pool with a temporary, alternate root path
	% os_shell2([zpool, export, PN]),
	format_to_atom(ETitle, ' exporting pool ~w ', [PN]),
	tui_progressbox_safe([zpool, export, PN, '2>&1'], '', [title(ETitle), sz([6, 40])]),
	% os_shell2([zpool, import, '-N', o('R', RD), PN]),
	format_to_atom(ITitle, ' importing pool ~w ', [PN]),
	tui_progressbox_safe([zpool, import, '-N', o('R', RD), PN, '2>&1'], '', [title(ITitle), sz([8, 60])]),
	true.

zfs_mkfs(PN, B, PTL) :-
	zfs_mkfs_multi(PN, PTL),
	zfs_set_bootfs(PN, B),
	true.

zfs_set_bootfs(PN, B) :-
	zfs_has_boot_part(B), !,
	os_call2([zpool, set, bootfs='zroot/ROOT/void', PN]).
zfs_set_bootfs(_PN, _B).

% PN - pool name.
% No native encryption.
zfs_zpool_create(false, Title, B, PN, DL, PTL, COL, RD) :- !,
	get_mkfs_attrs(COL, OL),
	zfs_make_zpool_create_cmd(PN, DL, OL, CL),

	tui_progressbox_safe(CL, '', [title(Title), sz([12, 80])]),

	zfs_zpool_export_import(PN, RD),
	zfs_mkfs(PN, B, PTL),
	!.
zfs_zpool_create(true, Title, B, PN, DL, PTL, COL, RD) :- !,
	inst_setting_tmp(passwd('$_zfs_$'), PSWD),
	get_mkfs_attrs(COL, OL),
	( zfs_has_boot_part(B)
	; zfs_pool_keylocation_file_col(COL, FN),
	  zfs_passphrase(PSWD, FN, '')
	), !,

	zfs_make_zpool_create_cmd(PN, DL, OL, CL),

	tui_infobox(Title, [sz([4, 40])]),
	os_scmdl(CL, CA),
	popen(CA, write, WS),
	write(WS, PSWD), % no nl(WS) should be here.
	close(WS),

	zfs_zpool_export_import(PN, RD),
	zfs_load_key(PN, PSWD),
	zfs_mkfs(PN, B, PTL),
	!.

zfs_export_pool_rd(RD) :-
	zpool_list(L),
	memberchk(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,RD), L),
	% os_call2_rc([zfs, unmount, '-f', '-a'], _),
	format_to_atom(Title, ' exporting pool ~w ', [PN]),
	tui_progressbox_safe([zpool, export, '-f', PN, '2>&1'], '', [title(Title), sz([6, 40])]),
	true.

zfs_destroy_pool_rd(RD) :-
	zpool_list(L),
	memberchk(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,RD), L),
	% tui_progressbox_safe([zpool, export, '-f', PN, '2>&1'], '', [title(' exporting zpool '), sz([6, 40])]),
	format_to_atom(Title, ' destroying pool ~w ', [PN]),
	tui_progressbox_safe([zpool, destroy, '-f', PN, '2>&1'], '', [title(Title), sz([6, 40])]),
	true.

zfs_mount_multi(PN, RD) :-
	% Mount the ZFS hierarchy
	os_call2([zfs, mount, 'zroot/ROOT/void']),
	os_call2([zfs, mount, '-a']),
	% Update device symlinks
	os_call2([udevadm, trigger]),

	% record the current pool configuration in a cache file that Void will use to avoid walking the entire device hierarchy to identify importable pools.
	os_mkdir_p(RD + '/etc/zfs'),
	os_call2([zpool, set, cachefile=concat(RD, '/etc/zfs/zpool.cache'), PN]),
	true.

zfs_setup_encr(FN, RD) :-
	os_call2([mv, FN, RD + FN]),
	true.

