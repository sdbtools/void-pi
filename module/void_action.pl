% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% Detect that we are running void-live ISO.
is_void_live :-
	% os_shell_line('uname -n', 'void-live').
	host_name('void-live').

make_sgdisk_par(TL, [PD|T], N, [A1, A2|T1]) :-
	memberchk(p4(PT, bd1([PD| _]), _CK, SZ), TL),
	part_typecode(PT, PTC),
	( T = [] ->
	  format_to_atom(A1, '--new=~d:0:0', [N])
	; format_to_atom(A1, '--new=~d:0:+~w', [N, SZ])
	),
	format_to_atom(A2, '--typecode=~d:~w', [N, PTC]),
	N1 is N + 1,
	make_sgdisk_par(TL, T, N1, T1),
	true.
make_sgdisk_par(_TL, [], _, []).

% PL - partition list.
part_sgdisk_pl(TL, D, PL) :-
	% tui_msgbox2([part_sgdisk_pl, PL]),
	make_sgdisk_par(TL, PL, 1, SGPL),
	format_to_atom(MA, ' Partitioning ~w ', [D]),
	tui_progressbox_safe([sgdisk, '--zap-all', '--clear', '--mbrtogpt', SGPL, D, '2>&1'], '', [title(MA), sz([6, 60])]),
	% This is required for zfs.
	% Watch the udev event queue, and exit if all current events are handled.
	os_call2([udevadm, settle]),
	true.

validate_fs(TL) :-
	( get_bootloader_dev7(TL, _)
	; tui_yesno('A bootloader device has not been selected. A bootloader will not be installed. Is this what you want?', [title(' WARNING ')])
	), !,
	( has_root_part(TL)
	; tui_msgbox('The mount point for the root filesystem (/) has not yet been configured.', [title(' ERROR ')]),
	  fail
	), !,
	% https://arch-general.archlinux.narkive.com/MgF0tcbX/usr-is-not-mounted-this-is-not-supported
	( \+ has_usr_part(TL)
	; tui_msgbox('/usr mount point has been configured but is not supported, please remove it to continue.', [title(' ERROR ')]),
	  fail
	), !,
	validate_efi(TL),
	true.

% EFI
validate_efi(TL) :-
	inst_setting(system(efi), _), !,
	% p4(PartType, device, create/keep, size)
	( memberchk(p4(sys_efi, bd1([PD|_]), _, _), TL)
	; os_gpt_part_type(UUID, sys_efi, PTN),
	  format_to_atom(MSG, 'The EFI System Partition has not been selected, please select a partition having type "~w" and UUID ~w and at least with 100MB of size.', [PTN, UUID]),
	  tui_msgbox(MSG, [title(' ERROR ')]),
	  fail
	), !,
	get_bootloader(TL, B),
	get_bootloader_mp(B, MP),
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	( memberchk(fs7(FS, _Label, MP, PD, _COL, _MOL, _CK), TL)
	; tui_msgbox2(['The EFI System Partition', PD, 'has not yet been configured, please mount it at', MP], [title(' ERROR ')]),
	  fail
	), !,
	( FS = vfat
	; tui_msgbox2(['The EFI System Partition', PD, 'has not yet been configured, please create it as FAT32'], [title(' ERROR ')]),
	  fail
	), !,
	true.
validate_efi(_TL).

parse_rootfs_name(N, ARCH) :-
	split_rootfs_file_name(N, NL),
	parse_rootfs_name_(NL, ARCH),
	!.
parse_rootfs_name(N, _NL) :-
	tui_msgbox2(['Invalid rootfs file name:', N]),
	fail.

split_rootfs_file_name(N, NL) :-
	atom_codes(N, NA),
	split_list_ne(NA, "-", NAL),
	maplist(codes_atom, NAL, NL),
	true.

parse_rootfs_name_([void, A, musl, 'ROOTFS', _], ARCH) :- !,
	atom_concat(A, '-musl', ARCH).
parse_rootfs_name_([void, ARCH, 'ROOTFS', _], ARCH) :- !.
parse_rootfs_name_([void, rpi, A, musl, 'ROOTFS', _], ARCH) :- !,
	atom_concat(A, '-musl', ARCH).
parse_rootfs_name_([void, rpi, ARCH, 'ROOTFS', _], ARCH) :- !.

make_chroot_inst_pref(ARCH, P) :-
	atom_concat('XBPS_ARCH=', ARCH, XBPS_ARCH),
	P = [stdbuf, '-oL', env, XBPS_ARCH],
	true.

make_chroot_inst_pref_chroot(ARCH, P, RD) :-
	atom_concat('XBPS_ARCH=', ARCH, XBPS_ARCH),
	P = [stdbuf, '-oL', env, XBPS_ARCH, chroot, RD],
	true.

need_to_remove_pkg(_TL, [dialog, 'xtools-minimal']).
need_to_remove_pkg(TL, [grub]) :-
	% Remove grub if we are using different bootloader.
	get_bootloader(TL, B),
	B \= grub2.

install_pkg(TL, rootfs, RD) :-
	% N = 'void-x86_64-ROOTFS-20221001.tar.xz',
	working_directory(PWD),
	tui_fselect(PWD, [sz(max)], N),
	decompose_file_name(N, _Dir, NPref, NSuf),
	atom_concat(NPref, NSuf, FN),
	parse_rootfs_name(FN, ARCH),

	retractall(inst_setting(system(arch), _)),
	assertz(inst_setting(system(arch), ARCH)),

	% os_call2([tar, 'xvf', N, o('C', RD)]),
	tui_progressbox_safe([tar, xvf, N, o('C', RD), '2>&1'], '', [title(' Extracting rootfs '), sz(max)]),

	% mount required fs
	mount_chroot_filesystems(RD),
	% Copy the DNS configuration into the new root so that XBPS can still download new packages inside the chroot.
	os_mkdir_p(RD + '/etc'), os_call2([cp, '-L', '/etc/resolv.conf', RD + '/etc/']),

	get_bootloader(TL, B),
	% dracut stuff.
	dracut_conf(TL, B, RD),

	install_target_dep(TL, RD),
	install_target_soft(TL, RD),
	setup_bootloader(B, TL, RD),

	% Remove stuff
	soft_remove_pkg_list(['base-voidstrap'], RD),

	tui_progressbox_safe(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),
	true.

install_pkg(TL, net, RD) :-
	% mount required fs
	mount_chroot_filesystems(RD),

	os_mkdir_p([RD + '/var/db/xbps/keys', RD + '/usr/share']),
	os_call2([cp, '-a', '/usr/share/xbps.d', RD + '/usr/share/']),
	os_shell2([cp, '/var/db/xbps/keys/*.plist', RD + '/var/db/xbps/keys']),
	( get_bootloader(TL, grub2) ->
	  os_mkdir_p(RD + '/boot/grub')
	; true
	),

	get_bootloader(TL, B),
	% dracut stuff.
	dracut_conf(TL, B, RD),

	install_target_dep(TL, RD),
	install_target_soft(TL, RD),
	setup_bootloader(B, TL, RD),

	tui_progressbox_safe(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),

	true.

install_pkg(TL, local, RD) :-
	copy_rootfs(RD),
	host_name(HN),
	( HN \= 'void-live'
	; os_rm_f(RD + '/etc/motd'),
	  % Remove modified sddm.conf to let sddm use the defaults.
	  os_rm_f(RD + '/etc/sddm.conf'),
	  os_rm_f(RD + '/etc/sudoers.d/99-void-live')
	),
	( (HN = 'void-live' ; HN = hrmpf) ->
	  os_rm_f(RD + '/etc/issue'),
	  os_rm_f(RD + '/usr/sbin/void-installer'),
	  % Remove live user.
	  tui_progressbox_safe([userdel, o('R', RD), '-r', anon, '2>&1'], '', [title(' Remove user anon '), sz([6, 60])])
	; true
	),
	( HN \= hrmpf
	; make_agetty_generic_run(RD)
	),

	% mount required fs
	mount_chroot_filesystems(RD),

	get_bootloader(TL, B),
	% dracut stuff.
	dracut_setup(TL, B, RD),

	install_target_dep(TL, RD),
	install_target_soft(TL, RD),
	setup_bootloader(B, TL, RD),

	% Remove stuff
	( HN = hrmpf
	; % Remove temporary packages from target
	  findall(P, (need_to_remove_pkg(TL, PL0), member(P, PL0)), PL),
	  soft_remove_pkg_list(PL, RD)
	),
	true.

install_target_dep(TL, RD) :-
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	( setof(D, target_dep(TL, D), TPL) ->
	  soft_install_deps(Pref, TPL)
	; true
	),
	true.

install_target_soft(TL, RD) :-
	setof(D, target_soft(TL, D), SL),
	soft_install_soft(SL, RD),
	!.
install_target_soft(_TL, _RD).

make_agetty_generic_run(RD) :-
	atom_concat(RD, '/etc/sv/agetty-generic/run', F),
	open(F, write, S),
	make_agetty_generic_run_(S),
	close(S),
	os_shell2([chmod, '755', F]),
	true.

make_agetty_generic_run_(S) :-
	write(S, '#!/bin/sh'), nl(S), nl(S),
	write(S, 'tty=${PWD##*-}'), nl(S), nl(S),
	write(S, '[ -r conf ] && . ./conf'), nl(S), nl(S),
	write(S, 'if [ -x /sbin/getty -o -x /bin/getty ]; then'), nl(S),
	write(S, '\t# busybox'), nl(S),
	write(S, '\tGETTY=getty'), nl(S),
	write(S, 'elif [ -x /sbin/agetty -o -x /bin/agetty ]; then'), nl(S),
	write(S, '\t# util-linux'), nl(S),
	write(S, '\tGETTY=agetty'), nl(S),
	write(S, 'fi'), nl(S), nl(S),
	write(S, 'exec chpst -P ${GETTY} ${GETTY_ARGS} "${tty}" "${BAUD_RATE}" "${TERM_NAME}"'), nl(S),
	true.

set_keymap(RD) :-
	inst_setting(keymap, KM),
	lx_set_keymap(RD, KM),
	!.
set_keymap(_) :-
	tui_msgbox('Setting of keymap has failed.'),
	fail.

set_locale(_RD) :-
	inst_setting(system(arch), 'x86_64-musl'),
	!.
set_locale(RD) :-
	inst_setting(locale, LC),
	lx_set_locale(RD, LC),
	!.
set_locale(_RD) :-
	tui_msgbox('Setting of locale has failed.'),
	fail.

set_timezone(RD) :-
	inst_setting(timezone, TZ),
	lx_set_timezone(RD, TZ),
	!.
set_timezone(_RD) :-
	tui_msgbox('Setting of timezone has failed.'),
	fail.

set_hostname(RD) :-
	inst_setting(hostname, HN),
	lx_set_hostname(RD, HN),
	!.
set_hostname(_RD) :-
	tui_msgbox('Setting of hostname has failed.'),
	fail.

set_rootpassword(RD) :-
	inst_setting_tmp(passwd(root), PW),
	lx_set_password(RD, root, PW),
	!.
set_rootpassword(_RD) :-
	tui_msgbox('Setting of root password has failed.'),
	fail.

on_useradd_rc(0) :- !.
on_useradd_rc(RC) :-
	lx_useradd_rc(RC, M), !,
	tui_msgbox(M, [title(' useradd ERROR ')]),
	fail.
on_useradd_rc(RC) :-
	number_atom(RC, RCA),
	tui_msgbox2(['Unknown error code:', RCA], [title(' useradd ERROR ')]),
	fail.

set_useraccount(RD) :-
	% UL - user login
	% UN - user name
	% UGL - user group list
	inst_setting(useraccount, user(UL, UN, UGL)),
	lx_chroot_useradd_rc(RD, UL, UN, UGL, RC),
	on_useradd_rc(RC),
	% UP - user password
	inst_setting_tmp(passwd(UL), UP),
	lx_set_password(RD, UL, UP),
	!.
set_useraccount(_RD) :-
	tui_msgbox('Setting up of a user account has failed.'),
	fail.

copy_rootfs(RD) :-
	TA = [tar, '--create', '--one-file-system', '--xattrs'],
	TAE = [TA, '-f', '-', '/', '2>/dev/null', '|',
		tar, '--extract', '--xattrs', '--xattrs-include=\'*\'', '--preserve-permissions', '-v', '-f', '-', '-C', RD, '2>&1'
	],
	% TAN = [TA, '-v', '-f', '/dev/null', '/', '2>/dev/null', '|', 'wc', '-l'],
	% os_shell2_number(TAN, N),
	% os_shell2(TAE),
	tui_progressbox_unsafe(TAE, '', [title(' Copying live image to target rootfs '), sz(max)]),
	% tui_programbox_unsafe(TAE, '', [title(' Copying live image to target rootfs '), sz(max)]),
	true.

set_sudoers :-
	inst_setting(useraccount, user(UL, _UN, UGL)),
	lx_chroot_set_sudoers(UL, UGL),
	!.
set_sudoers :-
	tui_msgbox('Setting up of sudoers has failed.'),
	fail.

mount_chroot_filesystems(RD) :-
	maplist(mount_chroot_filesystem_rbind(RD), ['/sys', '/dev', '/proc']),
	true.

mount_chroot_filesystem_rbind(RD, D) :-
	atom_concat(RD, D, D1),
	os_mkdir_p(D1),
	os_call2([mount, '--rbind', D, D1]),
	% os_call2([mount, '--make-rslave', D1]),
	true.

mount_chroot_filesystem_none(RD, m(FS, MP)) :-
	format_to_atom(MP1, '~w/~w', [RD, MP]),
	os_mkdir_p(MP1),
	os_call2([mount, '-t', FS, none, MP1]),
	true.

% Unmount ALL filesystems mounted during installation.
umount_filesystems(RD) :-
	% ??? swap ???
	os_call2([umount, '--recursive', RD]),
	% os_call2([umount, '--lazy', '--recursive', RD]),
	fail.
umount_filesystems(RD) :-
	zfs_export_pool_rd(RD),
	fail.
umount_filesystems(_RD).

wipe_disk(D) :-
	os_shell2_lines([wipefs, '--noheadings', D], L),
	% os_shell2_lines([wipefs, '--all', '--force', D, '2>&1', '1>/dev/null'], L),
	( L = [] ->
	  % os_shell2([sgdisk, '-Zo', D])
	  format_to_atom(Title, ' Cleaning Device ~w ', [D]),
	  tui_progressbox_safe([sgdisk, '-Zo', D, '2>&1'], '', [title(Title), sz([6, 40])])
	; os_shell2([wipefs, '--all', '--force', D, '2>&1', '1>/dev/null']),
	  wipe_disk(D)
	),
	!.

% TL - tree list.
% PL - partition list.
wipe_dev_tree_list(TL, PL) :-
	forall(member(T, TL), wipe_dev_tree(PL, T)).

wipe_dev_tree(PL, tree(NAME, L)) :-
	wipe_dev_tree_list(L, PL),
	% dev_part(NAME,name(SNAME,KNAME,DL),ET,SIZE)
	memberchk(dev_part(NAME, CN, ET, _SIZE), PL),
	wipe_dev(ET, NAME, CN),
	true.

% wipe_dev(type, device, compound_name)
wipe_dev(crypt(_UUID), _D, name(SNAME,_KNAME,_DL)) :- !,
	lx_luks_close(SNAME),
	true.
wipe_dev(part5(_PTTYPE,PARTTYPE,_PARTUUID,_UUID,FSTYPE), D, _CN) :- !,
	wipe_dev_part(PARTTYPE,FSTYPE, D),
	true.
wipe_dev(disk, D, _CN) :- !,
	wipe_disk(D),
	true.
wipe_dev(lv(VG,LV), _D, _CN) :- !,
	% lvm_vgremove_unsafe(VG),
	lvm_lvremove_unsafe(VG, LV),
	% wipe_disk(D),
	true.
wipe_dev(ET, D, _CN) :- !,
	format_to_atom(A, 'Unknown type "~w" of ~w.', [ET, D]),
	tui_msgbox(A),
	fail.

wipe_dev_part(linux_lvm, _FSTYPE, D) :- !,
	lvm_pvremove_unsafe(D),
	% wipe_disk(D),
	true.
wipe_dev_part(solaris_root, zfs_member, D) :- !,
	% tui_msgbox(D),
	zfs_zpool_destroy_all,
	wipe_disk(D),
	true.
wipe_dev_part(_T, _FSTYPE, D) :-
	wipe_disk(D),
	true.

ensure_settings(TT, TL) :-
	% S = [partition, bootloader_dev, keymap, network, source, hostname, locale, timezone, passwd, useraccount],
	S = [bootloader_dev, keymap, network, source, hostname, locale, timezone, passwd, useraccount],
	maplist(ensure_setting(TT, TL), S).

ensure_passwd(_TT, _TL) :-
	\+ inst_setting_tmp(passwd(root), _),
	menu_password_user(root),
	fail.
ensure_passwd(_TT, _TL) :-
	inst_setting(useraccount, user(U, _, _)),
	\+ inst_setting_tmp(passwd(U), _),
	menu_password_user(U),
	fail.
ensure_passwd(TT, _TL) :-
	memberchk(TT, [gpt_lvm_luks, gpt_luks, gpt_luks_lvm]),
	U = '$_luks_$',
	\+ inst_setting_tmp(passwd(U), _),
	menu_password_luks(U),
	fail.
ensure_passwd(_TT, TL) :-
	uses_encr_zfs(TL),
	U = '$_zfs_$',
	\+ inst_setting_tmp(passwd(U), _),
	menu_password_for('ZFS', U),
	fail.
ensure_passwd(_TT, _TL).

ensure_setting(TT, TL, passwd) :- !,
	ensure_passwd(TT, TL).
ensure_setting(TT, TL, bootloader_dev) :- !,
	ensure_bootloader_dev(TT, TL).
ensure_setting(_TT, _TL, S) :-
	inst_setting(S, _), !.
ensure_setting(TT, TL, S) :-
	cmd_menu(S, TT, TL), !.
ensure_setting(_TT, _TL, S) :-
	cmd_menu_common(S).

save_settings(S) :-
	inst_setting(N, V),
	portray_clause(S, inst_setting(N, V)),
	fail.
save_settings(_).


