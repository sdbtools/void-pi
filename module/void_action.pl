% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% Detect that we are running void-live ISO.
is_void_live :-
	% os_shell_line('uname -n', 'void-live').
	host_name('void-live').

make_sgdisk_par(TL, [PD|T], N, [A1, A2|T1]) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
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
	tui_progressbox_safe([sgdisk, '--zap-all', '--clear', '--mbrtogpt', SGPL, D], '', [title(MA), sz([6, 60])]),
	true.

% Partitions which should be created explicitly.
% PT - partition type
% explicit_part(PT, FS, MP).
explicit_part(bios_boot, _, _) :- !.
explicit_part(efi_system, vfat, '/boot/efi') :- !.

mount_boot_efi(ED, RD) :-
	atom_concat(RD, '/boot/efi', DA),
	os_mkdir_p(DA),
	os_call2([mount, '-o', 'rw,noatime', ED, DA]).

validate_fs(TL) :-
	( has_root_part(TL)
	; tui_msgbox('ERROR: the mount point for the root filesystem (/) has not yet been configured.'),
	  fail
	), !,
	% https://arch-general.archlinux.narkive.com/MgF0tcbX/usr-is-not-mounted-this-is-not-supported
	( \+ has_usr_part(TL)
	; tui_msgbox('ERROR: /usr mount point has been configured but is not supported, please remove it to continue.'),
	  fail
	), !,
	% EFI
	( inst_setting(system(efi), _) ->
	  ( has_efi_system_part(TL)
	  ; get_bootloader(TL, B),
		get_bootloader_mp(B, MP),
		tui_msgbox2(['ERROR: The EFI System Partition has not yet been configured, please create it as FAT32, mountpoint', MP, 'and at least with 100MB of size.']),
	    fail
	  ), !
	; true
	),
	true.

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
	tui_progressbox_safe([tar, xvf, N, o('C', RD)], '', [title(' Extracting rootfs '), sz(max)]),

	% mount required fs
	mount_chroot_filesystems(RD),
	% Copy the DNS configuration into the new root so that XBPS can still download new packages inside the chroot.
	os_mkdir_p(RD + '/etc'), os_call2([cp, '-L', '/etc/resolv.conf', RD + '/etc/']),

	get_bootloader(TL, B),
	% dracut stuff.
	dracut_conf(TL, B, RD),

	install_target_dep(TL, RD),
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
	setup_bootloader(B, TL, RD),

	tui_progressbox_safe(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),

	true.

install_pkg(TL, local, RD) :-
	copy_rootfs(RD),
	host_name(HN),
	( HN = 'void-live' ->
	  os_rm_f(RD + '/etc/motd'),
	  % Remove modified sddm.conf to let sddm use the defaults.
	  os_rm_f(RD + '/etc/sddm.conf'),
	  os_rm_f(RD + '/etc/sudoers.d/99-void-live')
	; true
	),
	( (HN = 'void-live' ; HN = hrmpf) ->
	  os_rm_f(RD + '/etc/issue'),
	  os_rm_f(RD + '/usr/sbin/void-installer'),
	  % Remove live user.
	  tui_progressbox_safe([userdel, o('R', RD), '-r', anon, '2>&1'], '', [title(' Remove user anon '), sz([6, 60])])
	; true
	),

	% mount required fs
	mount_chroot_filesystems(RD),

	get_bootloader(TL, B),
	% dracut stuff.
	dracut_conf(TL, B, RD),
	% DL = [chroot, RD, dracut, '--no-hostonly', '--force', '2>&1'],
	DL = [chroot, RD, dracut, '--regenerate-all', '--hostonly', '--force', '2>&1'],
	tui_progressbox_safe(DL, '', [title(' Rebuilding initramfs for target '), sz(max)]),

	install_target_dep(TL, RD),
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
	tui_msgbox(M),
	fail.
on_useradd_rc(RC) :-
	number_atom(RC, RCA),
	tui_msgbox2(['useradd. Unknown error code:', RCA]),
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

% Old code
% mount_chroot_filesystems(RD) :-
% 	maplist(mount_chroot_filesystem_rbind(RD), ['/sys', '/dev', '/proc']),
% 	true.

% New code.
mount_chroot_filesystems(RD) :-
	maplist(mount_chroot_filesystem_none(RD), [m(proc, proc), m(sysfs, sys)]),
	findall(M, mount_chroot_filesystem_rbind_(M), L),
	maplist(mount_chroot_filesystem_rbind(RD), L),
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

mount_chroot_filesystem_rbind_('/dev').
mount_chroot_filesystem_rbind_('/run').
mount_chroot_filesystem_rbind_('/sys/firmware/efi/efivars') :-
	inst_setting(system(efi), _).

% Unmount ALL filesystems mounted during installation.
umount_filesystems(RD) :-
	% ??? swap ???
	os_call2([umount, '--recursive', RD]),
	fail.
umount_filesystems(RD) :-
	zpool_list(L),
	memberchk(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,RD), L),
	tui_progressbox_safe([zpool, export, '-f', PN, '2>&1'], '', [title(' export zpool '), sz([6, 40])]),
	fail.
umount_filesystems(_RD).

% Old code.
% wipe_disk(D) :-
% 	os_shell2_lines([wipefs, '--noheadings', D], L),
% 	( L = []
% 	; os_shell2([wipefs, '--all', '--force', D, '2>&1', '1>/dev/null']),
% 	  wipe_disk(D)
% 	),
% 	!.

wipe_disk(D) :-
	os_shell2_lines([wipefs, '--noheadings', D], L),
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
	maplist(wipe_dev_tree(PL), TL).

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
wipe_dev(part3(PARTTYPE,_PARTUUID,_UUID), D, _CN) :- !,
	wipe_dev_part(PARTTYPE, D),
	true.
wipe_dev(disk, D, _CN) :- !,
	wipe_disk(D),
	true.
wipe_dev(lv(VG,_LV), _D, _CN) :- !,
	lvm_vgremove_unsafe(VG),
	% wipe_disk(D),
	true.
wipe_dev(ET, D, _CN) :- !,
	format_to_atom(A, 'Unknown type "~w" of ~w.', [ET, D]),
	tui_msgbox(A),
	fail.

wipe_dev_part(linux_lvm, D) :-
	% tui_msgbox(D),
	lvm_pvremove(D),
	% wipe_disk(D),
	true.
wipe_dev_part(_T, _D) :-
	% wipe_disk(D),
	true.

part2taglist(part_info(bd1([PD| _]), _FS, FSS, _Type), [PD, FSS]).

ensure_settings(TT, TL) :-
	% S = [partition, bootloader_dev, keymap, network, source, hostname, locale, timezone, passwd, useraccount],
	S = [bootloader_dev, keymap, network, source, hostname, locale, timezone, passwd, useraccount],
	maplist(ensure_setting(TT, TL), S).

ensure_passwd(_TT) :-
	\+ inst_setting_tmp(passwd(root), _),
	menu_password_user(root),
	fail.
ensure_passwd(_TT) :-
	inst_setting(useraccount, user(U, _, _)),
	\+ inst_setting_tmp(passwd(U), _),
	menu_password_user(U),
	fail.
ensure_passwd(TT) :-
	memberchk(TT, [gpt_lvm_luks, gpt_luks, gpt_luks_lvm]),
	U = '$_luks_$',
	\+ inst_setting_tmp(passwd(U), _),
	menu_password_luks(U),
	fail.
ensure_passwd(_TT).

ensure_setting(TT, _TL, passwd) :- !,
	ensure_passwd(TT).
ensure_setting(TT, TL, bootloader_dev) :- !,
	ensure_bootloader_dev(TT, TL).
ensure_setting(_TT, _TL, S) :-
	inst_setting(S, _), !.
ensure_setting(TT, TL, S) :-
	cmd_menu(S, TT, TL).

ensure_bootloader_dev(manual, _TL) :- !.
ensure_bootloader_dev(TT, TL) :-
	( memberchk(bootloader_dev(_), TL)
	; cmd_menu(bootloader_dev, TT, TL)
	), !.

save_settings(S) :-
	inst_setting(N, V),
	portray_clause(S, inst_setting(N, V)),
	fail.
save_settings(_).


