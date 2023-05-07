#!/usr/bin/gprolog --consult-file
% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- initialization(main).

:- include('lib/atom_common.pl').
:- include('lib/list_common.pl').
:- include('lib/os_common.pl').
:- include('lib/os_dracut.pl').
:- include('lib/os_grub.pl').
:- include('lib/cli_common.pl').
:- include('lib/unix_common.pl').
:- include('lib/linux_common.pl').
:- include('lib/linux_luks.pl').
:- include('lib/zfs_common.pl').
:- include('lib/lvm_common.pl').
:- include('lib/tui_common.pl').
:- include('lib/tui_dialog.pl').

:- include('module/void_info.pl').
:- include('module/void_cmd_arg.pl').
:- include('module/void_menu.pl').

:- include('module/void_luks.pl').
:- include('module/void_dracut.pl').
:- include('module/void_net.pl').
:- include('module/void_fstab.pl').

:- include('module/void_grub.pl').
:- include('module/void_limine.pl').
:- include('module/void_refind.pl').
:- include('module/void_bootloader.pl').

:- include('module/void_zfs.pl').
:- include('module/void_btrfs.pl').
:- include('module/void_fs.pl').

:- include('module/void_template.pl').

:- dynamic([inst_setting/2, inst_setting_tmp/2]).

% https://wiki.archlinux.org/title/Btrfs
% Installing Void on a ZFS Root - https://docs.voidlinux.org/installation/guides/zfs.html
% https://wiki.archlinux.org/title/Dm-crypt
% https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system
% https://wiki.archlinux.org/title/Dm-crypt/System_configuration
% Automatic LUKS unlock using keyfile on boot partition: https://unix.stackexchange.com/questions/666770/automatic-luks-unlock-using-keyfile-on-boot-partition
% Stacking LVM volumes: https://access.redhat.com/articles/2106521

def_settings :-
	setup_conf,
	setup_fs_template,
	B = grub2,
	assertz(inst_setting(keymap, us)),
	assertz(inst_setting(locale, 'en_US.UTF-8')),
	assertz(inst_setting(timezone, 'America/New_York')),
	assertz(inst_setting(useraccount, user(void, 'Void User', [wheel, floppy, audio, video, cdrom, optical, kvm, xbuilder]))),
	assertz(inst_setting(bootloader, B)),
	assertz(inst_setting(root_fs, ext4)),
	assertz(inst_setting(mbr_size, '2M')),
	assertz(inst_setting(esp_size, '550M')),
	assertz(inst_setting(boot_size, '1G')),
	assertz(inst_setting(root_dir, '/mnt')),
	% fs_attr(Name, MountPoint)
	assertz(inst_setting(fs_attr(btrfs, '/'), mount([rw, noatime, 'compress-force'=zstd, space_cache=v2, commit=120]))),
	assertz(inst_setting(fs_attr(vfat, '/boot/efi'), mount([rw, nosuid, nodev, noexec, relatime, fmask='0022', dmask='0022', codepage=437, iocharset='iso8859-1', shortname=mixed, utf8, errors='remount-ro']))),
	assertz(inst_setting(fs_attr(f2fs, '/'), mount([rw, compress_algorithm=lz4, compress_chksum, atgc, gc_merge, lazytime]))),
	assertz(inst_setting(fs_attr(f2fs, '/'), create([extra_attr, inode_checksum, sb_checksum, compression, encrypt]))),
	assertz(inst_setting(source, local)),
	assertz(inst_setting(hostname, voidpp)),
	assertz(inst_setting(lvm, lv(void, void, ''))),
	assertz(inst_setting(luks, luks(cryptroot))),
	assertz(inst_setting(config_file, 'settings.pl')),

	on_enable(template(manual), B),

	true.

source_dependency_pkg(Distro, DL) :-
	setof(D, source_dep(Distro, D), DL).

source_dep(Distro, D) :-
	% Collect all used filesystems.
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	findall(FS, inst_setting(fs, fs4(FS, _Label, _MP, _BD1)), FSL0),
	sort(FSL0, FSL),
	% tui_msgbox2(PTL),
	member(F, FSL),
	source_dep_module(Distro, filesystem(F), DL),
	member(D, DL),
	true.
source_dep(Distro, D) :-
	inst_setting(template, T),
	source_dep_module(Distro, template(T), DL),
	member(D, DL),
	true.
source_dep(Distro, D) :-
	inst_setting(source, S),
	source_dep_module(Distro, inst_method(S), DL),
	member(D, DL),
	true.
source_dep(Distro, D) :-
	inst_setting(bootloader, B),
	source_dep_module(Distro, bootloader(B), DL),
	member(D, DL),
	true.

install_deps(_, []) :- !.
install_deps(Pref, D) :-
	% tui_progressbox_safe([Pref, 'xbps-install', '-Suy', xbps, '2>&1'], '', [title(' Update xbps '), sz([12, 80])]),
	% % needed by old versions of xbps-install.
	% tui_progressbox_safe([Pref, 'xbps-install', '-S', '2>&1'], '', [title(' Synchronize remote repository index files '), sz([6, 80])]),
	tui_progressbox_safe([Pref, 'xbps-install', '-SyU', D, '2>&1'], '', [title(' Install dependencies '), sz(max)]).

install_deps_chroot(_, [], _) :- !.
install_deps_chroot(Pref, D, RD) :-
	% tui_programbox_safe([Pref, 'xbps-install', o(r, RD), '-SyU', D, '2>&1'], '', [title(' Installing base system packages... '), sz(max)]).
	tui_progressbox_safe([Pref, 'xbps-install', o(r, RD), '-SyU', D, '2>&1'], '', [title(' Installing base system packages... '), sz(max)]).

setup_tui :-
	retractall(tui_def_args_all(_)),
	asserta(tui_def_args_all([sz(auto), clear, no-shadow])).

setup_sys_kernel :-
	lx_sys_kernel(V1N, V2N, V3A),
	retractall(inst_setting(system(kernel), _)),
	assertz(inst_setting(system(kernel), v(V1N, V2N, V3A))),
	true.

setup_sys_arch :-
	lx_sys_arch(ARCH),
	% tui_msgbox(ARCH),
	retractall(inst_setting(system(arch), _)),
	assertz(inst_setting(system(arch), ARCH)),
	true.

setup_sys_efi :-
	lx_sys_efi(EFI_TARGET),
	retractall(inst_setting(system(efi), _)),
	assertz(inst_setting(system(efi), EFI_TARGET)),
	!.
setup_sys_efi.

setup_conf :-
	setup_sys_efi,
	setup_sys_arch,
	setup_sys_kernel,
	true.

setup_install :-
	% Install dependencies
	host_name(HN),
	( source_dependency_pkg(HN, D) ->
	  % tui_msgbox2(D),
	  install_deps([], D)
	; true
	).

parse_prop(P, dp(N, V)) :-
	atom_codes(P, PA),
	split_list_ne(PA, "=", [NL, [_|VL]]),
	append(VL1, "\"", VL),
	codes_atom(NL, N),
	codes_atom(VL1, V),
	true.

% Detect that we are running void-live ISO.
is_void_live :-
	% os_shell_line('uname -n', 'void-live').
	host_name('void-live').

make_sgdisk_par([PD|T], N, [A1, A2|T1]) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting(partition, part4(bd1([PD| _]), PT, _F, SZ)),
	part_typecode(PT, PTC),
	( T = [] ->
	  format_to_atom(A1, '--new=~d:0:0', [N])
	; format_to_atom(A1, '--new=~d:0:+~w', [N, SZ])
	),
	format_to_atom(A2, '--typecode=~d:~w', [N, PTC]),
	N1 is N + 1,
	make_sgdisk_par(T, N1, T1),
	true.
make_sgdisk_par([], _, []).

part_sgdisk(D) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, inst_setting(partition, part4(bd1([PD| D]), _, _, _SZ)), PL),
	sort(PL, SPL),
	part_sgdisk_pl(D, SPL),
	true.

part_dev(D) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, inst_setting(partition, part4(bd1([PD| D]), _, _, _SZ)), PL),
	sort(PL, SPL),
	make_par(D, SPL),
	true.

% PL - partition list.
part_sgdisk_pl(D, PL) :-
	% tui_msgbox2([part_sgdisk_pl, PL]),
	make_sgdisk_par(PL, 1, SGPL),
	format_to_atom(MA, ' Partitioning ~w ', [D]),
	tui_progressbox_safe([sgdisk, '--zap-all', '--clear', '--mbrtogpt', SGPL, D], '', [title(MA), sz([6, 60])]),
	true.

make_par(D, PL) :-
	part_sgdisk_pl(D, PL),
	true.

split_pl([PD|T], [PD|T1], T2) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	inst_setting(partition, part4(bd1([PD| _]), PT, _F, _SZ)),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(FS, _Label, MP, bd1([PD| _]))),
	explicit_part(PT, FS, MP), !,
	split_pl(T, T1, T2).
split_pl([H|T], T1, [H|T2]) :-
	split_pl(T, T1, T2).
split_pl([], [], []).

% Partitions which should be created explicitly.
% PT - partition type
% explicit_part(PT, FS, MP).
explicit_part(bios_boot, _, _) :- !.
explicit_part(efi_system, vfat, '/boot/efi') :- !.

mount_boot_efi(ED, RD) :-
	atom_concat(RD, '/boot/efi', DA),
	os_mkdir_p(DA),
	os_call2([mount, '-o', 'rw,noatime', ED, DA]).

validate_fs :-
	( has_root_part
	; tui_msgbox('ERROR: the mount point for the root filesystem (/) has not yet been configured.'),
	  fail
	), !,
	% https://arch-general.archlinux.narkive.com/MgF0tcbX/usr-is-not-mounted-this-is-not-supported
	( \+ has_usr_part
	; tui_msgbox('ERROR: /usr mount point has been configured but is not supported, please remove it to continue.'),
	  fail
	), !,
	% EFI
	( inst_setting(system(efi), _) ->
	  ( has_efi_system_part
	  ; tui_msgbox('ERROR: The EFI System Partition has not yet been configured, please create it as FAT32, mountpoint /boot/efi and at least with 100MB of size.'),
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

source_local :-
	inst_setting(source, local),
	!.

make_chroot_inst_pref(ARCH, P) :-
	atom_concat('XBPS_ARCH=', ARCH, XBPS_ARCH),
	P = [stdbuf, '-oL', env, XBPS_ARCH],
	true.

make_chroot_inst_pref_chroot(ARCH, P, RD) :-
	atom_concat('XBPS_ARCH=', ARCH, XBPS_ARCH),
	P = [stdbuf, '-oL', env, XBPS_ARCH, chroot, RD],
	true.

install_pkg(rootfs, RD) :-
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

	% dracut stuff.
	% tui_msgbox('dracut_conf'),
	dracut_conf(RD),

	% tui_msgbox('install_target_dep'),
	install_target_dep(RD),

	% Remove stuff
	tui_progressbox_safe(['xbps-remove', o(r, RD), '-y', 'base-voidstrap', '2>&1'], '', [title(' Remove base-voidstrap '), sz(max)]),

	tui_progressbox_safe(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),
	true.

install_pkg(net, RD) :-
	% mount required fs
	mount_chroot_filesystems(RD),

	os_mkdir_p([RD + '/var/db/xbps/keys', RD + '/usr/share']),
	os_call2([cp, '-a', '/usr/share/xbps.d', RD + '/usr/share/']),
	os_shell2([cp, '/var/db/xbps/keys/*.plist', RD + '/var/db/xbps/keys']),
	( inst_setting(bootloader, grub2) ->
	  os_mkdir_p(RD + '/boot/grub')
	; true
	),

	% dracut stuff.
	dracut_conf(RD),

	install_target_dep(RD),

	tui_progressbox_safe(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),

	true.

install_pkg(local, RD) :-
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

	% dracut stuff.
	dracut_conf(RD),
	% DL = [chroot, RD, dracut, '--no-hostonly', '--force', '2>&1'],
	DL = [chroot, RD, dracut, '--regenerate-all', '--hostonly', '--force', '2>&1'],
	tui_progressbox_safe(DL, '', [title(' Rebuilding initramfs for target '), sz(max)]),

	% tui_msgbox('install_target_dep'),
	install_target_dep(RD),

	% Remove stuff
	( HN = hrmpf
	; % Remove temporary packages from target
	  RL0 = [dialog, 'xtools-minimal'],
	  % Remove grub if we are using different bootloader.
	  ( inst_setting(bootloader, grub2) ->
	    RL1 = RL0
	  ; RL1 = ['grub-i386-efi', 'grub-x86_64-efi', grub| RL0]
	  ),
	  tui_progressbox_safe(['xbps-remove', o(r, RD), '-Ry', RL1, '2>&1'], '', [title(' xbps-remove '), sz([12, 80])])
	),
	true.

install_target_dep(RD) :-
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	( setof(D, target_dep(D), TPL) ->
	  install_deps(Pref, TPL)
	; true
	),
	true.

target_dep('base-system') :-
	\+ inst_setting(source, local),
	true.
target_dep(D) :-
	inst_setting(bootloader, B),
	target_dep_bootloader(B, D),
	true.
target_dep(zfs) :-
	uses_zfs,
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

mount_chroot_filesystems(RD) :-
	maplist(mount_chroot_filesystem_(RD), ['/sys', '/dev', '/proc']),
	true.

mount_chroot_filesystem_(RD, D) :-
	atom_concat(RD, D, D1),
	os_mkdir_p(D1),
	os_call2([mount, '--rbind', D, D1]),
	% os_call2([mount, '--make-rslave', D1]),
	true.

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

umount_dev(D) :-
	tui_msgbox(D),
	os_call2([umount, '--recursive', D]).
umount_dev(D) :-
	tui_msgbox2(['ERROR: filesystem unmounting has failed.', D], [sz([6, 40])]),
	fail.

wipe_disk(D) :-
	os_shell2_lines([wipefs, '--noheadings', D], L),
	( L = []
	; os_shell2([wipefs, '-a', D, '2>&1', '1>/dev/null']),
	  wipe_disk(D)
	),
	!.

wipe_dev_tree_list(L, PL) :-
	maplist(wipe_dev_tree(PL), L).

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
wipe_dev(part(_PARTUUID,_UUID), D, _CN) :- !,
	wipe_disk(D),
	true.
wipe_dev(disk, D, _CN) :- !,
	wipe_disk(D),
	true.
wipe_dev(lvm, D, _CN) :- !,
	wipe_disk(D),
	true.
wipe_dev(ET, D, _CN) :- !,
	format_to_atom(A, 'Unknown type "~w" of ~w.', [ET, D]),
	tui_msgbox(A),
	fail.

part2taglist(part_info(bd1([PD| _]), _FS, FSS, _Type), [PD, FSS]).

update_part_info(ONL, PLO) :-
	maplist(del_part_info(PLO), ONL),
	lx_list_part_info(PIL),
	maplist(ins_part_info(PIL, ONL), PLO),
	true.

% KL - list of partitions to keep.
del_part_info(KL, PD) :-
	memberchk(PD, KL), !.
del_part_info(_, PD) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	retractall(inst_setting(partition, part4(bd1([PD| _]), _, _, _SZ))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	retractall(inst_setting(fs, fs4(_, _, _, bd1([PD| _])))).

ins_part_info(_PIL, L, PD) :-
	memberchk(PD, L), !.
ins_part_info(PIL, _L, PD) :-
	member(part_info(BD1, FS, _FSS, _Type), PIL),
	BD1 = bd1([PD| _]),
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	assertz(inst_setting(partition, part4(BD1, linux, keep, _SZ))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	assertz(inst_setting(fs, fs4(FS, '', '', BD1))).

ensure_settings :-
	% S = [partition, bootloader_dev, keymap, network, source, hostname, locale, timezone, passwd, useraccount],
	S = [bootloader_dev, keymap, network, source, hostname, locale, timezone, passwd, useraccount],
	maplist(ensure_setting, S).

ensure_passwd :-
	\+ inst_setting_tmp(passwd(root), _),
	menu_password(root),
	fail.
ensure_passwd :-
	inst_setting(useraccount, user(U, _, _)),
	\+ inst_setting_tmp(passwd(U), _),
	menu_password(U),
	fail.
ensure_passwd :-
	inst_setting(template, gpt_luks1),
	U = '$_luks_$',
	\+ inst_setting_tmp(passwd(U), _),
	menu_password(U),
	fail.
ensure_passwd :-
	inst_setting(template, gpt_luks1_lvm),
	U = '$_luks_$',
	\+ inst_setting_tmp(passwd(U), _),
	menu_password(U),
	fail.
ensure_passwd.

ensure_setting(passwd) :- !,
	ensure_passwd.
ensure_setting(S) :-
	inst_setting(S, _), !.
ensure_setting(S) :-
	cmd_menu(S).

save_settings(S) :-
	inst_setting(N, V),
	portray_clause(S, inst_setting(N, V)),
	fail.
save_settings(_).

run_cmd(RD, prepare_to_install) :- !,
	setup_install,
	ensure_settings,
	validate_fs,
	clean_mnt(RD),
	!.
run_cmd(_RD, wipe(D, TL, PL)) :- !,
	( wipe_dev_tree_list(TL, PL)
	; tui_msgbox2([wipe_disk, D, has, failed]),
	  fail
	),
	!.
run_cmd(_RD, part(D, PL)) :- !,
	( part_sgdisk_pl(D, PL)
	; tui_msgbox('Disk partitioning has failed.'),
	  fail
	),
	!.
run_cmd(_RD, modprobe(FS)) :- !,
	( os_call2([modprobe, FS])
	; tui_msgbox2([modprobe, FS, has, failed]),
	  fail
	),
	!.
run_cmd(_RD, mkbd(BD, CMD)) :- !, % make block device.
	mkbd(BD, CMD),
	!.
run_cmd(RD, mkfs(FS, PD, Label)) :- !,
	mkfs(FS, PD, Label, RD),
	true.
run_cmd(RD, mount(FS, PD, MP)) :- !,
	mount_fs(FS, PD, MP, RD),
	true.
run_cmd(RD, install_pkg(IM)) :- !,
	install_pkg(IM, RD),

	% tui_msgbox('make_fstab'),
	make_fstab(RD),

	% tui_msgbox('set_keymap'),
	% set up keymap, locale, timezone, hostname, root passwd and user account.
	set_keymap(RD),

	% tui_msgbox('set_locale'),
	set_locale(RD),

	% tui_msgbox('set_timezone'),
	set_timezone(RD),

	% tui_msgbox('set_hostname'),
	set_hostname(RD),

	% tui_msgbox('set_rootpassword'),
	set_rootpassword(RD),

	% tui_msgbox('set_useraccount'),
	set_useraccount(RD),

	% tui_msgbox('cp /mnt/etc/skel/.[bix]* /mnt/root'),
	% Copy /etc/skel files for root.
	os_shell2([cp, RD + '/etc/skel/.[bix]*', RD + '/root']),

	% tui_msgbox('set_network'),
	% set network
	set_network(RD),

	% tui_msgbox('set_sudoers'),
	% set sudoers
	set_sudoers,

	% clean up polkit rule - it's only useful in live systems
	( IM = local ->
	  % tui_msgbox('rm -f /mnt/etc/polkit-1/rules.d/void-live.rules'),
	  os_rm_f(RD + '/etc/polkit-1/rules.d/void-live.rules')
	; true
	),

	% tui_msgbox('set_bootloader'),
	% install bootloader.
	set_bootloader(RD),

	% tui_msgbox('umount_filesystems'),
	% unmount all filesystems.
	umount_filesystems(RD),
	tui_msgbox('Void Linux has been installed successfully!', [sz([6, 40])]),
	os_call2([clear]),
	true.

run_cmdl(L) :-
	inst_setting(root_dir, RD),
	maplist(run_cmd(RD), L).

make_cmd(prepare_to_install).
make_cmd(wipe(D, TL, PL)) :-
	\+ inst_setting(template, manual),
	inst_setting(bootloader_dev, dev(D, PL, TL)),
	true.
make_cmd(part(D, SPL)) :-
	\+ inst_setting(template, manual),
	% Find all devices.
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(D0, inst_setting(partition, part4(bd1([_, D0]), _PT0, _F0, _SZ0)), DL0),
	sort(DL0, DL),
	% For each device
	member(D, DL),
	% Make partition list
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(PD, inst_setting(partition, part4(bd1([PD, D]), _, _, _SZ1)), PL0),
	sort(PL0, SPL),
	true.
make_cmd(modprobe(FS)) :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	findall(FS0, (inst_setting(fs, fs4(FS0, _Label, _MP, _BD1)), \+ memberchk(FS0, [swap, lvm, luks1])), FSL),
	sort(FSL, SFSL),
	member(FS, SFSL),
	true.
make_cmd(mkbd(BD, CMD)) :-
	inst_setting(bdev, bdev(BD, CMD)),
	true.
make_cmd(mkfs(FS, PD, Label)) :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(FS, Label, _MP, bd1([PD| _]))),
	true.
make_cmd(mount(FS, PD, MP)) :-
	get_mp_list(MPL),
	member(MP, MPL),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	inst_setting(fs, fs4(FS, _Label, MP, bd1([PD| _]))),
	true.
make_cmd(install_pkg(IM)) :-
	inst_setting(source, IM),
	true.

make_cmdl(CL) :-
	findall(C, make_cmd(C), CL),
	true.

run_install :-
	make_cmdl(CL),
	% write_to_atom(A, CL),
	% tui_msgbox(A, [sz(max)]),
	run_cmdl(CL),
	true.

do_install :-
	ux_user_root, !,
	setup_tui,
	menu_main,
	true.
do_install :-
	writenl('Installer must run as root.'),
	fail.

read_config(S) :-
	\+ at_end_of_stream(S),
	read_term(S, T, []),
	% writenl(T),
	assertz(T), !,
	read_config(S).
read_config(_).

load_config(F) :-
	open(F, read, S),
	read_config(S),
	close(S),
	true.

main :-
	argument_list(AL),
	( handle_cmd_args(AL) ->
	  ( inst_setting(config_file, CF), file_exists(CF) ->
		load_config(CF)
	  ; def_settings
	  ),
	  do_install,
	  os_call2([clear])
	; true
	),
	halt.
main :-
	writenl('Installer has failed.'),
	halt.
