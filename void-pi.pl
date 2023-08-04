#!/usr/bin/gprolog --consult-file
% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- initialization(main).

% library
:- include('lib/atom_common.pl').
:- include('lib/list_common.pl').
:- include('lib/os_common.pl').
:- include('lib/os_grub.pl').
:- include('lib/cli_common.pl').
:- include('lib/unix_common.pl').
:- include('lib/linux_common.pl').
:- include('lib/linux_dracut.pl').
:- include('lib/linux_luks.pl').
:- include('lib/zfs_common.pl').
:- include('lib/lvm_common.pl').
:- include('lib/tui_common.pl').
:- include('lib/tui_dialog.pl').

:- include('module/void_info.pl').
:- include('module/void_cmd_arg.pl').

% menu
:- include('module/void_menu_linux.pl').
:- include('module/void_menu_fs.pl').
:- include('module/void_menu.pl').

:- include('module/void_luks.pl').
:- include('module/void_dracut.pl').
:- include('module/void_net.pl').
:- include('module/void_fstab.pl').

% bootloaders
:- include('module/void_grub.pl').
:- include('module/void_limine.pl').
:- include('module/void_refind.pl').
:- include('module/void_efistub.pl').
:- include('module/void_syslinux.pl').
:- include('module/void_bootloader.pl').

% file systems
:- include('module/void_zfs.pl').
:- include('module/void_btrfs.pl').
:- include('module/void_fs.pl').

:- include('module/void_template.pl').
:- include('module/void_action.pl').

:- dynamic([inst_setting/2, inst_setting_tmp/2]).

% https://wiki.archlinux.org/title/Dm-crypt
% https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system
% https://wiki.archlinux.org/title/Dm-crypt/System_configuration
% Stacking LVM volumes: https://access.redhat.com/articles/2106521

def_settings :-
	setup_conf,
	setup_fs_template,
	B = grub2,
	assertz(inst_setting(keymap, us)),
	assertz(inst_setting(locale, 'en_US.UTF-8')),
	assertz(inst_setting(timezone, 'America/New_York')),
	assertz(inst_setting(useraccount, user(void, 'Void User', [wheel, floppy, audio, video, cdrom, optical, kvm, xbuilder]))),
	assertz(inst_setting(fs_info, info('/', ext4))),
	assertz(inst_setting(mbr_size, '1M')),
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
	assertz(inst_setting(luks, luks(crypt))),
	assertz(inst_setting(config_file, 'settings.pl')),

	enable_template(manual, B),

	true.

source_dependency_pkg(TL, Distro, DL) :-
	setof(D, source_dep(TL, Distro, D), DL).

source_dep(TL, Distro, D) :-
	% Collect all used filesystems.
	% fs4(Name, Label, MountPoint, [DevList])
	findall(FS, member(fs4(FS, _Label, _MP, _DL), TL), FSL0),
	sort(FSL0, FSL),
	% tui_msgbox2(PTL),
	member(F, FSL),
	source_dep_module(Distro, filesystem(F), DL),
	member(D, DL),
	true.
source_dep(_TL, Distro, D) :-
	inst_setting(template(T), _),
	source_dep_module(Distro, template(T), DL),
	member(D, DL),
	true.
source_dep(_TL, Distro, D) :-
	inst_setting(source, S),
	source_dep_module(Distro, inst_method(S), DL),
	member(D, DL),
	true.
source_dep(TL, Distro, D) :-
	memberchk(bootloader(B), TL),
	source_dep_module(Distro, bootloader(B), DL),
	member(D, DL),
	true.

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
	retractall(inst_setting(system(arch), _)),
	assertz(inst_setting(system(arch), ARCH)),
	true.

setup_sys_efi :-
	lx_sys_efi(EFI_TARGET),
	retractall(inst_setting(system(efi), _)),
	assertz(inst_setting(system(efi), EFI_TARGET)),
	!.
setup_sys_efi.

setup_sys_disk :-
	lx_list_dev7_disk(L),
	retractall(inst_setting(dev7, _)),
	assertz(inst_setting(dev7, available(L))),
	true.

setup_conf :-
	setup_sys_efi,
	setup_sys_arch,
	setup_sys_kernel,
	setup_sys_disk,
	true.

setup_install(TL) :-
	% Install dependencies
	host_name(HN),
	( source_dependency_pkg(TL, HN, D) ->
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

source_local :-
	inst_setting(source, local),
	!.

target_dep(_TL, 'base-system') :-
	\+ inst_setting(source, local),
	true.
target_dep(TL, D) :-
	memberchk(bootloader(B), TL),
	target_dep_bootloader(B, D),
	true.
target_dep(TL, zfs) :-
	uses_zfs(TL),
	true.

% for tracing.
% run_cmd(_TL, _RD, CMD) :-
% 	tui_msgbox_w(CMD),
% 	fail.
run_cmd(TL, RD, prepare_to_install) :- !,
	setup_install(TL),
	ensure_settings(TL),
	validate_fs(TL),
	umount_mnt(RD),
	!.
run_cmd(_TL, _RD, wipe_dev7(DEV7)) :- !,
	lx_dev7_to_dev3(DEV7, dev3(D, PL, TL1)),
	( wipe_dev_tree_list(TL1, PL)
	; tui_msgbox2([wipe_disk, D, has, failed]),
	  fail
	),
	!.
run_cmd(TL, _RD, ensure_lvm) :- !,
	ensure_lvm(TL),
	!.
run_cmd(TL, _RD, part(D, PL)) :- !,
	( part_sgdisk_pl(TL, D, PL)
	; tui_msgbox('Disk partitioning has failed.'),
	  fail
	),
	!.
run_cmd(_TL, _RD, modprobe(FS)) :- !,
	( os_call2([modprobe, FS])
	; tui_msgbox2([modprobe, FS, has, failed]),
	  fail
	),
	!.
run_cmd(_TL, _RD, mkbd(BD, CMD)) :- !, % make block device.
	mkbd(BD, CMD),
	!.
run_cmd(_TL, RD, mkfs(FS, DL, Label)) :- !,
	mkfs(FS, DL, Label, RD),
	true.
run_cmd(_TL, RD, mount(FS, PD, MP)) :- !,
	mount_fs(FS, PD, MP, RD),
	true.
run_cmd(TL, RD, install_pkg(IM)) :- !,
	install_pkg(TL, IM, RD),

	% tui_msgbox('make_fstab'),
	make_fstab(TL, RD),

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
	set_bootloader(TL, RD),

	% tui_msgbox('umount_filesystems'),
	% unmount all filesystems.
	umount_filesystems(RD),
	tui_msgbox('Void Linux has been installed successfully!', [sz([6, 40])]),
	os_call2([clear]),
	true.

run_cmdl(TL, L) :-
	inst_setting(root_dir, RD),
	maplist(run_cmd(TL, RD), L).

make_cmd(_TL, prepare_to_install).
make_cmd(_TL, wipe_dev7(DEV7)) :-
	\+ inst_setting(template(manual), _),
	inst_setting(dev7, used(UL)),
	member(DEV7, UL),
	true.
make_cmd(TL, ensure_lvm) :-
	memberchk(bdev(lvm, _), TL),
	true.
make_cmd(TL, part(D, SPL)) :-
	\+ inst_setting(template(manual), _),
	% Find all devices.
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	findall(D0, member(p4(_PT0, bd1([_, D0]), _F0, _SZ0), TL), DL0),
	sort(DL0, DL),
	% For each device
	member(D, DL),
	% Make partiotion list
	findall(PD, member(p4(_PT1, bd1([PD, D]), _CK1, _SZ1), TL), PL0),
	sort(PL0, SPL),
	true.
make_cmd(TL, modprobe(FS)) :-
	% fs4(Name, Label, MountPoint, [DevList])
	findall(FS0, (member(fs4(FS0, _Label, _MP, _DL), TL), \+ memberchk(FS0, [swap, lvm, luks1, bcachefs])), FSL),
	sort(FSL, SFSL),
	member(FS, SFSL),
	true.
make_cmd(TL, mkbd(BD, CMD)) :-
	member(bdev(BD, CMD), TL),
	true.
make_cmd(TL, mkfs(FS, DL, Label)) :-
	% fs4(Name, Label, MountPoint, [DevList])
	member(fs4(FS, Label, _MP, DL), TL),
	true.
make_cmd(TL, mount(FS, PD, MP)) :-
	get_mp_list(TL, MPL),
	member(MP, MPL),
	% fs4(Name, Label, MountPoint, [DevList])
	member(fs4(FS, _Label, MP, [PD| _]), TL),
	true.
make_cmd(_TL, install_pkg(IM)) :-
	inst_setting(source, IM),
	true.

run_install(TL) :-
	findall(C, make_cmd(TL, C), CL),
	run_cmdl(TL, CL),
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
