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
:- include('lib/linux_info.pl').
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
:- include('module/void_menu_dev.pl').
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
:- include('module/void_gummiboot.pl').
:- include('module/void_bootloader.pl').

% file systems
:- include('module/void_zfs.pl').
:- include('module/void_btrfs.pl').
:- include('module/void_fs.pl').

:- include('module/void_template.pl').
:- include('module/void_action.pl').
:- include('module/void_soft.pl').

:- dynamic([inst_setting/2, inst_setting_tmp/2]).

def_settings :-
	setup_conf,
	setup_fs_template,
	assertz(inst_setting(keymap, us)),
	assertz(inst_setting(locale, 'en_US.UTF-8')),
	assertz(inst_setting(timezone, 'America/New_York')),
	assertz(inst_setting(useraccount, user(void, 'Void User', [wheel, floppy, audio, video, cdrom, optical, kvm, xbuilder]))),
	assertz(inst_setting(mbr_size, '1M')),
	assertz(inst_setting(esp_size, '550M')),
	assertz(inst_setting(boot_size, '1G')),
	assertz(inst_setting(root_dir, '/mnt')),
	assertz(inst_setting(hostonly, no)),
	% fs_attr(Name, MountPoint, Bootloader)
	assertz(inst_setting(fs_attr(btrfs, '/', _), mount([rw, noatime, 'compress-force'=zstd, space_cache=v2, commit=120]))),
	assertz(inst_setting(fs_attr(vfat, '/boot', _), mount([rw, nosuid, nodev, noexec, relatime, fmask='0022', dmask='0022', codepage=437, iocharset='iso8859-1', shortname=mixed, utf8, errors='remount-ro']))),
	assertz(inst_setting(fs_attr(vfat, '/boot/efi', _), mount([rw, nosuid, nodev, noexec, relatime, fmask='0022', dmask='0022', codepage=437, iocharset='iso8859-1', shortname=mixed, utf8, errors='remount-ro']))),
	assertz(inst_setting(fs_attr(f2fs, '/', _), mount([rw, compress_algorithm=lz4, compress_chksum, atgc, gc_merge, lazytime]))),
	assertz(inst_setting(fs_attr(f2fs, '/', _), create([extra_attr, inode_checksum, sb_checksum, compression, encrypt]))),
	% https://wiki.syslinux.org/wiki/index.php?title=Filesystem#ext
	assertz(inst_setting(fs_attr(ext4, '/', syslinux), create(['^64bit']))),
	assertz(inst_setting(fs_attr(ext4, '/boot', syslinux), create(['^64bit']))),
	assertz(inst_setting(source, local)),
	assertz(inst_setting(hostname, voidpp)),
	assertz(inst_setting(lvm, lv(void, void, ''))),
	assertz(inst_setting(luks, luks(crypt))),
	assertz(inst_setting(config_file, 'settings.pl')),

	enable_template(manual, grub2),

	true.

source_dependency_pkg(TT, TL, Distro, DL) :-
	setof(D, source_dep(TT, TL, Distro, D), DL).

source_dep(_TT, TL, Distro, D) :-
	% Collect all used filesystems.
	% fs5(Name, Label, MountPoint, [DevList], create/keep)
	findall(FS, member(fs5(FS, _Label, _MP, _DL, _CK), TL), FSL0),
	sort(FSL0, FSL),
	% tui_msgbox2(PTL),
	member(F, FSL),
	source_dep_module(Distro, filesystem(F), DL),
	member(D, DL),
	true.
source_dep(TT, _TL, Distro, D) :-
	source_dep_module(Distro, template(TT), DL),
	member(D, DL),
	true.
source_dep(_TT, _TL, Distro, D) :-
	inst_setting(source, S),
	source_dep_module(Distro, inst_method(S), DL),
	member(D, DL),
	true.
source_dep(_TT, TL, Distro, D) :-
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

setup_sys_bios_efi :-
	setup_sys_efi,
	!.
setup_sys_bios_efi :-
	setup_sys_bios.

setup_sys_efi :-
	lx_sys_efi(EFI_TARGET),
	retractall(inst_setting(system(efi), _)),
	assertz(inst_setting(system(efi), EFI_TARGET)),
	true.

setup_sys_bios :-
	lx_sys_arch(ARCH),
	retractall(inst_setting(system(bios), _)),
	assertz(inst_setting(system(bios), ARCH)),
	true.

force_sys_efi :-
	lx_gen_efi(EFI_TARGET),
	retractall(inst_setting(system(efi), _)),
	assertz(inst_setting(system(efi), EFI_TARGET)),
	true.

setup_sys_disk :-
	lx_list_dev7_disk(L),
	retractall(inst_setting(dev7, _)),
	assertz(inst_setting(dev7, available(L))),
	true.

setup_conf :-
	setup_sys_bios_efi,
	setup_sys_arch,
	setup_sys_kernel,
	setup_sys_disk,
	true.

setup_install(TT, TL) :-
	% Install dependencies
	host_name(HN),
	( source_dependency_pkg(TT, TL, HN, D) ->
	  soft_install_deps([], D)
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
% run_cmd(_TT, _TL, _RD, CMD) :-
% 	tui_msgbox_w(CMD),
% 	fail.
run_cmd(TT, TL, RD, prepare_to_install) :- !,
	setup_install(TT, TL),
	ensure_settings(TT, TL),
	validate_fs(TL),
	umount_mnt(RD),
	!.
run_cmd(_TT, _TL, _RD, wipe_dev7(DEV7)) :- !,
	lx_dev7_to_dev3(DEV7, dev3(D, PL, TL1)),
	( wipe_dev_tree_list(TL1, PL)
	; tui_msgbox2([wipe_disk, D, has, failed]),
	  fail
	),
	!.
run_cmd(_TT, TL, _RD, ensure_lvm) :- !,
	ensure_lvm(TL),
	!.
run_cmd(_TT, TL, _RD, part(D, PL)) :- !,
	( part_sgdisk_pl(TL, D, PL)
	; tui_msgbox('Disk partitioning has failed.'),
	  fail
	),
	!.
run_cmd(_TT, _TL, _RD, modprobe(FS)) :- !,
	( os_call2([modprobe, FS])
	; tui_msgbox2([modprobe, FS, has, failed]),
	  fail
	),
	!.
run_cmd(_TT, _TL, _RD, mkbd(BD, CMD)) :- !, % make block device.
	mkbd(BD, CMD),
	!.
run_cmd(_TT, TL, RD, mkfs(FS, DL, Label)) :- !,
	format_to_atom(Title, ' Creating filesystem ~w ', [FS]),
	get_bootloader(TL, B),
	mkfs(FS, B, Title, DL, Label, RD),
	true.
run_cmd(_TT, _TL, RD, mount(FS, PD, MP)) :- !,
	mount_fs(FS, PD, MP, RD),
	true.
run_cmd(_TT, TL, RD, install_pkg(IM)) :- !,
	install_pkg(TL, IM, RD),
	make_fstab(TL, RD),
	% set up keymap, locale, timezone, hostname, root passwd and user account.
	set_keymap(RD),
	set_locale(RD),
	set_timezone(RD),
	set_hostname(RD),
	set_rootpassword(RD),
	set_useraccount(RD),
	% Copy /etc/skel files for root.
	os_shell2([cp, RD + '/etc/skel/.[bix]*', RD + '/root']),
	% set network
	set_network(RD),
	% set sudoers
	set_sudoers,
	% clean up polkit rule - it's only useful in live systems
	( IM \= local
	; os_rm_f(RD + '/etc/polkit-1/rules.d/void-live.rules')
	),
	% install software.
	soft_install(TL, RD),
	% install bootloader.
	install_bootloader(TL, RD),
	% unmount all filesystems.
	umount_filesystems(RD),
	tui_msgbox('Void Linux has been installed successfully!', [sz([6, 40])]),
	os_call2([clear]),
	true.

run_cmdl(TT, TL, L) :-
	inst_setting(root_dir, RD),
	maplist(run_cmd(TT, TL, RD), L).

make_cmd(_TT, _TL, prepare_to_install).
make_cmd(TT, _TL, wipe_dev7(DEV7)) :-
	TT \= manual,
	inst_setting(dev7, used(UL)),
	member(DEV7, UL),
	true.
make_cmd(_TT, TL, ensure_lvm) :-
	memberchk(bdev(lvm, _), TL),
	true.
make_cmd(TT, TL, part(D, SPL)) :-
	TT \= manual,
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
make_cmd(_TT, TL, modprobe(FS)) :-
	% fs5(Name, Label, MountPoint, [DevList], create/keep)
	findall(FS0, (member(fs5(FS0, _Label, _MP, _DL, _CK), TL), \+ memberchk(FS0, [swap, lvm, luks])), FSL),
	sort(FSL, SFSL),
	member(FS, SFSL),
	true.
make_cmd(_TT, TL, mkbd(BD, CMD)) :-
	member(bdev(BD, CMD), TL),
	true.
make_cmd(_TT, TL, mkfs(FS, DL, Label)) :-
	% fs5(Name, Label, MountPoint, [DevList], create/keep)
	member(fs5(FS, Label, _MP, DL, create), TL),
	true.
make_cmd(_TT, TL, mount(FS, PD, MP)) :-
	get_mp_list(TL, MPL),
	member(MP, MPL),
	% fs5(Name, Label, MountPoint, [DevList], create/keep)
	member(fs5(FS, _Label, MP, [PD| _], _CK), TL),
	true.
make_cmd(_TT, _TL, install_pkg(IM)) :-
	inst_setting(source, IM),
	true.

run_install(TT, TL) :-
	findall(C, make_cmd(TT, TL, C), CL),
	run_cmdl(TT, TL, CL),
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
