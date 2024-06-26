#!/usr/bin/gprolog --consult-file
% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- initialization(main).

% library
:- include('lib/atom_common.pl').
:- include('lib/list_common.pl').
:- include('lib/cli_common.pl').

:- include('lib/os_common.pl').
:- include('lib/os_grub.pl').

:- include('lib/unix_common.pl').
:- include('lib/linux_common.pl').
:- include('lib/linux_info.pl').
:- include('lib/linux_dracut.pl').

:- include('lib/tui_common.pl').
:- include('lib/tui_dialog.pl').
:- include('lib/tui_ext.pl').

:- include('lib/linux_luks.pl').
:- include('lib/linux_lvm.pl').

% file systems
:- include('lib/linux_prop_fs.pl').
:- include('lib/fs_proc.pl').
:- include('lib/fs_tmpfs.pl').
:- include('lib/fs_zfs.pl').
:- include('lib/fs_extfs.pl').
:- include('lib/fs_btrfs.pl').
:- include('lib/fs_f2fs.pl').
:- include('lib/fs_xfs.pl').
:- include('lib/fs_vfat.pl').
:- include('lib/fs_bcachefs.pl').
:- include('lib/fs_nilfs2.pl').
:- include('lib/fs_exfat.pl').

:- include('module/void_info.pl').
:- include('module/void_cmd_arg.pl').

% menu
:- include('module/void_menu_linux.pl').
:- include('module/void_menu_fs.pl').
:- include('module/void_menu_dev.pl').
:- include('module/void_menu_prop.pl').
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
:- include('module/void_zfsbootmenu.pl').
:- include('module/void_bootloader.pl').

% file systems
:- include('module/void_zfs.pl').
:- include('module/void_btrfs.pl').
:- include('module/void_fs.pl').

:- include('module/void_template.pl').
:- include('module/void_action.pl').
:- include('module/void_soft.pl').
:- include('module/void_state.pl').

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
	assertz(inst_setting(root_size, '20G')),
	assertz(inst_setting(root_dir, '/mnt')),
	assertz(inst_setting(hostonly, no)),

	% fs_attr(Name, MountPoint, Bootloader)
	assertz(inst_setting(fs_attr(btrfs, '/', _), mount([attr(mnt_indpn_feat, [atime=off]), attr(mnt_btrfs_opt, ['compress-force'=zstd, space_cache=v2, commit=60])]))),
	assertz(inst_setting(fs_attr(vfat, '/boot', _), mount([attr(mnt_indpn_feat, [atime=off, suid=off, dev=off, exec=off]), attr(mnt_vfat_feat, [utf8=on])]))),
	assertz(inst_setting(fs_attr(vfat, '/boot/efi', _), mount([attr(mnt_indpn_feat, [atime=off, suid=off, dev=off, exec=off]), attr(mnt_vfat_feat, [utf8=on])]))),
	assertz(inst_setting(fs_attr(f2fs, _, _), mount([attr(mnt_indpn_feat, [atime=off, lazytime=on]), attr(mnt_f2fs_opt, [compress_algorithm=lz4]), attr(mnt_f2fs_feat, [compress_chksum=on, atgc=on, gc_merge=on])]))),
	assertz(inst_setting(fs_attr(ext2, _, _), mount([attr(mnt_indpn_feat, [atime=off])]))),
	assertz(inst_setting(fs_attr(ext3, _, _), mount([attr(mnt_indpn_feat, [atime=off])]))),
	assertz(inst_setting(fs_attr(ext4, _, _), mount([attr(mnt_indpn_feat, [atime=off])]))),
	assertz(inst_setting(fs_attr(nilfs2, _, _), mount([attr(mnt_indpn_feat, [atime=off])]))),
	assertz(inst_setting(fs_attr(exfat, _, _), mount([attr(mnt_indpn_feat, [atime=off])]))),

	assertz(inst_setting(fs_attr(tmpfs, '/tmp', _), mount([attr(mnt_indpn_feat, [suid=off, dev=off, exec=off,strictatime=on]), attr(mnt_tmpfs_opt, [mode=1777])]))),
	assertz(inst_setting(fs_attr(proc, '/proc', _), mount([attr(mnt_indpn_feat, [suid=off, dev=off, exec=off]), attr(mnt_proc_opt, [hidepid=2,gid=proc])]))),
	assertz(inst_setting(fs_attr(efivarfs, _, _), mount([]))),
	assertz(inst_setting(fs_attr(swap, _, _), mount([]))),

	assertz(inst_setting(fs_attr(vfat, _, _), create([attr(vfat_rw, ['fat-size'=32])]))),
	assertz(inst_setting(fs_attr(f2fs, _, _), create([attr(f2fs_feat, [extra_attr=on, inode_checksum=on, sb_checksum=on, compression=on]), attr(f2fs_rw, [force=yes])]))),
	assertz(inst_setting(fs_attr(ext2, _, _), create([attr(extfs_rw, ['fs-type'=ext2, force=yes])]))),
	assertz(inst_setting(fs_attr(ext3, _, _), create([attr(extfs_rw, ['fs-type'=ext3, force=yes])]))),
	% https://wiki.syslinux.org/wiki/index.php?title=Filesystem#ext
	assertz(inst_setting(fs_attr(ext4, _, syslinux), create([attr(extfs_feat, ['64bit'=off]), attr(extfs_rw, ['fs-type'=ext4, force=yes])]))),
	assertz(inst_setting(fs_attr(ext4, _, _), create([attr(extfs_rw, ['fs-type'=ext4, force=yes])]))),
	assertz(inst_setting(fs_attr(btrfs, _, _), create([attr(btrfs_rw, [force=yes])]))),
	% grub2 does not support xfs filesystems with sparse inode allocation (https://bugzilla.redhat.com/show_bug.cgi?id=1575797)
	% https://patchwork.kernel.org/project/xfs/patch/ce584ae1-ee62-2dcb-9366-eb0a6df6e98e@sandeen.net/
	% assertz(inst_setting(fs_attr(xfs, _, grub2), create([attr(xfs_inode_rw, [sparse=0]), attr(xfs_rw, [force=yes])]))),
	assertz(inst_setting(fs_attr(xfs, _, _), create([attr(xfs_rw, [force=yes])]))),
	assertz(inst_setting(fs_attr(nilfs2, _, _), create([attr(nilfs2_rw, [force=yes])]))),
	assertz(inst_setting(fs_attr(exfat, _, _), create([attr(exfat_rw, [])]))),

	assertz(inst_setting(source, local)),
	assertz(inst_setting(hostname, voidpp)),
	assertz(inst_setting(lvm, lv(void, void, ''))),
	assertz(inst_setting(luks, luks(crypt))),
	assertz(inst_setting(config_file, 'settings.pl')),
	assertz(inst_setting(part(root), [])),

	set_template(manual, grub2),

	!.

source_dependency_pkg(TT, TL, Distro, DL) :-
	setof(D, source_dep(TT, TL, Distro, D), DL).

source_dep(_TT, TL, Distro, D) :-
	% Collect all used filesystems.
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	findall(FS, member(fs6(FS, _MP, _D, _COL, _MOL, _CK), TL), FSL0),
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

setup_sys_fs :-
	findall(FS, (setup_sys_fs_list(L), member(FS, L)), FSL),
	retractall(inst_setting(fs, _)),
	assertz(inst_setting(fs, available(FSL))),
	true.

setup_sys_fs_list([
	  btrfs
	, ext2
	, ext3
	, ext4
	, f2fs
	, swap
	, vfat
	, xfs
	, nilfs2
	, exfat
	]).
setup_sys_fs_list([FS]) :-
	member(FS, [bcachefs, zfs]),
	os_shell2([modprobe, FS, '2>/dev/null']).

setup_conf :-
	setup_sys_bios_efi,
	setup_sys_arch,
	setup_sys_kernel,
	setup_sys_disk,
	setup_sys_fs,
	true.

setup_install(TT, TL) :-
	% Install dependencies
	host_name(HN),
	( source_dependency_pkg(TT, TL, HN, D) ->
	  soft_install_deps([], D)
	; true
	),
	zfs_install(TL, _RD),
	true.

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

target_dep('base-system') :-
	\+ inst_setting(source, local),
	true.

target_soft(TL, D) :-
	memberchk(bootloader(B), TL),
	target_dep_bootloader(B, DL),
	member(D, DL),
	true.
target_soft(TL, zfs) :-
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
run_cmd(_TT, _TL, _RD, mkfs(FS, D, COL)) :- !,
	mkfs(FS, D, COL),
	true.
run_cmd(_TT, TL, RD, mkfs_multi(FS, DL, PTL, COL)) :- !,
	format_to_atom(Title, ' Creating filesystem ~w ', [FS]),
	mkfs_multi(FS, Title, TL, DL, PTL, COL, RD),
	true.
run_cmd(_TT, _TL, RD, mount_multi(FS, D, PTL)) :- !,
	mount_fs_multi(FS, D, PTL, RD),
	true.
run_cmd(_TT, _TL, RD, mount(FS, PD, MP)) :- !,
	mount_fs(FS, PD, MP, RD),
	true.
run_cmd(_TT, TL, RD, install_pkg(IM)) :- !,
	run_cmd_install_pkg(TL, IM, RD),
	true.

run_cmd_install_pkg(TL, IM, RD) :-
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
	set_network(RD),
	set_sudoers,
	% clean up polkit rule - it's only useful in live systems
	( IM \= local
	; os_rm_f(RD + '/etc/polkit-1/rules.d/void-live.rules')
	),
	soft_install(TL, RD),
	install_bootloader(TL, RD),
	umount_filesystems(RD),
	tui_msgbox('Void Linux has been installed successfully!', [sz([6, 40])]),
	os_call(clear, []),
	!.
run_cmd_install_pkg(_TL, _IM, _RD) :-
	tui_msgbox('Void Linux installation has FAILED!', [title(' ERROR '), sz([6, 40])]).

run_cmdl(TT, TL, L) :-
	inst_setting(root_dir, RD),
	maplist(run_cmd(TT, TL, RD), L).

make_cmd(_TT, _TL, prepare_to_install).
make_cmd(TT, TL, wipe_dev7(DEV7)) :-
	TT \= manual,
	st_used_d7(TL, UL),
	member(DEV7, UL),
	true.
make_cmd(_TT, TL, ensure_lvm) :-
	memberchk(bdev(lvm, _), TL),
	true.
make_cmd(TT, TL, part(D, SPL)) :-
	TT \= manual,
	% Find all devices.
	findall(D0, member(p4(_PT0, bd1([_, D0]), _F0, _SZ0), TL), DL0),
	sort(DL0, DL),
	% For each device
	member(D, DL),
	% Make partiotion list
	findall(PD, member(p4(_PT1, bd1([PD, D]), _CK1, _SZ1), TL), PL0),
	sort(PL0, SPL),
	true.
make_cmd(_TT, TL, modprobe(FS)) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	findall(FS0, (member(fs6(FS0, _MP, _D, _COL, _MOL, _CK), TL), \+ memberchk(FS0, [proc, tmpfs, swap, lvm, luks])), FSL),
	sort(FSL, SFSL),
	member(FS, SFSL),
	true.
make_cmd(_TT, TL, mkbd(BD, CMD)) :-
	member(bdev(BD, CMD), TL),
	true.
make_cmd(_TT, TL, mkfs_multi(FS, DL, PTL, COL)) :-
	member(fs5_multi(FS, COL, DL, PTL, create), TL),
	true.
make_cmd(_TT, TL, mkfs(FS, D, COL)) :-
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	member(fs6(FS, _MP, D, COL, _MOL, create), TL),
	true.
make_cmd(_TT, TL, mount_multi(FS, D, PTL)) :-
	member(fs5_multi(FS, _COL, [D|_], PTL, create), TL),
	true.
make_cmd(_TT, TL, mount(FS, PD, MP)) :-
	get_mp_list(TL, MPL),
	member(MP, MPL),
	% fs6(Name, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	member(fs6(FS, MP, PD, _COL, _MOL, _CK), TL),
	true.
make_cmd(_TT, _TL, install_pkg(IM)) :-
	inst_setting(source, IM),
	true.

run_install(TT, TL) :-
	findall(C, make_cmd(TT, TL, C), CL),
	run_cmdl(TT, TL, CL),
	true.

check_dialog :-
	os_shell2([dialog, '>/dev/null', '2>/dev/null']),
	!.
check_dialog :-
	writenl('dialog not found.'),
	writenl('Call "xbps-install dialog" to install it.'),
	fail.

do_install :-
	( ux_user_root
	; writenl('Installer must run as root.'),
	  fail
	), !,
	check_dialog,
	setup_tui,
	menu_main,
	true.

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

main0 :-
	( inst_setting(config_file, CF), file_exists(CF) ->
	  load_config(CF)
	; def_settings
	),
	do_install,
	os_call2([clear]).
main0 :-
	writenl('Installer has failed.').

main :-
	argument_list(AL),
	handle_cmd_args(AL),
	main0,
	halt.
main :-
	halt.

