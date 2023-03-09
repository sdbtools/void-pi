#!/usr/bin/gprolog --consult-file
% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- initialization(main).

:- include('lib/atom_common.pl').
:- include('lib/list_common.pl').
:- include('lib/os_common.pl').
:- include('lib/unix_common.pl').
:- include('lib/linux_common.pl').
:- include('lib/tui_common.pl').
:- include('lib/tui_dialog.pl').
:- include('lib/cli_common.pl').
:- include('lib/zfs_common.pl').
:- include('lib/lvm_common.pl').
:- include('void_info.pl').

:- dynamic([inst_setting/2, inst_setting_tmp/2]).

% https://wiki.archlinux.org/title/Btrfs
% Installing Void on a ZFS Root - https://docs.voidlinux.org/installation/guides/zfs.html

def_settings :-
	setup_conf,
	B = grub2,
	assertz(inst_setting(keymap, us)),
	assertz(inst_setting(locale, 'en_US.UTF-8')),
	assertz(inst_setting(timezone, 'America/New_York')),
	assertz(inst_setting(useraccount, user(void, 'Void User', [wheel, floppy, audio, video, cdrom, optical, kvm, xbuilder]))),
	assertz(inst_setting(bootloader, B)),
	assertz(inst_setting(root_fs, ext4)),
	assertz(inst_setting(root_dir, '/mnt')),
	assertz(inst_setting(fs(btrfs, '/'), mount([rw, noatime, 'compress-force=zstd', 'space_cache=v2', 'commit=120']))),
	assertz(inst_setting(fs(vfat, '/boot/efi'), mount([rw, nosuid, nodev, noexec, relatime, 'fmask=0022', 'dmask=0022', 'codepage=437', 'iocharset=iso8859-1', 'shortname=mixed', utf8, 'errors=remount-ro']))),
	assertz(inst_setting(source, local)),
	assertz(inst_setting(hostname, voidpp)),
	assertz(inst_setting(lvm, lv(void, void, ''))),

	on_enable(template(manual), B),

	true.

action_info(common_settings, 'Common Attrs', 'Common settings').
action_info(template, 'Template', 'Predefined configuration').
action_info(show, 'Show', 'Show current settings').
action_info(filesystem, 'Filesystem', 'Configure filesystems and mount points').
action_info(part_manually, 'Partition', 'Manually partition disk(s)').
action_info(part_select, 'Select Part', 'Select partition(s) to use').
action_info(keymap, 'Keyboard', 'Set system keyboard').
action_info(locale, 'Locale', 'Set system locale').
action_info(timezone, 'Timezone', 'Set system time zone').
action_info(useraccount, 'User Account', 'Set primary user name and password').
action_info(lvm_info, 'LVM Info', 'Set LVM info').
action_info(bootloader_dev, 'Bootloader Dev', 'Set disk to install bootloader').
action_info(bootloader, 'Bootloader', 'Set bootloader application').
action_info(network, 'Network', 'Set up the network').
action_info(source, 'Source', 'Set source installation').
action_info(hostname, 'Hostname', 'Set system hostname').
action_info(save, extra, 'Save settings on disk').
action_info(install, 'Install', 'Start installation').
action_info(exit, cancel, 'Exit installation').
action_info(root_passwd, 'Root Password', 'Set system root password').
action_info(user_passwd, 'User Password', 'Set user password').
action_info(btrfs_opt, 'Btrfs', 'Btrfs as root options').
action_info(partition, 'Partitions', 'Partitions to use during installation').
action_info(root_fs, 'Root FS', 'Root File System').

dialog_msg(menu, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(list, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and ENTER or SPACE to select.') :- !.
dialog_msg(radiolist, 'Use UP and DOWN arrows to navigate menus. Use TAB to switch between buttons and SPACE to select.') :- !.
dialog_msg(form, 'Use UP and DOWN arrows (or Ctrl/N, Ctrl/P) to move between fields. Use TAB to move between windows.') :- !.

source_dependency_pkg(Distro, DL) :-
	setof(D, source_dep(Distro, D), DL).

source_dep(Distro, D) :-
	% Collect all used filesystems.
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	findall(FS, inst_setting(partition, part(_D, _S1, _P1, _PT, FS, _Label, _MP, _CK, _SZ)), FSL0),
	sort(FSL0, FSL),
	% tui_msgbox2(PTL, []),
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

source_dep_module('void-live', filesystem(zfs), [zfs]).
source_dep_module('void-live', template(_), [gptfdisk]).
source_dep_module('void-live', inst_method(rootfs), [xz]).

inst_method_tag(1, local, 'Local', 'Packages from live ISO image').
inst_method_tag(2, net, 'Network', 'Base system only, downloaded from official repository').
% inst_method_tag(3, rootfs, 'Rootfs', 'Packages from rootfs ISO image').

on_inst_method(net) :-
	\+ inst_setting(network, _),
	menu_network, !.
on_inst_method(_).

select_pkg_inst_method :-
	dialog_msg(radiolist, RADIOLABEL),
	findall([Tag, Label], inst_method_tag(_, _, Tag, Label), L1),
	inst_setting(source, OM),
	inst_method_tag(_, OM, OMT, _),
	tui_radiolist_tag2(L1, OMT, RADIOLABEL, [title(' Select installation source ')], MT),
	inst_method_tag(_, A, MT, _),

	on_inst_method(A),
	retractall(inst_setting(source, _)),
	assertz(inst_setting(source, A)).

install_deps(_, []) :- !.
install_deps(Pref, D) :-
	% tui_progressbox([Pref, 'xbps-install', '-Suy', xbps, '2>&1'], '', [title(' Update xbps '), sz([12, 80])]),
	% tui_progressbox([Pref, 'xbps-install', '-S', '2>&1'], '', [title(' Synchronize remote repository index files '), sz([6, 80])]),
	% tui_progressbox([Pref, 'xbps-install', '-y', D, '2>&1'], '', [title(' Install dependencies '), sz(max)]).
	tui_progressbox([Pref, 'xbps-install', '-SyU', D, '2>&1'], '', [title(' Install dependencies '), sz(max)]).

install_deps_chroot(_, [], _) :- !.
install_deps_chroot(Pref, D, RD) :-
	% tui_programbox([Pref, 'xbps-install', o(r, RD), '-SyU', D, '2>&1'], '', [title(' Installing base system packages... '), sz(max)]).
	tui_progressbox([Pref, 'xbps-install', o(r, RD), '-SyU', D, '2>&1'], '', [title(' Installing base system packages... '), sz(max)]).

setup_tui :-
	retractall(tui_def_args_all(_)),
	asserta(tui_def_args_all([sz(auto), clear, no-shadow])).

setup_sys_arch :-
	% x86_64 | x86_64-musl ...
	% os_shell_line('xbps-uhelper arch', ARCH),
	os_shell_line('uname -m', A0),
	( A0 = x86_64, file_exists('/lib/ld-musl-x86_64.so.1') ->
	  ARCH = 'x86_64-musl'
	; ARCH = A0
	),
	% tui_msgbox(ARCH, []),
	retractall(inst_setting(system(arch), _)),
	assertz(inst_setting(system(arch), ARCH)),
	true.

setup_sys_efi :-
	file_exists('/sys/firmware/efi/systab'),
	os_shell2_number([cat, '/sys/firmware/efi/fw_platform_size'], N),
	( N = 64 ->
	  EFI_TARGET='x86_64-efi'
	; EFI_TARGET='i386-efi'
	),
	retractall(inst_setting(system(efi), _)),
	assertz(inst_setting(system(efi), EFI_TARGET)),
	!.
setup_sys_efi.

setup_conf :-
	setup_sys_efi,
	setup_sys_arch,
	true.

setup_install :-
	% Install dependencies
	host_name(HN),
	( source_dependency_pkg(HN, D) ->
	  % tui_msgbox2(D, []),
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
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, _P, PD, PT, _FD, _Label, _MP, _F, SZ)),
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
	findall(P, inst_setting(partition, part(D, P, _, _, _, _, _, _, _SZ)), PL),
	sort(PL, SPL),
	part_sgdisk_pl(D, SPL),
	true.

part_dev(D) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	findall(PD, inst_setting(partition, part(D, _P, PD, _, _, _, _, _, _SZ)), PL),
	sort(PL, SPL),
	make_par(D, SPL),
	true.

% PL - partition list.
part_sgdisk_pl(D, PL) :-
	% tui_msgbox2([part_sgdisk_pl, PL], []),
	make_sgdisk_par(PL, 1, SGPL),
	format_to_atom(MA, ' Partitioning ~w ', [D]),
	tui_progressbox([sgdisk, '--zap-all', '--clear', '--mbrtogpt', SGPL, D], '', [title(MA), sz([6, 60])]),
	true.

% PL - partition list.
part_lvm_lv(PL) :-
	maplist(part_lvm_lv_, PL),
	true.

part_lvm_lv_(PD) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, _P, PD, _PT, _FS, Label, _MP, _F, SZ)),
	lvm_lvcreate_unsafe(void, Label, SZ),
	true.

% DO NOT delete ...
% PL - partition list.
% make_par(D, PL) :-
% 	inst_setting(vol_mgr, lvm),
% 	split_pl(PL, SGL, LVML),
% 	LVML = [PV|_], !,
% 	% LVML is not empty.
% 	append(SGL, [PV], SGL1),
% 	part_sgdisk_pl(D, SGL1),
% 	% Create one PV instead of a bunch of partitions.
% 	( lvm_pvcreate_unsafe(PV), !
% 	; tui_msgbox('lvm_pvcreate has failed', []),
% 	  fail
% 	),
% 	( lvm_vgcreate(void, [PV]), !
% 	; tui_msgbox('lvm_vgcreate has failed', []),
% 	  fail
% 	),
% 	part_lvm_lv(LVML),
% 	true.
make_par(D, PL) :-
	part_sgdisk_pl(D, PL),
	true.

split_pl([PD|T], [PD|T1], T2) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, _P, PD, PT, FS, _Label, MP, _F, _SZ)),
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

disk_partitioning :-
	inst_setting(template, manual),
	!.
disk_partitioning :-
	% setof(D, P^PD^PT^FD^Label^MP^F^SZ^inst_setting(partition, part(D, P, PD, PT, FD, Label, MP, F, SZ)), DL),
	findall(D, inst_setting(partition, part(D,_P,_PD,_PT,_FS,_Label,_MP,_F,_SZ)), DL0),
	sort(DL0, DL),
	maplist(part_dev, DL),
	true.

mkfs(RD) :-
	% tui_msgbox('mkfs', []),
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_, P, PD, _, FS, Label, _MP, create, _SZ)),
	% tui_msgbox2([partition, FS, P, PD, Label], []),
	% modprobe
	( FS = swap; FS = lvm ->
	  true
	; ( os_call2([modprobe, FS]) ->
	    true
	  ; tui_msgbox2([modprobe, FS, has, failed], []),
	    fail
	  ),
	  true
	),
	% wipe_disk(PD), % Doesn't seem to be needed anymore.
	mkfs(FS, P, PD, Label, RD),
	fail.
mkfs(_).

mkfs(zfs, P, _PD, Label, RD) :- !,
	% tui_msgbox2([mkfs, zfs, P, Label], []),
	add_dquote(Label, _LQ),
	% lx_get_dev_disk_partuuid(PD, PID),
	lx_get_dev_id(P, PID),

	% Create a ZFS pool
	% tui_programbox([
	tui_progressbox([
		zpool,
		create,
		'-f',
		'-o', 'ashift=12',
		'-o', 'autotrim=on',
		'-O', 'compression=lz4',
		'-O', 'acltype=posixacl',
		'-O', 'xattr=sa',
		'-O', 'relatime=on',
		% '-O', 'mountpoint=/',
		'-m', none,
		'zroot',
		PID,
		'2>&1'
		], 'Creating filesystem zfs', [sz([12, 80])]
	),
	% export and re-import the pool with a temporary, alternate root path
	os_shell2([zpool, export, zroot]),
	os_shell2([zpool, import, '-N', o('R', RD), zroot]),
	create_zfs_dataset,

	% Mount the ZFS hierarchy
	os_shell2([zfs, mount, 'zroot/ROOT/void', '2>&1']),
	os_shell2([zfs, mount, '-a', '2>&1']),

	% % record the current pool configuration in a cache file that Void will use to avoid walking the entire device hierarchy to identify importable pools.
	% os_shell2([mkdir, '-p', '/mnt/etc/zfs']),
	% os_shell2([zpool, set, 'cachefile=/mnt/etc/zfs/zpool.cache', zroot]),
	!.
mkfs(lvm, _P, PD, Label, RD) :- !,
	add_dquote(Label, LQ),
	( lvm_pvcreate_unsafe(PD), !
	; tui_msgbox('lvm_pvcreate has failed', []),
	  fail
	),
	( lvm_vgcreate(LQ, [PD]), !
	; tui_msgbox('lvm_vgcreate has failed', []),
	  fail
	),
	inst_setting(lvm, lv(VG, LV, SZ)),
	inst_setting(root_fs, FS),
	lvm_lvcreate_unsafe(Label, LV, SZ),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	mkfs(FS, _P, LVM_PD, Label, RD),
	!.
mkfs(btrfs, _P, PD, Label, RD) :- !,
	add_dquote(Label, LQ),
	tui_progressbox(['mkfs.btrfs', '-f', '-L', LQ, PD, '2>&1'], 'Creating filesystem btrfs', [sz([12, 80])]),
	% tui_programbox(['mkfs.btrfs', '-f', '-L', LQ, PD, '2>&1'], 'Creating filesystem btrfs', [sz([12, 80])]),
	create_btrfs_subv(PD, RD).
mkfs(ext2, _P, PD, _Label, _RD) :- !,
	tui_progressbox(['mke2fs', '-F', PD, '2>&1'], 'Creating filesystem ext2', [sz([12, 80])]),
	true.
mkfs(ext3, _P, PD, _Label, _RD) :- !,
	tui_progressbox(['mke2fs', '-F', '-j', PD, '2>&1'], 'Creating filesystem ext3', [sz([12, 80])]),
	true.
mkfs(ext4, _P, PD, _Label, _RD) :- !,
	tui_progressbox(['mke2fs', '-F', '-t', 'ext4', PD, '2>&1'], 'Creating filesystem ext4', [sz([12, 80])]),
	% tui_programbox(['mke2fs', '-F', '-t', 'ext4', PD, '2>&1'], 'Creating filesystem ext4', [sz([24, 80])]),
	true.
mkfs(f2fs, _P, PD, _Label, _RD) :- !,
	tui_progressbox(['mkfs.f2fs', '-f', PD, '2>&1'], 'Creating filesystem f2fs', [sz([12, 80])]),
	true.
mkfs(vfat, _P, PD, Label, _RD) :- !,
	upper(Label, UL),
	add_dquote(UL, LQ),
	tui_progressbox(['mkfs.vfat', '-F', '32', '-n', LQ, PD, '2>&1'], 'Creating filesystem vfat', [sz([12, 80])]),
	% tui_programbox(['mkfs.vfat', '-F', '32', '-n', LQ, PD, '2>&1'], 'Creating filesystem vfat', [sz([12, 80])]),
	true.
mkfs(xfs, _P, PD, _Label, _RD) :- !,
	tui_progressbox(['mkfs.xfs', '-f', '-i', 'sparse=0', PD, '2>&1'], 'Creating filesystem xfs', [sz([12, 80])]),
	true.
mkfs(swap, _P, PD, _Label, _RD) :- !,
	os_shell2_rc([swapoff, PD, '>/dev/null', '2>&1'], _),
	( os_shell2l([mkswap, PD, '2>&1']) ->
	  true
	; tui_msgbox('ERROR: failed to create swap', []),
	  fail
	),
	( os_shell2l([swapon, PD, '2>&1']) ->
	  true
	; tui_msgbox('ERROR: failed to activate swap', []),
	  fail
	),
	true.
mkfs(FS, _P, _, _, _, _RD) :- !,
	tui_msgbox2(['Unknown filesystem', FS], []),
	fail.

create_btrfs_subv(RD) :-
	inst_setting(btrfs, subv(S, _, _, _)),
	format_to_atom(SA, '~w/~w', [RD, S]),
	os_shell2l([btrfs, subvolume, create, SA]),
	fail.
create_btrfs_subv(RD) :-
	% set nodatacow attr.
	inst_setting(btrfs, subv(S, _, _, nodatacow)),
	format_to_atom(SA, '~w/~w', [RD, S]),
	os_shell2l([chattr, '-R', '+C', SA]),
	fail.
create_btrfs_subv(_RD).

create_btrfs_subv(D, RD) :-
	( inst_setting(fs(btrfs, '/'), mount(AL)) ->
	  true
	; AL = [rw, noatime]
	),
	join_atoms(AL, ',', AA),
	os_call2([mount, '-o', AA, D, RD]),
	create_btrfs_subv(RD),
	os_call2([umount, RD]),
	true.

make_zfs_pool_cmd(MP, DS, AL, [zfs, create, '-o', MPA|T]) :-
	atom_concat('mountpoint=', MP, MPA),
	make_zfs_pool_cmd_(DS, AL, T),
	true.

make_zfs_pool_cmd_(DS, [H|T], ['-o', H|T1]) :-
	make_zfs_pool_cmd_(DS, T, T1),
	true.
make_zfs_pool_cmd_(DS, [], [DSA]) :-
	atom_concat('zroot/', DS, DSA),
	true.

create_zfs_dataset :-
	% Create initial filesystems
	% dataset(Mountpoint, dataset name, attrs)
	inst_setting(zfs, dataset(DS, MP, AL)),
	make_zfs_pool_cmd(MP, DS, AL, CMD),
	% writenl(CMD),
	os_shell2(CMD),
	% tui_msgbox2(CMD, []),
	fail.
create_zfs_dataset :-
	true.

mount_btrfs(BD, RD) :-
	inst_setting(btrfs, subv(SV, mp(MP), OL, _)),
	atom_concat(RD, MP, DA),
	atom_concat('subvol=/', SV, SO),
	O = [SO|OL],
	join_atoms(O, ',', OA),
	os_mkdir_p(DA),
	os_call2([mount, '-o', OA, BD, DA]),
	fail.
mount_btrfs(_, _).

mount_boot_efi(ED, RD) :-
	atom_concat(RD, '/boot/efi', DA),
	os_mkdir_p(DA),
	os_call2([mount, '-o', 'rw,noatime', ED, DA]).

% Get list of mounting points in order in which they should be mounted (except of swap).
get_mp_list(MPL1) :-
	% Ignore swap partition
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	findall(MP, (inst_setting(partition, part(_D, _S1, _P1, _PT, FS, _Label, MP, _CK, _SZ)), FS \= swap), MPL0),
	sort(MPL0, MPL1),
	true.

% Similar to get_mp_list but including swap.
get_fstab_list(MPL1) :-
	% Ignore BIOS boot partition
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	findall(fstab(MP, FS, PD), (inst_setting(partition, part(_D, _P, PD, PT, FS, _Label, MP, _CK, _SZ)), PT \= bios_boot), MPL0),
	sort(MPL0, MPL1),
	true.

mount_fs(RD) :-
	get_mp_list(MPL),
	maplist(mount_mp(RD), MPL),
	true.

mount_mp(RD, MP) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_, _RP, PD, _, FS, _Label, MP, _, _SZ)),
	mount_fs(FS, PD, MP, RD),
	true.

% Ignore swap partition.
mount_fs(swap, _D, _MP, _RD) :-
	!.
% Ignore empty mount point.
mount_fs(_FS, _D, '', _RD) :-
	!.
mount_fs(btrfs, D, _MP, RD) :-
	mount_btrfs(D, RD),
	!.
mount_fs(zfs, _D, _MP, _RD) :-
	% tui_msgbox2([before, zfs, mount, 'zroot/ROOT/void'], []),
	% % Mount the ZFS hierarchy
	% % os_shell2([zfs, mount, 'zroot/ROOT/void']),
	% tui_programbox([zfs, mount, 'zroot/ROOT/void', '2>&1'], '', [title(' zfs mount zroot/ROOT/void '), sz([12, 80])]),
	% tui_msgbox2([after, zfs, mount, 'zroot/ROOT/void'], []),
	% os_shell2([zfs, mount, '-a']),
	% tui_msgbox2([after, zfs, mount, '-a'], []),
	!.
mount_fs(lvm, _D, MP, RD) :-
	inst_setting(lvm, lv(VG, LV, _SZ)),
	inst_setting(root_fs, FS),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	mount_fs(FS, LVM_PD, MP, RD),
	!.
mount_fs(FS, D, MP, RD) :-
	memberchk(FS, [vfat, ext2, ext3, ext4, f2fs, xfs]),
	atom_concat(RD, MP, MP1),
	os_mkdir_p(MP1),
	% tui_msgbox2([after, FS, mkdir, '-p', MP1], [sz([6, 40])]),
	os_shell2([mount, '-t', FS, '-o', 'rw,noatime', D, MP1, '2>&1']),
	% tui_msgbox2([after, FS, mount, '-t', FS, '-o', 'rw,noatime', D, MP1], [sz([6, 40])]),
	!.
mount_fs(FS, D, MP, _RD) :-
	tui_msgbox2(['mount_fs has failed.', [FS, D, MP]], [sz([6, 40])]),
	fail.

% DO NOT delete
% post_setup_fs :-
% 	get_mp_list(MPL),
% 	maplist(setup_mp, MPL),
% 	true.

% setup_mp(MP) :-
% 	inst_setting(partition, part(_, _RP, D, _, FS, _, MP, _, _SZ)),
% 	setup_mp(FS, D, MP),
% 	true.

% setup_mp(btrfs, _D, '/') :- !,
% 	( inst_setting(fs(btrfs, '/'), snap_dir(SD)) ->
% 	  os_shell2([mkdir, '-p', SD, '2>&1'])
% 	; true
% 	),
% 	true.
% setup_mp(zfs, _D, _MP) :- !,Device major number
% 	% zfs requires hostid.
% 	% os_shell2([cp, '/etc/hostid', '/mnt/etc/hostid']),
% 	tui_programbox([cp, '/etc/hostid', '/mnt/etc/hostid'], '', [title(' copy hostid '), sz([12, 80])]),
% 	% record the current pool configuration in a cache file that Void will use to avoid walking the entire device hierarchy to identify importable pools.
% 	os_shell2([mkdir, '-p', '/mnt/etc/zfs']),
% 	os_shell2([zpool, set, 'cachefile=/mnt/etc/zfs/zpool.cache', zroot]),
% 	true.
setup_mp(_FS, _D, _MP).

clean_mnt(RD) :-
	os_shell2([umount, oo(recursive), RD, '2>/dev/nul']),
	fail.
clean_mnt(_RD) :-
	% LVM
	inst_setting(bootloader_dev, D),
	lvm_pvs(PVL),
	findall(pv(PV,VG), (member(pv(PV,VG), PVL), atom_concat(D, _, PV)), VGL),
	maplist(clean_mnt_lvm_, VGL),
	fail.
clean_mnt(RD) :-
	% uses_zfs,
	zpool_list(L),
	memberchk(zp(PN,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,_A10,RD), L),
	tui_progressbox([zpool, destroy, '-f', PN, '2>&1'], '', [title(' zpool destroy '), sz([6, 40])]),
	fail.
% lsblk -n -o KNAME,PKNAME /dev/sda1
% lsblk -J -o NAME,TYPE
clean_mnt(_).

clean_mnt_lvm_(pv(PV,VG)) :-
	lvm_vgremove_unsafe(VG, PV).

validate_fs :-
	( root_part(_) ->
	  true
	; tui_msgbox('ERROR: the mount point for the root filesystem (/) has not yet been configured.', []),
	  fail
	),
	% https://arch-general.archlinux.narkive.com/MgF0tcbX/usr-is-not-mounted-this-is-not-supported
	( usr_part(_) ->
	  tui_msgbox('ERROR: /usr mount point has been configured but is not supported, please remove it to continue.', []),
	  fail
	; true
	),
	% EFI
	( inst_setting(system(efi), _) ->
	  ( efi_system_part(_) -> true
	  ; tui_msgbox('ERROR: The EFI System Partition has not yet been configured, please create it as FAT32, mountpoint /boot/efi and at least with 100MB of size.', []),
	    fail
	  )
	; true
	),
	true.

root_part(P) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, P, _PD, _PT, _FS, _Label, '/', _CK, _SZ)), !,
	true.

efi_system_part(P) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, P, _PD, _PT, vfat, _Label, '/boot/efi', _CK, _SZ)), !,
	true.

usr_part(P) :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, P, _PD, _PT, _FS, _Label, '/usr', _CK, _SZ)), !,
	true.

install_zfs(RD) :-
	uses_zfs,
	lx_gen_hostid(''),
	\+ host_name(hrmpf),
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	install_deps(Pref, [zfs]),
	!.
install_zfs(_).

run_mkfs(RD) :-
	install_zfs(RD),
	mkfs(RD),
	mount_fs(RD),
	% post_setup_fs,
	!.
run_mkfs(_) :-
	tui_msgbox('mkfs has failed.', []),
	fail.

parse_rootfs_name(N, ARCH) :-
	split_rootfs_file_name(N, NL),
	parse_rootfs_name_(NL, ARCH),
	!.
parse_rootfs_name(N, _NL) :-
	tui_msgbox2(['Invalid rootfs file name:', N], []),
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

arch2grub(_, grub) :-
	\+ inst_setting(system(efi), _), !.
arch2grub('x86_64', 'grub-x86_64-efi') :- !.
arch2grub('x86_64-musl', 'grub-x86_64-efi') :- !.
arch2grub('i686', 'grub') :- !.
arch2grub('armv6l', 'grub-arm-efi') :- !. % ???
arch2grub('armv7l', 'grub-arm-efi') :- !. % ???
arch2grub('aarch64', 'grub-arm64-efi') :- !.
% arch2grub('ia32', 'grub-i386-efi') :- !.

uses_zfs :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, _P, _PD, _PT, zfs, _Label, _MP, _CK, _SZ)),
	!.

source_local :-
	inst_setting(source, local),
	!.

dracut_zfs(RD) :-
	atom_concat(RD, '/etc/dracut.conf.d', DCD),
	os_mkdir_p(DCD),
	atom_concat(DCD, '/zol.conf', ZCF),
	open(ZCF, write, S),
	write(S, 'nofsck="yes"'), nl(S),
	write(S, 'add_dracutmodules+=" zfs "'), nl(S),
	write(S, 'omit_dracutmodules+=" btrfs resume "'), nl(S),
	close(S),
	true.

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
	tui_progressbox([tar, xvf, N, o('C', RD)], '', [title(' Extracting rootfs '), sz(max)]),

	% mount required fs
	mount_chroot_filesystems(RD),
	% Copy the DNS configuration into the new root so that XBPS can still download new packages inside the chroot.
	os_mkdir_p(RD + '/etc'), os_call2([cp, '-L', '/etc/resolv.conf', RD + '/etc/']),

	( uses_zfs ->
	  dracut_zfs(RD)
	; true
	),

	install_target_dep(RD),

	tui_progressbox(['xbps-remove', o(r, RD), '-y', 'base-voidstrap', '2>&1'], '', [title(' Remove base-voidstrap '), sz(max)]),

	tui_progressbox(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),
	true.

install_pkg(net, RD) :-
	% mount required fs
	mount_chroot_filesystems(RD),

	% tui_msgbox('OK', []),
	os_mkdir_p([RD + '/var/db/xbps/keys', RD + '/usr/share']),
	os_call2([cp, '-a', '/usr/share/xbps.d', RD + '/usr/share/']),
	os_shell2([cp, '/var/db/xbps/keys/*.plist', RD + '/var/db/xbps/keys']),
	( inst_setting(bootloader, grub2) ->
	  os_mkdir_p(RD + '/boot/grub')
	; true
	),

	( uses_zfs ->
	  dracut_zfs(RD)
	; true
	),

	install_target_dep(RD),

	tui_progressbox(['xbps-reconfigure', o(r, RD), '-f', 'base-files', '2>&1'], '', [title(' Reconfigure base-files '), sz(max)]),
	tui_progressbox([chroot, RD, 'xbps-reconfigure', '-a', '2>&1'], '', [title(' Reconfigure all '), sz(max)]),

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
	  tui_progressbox([userdel, o('R', RD), '-r', anon, '2>&1'], '', [title(' Remove user anon '), sz([6, 60])])
	; true
	),

	% mount required fs
	mount_chroot_filesystems(RD),

	( uses_zfs ->
	  dracut_zfs(RD)
	; true
	),

	DL = [chroot, RD, dracut, '--no-hostonly', '--add-drivers', '"ahci"', '--force', '2>&1'],
	tui_progressbox(DL, '', [title(' Rebuilding initramfs for target '), sz(max)]),

	install_target_dep(RD),

	( HN = hrmpf ->
	  true
	; % Remove temporary packages from target
	  RL0 = [dialog, 'xtools-minimal'],
	  % Remove grub if we are using different bootloader.
	  ( inst_setting(bootloader, grub2) ->
	    RL1 = RL0
	  ; RL1 = ['grub-i386-efi', 'grub-x86_64-efi', grub| RL0]
	  ),
	  tui_progressbox(['xbps-remove', o(r, RD), '-Ry', RL1, '2>&1'], '', [title(' xbps-remove '), sz([12, 80])])
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

target_dep_bootloader(limine, limine) :- !.
target_dep_bootloader(rEFInd, refind) :- !.
target_dep_bootloader(grub2, GRUB) :-
	\+ inst_setting(source, local),
	inst_setting(system(arch), ARCH),
	arch2grub(ARCH, GRUB), !.

% enable text console for grub if chosen
set_grub_text_console(RD) :-
	os_call2([sed, '-i', RD + '/etc/default/grub', '-e', 's|#\\(GRUB_TERMINAL_INPUT\\).*|\\1=console|', '-e', 's|#\\(GRUB_TERMINAL_OUTPUT\\).*|\\1=console|']),
	true.

set_keymap(RD) :-
	inst_setting(keymap, KM),
	lx_set_keymap(RD, KM),
	!.
set_keymap(_) :-
	tui_msgbox('Setting of keymap has failed.', []),
	fail.

set_locale(_RD) :-
	inst_setting(system(arch), 'x86_64-musl'),
	!.
set_locale(RD) :-
	inst_setting(locale, LC),
	lx_set_locale(RD, LC),
	!.
set_locale(_RD) :-
	tui_msgbox('Setting of locale has failed.', []),
	fail.

set_timezone(RD) :-
	inst_setting(timezone, TZ),
	lx_set_timezone(RD, TZ),
	!.
set_timezone(_RD) :-
	tui_msgbox('Setting of timezone has failed.', []),
	fail.

set_hostname(RD) :-
	inst_setting(hostname, HN),
	lx_set_hostname(RD, HN),
	!.
set_hostname(_RD) :-
	tui_msgbox('Setting of hostname has failed.', []),
	fail.

set_rootpassword(RD) :-
	inst_setting_tmp(passwd(root), PW),
	lx_set_password(RD, root, PW),
	!.
set_rootpassword(_RD) :-
	tui_msgbox('Setting of root password has failed.', []),
	fail.

on_useradd_rc(0) :- !.
on_useradd_rc(RC) :-
	lx_useradd_rc(RC, M), !,
	tui_msgbox(M, []),
	fail.
on_useradd_rc(RC) :-
	number_atom(RC, RCA),
	tui_msgbox2(['useradd. Unknown error code:', RCA], []),
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
	tui_msgbox('Setting up of a user account has failed.', []),
	fail.

copy_rootfs(RD) :-
	TA = [tar, '--create', '--one-file-system', '--xattrs'],
	TAE = [TA, '-f', '-', '/', '2>/dev/null', '|', 
		tar, '--extract', '--xattrs', '--xattrs-include=\'*\'', '--preserve-permissions', '-v', '-f', '-', '-C', RD, '2>&1'
	],
	% os_shell2(TAE),
	tui_progressbox(TAE, '', [title(' Copying live image to target rootfs '), sz(max)]),
	true.

set_bootloader(RD) :-
	inst_setting(bootloader_dev, BD),
	inst_setting(bootloader, B),
	set_bootloader(B, BD, RD), !.
set_bootloader(_RD) :-
	tui_msgbox('Setting up of a bootloader has failed.', []),
	fail.

% set_bootloader(bootloader, bootloader_dev, root_dir)
set_bootloader(_, none, _RD) :- !.
set_bootloader(grub2, BD, RD) :-
	( inst_setting(system(efi), EFI_TARGET) ->
	  O = [oo(target, EFI_TARGET), '--efi-directory=/boot/efi', '--bootloader-id=void_grub', '--recheck']
	; O = []
	),
	( uses_zfs ->
	  ENV = ['ZPOOL_VDEV_NAME_PATH=1']
	; ENV = []
	),
	CL1 = [chroot, RD, ENV, 'grub-install', O, BD, '2>&1'],
	% os_shell2(CL1),
	tui_progressbox(CL1, '', [title(' Installing bootloader '), sz([6, 60])]),
	CL2 = [chroot, RD, 'grub-mkconfig', '-o', '/boot/grub/grub.cfg', '2>&1'],
	% os_shell2(CL2),
	tui_progressbox(CL2, '', [title(' Generating grub configuration file '), sz([10, 60])]),
	% SL = [sync],
	% os_call2(SL), os_call2(SL), os_call2(SL),
	os_call2([udevadm, settle]),
	!.
set_bootloader(rEFInd, BD, RD) :-
	% BD is the disk (not a partition)
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	( inst_setting(partition, part(BD, _P, EFI_PD, efi_system, _FS, _Label, _MP, _CK, _SZ))
	; tui_msgbox('efi system partition was not found', []),
	  fail
	), !,
	( inst_setting(partition, part(BD, _P1, ROOT_PD, _PT1, _FS1, _Label1, '/', _CK1, _SZ1))
	; tui_msgbox('root partition was not found', []),
	  fail
	), !,
	CL1 = [chroot, RD, 'refind-install', oo(usedefault), EFI_PD, '2>&1'],
	tui_progressbox(CL1, '', [title(' Installing bootloader '), sz([6, 60])]),
	% os_shell2(CL1),
	( inst_setting(root_fs, btrfs) ->
	  % This is not needed.
	  % os_mkdir_p(RD + '/boot/efi/EFI/refind/drivers_x64'),
	  % os_call2([cp, '-f', RD + '/usr/share/refind/drivers_x64/btrfs_x64.efi', RD + '/boot/efi/EFI/refind/drivers_x64/btrfs_x64.efi']),

	  lx_get_dev_partuuid(ROOT_PD, RPID),
	  atom_concat(RD, '/boot/refind_linux.conf', FN),
	  open(FN, write, S),

	  write(S, '"Boot with standard options" "root=PARTUUID='), write(S, RPID),
	  write_refind_btrfs(S),
	  write(S, '"'), nl(S),

	  write(S, '"Boot to single-user mode" "root=PARTUUID='), write(S, RPID),
	  write_refind_btrfs(S),
	  write(S, ' single"'), nl(S),

	  close(S)
	; true
	),
	% CL2 = [chroot, RD, mkrlconf, '2>&1'],
	% os_shell2(CL2),
	!.
set_bootloader(limine, _BD, RD) :-
	os_mkdir_p(RD + '/boot/efi/EFI/BOOT'),
	os_call2([cp, '-f', RD + '/usr/share/limine/BOOTX64.EFI', RD + '/boot/efi/EFI/BOOT/BOOTX64.EFI']),

	% os_mkdir_p(RD + '/boot/limine'),
	% atom_concat(RD + '/boot/limine/limine.cfg', CF),

	atom_concat(RD, '/boot/efi/EFI/BOOT/limine.cfg', CF),
	open(CF, write, S),
	close(S),
	!.

write_refind_btrfs(S) :-
	inst_setting(keymap, KB),
	inst_setting(locale, LC),
	write(S, ' rw rootflags=subvol=@ initrd=@\\boot\\initramfs-%v.img rd.luks=0 rd.md=0 rd.dm=0 loglevel=4 gpt add_efi_memmap vconsole.unicode=1'),
	format(S, ' vconsole.keymap=~w locale.LANG=~w rd.live.overlay.overlayfs=1', [KB, LC]),
	true.

set_sudoers :-
	inst_setting(useraccount, user(UL, _UN, UGL)),
	lx_chroot_set_sudoers(UL, UGL),
	!.
set_sudoers :-
	tui_msgbox('Setting up of sudoers has failed.', []),
	fail.

set_network(RD) :-
	inst_setting(network, NC), !,
	( set_network_(NC, RD)
	; tui_msgbox('Setting up of network has failed.', []),
	  fail
	),
	!.
set_network(_RD).

set_network_(none, _RD) :- !.
set_network_(dhcp(D), RD) :- !,
	( atom_concat('wl', _, D) ->
	  os_call2([cp, '/etc/wpa_supplicant/wpa_supplicant.conf', RD + '/etc/wpa_supplicant']),
	  os_call2([ln, '-sf', '/etc/sv/wpa_supplicant', RD + '/etc/runit/runsvdir/default/wpa_supplicant'])
	; true
	),
	enable_dhcpd(RD),
	true.
set_network_(static(D, IP, GW, DNS1, DNS2), RD) :- !,
	% static IP through dhcpcd.
	atom_concat(RD, '/etc/dhcpcd.conf', CF),
	os_call2([mv, CF, CF + '.orig']),
	open(CF, write, S),
	format(S, '# Static IP configuration set by the void-installer for ~w.\n', [D]),
	format(S, 'interface ~w\n', [D]),
	format(S, 'static ip_address=~w\n', [IP]),
	format(S, 'static routers=~w\n', [GW]),
	format(S, 'static domain_name_servers=~w ~w\n', [DNS1, DNS2]),
	close(S),
	enable_dhcpd(RD),
	true.
set_network_(_, _RD) :- !,
	tui_msgbox('Invalid network connection type.', []),
	fail.

enable_dhcpd(RD) :-
	os_call2([ln, '-sf', '/etc/sv/dhcpcd', RD + '/etc/runit/runsvdir/default/dhcpcd']),
	true.

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
	tui_progressbox([zpool, export, '-f', PN, '2>&1'], '', [title(' export zpool '), sz([6, 40])]),
	fail.
umount_filesystems(_RD).

umount_dev(D) :-
	tui_msgbox(D, []),
	os_call2([umount, '--recursive', D]).
umount_dev(D) :-
	tui_msgbox2(['ERROR: filesystem unmounting has failed.', D], [sz([6, 40])]),
	fail.

menu_password(UL) :-
	format_to_atom(MP1, ' Enter password for user ~w ', [UL]),
	dialog_msg(form, FORMLABEL),
	( inst_setting_tmp(passwd(UL), UP) ->
	  true
	; UP = ''
	),
	tui_passwordform_v(25, 0, [item('Choose a password:', UP), item('Confirm your password:', UP)], FORMLABEL, [title(MP1)], [P1, P2|_]),
	check_password(UL, P1, P2), !,
	retractall(inst_setting_tmp(passwd(UL), _)),
	assertz(inst_setting_tmp(passwd(UL), P1)).

check_password(UL, P1, P2) :-
	P1 \= P2, !,
	tui_yesno('Passwords don\'t match. Would you like to reenter?', [sz([6, 40])]),
	menu_password(UL).
check_password(UL, '', _) :- !,
	tui_yesno('Password is empty. Would you like to reenter?', [sz([6, 40])]),
	menu_password(UL).
check_password(_, _, _).

enter_root_password(P) :-
	format_to_atom(PA, 'echo "root:~w" | chpasswd', [P]),
	os_shell(PA),
	!.

make_opt(N, V, O) :-
	join_atoms([N, V], '=', O).

% generate fstab
make_fstab(RD) :-
	atom_concat(RD, '/etc/fstab', FN),
	open(FN, write, S),
	get_fstab_list(MPL),
	maplist(make_fstab_(S), MPL),
	close(S),
	os_call2([chmod, '644', FN]),
	!.
make_fstab(_) :-
	tui_msgbox('Making of fstab has failed.', []),
	fail.

make_fstab_(S, fstab(MP, FS, PD)) :-
	write_fstab(FS, PD, MP, S),
	true.

write_fstab(vfat, D, '/boot/efi', S) :- !,
	MP = '/boot/efi',
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	( inst_setting(fs(vfat, '/boot/efi'), mount(O)) -> true
	; O = [rw, noatime]
	),
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	L = [UU, MP, vfat, OA, '0 2'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(zfs, _D, _MP, _S) :- !,
	% Do nothing
	true.
write_fstab(lvm, _D, MP, S) :- !,
	inst_setting(lvm, lv(VG, LV, _SZ)),
	inst_setting(root_fs, FS),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	write_fstab(FS, LVM_PD, MP, S),
	true.
write_fstab(btrfs, D, _MP, S) :- !,
	lx_get_dev_uuid(D, U),
	write_fstab_btrfs(D, U, S),
	% write_fstab_btrfs_snap(U, S),
	true.
write_fstab(swap, D, _MP, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	% O = [sw],
	O = [defaults],
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	L = [UU, none, swap, OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(proc, _, _, S) :- !,
	write(S, '# /proc with hidepid (https://wiki.archlinux.org/index.php/Security#hidepid)'), nl(S),
	O = [nodev, noexec, nosuid, 'hidepid=2', 'gid=proc'],
	join_atoms(O, ',', OA),
	L = [proc, '/proc', proc, OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(tmp, _, _, S) :- !,
	O = [defaults, nosuid, nodev],
	join_atoms(O, ',', OA),
	L = [tmpfs, '/tmp', tmpfs, OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.
write_fstab(FS, D, MP, S) :- !,
	lx_get_dev_uuid(D, U),
	format(S, '# ~w\n', [D]),
	O = [defaults],
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	L = [UU, MP, FS, OA, '0 1'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	true.

write_fstab_btrfs(D, U, S) :-
	inst_setting(btrfs, subv(SV, mp(MP), OL, _)),
	format(S, '# ~w\n', [D]),
	% format(S, '#UUID=~w\t~w\tbtrfs\n', [U, MP]),
	atom_concat('subvol=/', SV, SO),
	O = [SO|OL],
	join_atoms(O, ',', OA),
	atom_concat('UUID=', U, UU),
	L = [UU, MP, 'btrfs', OA, '0 0'],
	join_atoms(L, '\t', LA),
	write(S, LA), nl(S), nl(S),
	fail.
write_fstab_btrfs(_, _, _).

% write_fstab_btrfs_snap(U, S) :-
% 	inst_setting(fs(btrfs, '/'), snap_dir(MP)), !,
% 	format(S, '# ~w\n', [MP]),
% 	O = ['subvolid=5', noatime],
% 	join_atoms(O, ',', OA),
% 	atom_concat('UUID=', U, UU),
% 	L = [UU, MP, 'btrfs', OA, '0 0'],
% 	join_atoms(L, '\t', LA),
% 	write(S, LA), nl(S), nl(S),
% 	!.
% write_fstab_btrfs_snap(_, _).

wipe_disk :-
	inst_setting(template, manual),
	!.
wipe_disk :-
	inst_setting(bootloader_dev, D),
	( wipe_disk(D)
	; tui_msgbox2([wipe_disk, D, has, failed], []),
	  fail
	),
	!.

wipe_disk(D) :-
	os_shell2_lines([wipefs, '--noheadings', D], L),
	( L = [] ->
	  true
	; os_shell2([wipefs, '-a', D, '2>&1', '1>/dev/null']),
	  wipe_disk(D)
	),
	!.

menu_part_soft(S) :-
	SL = [[cfdisk, 'Easy to use'], [fdisk, 'More advanced']],
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [title(' Select the software for partitioning ')], S).

menu_part_manually :-
	menu_dev(' Select the disk to partition ', D),
	menu_part_soft(S),
	os_call2([S, D]),
	true.

part2checklist_tag_on_off(ON, part_info(_D, P, _PA, _FS, FSS), [P, FSS, I]) :-
	( member(P, ON) ->
	  I = on
	; I = off
	),
	true.

part2taglist(part_info(_D, P, _PA, _FS, FSS), [P, FSS]).

update_part_info(ONL, PLO) :-
	maplist(del_part_info(PLO), ONL),
	lx_list_part_info(PIL),
	maplist(ins_part_info(PIL, ONL), PLO),
	true.

% L - list of partitions to keep.
del_part_info(L, P) :-
	( member(P, L) ->
	  true
	; retractall(inst_setting(partition, part(_, P, _, _, _, _, _, _, _SZ)))
	),
	true.

ins_part_info(PIL, L, P) :-
	( member(P, L) ->
	  true
	; memberchk(part_info(D, P, PA, FS, _FSS), PIL),
	  assertz(inst_setting(partition, part(D, P, PA, linux, FS, '', '', keep, _SZ)))
	),
	true.

menu_part_select :-
	% ONL - list of already configured partitions.
	findall(PN, inst_setting(partition, part(_, PN, _, _, _, _, _, _, _SZ)), ONL),
	lx_list_part_info(PIL),
	PIL \= [], !,
	maplist(part2taglist, PIL, ML1),
	maplist(tui_checklist_on_off(ONL), ML1, ML2),
	MT1 = ' Select partition(s) to use ',
	dialog_msg(menu, MENULABEL),
	tui_checklist_tag(ML2, MENULABEL, [title(MT1)], PLO),
	update_part_info(ONL, PLO),
	!.
menu_part_select :-
	tui_msgbox('There are no partitions available.', [sz([6, 40])]),
	true.

menu_keymap :-
	os_shell_lines('find /usr/share/kbd/keymaps/ -type f -iname "*.map.gz" -printf "%f\n" | sed \'s|.map.gz||g\' | sort', KML),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(keymap, OKM) ->
	  true
	; OKM = us
	),
	tui_radiolist_tag2(KML, OKM, RADIOLABEL, [no-tags, title(' Select your keymap ')], KM), !,
	retractall(inst_setting(keymap, _)),
	assertz(inst_setting(keymap, KM)).

make_lng_cntr(A1, [A1, R]) :-
	atom_concat(A2, '.UTF-8', A1),
	atom_chars(A2, LC),
	split_list_ne(LC, ['_'], LCL),
	chars_lc(LCL, LNG, CNTR),
	get_lng_name(LNG, LN),
	get_country_name(CNTR, CN),
	format_to_atom(R, '~w (~w)', [LN, CN]).

chars_lc([LC1, LC2], LNG, CNTR) :- !,
	atom_chars(LNG, LC1),
	atom_chars(CNTR, LC2).
chars_lc([LC1], LNG, '') :- !,
	atom_chars(LNG, LC1).

menu_locale :-
	os_shell_lines('grep -E \'\\.UTF-8\' /etc/default/libc-locales|awk \'{print $1}\'|sed -e \'s/^#//\'', LCL),
	maplist(make_lng_cntr, LCL, LCL1),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(locale, OLC) ->
	  true
	; OLC = 'en_US.UTF-8'
	),
	tui_radiolist_tag2(LCL1, OLC, RADIOLABEL, [title(' Select your locale ')], LC), !,
	retractall(inst_setting(locale, _)),
	assertz(inst_setting(locale, LC)).

split_tz(TZ, A1, A2) :-
	atom_codes(TZ, TZL),
	split_list_ne(TZL, "/", LL),
	maplist(codes_atom, LL, [A1, A2]),
	true.

menu_timezone :-
    AREAS = ['Africa', 'America', 'Antarctica', 'Arctic', 'Asia', 'Atlantic', 'Australia', 'Europe', 'Indian', 'Pacific'],

	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(timezone, OTZ) ->
	  true
	; OTZ = 'America/New_York'
	),
	split_tz(OTZ, A1, A2),
	tui_radiolist_tag2(AREAS, A1, RADIOLABEL, [no-tags, title(' Select area ')], A), !,

	os_shell2_lines([ls, '/usr/share/zoneinfo/' + A], TZL),

	( A1 = A ->
	  TZ1 = A2
	; TZ1 = none
	),
	tui_radiolist_tag2(TZL, TZ1, RADIOLABEL, [no-tags, title(' Select location ')], TZ), !,

	format_to_atom(ATZ, '~w/~w', [A, TZ]),
	retractall(inst_setting(timezone, _)),
	assertz(inst_setting(timezone, ATZ)).

menu_dev_list(dev(_D, DA, GB, DSSZ), [DA, DIA]) :-
	format_to_atom(DIA, 'size:~2fGB;sector_size:~d', [GB, DSSZ]),
	true.

menu_dev(Title, D) :-
	lx_list_dev(L),
	maplist(menu_dev_list, L, DL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(DL, MENULABEL, [title(Title)], D).

% If there is only one device select it automatically.
menu_dev_light(Title, DA) :-
	lx_list_dev(L),
	( L = [dev(_D, DA, _GB, _DSSZ)] ->
	  true
	; maplist(menu_dev_list, L, DL),
	  dialog_msg(menu, MENULABEL),
	  tui_menu_tag(DL, MENULABEL, [title(Title)], DA)
	).

menu_btrfs :-
	tui_msgbox2([not, implemented, yet], []),
	true.

menu_bootloader :-
	B = [
		  grub2
		, rEFInd
		% , limine
		% , zfsBootMenu
	],
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(bootloader, OB)
	; OB = none
	), !,
	tui_radiolist_tag2(B, OB, RADIOLABEL, [no-tags, title(' Select a bootloader ')], NB), !,
	( OB = NB
	; menu_template(NB),
	  retractall(inst_setting(bootloader, _)),
	  assertz(inst_setting(bootloader, NB))
	),
	!.

menu_bootloader_dev :-
	lx_list_dev(L),
	maplist(menu_dev_list, L, DL),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(bootloader_dev, OB) ->
	  true
	; OB = none
	),
	append(DL, [[none, 'Manage bootloader otherwise']], BL1),
	tui_radiolist_tag2(BL1, OB, RADIOLABEL, [title(' Select the disk to install the bootloader ')], D), !,
	retractall(inst_setting(bootloader_dev, _)),
	assertz(inst_setting(bootloader_dev, D)).

split_grp(G, GL) :-
	atom_chars(G, GC),
	split_list_ne(GC, [':'], GCL),
	maplist(chars_atom, GCL, GL),
	true.

grp_on_off(ON, [G, _, N|_], [A, I]) :-
	( member(G, ON)
	-> I = on
	;  I = off
	),
	format_to_atom(A, '~w:~w', [G, N]).

grp_ind2name(L, N, G) :-
	nth(N, L, [G|_]).

menu_usergroups(GL3) :-
    G = [wheel, audio, video, floppy, cdrom, optical, kvm, xbuilder],
	os_shell_lines('cat /etc/group', GL),
	maplist(split_grp, GL, GL1),
	maplist(grp_on_off(G), GL1, GL2),
	dialog_msg(menu, LISTLABEL),
	tui_checklist_ind(GL2, LISTLABEL, [title(' Select group ')], SGL1),
	maplist(grp_ind2name(GL1), SGL1, GL3),
	true.

menu_useraccount :-
	inst_setting(useraccount, user(LN, UN, _GL)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Login name:', LN),
		item('User name:', UN)
		], FORMLABEL, [title(' User account settings ')], [LN, UN|_]),
	menu_password(LN),
	menu_usergroups(GL),
	retractall(inst_setting(useraccount, _)),
	assertz(inst_setting(useraccount, user(LN, UN, GL))),
	true.

menu_lvm :-
	inst_setting(lvm, lv(VG, LV, SZ)),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 100, [
		item('Volume Group:', VG),
		item('Logic Volume:', LV)
		], FORMLABEL, [title(' LVM settings ')], [VG1, LV1|_]),
	retractall(inst_setting(lvm, _)),
	assertz(inst_setting(lvm, lv(VG1, LV1, SZ))),
	true.

part2menu_tag(PIL, PA, [P, FSS]) :-
	memberchk(part_info(_D, P, PA, _FS, FSS), PIL),
	true.

menu_filesystem :-
	% do not try to edit swap partition.
	findall(PD, (inst_setting(partition, part(_D, _P, PD, PT, _FD, _Label, _MP, _F, _SZ)), PT \= swap), PL0),
	( PL0 = [] ->
	  tui_msgbox('No partitions was selected.', []),
	  fail
	; sort(PL0, PL1)
	),
	lx_list_part_info(PIL),
	maplist(part2menu_tag(PIL), PL1, ML1),
	MT1 = ' Select the partition to edit ',
	dialog_msg(menu, MENULABEL),
	repeat,
	tui_menu_tag2(edit_fs_short, ML1, MENULABEL, [cancel-label('Done'), title(MT1)], P),
	menu_fs_short(P),
	!.

menu_fs_short(cancel) :- !,
	true.
menu_fs_short(P) :-
	dialog_msg(menu, MENULABEL),
	make_tmp_part_rec(P),
	repeat,
	inst_setting_tmp(partition, part(_, P, _Dev, _, Type, Label, MP, F, _SZ)),
	( F = create -> FV = yes
	; FV = no
	),
	ML1 = [
		[label, Label],
		[type, Type],
		[mount_point, MP],
		[create, FV]
	],
	maplist(make_menu_fs_short, ML1, ML2),
	format_to_atom(TA, ' Set ~w filesystem parameters ', [P]),
	tui_menu_tag2(file_system, ML2, MENULABEL, [extra-button, extra-label('Accept'), ok-label('Edit'), title(TA)], Tag),
	menu_fs_info(CMD, Tag),
	menu_fs_action(CMD, P), !,
	fail.

make_menu_fs_short([T, V], [N, V]) :-
	menu_fs_info(T, N),
	true.

menu_fs_info(create, 'Create FS') :- !.
menu_fs_info(mount_point, 'Mount point') :- !.
menu_fs_info(type, 'Type') :- !.
menu_fs_info(label, 'Label') :- !.
menu_fs_info(save, extra) :- !.
menu_fs_info(exit, cancel) :- !.

menu_fs_action(exit, _) :- !,
	true.
menu_fs_action(save, P) :-
	make_perm_part_rec(P), !,
	tui_msgbox('Settings are saved.', []),
	true.
menu_fs_action(label, P) :- !,
	inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, MP, F, SZ)), !,
	tui_inputbox('', Label, [title('Label')], A),
	retractall(inst_setting_tmp(partition, part(_, P, _, _, _, _, _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part(D, P, PD, PT, Type, A, MP, F, SZ))),
	fail.
menu_fs_action(type, P) :- !,
	inst_setting_tmp(partition, part(D, P, PD, PT, OFS, Label, MP, F, SZ)), !,
	menu_select_fs(OFS, NFS),
	retractall(inst_setting_tmp(partition, part(_, P, _, _, _, _, _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part(D, P, PD, PT, NFS, Label, MP, F, SZ))),
	fail.
menu_fs_action(mount_point, P) :- !,
	inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, MP, F, SZ)), !,
	tui_inputbox('', MP, [title('Mount Point')], A),
	retractall(inst_setting_tmp(partition, part(_, P, _, _, _, _, _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, A, F, SZ))),
	fail.
menu_fs_action(create, P) :-
	inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, MP, _F, SZ)), !,
	( tui_yesno('Create file system?', [sz([6, 40])]) -> FV = create
	; FV = keep
	),
	retractall(inst_setting_tmp(partition, part(_, P, _, _, _, _, _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, MP, FV, SZ))),
	fail.

make_tmp_part_rec(P) :-
	inst_setting(partition, part(D, P, PD, PT, Type, Label, MP, F, SZ)),
	retractall(inst_setting_tmp(partition, part(_, P, _, _, _, _, _, _, _SZ))),
	assertz(inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, MP, F, SZ))),
	true.

make_perm_part_rec(P) :-
	inst_setting_tmp(partition, part(D, P, PD, PT, Type, Label, MP, F, SZ)), !,
	retractall(inst_setting(partition, part(_, P, _, _, _, _, _, _, _SZ))),
	assertz(inst_setting(partition, part(D, P, PD, PT, Type, Label, MP, F, SZ))),
	true.

fs_type_to_menu(FST, [FST, Descr]) :-
	fs_info(FST, Descr),
	true.

% bootloader_info(bootloade, supported_fs, supported_template).
bootloader_info(grub2, [
		  btrfs
		, ext2
		, ext3
		, ext4
		% , f2fs
		, swap
		, vfat
		, xfs
		% , zfs
	], [
		  manual
		, efi_basic
		, efi_lvm
		% , efi_zfsbootmenu
	]).
bootloader_info(rEFInd, [
		  btrfs
		, ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, efi_basic
	]).
bootloader_info(limine, [
		  ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, efi_basic
	]).
bootloader_info(zfsBootMenu, [
		  zfs
	], [
		  efi_zfsbootmenu
	]).

% OFS - old file system
% NFS - new file system
menu_select_fs(OFS, NFS) :-
	inst_setting(bootloader, B),
	inst_setting(template, TN),
	menu_select_fs(B, TN, OFS, NFS),
	true.

% It is not supposed to assertz.
% TN - template name
% OFS - old file system
% NFS - new file system
menu_select_fs(B, TN, OFS, NFS) :-
	bootloader_info(B, ML0, _),
	template_info(TN, _Descr, EL),
	subtract(ML0, EL, ML1),
	maplist(fs_type_to_menu, ML1, ML),

	dialog_msg(radiolist, RADIOLABEL),
	tui_radiolist_tag2(ML, OFS, RADIOLABEL, [title(' Select the filesystem type ')], NFS),
	true.

menu_root_fs :-
	inst_setting(bootloader, B),
	inst_setting(template, TN),
	menu_root_fs(B, TN, _NFS),
	true.

% B - bootloader
% TN - template name
menu_root_fs(B, TN, NFS) :-
	inst_setting(root_fs, OFS),
	menu_select_fs(B, TN, OFS, NFS),

	retractall(inst_setting(root_fs, _)),
	assertz(inst_setting(root_fs, NFS)),
	true.

% DO NOT delete
% Use portray_clause.
% setting_value(partition, V1) :- !,
% 	findall(N, inst_setting(partition, part(_, N, _, _, _, _, _, _, _)), VL),
% 	open_output_codes_stream(ST),
% 	portray_clause(ST, VL),
% 	close_output_atom_stream(ST, V1).
% setting_value(S, V1) :-
% 	inst_setting(S, V), !,
% 	open_output_codes_stream(ST),
% 	portray_clause(ST, V),
% 	close_output_atom_stream(ST, V1).

setting_value(partition, V1) :- !,
	findall(N, inst_setting(partition, part(_, N, _, _, _, _, _, _, _)), VL),
	write_to_atom(V1, VL).
setting_value(root_passwd, '********') :-
	inst_setting_tmp(passwd(root), _), !.
setting_value(user_passwd, '********') :-
	inst_setting(useraccount, user(UN, _, _)),
	inst_setting_tmp(passwd(UN), _), !.
setting_value(lvm_info, V1) :- !,
	inst_setting(lvm, lv(VG, LV, _SZ)),
	format_to_atom(V1, 'VG: ~w, LV: ~w', [VG, LV]).
setting_value(S, V1) :-
	inst_setting(S, V), !,
	write_to_atom(V1, V).
setting_value(_, 'not set').

setting_value_str(S, V) :-
	setting_value(S, V1),
	format_to_atom(V, '~w: ~w', [S, V1]).

setting_value_list(S, [S, V]) :-
	setting_value(S, V).

menu_show :-
	( inst_setting(template, efi_lvm) ->
	  S0 = [lvm_info]
	; S0 = []
	),
	S = [
		  partition
		, root_fs
		, keymap
		, locale
		, timezone
		, bootloader
		, bootloader_dev
		, network
		, hostname
		, source
		, root_passwd
		, useraccount
		, user_passwd
		| S0
	],
	% maplist(setting_value_list, S, SL),
	maplist(menu_tag_v, S, SL),
	dialog_msg(menu, MENULABEL),
	tui_menu_tag(SL, MENULABEL, [no-cancel, title(' Current settings ')], _Tag),
	true.

switch_template(OT, OT, _B) :- !.
switch_template(OT, NT, B) :-
	on_disable(template(OT)),
	on_enable(template(NT), B),
	!.

on_enable(template(manual), _B) :- !,
	assertz(inst_setting(template, manual)),
	% dev-time settings.
	% assertz(inst_setting(bootloader_dev, '/dev/sda')),
	% assertz(inst_setting(partition, part('/dev/sda', sda1, '/dev/sda1', efi_system, vfat, efi, '/boot/efi', create, _))),
	% assertz(inst_setting(partition, part('/dev/sda', sda2, '/dev/sda2', swap, swap, swap, '', create, _))),
	% assertz(inst_setting(partition, part('/dev/sda', sda3, '/dev/sda3', linux, ext4, void, '/', create, _))),
	true.
on_enable(template(efi_basic), B) :- !,
	assertz(inst_setting(template, efi_basic)),
	menu_dev_light(' Select the disk to partition ', D),
	assertz(inst_setting(bootloader_dev, D)),

	menu_root_fs(B, efi_basic, FS),

	% pi(PartType, FileSystem, Label, MountPoint, create/keep, size)
	( FS = zfs ->
	  BPL0 = [pi(linux, ext4, boot, '/boot', create, '1G'), pi(solaris_root, zfs, void, '/', create, '')]
	; BPL0 = [pi(linux, FS, void, '/', create, '')]
	),
	BPL1 = [pi(efi_system, vfat, efi, '/boot/efi', create, '550M')|BPL0],
	( inst_setting(system(efi), _) ->
	  BPL2 = BPL1
	; BPL2 = [pi(bios_boot, '', 'BIOS boot', '', keep, '2M')|BPL1]
	),
	part_template(D, BPL2),

	% subv(name, mount_point, mount_attrs, cow)
	assertz(inst_setting(btrfs, subv('@', mp('/'), [rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], cow))),
	% assertz(inst_setting(btrfs, subv('@home', mp('/home'), [nosuid, nodev, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], cow))),
	assertz(inst_setting(btrfs, subv('@home', mp('/home'), [nosuid, nodev, rw, noatime, 'space_cache=v2'], cow))),
	assertz(inst_setting(btrfs, subv('@opt', mp('/opt'), [nodev, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], cow))),
	assertz(inst_setting(btrfs, subv('@srv', mp('/srv'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var', mp('/var'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], cow))),
	assertz(inst_setting(btrfs, subv('@var-cache-xbps'), mp('/var/cache/xbps', [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], cow))),
	assertz(inst_setting(btrfs, subv('@var-lib-ex', mp('/var/lib/ex'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-log', mp('/var/log'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-opt', mp('/var/opt'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], cow))),
	assertz(inst_setting(btrfs, subv('@var-spool', mp('/var/spool'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-tmp', mp('/var/tmp'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], nodatacow))),
	assertz(inst_setting(btrfs, subv('@snapshots', mp('/.snapshots'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', 'space_cache=v2'], nodatacow))),

	% dataset(name, mount_point, mount_attrs)
	assertz(inst_setting(zfs, dataset('ROOT', 'none', ['canmount=off']))),
	assertz(inst_setting(zfs, dataset('ROOT/void', '/', ['canmount=noauto', 'atime=off']))),
	% assertz(inst_setting(zfs, dataset('ROOT/void', '/', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('home', '/home', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('opt', '/opt', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('srv', '/srv', ['atime=off']))),
	% assertz(inst_setting(zfs, dataset('usr', '/usr', ['canmount=off', 'atime=off']))),
	% assertz(inst_setting(zfs, dataset('usr-local', '/usr/local', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('tmp', '/tmp', ['com.sun:auto-snapshot=false', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var', '/var', ['canmount=off', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var-lib', '/var/lib', ['canmount=off', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var-cache-xbps', '/var/cache/xbps', ['com.sun:auto-snapshot=false', 'atime=off']))),
	assertz(inst_setting(zfs, dataset('var-lib-ex', '/var/lib/ex', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-log', '/var/log', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-opt', '/var/opt', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-spool', '/var/spool', ['atime=off']))),
	assertz(inst_setting(zfs, dataset('var-tmp', '/var/tmp', ['com.sun:auto-snapshot=false', 'atime=off']))),
	% os_shell2([zfs, create, '-o', 'mountpoint=/var/tmp', 'zroot/var-tmp']),

	true.
on_enable(template(efi_lvm), B) :- !,
	assertz(inst_setting(template, efi_lvm)),
	menu_dev_light(' Select the disk to partition ', D),
	assertz(inst_setting(bootloader_dev, D)),

	menu_root_fs(B, efi_lvm, _FS),

	% pi(PartType, FileSystem, Label, MountPoint, create/keep, size)
	BPL1 = [pi(efi_system, vfat, efi, '/boot/efi', create, '550M'), pi(linux, lvm, void, '/', create, '')],
	( inst_setting(system(efi), _) ->
	  BPL2 = BPL1
	; BPL2 = [pi(bios_boot, '', 'BIOS boot', '', keep, '2M')|BPL1]
	),
	part_template(D, BPL2),

	true.
on_enable(template(efi_zfsbootmenu), _B) :- !,
	assertz(inst_setting(template, efi_zfsbootmenu)),
	tui_msgbox2([not, implemented, yet], []),
	true.
on_enable(template(TMPL), _B) :- !,
	tui_msgbox2(['Unknown template ', TMPL], []),
	fail.

on_disable(template(manual)) :- !,
	retractall(inst_setting(template, manual)),
	retractall(inst_setting(partition, _)),
	% There is no need to delete these settings.
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(efi_basic)) :- !,
	retractall(inst_setting(template, efi_basic)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(btrfs, _)),
	retractall(inst_setting(zfs, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(efi_lvm)) :- !,
	retractall(inst_setting(template, efi_lvm)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(efi_zfsbootmenu)) :- !,
	retractall(inst_setting(template, efi_zfsbootmenu)),
	true.
on_disable(template(_)) :- !,
	true.

part_template(D, L) :-
	lx_split_dev(D, P, S),
	part_template_(L, 1, D, P, S),
	true.

% pi(PartType, FileSystem, Label, MountPoint, create/keep, size)
part_template_([pi(PT, FS, Label, MP, CK, SZ)|T], N, D, P, S) :-
	format_to_atom(S1, '~w~d', [S, N]),
	atom_concat(P, S1, PD),
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	assertz(inst_setting(partition, part(D, S1, PD, PT, FS, Label, MP, CK, SZ))),
	N1 is N + 1,
	part_template_(T, N1, D, P, S),
	true.
part_template_([], _, _, _, _).

% template_info(name, descr, except_fs).
template_info(manual, 'Manual configuration of everything', []).
template_info(efi_basic, 'EFI. One device', [swap, vfat]).
template_info(efi_lvm, 'EFI. LVM. One device', [swap, btrfs, vfat]).
template_info(efi_zfsbootmenu, 'EFI. ZFS. One device', []).

template_to_menu(T, [T, Descr]) :-
	template_info(T, Descr, _),
	true.

menu_template(B) :-
	inst_setting(template, OT),
	bootloader_info(B, _, TL0),
	( TL0 = [NT]
	; maplist(template_to_menu, TL0, TL),
	  dialog_msg(radiolist, RADIOLABEL),
	  tui_radiolist_tag2(TL, OT, RADIOLABEL, [no-tags, title(' Choose configuration ')], NT)
	),
	switch_template(OT, NT, B),
	true.

menu_hostname :-
	setting_value(hostname, I),
	tui_inputbox('', I, [title(' Set host name ')], HN),
	retractall(inst_setting(hostname, _)),
	assertz(inst_setting(hostname, HN)).

menu_save :-
	( tui_yesno('Save settings?', [sz([6, 40])]) ->
	  open('settings.pl', write, S),
	  save_settings(S),
	  close(S)
	; true
	), !.

menu_tag(A, [T,D]) :-
	action_info(A,T,D), !.
menu_tag(A, [A,A]).

menu_tag_v(A, [T, V]) :-
	action_info(A, T, _), !,
	setting_value(A, V).
menu_tag_v(A, [A,A]).

test_network(_) :-
	% tui_msgbox('before xbps-uhelper fetch', []),
	tui_infobox('Testing network connection.', [sz([4, 40])]),
	between(1, 20, _),
	sleep(0.25),
	os_rm_f(otime),
	os_shell('xbps-uhelper fetch https://repo-default.voidlinux.org/current/otime 1>/dev/null 2>&1'),
	% os_shell('xbps-uhelper fetch https://repo-default.voidlinux.org/current/otime 2>&1'),
	% tui_progressbox(['xbps-uhelper', fetch, 'https://repo-default.voidlinux.org/current/otime', '2>&1'], '', [title(' Test Network Connection '), sz([6, 80])]),
	tui_msgbox('Network is working properly!', [sz([6, 40])]),
	!.
test_network(nm) :-
	tui_msgbox('Network Manager is enabled but network is inaccessible, please set it up externally with nmcli, nmtui, or the Network Manager tray applet.', [sz([6, 40])]), !,
	fail.
test_network(_) :-
	tui_msgbox('Network is inaccessible, please set it up properly.', [sz([6, 40])]), !,
	fail.

select_net_conf(D, dhcp) :-
	format_to_atom(A, 'Do you want to use DHCP or STATIC for ~w?', [D]),
	tui_yesno(A, [yes-label(dhcp), no-label(static), sz([6, 40])]), !.
select_net_conf(_, static).

configure_net(D, dhcp) :-
	% tui_msgbox('dhcp', []),
	lx_iface_setup(D, RC1),
	( RC1 = 1 ->
	  ( os_shell2([sv, restart, dhcpcd, '1>/dev/null'])
	  ; tui_msgbox('ERROR: failed to run dhcpcd', []),
	    fail
	  ), !,
	  ( tui_infobox('Retrieving IP address.', [sz([6, 40])]),
		between(1, 40, _),
		sleep(0.25),
		lx_iface_setup(D, 0)
	  ; tui_msgbox2(['ERROR: DHCP request failed for', D], []),
	    fail
	  )
	; true
	),
	test_network(any),
	retractall(inst_setting(network, _)),
	assertz(inst_setting(network, dhcp(D))),
	!.
configure_net(D, static) :-
	format_to_atom(MA, 'Static IP configuration for ~w:', [D]),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 0, [
		item('IP address:'),
		item('Gateway:'),
		item('DNS Primary', '8.8.8.8'),
		item('DNS Secondary', '8.8.4.4')
		], FORMLABEL, [title(MA)], L),
	L = [IP, GW, DNS1, DNS2| _],
	( IP = '' -> tui_msgbox('IP adress is missing', []), fail ; true),
	( GW = '' -> tui_msgbox('Gateway adress is missing', []), fail ; true),
	( DNS1 = '' -> tui_msgbox('Primary DNS is missing', []), fail ; true),
	( DNS2 = '' -> tui_msgbox('Secondary DNS is missing', []), fail ; true),
	( os_shell2([ip, link, set, dev, D, up]) ->
	  true
	; format_to_atom(EA1, 'ERROR: Failed to bring ~w interface.', [D]),
	  tui_msgbox(EA1, []),
	  fail
	),
	format_to_atom(IPA, 'ip addr add "~w" dev ~w', [IP, D]),
	( os_shell(IPA) ->
	  true
	; format_to_atom(EA2, 'ERROR: Failed to set ip to the ~w interface.', [D]),
	  tui_msgbox(EA2, []),
	  fail
	),
	format_to_atom(GA, 'ip route add default via ~w', [GW]),
	( os_shell(GA) ->
	  true
	; format_to_atom(EA3, 'ERROR: Failed to setup gateway.', []),
	  tui_msgbox(EA3, []),
	  fail
	),
	format_to_atom(DNS1A, 'echo "nameserver ~w" >/etc/resolv.conf', [DNS1]),
	os_shell(DNS1A),
	format_to_atom(DNS2A, 'echo "nameserver ~w" >>/etc/resolv.conf', [DNS2]),
	os_shell(DNS2A),
    test_network(all),
	retractall(inst_setting(network, _)),
	assertz(inst_setting(network, static(D, IP, GW, DNS1, DNS2))),
	!.

configure_wifi(D) :-
	% format_to_atom(MA, 'Wireless configuration for ~w\n(encryption type: wep or wpa)', [D]),
	format_to_atom(MA, ' Wireless configuration for ~w (wep or wpa) ', [D]),
	dialog_msg(form, FORMLABEL),
	tui_mixedform_v(30, 100, [
		item('SSID:', ''),
		item('Password:', '', 1),
		item('Encryption:', 'wpa')
		], FORMLABEL, [title(MA)], L),
	L = [SSID, PSWD, ENCR| _],
	( SSID = '' ->
	  tui_msgbox('Invalid SSID.', []), fail
	; true
	),
	( \+ member(ENCR, [wep, wpa]) ->
	  tui_msgbox('Invalid encryption type (possible values: wep or wpa', []), fail
	; true
	),
	( PSWD = '' ->
	  tui_msgbox('Invalid AP password.', []), fail
	; true
	),
	WPASUPCONF = '/etc/wpa_supplicant/wpa_supplicant.conf',
    % reset the configuration to the default, if necessary otherwise backup the configuration
	atom_concat(WPASUPCONF, '.orig', OF),
	( file_exists(OF) ->
	  os_call2([cp, '-f', OF, WPASUPCONF])
	; os_call2([cp, '-f', WPASUPCONF, OF])
	),
	( ENCR = 'wep' ->
	  format_to_atom(WA, 'network={\n\tssid="~w"\n\twep_key0="~w"\n\twep_tx_keyidx=0\n\tauth_alg=SHARED\n}', [SSID, PSWD]),
	  open(WPASUPCONF, write, S),
	  write(S, WA),
	  close(S)
	; format_to_atom(WPAA, 'wpa_passphrase "~w" "~w" >> ~w', [SSID, PSWD, WPASUPCONF]),
	  os_shell(WPAA)
	  % os_shell2([wpa_supplicant, '-B', '-i', D, '-c', WPASUPCONF])
	),
    os_call2([sv, restart, wpa_supplicant]),
    configure_net(D, dhcp),
	true.

configure_net(none) :- !,
	retractall(inst_setting(network, _)),
	assertz(inst_setting(network, none)).
configure_net(D) :-
	select_net_conf(D, NT),
	configure_net(D, NT),
	true.

get_mac_addr(N, [N, M]) :-
	lx_get_mac_addr(N, M),
	true.

net_dev_name(dhcp(D), D) :- !.
net_dev_name(static(D, _IP, _GW, _DNS1, _DNS2), D) :- !.

get_net_devs :-
	lx_get_net_devs(AL1),
	maplist(get_mac_addr, AL1, AL2),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(network, D), net_dev_name(D, ON) ->
	  true
	; ON = none
	),
	append(AL2, [[none, 'Disable network']], AL3),
	tui_radiolist_tag2(AL3, ON, RADIOLABEL, [title(' Select the network interface to configure ')], Tag), !,
	( Tag = ON ->
	  % value hasn't change.
	  true
	; ( atom_concat('wl', _, Tag) ->
	    configure_wifi(Tag)
	  ; configure_net(Tag)
	  )
	),
	true.

menu_network :-
	( file_exists('/var/service/NetworkManager') ->
	  test_network(nm)
	; get_net_devs
	),
	true.

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
ensure_passwd.

ensure_setting(passwd) :- !,
	ensure_passwd.
ensure_setting(S) :-
	inst_setting(S, _), !.
ensure_setting(S) :-
	cmd_menu(S).

menu_common :-
	( inst_setting(template, efi_lvm) ->
	  M0 = [lvm_info]
	; M0 = []
	),
	( inst_setting(root_fs, btrfs) ->
	  % M1 = [btrfs_opt|M0]
	  M1 = M0
	; M1 = M0
	),
	M = [
		  keymap
		, network
		, source
		, hostname
		, locale
		, timezone
		, root_passwd
		, useraccount
		, user_passwd
		| M1
	],
	dialog_msg(menu, MENULABEL),
	repeat,
	maplist(menu_tag_v, M, ML),
	tui_menu_tag2(main_common, ML, MENULABEL, [cancel-label('Exit'), title(' Common installation settings ')], Tag),
	action_info(A, Tag, _),
	menu_action(A),
	!.

menu :-
	dialog_msg(menu, MENULABEL),
	repeat,
	M = [
		  show
		, bootloader
		, template
		, root_fs
		, bootloader_dev
		, part_manually
		, part_select
		, filesystem
		, common_settings
		, install
	],
	( inst_setting(template, manual) ->
	  subtract(M, [root_fs], M1)
	; subtract(M, [bootloader_dev, part_manually, part_select, filesystem], M1)
	),
	maplist(menu_tag, M1, ML),
	tui_menu_tag2(main, ML, MENULABEL, [extra-button, extra-label('Save'), cancel-label('Exit'), title(' Void Linux installation menu ')], Tag),
	action_info(A, Tag, _),
	menu_action(A),
	true.

cmd_menu(root_fs) :- !,
	menu_root_fs,
	true.
cmd_menu(btrfs_opt) :- !,
	menu_btrfs,
	true.
cmd_menu(common_settings) :- !,
	menu_common,
	true.
cmd_menu(template) :- !,
	inst_setting(bootloader, B),
	menu_template(B),
	true.
cmd_menu(keymap) :- !,
	menu_keymap,
	true.
cmd_menu(network) :- !,
	menu_network,
	true.
cmd_menu(source) :- !,
	select_pkg_inst_method,
	true.
cmd_menu(hostname) :- !,
	menu_hostname,
	true.
cmd_menu(locale) :- !,
	menu_locale,
	true.
cmd_menu(timezone) :- !,
	menu_timezone,
	true.
cmd_menu(root_passwd) :- !,
	menu_password(root),
	true.
cmd_menu(user_passwd) :- !,
	inst_setting(useraccount, user(UL, _UN, _UGL)),
	menu_password(UL),
	true.
cmd_menu(useraccount) :- !,
	menu_useraccount,
	true.
cmd_menu(lvm_info) :- !,
	menu_lvm,
	true.
cmd_menu(bootloader) :- !,
	menu_bootloader,
	true.
cmd_menu(bootloader_dev) :- !,
	menu_bootloader_dev,
	true.
cmd_menu(part_manually) :- !,
	menu_part_manually,
	true.
cmd_menu(part_select) :- !,
	menu_part_select,
	true.
cmd_menu(filesystem) :- !,
	menu_filesystem,
	true.
cmd_menu(show) :- !,
	menu_show,
	true.
cmd_menu(save) :- !,
	menu_save,
	true.
cmd_menu(install) :- !,
	run_install,
	true.
cmd_menu(exit) :- !,
	% tui_yesno('Exit installer?', [sz([6, 40])]),
	true.

menu_action(install) :- !,
	cmd_menu(install),
	true.
menu_action(exit) :- !,
	cmd_menu(exit),
	true.
menu_action(A) :- !,
	cmd_menu(A),
	fail.

save_settings(S) :-
	inst_setting(N, V),
	portray_clause(S, inst_setting(N, V)),
	fail.
save_settings(_).

run_install :-
	setup_install,
	ensure_settings,
	validate_fs,
	inst_setting(root_dir, RD),
	clean_mnt(RD),
	wipe_disk,
	disk_partitioning,
	run_mkfs(RD),
	inst_setting(source, IM),
	install_pkg(IM, RD),

	% tui_msgbox('make_fstab', []),
	make_fstab(RD),

	% tui_msgbox('set_keymap', []),
    % set up keymap, locale, timezone, hostname, root passwd and user account.
	set_keymap(RD),

	% tui_msgbox('set_locale', []),
	set_locale(RD),

	% tui_msgbox('set_timezone', []),
	set_timezone(RD),

	% tui_msgbox('set_hostname', []),
	set_hostname(RD),

	% tui_msgbox('set_rootpassword', []),
	set_rootpassword(RD),

	% tui_msgbox('set_useraccount', []),
	set_useraccount(RD),

	% tui_msgbox('cp /mnt/etc/skel/.[bix]* /mnt/root', []),
    % Copy /etc/skel files for root.
	os_shell2([cp, RD + '/etc/skel/.[bix]*', RD + '/root']),

	% tui_msgbox('set_network', []),
    % set network
    set_network(RD),

	% tui_msgbox('set_sudoers', []),
    % set sudoers
	set_sudoers,

    % clean up polkit rule - it's only useful in live systems
	( IM = local ->
	  % tui_msgbox('rm -f /mnt/etc/polkit-1/rules.d/void-live.rules', []),
	  os_rm_f(RD + '/etc/polkit-1/rules.d/void-live.rules')
	; true
	),

    % enable text console for grub if chosen
    % set_grub_text_console(RD),

	% tui_msgbox('set_bootloader', []),
    % install bootloader.
    set_bootloader(RD),

	% tui_msgbox('umount_filesystems', []),
    % unmount all filesystems.
    umount_filesystems(RD),
	tui_msgbox('Void Linux has been installed successfully!', [sz([6, 40])]),
	os_call2([clear]),
	true.

do_install :-
	ux_user_root, !,
	setup_tui,
	menu,
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

version :-
	writenl('version 0.1').

usage :-
	cmd_arg_usage_short(SAL),
	surround_atoms('[', ']', SAL, SAL1),
	join_atoms(['Usage: void-pi'| SAL1], ' ', SAL2),
	writenl(SAL2), nl,
	writenl('Void Linux installer implemented in GNU Prolog.'), nl,
	cmd_arg_usage_long(S),
	writenl(S), nl,
	writenl('Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv3 license'),
	true.

% cmd_arg_info(alias, short, long, value_descr, descr)
% short argument is a character code.
cmd_arg_info(help, 0'h, help, '', 'Show this help and exit.').
cmd_arg_info(version, 0'v, version, '', 'Show the version and exit.').
cmd_arg_info(rootdir, 0'r, rootdir, rootdir, 'Use an alternative rootdir. Acts like xbps\'s -r flag. Default is /mnt.').

% first argument is an alias.
on_cmd_arg(help, _, _) :- !,
	usage,
	fail.
on_cmd_arg(version, _, _) :- !,
	version,
	fail.
on_cmd_arg(rootdir, LI, T) :- !,
	( LI = [RD|T]
	; writenl('rootdir value expected'),
	  fail
	), !,
	retractall(inst_setting(root_dir, _)),
	assertz(inst_setting(root_dir, RD)),
	true.

main :-
	argument_list(AL),
	( handle_cmd_args(AL) ->
	  (F = 'settings.pl', file_exists(F) -> load_config(F); def_settings),
	  do_install,
	  os_call2([clear])
	; true
	),
	halt.
main :-
	writenl('Installer has failed.'),
	halt.
