% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% template_info(name, descr, except_fs).
template_info(manual, 'Manual configuration of everything', []).
template_info(gpt_basic, 'GPT. One device', [swap, vfat]).
template_info(gpt_lvm, 'GPT. LVM. One device', [swap, vfat]).
template_info(gpt_luks1, 'GPT. LUKS1. One device', [swap, vfat]).
template_info(gpt_luks1_lvm, 'GPT. LUKS1. LVM. One device', [swap, vfat]).
template_info(gpt_raid, 'GPT. RAID. One device', [swap]).
template_info(gpt_zfsbootmenu, 'GPT. ZFS. One device', []).

setup_fs_template :-
	% subv(name, mount_point, mount_attrs, cow)
	assertz(inst_setting(btrfs, subv('@', mp('/'), [rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	% assertz(inst_setting(btrfs, subv('@home', mp('/home'), [nosuid, nodev, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@home', mp('/home'), [nosuid, nodev, rw, noatime, space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@opt', mp('/opt'), [nodev, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@srv', mp('/srv'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var', mp('/var'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@var-cache-xbps', mp('/var/cache/xbps'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@var-lib-ex', mp('/var/lib/ex'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-log', mp('/var/log'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-opt', mp('/var/opt'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], cow))),
	assertz(inst_setting(btrfs, subv('@var-spool', mp('/var/spool'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@var-tmp', mp('/var/tmp'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),
	assertz(inst_setting(btrfs, subv('@snapshots', mp('/.snapshots'), [nosuid, nodev, noexec, rw, noatime, 'compress-force=zstd:3', space_cache=v2], nodatacow))),

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

switch_template(OT, OT, _B) :- !.
switch_template(OT, NT, B) :-
	on_disable(template(OT)),
	on_enable(template(NT), B),
	!.

% Partition + File System.
% pfs(PartType, FileSystem, Label, MountPoint, create/keep, size)
% p(PartType, create/keep, size)
% fs(FileSystem, Label, MountPoint, [device_list])
partition_set(_B, [pfs(bios_boot, keep, MBR_SZ)]) :-
	% MBR doesn't have a file system.
	\+ inst_setting(system(efi), _),
	inst_setting(mbr_size, MBR_SZ),
	true.
partition_set(_B, [pfs(efi_system, vfat, efi, '/boot/efi', create, ESP_SZ)]) :-
	inst_setting(esp_size, ESP_SZ),
	true.
partition_set(B, [pfs(linux, ext4, boot, '/boot', create, BOOT_SZ)]) :-
	inst_setting(root_fs, FS),
	need_boot_part(B, FS),
	inst_setting(boot_size, BOOT_SZ),
	true.
partition_set(_B, [p(linux_lvm, create, ''), bdev(lvm, vg(VG)), bdev(lvm, lv(LV, VG, SZ)), fs(FS, void, '/', LVM_PD)]) :-
	inst_setting(template, gpt_lvm), !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	inst_setting(root_fs, FS),
	true.
partition_set(_B, [p(linux_luks, create, ''), bdev(luks, luks(luks1)), fs(FS, void, '/', LUKS_PD)]) :-
	inst_setting(template, gpt_luks1), !,
	inst_setting(root_fs, FS),
	luks_dev_name(LUKS_PD),
	true.
partition_set(_B, [p(linux_luks, create, ''), bdev(luks, luks(luks1)), bdev(lvm, vg(VG)), bdev(lvm, lv(LV, VG, SZ)), fs(FS, void, '/', [LVM_PD, LUKS_PD])]) :-
	inst_setting(template, gpt_luks1_lvm), !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/~w/~w', [VG, LV]),
	inst_setting(root_fs, FS),
	luks_dev_name(LUKS_PD),
	true.
partition_set(_B, [pfs(linux_raid, raid, void, '/', create, '')]) :-
	inst_setting(template, gpt_raid), !,
	true.
partition_set(_B, [pfs(PT, FS, void, '/', create, '')]) :-
	inst_setting(root_fs, FS),
	fs2parttype(FS, PT), !.

fs2parttype(zfs, solaris_root).
fs2parttype(_, linux).

% need_boot_part(BootLoader, FileSystem).
need_boot_part(B, FS) :-
	% bootloader_info(bootloade, supported_fs, supported_template).
	bootloader_info(B, FSL, _),
	\+ member(FS, FSL),
	!.

on_enable(template(manual), _B) :- !,
	assertz(inst_setting(template, manual)),
	true.
on_enable(template(gpt_zfsbootmenu), _B) :- !,
	assertz(inst_setting(template, gpt_zfsbootmenu)),
	tui_msgbox2([not, implemented, yet]),
	true.
on_enable(template(TT), B) :- !,
	assertz(inst_setting(template, TT)),
	menu_dev_light(' Select the disk to partition ', D),
	set_bootloader_dev(D),
	menu_root_fs(TT),
	findall(P0, (partition_set(B, PL0), member(P0, PL0)), BPL2),
	part_template(D, BPL2),
	% part_template2(D, BPL2, PL),
	% assertz(inst_setting(partition, list(PL))),
	true.

on_disable(template(TT)) :- !,
	retractall(inst_setting(template, TT)),
	retractall(inst_setting(fs, _)),
	retractall(inst_setting(bdev, _)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.

part_template(D, L) :-
	lx_split_dev(D, P, S),
	part_template_(L, 0, D, P, S),
	true.

% Partition + File System.
% pfs(PartType, FileSystem, Label, MountPoint, create/keep, size)
% Partition only.
% p(PartType, create/keep, size)
% N - number
% D - device
% P - prefix
% S - suffix
part_template_([pfs(PT, FS, Label, MP, CK, SZ)| T], N, D, P, S) :-
	N1 is N + 1,
	format_to_atom(S1, '~w~d', [S, N1]),
	atom_concat(P, S1, PD),
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	assertz(inst_setting(partition, part4(bd1([PD, D]), PT, CK, SZ))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	assertz(inst_setting(fs, fs4(FS, Label, MP, bd1([PD, D])))),
	part_template_(T, N1, D, P, S),
	true.
part_template_([p(PT, CK, SZ)| T], N, D, P, S) :-
	N1 is N + 1,
	format_to_atom(S1, '~w~d', [S, N1]),
	atom_concat(P, S1, PD),
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	assertz(inst_setting(partition, part4(bd1([PD, D]), PT, CK, SZ))),
	part_template_(T, N1, D, P, S),
	true.
part_template_([fs(FS, Label, MP, PD)| T], N, D, P, S) :-
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	assertz(inst_setting(fs, fs4(FS, Label, MP, bd1([PD, D])))),
	part_template_(T, N, D, P, S),
	true.
% Retrieve last partition name.
part_template_([bdev(lvm, vg(VG)), bdev(lvm, CMD), fs(FS, Label, MP, LVM_PD)| T], N, D, P, S) :-
	format_to_atom(S1, '~w~d', [S, N]),
	atom_concat(P, S1, PD),
	assertz(inst_setting(bdev, bdev(lvm, vg(VG, [PD])))),
	assertz(inst_setting(bdev, bdev(lvm, CMD))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	assertz(inst_setting(fs, fs4(FS, Label, MP, bd1([LVM_PD, PD, D])))),
	part_template_(T, N, D, P, S),
	true.
% Retrieve last partition name.
part_template_([bdev(luks, luks(Type)), fs(FS, Label, MP, LUKS_PD)| T], N, D, P, S) :-
	format_to_atom(S1, '~w~d', [S, N]),
	atom_concat(P, S1, PD),
	assertz(inst_setting(bdev, bdev(luks, luks(Type, PD)))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	assertz(inst_setting(fs, fs4(FS, Label, MP, bd1([LUKS_PD, PD, D])))),
	part_template_(T, N, D, P, S),
	true.
% Retrieve last partition name.
part_template_([bdev(luks, luks(Type)), bdev(lvm, vg(VG)), bdev(lvm, CMD), fs(FS, Label, MP, [LVM_PD, LUKS_PD])| T], N, D, P, S) :-
	format_to_atom(S1, '~w~d', [S, N]),
	atom_concat(P, S1, PD),
	assertz(inst_setting(bdev, bdev(luks, luks(Type, PD)))),
	assertz(inst_setting(bdev, bdev(lvm, vg(VG, [LUKS_PD])))),
	assertz(inst_setting(bdev, bdev(lvm, CMD))),
	% fs4(Name, Label, MountPoint, bd1([PartDev, Dev]))
	assertz(inst_setting(fs, fs4(FS, Label, MP, bd1([LVM_PD, LUKS_PD, PD, D])))),
	part_template_(T, N, D, P, S),
	true.
part_template_([bdev(BD, CMD)| T], N, D, P, S) :-
	assertz(inst_setting(bdev, bdev(BD, CMD))),
	part_template_(T, N, D, P, S),
	true.
part_template_([], _, _, _, _).

