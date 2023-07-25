% vi: noexpandtab:tabstop=4:ft=gprolog
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

fs2parttype(zfs, solaris_root).
fs2parttype(_, linux).

% need_boot_part(BootLoader, FileSystem).
need_boot_part(B, _FS) :-
	member(B, [rEFInd, limine]),
	inst_setting(template, TT),
	member(TT, [gpt_lvm, gpt_luks1, gpt_luks1_lvm]),
	!.
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
	menu_dev_light(' Select the disk to partition ', dev7(LN,_SN,_TYPE,_RO,_RM,_SIZE,_SSZ)),
	set_bootloader_dev(LN),
	menu_root_fs(TT),
	partition_set_mbr(B, LN, 1, L),
	maplist(part_set_, L),
	true.

on_disable(template(TT)) :- !,
	retractall(inst_setting(template, TT)),
	retractall(inst_setting(fs, _)),
	retractall(inst_setting(bdev, _)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.

% Partition + File System (device_list depends on partition).
% pfs(PartType, FileSystem, Label, MountPoint, create/keep, size)
% p(PartType, create/keep, size)
% fs(FileSystem, Label, MountPoint, [device_list])
% N - partition number.
% vg(Name, [PhysicalVolumeList], [LogicalVolumeList])
partition_set_mbr(B, D, N, L) :-
	inst_setting(system(efi), _), !,
	partition_set_efi(B, D, N, L).
partition_set_mbr(B, D, N, [p4(bios_boot, bd1([PD, D]), keep, MBR_SZ)| L]) :-
	part_name(D, N, PD),
	inst_setting(mbr_size, MBR_SZ),
	N1 is N + 1,
	partition_set_efi(B, D, N1, L).

partition_set_efi(B, D, N, [p4(efi_system, bd1([PD, D]), create, ESP_SZ), fs4(vfat, efi, '/boot/efi', bd1([PD, D]))| L]) :-
	part_name(D, N, PD),
	inst_setting(esp_size, ESP_SZ),
	N1 is N + 1,
	partition_set_boot(B, D, N1, L).

partition_set_boot(B, D, N, [p4(linux, bd1([PD, D]), create, BOOT_SZ), fs4(ext4, boot, '/boot', bd1([PD, D]))| L]) :-
	inst_setting(root_fs, FS),
	need_boot_part(B, FS), !,
	part_name(D, N, PD),
	inst_setting(boot_size, BOOT_SZ),
	N1 is N + 1,
	partition_set_template(D, N1, L).
partition_set_boot(_B, D, N, L) :-
	partition_set_template(D, N, L).

partition_set_template(D, N, [p4(linux_lvm, bd1([PD, D]), create, ''), bdev(lvm, vg(VG, [PD], [lv(LV, SZ)])), fs4(FS, void, '/', bd1([LVM_PD, PD, D]))]) :-
	inst_setting(template, gpt_lvm), !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	part_name(D, N, PD),
	inst_setting(root_fs, FS),
	true.
partition_set_template(D, N, [p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(luks1, PD)), fs4(FS, void, '/', bd1([LUKS_PD, PD, D]))]) :-
	inst_setting(template, gpt_luks1), !,
	part_name(D, N, PD),
	inst_setting(root_fs, FS),
	luks_dev_name(LUKS_PD),
	true.
partition_set_template(D, N, [p4(linux_luks, bd1([PD, D]), create, ''), bdev(luks, luks(luks1, PD)), bdev(lvm, vg(VG, [LUKS_PD], [lv(LV, SZ)])), fs4(FS, void, '/', bd1([LVM_PD, LUKS_PD, PD, D]))]) :-
	inst_setting(template, gpt_luks1_lvm), !,
	inst_setting(lvm, lv(VG, LV, SZ)),
	format_to_atom(LVM_PD, '/dev/mapper/~w-~w', [VG, LV]),
	part_name(D, N, PD),
	inst_setting(root_fs, FS),
	luks_dev_name(LUKS_PD),
	true.
partition_set_template(D, N, [p4(PT, bd1([PD, D]), create, ''), fs4(FS, void, '/', bd1([PD, D]))]) :-
	inst_setting(root_fs, FS),
	part_name(D, N, PD),
	fs2parttype(FS, PT), !.

part_set_(p4(PT, BD1, CK, SZ)) :- !,
	assertz(inst_setting(partition, part4(BD1, PT, CK, SZ))),
	true.
part_set_(fs4(FS, Label, MP, BD1)) :- !,
	assertz(inst_setting(fs, fs4(FS, Label, MP, BD1))),
	true.
part_set_(bdev(Type, Value)) :- !,
	assertz(inst_setting(bdev, bdev(Type, Value))),
	true.

part_name(D, N, PD) :-
	atom_concat('/dev/nvme', _, D), !,
	format_to_atom(PD, '~wp~d', [D, N]).
part_name(D, N, PD) :-
	format_to_atom(PD, '~w~d', [D, N]).


