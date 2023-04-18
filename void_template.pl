% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% template_info(name, descr, except_fs).
template_info(manual, 'Manual configuration of everything', []).
template_info(gpt_basic, 'GPT. One device', [swap, vfat]).
template_info(gpt_lvm, 'GPT. LVM. One device', [swap, btrfs, vfat]).
template_info(gpt_luks1, 'GPT. LUKS1. One device', [swap, vfat]).
template_info(gpt_raid, 'GPT. RAID. One device', [swap]).
template_info(gpt_zfsbootmenu, 'GPT. ZFS. One device', []).

switch_template(OT, OT, _B) :- !.
switch_template(OT, NT, B) :-
	on_disable(template(OT)),
	on_enable(template(NT), B),
	!.

% pi(PartType, FileSystem, Label, MountPoint, create/keep, size)
partition_set(_B, [pi(bios_boot, '', 'BIOS boot', '', keep, MBR_SZ)]) :-
	\+ inst_setting(system(efi), _),
	inst_setting(mbr_size, MBR_SZ),
	true.
partition_set(_B, [pi(efi_system, vfat, efi, '/boot/efi', create, ESP_SZ)]) :-
	inst_setting(esp_size, ESP_SZ),
	true.
partition_set(B, [pi(linux, ext4, boot, '/boot', create, BOOT_SZ)]) :-
	inst_setting(root_fs, FS),
	need_boot_part(B, FS),
	inst_setting(boot_size, BOOT_SZ),
	true.
partition_set(_B, [pi(linux_lvm, lvm, void, '/', create, '')]) :-
	inst_setting(template, gpt_lvm), !,
	true.
partition_set(_B, [pi(linux_luks, luks1, void, '/', create, '')]) :-
	inst_setting(template, gpt_luks1), !,
	true.
partition_set(_B, [pi(PT, FS, void, '/', create, '')]) :-
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
	% dev-time settings.
	% set_bootloader_dev('/dev/sda'),
	% assertz(inst_setting(partition, part('/dev/sda', sda1, '/dev/sda1', efi_system, vfat, efi, '/boot/efi', create, _))),
	% assertz(inst_setting(partition, part('/dev/sda', sda2, '/dev/sda2', swap, swap, swap, '', create, _))),
	% assertz(inst_setting(partition, part('/dev/sda', sda3, '/dev/sda3', linux, ext4, void, '/', create, _))),
	true.
on_enable(template(gpt_basic), B) :- !,
	TT = gpt_basic,
	assertz(inst_setting(template, TT)),
	menu_dev_light(' Select the disk to partition ', D),
	set_bootloader_dev(D),

	menu_root_fs(TT),

	findall(P0, (partition_set(B, PL0), member(P0, PL0)), BPL2),
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
on_enable(template(gpt_lvm), B) :- !,
	TT = gpt_lvm,
	assertz(inst_setting(template, TT)),
	menu_dev_light(' Select the disk to partition ', D),
	set_bootloader_dev(D),

	menu_root_fs(TT),

	findall(P0, (partition_set(B, PL0), member(P0, PL0)), BPL2),
	part_template(D, BPL2),

	true.
on_enable(template(gpt_luks1), B) :- !,
	TT = gpt_luks1,
	assertz(inst_setting(template, TT)),
	menu_dev_light(' Select the disk to partition ', D),
	set_bootloader_dev(D),

	menu_root_fs(TT),

	findall(P0, (partition_set(B, PL0), member(P0, PL0)), BPL2),
	part_template(D, BPL2),

	true.
on_enable(template(gpt_raid), _B) :- !,
	TT = gpt_raid,
	assertz(inst_setting(template, TT)),
	menu_dev_light(' Select the disk to partition ', D),
	set_bootloader_dev(D),

	menu_root_fs(TT),

	inst_setting(esp_size, ESP_SZ),
	inst_setting(boot_size, BOOT_SZ),
	% pi(PartType, FileSystem, Label, MountPoint, create/keep, size)
	BPL1 = [
		  pi(efi_system, vfat, efi, '/boot/efi', create, ESP_SZ)
		, pi(linux, ext4, boot, '/boot', create, BOOT_SZ)
		, pi(linux_raid, luks1, void, '/', create, '')
	],
	( inst_setting(system(efi), _) ->
	  BPL2 = BPL1
	; inst_setting(mbr_size, MBR_SZ),
	  BPL2 = [pi(bios_boot, '', 'BIOS boot', '', keep, MBR_SZ)|BPL1]
	),
	part_template(D, BPL2),

	true.
on_enable(template(gpt_zfsbootmenu), _B) :- !,
	assertz(inst_setting(template, gpt_zfsbootmenu)),
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
on_disable(template(gpt_basic)) :- !,
	retractall(inst_setting(template, gpt_basic)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(btrfs, _)),
	retractall(inst_setting(zfs, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(gpt_lvm)) :- !,
	retractall(inst_setting(template, gpt_lvm)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(gpt_luks1)) :- !,
	retractall(inst_setting(template, gpt_luks1)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(gpt_raid)) :- !,
	retractall(inst_setting(template, gpt_raid)),
	retractall(inst_setting(partition, _)),
	retractall(inst_setting(bootloader_dev, _)),
	true.
on_disable(template(gpt_zfsbootmenu)) :- !,
	retractall(inst_setting(template, gpt_zfsbootmenu)),
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

template_to_menu(T, [T, Descr]) :-
	template_info(T, Descr, _),
	true.

