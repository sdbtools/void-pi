% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://github.com/AdisonCavani/distro-grub-themes

arch2grub(ARCH, GRUB) :-
	% efi-grub depends on bios-grub.
	inst_setting(system(efi), _), !,
	arch2grub_efi(ARCH, GRUB),
	!.
arch2grub(_, grub).

arch2grub_efi('x86_64', 'grub-x86_64-efi').
arch2grub_efi('x86_64-musl', 'grub-x86_64-efi').
arch2grub_efi('i686', 'grub').
arch2grub_efi('armv6l', 'grub-arm-efi'). % ???
arch2grub_efi('armv7l', 'grub-arm-efi'). % ???
arch2grub_efi('aarch64', 'grub-arm64-efi').
% arch2grub_efi('ia32', 'grub-i386-efi').

% Install grub.
% BD is the disk (not a partition)
grub_install(TL, BD, RD) :-
	grub_install_env(TL, ENV),
	( \+ inst_setting(system(efi), _)
	; grub_install_efi(ENV, BD, RD)
	), !,
	( \+ inst_setting(system(bios), _)
	; grub_install_bios(ENV, BD, RD)
	), !,
	true.

% BD is the disk (not a partition)
grub_install_efi(ENV, BD, RD) :-
	inst_setting(system(efi), EFI_TARGET),
	OL = [oo(target, EFI_TARGET), '--efi-directory=/boot/efi', '--bootloader-id=void_grub_efi', '--recheck'],
	CL = [ENV, chroot, RD, 'grub-install', OL, BD, '2>&1'],
	% os_shell2(CL),
	tui_progressbox_safe(CL, '', [title(' Installing GRUB for EFI '), sz([6, 60])]),
	true.

grub_install_bios(ENV, BD, RD) :-
	OL = ['--target=i386-pc', '--bootloader-id=void_grub_bios', '--recheck'],
	CL = [ENV, chroot, RD, 'grub-install', OL, BD, '2>&1'],
	% os_shell2(CL),
	tui_progressbox_safe(CL, '', [title(' Installing GRUB for BIOS '), sz([6, 60])]),
	true.

grub_install_env(TL, ['ZPOOL_VDEV_NAME_PATH=1']) :-
% grub_install_env(TL, ['ZPOOL_VDEV_NAME_GUID=1']) :-
	uses_zfs(TL), !,
	true.
grub_install_env(_TL, []) :-
	true.

grub_config(TL, L) :-
	grub_config_luks(TL, L).
grub_config([
		  v('GRUB_DEFAULT', 0, '')
		, v('GRUB_TIMEOUT', 5, '')
		, v('GRUB_DISTRIBUTOR', 'Void', '')
		, v('GRUB_DISABLE_OS_PROBER', true, '')
		% , v('GRUB_DISABLE_RECOVERY', true, '')
		% , v('GRUB_TERMINAL_INPUT', console, '')
		% , v('GRUB_TERMINAL_OUTPUT', console, '')
		% , v('', '', '')
	]).

grub_config_luks(TL, [
		  v('GRUB_ENABLE_CRYPTODISK', y, '')
		, v('GRUB_CMDLINE_LINUX_DEFAULT', V, 'Generic settings')
	  ]) :-
	uses_luks(TL), !,
	findall(M, (grub_linux_cmdline_luks(TL, L), member(M, L)), VL),
	os_scmdl(VL, V).
grub_config_luks(TL, [
		  v('GRUB_CMDLINE_LINUX_DEFAULT', V, 'Generic settings')
	]) :-
	( root_fs(TL, bcachefs) ->
	  % root_pd(TL, PD),
	  % lx_get_dev_partuuid(PD, PU),
	  % VL = [root=v('PARTUUID', PU), rootfstype=bcachefs, 'rd.luks'=0, loglevel=4]
	  VL = [rootfstype=bcachefs, 'rd.luks'=0, loglevel=4]
	; VL = ['rd.luks'=0, loglevel=4]
	),
	os_scmdl(VL, V).

grub_linux_cmdline_luks(TL, ['rd.luks.name'=v(PUUID, LUKS_PD)]) :-
	member(bdev(luks, luks(_, PD)), TL),
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	true.
grub_linux_cmdline_luks(_TL, ['rd.auto'=1]) :-
	inst_setting(hostonly, no).
% grub_linux_cmdline_luks(_TL, ['rd.debug', rootfstype=zfs, loglevel=4, slub_debug='FZ', slab_nomerge=1, pti=on, mce=0, 'printk.time'=1]).
% grub_linux_cmdline_luks(_TL, [root='/dev/mapper/crypt_sda2', rootfstype=zfs, loglevel=4, slub_debug='FZ', slab_nomerge=1, mce=0, 'printk.time'=1]).
grub_linux_cmdline_luks(_TL, [loglevel=4, slub_debug='FZ', slab_nomerge=1, mce=0, 'printk.time'=1]).

grub_configure(TL, RD) :-
	% Configure grub.
	findall(M, (grub_config(TL, L), member(M, L)), GVL),
	% Generate /etc/default/grub.
	grub_sconf(GVL, RD),
	true.

grub_mkconfig(TL, RD) :-
	grub_install_env(TL, ENV),
	% CL2 = [chroot, RD, 'grub-mkconfig', o(o, '/boot/grub/grub.cfg'), '2>&1'],
	% CL2 = [stdbuf, '-oL', env, ENV, chroot, RD, 'grub-mkconfig', o(o, '/boot/grub/grub.cfg'), '2>&1'],
	CL2 = [ENV, chroot, RD, 'grub-mkconfig', o(o, '/boot/grub/grub.cfg'), '2>&1'],
	% os_shell2(CL2),
	tui_progressbox_safe(CL2, '', [title(' Generating grub configuration file '), sz([10, 60])]),
	% Watch the udev event queue, and exit if all current events are handled.
	os_call2([udevadm, settle]),
	true.

