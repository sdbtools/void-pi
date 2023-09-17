% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

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
	CL = [chroot, RD, ENV, 'grub-install', OL, BD, '2>&1'],
	% os_shell2(CL),
	tui_progressbox_safe(CL, '', [title(' Installing GRUB for EFI '), sz([6, 60])]),
	true.

grub_install_bios(ENV, BD, RD) :-
	OL = ['--target=i386-pc', '--bootloader-id=void_grub_bios', '--recheck'],
	CL = [chroot, RD, ENV, 'grub-install', OL, BD, '2>&1'],
	% os_shell2(CL),
	tui_progressbox_safe(CL, '', [title(' Installing GRUB for BIOS '), sz([6, 60])]),
	true.

grub_install_env(TL, ['ZPOOL_VDEV_NAME_PATH=1']) :-
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
	]).

grub_config_luks(TL, [
		  v('GRUB_ENABLE_CRYPTODISK', y, '')
		, v('GRUB_CMDLINE_LINUX_DEFAULT', V, 'Generic settings')
		, v('GRUB_DISABLE_OS_PROBER', true, '')
		% , v('GRUB_DISABLE_RECOVERY', true, '')
		% , v('GRUB_TERMINAL_INPUT', console, '')
		% , v('GRUB_TERMINAL_OUTPUT', console, '')
		% , v('', '', '')
	  ]) :-
	uses_luks(TL), !,
	findall(M, (grub_linux_cmdline(TL, L), member(M, L)), VL),
	os_scmdl(VL, V).
grub_config_luks(_TL, ['rd.luks'=0, v('GRUB_CMDLINE_LINUX_DEFAULT', 'loglevel=4', '')]).

grub_linux_cmdline(TL, ['rd.luks.name'=v(PUUID, LUKS_PD)]) :-
	member(bdev(luks, luks(_, PD)), TL),
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	true.
grub_linux_cmdline(_TL, [loglevel=4, slub_debug='FZ', slab_nomerge=1, pti=on, mce=0, 'printk.time'=1]).

grub_configure(TL, RD) :-
	% Configure grub.
	findall(M, (grub_config(TL, L), member(M, L)), GVL),
	% Generate /etc/default/grub.
	grub_sconf(GVL, RD),
	true.

grub_mkconfig(RD) :-
	CL2 = [chroot, RD, 'grub-mkconfig', o(o, '/boot/grub/grub.cfg'), '2>&1'],
	% os_shell2(CL2),
	tui_progressbox_safe(CL2, '', [title(' Generating grub configuration file '), sz([10, 60])]),
	os_call2([udevadm, settle]),
	true.

