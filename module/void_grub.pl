% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

arch2grub(_, grub) :-
	\+ inst_setting(system(efi), _), !.
arch2grub('x86_64', 'grub-x86_64-efi') :- !.
arch2grub('x86_64-musl', 'grub-x86_64-efi') :- !.
arch2grub('i686', 'grub') :- !.
arch2grub('armv6l', 'grub-arm-efi') :- !. % ???
arch2grub('armv7l', 'grub-arm-efi') :- !. % ???
arch2grub('aarch64', 'grub-arm64-efi') :- !.
% arch2grub('ia32', 'grub-i386-efi') :- !.

grub_install(BD, RD) :-
	% BD is the disk (not a partition)
	% Install grub.
	grub_install_opt(O),
	grub_install_env(ENV),
	CL1 = [chroot, RD, ENV, 'grub-install', O, BD, '2>&1'],
	% os_shell2(CL1),
	tui_progressbox_safe(CL1, '', [title(' Installing bootloader '), sz([6, 60])]),
	true.

grub_install_opt([oo(target, EFI_TARGET), '--efi-directory=/boot/efi', '--bootloader-id=void_grub', '--recheck']) :-
	inst_setting(system(efi), EFI_TARGET), !,
	true.
grub_install_opt([]) :-
	true.

grub_install_env(['ZPOOL_VDEV_NAME_PATH=1']) :-
	uses_zfs, !,
	true.
grub_install_env([]) :-
	true.

grub_configure(BD, RD) :-
	% Configure grub.
	GVL0 = [
		  v('GRUB_DEFAULT', 0, '')
		, v('GRUB_TIMEOUT', 5, '')
		, v('GRUB_DISTRIBUTOR', 'Void', '')
	],
	( uses_luks ->
	  root_pd(BD, ROOT_PD),
	  lx_get_dev_uuid(ROOT_PD, PUUID),
	  luks_dev_name(LUKS_PD),
	  os_scmdl([
		  'rd.luks'=1
		, 'rd.luks.name'=v(PUUID, LUKS_PD)
		, loglevel=6
		, slub_debug='FZ'
		, slab_nomerge=1
		, pti=on
		, mce=0
		, 'printk.time'=1
	  ], V),
	  GVL = [
		  v('GRUB_ENABLE_CRYPTODISK', y, '')
		, v('GRUB_CMDLINE_LINUX_DEFAULT', V, 'Generic settings')
		, v('GRUB_DISABLE_OS_PROBER', true, '')
		% , v('GRUB_DISABLE_RECOVERY', true, '')
		% , v('GRUB_TERMINAL_INPUT', console, '')
		% , v('GRUB_TERMINAL_OUTPUT', console, '')
		% , v('', '', '')
		| GVL0
	  ]
	; GVL = [
		  v('GRUB_CMDLINE_LINUX_DEFAULT', 'loglevel=4', '')
		| GVL0
	  ]
	),

	% Generate /etc/default/grub.
	grub_sconf(GVL, RD),
	true.

grub_mkconfig(RD) :-
	CL2 = [chroot, RD, 'grub-mkconfig', o(o, '/boot/grub/grub.cfg'), '2>&1'],
	% os_shell2(CL2),
	tui_progressbox_safe(CL2, '', [title(' Generating grub configuration file '), sz([10, 60])]),
	os_call2([udevadm, settle]),
	true.

