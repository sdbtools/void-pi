% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

limine_install(BD, RD) :-
	os_mkdir_p(RD + '/boot/limine'),

	( inst_setting(system(efi), _) ->
	  os_mkdir_p(RD + '/boot/efi/EFI/BOOT'),
	  inst_setting(system(arch), ARCH),
	  arch2limine(ARCH, BL),
	  os_call2([cp, '-f', RD + '/usr/share/limine/' + BL, RD + '/boot/efi/EFI/BOOT/' + BL])
	; tui_progressbox_safe([chroot, RD, 'limine-deploy', BD, '2>&1'], '', [title(' Deploying Limine '), sz([6, 60])]),
	  os_call2([cp, '-f', RD + '/usr/share/limine/limine.sys', RD + '/boot/limine/limine.sys'])
	).

arch2limine('x86_64', 'BOOTX64.EFI').
arch2limine('x86_64-musl', 'BOOTX64.EFI').
arch2limine('i686', 'BOOTIA32.EFI').
arch2limine('aarch64', 'BOOTAA64.EFI').

limine_configure(TL, RD) :-
	atom_concat(RD, '/boot/vmlinuz-', P0),
	atom_concat(P0, '*', P1),
	os_shell2_line([ls, P1], A0),
	atom_concat(P0, V, A0),

	atom_concat(RD, '/boot/limine/limine.cfg', CF),
	open(CF, write, S),
	limine_write_cfg(TL, V, S),
	close(S),
	true.

limine_write_cfg(TL, V, S) :-
	boot_pref(TL, Pref),

	write(S, 'INTERFACE_BRANDING=Void Linux'), nl(S),
	write(S, 'TIMEOUT=5'), nl(S), nl(S),
	write(S, ':Boot with standard options'), nl(S),
	write(S, '    PROTOCOL=linux'), nl(S),
	write(S, '    KERNEL_PATH=boot:///'), write(S, Pref), write(S, 'vmlinuz-'), write(S, V), nl(S),
	write(S, '    MODULE_PATH=boot:///'), write(S, Pref), write(S, 'initramfs-'), write(S, V), write(S, '.img'), nl(S),
	write(S, '    CMDLINE='), limine_write_cmdline(TL, S), nl(S),
	true.

limine_kernel_params(TL, [
		  root=v('UUID', RPID)
		, init='/sbin/init'
		, rw
	]) :-
	root_pd(TL, ROOT_PD),
	lx_get_dev_uuid(ROOT_PD, RPID),
	true.
limine_kernel_params(TL, L) :-
	% LUKS
	( memberchk(bdev(luks, luks(luks1, PD)), TL) ->
	  lx_get_dev_uuid(PD, PDID),
	  lx_split_dev(PD, _P, SDN),
      luks_dev_name_short(SDN, LUKS_PD),
	  L = ['rd.luks.name'=v(PDID, LUKS_PD)]
	; L = ['rd.luks'=0]
	),
	true.
limine_kernel_params(TL, ['rd.lvm'=0]) :-
	\+ memberchk(bdev(lvm, _Value), TL),
	true.
limine_kernel_params(_TL, [
		  'rd.md'=0
		, 'rd.dm'=0
		, loglevel=4
		, gpt
		, add_efi_memmap
		, 'vconsole.unicode'=1
		, 'vconsole.keymap'=KB
		, 'locale.LANG'=LC
		% , 'rd.live.overlay.overlayfs'=1
	]) :-
	inst_setting(keymap, KB),
	inst_setting(locale, LC),
	true.

limine_write_cmdline(TL, S) :-
	findall(P0, (limine_kernel_params(TL, PL0), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

