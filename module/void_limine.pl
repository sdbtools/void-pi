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

limine_configure(RD) :-
	atom_concat(RD, '/boot/vmlinuz-', P0),
	atom_concat(P0, '*', P1),
	os_shell2_line([ls, P1], A0),
	atom_concat(P0, V, A0),

	atom_concat(RD, '/boot/limine/limine.cfg', CF),
	open(CF, write, S),
	limine_write_cfg(V, S),
	close(S),
	true.

limine_write_cfg(V, S) :-
	boot_pref(Pref),

	write(S, 'INTERFACE_BRANDING=Void Linux'), nl(S),
	write(S, 'TIMEOUT=5'), nl(S), nl(S),
	write(S, ':Boot with standard options'), nl(S),
	write(S, '    PROTOCOL=linux'), nl(S),
	write(S, '    KERNEL_PATH=boot:///'), write(S, Pref), write(S, 'vmlinuz-'), write(S, V), nl(S),
	write(S, '    MODULE_PATH=boot:///'), write(S, Pref), write(S, 'initramfs-'), write(S, V), write(S, '.img'), nl(S),
	write(S, '    CMDLINE='), limine_write_cmdline(S), nl(S),
	true.

limine_kernel_params([
		  root=v('UUID', RPID)
		, init='/sbin/init'
		, rw
	]) :-
	root_pd(ROOT_PD),
	lx_get_dev_uuid(ROOT_PD, RPID),
	true.
limine_kernel_params(L) :-
	% LUKS
	( inst_setting(bdev, bdev(luks, luks(luks1, PD))) ->
	  lx_get_dev_uuid(PD, PDID),
	  inst_setting(luks, luks(Name)),
	  L = ['rd.luks.name'=v(PDID, Name)]
	; L = ['rd.luks'=0]
	),
	true.
limine_kernel_params(['rd.lvm'=0]) :-
	\+ inst_setting(bdev, bdev(lvm, _Value)),
	true.
limine_kernel_params([
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

limine_write_cmdline(S) :-
	findall(P0, (limine_kernel_params(PL0), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

