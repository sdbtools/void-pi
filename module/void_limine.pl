% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

limine_install(BD, RD) :-
	os_mkdir_p(RD + '/boot/limine'),
	( \+ inst_setting(system(efi), _)
	; limine_install_efi(RD)
	), !,
	( \+ inst_setting(system(bios), _)
	; limine_install_bios(BD, RD)
	), !,
	true.

limine_install_efi(RD) :-
	os_mkdir_p(RD + '/boot/efi/EFI/BOOT'),
	inst_setting(system(arch), ARCH),
	arch2limine(ARCH, BL),
	os_call2([cp, '-f', RD + '/usr/share/limine/' + BL, RD + '/boot/efi/EFI/BOOT/' + BL]),
	true.

limine_install_bios(BD, RD) :-
	tui_progressbox_safe([chroot, RD, 'limine', 'bios-install', BD, '2>&1'], '', [title(' Installing Limine '), sz([6, 60])]),
	os_call2([cp, '-f', RD + '/usr/share/limine/limine-bios.sys', RD + '/boot/limine/limine-bios.sys']),
	true.

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
	write(S, '    CMDLINE='), bootloader_write_cmdline(TL, S), nl(S),
	true.

