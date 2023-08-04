% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/Syslinux

syslinux_install(BD, RD) :-
	( inst_setting(system(efi), _) ->
	  syslinux_install_efi(BD, RD)
	; syslinux_install_bios(BD, RD)
	).

syslinux_install_efi(BD, RD) :-
	os_mkdir_p(RD + '/boot/EFI/syslinux'),
	os_shell2([cp, '-r', RD + '/usr/lib/syslinux/efi64/*', RD + '/boot/EFI/syslinux']),
	% os_shell2([efibootmgr, '--create', oo(disk, BD), oo(part, 1), oo(loader, '/EFI/syslinux/syslinux.efi'), oo(label, '"Syslinux"'), '--unicode']),
	tui_progressbox_safe([efibootmgr, '--create', oo(disk, BD), oo(part, 1), oo(loader, '/EFI/syslinux/syslinux.efi'), oo(label, '"Syslinux"'), '2>&1'], 'efibootmgr', [sz([12, 80])]),
	true.

syslinux_install_bios(BD, RD) :-
	os_mkdir_p(RD + '/boot/syslinux'),
	os_shell2([cp, '-r', RD + '/usr/lib/syslinux/*.c32', RD + '/boot/syslinux/']),
	tui_progressbox_safe([chroot, RD, extlinux, '--install', '/boot/syslinux', '2>&1'], '', [sz([6, 60])]),
	os_shell2([sgdisk, BD, '--attributes=1:set:2']),
	% os_shell2([dd, bs=440, count=1, conv=notrunc, if=concat(RD, '/usr/lib/syslinux/gptmbr.bin'), of=BD]),
	os_shell2([chroot, RD, dd, bs=440, count=1, conv=notrunc, if='/usr/lib/syslinux/gptmbr.bin', of=BD]),
	true.

syslinux_configure(TL, RD) :-
	( inst_setting(system(efi), _) ->
	  FN = '/boot/EFI/syslinux/syslinux.cfg'
	; FN = '/boot/syslinux/syslinux.cfg'
	),
	atom_concat(RD, FN, CF),
	atom_concat(RD, '/boot/vmlinuz-', P0),
	atom_concat(P0, '*', P1),
	os_shell2_line([ls, P1], A0),
	atom_concat(P0, V, A0),
	syslinux_configure_(TL, V, CF).

syslinux_configure_(TL, V, CF) :-
	open(CF, write, S),
	syslinux_write_cfg(TL, V, S),
	close(S).

syslinux_write_cfg(TL, V, S) :-
	( inst_setting(system(efi), _) ->
	  Pref = ''
	; Pref = 'boot/'
	),
	write(S, 'PROMPT 1'), nl(S),
	write(S, 'TIMEOUT 50'), nl(S),
	write(S, 'DEFAULT Void'), nl(S), nl(S),
	write(S, 'LABEL Void'), nl(S),
	write(S, '    LINUX /'), write(S, Pref), write(S, 'vmlinuz-'), write(S, V), nl(S),
	write(S, '    INITRD /'), write(S, Pref), write(S, 'initramfs-'), write(S, V), write(S, '.img'), nl(S),
	write(S, '    APPEND '), syslinux_write_cmdline(TL, S), nl(S),
	true.

syslinux_kernel_params(TL, [
		  root=v('UUID', RPID)
		, init='/sbin/init'
		, rw
	]) :-
	root_pd(TL, ROOT_PD),
	lx_get_dev_uuid(ROOT_PD, RPID),
	true.
syslinux_kernel_params(TL, L) :-
	% LUKS
	( memberchk(bdev(luks, luks(luks1, PD)), TL) ->
	  lx_get_dev_uuid(PD, PDID),
	  lx_split_dev(PD, _P, SDN),
      luks_dev_name_short(SDN, LUKS_PD),
	  L = ['rd.luks.name'=v(PDID, LUKS_PD)]
	; L = ['rd.luks'=0]
	),
	true.
syslinux_kernel_params(TL, ['rd.lvm'=0]) :-
	\+ memberchk(bdev(lvm, _Value), TL),
	true.
syslinux_kernel_params(_TL, [
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

syslinux_write_cmdline(TL, S) :-
	findall(P0, (syslinux_kernel_params(TL, PL0), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

