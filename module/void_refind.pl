% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

refind_install(TL, RD) :-
	( memberchk(p4(sys_efi, bd1([EFI_PD| _]), _CK, _SZ), TL)
	; tui_msgbox('efi system partition was not found'),
	  fail
	), !,
	CL1 = [chroot, RD, 'refind-install', oo(usedefault), EFI_PD, '2>&1'],
	tui_progressbox_safe(CL1, '', [title(' Installing bootloader '), sz([6, 60])]),
	% os_shell2(CL1),
	true.

refind_configure(TL, RD) :-
	atom_concat(RD, '/boot/refind_linux.conf', FN),
	open(FN, write, S),

	write(S, '"Boot with standard options" "'),
	refind_write_cfg(TL, S, []),
	write(S, '"'), nl(S),

	write(S, '"Boot to single-user mode" "'),
	refind_write_cfg(TL, S, [single]),
	write(S, '"'), nl(S),

	close(S),
	true.

% It is different from bootloader_kernel_params.
refind_kernel_params(TL, L) :-
	% Device
	root_pd(TL, ROOT_PD),
	( atom_concat('/dev/mapper/', _, ROOT_PD) ->
	  L = [root=ROOT_PD]
	; lx_get_dev_partuuid(ROOT_PD, RPID),
	  L = [root=v('PARTUUID', RPID)]
	),
	true.
refind_kernel_params(TL, L) :-
	% LUKS
	( uses_luks(TL) ->
	  bootloader_kernel_params_luks(TL, L)
	; L = ['rd.luks'=0]
	),
	true.
refind_kernel_params(TL, ['rd.lvm'=0]) :-
	\+ memberchk(bdev(lvm, _Value), TL),
	true.
refind_kernel_params(_TL, [
		  'rd.md'=0
		, 'rd.dm'=0
		, loglevel=4
		, gpt
		, 'vconsole.unicode'=1
		, 'vconsole.keymap'=KB
		, 'locale.LANG'=LC
	]) :-
	inst_setting(keymap, KB),
	inst_setting(locale, LC),
	true.
refind_kernel_params(TL, L) :-
	( root_fs(TL, btrfs), \+ has_boot_part(TL) ->
	  L = [rootflags=v(subvol, '@'), initrd='@\\boot\\initramfs-%v.img']
	; boot_pref(TL, Pref),
	  atom_concat(Pref, 'initramfs-%v.img', BI),
	  L = [initrd=BI]
	).

refind_write_cfg(TL, S, L) :-
	findall(P0, ((refind_kernel_params(TL, PL0); PL0 = L), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

