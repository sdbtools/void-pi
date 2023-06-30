% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

refind_install(RD) :-
	% part4(bd1([PartDev, Dev]), PartType, create/keep, size)
	( inst_setting(partition, part4(bd1([EFI_PD| _]), efi_system, _CK, _SZ))
	; tui_msgbox('efi system partition was not found'),
	  fail
	), !,
	CL1 = [chroot, RD, 'refind-install', oo(usedefault), EFI_PD, '2>&1'],
	tui_progressbox_safe(CL1, '', [title(' Installing bootloader '), sz([6, 60])]),
	% os_shell2(CL1),
	true.

refind_configure(RD) :-
	atom_concat(RD, '/boot/refind_linux.conf', FN),
	open(FN, write, S),

	write(S, '"Boot with standard options" "'),
	refind_write_cfg(S, []),
	write(S, '"'), nl(S),

	write(S, '"Boot to single-user mode" "'),
	refind_write_cfg(S, [single]),
	write(S, '"'), nl(S),

	close(S),
	true.

refind_kernel_params(L) :-
	% Device
	root_pd(ROOT_PD),
	( atom_concat('/dev/mapper/', _, ROOT_PD) ->
	  L = [root=ROOT_PD]
	; lx_get_dev_partuuid(ROOT_PD, RPID),
	  L = [root=v('PARTUUID', RPID)]
	),
	true.
refind_kernel_params(L) :-
	% LUKS
	( inst_setting(bdev, bdev(luks, luks(luks1, PD))) ->
	  lx_get_dev_uuid(PD, PDID),
	  inst_setting(luks, luks(Name)),
	  L = ['rd.luks.name'=v(PDID, Name)]
	; L = ['rd.luks'=0]
	),
	true.
refind_kernel_params(['rd.lvm'=0]) :-
	\+ inst_setting(bdev, bdev(lvm, _Value)),
	true.
refind_kernel_params([
		  'rd.md'=0
		, 'rd.dm'=0
		, loglevel=4
		, gpt
		, add_efi_memmap
		, 'vconsole.unicode'=1
		, 'vconsole.keymap'=KB
		, 'locale.LANG'=LC
	]) :-
	inst_setting(keymap, KB),
	inst_setting(locale, LC),
	true.
refind_kernel_params(L) :-
	( inst_setting(root_fs, btrfs), \+ has_boot_part ->
	  L = [rootflags=v(subvol, '@'), initrd='@\\boot\\initramfs-%v.img']
	; boot_pref(Pref),
	  atom_concat(Pref, 'initramfs-%v.img', BI),
	  L = [initrd=BI]
	).

refind_write_cfg(S, L) :-
	findall(P0, ((refind_kernel_params(PL0); PL0 = L), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

