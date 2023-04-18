% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

refind_install(BD, RD) :-
	% BD is the disk (not a partition)
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	( inst_setting(partition, part(BD, _P, EFI_PD, efi_system, _FS, _Label, _MP, _CK, _SZ))
	; tui_msgbox('efi system partition was not found', []),
	  fail
	), !,
	CL1 = [chroot, RD, 'refind-install', oo(usedefault), EFI_PD, '2>&1'],
	tui_progressbox_safe(CL1, '', [title(' Installing bootloader '), sz([6, 60])]),
	% os_shell2(CL1),
	true.

refind_configure(BD, RD) :-
	( inst_setting(partition, part(BD, _P1, ROOT_PD, _PT1, _FS1, _Label1, '/', _CK1, _SZ1))
	; tui_msgbox('root partition was not found', []),
	  fail
	), !,

	( inst_setting(partition, part(BD, _P2, _ROOT_PD2, _PT2, _FS2, _Label2, '/boot', _CK2, _SZ2)) ->
	  Pref = ''
	; Pref = 'boot\\'
	),
	(
	  lx_get_dev_partuuid(ROOT_PD, RPID),
	  atom_concat(RD, '/boot/refind_linux.conf', FN),
	  open(FN, write, S),

	  write(S, '"Boot with standard options" "'),
	  refind_write_cfg(S, Pref, RPID, []),
	  write(S, '"'), nl(S),

	  write(S, '"Boot to single-user mode" "'),
	  refind_write_cfg(S, Pref, RPID, [single]),
	  write(S, '"'), nl(S),

	  close(S)
	),
	% CL2 = [chroot, RD, mkrlconf, '2>&1'],
	% os_shell2(CL2),
	true.

refind_write_cfg(S, Pref, RPID, L) :-
	inst_setting(keymap, KB),
	inst_setting(locale, LC),
	( inst_setting(root_fs, btrfs) ->
	  L0 = [rootflags=v(subvol, '@'), initrd='@\\boot\\initramfs-%v.img'| L]
	; atom_concat(Pref, 'initramfs-%v.img', BI),
	  L0 = [initrd=BI| L]
	),
	AL = [
		  root=v('PARTUUID', RPID)
		, rw
		, 'rd.luks'=0
		, 'rd.md'=0
		, 'rd.dm'=0
		, loglevel=4
		, gpt
		, add_efi_memmap
		, 'vconsole.unicode'=1
		, 'vconsole.keymap'=KB
		, 'locale.LANG'=LC
		% , 'rd.live.overlay.overlayfs'=1
		| L0
	],
	os_wcmdl(AL, S),
	true.

