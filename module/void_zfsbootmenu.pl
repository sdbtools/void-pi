% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

/*
% Original config.yaml
% /etc/zfsbootmenu/config.yaml
Global:
  ManageImages: false
  BootMountPoint: /boot/efi
  DracutConfDir: /etc/zfsbootmenu/dracut.conf.d
  PreHooksDir: /etc/zfsbootmenu/generate-zbm.pre.d
  PostHooksDir: /etc/zfsbootmenu/generate-zbm.post.d
  InitCPIOConfig: /etc/zfsbootmenu/mkinitcpio.conf
Components:
  ImageDir: /boot/efi/EFI/zbm
  Versions: 3
  Enabled: true
  syslinux:
    Config: /boot/syslinux/syslinux.cfg
    Enabled: false
EFI:
  ImageDir: /boot/efi/EFI/zbm
  Versions: false
  Enabled: false
Kernel:
  CommandLine: ro quiet loglevel=0
*/

zfsbootmenu_install(BD, RD) :-
	os_call2([zfs, set, 'org.zfsbootmenu:commandline="quiet loglevel=4"', 'zroot/ROOT']),
	tui_progressbox_safe([chroot, RD, 'generate-zbm', '2>&1'], '', [title(' generate-zbm '), sz([6, 60])]),
	tui_progressbox_safe([efibootmgr, '--create', oo(disk, BD), oo(part, 1), oo(loader, '/EFI/zbm/vmlinuz-backup.EFI'), oo(label, '"ZFSBootMenu (Backup)"'), '2>&1'], 'efibootmgr', [sz([12, 80])]),
	tui_progressbox_safe([efibootmgr, '--create', oo(disk, BD), oo(part, 1), oo(loader, '/EFI/zbm/vmlinuz.EFI'), oo(label, '"ZFSBootMenu"'), '2>&1'], 'efibootmgr', [sz([12, 80])]),
	true.

zfsbootmenu_configure(_TL, RD) :-
	atom_concat(RD, '/etc/zfsbootmenu/config.yaml', FN),
	open(FN, write, S),

	write(S, 'Global:'), nl(S),
	write(S, '  ManageImages: true'), nl(S),
	write(S, '  BootMountPoint: /boot/efi'), nl(S),
	write(S, 'Components:'), nl(S),
	write(S, '  Enabled: false'), nl(S),
	write(S, 'EFI:'), nl(S),
	write(S, '  ImageDir: /boot/efi/EFI/zbm'), nl(S),
	write(S, '  Versions: false'), nl(S),
	write(S, '  Enabled: true'), nl(S),
	write(S, 'Kernel:'), nl(S),
	write(S, '  CommandLine: quiet loglevel=0'), nl(S),

	close(S),
	true.

