% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

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

zfsbootmenu_install(TL, BD, RD) :-
	os_call2([zfs, set, 'org.zfsbootmenu:commandline="quiet loglevel=4"', 'zroot/ROOT']),
	( \+ inst_setting(system(efi), _)
	; zfsbootmenu_install_efi(BD, RD)
	), !,
	( \+ inst_setting(system(bios), _)
	; zfsbootmenu_install_bios(TL, BD, RD)
	), !,
	true.

zfsbootmenu_install_efi(BD, RD) :-
	tui_progressbox_safe([chroot, RD, 'generate-zbm', '2>&1'], '', [title(' generate-zbm '), sz([6, 60])]),
	tui_progressbox_safe([chroot, RD, efibootmgr, '--create', oo(disk, BD), oo(part, 1), oo(loader, '/EFI/zbm/vmlinuz-backup.EFI'), oo(label, '"ZFSBootMenu (Backup)"'), '2>&1'], 'efibootmgr', [sz([12, 80])]),
	tui_progressbox_safe([chroot, RD, efibootmgr, '--create', oo(disk, BD), oo(part, 1), oo(loader, '/EFI/zbm/vmlinuz.EFI'), oo(label, '"ZFSBootMenu"'), '2>&1'], 'efibootmgr', [sz([12, 80])]),
	true.

zfsbootmenu_install_bios(TL, BD, RD) :-
	soft_install_soft_chroot([syslinux], RD),
	syslinux_install_bios(TL, BD, RD),
	tui_progressbox_safe(['xbps-reconfigure', o(r, RD), '-f', zfsbootmenu, '2>&1'], '', [title(' Reconfigure zfsBootMenu '), sz([18, 60])]),
	true.

zfsbootmenu_configure(RD) :-
	os_mkdir_p(RD + '/etc/zfsbootmenu'),
	atom_concat(RD, '/etc/zfsbootmenu/config.yaml', FN),
	open(FN, write, S),
	( \+ inst_setting(system(efi), _)
	; zfsbootmenu_configure_efi(S)
	), !,
	( \+ inst_setting(system(bios), _)
	; zfsbootmenu_configure_bios(S),
	  zfsbootmenu_configure_syslinux(RD)
	), !,
	close(S),
	true.

zfsbootmenu_configure_efi(S) :-
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
	true.

zfsbootmenu_configure_bios(S) :-
	write(S, 'Global:'), nl(S),
	write(S, '  ManageImages: true'), nl(S),
	write(S, '  BootMountPoint: /boot/syslinux'), nl(S),
	write(S, 'Components:'), nl(S),
	write(S, '  Enabled: true'), nl(S),
	write(S, '  Versions: false'), nl(S),
	write(S, '  ImageDir: /boot/syslinux/zfsbootmenu'), nl(S),
	true.

zfsbootmenu_configure_syslinux(RD) :-
	os_mkdir_p(RD + '/boot/syslinux'),
	atom_concat(RD, '/boot/syslinux/syslinux.cfg', FN),
	open(FN, write, S),

	write(S, 'UI menu.c32'), nl(S),
	write(S, 'PROMPT 0'), nl(S),
	nl(S),
	write(S, 'MENU TITLE ZFSBootMenu'), nl(S),
	write(S, 'TIMEOUT 50'), nl(S),
	nl(S),
	write(S, 'DEFAULT zfsbootmenu'), nl(S),
	nl(S),
	write(S, 'LABEL zfsbootmenu'), nl(S),
	write(S, '  MENU LABEL ZFSBootMenu'), nl(S),
	write(S, '  KERNEL /zfsbootmenu/vmlinuz-bootmenu'), nl(S),
	write(S, '  INITRD /zfsbootmenu/initramfs-bootmenu.img'), nl(S),
	write(S, '  APPEND zfsbootmenu quiet loglevel=4'), nl(S),
	nl(S),
	write(S, 'LABEL zfsbootmenu-backup'), nl(S),
	write(S, '  MENU LABEL ZFSBootMenu (Backup)'), nl(S),
	write(S, '  KERNEL /zfsbootmenu/vmlinuz-bootmenu-backup'), nl(S),
	write(S, '  INITRD /zfsbootmenu/initramfs-bootmenu-backup.img'), nl(S),
	write(S, '  APPEND zfsbootmenu quiet loglevel=4'), nl(S),
	write(S, 'EOF'), nl(S),

	close(S),
	true.

