% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% List of boot managers which require mounting of EFI partition to /boot.
bootloader_boot_efi([syslinux, efistub, gummiboot]).

% bootloader_info(bootloade, supported_fs, supported_template, except_fs).
bootloader_info(grub2, [
		  btrfs
		, ext2
		, ext3
		, ext4
		% , f2fs % drub doesn't support extra_attr
		, swap
		, vfat
		, xfs
		% , zfs
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
		% , gpt_wizard
		% , gpt_raid
		% , gpt_zfsbootmenu
	], []).
bootloader_info(rEFInd, [
		  btrfs
		, ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], []).
bootloader_info(limine, [
		  ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], []).
bootloader_info(efistub, [
		  ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], [
		  btrfs % Error: dracut /sysroot has no proper rootfs layout. Can't mount root filesystem.
	]).
bootloader_info(syslinux, [
		  btrfs
		, ext2
		, ext3
		, ext4
		, f2fs
		% , swap
		, vfat
		, xfs
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], []).
bootloader_info(gummiboot, [
		  btrfs
		, ext2
		, ext3
		, ext4
		, f2fs
		% , swap
		, vfat
		, xfs
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], [
		  btrfs % Error: dracut /sysroot has no proper rootfs layout. Can't mount root filesystem.
	]).
bootloader_info(zfsBootMenu, [
		  zfs
	], [
		  gpt_zfsbootmenu
	], []).

% Get bootloader name/dependency
% target_dep_bootloader(efistub, efibootmgr) :- !. % Already installed.
target_dep_bootloader(gummiboot, gummiboot) :- !.
target_dep_bootloader(syslinux, syslinux) :- !.
target_dep_bootloader(limine, limine) :- !.
target_dep_bootloader(rEFInd, refind) :- !.
target_dep_bootloader(grub2, GRUB) :-
	\+ inst_setting(source, local),
	inst_setting(system(arch), ARCH),
	arch2grub(ARCH, GRUB), !.

get_bootloader(TL, B) :-
	memberchk(bootloader(B), TL).

set_bootloader(TL, RD) :-
	get_bootloader(TL, B),
	memberchk(bootloader_dev(dev3(BD, _, _)), TL),
	set_bootloader(B, TL, BD, RD), !.
set_bootloader(_TL, _RD) :-
	tui_msgbox('Setting up of a bootloader has failed.'),
	fail.

% Get bootloader mount point.
get_bootloader_mp(B, MP) :-
	bootloader_boot_efi(BL),
	( memberchk(B, BL) ->
	  MP = '/boot'
	; MP = '/boot/efi'
	).

% set_bootloader(template_list, bootloader, bootloader_dev, root_dir)
set_bootloader(_, _TL, none, _RD) :- !.
set_bootloader(grub2, TL, BD, RD) :- !,
	grub_configure(TL, RD),
	grub_install(TL, BD, RD),
	grub_mkconfig(RD),
	!.
set_bootloader(rEFInd, TL, _BD, RD) :- !,
	refind_install(TL, RD),
	refind_configure(TL, RD),
	!.
set_bootloader(limine, TL, BD, RD) :- !,
	limine_install(BD, RD),
	limine_configure(TL, RD),
	!.
set_bootloader(efistub, _TL, _BD, _RD) :- !,
	% efistub_install(BD, RD),
	% efistub_configure(TL, RD),
	!.
set_bootloader(syslinux, TL, BD, RD) :- !,
	syslinux_install(BD, RD),
	syslinux_configure(TL, RD),
	!.
set_bootloader(gummiboot, _TL, _BD, RD) :- !,
	gummiboot_install(RD),
	% gummiboot_configure(TL, RD),
	!.

bootloader_kernel_params(TL, [
		  root=v('UUID', RPID)
		, init='/sbin/init'
		, rw
	]) :-
	root_pd(TL, ROOT_PD),
	lx_get_dev_uuid(ROOT_PD, RPID),
	true.
bootloader_kernel_params(TL, L) :-
	% LUKS
	( memberchk(bdev(luks, luks(_, PD)), TL) ->
	  lx_get_dev_uuid(PD, PDID),
	  lx_split_dev(PD, _P, SDN),
      luks_dev_name_short(SDN, LUKS_PD),
	  L = ['rd.luks.name'=v(PDID, LUKS_PD)]
	; L = ['rd.luks'=0]
	),
	true.
bootloader_kernel_params(TL, ['rd.lvm'=0]) :-
	\+ memberchk(bdev(lvm, _Value), TL),
	true.
bootloader_kernel_params(_TL, [
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

bootloader_write_cmdline(TL, S) :-
	findall(P0, (bootloader_kernel_params(TL, PL0), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

reconfig_kernel(RD) :-
	lx_kernel_ver(RD, LV),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-f', concat(linux, LV), '2>&1'], '', [title(' Reconfigure Linux '), sz([18, 60])]),
	true.

setup_bootloader(gummiboot, TL, RD) :- !,
	gummiboot_configure(TL, RD),
	reconfig_kernel(RD),
	true.
setup_bootloader(efistub, _TL, RD) :- !,
	reconfig_kernel(RD),
	efistub_install(RD),
	true.
setup_bootloader(_B, _TL, _RD) :-
	true.
