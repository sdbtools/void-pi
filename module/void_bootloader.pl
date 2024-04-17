% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% List of boot managers which require mounting of EFI partition to /boot.
bootloader_boot_efi([syslinux, efistub, gummiboot]).

% bootloader_info(bootloader, supported_fs, supported_template, except_fs).
bootloader_info(grub2, [
		  btrfs
		, ext2
		, ext3
		, ext4
		% , f2fs % drub doesn't support extra_attr
		, swap
		, vfat
		% , xfs % GRUB2 Fails to boot off XFS Partition (https://bugzilla.redhat.com/show_bug.cgi?id=2254370)
		, zfs
		, nilfs2
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
		% , gpt_wizard
		% , gpt_raid
	], [
		  exfat
	]).
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
	], [
		  exfat
	]).
bootloader_info(limine, [
		  vfat
		% , ext2 % Limine extX support is removed in 6.20231210.0 as per the ChangeLog.
		% , ext3
		% , ext4
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], [
		  btrfs % Error: dracut /sysroot has no proper rootfs layout. Can't mount root filesystem.
		, exfat
	]).
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
		, exfat
	]).
bootloader_info(syslinux, [
		  % btrfs % Only uncompressed single-device Btrfs is supported.
		  ext2
		, ext3
		, ext4
		% , f2fs % Not supported by syslinux
		% , swap
		, vfat
		% , xfs
		% , ufs
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], [
		  % btrfs % It boots with the current configuration of btrfs + EFI. Doesn't boot with BIOS.
		  btrfs % Error: dracut /sysroot has no proper rootfs layout. Can't mount root filesystem.
		, exfat
		% , f2fs % Not supported by syslinux
		% , xfs % Won't boot with BIOS
	]).
bootloader_info(gummiboot, [
		  btrfs
		, ext2
		, ext3
		, ext4
		, f2fs
		% , swap
		, vfat
		, xfs
		, nilfs2
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], [
		  exfat
		, btrfs % Error: dracut /sysroot has no proper rootfs layout. Can't mount root filesystem.
	]).
bootloader_info(zfsBootMenu, [
		  zfs
	], [
		  % manual
		  gpt_basic
	], []).

% Get bootloader name/dependency
target_dep_bootloader(efistub, [efibootmgr]) :- !.
target_dep_bootloader(zfsBootMenu, [zfsbootmenu, efibootmgr, 'gummiboot-efistub']) :- !.
target_dep_bootloader(gummiboot, [gummiboot]) :- !.
target_dep_bootloader(syslinux, [syslinux, efibootmgr]) :- !.
target_dep_bootloader(limine, [limine]) :- !.
target_dep_bootloader(rEFInd, [refind]) :- !.
target_dep_bootloader(grub2, [GRUB]) :-
	inst_setting(system(arch), ARCH),
	arch2grub(ARCH, GRUB), !.

get_bootloader(TL, B) :-
	memberchk(bootloader(B), TL).

get_bootloader_dev7(TL, DEV7) :-
	memberchk(bootloader_dev7(DEV7), TL).

install_bootloader(TL, _RD) :-
	% Do not install bootloader if a bootloader dev has not been selected.
	\+ get_bootloader_dev7(TL, _), !.
install_bootloader(TL, RD) :-
	get_bootloader(TL, B),
	get_bootloader_dev7(TL, DEV7),
	lx_dev7_to_ldn(DEV7, BD),
	install_bootloader(B, TL, BD, RD), !.
install_bootloader(_TL, _RD) :-
	tui_msgbox('Setting up of a bootloader has failed.'),
	fail.

% Get bootloader mount point.
get_bootloader_mp(B, MP) :-
	bootloader_boot_efi(BL),
	( memberchk(B, BL) ->
	  MP = '/boot'
	; MP = '/boot/efi'
	).

% install_bootloader(bootloader, template_list, bootloader_dev, root_dir)
install_bootloader(_, _TL, none, _RD) :- !.
install_bootloader(grub2, TL, BD, RD) :- !,
	grub_configure(TL, RD),
	grub_install(TL, BD, RD),
	grub_mkconfig(TL, RD),
	!.
install_bootloader(rEFInd, TL, _BD, RD) :- !,
	refind_install(TL, RD),
	refind_configure(TL, RD),
	!.
install_bootloader(limine, TL, BD, RD) :- !,
	limine_install(BD, RD),
	limine_configure(TL, RD),
	!.
install_bootloader(efistub, _TL, _BD, _RD) :- !,
	% efistub_install(BD, RD),
	% efistub_configure(TL, RD),
	!.
install_bootloader(syslinux, TL, BD, RD) :- !,
	syslinux_install(TL, BD, RD),
	syslinux_configure(TL, RD),
	!.
install_bootloader(gummiboot, _TL, _BD, RD) :- !,
	gummiboot_install(RD),
	% gummiboot_configure(TL, RD),
	!.
install_bootloader(zfsBootMenu, TL, BD, RD) :- !,
	% tui_msgbox(install_bootloader_1),
	zfsbootmenu_configure(RD),
	% tui_msgbox(install_bootloader_2),
	zfsbootmenu_install(TL, BD, RD),
	% tui_msgbox(install_bootloader_3),
	!.

/* Old code. */
bootloader_kernel_params(TL, L) :-
	bootloader_kernel_params_root(TL, L),
	true.
/* DO NOT delete. New code. It doesn't work with Limine and gpt_luks template
bootloader_kernel_params(TL, L) :-
	% Device
	root_pd(TL, ROOT_PD),
	( atom_concat('/dev/mapper/', _, ROOT_PD) ->
	  L = [root=ROOT_PD]
	; lx_get_dev_uuid(ROOT_PD, RPID),
	  L = [root=v('UUID', RPID)]
	),
	lx_get_dev_uuid(ROOT_PD, RPID),
	true.
*/
bootloader_kernel_params(_TL, [init='/sbin/init', rw]).
bootloader_kernel_params(TL, L) :-
	% LUKS
	( uses_luks(TL) ->
	  bootloader_kernel_params_luks(TL, L)
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
		, 'vconsole.unicode'=1
		, 'vconsole.keymap'=KB
		, 'locale.LANG'=LC
		% , 'rd.live.overlay.overlayfs'=1
	]) :-
	inst_setting(keymap, KB),
	inst_setting(locale, LC),
	true.

bootloader_kernel_params_luks(TL, ['rd.luks.name'=v(PUUID, LUKS_PD)]) :-
	member(bdev(luks, luks(_, PD)), TL),
	lx_get_dev_uuid(PD, PUUID),
	lx_split_dev(PD, _P, SDN),
	luks_dev_name_short(SDN, LUKS_PD),
	true.
bootloader_kernel_params_luks(_TL, ['rd.auto'=1]) :-
	inst_setting(hostonly, no).

bootloader_kernel_params_root(TL, [root='zfs:AUTO']) :-
	uses_zfs(TL),
	% has_boot_part(TL),
	!.
bootloader_kernel_params_root(TL, [root=v('UUID', RPID)]) :-
	root_pd(TL, ROOT_PD),
	lx_get_dev_uuid(ROOT_PD, RPID),
	true.

bootloader_write_cmdline(TL, S) :-
	findall(P0, (bootloader_kernel_params(TL, PL0), member(P0, PL0)), AL),
	os_wcmdl(AL, S),
	true.

reconfig_kernel(RD) :-
	lx_kernel_ver(RD, LV),
	tui_progressbox_safe([chroot, RD, 'xbps-reconfigure', '-f', concat(linux, LV), '2>&1'], '', [title(' Reconfigure Linux '), sz([18, 60])]),
	true.

setup_bootloader(_B, TL, _RD) :-
	% Do not setup bootloader if a bootloader dev has not been selected.
	\+ get_bootloader_dev7(TL, _), !.
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

replace_bootloader_dev7(none, NSN, L, TL, NTL) :- !,
	% add
	lx_sdn_to_dev7(L, NSN, DEV7),
	NTL = [bootloader_dev7(DEV7)| TL].
replace_bootloader_dev7(_, none, _L, TL, NTL) :- !,
	% remove
	findall(E, (member(E, TL), E \= bootloader_dev7(_)), NTL).
replace_bootloader_dev7(_, NSN, L, TL, NTL) :-
	% replace
	lx_sdn_to_dev7(L, NSN, DEV7),
	maplist(replace_element(bootloader_dev7(_), bootloader_dev7(DEV7)), TL, NTL).

replace_bootloader(B) :-
	retract(inst_setting(template(TT), OTL)),
	replace_bootloader(B, OTL, NTL),
	assertz(inst_setting(template(TT), NTL)).

% B - new bootloader.
replace_bootloader(B, TL, NTL) :-
	maplist(replace_element(bootloader(_), bootloader(B)), TL, NTL).

ensure_bootloader_dev(manual, _TL) :- !.
ensure_bootloader_dev(TT, TL) :-
	( get_bootloader_dev7(TL, _)
	; cmd_menu(bootloader_dev, TT, TL)
	), !.

