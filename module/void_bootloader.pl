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
		  % btrfs
		  ext2
		, ext3
		, ext4
		% , f2fs % Not supported by syslinux
		% , swap
		, vfat
		% , xfs
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks
		, gpt_luks
		, gpt_luks_lvm
	], [
		  % btrfs % It boots with the current configuration of btrfs + EFI. Doesn't boot with BIOS.
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

get_bootloader_dev3(TL, DEV3) :-
	memberchk(bootloader_dev(DEV3), TL).

install_bootloader(TL, _RD) :-
	% Do not install bootloader if a bootloader dev has not been selected.
	\+ get_bootloader_dev3(TL, _), !.
install_bootloader(TL, RD) :-
	get_bootloader(TL, B),
	get_bootloader_dev3(TL, dev3(BD, _, _)),
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
	grub_mkconfig(RD),
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

setup_bootloader(_B, TL, _RD) :-
	% Do not setup bootloader if a bootloader dev has not been selected.
	\+ get_bootloader_dev3(TL, _), !.
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

replace_bootloader_dev(none, NSN, L, TL, NTL) :- !,
	% add
	lx_sdn_to_dev7(L, NSN, DEV7),
	lx_dev7_to_dev3(DEV7, DEV3),
	NTL = [bootloader_dev(DEV3)| TL].
replace_bootloader_dev(_, none, _L, TL, NTL) :- !,
	% remove
	findall(E, (member(E, TL), E \= bootloader_dev(_)), NTL).
replace_bootloader_dev(_, NSN, L, TL, NTL) :-
	% replace
	lx_sdn_to_dev7(L, NSN, DEV7),
	lx_dev7_to_dev3(DEV7, DEV3),
	maplist(replace_element(bootloader_dev(_), bootloader_dev(DEV3)), TL, NTL).

replace_bootloader(B) :-
	retract(inst_setting(template(TT), OTL)),
	replace_bootloader(B, OTL, NTL),
	assertz(inst_setting(template(TT), NTL)).

% B - new bootloader.
replace_bootloader(B, TL, NTL) :-
	maplist(replace_element(bootloader(_), bootloader(B)), TL, NTL).

ensure_bootloader_dev(manual, _TL) :- !.
ensure_bootloader_dev(TT, TL) :-
	( memberchk(bootloader_dev(_), TL)
	; cmd_menu(bootloader_dev, TT, TL)
	), !.

