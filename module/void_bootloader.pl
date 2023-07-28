% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% bootloader_info(bootloade, supported_fs, supported_template).
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
		, gpt_lvm_luks1
		, gpt_luks1
		, gpt_luks1_lvm
		% , gpt_raid
		% , gpt_zfsbootmenu
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
		, gpt_lvm_luks1
		, gpt_luks1
		, gpt_luks1_lvm
	]).
bootloader_info(limine, [
		  ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, gpt_basic
		, gpt_lvm
		, gpt_lvm_luks1
		, gpt_luks1
		, gpt_luks1_lvm
	]).
bootloader_info(zfsBootMenu, [
		  zfs
	], [
		  gpt_zfsbootmenu
	]).

target_dep_bootloader(limine, limine) :- !.
target_dep_bootloader(rEFInd, refind) :- !.
target_dep_bootloader(grub2, GRUB) :-
	\+ inst_setting(source, local),
	inst_setting(system(arch), ARCH),
	arch2grub(ARCH, GRUB), !.

set_bootloader(TL, RD) :-
	memberchk(bootloader(B), TL),
	memberchk(bootloader_dev(dev3(BD, _, _)), TL),
	set_bootloader(B, TL, BD, RD), !.
set_bootloader(_TL, _RD) :-
	tui_msgbox('Setting up of a bootloader has failed.'),
	fail.

% set_bootloader(template_list, bootloader, bootloader_dev, root_dir)
set_bootloader(_, _TL, none, _RD) :- !.
set_bootloader(grub2, TL, BD, RD) :- !,
	grub_configure(TL, RD),
	grub_install(TL, BD, RD),
	grub_mkconfig(RD),
	!.
set_bootloader(rEFInd, TL, _BD, RD) :-
	refind_install(TL, RD),
	refind_configure(TL, RD),
	!.
set_bootloader(limine, TL, BD, RD) :-
	limine_install(BD, RD), !,
	limine_configure(TL, RD),
	!.

