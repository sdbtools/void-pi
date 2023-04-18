% vi: noexpandtab:tabstop=4:ft=prolog
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
		, gpt_luks1
		, gpt_raid
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
	]).
bootloader_info(limine, [
		  ext2
		, ext3
		, ext4
		, vfat
	], [
		  manual
		, gpt_basic
	]).
bootloader_info(zfsBootMenu, [
		  zfs
	], [
		  gpt_zfsbootmenu
	]).

set_bootloader_dev(D) :-
	% We are trying to set the same value.
	inst_setting(bootloader_dev, dev(D, _, _)), !.
set_bootloader_dev(D) :-
	lx_list_dev_part(D, PL),
	lx_dev_part_tree(D, PL, TL),
	retractall(inst_setting(bootloader_dev, _)),
	assertz(inst_setting(bootloader_dev, dev(D, PL, TL))).

set_bootloader(RD) :-
	inst_setting(bootloader_dev, dev(BD, _, _)),
	inst_setting(bootloader, B),
	set_bootloader(B, BD, RD), !.
set_bootloader(_RD) :-
	tui_msgbox('Setting up of a bootloader has failed.', []),
	fail.

% set_bootloader(bootloader, bootloader_dev, root_dir)
set_bootloader(_, none, _RD) :- !.
set_bootloader(grub2, BD, RD) :- !,
	grub_configure(BD, RD),
	grub_install(BD, RD),
	grub_mkconfig(RD),
	!.
set_bootloader(rEFInd, BD, RD) :-
	refind_install(BD, RD),
	refind_configure(BD, RD),
	!.
set_bootloader(limine, BD, RD) :-
	limine_install(BD, RD), !,
	limine_configure(BD, RD),
	!.

