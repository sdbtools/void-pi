% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

gummiboot_install(RD) :-
	tui_progressbox_safe([chroot, RD, gummiboot, install, '2>&1'], '', [sz([6, 60])]),
	true.

gummiboot_configure(TL, RD) :-
	os_mkdir_p(RD + '/boot/loader/'),
	FN = '/boot/loader/void-options.conf',
	atom_concat(RD, FN, CF),
	open(CF, write, S),
	% It should contain, on a single line, only the command-line options you want passed to the kernel.
	bootloader_write_cmdline(TL, S), nl(S),
	close(S),
	true.

