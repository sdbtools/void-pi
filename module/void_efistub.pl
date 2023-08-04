% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/EFISTUB
% https://mth.st/blog/void-efistub

% efistub_install(_BD, _RD) :-
% 	true.

efistub_configure(TL, RD) :-
	atom_concat(RD, '/etc/default/efibootmgr-kernel-hook', CF),
	open(CF, write, S),
	efistub_write_cfg(TL, S),
	close(S),
	true.

efistub_write_cfg(TL, S) :-
	% fs4(FileSystem, Label, MountPoint, [device_list])
	member(fs4(vfat, _efi, '/boot/efi', [PD]), TL),
	parent_dev_name(PD, N, PD0),
	write(S, 'MODIFY_EFI_ENTRIES="1"'), nl(S),
	format(S, 'DISK="~w"', [PD0]), nl(S),
	format(S, 'PART="~w"', [N]), nl(S),
	true.

parent_dev_name(D, N, PD) :-
	% sub_atom(Atom, Before, Length, After, SubAtom),
	sub_atom(D, _, 1, 0, N),
	( atom_concat('/dev/nvme', _, D) ->
	  sub_atom(D, 0, _, 2, PD)
	; sub_atom(D, 0, _, 1, PD)
	).

