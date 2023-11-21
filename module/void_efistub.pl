% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://wiki.archlinux.org/title/EFISTUB
% https://mth.st/blog/void-efistub

efistub_install(RD) :-
	lx_kernel_ver(RD, LV),
	atom_concat('Void Linux with kernel ', LV, Name),
	efibootmgr_get(MgrL),
	memberchk(boot(Num, Name, _), MgrL),
	memberchk(order(OL), MgrL),
	delete(OL, Num, OL1),
	efibootmgr_set_order([Num|OL1]),
	true.

efistub_configure(TL, RD) :-
	atom_concat(RD, '/etc/default/efibootmgr-kernel-hook', CF),
	open(CF, write, S),
	efistub_write_cfg(TL, S),
	close(S),
	true.

efistub_write_cfg(TL, S) :-
	% fs7(Name, Label, MountPoint, Dev, [CreateOptList], [MountOptList], create/keep)
	member(fs7(vfat, _efi, '/boot', PD, _COL, _MOL, _CK), TL),
	lx_parent_dev_name(PD, N, PD0),
	write(S, 'MODIFY_EFI_ENTRIES="1"'), nl(S),
	write(S, 'OPTIONS="'), bootloader_write_cmdline(TL, S), write(S, '"'), nl(S),
	format(S, 'DISK="~w"', [PD0]), nl(S),
	format(S, 'PART=~w', [N]), nl(S),
	true.

efibootmgr_get(L) :-
	os_shell_lines_codes(efibootmgr, CL),
	maplist(efibootmgr_get_, CL, L),
	true.

efibootmgr_get_(L, timeout(A)) :-
	append("Timeout: ", L0, L), !,
	split_list_ne(L0, " ", [L1| _]),
	atom_codes(A, L1),
	true.
efibootmgr_get_(L, current(A)) :-
	append("BootCurrent: ", L0, L), !,
	atom_codes(A, L0),
	true.
efibootmgr_get_(L, next(A)) :-
	append("BootNext: ", L0, L), !,
	atom_codes(A, L0),
	true.
efibootmgr_get_(L, order(L2)) :-
	append("BootOrder: ", L0, L), !,
	split_list_ne(L0, ",", L1),
	maplist(codes_atom, L1, L2),
	true.
efibootmgr_get_(L, boot(A1, A2, F)) :-
	% Boot0000*
	split_list(L, "\t", [L0| _]),
	split_list(L0, " ", [L1| T]),
	append("Boot", L11, L1),
	( append(L12, [0'*], L11),
	  F = active
	; L12 = L11,
	  F = inactive
	),
	atom_codes(A1, L12),
	join(T, " ", L2),
	flatten(L2, L3),
	atom_codes(A2, L3),
	true.

efibootmgr_set_order(L) :-
	os_call2([efibootmgr, '-o', lc(L)]).
