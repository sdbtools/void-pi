% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

zpool_list(L) :-
	os_shell_lines_codes('zpool list -Hp 2>/dev/null', CL),
	CL \= [],
	maplist(zpool_list_convert, CL, L),
	true.

zpool_list_convert(L, zp(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)) :-
	split_list_ne(L, "\t", SL),
	maplist(codes_atom, SL, [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11]),
	true.

