% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- dynamic([get_log/1]).

get_log('/dev/tty8').

os_mkdir(D) :-
	os_call2([mkdir, D]).

os_mkdir_p(D) :-
	os_call2([mkdir, '-p', D]).

os_rm_f(F) :-
	os_call2([rm, '-f', F]).

% C - command.
os_call(C, O) :-
	spawn(C, O, 0).

os_call_rc(C, O, RC) :-
	spawn(C, O, RC).

os_call2(L) :-
	os_ccmdl(L, [H|T]),
	os_call(H, T).

os_call2_rc(L, RC) :-
	os_ccmdl(L, [H|T]),
	os_call_rc(H, T, RC).

os_shell(C) :-
	shell(C, 0).

os_shell_rc(C, RC) :-
	shell(C, RC).

os_shell2(L) :-
	os_scmdl(L, LA),
	os_shell(LA).

os_shell2l(L) :-
	get_log(LOG),
	os_scmdl([L, ['>>', LOG]], LA),
	os_shell(LA).

os_shell2_rc(L, RC) :-
	os_scmdl(L, LA),
	os_shell_rc(LA, RC).

% Empty list if there is no output.
os_shell_codes(C, L) :-
	popen(C, read, RS), !,
	( at_end_of_stream(RS) ->
	  L = []
	; read_file_codes(RS, L),
	  close(RS)
	),
	true.

os_shell2_codes(IL, L) :-
	os_scmdl(IL, C),
	os_shell_codes(C, L).

% VL - value list.
os_shell_codes(C, VL, L) :-
	format_to_atom(CA, C, VL),
	os_shell_codes(CA, L).

os_shell2_codes(IL, VL, L) :-
	os_scmdl(IL, C),
	os_shell_codes(C, VL, L).

% Empty list if there is no output.
os_shell_codes_rc(C, L, RC) :-
	exec(C, SI, SO, SE, Pid),
	wait(Pid, RC),
	( at_end_of_stream(SO) ->
	  L = []
	; read_file_codes(SO, L),
	  close(SI),
	  close(SO),
	  close(SE)
	),
	true.

% VL - value list.
os_shell_codes_rc(C, VL, L, RC) :-
	format_to_atom(CA, C, VL),
	os_shell_codes_rc(CA, L, RC).

os_shell_atom(C, A) :-
	os_shell_codes(C, L),
	atom_codes(A, L).

os_shell2_atom(IL, A) :-
	os_shell2_codes(IL, L),
	atom_codes(A, L).

os_shell_atom(C, VL, A) :-
	os_shell_codes(C, VL, L),
	atom_codes(A, L).

os_shell2_atom(IL, VL, A) :-
	os_shell2_codes(IL, VL, L),
	atom_codes(A, L).

os_shell_atom_list(C, AL) :-
	os_shell_codes(C, L),
	split_atom_codes_ne(L, LL),
	maplist(codes_atom, LL, AL).

os_shell_atom_list(C, VL, AL) :-
	os_shell_codes(C, VL, L),
	split_atom_codes_ne(L, LL),
	maplist(codes_atom, LL, AL).

os_shell_number(C, N) :-
	os_shell_codes_line(C, L),
	number_codes(N, L).

os_shell2_number(IL, N) :-
	os_shell2_codes_line(IL, L),
	number_codes(N, L).

os_shell_number(C, VL, N) :-
	os_shell_codes_line(C, VL, L),
	number_codes(N, L).

os_shell2_number(IL, VL, N) :-
	os_shell2_codes_line(IL, VL, L),
	number_codes(N, L).

os_shell_codes_line(C, L) :-
	popen(C, read, RS), !,
	( at_end_of_stream(RS) ->
	  L = []
	; read_file_codes_line(RS, L),
	  close(RS)
	),
	true.

os_shell2_codes_line(IL, L) :-
	os_scmdl(IL, LA),
	os_shell_codes_line(LA, L).

os_shell_codes_line(C, VL, L) :-
	format_to_atom(CA, C, VL),
	os_shell_codes_line(CA, L).

os_shell2_codes_line(IL, VL, L) :-
	os_scmdl(IL, LA),
	os_shell_codes_line(LA, VL, L).

os_shell_line(C, A) :-
	os_shell_codes_line(C, L),
	atom_codes(A, L).

os_shell2_line(IL, A) :-
	os_shell2_codes_line(IL, L),
	atom_codes(A, L).

os_shell_line(C, VL, A) :-
	os_shell_codes_line(C, VL, L),
	atom_codes(A, L).

os_shell2_line(IL, VL, A) :-
	os_shell2_codes_line(IL, VL, L),
	atom_codes(A, L).

os_shell_lines_codes(C, CL) :-
	popen(C, read, RS),
	( at_end_of_stream(RS) ->
	  CL = []
	; read_file_codes_lines(RS, CL),
	  close(RS)
	),
	true.

os_shell2_lines_codes(IL, CL) :-
	os_scmdl(IL, LA),
	os_shell_lines_codes(LA, CL),
	true.

% Always succeeds. Returns [] if there are no lines.
os_shell_lines(C, AL) :-
	os_shell_lines_codes(C, CL),
	maplist(codes_atom, CL, AL).

os_shell_lines(C, VL, A) :-
	format_to_atom(CA, C, VL),
	os_shell_lines(CA, A).

% Always succeeds. Returns [] if there are no lines.
os_shell2_lines(IL, AL) :-
	os_shell2_lines_codes(IL, CL),
	maplist(codes_atom, CL, AL).

os_ccmdl(IL, OL) :-
	phrase(os_cmdl(IL), OL), !.

os_scmdl(IL, A) :-
	os_ccmdl(IL, OL),
	join_atoms(OL, ' ', A), !.

os_wcmdl(IL, S) :-
	os_ccmdl(IL, OL),
	write_atoms(OL, ' ', S), !.

% !!! Do not use os_cmdl directly!
os_cmdl([H|T]) -->
	os_cmd(H),
	os_cmdl(T).
os_cmdl([]) -->
	[].

os_cmd(o(O)) --> { atom_concat('-', O, O1) }, [O1].
os_cmd(o(O, V)) --> { phrase(os_cmd(V), [V1]), atom_concat('-', O, O1) }, [O1, V1].
os_cmd(oo(O)) --> { atom_concat('--', O, O1) }, [O1].
os_cmd(oo(O, V)) --> { phrase(os_cmd(V), [V1]), format_to_atom(O1, '--~w=~w', [O, V1]) }, [O1].
os_cmd(dq(V)) --> { phrase(os_cmd(V), [V1]), format_to_atom(QV, '"~w"', [V1]) }, [QV].
% value assignment
os_cmd(v(O, V)) --> { phrase(os_cmd(V), [V1]), format_to_atom(O1, '~w=~w', [O, V1]) }, [O1].
os_cmd(O = V) --> { phrase(os_cmd(V), [V1]), format_to_atom(O1, '~w=~w', [O, V1]) }, [O1].
% concat
os_cmd(concat(V1, V2)) --> { phrase(os_cmd(V1), [V11]), phrase(os_cmd(V2), [V21]), atom_concat(V11, V21, V3) }, [V3].
os_cmd(V1 + V2) --> { phrase(os_cmd(V1), [V11]), phrase(os_cmd(V2), [V21]), atom_concat(V11, V21, V3) }, [V3].

os_cmd(V) --> { is_list(V) }, os_cmdl(V).
os_cmd(V) --> [V].

