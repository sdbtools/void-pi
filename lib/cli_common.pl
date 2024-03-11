% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

handle_cmd_args([A|T]) :-
	atom_codes(A, CL),
	handle_cmd_arg_cl(CL,T,T1), !,
	handle_cmd_args(T1).
handle_cmd_args([]).

% First argument is a code list.
handle_cmd_arg_short_cl([], L, L) :- !.
handle_cmd_arg_short_cl([C|T], LI, LO) :-
	cmd_arg_info(Alias, C, _, _, _), !,
	on_cmd_arg(Alias, LI, LO1),
	handle_cmd_arg_short_cl(T, LO1, LO).
handle_cmd_arg_short_cl([C|_], _, _) :- !,
	write('Unknown argument -'),
	atom_codes(A, [C]),
	writenl(A),
	fail.

% First argument is a code list.
handle_cmd_arg_long_cl(CL, LI, LO) :-
	split_list(CL, "=", [H|T1]),
	atom_codes(A1, H),
	cmd_arg_info(Alias, _, A1, _, _), !,
	( T1 = [] ->
	  on_cmd_arg(Alias, LI, LO)
	; append(H, [0'=|T2], CL),
	  atom_codes(V, T2),
	  on_cmd_arg(Alias, [V|LI], LO)
	),
	true.
handle_cmd_arg_long_cl(CL, _, _) :-
	write('Unknown argument --'),
	atom_codes(A, CL),
	writenl(A),
	fail.

% First argument is a code list.
handle_cmd_arg_cl([0'-,0'-|T], LI, LO) :- !,
	handle_cmd_arg_long_cl(T, LI, LO),
	true.
handle_cmd_arg_cl([0'-|T], LI, LO) :-
	handle_cmd_arg_short_cl(T, LI, LO),
	true.

cmd_arg_usage_short(S) :-
	% arguments without value
	findall(SA, (cmd_arg_info(_, SA, _, '', _), SA \= 0), AL1),
	% short arguments with value
	findall(A, (cmd_arg_info(_, SA, _, V, _), SA \= 0, V \= '', format_to_atom(A, '-~c ~w', [SA, V])), AL2),
	% long only arguments with value
	findall(A, (cmd_arg_info(_, 0, LA, V, _), V \= '', format_to_atom(A, '--~w ~w', [LA, V])), AL3),
	append(AL2, AL3, CL1),
	( AL1 = [] ->
	  S = AL2
	; atom_codes(A1, [0'-|AL1]),
	  S = [A1|CL1]
	),
	true.

cmd_arg_usage_long(S) :-
	findall(Line, format_cmd_arg_long0(Line), LL),	
	maplist(format_cmd_arg_len, LL, LLen),
	max_list(LLen, Max),
	maplist(format_cmd_arg_long1(Max), LL, LL1),
	join_atoms(LL1, '\n', S),
	true.

format_cmd_arg_len(a0(Line, _), Len) :-
	atom_length(Line, Len).

format_cmd_arg_long1(Max, a0(Line, D), S) :-
	atom_length(Line, Len),
	N is Max - Len + 3,
	make_offset(N, O),
	atom_concat(Line, O, S0),
	atom_concat(S0, D, S),
	true.

format_cmd_arg_long0(a0(Line, D)) :-
	cmd_arg_info(_, SA, LA, _, D),
	format_cmd_arg_long0(SA, LA, Line),
	true.

format_cmd_arg_long0(0, LA, Line) :- !,
	format_to_atom(Line, '        --~w', [LA]).
format_cmd_arg_long0(SA, '', Line) :- !,
	format_to_atom(Line, '    -~c', [SA]).
format_cmd_arg_long0(SA, LA, Line) :- !,
	format_to_atom(Line, '    -~c, --~w', [SA, LA]).

