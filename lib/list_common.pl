% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% S - separator.
join_atoms([H1,H2|T], S, T2) :-
	atom_concat(H1, S, H3),
	join_atoms([H2|T], S, T1),
	atom_concat(H3, T1, T2).
join_atoms([H], _, H).
join_atoms([], _, '').

join_atoms([H|T], A2) :-
	join_atoms(T, A1),
	atom_concat(H, A1, A2).
join_atoms([], '').

% Split list, no empty elements.
split_list_ne(I, SL, L) :-
	split_list_ne_(I, SL, H, T),
	split_list_combine(H, T, L).

split_list_ne_([H|T], SL, [], T3) :-
	member(H, SL), !,
	split_list_ne_(T, SL, T1, T2),
	split_list_combine(T1, T2, T3).
split_list_ne_([H|T], SL, [H|T1], T2) :- !,
	split_list_ne_(T, SL, T1, T2).
split_list_ne_([], _, [], []).

split_list(I, SL, [H|T]) :-
	split_list_(I, SL, H, T).

split_list_([H|T], SL, [], [T1|T2]) :-
	member(H, SL), !,
	split_list_(T, SL, T1, T2).
split_list_([H|T], SL, [H|T1], T2) :- !,
	split_list_(T, SL, T1, T2).
split_list_([], _, [], []).

split_list_combine([], T, T) :- !.
split_list_combine(H, T, [H|T]).

split_line_chars(I, L) :-
	split_list_ne(I, ['\n'], L).

split_line_codes(I, L) :-
	split_list(I, "\n", L).

% No empty lines.
split_line_codes_ne(I, L) :-
	split_list_ne(I, "\n", L).

split_atom_chars(I, L) :-
	split_list(I, [' ', '\t', '\n'], L).

% No empty atoms.
split_atom_chars_ne(I, L) :-
	split_list_ne(I, [' ', '\t', '\n'], L).

split_atom_codes(I, L) :-
	split_list(I, " \t\n", L).

% No empty atoms.
split_atom_codes_ne(I, L) :-
	split_list_ne(I, " \t\n", L).

