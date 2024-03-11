% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

writenl(A) :-
	write(A), nl.

upper(I, O) :-
	atom_chars(I, IL),
	maplist(lower_upper, IL, OL),
	atom_chars(O, OL).

chars_atom(C, A) :-
	atom_chars(A, C).

codes_atom(C, A) :-
	atom_codes(A, C).

codes_number(C, N) :-
	number_codes(N, C).

% LA - left atom
% RA - right atom
surround_atoms(LA, RA, LI, LO) :-
	maplist(surround_atom(LA, RA), LI, LO).

% LA - left atom
% RA - right atom
surround_atom(LA, RA, I, O) :-
	atom_concat(LA, I, LA1),
	atom_concat(LA1, RA, O).

add_dquote(I, O) :-
	format_to_atom(O, '"~w"', [I]).
	% concat_atoms(['"', I, '"'], O),

add_dquote_list(I, O) :-
	maplist(add_dquote_list_, I, O).

add_dquote_list_(I, O) :-
	maplist(add_dquote, I, O).

offset1s(0,  '').
offset1s(1,  ' ').
offset1s(2,  '  ').
offset1s(3,  '   ').
offset1s(4,  '    ').
offset1s(5,  '     ').
offset1s(6,  '      ').
offset1s(7,  '       ').
offset1s(8,  '        ').
offset1s(9,  '         ').
offset1s(10, '          ').

make_offset(N, O) :-
	M is min(N, 10),
	N1 is N - M,
	offset1s(M, O1),
	( N1 =< 10 ->
	  O = O1
	; make_offset(N1, O2),
	  atom_concat(O1, O2, O)
	), !,
	true.

read_file_codes(S, [C|T]) :-
	get_code(S, C),
	% writenl(C),
	C =\= -1, !,
	read_file_codes(S, T).
read_file_codes(_, []).

% Reads till a separator or EOS. 
% (Reads only ONE line)
% SL - List of separators (codes)
read_file_codes(S, SL, [C|T]) :-
	get_code(S, C),
	% writenl(C),
	C =\= -1,
	\+ member(C, SL), !,
	read_file_codes(S, SL, T).
read_file_codes(_, _, []).

read_file_codes_line(S, L) :-
	read_file_codes(S, [10], L).

% Reads ALL lines.
read_file_codes_lines(S, LL) :-
	read_file_codes_line(S, L1),
	( L1 = [] ->
	  LL = L1
	; read_file_codes_lines(S, L2),
	  LL = [L1|L2]
	).

% Reads lines, skip comments.
read_file_codes_lines_nc(S, C, LL) :-
	read_file_codes_line(S, L1),
	( L1 = [] ->
	  LL = L1
	; append(C, _, L1) ->
	  read_file_codes_lines_nc(S, C, LL)
	; read_file_codes_lines_nc(S, C, L2),
	  LL = [L1|L2]
	).

read_file_chars(S, [C|T]) :-
	get_char(S, C),
	% writenl(C),
	C \= end_of_file, !,
	read_file_chars(S, T).
read_file_chars(_, []).

read_stream_codes(S, []) :-
	at_end_of_stream(S), !.
read_stream_codes(S, [C|T]) :-
	get_code(S, C),
	read_stream_codes(S, T).

read_stream_codes(S, _, []) :-
	at_end_of_stream(S), !.
read_stream_codes(S, SL, [C|T]) :-
	get_code(S, C),
	\+ member(C, SL), !,
	read_stream_codes(S, T).
read_stream_codes(_, _, []).

read_stream_codes_line(S, L) :-
	read_file_codes(S, "\n", L).

% Reads ALL lines
read_stream_codes_lines(S, LL) :-
	read_stream_codes_line(S, L1),
	( L1 = []
	-> LL = L1
	; read_stream_codes_lines(S, L2),
	  LL = [L1|L2]
	).

