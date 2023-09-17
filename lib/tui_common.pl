% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

tui_radiolist_on_off(ON, [T], [T, T, I]) :- !,
	( T = ON ->
	  I = on
	; I = off
	).
tui_radiolist_on_off(ON, [T, V|_], [T, V, I]) :- !,
	( T = ON ->
	  I = on
	; I = off
	).
tui_radiolist_on_off(ON, T, [T, T, I]) :- !,
	( T = ON ->
	  I = on
	; I = off
	).

tui_checklist_on_off(ONL, [T], [T, T, I]) :- !,
	( memberchk(T, ONL) ->
	  I = on
	; I = off
	).
tui_checklist_on_off(ONL, [T, V|_], [T, V, I]) :- !,
	( memberchk(T, ONL) ->
	  I = on
	; I = off
	).
tui_checklist_on_off(ONL, T, [T, T, I]) :- !,
	( memberchk(T, ONL) ->
	  I = on
	; I = off
	).

tui_add_num_pref(N, [H|T], [A, H1|T1]) :-
	number_atom(N, A),
	N1 is N + 1,
	add_dquote(H, H1),
	tui_add_num_pref(N1, T, T1).
tui_add_num_pref(_, [], []).

tui_make_ind_param([H, S], H1, S) :- !,
	add_dquote(H, H1).
tui_make_ind_param([H], H1, off) :- !,
	add_dquote(H, H1).
tui_make_ind_param(A, A1, off) :- !,
	add_dquote(A, A1).

tui_make_tag_param([A1, A2, S], [A11, A12, S]) :- !,
	add_dquote(A1, A11),
	add_dquote(A2, A12).
tui_make_tag_param([A1, A2], [A11, A12, off]) :- !,
	add_dquote(A1, A11),
	add_dquote(A2, A12).
tui_make_tag_param([A], [A1, A1, off]) :- !,
	add_dquote(A, A1).
tui_make_tag_param(A, [A1, A1, off]) :- !,
	add_dquote(A, A1).

tui_add_check_num_pref(N, [H|T], [NA, I, S|T1]) :-
	tui_make_ind_param(H, I, S),
	number_atom(N, NA),
	N1 is N + 1,
	tui_add_check_num_pref(N1, T, T1).
tui_add_check_num_pref(_, [], []).

tui_add_check_tag_suf(LI, LO) :-
	maplist(tui_make_tag_param, LI, LO).

% +def_args, +user_args, -result
tui_merge_args([H|T], U, T2) :- !,
	tui_merge_args(T, U, T1),
	tui_member_args(H, U, T1, T2).
tui_merge_args([], U, U).

tui_args_mask(sz(_), sz(_)) :- !.
tui_args_mask(V, V) :- !.

tui_member_args(M, U, T, T) :-
	tui_args_mask(M, MM),
	member(MM, U), !.
tui_member_args(M, _, T, [M|T]).

tui_make_args2([sz(V)|T], SZA, AL) :- !,
	tui_make_sz2(sz(V), SZA),
	tui_make_args2(T, _, AL).
tui_make_args2([H|T], SZ, [H1|AL]) :-
	tui_arg_map(H, H1),
	tui_make_args2(T, SZ, AL).
tui_make_args2([], _, []).

tui_make_args3([sz(V)|T], SZA, AL) :- !,
	tui_make_sz3(sz(V), SZA),
	tui_make_args3(T, _, AL).
tui_make_args3([H|T], SZ, [H1|AL]) :-
	tui_arg_map(H, H1),
	tui_make_args3(T, SZ, AL).
tui_make_args3([], _, []).

