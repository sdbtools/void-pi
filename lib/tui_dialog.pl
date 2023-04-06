% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- dynamic([tui_def_args_all/1, tui_def_args/1, menu_def_item/2]).

% mixedgauge
% treeview

% Present in newt/whiptail.
tui_arg_map(default-item(V), ['--default-item', O]) :- !, add_dquote(V, O).
tui_arg_map(title(V), ['--title', O]) :- !, add_dquote(V, O).
tui_arg_map(backtitle(V), ['--backtitle', O]) :- !, add_dquote(V, O).

% dialog-specific.
tui_arg_map(aspect(V), ['--aspect', V]) :- !.
tui_arg_map(begin(Y, X), ['--begin', Y, X]) :- !.
tui_arg_map(cancel-label(V), ['--cancel-label', O]) :- !, add_dquote(V, O).
tui_arg_map(column-separator(V), ['--column-separator', V]) :- !.
tui_arg_map(create-rc(V), ['--create-rc', V]) :- !.
tui_arg_map(date-format(V), ['--date-format', V]) :- !.
tui_arg_map(default-button(V), ['--default-button', O]) :- !, add_dquote(V, O).
tui_arg_map(exit-label(V), ['--exit-label', O]) :- !, add_dquote(V, O).
tui_arg_map(help-label(V), ['--help-label', O]) :- !, add_dquote(V, O).
tui_arg_map(hfile(V), ['--hfile', O]) :- !, add_dquote(V, O).
tui_arg_map(hline(V), ['--hline', V]) :- !.
tui_arg_map(max-input(V), ['--max-input', V]) :- !.
% tui_arg_map(no-label(V), ['--no-label', O]) :- !, add_dquote(V, O).
tui_arg_map(no-label(V), ['--no-label', V]) :- !.
tui_arg_map(ok-label(V), ['--ok-label', O]) :- !, add_dquote(V, O).
tui_arg_map(separator(V), ['--separator', V]) :- !.
tui_arg_map(output-separator(V), ['--output-separator', V]) :- !.
tui_arg_map(print-text-only(V), ['--print-text-only', V]) :- !.
tui_arg_map(print-text-size(V), ['--print-text-size', V]) :- !.
tui_arg_map(separate-widget(V), ['--separate-widget', V]) :- !.
tui_arg_map(sleep(V), ['--sleep', V]) :- !.
tui_arg_map(tab-len(V), ['--tab-len', V]) :- !.
tui_arg_map(time-format(V), ['--time-format', V]) :- !.
tui_arg_map(timeout(V), ['--timeout', V]) :- !.
tui_arg_map(trace(V), ['--trace', V]) :- !.
tui_arg_map(week-start(V), ['--week-start', V]) :- !.
% tui_arg_map(yes-label(V), ['--yes-label', O]) :- !, add_dquote(V, O).
tui_arg_map(yes-label(V), ['--yes-label', V]) :- !.
tui_arg_map(extra-label(V), ['--extra-label', O]) :- !, add_dquote(V, O).
tui_arg_map(A, O) :- !,
	format_to_atom(O, '--~w', [A]).

dialog_rc(0, ok) :- !.
dialog_rc(1, cancel) :- !.
dialog_rc(2, help) :- !.
dialog_rc(3, extra) :- !.
dialog_rc(4, item_help) :- !.
dialog_rc(5, timeout) :- !.
dialog_rc(-1, error) :- !.

% This is dialog-specific implementation.
tui_make_sz2(sz(auto), ['0', '0']) :- !.
tui_make_sz2(sz(max), ['-1', '-1']) :- !.
tui_make_sz2(sz([H|T]), L) :-
	maplist(number_atom, [H|T], L).

tui_make_sz3(sz(auto), ['0', '0', '0']) :- !.
tui_make_sz3(sz(max), ['-1', '-1', '0']) :- !.
tui_make_sz3(sz([H|T]), L) :-
	maplist(number_atom, [H|T], L).

tui_buildlist(L, M, UA, A) :-
	tui_list3_ind(buildlist, L, M, UA, OutL),
	atom_codes(A, OutL).

% NL - list of numbers.
tui_checklist_ind(L, M, UA, NL) :-
	tui_list3_ind(checklist, L, M, UA, OutL),
	split_list_ne(OutL, " ", CNL),
	maplist(codes_number, CNL, NL).

tui_checklist_tag(L, M, UA, AL) :-
	tui_list3_tag(checklist, L, M, UA, OutL),
	split_list_ne(OutL, " ", CL),
	maplist(codes_atom, CL, AL).

tui_radiolist_ind(L, M, UA, N) :-
	tui_list3_ind(radiolist, L, M, UA, OutL),
	number_codes(N, OutL).

tui_radiolist_tag(L, M, UA, A) :-
	tui_list3_tag(radiolist, L, M, UA, OutL),
	atom_codes(A, OutL).

% SI - default item.
tui_radiolist_tag2(L, DI, M, UA, A) :-
	maplist(tui_radiolist_on_off(DI), L, L1),
	tui_radiolist_tag(L1, M, [default-item(DI)|UA], A).

% explicitly adds 1-based index.
tui_inputmenu_ind(L, M, UA, A) :-
	tui_box3_ind(inputmenu, L, M, UA, OutL),
	atom_codes(A, OutL).

% explicitly adds 1-based index.
tui_menu_ind(L, M, UA, N) :-
	tui_box3_ind(menu, L, M, UA, OutL),
	number_codes(N, OutL).

tui_menu_tag(L, M, UA, A) :-
	tui_menu3_tag(menu, L, M, UA, OutL),
	atom_codes(A, OutL).

tui_menu_tag_rc(L, M, UA, A, RC) :-
	tui_menu3_tag_rc(menu, L, M, UA, OutL, RC),
	atom_codes(A, OutL).

% N - name
tui_menu_tag2(N, L, M, UA, A1) :-
	% get default item
	( menu_def_item(N, DI) -> true
	; L = [[DI|_]|_]
	),
	tui_menu_tag_rc(L, M, [default-item(DI)|UA], A, RC), !,
	RC \= error,
	( RC = ok ->
	  A1 = A
	; A1 = RC
	),
	% tui_msgbox(A1, [sz([6, 40])]),
	% set default item
	retractall(menu_def_item(N, _)),
	assertz(menu_def_item(N, A1)),
	true.

% Vertical form.
% FLen - edit field lenght
% ILen - input field lenght
% L - list of items
% M - message
% AL - atom list
tui_form_v(FLen, ILen, L, M, UA, AL) :-
	tui_make_form_list(FLen, ILen, L, 1, SO),
	tui_box3(form, M, SO, UA, OutL, ok),
	split_line_codes(OutL, LL),
	maplist(codes_atom, LL, AL).

tui_passwordform_v(FLen, ILen, L, M, UA, AL) :-
	tui_make_form_list(FLen, ILen, L, 1, SO),
	tui_box3(passwordform, M, SO, UA, OutL, ok),
	split_line_codes(OutL, LL),
	maplist(codes_atom, LL, AL).

% Fields have a type: 0, 1 - hidden/password, 2 - readonly, 3.
tui_mixedform_v(FLen, ILen, L, M, UA, AL) :-
	tui_make_form_list_t(FLen, ILen, L, 1, SO),
	tui_box3(mixedform, M, SO, UA, OutL, ok),
	split_line_codes(OutL, LL),
	maplist(codes_atom, LL, AL).

tui_editbox(F, UA) :-
	tui_spawn(editbox, F, [], UA, 0).

tui_inputbox(M, I, UA, A) :-
	add_dquote(I, I1),
	tui_box2(inputbox, M, I1, UA, OutL),
	atom_codes(A, OutL).

tui_passwordbox(M, I, UA, A) :-
	add_dquote(I, I1),
	tui_box2(passwordbox, M, I1, UA, OutL),
	atom_codes(A, OutL).

tui_calendar(Msg, D, M, Y, UA, A) :-
	number_atom(D, Da),
	number_atom(M, Ma),
	number_atom(Y, Ya),
	tui_box2(calendar, Msg, [Da, Ma, Ya], UA, OutL),
	atom_codes(A, OutL).

tui_rangebox(Msg, Min, Max, Def, UA, N) :-
	number_atom(Min, MinA),
	number_atom(Max, MaxA),
	number_atom(Def, DefA),
	tui_box2(rangebox, Msg, [MinA, MaxA, DefA], UA, OutL),
	number_codes(N, OutL).

tui_timebox(Msg, H, M, S, UA, A) :-
	number_atom(H, Ha),
	number_atom(M, Ma),
	number_atom(S, Sa),
	tui_box2(timebox, Msg, [Ha, Ma, Sa], UA, OutL),
	atom_codes(A, OutL).

tui_gauge_safe(PL, M, UA) :-
	tui_tailbox2_safe(PL, gauge, M, [], UA).

% text + OK
tui_programbox_safe(PL, M, UA) :-
	tui_tailbox2_safe(PL, programbox, M, [], UA).

% result of command execution + OK
% C - command
% M - message
tui_prgbox(M, C, UA) :-
	tui_prgbox_rc(M, C, UA, RC),
	RC == 0.

tui_prgbox_rc(M, C, UA, RC) :-
	tui_spawn(prgbox, [M, C], [], UA, RC).

% Autoclose.
tui_progressbox_unsafe(PL, M, UA) :-
	tui_tailbox2_unsafe(PL, progressbox, M, [], UA).

tui_progressbox_safe(PL, M, UA) :-
	tui_tailbox2_safe(PL, progressbox, M, [], UA).

% text + Yes + No
tui_yesno(M, UA) :-
	tui_spawn(yesno, M, [], UA, RC),
	RC == 0.

% timer + OK + Cancel
tui_pause(M, N, UA) :-
	number_atom(N, NA),
	tui_spawn(pause, M, NA, UA, RC),
	RC == 0.

% text + OK
tui_msgbox(M, UA) :-
	tui_spawn(msgbox, M, [], UA, _).

tui_msgbox2(ML, UA) :-
	os_scmdl(ML, AL),
	tui_msgbox(AL, UA).

% text view + EXIT
tui_textbox(F, UA) :-
	tui_spawn(textbox, F, [], UA, _).

% tail + EXIT
tui_tailbox(F, UA) :-
	tui_spawn(tailbox, F, [], UA, _).

tui_tailboxbg(F, UA) :-
	tui_spawn(tailboxbg, F, [], UA, _).

% No buttons
tui_infobox(M, UA) :-
	tui_spawn(infobox, M, [], UA, _).

tui_fselect(P, UA, A) :-
	tui_box2(fselect, P, [], UA, OutL),
	atom_codes(A, OutL).

tui_dselect(P, UA, A) :-
	tui_box2(dselect, P, [], UA, OutL),
	atom_codes(A, OutL).

tui_make_form_list(FLen, ILen, [item(Name)|T], N, [[NQ, NA, '1', '""', NA, NEA, FLA, ILA]|T1]) :- !,
	add_dquote(Name, NQ),
	number_atom(N, NA),
	NE is FLen + 1,
	number_atom(NE, NEA),
	number_atom(FLen, FLA),
	number_atom(ILen, ILA),
	N1 is N + 1,
	tui_make_form_list(FLen, ILen, T, N1, T1).
tui_make_form_list(FLen, ILen, [item(Name, IV)|T], N, [[NQ, NA, '1', IVQ, NA, NEA, FLA, ILA]|T1]) :- !,
	add_dquote(Name, NQ),
	add_dquote(IV, IVQ),
	number_atom(N, NA),
	NE is FLen + 1,
	number_atom(NE, NEA),
	number_atom(FLen, FLA),
	number_atom(ILen, ILA),
	N1 is N + 1,
	tui_make_form_list(FLen, ILen, T, N1, T1).
tui_make_form_list(_, _, [], _, []).

tui_make_form_list_t(FLen, ILen, [item(Name)|T], N, [[NQ, NA, '1', '""', NA, NEA, FLA, ILA, '0']|T1]) :- !,
	add_dquote(Name, NQ),
	number_atom(N, NA),
	NE is FLen + 1,
	number_atom(NE, NEA),
	number_atom(FLen, FLA),
	number_atom(ILen, ILA),
	N1 is N + 1,
	tui_make_form_list_t(FLen, ILen, T, N1, T1).
tui_make_form_list_t(FLen, ILen, [item(Name, IV)|T], N, [[NQ, NA, '1', IVQ, NA, NEA, FLA, ILA, '0']|T1]) :- !,
	add_dquote(Name, NQ),
	add_dquote(IV, IVQ),
	number_atom(N, NA),
	NE is FLen + 1,
	number_atom(NE, NEA),
	number_atom(FLen, FLA),
	number_atom(ILen, ILA),
	N1 is N + 1,
	tui_make_form_list_t(FLen, ILen, T, N1, T1).
tui_make_form_list_t(FLen, ILen, [item(Name, IV, Type)|T], N, [[NQ, NA, '1', IVQ, NA, NEA, FLA, ILA, TA]|T1]) :- !,
	add_dquote(Name, NQ),
	add_dquote(IV, IVQ),
	number_atom(Type, TA),
	number_atom(N, NA),
	NE is FLen + 1,
	number_atom(NE, NEA),
	number_atom(FLen, FLA),
	number_atom(ILen, ILA),
	N1 is N + 1,
	tui_make_form_list_t(FLen, ILen, T, N1, T1).
tui_make_form_list_t(_, _, [], _, []).

% B - box type
% L - list of items
% M - message
% UA - user attributes
% OutL - ooutput list
tui_list3_ind(B, L, M, UA, OutL) :-
	tui_add_check_num_pref(1, L, SO),
	tui_box3(B, M, SO, UA, OutL, ok),
	true.

tui_list3_tag(B, L, M, UA, OutL) :-
	tui_add_check_tag_suf(L, SO),
	tui_box3(B, M, SO, UA, OutL, ok).

% B - box type
% M - message
% L - list of items
% UA - user attributes
% OutL - ooutput list
tui_box3_ind(B, L, M, UA, OutL) :-
	tui_add_num_pref(1, L, SO),
	tui_box3(B, M, SO, UA, OutL, ok).

tui_menu3_tag(B, L, M, UA, OutL) :-
	tui_menu3_tag_rc(B, L, M, UA, OutL, ok).

tui_menu3_tag_rc(B, L, M, UA, OutL, RA) :-
	add_dquote_list(L, SO),
	tui_box3(B, M, SO, UA, OutL, RA).

% DL - dialog command list
tui_make_box3_cmdlist(B, M, SO, UA, DL) :-
	tui_def_args(B, DEFA),
	tui_merge_args(DEFA, UA, MA),
	tui_make_args3(MA, SZA, TAL),
	add_dquote(M, M1),
	atom_concat('--', B, BA),
	DL = [dialog, '--stdout', TAL, BA, M1, SZA, SO].

tui_box3(B, M, SO, UA, OutL, RA) :-
	tui_make_box3_cmdlist(B, M, SO, UA, DL),
	os_scmdl(DL, AA),
	os_shell_codes_rc(AA, OutL, RC),
	dialog_rc(RC, RA).

% DL - dialog command list
tui_make_box2_cmdlist(B, M, SO, UA, DL) :-
	tui_def_args(B, DEFA),
	tui_merge_args(DEFA, UA, MA),
	tui_make_args2(MA, SZA, TAL),
	add_dquote(M, M1),
	atom_concat('--', B, BA),
	DL = [dialog, '--stdout', TAL, BA, M1, SZA, SO].

tui_tailbox2_unsafe(PL, B, M, SO, UA) :-
	tui_make_box2_cmdlist(B, M, SO, UA, DL),
	os_scmdl([PL, '|', DL], AA),
	os_shell(AA).

tui_tailbox2_safe(PL, B, M, SO, UA) :-
	tui_make_box2_cmdlist(B, M, SO, UA, DL),
	% os_scmdl([PL, '|', DL], AA),
	% os_scmdl([set, '-o', pipefail, ';', PL, '|', DL], AA),
	% os_scmdl([mkfifo, named_pipe, ';', DL, '<', named_pipe, '&', PL, '>', named_pipe], AA),
	% os_scmdl([mkfifo, named_pipe, ';', DL, '<', named_pipe, '&', PL, '>', named_pipe, ';', 'RC=$?', rm, named_pipe, ';', exit, '$RC'], AA),
	os_scmdl([bash, '-c', '\'', set, '-o', pipefail, ';', PL, '|', DL, '\''], AA),
	( os_shell(AA), !
	; 
	  tui_msgbox2(PL, [title(' Command has failed: ')]),
	  % tui_msgbox(AA, [title(' Command has failed: ')]),
	  fail
	).

tui_tailbox3(PL, B, M, SO, UA) :-
	tui_make_box3_cmdlist(B, M, SO, UA, DL),
	% os_scmdl([PL, '|', DL], AA),
	os_scmdl([bash, '-c', '\'', set, '-o', pipefail, ';', PL, '|', DL, '\''], AA),
	os_shell(AA).

% B - box type
% M - message
% SO - suffix options 
% UA - user attributes
% OutL - ooutput list
tui_box2(B, M, SO, UA, OutL) :-
	tui_make_box2_cmdlist(B, M, SO, UA, DL),
	os_scmdl(DL, AA),
	os_shell_codes(AA, OutL).

% PO - prefix options
% SO - suffix options
tui_spawn(B, PO, SO, UA, RC) :-
	tui_def_args(B, DEFA),
	tui_merge_args(DEFA, UA, MA),
	tui_make_args2(MA, SZA, TAL),
	atom_concat('--', B, BA),
	DA = [TAL, BA, PO, SZA, SO],
	os_ccmdl(DA, AL),
	spawn(dialog, AL, RC).

tui_def_args_all([sz(auto), clear]).

tui_def_args(infobox, L) :- !,
	tui_def_args_all(AL),
	delete(AL, clear, L).
tui_def_args(passwordbox, [insecure|AL]) :- !,
	tui_def_args_all(AL).
tui_def_args(passwordform, [insecure|AL]) :- !,
	tui_def_args_all(AL).
tui_def_args(mixedform, [insecure|AL]) :- !,
	tui_def_args_all(AL).
tui_def_args(_, AL) :-
	tui_def_args_all(AL).

