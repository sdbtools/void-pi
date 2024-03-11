% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

:- dynamic(tui_setting_tmp/2).

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

% IL - input list
% OL - output list or a value
% R - reset/keep stored settings
menu_list_2(TAG, Title, EV, IL, OL, R) :-
	dialog_msg(menu, LABEL),
	% AL - "all" list.
	menu_list_on_all(TAG, EV, IL, AL),
	( tui_setting_tmp(menu_list(TAG), v(_, _IL0)), R \= reset
	; retractall(tui_setting_tmp(menu_list(TAG), _)),
	  assertz(tui_setting_tmp(menu_list(TAG), v(none, IL)))
	), !,
	repeat,
	% VL - value list.
	retract(tui_setting_tmp(menu_list(TAG), v(DI, VL))),
	menu_list_on_make(TAG, EV, AL, VL, PL),
	tui_menu_tag_rc(PL, LABEL, [default-item(DI), cancel-label('Return'), ok-label('Edit'), title(Title)], Tag, RC),
	( RC = cancel ->
	  % Accept
	  menu_list_on_get(TAG, EV, VL, Tag, OL),
	  assertz(tui_setting_tmp(menu_list(TAG), v(DI, VL)))
	; menu_list_on_set(TAG, EV, AL, VL, Tag, OL1),
	  assertz(tui_setting_tmp(menu_list(TAG), v(Tag, OL1))),
	  fail
	), !.

% IL - input list
% OL - output list or a value
menu_list_3(TAG, Title, Label, EV, IL, OL) :-
	% AL - "all" list.
	menu_list_on_all(TAG, EV, IL, AL),
	( tui_setting_tmp(menu_list(TAG), v(DI0, IL0))
	; IL0 = IL,
	  DI0 = none,
	  assertz(tui_setting_tmp(menu_list(TAG), v(DI0, IL0)))
	), !,
	repeat,
	% VL - value list.
	retract(tui_setting_tmp(menu_list(TAG), v(DI, VL))),
	menu_list_on_make(TAG, EV, AL, VL, PL),
	tui_menu_tag_rc(PL, Label, [default-item(DI), extra-button, extra-label('Accept'), ok-label('Edit'), title(Title)], Tag, RC),
	( RC = extra ->
	  % Accept
	  menu_list_on_get(TAG, EV, VL, Tag, OL),
	  assertz(tui_setting_tmp(menu_list(TAG), v(DI, VL)))
	; RC = cancel ->
	  menu_list_on_get(TAG, EV, IL0, DI0, OL),
	  assertz(tui_setting_tmp(menu_list(TAG), v(DI, IL0)))
	; menu_list_on_set(TAG, EV, AL, VL, Tag, OL1),
	  assertz(tui_setting_tmp(menu_list(TAG), v(Tag, OL1))),
	  false
	),
	!.

checklist_2(TAG, Title, EV, IL, OL) :-
	dialog_msg(checklist, LABEL),
	% AL - "all" list.
	checklist_on_all(TAG, EV, AL),
	( retract(tui_setting_tmp(check_list(TAG), VL))
	; VL = IL
	), !,
	% Convert prop-list into checklist format.
	checklist_on_make(TAG, EV, AL, VL, PL),
	tui_checklist_tag_rc(PL, LABEL, [no-tags, title(Title)], OL1, RC),
	( RC = cancel ->
	  assertz(tui_setting_tmp(check_list(TAG), VL)),
	  OL = VL
	; checklist_on_set(TAG, EV, AL, OL1, OL),
	  assertz(tui_setting_tmp(check_list(TAG), OL))
	),
	!.


