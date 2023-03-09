% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% pvs -o pv_name --no-heading
lvm_pvs(L) :-
	os_shell_lines_codes('pvs -o pv_name,vg_name --readonly --noheading --separator "\t" --units b --nosuffix 2>/dev/null', CL),
	% os_shell_lines_codes('pvs --readonly --noheading --separator "\t" --units b --nosuffix 2>/dev/null', CL),
	CL \= [],
	maplist(lvm_pvs_convert_, CL, L),
	true.

lvm_pvs_convert_(L, pv(PV,VG)) :-
	split_list(L, "\t", SL0),
	SL0 = [[32,32|H]|T],
	SL = [H|T],
	maplist(codes_atom, SL, [PV,VG]),
	true.

% PV - physical volume.
lvm_pvcreate_unsafe(PV) :-
	% writenl([lvm_pvcreate, PV]),
	os_shell2([pvcreate, '--yes', PV, '1>/dev/null', '2>/dev/null']),
	% os_shell2([pvcreate, PV, '1>/dev/null']),
	% os_call2([pvcreate, '--yes', PV]),
	true.

lvm_pvremove(_PV) :-
	% Remove from a VG.
	true.

lvm_vgcreate(VG, PVL) :-
	% os_shell2([vgcreate, VG, PVL]),
	os_shell2([vgcreate, VG, PVL, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgremove('') :- !. % There is no VG.
lvm_vgremove(VG) :-
	os_shell2([vgremove, VG, '1>/dev/null', '2>/dev/null']),
	true.

% Remove PV from VG.
lvm_vgremove_unsafe(VG, PV) :-
	lvm_pvs(L),
	findall(PV1, member(pv(PV1,VG), L), PVL),
	lvm_vgremove_unsafe_(VG, PV, PVL),
	true.

lvm_vgremove_unsafe_(_VG, _PV, []) :- !.
lvm_vgremove_unsafe_(VG, PV, [PV]) :- !,
	% This is the last device in VG.
	% Remove all LV in VG.
	lvm_lvremove_unsafe(VG),
	% Remove whole VG.
	lvm_vgremove(VG).
lvm_vgremove_unsafe_(VG, PV, _) :- !,
	lvm_vgreduce(VG, PV).

lvm_vgreduce(VG, PV) :-
	os_call2([vgreduce, VG, PV]),
	true.

% with the rest of capacity
lvm_lvcreate_unsafe(VG, LV) :-
	lvm_lvcreate_unsafe(VG, LV, '100%FREE').

lvm_lvcreate_unsafe(VG, LV, '') :- !,
	lvm_lvcreate_unsafe(VG, LV, '100%FREE').
lvm_lvcreate_unsafe(VG, LV, SZ) :-
	% os_shell2([lvcreate, '--yes', '-l', SZ, VG, '-n', LV]),
	os_shell2([lvcreate, '--yes', '-l', SZ, VG, '-n', LV, '1>/dev/null', '2>/dev/null']),
	true.

% Remove all LVs the specified VG.
lvm_lvremove_unsafe('') :- !. % There is no LV.
lvm_lvremove_unsafe(VG) :-
	% os_call2([lvremove, '--yes', VG]),
	os_shell2([lvremove, '--yes', VG, '1>/dev/null', '2>/dev/null']),
	true.

lvm_lvremove_unsafe(VG, LV) :-
	format_to_atom(A, '~w/~w', [VG, LV]),
	% os_call2([lvremove, '--yes', A]),
	os_call2([lvremove, '--yes', A, '1>/dev/null', '2>/dev/null']),
	true.

