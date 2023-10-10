% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% pvs -o pv_name --no-heading
lvm_pvs(L) :-
	os_shell_lines_codes('pvs -o pv_name,vg_name --readonly --noheading --separator "\t" --units b --nosuffix 2>/dev/null', CL),
	% os_shell_lines_codes('pvs --readonly --noheading --separator "\t" --units b --nosuffix 2>/dev/null', CL),
	% CL \= [],
	maplist(lvm_pvs_convert_, CL, L),
	true.

lvm_lvs(L) :-
	os_shell_lines_codes('lvs -o vg_name,lv_name,lv_dm_path --noheading --separator "\t" 2>/dev/null', CL),
	CL \= [],
	maplist(lvm_lvs_convert_, CL, L),
	true.

lvm_pvs_convert_(L, pv(PV,VG)) :-
	split_list(L, "\t", SL0),
	SL0 = [[32,32|H]|T],
	SL = [H|T],
	maplist(codes_atom, SL, [PV,VG]),
	true.

lvm_lvs_convert_(L, lv(VG,LV,Path)) :-
	split_list(L, "\t", SL0),
	SL0 = [[32,32|H]|T],
	SL = [H|T],
	maplist(codes_atom, SL, [VG,LV,Path]),
	true.

% PV - physical volume.
lvm_pvcreate_unsafe(PV) :-
	% tui_msgbox_w([pvcreate, '--yes', PV, '1>/dev/null', '2>/dev/null']),
	os_shell2([pvcreate, '--yes', PV, '1>/dev/null', '2>/dev/null']),
	% os_shell2([pvcreate, PV, '1>/dev/null']),
	% os_call2([pvcreate, '--yes', PV]),
	true.

lvm_pvremove(PV) :-
	% Remove from a VG.
	os_shell2([pvremove, '-y', PV, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgcreate(VG, PVL) :-
	% os_shell2([vgcreate, VG, PVL]),
	os_shell2([vgcreate, VG, PVL, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgremove('') :- !. % There is no VG.
lvm_vgremove(VG) :-
	os_shell2([vgremove, VG, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgremove_unsafe('') :- !. % There is no VG.
lvm_vgremove_unsafe(VG) :-
	os_shell2([vgremove, '-y', VG, '1>/dev/null', '2>/dev/null']),
	true.

/* Is it correct?
% Remove PV from VG. Remove VG if there is no more PVs left in it.
% PV - long device name
lvm_pvremove_unsafe(VG, PV) :-
	% list all PVs
	lvm_pvs(L),
	% get all PVs in VG
	findall(PV1, member(pv(PV1,VG), L), PVL),
	lvm_vgremove_unsafe_(VG, PV, PVL),
	true.

% PV - long device name
lvm_vgremove_unsafe_(_VG, _PV, []) :- !. % There are no PVs in VG
lvm_vgremove_unsafe_(VG, PV, [PV]) :- !, % This is the last device in VG.
	% Remove all LV in VG.
	lvm_lvremove_unsafe(VG),
	% Remove whole VG.
	lvm_vgremove(VG).
lvm_vgremove_unsafe_(VG, PV, _) :- !,
	lvm_vgreduce(VG, PV).

lvm_vgreduce('', _PV) :- !.
lvm_vgreduce(VG, PV) :-
	os_call2([vgreduce, VG, PV]),
	true.
*/

lvm_pvremove_unsafe(PV) :-
	% list all PVs
	lvm_pvs(L),
	lvm_pvremove_unsafe_1(PV, L),
	true.

lvm_pvremove_unsafe_1(_PV, []) :- !.
lvm_pvremove_unsafe_1(PV, L) :-
	% Get a VG.
	memberchk(pv(PV, VG), L),
	( VG = '' ->
	  lvm_pvremove(PV)
	; % Get a list of all PV in VG.
	  findall(P, member(pv(P, VG), L), PVL),
	  ( PVL = [_] ->
	    lvm_vgremove(VG)
	  ; os_call2([vgreduce, '--yes', '--force', VG, PV]),
	    lvm_pvremove(PV)
	  )
	),
	true.

% with the rest of capacity
lvm_lvcreate_unsafe(VG, LV) :-
	lvm_lvcreate_unsafe(VG, LV, '100%FREE').

lvm_lvcreate_unsafe(VG, LV, '') :- !,
	os_shell2([lvcreate, '--yes', '--extents', '100%FREE', '--name', LV, VG, '1>/dev/null', '2>/dev/null']),
	true.
lvm_lvcreate_unsafe(VG, LV, SZ) :-
	% os_shell2([lvcreate, '--yes', '--extents', SZ, VG, '-n', LV]),
	os_shell2([lvcreate, '--yes', '--size', SZ, '--name', LV, VG, '1>/dev/null', '2>/dev/null']),
	true.

% Remove all LVs the specified VG.
lvm_lvremove_unsafe('') :- !. % There is no LV.
lvm_lvremove_unsafe(VG) :-
	% os_call2([lvremove, '--yes', VG]),
	os_shell2([lvremove, '--yes', '--force', VG, '1>/dev/null', '2>/dev/null']),
	true.

lvm_lvremove_unsafe(VG, LV) :-
	format_to_atom(A, '~w/~w', [VG, LV]),
	os_shell2([lvremove, '--yes', '--force', A, '1>/dev/null', '2>/dev/null']),
	true.

