% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

fs_set_prop(Tag, P, V, IPL, OPL) :-
	fs_set_prop0(IPL, Tag, P, V, OPL).

fs_set_prop0([attr(Tag, L)| T], Tag, P, V, [attr(Tag, L1)| T]) :- !,
	fs_set_prop1(L, P, V, L1).
fs_set_prop0([H| T], Tag, P, V, [H| T1]) :- !,
	fs_set_prop0(T, Tag, P, V, T1).
fs_set_prop0([], Tag, P, V, [attr(Tag, [P=V])]).

fs_set_prop1([P=_V0| T], P, V, [P=V| T]) :- !.
fs_set_prop1([H| T], P, V, [H| T1]) :- !,
	fs_set_prop1(T, P, V, T1).
fs_set_prop1([], P, V, [P=V]).

fs_set_label(zfs, _Label, IPL, IPL) :- !. % zfs pool doesn't have a label.
fs_set_label(FS, Label, IPL, OPL) :-
	prop_info_label(FS, Tag),
	( FS = vfat ->
	  upper(Label, Label0)
	; Label0 = Label
	),
	fs_set_prop(Tag, label, Label0, IPL, OPL),
	true.

prop_info_label(ext2, extfs_rw) :- !.
prop_info_label(ext3, extfs_rw) :- !.
prop_info_label(ext4, extfs_rw) :- !.
prop_info_label(btrfs, btrfs_rw) :- !.
prop_info_label(f2fs, f2fs_rw) :- !.
prop_info_label(vfat, vfat_rw) :- !.
prop_info_label(xfs, xfs_rw) :- !.
prop_info_label(bcachefs, bcachefs_rw) :- !.
