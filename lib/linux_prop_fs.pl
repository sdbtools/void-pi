% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% :- multifile(prop_info/3).
:- discontiguous([prop_info/3]).

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

fs_get_prop(Tag, P, IPL, V) :-
	memberchk(attr(Tag, PL), IPL),
	memberchk(P=V, PL).

fs_get_label(zfs, _IPL, '') :- !, % zfs pool doesn't have a label.
	true.
fs_get_label(FS, IPL, V) :-
	prop_info_label(FS, Tag),
	fs_get_prop(Tag, label, IPL, V), !,
	true.
fs_get_label(_FS, _IPL, '').

fs_set_label(FS, _Label, IPL, IPL) :-
	memberchk(FS, [zfs, tmpfs, proc]), !. % zfs pool doesn't have a label.
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
prop_info_label(nilfs2, nilfs2_rw) :- !.
prop_info_label(exfat, exfat_rw) :- !.

% default options: rw, suid, dev, exec, auto, nouser, async, (relatime).
% "Do you need to specify the "defaults" option in fstab?" (https://unix.stackexchange.com/questions/191405/do-you-need-to-specify-the-defaults-option-in-fstab)
prop_info(mnt_indpn_feat, feat4s('--options', ',', on_off), [
	  % prop_feat4(defaults, off, std, none)
	  prop_feat4(atime, on, pref1(no), none)
	, prop_feat4(auto, on, pref1(no), none)
	, prop_feat4(dev, on, pref1(no), none)
	, prop_feat4(diratime, off, pref1(no), none)
	, prop_feat4(dirsync, off, std, none)
	, prop_feat4(exec, on, pref1(no), none)
	, prop_feat4(group, off, std, none)
	, prop_feat4(iversion, off, pref1(no), none)
	, prop_feat4(lazytime, off, pref1(no), none)
	, prop_feat4(mand, off, pref1(no), none)
	, prop_feat4(nofail, off, std, none)
	, prop_feat4(nosymfollow, off, std, none)
	, prop_feat4(owner, off, std, none)
	, prop_feat4(relatime, on, pref1(no), none)
	, prop_feat4(remount, off, std, none)
	, prop_feat4(ro, off, v2(ro, rw), none)
	, prop_feat4(silent, off, v2(silent, loud), none)
	, prop_feat4(strictatime, off, pref1(no), none)
	, prop_feat4(suid, on, pref1(no), none)
	, prop_feat4(sync, off, v2(sync, async), none)
	, prop_feat4(user, off, pref1(no), none)
	, prop_feat4('_netdev', off, std, none)
	]).

prop_info(mnt_indpn_opt, opt3s('--options', ','), [
	  opt3(context, str, '')
	, opt3(fscontext, str, '')
	, opt3(defcontext, str, '')
	, opt3(rootcontext, str, '')
	]).

% X-*
% x-*
% X-mount.auto-fstypes
% X-mount.mkdir
% X-mount.subdir
% X-mount.owner
% X-mount.group
% X-mount.mode
% X-mount.idmap

