% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://github.com/Oku-Code/void-linux-guide/blob/main/void-setup-guide.md

% soft_info(Name, B, FS, PkgList, Descr).
soft_info(btrbk, _, btrfs, [snooze, btrbk], 'Backup tool for btrfs subvolumes').
soft_info('grub-btrfs', grub2, btrfs, ['grub-btrfs', 'grub-btrfs-runit'], 'Add a btrfs snapshots sub-menu to GRUB').
% soft_info(snapper, _, btrfs, [snapper], 'Manage filesystem snapshots and allow undo of system modifications').

% Remove list of packages
soft_remove_pkg_list(L, RD) :-
	% find installed packages
	findall(P0, (member(P0, L), os_call2_rc(['xbps-query', P0], 0)), L0),
	% find dependencies
	findall(P2, (member(P1, L0), os_shell2_lines(['xbps-query', '-X', P1], P2L), member(P2, P2L)), L1),
	append(L1, L0, L3),
	tui_progressbox_safe(['xbps-remove', o(r, RD), '-Ry', L3, '2>&1'], '', [title(' xbps-remove '), sz([12, 80])]),
	true.

soft_install(TL, RD) :-
	findall(S, member(soft(S), TL), SL),
	% Add dependencies to the list.
	findall(S1, (member(S0, SL), soft_info(S0, _, _, PKGL, _), member(S1, PKGL)), SL1),
	dedup(SL1, SL2),
	soft_install_soft_chroot(SL2, RD),
	maplist(soft_configure(RD), SL2),
	true.
soft_install(_TL, _RD) :-
	tui_msgbox('Software installation has failed.'),
	fail.

soft_update(RD) :-
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	% This is mandatory.
	tui_progressbox_safe([Pref, 'xbps-install', '-uy', xbps, '2>&1'], '', [title(' Update xbps '), sz([12, 80])]),
	tui_progressbox_safe([Pref, 'xbps-install', '-Suy', '2>&1'], '', [title(' Update Software '), sz(max)]),
	true.

soft_install_soft([], _RD) :- !.
soft_install_soft(SL, RD) :-
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref(ARCH, Pref),
	% This is mandatory.
	tui_progressbox_safe([Pref, 'xbps-install', o(r, RD), '-uy', xbps, '2>&1'], '', [title(' Update xbps '), sz([12, 80])]),
	tui_progressbox_safe([Pref, 'xbps-install', o(r, RD), '-Sy', SL, '2>&1'], '', [title(' Install Software '), sz([12, 80])]),
	true.

soft_install_soft_chroot([], _RD) :- !.
soft_install_soft_chroot(SL, RD) :-
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	% This is mandatory.
	tui_progressbox_safe([Pref, 'xbps-install', '-uy', xbps, '2>&1'], '', [title(' Update xbps '), sz([12, 80])]),
	tui_progressbox_safe([Pref, 'xbps-install', '-Sy', SL, '2>&1'], '', [title(' Install Software '), sz([12, 80])]),
	true.

soft_install_deps(_, []) :- !.
soft_install_deps(Pref, D) :-
	% This is mandatory.
	tui_progressbox_safe([Pref, 'xbps-install', '-uy', xbps, '2>&1'], '', [title(' Update xbps '), sz([12, 80])]),
	tui_progressbox_safe([Pref, 'xbps-install', '-SyU', D, '2>&1'], '', [title(' Install Dependencies '), sz([12, 80])]).

soft_install_deps_rd(_, [], _RD) :- !.
soft_install_deps_rd(Pref, D, RD) :-
	tui_progressbox_safe([Pref, 'xbps-install', o(r, RD), '-SyU', D, '2>&1'], '', [title(' Install Dependencies '), sz([12, 80])]).

soft_configure(RD, snooze) :- !,
	os_call2([chroot, RD, ln, '-sf', '/etc/sv/snooze-daily', '/var/service']),
	os_call2([chroot, RD, ln, '-sf', '/etc/sv/snooze-hourly', '/var/service']),
	true.
soft_configure(RD, 'grub-btrfs') :- !,
	os_call2([chroot, RD, ln, '-sf', '/etc/sv/grub-btrfs', '/var/service']),
	true.
soft_configure(RD, btrbk) :- !,
	os_mkdir_p(RD + '/mnt/btr_pool/btrbk_snapshots'),
	atom_concat(RD, '/etc/btrbk/btrbk.conf', CF1),
	open(CF1, write, S1),
	soft_btrbk_write_cfg(S1),
	close(S1),

	atom_concat(RD, '/etc/cron.daily/btrbk', CF2),
	open(CF2, write, S2),
	soft_btrbk_write_cron(S2),
	close(S2),
	os_shell2([chmod, '711', CF2]),
	true.
soft_configure(RD, snapper) :- !,
	soft_update(RD),
	os_call2([chroot, RD, ln, '-srf', '/etc/sv/dbus', '/var/service']),
	os_call2([chroot, RD, umount, '/.snapshots']),
	os_call2([chroot, RD, rm, '-rf', '/.snapshots']),
	os_call2([chroot, RD, snapper, '-c', root, 'create-config', '/']),
	os_call2([chroot, RD, mkdir, '/.snapshots']),
	os_call2([chroot, RD, chmod, '750', '/.snapshots']),
	os_call2([chroot, RD, mount, '-a']),
	os_call2([chroot, RD, ln, '-srf', '/etc/sv/snapperd', '/var/service']),
	true.
soft_configure(_RD, _) :-
	true.

soft_btrbk_write_cfg(S) :-
	write(S, 'transaction_log            /var/log/btrbk.log'), nl(S),
	write(S, 'timestamp_format           long'), nl(S),
	write(S, 'stream_buffer              256m'), nl(S), nl(S),

	write(S, '# Configuration:'), nl(S),
	write(S, 'snapshot_preserve_min   2d'), nl(S),
	write(S, 'snapshot_preserve       14d'), nl(S), nl(S),

	write(S, 'target_preserve_min     no'), nl(S),
	write(S, 'target_preserve         20d 10w *m'), nl(S), nl(S),

	write(S, 'archive_preserve_min    latest'), nl(S),
	write(S, 'archive_preserve        12m 10y'), nl(S), nl(S),

	write(S, 'volume /mnt/btr_pool'), nl(S),
	write(S, '  snapshot_dir btrbk_snapshots'), nl(S), nl(S),

	write(S, '  subvolume @'), nl(S),
	write(S, '  subvolume @opt'), nl(S),
	write(S, '  subvolume @var'), nl(S),
	write(S, '  subvolume @srv'), nl(S),
	write(S, '  subvolume @home'), nl(S),
	true.

soft_btrbk_write_cron(S) :-
	write(S, '#!/bin/sh'), nl(S),
	write(S, 'exec /usr/bin/btrbk -q run'), nl(S),
	true.

