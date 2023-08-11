% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% soft_info(PkgName, B, FS, DependencyList, Descr).
soft_info(btrbk, _, btrfs, [snooze], 'Backup tool for btrfs subvolumes').
soft_info('grub-btrfs', grub2, btrfs, [], 'Add a btrfs snapshots sub-menu to GRUB').

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
	findall(S1, (member(S0, SL), soft_dep(S0, S1)), SL1),
	tui_progressbox_safe([chroot, RD, 'xbps-install', '-SyU', SL1, '2>&1'], '', [title(' Install Software '), sz([12, 80])]),
	maplist(soft_configure(RD), SL1),
	true.

soft_dep(S, D) :-
	soft_info(S, _, _, DL, _Descr),
	member(D, DL),
	true.
soft_dep(S, S).

soft_configure(RD, snooze) :- !,
	os_call2([chroot, RD, ln, '-s', '--force', '/etc/sv/snooze-daily', '/var/service']),
	os_call2([chroot, RD, ln, '-s', '--force', '/etc/sv/snooze-hourly', '/var/service']),
	true.
soft_configure(_RD, 'grub-btrfs') :- !,
	true.
soft_configure(RD, btrbk) :- !,
	% os_mkdir_p(RD + '/.snapshots/btrbk_snapshots'),
	os_mkdir_p(RD + '/mnt/btr_pool/btrbk_snapshots'),
	atom_concat(RD, '/etc/btrbk/btrbk.conf', CF1),
	open(CF1, write, S1),
	btrbk_write_cfg(S1),
	close(S1),

	atom_concat(RD, '/etc/cron.daily/btrbk', CF2),
	open(CF2, write, S2),
	btrbk_write_cron(S2),
	close(S2),
	os_shell2([chmod, '711', CF2]),
	true.
soft_configure(_RD, _) :-
	true.

btrbk_write_cfg(S) :-
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

	% write(S, 'volume /.snapshots'), nl(S),
	write(S, 'volume /mnt/btr_pool'), nl(S),
	write(S, '  snapshot_dir btrbk_snapshots'), nl(S), nl(S),

	write(S, '  subvolume @'), nl(S),
	write(S, '  subvolume @opt'), nl(S),
	write(S, '  subvolume @var'), nl(S),
	write(S, '  subvolume @srv'), nl(S),
	write(S, '  subvolume @home'), nl(S),
	true.

btrbk_write_cron(S) :-
	write(S, '#!/bin/sh'), nl(S),
	write(S, 'exec /usr/bin/btrbk -q run'), nl(S),
	true.

