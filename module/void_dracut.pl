% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

dracut_conf(TL, RD) :-
	member(C, [common, luks, fs(zfs)]),
	dracut_enable(C, TL),
	findall(V, (dracut_conf(C, TL, RD, L), member(V, L)), VL),
	dracut_key(C, K),
	lx_dracut_conf(VL, K, RD),
	fail.
dracut_conf(TL, RD) :-
	get_bootloader(TL, efistub),
	efistub_configure(TL, RD),
	fail.
dracut_conf(_TL, _RD).

dracut_enable(luks, TL) :- !,
	uses_luks(TL).
dracut_enable(fs(zfs), TL) :- !,
	uses_zfs(TL).
dracut_enable(_, _TL) :- !.

dracut_key(fs(FS), FS) :- !.
dracut_key(K, K) :- !.

dracut_conf(common, _TL, _RD, [
		  v(add_dracutmodules, [dm, 'kernel-modules'])
		, v(add_drivers, [ahci, i915])
		, v(omit_dracutmodules, ['dracut-systemd', plymouth, systemd, 'systemd-initrd', usrmount])
		, v(persistent_policy, 'by-uuid')
		, v(tmpdir, '/tmp')
	]) :- !.

dracut_conf(luks, _TL, _RD, [
		  v(add_dracutmodules, [crypt])
		, v(add_drivers, [lz4, lz4hc, xxhash_generic])
		, v(compress, lz4)
		, v(hostonly, yes)
		% , v(kernel_cmdline, ['rd.lvm'=0, 'rd.md'=0, 'rd.dm'=0])
	]).
dracut_conf(luks, TL, RD, [v(install_items, ['/boot/volume.key', '/etc/crypttab'])]) :- !,
	% In case of a dedicated boot partition we do not need a key-file.
	\+ has_boot_part(TL),
	setup_crypt(TL, RD).

dracut_conf(lvm, _TL, _RD, [v(add_dracutmodules, [lvm]), v(add_drivers, [lvm])]) :- !.
dracut_conf(mdraid, _TL, _RD, [v(add_dracutmodules, [mdraid]), v(add_drivers, [mdraid])]) :- !.

dracut_conf(fs(zfs), _TL, _RD, [
		  v(nofsck, yes)
		, v(add_dracutmodules, [zfs])
		, v(omit_dracutmodules, [btrfs, resume])
	]) :- !.
dracut_conf(fs(btrfs), _TL, _RD, [v(add_dracutmodules, [btrfs]), v(add_drivers, [btrfs])]) :- !.
dracut_conf(fs(cifs), _TL, _RD, [v(add_dracutmodules, [cifs]), v(add_drivers, [cifs])]) :- !.
dracut_conf(fs(nfs), _TL, _RD, [v(add_dracutmodules, [nfs]), v(add_drivers, [nfs])]) :- !.

