% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

dracut_conf(RD) :-
	uses_zfs,
	dracut_conf(fs(zfs), RD),
	fail.
dracut_conf(RD) :-
	uses_luks,
	dracut_conf(luks, RD),
	fail.
dracut_conf(RD) :-
	dracut_conf(common, RD).

dracut_conf(common, RD) :- !,
	VL = [
		  v(add_dracutmodules, [dm, 'kernel-modules'])
		, v(add_drivers, [ahci, i915])
		, v(omit_dracutmodules, ['dracut-systemd', plymouth, systemd, 'systemd-initrd', usrmount])
		, v(persistent_policy, 'by-uuid')
		, v(tmpdir, '/tmp')
	],
	dracut_conf(VL, common, RD),
	true.
dracut_conf(luks, RD) :- !,
	setup_crypt(RD),
	VL = [
		  v(add_dracutmodules, [crypt])
		, v(add_drivers, [lz4, lz4hc, xxhash_generic])
		, v(compress, lz4)
		, v(hostonly, yes)
		, v(install_items, ['/boot/volume.key', '/etc/crypttab'])
		% , v(kernel_cmdline, ['rd.lvm=0', 'rd.md=0', 'rd.dm=0'])
	],
	dracut_conf(VL, luks, RD),
	true.
dracut_conf(lvm, RD) :- !,
	VL = [
		  v(add_dracutmodules, [lvm])
		, v(add_drivers, [lvm])
	],
	dracut_conf(VL, lvm, RD),
	true.
dracut_conf(mdraid, RD) :- !,
	VL = [
		  v(add_dracutmodules, [mdraid])
		, v(add_drivers, [mdraid])
	],
	dracut_conf(VL, mdraid, RD),
	true.
dracut_conf(fs(zfs), RD) :- !,
	VL = [
		  v(nofsck, yes)
		, v(add_dracutmodules, [zfs])
		, v(omit_dracutmodules, [btrfs, resume])
	],
	dracut_conf(VL, zfs, RD),
	true.
dracut_conf(fs(btrfs), RD) :- !,
	VL = [
		  v(add_dracutmodules, [btrfs])
		, v(add_drivers, [btrfs])
	],
	dracut_conf(VL, btrfs, RD),
	true.
dracut_conf(fs(cifs), RD) :- !,
	VL = [
		  v(add_dracutmodules, [cifs])
		, v(add_drivers, [cifs])
	],
	dracut_conf(VL, cifs, RD),
	true.
dracut_conf(fs(nfs), RD) :- !,
	VL = [
		  v(add_dracutmodules, [nfs])
		, v(add_drivers, [nfs])
	],
	dracut_conf(VL, nfs, RD),
	true.
dracut_conf(fs(_), _RD) :- !,
	true.


