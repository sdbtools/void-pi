% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

uses_zfs :-
	% part(Dev, Part, PartDev, PartType, FileSystem, Label, MountPoint, create/keep, size)
	inst_setting(partition, part(_D, _P, _PD, _PT, zfs, _Label, _MP, _CK, _SZ)),
	!.

make_zfs_pool_cmd(MP, DS, AL, [zfs, create, '-o', MPA|T]) :-
	atom_concat('mountpoint=', MP, MPA),
	make_zfs_pool_cmd_(DS, AL, T),
	true.

make_zfs_pool_cmd_(DS, [H|T], ['-o', H|T1]) :-
	make_zfs_pool_cmd_(DS, T, T1),
	true.
make_zfs_pool_cmd_(DS, [], [DSA]) :-
	atom_concat('zroot/', DS, DSA),
	true.

create_zfs_dataset :-
	% Create initial filesystems
	% dataset(Mountpoint, dataset name, attrs)
	inst_setting(zfs, dataset(DS, MP, AL)),
	make_zfs_pool_cmd(MP, DS, AL, CMD),
	os_shell2(CMD),
	fail.
create_zfs_dataset :-
	true.

install_zfs(RD) :-
	uses_zfs,
	lx_gen_hostid(''),
	\+ host_name(hrmpf),
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	install_deps(Pref, [zfs]),
	!.
install_zfs(_).

