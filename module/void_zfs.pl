% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% Installing Void on a ZFS Root - https://docs.voidlinux.org/installation/guides/zfs.html

uses_zfs(TL) :-
	% fs4(Name, Label, MountPoint, [DevList])
	memberchk(fs4(zfs, _, _, _), TL),
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

install_zfs(TL, RD) :-
	uses_zfs(TL),
	lx_gen_hostid(''),
	\+ host_name(hrmpf),
	inst_setting(system(arch), ARCH),
	make_chroot_inst_pref_chroot(ARCH, Pref, RD),
	soft_install_deps(Pref, [zfs]),
	!.
install_zfs(_TL, _).

