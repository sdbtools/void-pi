% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

lx_distro_props(L1) :-
	os_shell_lines('cat /etc/*-release', L),
	maplist(parse_prop, L, L1),
	true.

lx_is_ssd(D) :-
	format_to_atom(C, 'cat /sys/block/~d/queue/rotational', [D]),
	os_shell_number(C, 0).

% Similar to lx_sys_kernel.
lx_kernel_ver(RD, LV) :-
	os_shell2_codes_line([ls, RD + '/lib/modules/'], KL),
	split_list_ne(KL, ".", [V1, V2|_]),
	maplist(codes_atom, [V1, V2], VL),
	format_to_atom(LV, '~w.~w', VL),
	true.

% V1N - number
% V2N - number
% V3A - atom
lx_sys_kernel(V1N, V2N, V3A) :-
	os_shell_codes_line('uname -r', CL),
	split_list_ne(CL, ".", [V1, V2, V3| _]),
	number_codes(V1N, V1),
	number_codes(V2N, V2),
	atom_codes(V3A, V3),
	true.

lx_sys_arch(ARCH) :-
	% x86_64 | x86_64-musl ...
	% os_shell_line('xbps-uhelper arch', ARCH),
	os_shell_line('uname -m', A0),
	( A0 = x86_64, file_exists('/lib/ld-musl-x86_64.so.1') ->
	  ARCH = 'x86_64-musl'
	; ARCH = A0
	),
	true.

% Check for EFI.
lx_sys_efi(EFI_TARGET) :-
	file_exists('/sys/firmware/efi/systab'),
	os_shell2_number([cat, '/sys/firmware/efi/fw_platform_size'], N),
	( N = 64 ->
	  EFI_TARGET='x86_64-efi'
	; EFI_TARGET='i386-efi'
	),
	true.

% Generate EFI value.
lx_gen_efi(EFI_TARGET) :-
	os_shell_line('uname -m', A0),
	( A0 ='x86_64' ->
	  EFI_TARGET = 'x86_64-efi'
	; EFI_TARGET = 'i386-efi'
	),
	!.

% N is atom.
lx_parent_dev_name(D, N, PD) :-
	% sub_atom(Atom, Before, Length, After, SubAtom),
	sub_atom(D, _, 1, 0, N),
	( atom_concat('/dev/nvme', _, D) ->
	  sub_atom(D, 0, _, 2, PD)
	; sub_atom(D, 0, _, 1, PD)
	).

% N is digit.
lx_part_name(D, N, PD) :-
	atom_concat('/dev/nvme', _, D), !,
	format_to_atom(PD, '~wp~d', [D, N]).
lx_part_name(D, N, PD) :-
	format_to_atom(PD, '~w~d', [D, N]).

lx_set_keymap(RD, KM) :-
	file_exists('/etc/vconsole.conf'), !,
	format_to_atom(KMA, 's|KEYMAP=.*|KEYMAP=~w|g', [KM]),
	os_call2([sed, '-i', '-e', KMA, RD + '/etc/vconsole.conf']).
lx_set_keymap(RD, KM) :-
	format_to_atom(KMA, 's|#\\?KEYMAP=.*|KEYMAP=~w|g', [KM]),
	os_call2([sed, '-i', '-e', KMA, RD + '/etc/rc.conf']).

lx_set_locale(RD, LC) :-
	atom_concat(RD, '/etc/default/libc-locales', FN),
	file_exists(FN),
	format_to_atom(LCA1, 's|LANG=.*|LANG=~w|g', [LC]),
	atom_concat(RD, '/etc/locale.conf', LCN),
	os_call2([sed, '-i', '-e', LCA1, LCN]),
	% Uncomment locale from /etc/default/libc-locales and regenerate it.
	format_to_atom(LCA2, '/~w/s/^\\#//', [LC]),
	os_call2([sed, '-e', LCA2, '-i', FN]) ,
	CL = [chroot, RD, 'xbps-reconfigure', '-f', 'glibc-locales', '2>&1'],
	% os_shell2l(CL)
	tui_progressbox_safe(CL, '', [title(' Set locale '), sz([6, 40])]),
	!.

lx_set_timezone(RD, TZ) :-
	C = [ln, '-sf', '/usr/share/zoneinfo/' + TZ, RD + '/etc/localtime'],
	os_call2(C),
	!.

lx_set_hostname(RD, HN) :-
	format_to_atom(HNA, 'echo "~w" > ~w/etc/hostname', [HN, RD]),
	os_shell(HNA),
	!.

lx_set_password(RD, UL, UP) :-
	format_to_atom(UPA, 'echo "~w:~w" | chpasswd -R ~w -c SHA512', [UL, UP, RD]),
	os_shell(UPA),
	!.

lx_useradd_rc(0, success).
lx_useradd_rc(1, 'can\'t update password file').
lx_useradd_rc(2, 'invalid command syntax').
lx_useradd_rc(3, 'invalid argument to option').
lx_useradd_rc(4, 'UID already in use (and no -o)').
lx_useradd_rc(6, 'specified group doesn\'t exist').
lx_useradd_rc(9, 'username already in use').
lx_useradd_rc(10, 'can\'t update group file').
lx_useradd_rc(12, 'can\'t create home directory').
lx_useradd_rc(14, 'can\'t update SELinux user mapping').

% UL - user login
% UN - user name
% UGL - user group list
lx_chroot_useradd_rc(RD, UL, UN, UGL, RC) :-
	% There is no need to add quotes here because this is an os_call2_rc.
	CL = [chroot, RD, useradd, '-m', '-G', lc(UGL), '-c', UN, UL],
	os_call2_rc(CL, RC).

lx_chroot_useradd(RD, UL, UN, UGL) :-
	lx_chroot_useradd_rc(RD, UL, UN, UGL, 0).

% UL - user login
% UGL - user group list
lx_chroot_set_sudoers(UL, UGL) :-
	file_exists('/mnt/etc/sudoers.d'),
	( memberchk(wheel, UGL) ->
	  % enable the sudoers entry for members of group wheel
	  os_shell('echo "%wheel ALL=(ALL:ALL) ALL" > "/mnt/etc/sudoers.d/wheel"')
	; % enable sudo for primary user USERLOGIN who is not member of wheel
	  format_to_atom(C1, 'echo "# Enable sudo for login \'~w\'" > "/mnt/etc/sudoers.d/$USERLOGIN"', [UL]),
	  os_shell(C1),
	  format_to_atom(C2, 'echo "~w ALL=(ALL:ALL) ALL" >> "/mnt/etc/sudoers.d/~w"', [UL, UL]),
	  os_shell(C2)
	),
	!.

lx_iface_setup(D, RC) :-
	format_to_atom(A, 'ip addr show dev ~w | grep -q -e \'inet \' -e \'inet6 \'', [D]),
	os_shell_rc(A, RC),
	true.

% UUID is a mechanism to give each filesystem a unique identifier.
lx_get_dev_uuid(D, A) :-
	atom_concat('blkid -s UUID -o value ', D, C),
	os_shell_line(C, A).

lx_get_dev_disk_uuid(D, PID) :-
	lx_get_dev_uuid(D, A),
	atom_concat('/dev/disk/by-uuid/', A, PID).

% GPT partition UUIDs are defined in the partition entry on GPT disks.
% /dev/disk/by-partuuid/XXXXX
lx_get_dev_partuuid(D, PID) :-
	atom_concat('blkid -s PARTUUID -o value ', D, C),
	os_shell_line(C, PID).

lx_get_dev_disk_partuuid(D, PID) :-
	lx_get_dev_partuuid(D, A),
	atom_concat('/dev/disk/by-partuuid/', A, PID).

% D - long device name
% /dev/disk/by-id/XXXXX
lx_get_dev_disk_id(D, DID) :-
	atom_concat('udevadm info -q symlink --name=', D, C),
	os_shell_atom_list(C, AL),
	findall(Suf, (member(A, AL), atom_concat('disk/by-id/', Suf, A)), IDL),
	lx_fiter_disk_wwn(IDL, WWN),
	atom_concat('/dev/disk/by-id/', WWN, DID),
	true.

lx_fiter_disk_wwn([H|T], ID) :-
	( atom_concat('wwn-', _, H) ->
	  ID = H
	; lx_fiter_disk_wwn(T, ID)
	), !.
lx_fiter_disk_wwn([H], H).

% D - device short name
% DA - device full name
% GB - device size in GB
% DSSZ - device block size
lx_dev_info_ide_sata(D, dev4(D, DA, GB, DSSZ)) :-
	os_shell_number('cat /sys/block/~w/size', [D], DSZ),
	os_shell_number('cat /sys/block/~w/queue/hw_sector_size', [D], DSSZ),
	GB is DSZ * DSSZ / 1024 / 1024 /1024,
	atom_concat('/dev/', D, DA),
	true.

lx_list_dev7(DL) :-
	os_shell_lines_codes('lsblk -nrdp -o NAME,TYPE,RO,RM,SIZE,PHY-SEC', CLL),
	maplist(lx_list_dev_, CLL, DL),
	true.

lx_list_dev7_disk(DL) :-
	lx_list_dev7(L),
	findall(D, (member(D, L), D=dev7(_NAME,_SNAME,disk,_RO,_RM,_SIZE,_SSZ)), DL),
	true.

% List all available partitions.
lx_list_part(PL) :-
	findall(P, (lx_list_dev_part([_| PL0]), member(P, PL0)), PL),
	true.

% It supposed to be used with backtracking.
lx_list_dev_part(PL) :-
	lx_list_dev7_disk(DL),
	member(DEV7, DL),
	lx_dev7_to_ldn(DEV7, D),
	lx_list_dev_part(D, PL),
	true.

% PL - linear list of a device and all sub-devices.
lx_list_dev_part(D, PL) :-
	% lsblk -prn -o NAME,KNAME,TYPE,SIZE
	os_shell2_lines_codes([lsblk, '-prn', '-o', 'NAME,KNAME,TYPE,SIZE', D], CLL),
	maplist(lx_list_dev_part_, CLL, PL),
	!.

% SSZ - number.
lx_list_dev_(IL, dev7(NAME,SNAME,TYPE,RO,RM,SIZE,SSZ)) :-
	split_list_ne(IL, " ", LL),
	LL = [H|_],
	split_list_ne(H, "/", NL),
	append(_, [SN], NL),
	maplist(codes_atom, [SN|LL], [SNAME,NAME,TYPE,RO,RM,SIZE,SSIZE]),
	number_atom(SSZ, SSIZE),
	true.

% SIZE - atom
% name(sort_name, kernel_name, dev_dep_list)
% dev_part(name, ext_name, type, size)
lx_list_dev_part_(IL, dev_part(NAME,name(SNAME,KNAME,DL),ET,SIZE)) :-
	split_list(IL, " ", LL),
	LL = [H|_],
	split_list_ne(H, "/", NL),
	append(_, [SN], NL),
	maplist(codes_atom, [SN|LL], [SNAME,NAME,KNAME,TYPE,SIZE]),
	lx_dev_part_type(TYPE, NAME, ET),
	lx_dev_part_parents(NAME, DL),
	true.

% PTTYPE: gpt, dos, sun, sgi
lx_dev_part_type(part, NAME, part5(PTTYPE,TYPE,PARTUUID,UUID,FSTYPE)) :- !,
	os_shell2_codes_line(['lsblk -dnr -o PTTYPE,PARTUUID,UUID,PARTTYPE,FSTYPE', NAME], CL),
	split_atom(" ", CL, [PTTYPE,PARTUUID,UUID,PARTTYPE,FSTYPE]),
	lx_part_type(PTTYPE, PARTTYPE, TYPE),
	true.
lx_dev_part_type(crypt, NAME, crypt(UUID)) :- !,
	% !!! UUID doesn't seem to exist.
	os_shell2_atom(['lsblk -dnr -o UUID', NAME], UUID),
	true.
lx_dev_part_type(lvm, NAME, lv(VG,LV)) :-
	lvm_lvs(L),
	memberchk(lv(VG,LV,NAME), L),
	true.
lx_dev_part_type(Type, _NAME, Type).

lx_part_type(gpt, PARTTYPE, TYPE) :- !,
	os_gpt_part_type(PARTTYPE, TYPE, _),
	!.
lx_part_type(dos, PARTTYPE, TYPE) :- !,
	os_mbr_part_type(PARTTYPE, [t4(TYPE, _, _, _)|_]),
	!.
lx_part_type(_, _PARTTYPE, unknown).

% dependency
lx_dev_part_parents(NAME, OLL) :-
	os_shell2_lines_codes(['lsblk -pns -o NAME', NAME], L),
	lx_dev_part_parents_0(L, OLL).

lx_dev_part_parents_0([], []) :- !.
lx_dev_part_parents_0([_|T], LL) :-
	lx_dev_part_parents_1(T, LL).

lx_dev_part_parents_1([], []) :- !.
lx_dev_part_parents_1(IL, [RL|OLL]) :-
	lx_dev_part_parents_2(IL, 0, RL, OL1),
	lx_dev_part_parents_1(OL1, OLL).

% RL - result list
% lx_dev_part_parents_2(IL, N, RL, OL) :- !.
lx_dev_part_parents_2([], _N, [], []) :- !.
lx_dev_part_parents_2([H|T], N, [S|RL], OL) :-
	suffix(0'/, H, N1, SL),
	N1 > N, !,
	atom_codes(S, SL),
	lx_dev_part_parents_2(T, N1, RL, OL),
	true.
lx_dev_part_parents_2(IL, _N, [], IL).

% PL - partition list
% TL - tree list of long device names.
lx_make_dev3(D, dev3(D, PL, TL)) :-
	lx_list_dev_part(D, PL),
	lx_dev_part_tree(D, PL, TL),
	true.

lx_dev7_to_dev3(dev7(LN,_SNAME,_TYPE,_RO,_RM,_SIZE,_SSZ), DEV3) :-
	lx_make_dev3(LN, DEV3),
	true.

lx_dev3_to_sdn(dev3(_, [dev_part(_, name(SN, _, _), _, _)| _], _TL), SN).

% convert dev7 to long device name.
lx_dev7_to_ldn(dev7(LN,_SN,_TYPE,_RO,_RM,_SIZE,_SSZ), LN).

% convert dev7 to short device name.
lx_dev7_to_sdn(dev7(_LN,SN,_TYPE,_RO,_RM,_SIZE,_SSZ), SN).

% convert dev7 to long and short device names. 
lx_dev7_to_ldn_sdn(dev7(LN,SN,_TYPE,_RO,_RM,_SIZE,_SSZ), LN, SN).

% Short device name to dev7
lx_sdn_to_dev7(L, SDN, DEV7) :-
	member(DEV7, L),
	DEV7 = dev7(_LN,SDN,_TYPE,_RO,_RM,_SIZE,_SSZ),
	!.

% Long device name to dev7
lx_ldn_to_dev7(L, LDN, DEV7) :-
	member(DEV7, L),
	DEV7 = dev7(LDN,_SD,_TYPE,_RO,_RM,_SIZE,_SSZ),
	!.

lx_dev_part_tree(D, PL, OL) :-
	% dev_part(NAME,name(SNAME,KNAME,DL),ET,SIZE)
	lx_dev_part_tree_1(D, PL, [tree(D, [])], OL),
	true.

% lx_dev_part_tree_1(D, PL, IL, OL)
lx_dev_part_tree_1(_D, [], L, L) :- !.
lx_dev_part_tree_1(D, [dev_part(NAME,name(_SNAME,_KNAME,DL),_ET,_SIZE)|T], IL, OL) :-
	( DL = [] ->
	  DLM = DL
	; member(DLM, DL),
	  memberchk(D, DLM)
	), !,
	reverse([NAME|DLM], RL),
	lx_dev_part_tree_2(RL, IL, IL1),
	lx_dev_part_tree_1(D, T, IL1, OL),
	true.

% lx_dev_part_tree_2(RL, IL, OL)
lx_dev_part_tree_2([], L, L) :- !.
lx_dev_part_tree_2([H|T], IL, OL) :-
	memberchk2(tree(H, L), IL, PL, SL), !,
	lx_dev_part_tree_2(T, L, L1),
	append(PL, [tree(H, L1)|SL], OL),
	true.
lx_dev_part_tree_2([H|T], IL, [tree(H, L)|IL]) :-
	lx_dev_part_tree_3(T, L).

lx_dev_part_tree_3([], []) :- !.
lx_dev_part_tree_3([H|T], [tree(H, L)]) :-
	lx_dev_part_tree_3(T, L).

% In addition to checking for membership retrieve prefix and suffix lists
% surrounding a member of a list.
% memberchk2(member, list, pref, suf)
memberchk2(V, [V|T], [], T) :- !.
memberchk2(V, [H|T], [H|PL], SL) :-
	memberchk2(V, T, PL, SL), !.

% D - device
% P - prefix
% S - suffix
lx_split_dev(D, P, S) :-
	atom_codes(D, L),
	split_list_ne(L, "/", LL),
	( LL = [_, SL]
	; LL = [_, _, SL]
	),
	atom_codes(S, SL),
	atom_concat(P, S, D),
	!.

lx_split_dev_codes(D, PL, SL) :-
	atom_codes(D, L),
	split_list_ne(L, "/", LL),
	( LL = [_, SL]
	; LL = [_, _, SL]
	),
	append(PL, SL, L),
	!.

lx_get_mac_addr_codes(N, MC) :-
	format_to_atom(C, '/sys/class/net/~w/address', [N]),
	open(C, read, S),
	read_stream_codes_line(S, MC),
	close(S),
	!.

lx_get_mac_addr(N, M) :-
	lx_get_mac_addr_codes(N, MC),
	atom_codes(M, MC).

lx_get_net_devs(AL) :-
	% os_shell_lines('ls /sys/class/net', CL),
	% delete(CL, lo, AL),
	directory_files('/sys/class/net', CL),
	subtract(CL, [lo, '.', '..'], AL),
	!.

lx_gen_hostid(RD) :-
	lx_get_net_devs([ND|_]),
	lx_get_mac_addr_codes(ND, MC),
	split_list_ne(MC, ":", ML),
	flatten(ML, FML),
	atom_codes(HID, FML),
	atom_concat(RD, '/etc/hostid', FN),
	open(FN, write, S),
	write(S, HID),
	close(S),
	true.

