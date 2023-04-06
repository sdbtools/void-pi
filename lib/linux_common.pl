% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

lx_distro_props(L1) :-
	os_shell_lines('cat /etc/*-release', L),
	maplist(parse_prop, L, L1),
	true.

lx_is_ssd(D) :-
	format_to_atom(C, 'cat /sys/block/~d/queue/rotational', [D]),
	os_shell_number(C, 0).

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
	join_atoms(UGL, ',', GA),
	add_dquote(UN, UNQ),
	CL = [chroot, RD, useradd, '-m', '-G', GA, '-c', UNQ, UL],
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

% D - short device name
% /dev/disk/by-id/XXXXX
lx_get_dev_id(D, DID) :-
	atom_codes(D, DL),
	append(PR, [NC], DL),
	( between(0'0, 0'9, NC) ->
	  atom_codes(D1, PR)
	; D1 = D
	),
	% atom_concat(WWN, NA, PID),
	atom_concat('udevadm info -q symlink --path=/sys/block/', D1, C),
	os_shell_atom_list(C, AL),
	lx_fiter_disk_byid(AL, IDL),
	lx_fiter_disk_wwn(IDL, WWN),
	( D1 = D ->
	  WWN1 = WWN
	; format_to_atom(WWN1, '~w-part~c', [WWN, NC])
	),
	atom_concat('/dev/disk/by-id/', WWN1, DID),
	true.

lx_fiter_disk_byid([H|T], [Suf|T1]) :-
	atom_concat('disk/by-id/', Suf, H), !,
	lx_fiter_disk_byid(T, T1),
	true.
lx_fiter_disk_byid([_|T], T1) :- !,
	lx_fiter_disk_byid(T, T1),
	true.
lx_fiter_disk_byid([], []).

lx_fiter_disk_wwn([H|T], ID) :-
	( atom_concat('wwn-', _, H) ->
	  ID = H
	; lx_fiter_disk_wwn(T, ID)
	), !.
lx_fiter_disk_wwn([H], H).

lx_dev_info_cciss(D, dev(D, DA, GB, DSSZ)) :-
	os_shell_number('cat /sys/block/cciss\\\\!~w/size', [D], DSZ),
	os_shell_number('cat /sys/block/cciss\\\\!~w/queue/hw_sector_size', [D], DSSZ),
	GB is DSZ * DSSZ / 1024 / 1024 /1024,
	atom_concat('/dev/cciss/', D, DA),
	true.

% D - device short name
% DA - device full name
% GB - device size in GB
% DSSZ - device block size
lx_dev_info_ide_sata(D, dev(D, DA, GB, DSSZ)) :-
	os_shell_number('cat /sys/block/~w/size', [D], DSZ),
	os_shell_number('cat /sys/block/~w/queue/hw_sector_size', [D], DSSZ),
	GB is DSZ * DSSZ / 1024 / 1024 /1024,
	atom_concat('/dev/', D, DA),
	true.

lx_list_dev(DL) :-
	os_shell_lines_codes('lsblk -nrdp -o NAME,TYPE,RO,RM,SIZE,PHY-SEC', CLL),
	maplist(lx_list_dev_, CLL, DL),
	true.

lx_list_dev_disk(DL) :-
	lx_list_dev(L),
	findall(D, (member(D, L), D=dev(_NAME,_SNAME,disk,_RO,_RM,_SIZE,_SSZ)), DL),
	true.

lx_list_dev_part(D, PL) :-
	os_shell2_lines_codes([lsblk, '-prn', '-o', 'NAME,KNAME,TYPE,SIZE', D], CLL),
	maplist(lx_list_dev_part_, CLL, PL),
	!.

% SSZ - number.
lx_list_dev_(IL, dev(NAME,SNAME,TYPE,RO,RM,SIZE,SSZ)) :-
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

lx_dev_part_type(part, NAME, part(PARTUUID,UUID)) :- !,
	os_shell2_codes_line(['lsblk -dnr -o PARTUUID,UUID', NAME], CL),
	split_atom(" ", CL, [PARTUUID,UUID]),
	true.
lx_dev_part_type(crypt, NAME, crypt(UUID)) :- !,
	os_shell2_atom(['lsblk -dnr -o UUID', NAME], UUID),
	true.
lx_dev_part_type(Type, _NAME, Type).

% dependency
lx_dev_part_parents(NAME, L1) :-
	os_shell2_lines(['lsblk -prns -o NAME', NAME], L),
	( L = [] ->
	  L1 = []
	; L = [_|L1]
	),
	true.

lx_dev_part_tree(D, PL, OL) :-
	% dev_part(NAME,name(SNAME,KNAME,DL),ET,SIZE)
	lx_dev_part_tree_1(PL, [tree(D, [])], OL),
	true.

lx_dev_part_tree_1([], L, L) :- !.
lx_dev_part_tree_1([dev_part(NAME,name(_SNAME,_KNAME,DL),_ET,_SIZE)|T], IL, OL) :-
	reverse([NAME|DL], RL),
	lx_dev_part_tree_2(RL, IL, IL1),
	lx_dev_part_tree_1(T, IL1, OL),
	true.

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

% memberchk2(member, list, pref, suf) :- !.
memberchk2(V, [V|T], [], T) :- !.
memberchk2(V, [H|T], [H|PL], SL) :-
	memberchk2(V, T, PL, SL), !.

lx_part_info_fs(DA, FS, FSS, Type) :-
	atom_concat('lsblk -nfr ', DA, P2),
	os_shell_atom_list(P2, L1),
	( L1 = [_, FS|_]
	; FS = none
	), !,
	atom_concat('lsblk -nr ', DA, P3),
	os_shell_atom_list(P3, L2),
	( L2 = [_, _, _, FSS, _, Type|_]
	; FSS = unknown,
	  Type = unknown
	),
	!.

lx_part_info_disk_(DP, DA, P, part_info(DA, P1, PA, FS, FSS, Type)) :-
	atom_codes(P, PCL),
	split_list_ne(PCL, "/", SL),
	SL = [_, _, _, PL],
	atom_codes(P1, PL),
	atom_concat(DP, P1, PA),
	lx_part_info_fs(PA, FS, FSS, Type),
	true.

% Get list of partitions for a device.
% D - device
% DA - device full name ??? Is it really necessary ???
lx_part_info_disk(D, DA, L) :-
	format_to_atom(PLA, 'ls -d1 /sys/block/~w/~w*', [D, D]),
	os_shell_lines(PLA, PL),
	% DP - device prefix.
	atom_concat(DP, D, DA),
	maplist(lx_part_info_disk_(DP, DA), PL, L),
	!.

% Get list of partitions for a device except of LUKS, LVM, and iso9660.
lx_part_info_disk_base(D, DA, L1) :-
	lx_part_info_disk(D, DA, L),
	subtract(L, [part_info(_, _, _, iso9660, _, _), part_info(_, _, _, crypto_LUKS, _, _), part_info(_, _, _, 'LVM2_member', _, _)], L1),
	!.
lx_part_info_disk_base(_, _, []).

lx_dev2part(dev(NAME,SNAME,_TYPE,_RO,_RM,_SIZE,_SSZ), L1) :-
	lx_part_info_disk_base(SNAME, NAME, L1),
	true.

lx_part_info_mapper_(P, part_info('/dev/mapper', P, PA, FS, FSS, Type)) :-
	atom_concat('/dev/mapper/', P, PA),
	lx_part_info_fs(PA, FS, FSS, Type),
	true.

lx_part_info_mapper(L2) :-
	os_shell_lines('ls -1 /dev/mapper', L), !,
	subtract(L, [control, 'live-base', 'live-rw'], L1),
	maplist(lx_part_info_mapper_, L1, L2),
	true.
lx_part_info_mapper([]).

lx_part_info_raid_(P, part_info('/dev/md', P1, P, FS, FSS, Type)) :-
	atom_chars(P, PCL),
	split_list_ne(PCL, ['/'], [_, PL]),
	atom_chars(P1, PL),
	lx_part_info_fs(P, FS, FSS, Type),
	true.

lx_part_info_raid(L3) :-
	os_shell_lines('ls -d /dev/md* 2>/dev/null | grep \'[0-9]\'', L1), !,
	maplist(lx_part_info_raid_, L1, L2),
	subtract(L2, [part_info(_, _, _, crypto_LUKS, _, _), part_info(_, _, _, 'LVM2_member', _, _)], L3),
	true.
lx_part_info_raid([]).

lx_part_info_cciss_(P, part_info('/dev/cciss', P, PA, FS, FSS, Type)) :-
	atom_concat('/dev/cciss/', P, PA),
	lx_part_info_fs(PA, FS, FSS, Type),
	true.

lx_part_info_cciss(L3) :-
	os_shell_lines('ls /dev/cciss 2>/dev/null | grep -E \'c[0-9]d[0-9]p[0-9]+\'', L1), !,
	maplist(lx_part_info_cciss_, L1, L2),
	subtract(L2, [part_info(_, _, _, crypto_LUKS, _, _), part_info(_, _, _, 'LVM2_member', _, _)], L3),
	true.
lx_part_info_cciss([]).

% LVM
%         lvs --noheadings | while read lvname vgname perms size; do
%             echo "/dev/mapper/${vgname}-${lvname}"
%             echo "size:${size};fstype:lvm"
%         done
lx_part_info_lvm(L1) :-
	file_exists('/sbin/lvs'), !,
	os_shell_lines('lvs --noheadings', L1), !,
	true.
lx_part_info_lvm([]).

lx_list_part_info(PL) :-
	lx_list_dev_disk(DL),
	% ATA/SCSI/SATA
	maplist(lx_dev2part, DL, PL1),
	% Device Mapper
	lx_part_info_mapper(PL2),
	% raid
	lx_part_info_raid(PL3),
	% cciss(4) devices
	lx_part_info_cciss(PL4),
	flatten([PL1, PL2, PL3, PL4], PL),
	!.

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

