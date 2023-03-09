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
	tui_progressbox(CL, '', [title(' Set locale '), sz([6, 40])]),
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
	( between(48, 57, NC) ->
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
	% IDE
	( os_shell_lines('ls /sys/block | grep -E \'^hd\'', DL1) ->
	  maplist(lx_dev_info_ide_sata, DL1, L1)
	; L1 = []
	),
    % SATA/SCSI and Virtual disks (virtio)
	( os_shell_lines('ls /sys/block | grep -E \'^([sv]|xv)d|mmcblk|nvme\'', DL2) ->
	  maplist(lx_dev_info_ide_sata, DL2, L2)
	; L2 = []
	),
	append(L1, L2, L12),
    % cciss(4) devices
	( os_shell_lines('ls /dev/cciss 2>/dev/null | grep -E \'c[0-9]d[0-9]$\'', DL3) ->
	  maplist(lx_dev_info_cciss, DL3, L3)
	; L3 = []
	),
	append(L12, L3, DL),
	true.

lx_part_info_fs(DA, FS, FSS) :-
	atom_concat('lsblk -nfr ', DA, P2),
	% tui_msgbox(P2, []),
	os_shell_atom_list(P2, L1),
	( L1 = [_, FS|_] ->
	  true
	; FS = none
	),
	atom_concat('lsblk -nr ', DA, P3),
	os_shell_atom_list(P3, L2),
	( L2 = [_, _, _, FSS|_] ->
	  true
	; FSS = unknown
	),
	true.

lx_part_info_disk_(DP, DA, P, part_info(DA, P1, PA, FS, FSS)) :-
	atom_codes(P, PCL),
	split_list_ne(PCL, "/", SL),
	SL = [_, _, _, PL],
	atom_codes(P1, PL),
	% tui_msgbox(P1, []),
	atom_concat(DP, P1, PA),
	lx_part_info_fs(PA, FS, FSS),
	true.

% D - device
% DA - device full name
lx_part_info_disk(D, DA, L1) :-
	format_to_atom(PLA, 'ls -d1 /sys/block/~w/~w*', [D, D]),
	os_shell_lines(PLA, PL),
	% DP - device prefix.
	atom_concat(DP, D, DA),
	% tui_msgbox(DA, []),
	maplist(lx_part_info_disk_(DP, DA), PL, L),
	% write_to_atom(A, L),
	% tui_msgbox(A, []),
	subtract(L, [part_info(_, _, _, iso9660, _), part_info(_, _, _, crypto_LUKS, _), part_info(_, _, _, 'LVM2_member', _)], L1),
	!.
lx_part_info_disk(_, _, []).

lx_dev2part(dev(D, DA, _GB, _DSSZ), L1) :-
	lx_part_info_disk(D, DA, L1),
	true.

lx_part_info_mapper_(P, part_info('/dev/mapper', P, PA, FS, FSS)) :-
	atom_concat('/dev/mapper/', P, PA),
	lx_part_info_fs(PA, FS, FSS),
	true.

lx_part_info_mapper(L2) :-
	os_shell_lines('ls -1 /dev/mapper', L), !,
	subtract(L, [control, 'live-base', 'live-rw'], L1),
	maplist(lx_part_info_mapper_, L1, L2),
	true.
lx_part_info_mapper([]).

lx_part_info_raid_(P, part_info('/dev/md', P1, P, FS, FSS)) :-
	atom_chars(P, PCL),
	split_list_ne(PCL, ['/'], [_, PL]),
	atom_chars(P1, PL),
	lx_part_info_fs(P, FS, FSS),
	true.

lx_part_info_raid(L3) :-
	os_shell_lines('ls -d /dev/md* 2>/dev/null | grep \'[0-9]\'', L1), !,
	maplist(lx_part_info_raid_, L1, L2),
	subtract(L2, [part_info(_, _, _, crypto_LUKS, _), part_info(_, _, _, 'LVM2_member', _)], L3),
	true.
lx_part_info_raid([]).

lx_part_info_cciss_(P, part_info('/dev/cciss', P, PA, FS, FSS)) :-
	atom_concat('/dev/cciss/', P, PA),
	lx_part_info_fs(PA, FS, FSS),
	true.

lx_part_info_cciss(L3) :-
	os_shell_lines('ls /dev/cciss 2>/dev/null | grep -E \'c[0-9]d[0-9]p[0-9]+\'', L1), !,
	maplist(lx_part_info_cciss_, L1, L2),
	subtract(L2, [part_info(_, _, _, crypto_LUKS, _), part_info(_, _, _, 'LVM2_member', _)], L3),
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
	lx_list_dev(DL),
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
	( LL = [_, SL] ->
	  true
	; LL = [_, _, SL]
	),
	atom_codes(S, SL),
	atom_concat(P, S, D),
	true.

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
	% tui_msgbox2([file, name, FN], []),
	open(FN, write, S),
	% tui_msgbox2([open, FN], []),
	write(S, HID),
	close(S),
	% tui_msgbox2([close, FN], []),
	true.

