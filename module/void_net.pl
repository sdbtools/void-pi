% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

set_network(RD) :-
	inst_setting(network, NC), !,
	( set_network_(NC, RD)
	; tui_msgbox('Setting up of network has failed.'),
	  fail
	),
	!.
set_network(_RD).

set_network_(none, _RD) :- !.
set_network_(dhcp(D), RD) :- !,
	( atom_concat('wl', _, D) ->
	  os_call2([cp, '/etc/wpa_supplicant/wpa_supplicant.conf', RD + '/etc/wpa_supplicant']),
	  os_call2([ln, '-sf', '/etc/sv/wpa_supplicant', RD + '/etc/runit/runsvdir/default/wpa_supplicant'])
	; true
	),
	enable_dhcpd(RD),
	true.
set_network_(static(D, IP, GW, DNS1, DNS2), RD) :- !,
	% static IP through dhcpcd.
	atom_concat(RD, '/etc/dhcpcd.conf', CF),
	os_call2([mv, CF, CF + '.orig']),
	open(CF, write, S),
	format(S, '# Static IP configuration set by the void-installer for ~w.\n', [D]),
	format(S, 'interface ~w\n', [D]),
	format(S, 'static ip_address=~w\n', [IP]),
	format(S, 'static routers=~w\n', [GW]),
	format(S, 'static domain_name_servers=~w ~w\n', [DNS1, DNS2]),
	close(S),
	enable_dhcpd(RD),
	true.
set_network_(_, _RD) :- !,
	tui_msgbox('Invalid network connection type.'),
	fail.

enable_dhcpd(RD) :-
	os_call2([ln, '-sf', '/etc/sv/dhcpcd', RD + '/etc/runit/runsvdir/default/dhcpcd']),
	true.

test_network(_) :-
	% tui_msgbox('before xbps-uhelper fetch'),
	tui_infobox('Testing network connection.', [sz([4, 40])]),
	between(1, 20, _),
	sleep(0.25),
	os_rm_f(otime),
	os_shell('xbps-uhelper fetch https://repo-default.voidlinux.org/current/otime 1>/dev/null 2>&1'),
	% os_shell('xbps-uhelper fetch https://repo-default.voidlinux.org/current/otime 2>&1'),
	% tui_progressbox_safe(['xbps-uhelper', fetch, 'https://repo-default.voidlinux.org/current/otime', '2>&1'], '', [title(' Test Network Connection '), sz([6, 80])]),
	tui_msgbox('Network is working properly!', [sz([6, 40])]),
	!.
test_network(nm) :-
	tui_msgbox('Network Manager is enabled but network is inaccessible, please set it up externally with nmcli, nmtui, or the Network Manager tray applet.', [sz([6, 40])]), !,
	fail.
test_network(_) :-
	tui_msgbox('Network is inaccessible, please set it up properly.', [sz([6, 40])]), !,
	fail.

select_net_conf(D, dhcp) :-
	format_to_atom(A, 'Do you want to use DHCP or STATIC for ~w?', [D]),
	tui_yesno(A, [yes-label(dhcp), no-label(static), sz([6, 40])]), !.
select_net_conf(_, static).

configure_net(D, dhcp) :-
	% tui_msgbox('dhcp'),
	lx_iface_setup(D, RC1),
	( RC1 = 1 ->
	  ( os_shell2([sv, restart, dhcpcd, '1>/dev/null'])
	  ; tui_msgbox('ERROR: failed to run dhcpcd'),
	    fail
	  ), !,
	  ( tui_infobox('Retrieving IP address.', [sz([6, 40])]),
		between(1, 40, _),
		sleep(0.25),
		lx_iface_setup(D, 0)
	  ; tui_msgbox2(['ERROR: DHCP request failed for', D]),
	    fail
	  )
	; true
	),
	test_network(any),
	retractall(inst_setting(network, _)),
	assertz(inst_setting(network, dhcp(D))),
	!.
configure_net(D, static) :-
	format_to_atom(MA, 'Static IP configuration for ~w:', [D]),
	dialog_msg(form, FORMLABEL),
	tui_form_v(20, 0, [
		item('IP address:'),
		item('Gateway:'),
		item('DNS Primary', '8.8.8.8'),
		item('DNS Secondary', '8.8.4.4')
		], FORMLABEL, [title(MA)], L),
	L = [IP, GW, DNS1, DNS2| _],
	( IP = '' -> tui_msgbox('IP adress is missing'), fail ; true),
	( GW = '' -> tui_msgbox('Gateway adress is missing'), fail ; true),
	( DNS1 = '' -> tui_msgbox('Primary DNS is missing'), fail ; true),
	( DNS2 = '' -> tui_msgbox('Secondary DNS is missing'), fail ; true),
	( os_shell2([ip, link, set, dev, D, up])
	; format_to_atom(EA1, 'ERROR: Failed to bring ~w interface.', [D]),
	  tui_msgbox(EA1),
	  fail
	), !,
	% format_to_atom(IPA, 'ip addr add "~w" dev ~w', [IP, D]),
	( os_call2([ip, addr, add, IP, dev, D])
	; format_to_atom(EA2, 'ERROR: Failed to set ip to the ~w interface.', [D]),
	  tui_msgbox(EA2),
	  fail
	), !,
	( os_call2([ip, route, add, default, via, GW])
	; format_to_atom(EA3, 'ERROR: Failed to setup gateway.', []),
	  tui_msgbox(EA3),
	  fail
	), !,
	open('/etc/resolv.conf', write, S),
	write(S, 'nameserver '), write(S, DNS1), nl(S),
	write(S, 'nameserver '), write(S, DNS2), nl(S),
	close(S),
    test_network(all),
	retractall(inst_setting(network, _)),
	assertz(inst_setting(network, static(D, IP, GW, DNS1, DNS2))),
	!.

configure_wifi(D) :-
	% format_to_atom(MA, 'Wireless configuration for ~w\n(encryption type: wep or wpa)', [D]),
	format_to_atom(MA, ' Wireless configuration for ~w (wep or wpa) ', [D]),
	dialog_msg(form, FORMLABEL),
	tui_mixedform_v(30, 100, [
		item('SSID:', ''),
		item('Password:', '', 1),
		item('Encryption:', 'wpa')
		], FORMLABEL, [title(MA)], L),
	L = [SSID, PSWD, ENCR| _],
	( SSID = '' ->
	  tui_msgbox('Invalid SSID.'), fail
	; true
	),
	( \+ member(ENCR, [wep, wpa]) ->
	  tui_msgbox('Invalid encryption type (possible values: wep or wpa'), fail
	; true
	),
	( PSWD = '' ->
	  tui_msgbox('Invalid AP password.'), fail
	; true
	),
	WPASUPCONF = '/etc/wpa_supplicant/wpa_supplicant.conf',
    % reset the configuration to the default, if necessary otherwise backup the configuration
	atom_concat(WPASUPCONF, '.orig', OF),
	( file_exists(OF) ->
	  os_call2([cp, '-f', OF, WPASUPCONF])
	; os_call2([cp, '-f', WPASUPCONF, OF])
	),
	( ENCR = 'wep' ->
	  format_to_atom(WA, 'network={\n\tssid="~w"\n\twep_key0="~w"\n\twep_tx_keyidx=0\n\tauth_alg=SHARED\n}', [SSID, PSWD]),
	  open(WPASUPCONF, write, S),
	  write(S, WA),
	  close(S)
	; format_to_atom(WPAA, 'wpa_passphrase "~w" "~w" >> ~w', [SSID, PSWD, WPASUPCONF]),
	  os_shell(WPAA)
	  % os_shell2([wpa_supplicant, '-B', '-i', D, '-c', WPASUPCONF])
	),
    os_call2([sv, restart, wpa_supplicant]),
    configure_net(D, dhcp),
	true.

configure_net(none) :- !,
	retractall(inst_setting(network, _)),
	assertz(inst_setting(network, none)).
configure_net(D) :-
	select_net_conf(D, NT),
	configure_net(D, NT),
	true.

get_mac_addr(N, [N, M]) :-
	lx_get_mac_addr(N, M),
	true.

net_dev_name(dhcp(D), D) :- !.
net_dev_name(static(D, _IP, _GW, _DNS1, _DNS2), D) :- !.

get_net_devs :-
	lx_get_net_devs(AL1),
	maplist(get_mac_addr, AL1, AL2),
	dialog_msg(radiolist, RADIOLABEL),
	( inst_setting(network, D), net_dev_name(D, ON) ->
	  true
	; ON = none
	),
	append(AL2, [[none, 'Disable network']], AL3),
	tui_radiolist_tag2(AL3, ON, RADIOLABEL, [title(' Select the network interface to configure ')], Tag), !,
	( Tag = ON ->
	  % value hasn't change.
	  true
	; ( atom_concat('wl', _, Tag) ->
	    configure_wifi(Tag)
	  ; configure_net(Tag)
	  )
	),
	true.


