% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

grub_sconf(VL, RD) :-
	% v(name, value, comment)
	ALL = [
		  v('GRUB_DEFAULT', 0, '')
		, v('GRUB_SAVEDEFAULT', false, '')
		, v('GRUB_TIMEOUT', 5, '')
		, v('GRUB_TIMEOUT_STYLE', 'menu', '') % countdown, hidden
		, v('GRUB_DEFAULT_BUTTON', '', '')
		, v('GRUB_TIMEOUT_BUTTON', '', '')
		, v('GRUB_TIMEOUT_STYLE_BUTTON', '', '')
		, v('GRUB_BUTTON_CMOS_ADDRESS', '', '')
		, v('GRUB_DISTRIBUTOR', 'Unix', '')
		% console, serial, serial_<port>, at_keyboard, usb_keyboard.
		, v('GRUB_TERMINAL_INPUT', 'console', 'Select the terminal input device. You may select multiple devices here, separated by spaces.')
		% console, serial, serial_<port>, gfxterm, vga_text, mda_text, morse, spkmodem.
		, v('GRUB_TERMINAL_OUTPUT', 'console', 'Uncomment to disable graphical terminal')
		, v('GRUB_TERMINAL', 'console', '')
		, v('GRUB_SERIAL_COMMAND', 'serial', '')
		, v('GRUB_CMDLINE_LINUX', '', 'Command-line arguments to add to menu entries for the Linux kernel.')
		, v('GRUB_CMDLINE_LINUX_DEFAULT', 'loglevel=4', ' This option lists command-line arguments to add only to the default menu entry, after those listed in GRUB_CMDLINE_LINUX.')
		% , v('GRUB_CMDLINE_NETBSD', '', '')
		% , v('GRUB_CMDLINE_NETBSD_DEFAULT', '', '')
		% , v('GRUB_CMDLINE_GNUMACH', '', '')
		% , v('GRUB_CMDLINE_XEN', '', '')
		% , v('GRUB_CMDLINE_XEN_DEFAULT', '', '')
		% , v('GRUB_CMDLINE_LINUX_XEN_REPLACE', '', '')
		% , v('GRUB_CMDLINE_LINUX_XEN_REPLACE_DEFAULT', '', '')
		% , v('GRUB_EARLY_INITRD_LINUX_CUSTOM', '', '')
		% , v('GRUB_EARLY_INITRD_LINUX_STOCK', '', '')
		, v('GRUB_DISABLE_LINUX_UUID', false, '')
		, v('GRUB_DISABLE_LINUX_PARTUUID', false, '')
		, v('GRUB_DISABLE_RECOVERY', false, 'If this option is set to "true", disable the generation of recovery mode menu entries.')
		, v('GRUB_DISABLE_UUID', false, '')
		, v('GRUB_VIDEO_BACKEND', '', '')
		, v('GRUB_GFXMODE', '1920x1080x32', '')
		, v('GRUB_BACKGROUND', '/usr/share/void-artwork/splash.png', '')
		, v('GRUB_THEME', '', '')
		, v('GRUB_GFXPAYLOAD_LINUX', '', '')
		, v('GRUB_DISABLE_OS_PROBER', false, '')
		, v('GRUB_OS_PROBER_SKIP_LIST', '', '')
		, v('GRUB_DISABLE_SUBMENU', false, '')
		, v('GRUB_ENABLE_CRYPTODISK', 'n', '')
		, v('GRUB_INIT_TUNE', '', '')
		, v('GRUB_BADRAM', '', '')
		, v('GRUB_PRELOAD_MODULES', '', 'List of GRUB module names separated by spaces.')
		, v('GRUB_COLOR_NORMAL', 'light-blue/black', '')
		, v('GRUB_COLOR_HIGHLIGHT', 'light-cyan/blue', '')
		% Deprecated
		% , v('GRUB_HIDDEN_TIMEOUT', '', '')
		% Deprecated
		% , v('GRUB_HIDDEN_TIMEOUT_QUIET', '', '')
		% Deprecated
		% , v('GRUB_HIDDEN_TIMEOUT_BUTTON', '', '')
	],
	atom_concat(RD, '/etc/default/grub', CF),
	open(CF, write, S),
	write(S, '#\n'),
	write(S, '# Configuration file for GRUB.\n'),
	write(S, '#\n'),
	maplist(write_grub_sconf_value(VL, S), ALL),
	close(S),
	true.

write_grub_sconf_value(VL, S, v(K, V1, C1)) :-
	( memberchk(v(K, V2, C2), VL) ->
	  write_grub_sconf_value(S, K, V2, C2, enable)
	; write_grub_sconf_value(S, K, V1, C1, disable)
	),
	true.

write_grub_sconf_value(S, K, V, C, ED) :-
	( C = ''
	; write(S, '# '), write(S, C), nl(S)
	),
	( ED = enable
	; write(S, '#')
	),
	write(S, K), write(S, '='),
	( number(V) ->
	  write(S, V), nl(S)
	; write(S, '"'), write(S, V), write(S, '"'), nl(S)
	),
	!.

