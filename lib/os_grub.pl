% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

grub_sconf(VL, RD) :-
	% v(name, value, comment)
	ALL = [
		  v('GRUB_DEFAULT', 0, 'The default menu entry.')
		, v('GRUB_SAVEDEFAULT', false, 'If this option is set to "true", then, when an entry is selected, save it as a new default entry for use by future runs of GRUB. This is only useful if "GRUB_DEFAULT=saved".')
		, v('GRUB_TIMEOUT', 5, 'Boot the default entry this many seconds after the menu is displayed, unless a key is pressed.')
		, v('GRUB_TIMEOUT_STYLE', 'menu', 'Values: menu, countdown, hidden.') % countdown, hidden
		, v('GRUB_DEFAULT_BUTTON', '', 'Support vendor-specific power button.')
		, v('GRUB_TIMEOUT_BUTTON', '', 'Support vendor-specific power button.')
		, v('GRUB_TIMEOUT_STYLE_BUTTON', '', 'Support vendor-specific power button.')
		, v('GRUB_BUTTON_CMOS_ADDRESS', '', 'Support vendor-specific power button.')
		, v('GRUB_DISTRIBUTOR', 'Unix', 'Set by distributors of GRUB to their identifying name.')
		% console, serial, serial_<port>, at_keyboard, usb_keyboard.
		, v('GRUB_TERMINAL_INPUT', 'console', 'Select the terminal input device. You may select multiple devices here, separated by spaces.')
		% console, serial, serial_<port>, gfxterm, vga_text, mda_text, morse, spkmodem.
		, v('GRUB_TERMINAL_OUTPUT', 'console', 'Select the terminal output device. You may select multiple devices here, separated by spaces.')
		, v('GRUB_TERMINAL', 'console', 'If this option is set, it overrides both "GRUB_TERMINAL_INPUT" and "GRUB_TERMINAL_OUTPUT" to the same value.')
		, v('GRUB_SERIAL_COMMAND', 'serial', 'A command to configure the serial port when using the serial console.')
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
		, v('GRUB_DISABLE_LINUX_UUID', false, 'To disable the use of UUIDs, set this option to "true".')
		, v('GRUB_DISABLE_LINUX_PARTUUID', true, 'To enable the use of partition UUIDs, set this option to "false".')
		, v('GRUB_DISABLE_RECOVERY', false, 'If this option is set to "true", disable the generation of recovery mode menu entries.')
		, v('GRUB_DISABLE_UUID', false, ' To disable this use of UUIDs, set this option to "true".')
		, v('GRUB_VIDEO_BACKEND', '', 'If graphical video support is required, either because the "gfxterm" graphical terminal is in use or because "GRUB_GFXPAYLOAD_LINUX" is set, then grub-mkconfig will normally load all available GRUB video drivers and use the one most appropriate for your hardware. If you need to override this for some reason, then you can set this option.')
		, v('GRUB_GFXMODE', '1920x1080x32', 'Set the resolution used on the ‘gfxterm’ graphical terminal.')
		, v('GRUB_BACKGROUND', '/usr/share/void-artwork/splash.png', 'Set a background image for use with the ‘gfxterm’ graphical terminal.')
		, v('GRUB_THEME', '', 'Set a theme for use with the "gfxterm" graphical terminal.')
		, v('GRUB_GFXPAYLOAD_LINUX', '', 'Set to "text" to force the Linux kernel to boot in normal text mode, "keep" to preserve the graphics mode set using "GRUB_GFXMODE", "widthxheight"["xdepth"] to set a particular graphics mode, or a sequence of these separated by commas or semicolons to try several modes in sequence.')
		, v('GRUB_DISABLE_OS_PROBER', false, 'The grub-mkconfig has a feature to use the external os-prober program to discover other operating systems installed on the same machine and generate appropriate menu entries for them.')
		, v('GRUB_OS_PROBER_SKIP_LIST', '', 'List of space-separated FS UUIDs of filesystems to be ignored from os-prober output.')
		, v('GRUB_DISABLE_SUBMENU', false, 'If this option is set to "true", flat menu with all entries on top level will be generated.')
		, v('GRUB_ENABLE_CRYPTODISK', 'n', 'If set to "y", grub-mkconfig and grub-install will check for encrypted disks and generate additional commands needed to access them during boot.')
		, v('GRUB_INIT_TUNE', '', 'Play a tune on the speaker when GRUB starts.')
		, v('GRUB_BADRAM', '', 'If this option is set, GRUB will issue a badram command to filter out specified regions of RAM.')
		, v('GRUB_PRELOAD_MODULES', '', 'List of GRUB module names separated by spaces.')
		, v('GRUB_HIDDEN_TIMEOUT', '5', 'Wait this many seconds before displaying the menu.')
		, v('GRUB_HIDDEN_TIMEOUT_QUIET', 'false', 'In conjunction with "GRUB_HIDDEN_TIMEOUT", set this to "true" to suppress the verbose countdown while waiting for a key to be pressed before displaying the menu.')
		, v('GRUB_HIDDEN_TIMEOUT_BUTTON', '', 'Support vendor-specific power button')
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

