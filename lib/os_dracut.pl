% vi: noexpandtab:tabstop=4:ft=prolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% v(name, value, comment)
dracut_conf(VL, FN, RD) :-
	format_to_atom(DF, '~w/etc/dracut.conf.d/~w.conf', [RD, FN]),
	open(DF, write, S),
	maplist(write_dracut_value(S), VL),
	close(S),
	true.

write_dracut_value(S, VV) :-
	( VV = v(K, V, C1)
	; VV = v(K, V), C1 = ''
	),
	dracut_info(K, OP, F, C2),
	% Write comment.
	write(S, '# '),
	( C1 \= '' ->
	  write(S, C1)
	; write(S, C2)
	),
	nl(S),
	% Write value.
	write(S, K), write(S, OP), write(S, '"'),
	write_dracut_value2(F, V, S),
	write(S, '"\n'),
	true.

write_dracut_value2(f(SEP, _C), VL, S) :-
	write(S, ' '),
	( list(VL) ->
	  write_atoms(VL, SEP, S)
	; write(S, VL)
	),
	write(S, ' '),
	true.

write_dracut_value2(f(_C), V, S) :-
	( list(V) ->
	  maplist(write(S), V)
	; write(S, V)
	),
	true.

% dracut_info(name, operand, f(sep, comment), comment)
dracut_info(add_dracutmodules, '+=', f(' ', ' <dracut modules> '), 'Specify a space-separated list of dracut modules to call when building the initramfs.').
dracut_info(force_add_dracutmodules, '+=', f(' ', ' <dracut modules> '), 'Force to add a space-separated list of dracut modules to the default set of modules, when host-only mode is specified.').
dracut_info(omit_dracutmodules, '+=', f(' ', ' <dracut modules> '), 'Omit a space-separated list of dracut modules to call when building the initramfs.').
dracut_info(dracutmodules, '+=', f(' ', ' <dracut modules> '), 'Specify a space-separated list of dracut modules to call when building the initramfs.').
dracut_info(add_drivers, '+=', f(' ', ' <kernel modules> '), 'Specify a space-separated list of kernel modules to add to the initramfs.').
dracut_info(force_drivers, '+=', f(' ', ' <list of kernel modules> '), 'See add_drivers. But in this case it is ensured that the drivers are tried to be loaded early via modprobe.').
dracut_info(omit_drivers, '+=', f(' ', ' <kernel modules> '), 'Specify a space-separated list of kernel modules not to add to the initramfs.').
dracut_info(drivers, '+=', f(' ', ' <kernel modules> '), 'Specify a space-separated list of kernel modules to exclusively include in the initramfs.').
dracut_info(filesystems, '+=', f(' ', ' <filesystem names> '), 'Specify a space-separated list of kernel filesystem modules to exclusively include in the generic initramfs.').
dracut_info(drivers_dir, '=', f('<kernel modules directory>'), 'Specify the directory where to look for kernel modules.').
dracut_info(fw_dir, '+=', f(':', ' :<dir>[:<dir> ...] '), 'Specify additional colon-separated list of directories where to look for firmware files.').
dracut_info(libdirs, '+=', f(' ', ' <dir>[ <dir> ...] '), 'Specify a space-separated list of directories where to look for libraries.').
dracut_info(install_items, '+=', f(' ', ' <file>[ <file> ...] '), 'Specify additional files to include in the initramfs, separated by spaces.').
dracut_info(install_optional_items, '+=', f(' ', ' <file>[ <file> ...] '), 'Specify additional files to include in the initramfs, separated by spaces, if they exist.').
dracut_info(compress, '=', f('{cat|bzip2|lzma|xz|gzip|lzop|lz4|zstd|<compressor [args ...]>}'), 'Compress the generated initramfs using the passed compression program.').
dracut_info(squash_compress, '=', f('{<compressor [args ...]>}'), 'Compress the squashfs image using the passed compressor and compressor specific options for mksquashfs.').
dracut_info(do_strip, '=', f('{yes|no}'), 'Strip binaries in the initramfs (default=yes).').
dracut_info(aggressive_strip, '=', f('{yes|no}'), 'Strip more than just debug symbol and sections, for a smaller initramfs build.').
dracut_info(do_hardlink, '=', f('{yes|no}'), 'Hardlink files in the initramfs (default=yes).').
dracut_info(prefix, '=', f('', ' <directory> '), 'Prefix initramfs files with <directory>.').
dracut_info(hostonly, '=', f('{yes|no}'), 'Host-only mode: Install only what is needed for booting the local host instead of a generic host and generate host-specific configuration (default=no).').
dracut_info(hostonly_mode, '=', f('{sloppy|strict}'), 'Specify the host-only mode to use (default=sloppy).').
dracut_info(hostonly_cmdline, '=', f('{yes|no}'), 'If set to "yes", store the kernel command line arguments needed in the initramfs. If hostonly="yes" and this option is not configured, it’s automatically set to "yes".').
dracut_info(hostonly_nics, '+=', f(' ', ' [<nic>[ <nic> ...]] '), 'Only enable listed NICs in the initramfs.').
dracut_info(persistent_policy, '=', f('<policy>'), 'Use <policy> to address disks and partitions.').
dracut_info(tmpdir, '=', f('<temporary directory>'), 'Specify temporary directory to use.').
dracut_info(use_fstab, '=', f('{yes|no}'), 'Use /etc/fstab instead of /proc/self/mountinfo (default=no).').
dracut_info(add_fstab, '+=', f('', ' <filename> '), 'Add entries of <filename> to the initramfs /etc/fstab.').
dracut_info(add_device, '+=', f('', ' <device> '), 'Bring up <device> in initramfs, <device> should be the device name.').
dracut_info(mdadmconf, '=', f('{yes|no}'), 'Include local /etc/mdadm.conf (default=no).').
dracut_info(lvmconf, '=', f(''), '"{yes|no}" Include local /etc/lvm/lvm.conf (default=no).').
dracut_info(fscks, '=', f(' ', ' <fsck tools> '), 'Add a space-separated list of fsck tools.').
dracut_info(nofscks, '=', f('{yes|no}'), 'If specified, inhibit installation of any fsck tools (default=no).').
dracut_info(ro_mnt, '=', f('{yes|no}'), 'Mount / and /usr read-only by default (default=no).').
dracut_info(kernel_cmdline, '=', f('parameters'), 'Specify default kernel command line parameters.').
dracut_info(kernel_only, '=', f('{yes|no}'), 'Only install kernel drivers and firmware files (default=no).').
dracut_info(no_kernel, '=', f('{yes|no}'), 'Do not install kernel drivers and firmware files (default=no).').
dracut_info(acpi_override, '=', f('{yes|no}'), 'Override BIOS provided ACPI tables.').
dracut_info(acpi_table_dir, '=', f('<dir>'), 'Directory to search for ACPI tables if acpi_override= is set to yes.').
dracut_info(early_microcode, '=', f('{yes|no}'), 'Combine early microcode with ramdisk (default=yes).').
dracut_info(stdloglvl, '=', f('{0-6}'), 'Specify logging level for standard error (default=4).').
dracut_info(sysloglvl, '=', f('{0-6}'), 'Specify logging level for syslog (default=0).').
dracut_info(fileloglvl, '=', f('{0-6}'), 'Specify logging level for logfile (default=4).').
dracut_info(logfile, '=', f('<file>'), 'Path to logfile.').
dracut_info(sshkey, '=', f('<file>'), 'SSH key file used with ssh-client module.').
dracut_info(show_modules, '=', f('{yes|no}'), 'Print the name of the included modules to standard output during build (default=no).').
dracut_info(i18n_vars, '=', f('<variable mapping>'), 'Distribution specific variable mapping.').
dracut_info(i18n_default_font, '=', f('<fontname>'), 'The font <fontname> to install, if not specified otherwise. Default is "eurlatgr".').
dracut_info(i18n_install_all, '=', f('{yes|no}'), 'Install everything regardless of generic or host-only mode (default=no).').
dracut_info(reproducible, '=', f('{yes|no}'), 'Create reproducible images (default=no).').
dracut_info(noimageifnotneeded, '=', f('{yes|no}'), 'Do not create an image in host-only mode, if no kernel driver is needed and no /etc/cmdline/*.conf will be generated into the initramfs (default=no).').
dracut_info(loginstall, '=', f('<directory>'), 'Log all files installed from the host to <directory>.').
dracut_info(uefi, '=', f('{yes|no}'), 'Instead of creating an initramfs image, dracut will create an UEFI executable, which can be executed by an UEFI BIOS (default=no).').
dracut_info(machine_id, '=', f('{yes|no}'), 'Affects the default output filename of the UEFI executable, including the <MACHINE_ID> part (default=yes).').
dracut_info(uefi_stub, '=', f('<file>'), 'Specifies the UEFI stub loader, which will load the attached kernel, initramfs and kernel command line and boots the kernel.').
dracut_info(uefi_splash_image, '=', f('<file>'), 'Specifies the UEFI stub loader’s splash image. Requires bitmap (.bmp) image format.').
dracut_info(uefi_secureboot_cert, '=', f('<file>'), 'Specifies a certificate, which are used to sign the created UEFI executable.').
dracut_info(uefi_secureboot_key, '=', f('<file>'), 'Specifies a key, which are used to sign the created UEFI executable.').
dracut_info(uefi_secureboot_engine, '=', f('parameter'), 'Specifies an engine to use when signing the created UEFI executable. E.g. "pkcs11"').
dracut_info(kernel_image, '=', f('<file>'), 'Specifies the kernel image, which to include in the UEFI executable.').
dracut_info(enhanced_cpio, '=', f('{yes|no}'), 'Attempt to use the dracut-cpio binary, which optimizes archive creation for copy-on-write filesystems (default=no).').
dracut_info(parallel, '=', f('{yes|no}'), 'If set to yes, try to execute tasks in parallel (currently only supported for --regenerate-all).').

