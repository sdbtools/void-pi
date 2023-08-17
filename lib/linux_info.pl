% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

%
part_type_guid('00000000-0000-0000-0000-000000000000', sys_unused, 'Unused entry').
part_type_guid('024dee41-33e7-11d3-9d69-0008c781f39f', sys_mbr, 'MBR partition scheme').
part_type_guid('c12a7328-f81f-11d2-ba4b-00a0c93ec93b', sys_efi, 'EFI System partition').
part_type_guid('21686148-6449-6e6f-744e-656564454649', sys_bios_boot, 'BIOS boot partition').
part_type_guid('d3bfe2de-3daf-11df-ba40-e3a556d89593', sys_iffs, 'Intel Fast Flash (iFFS) partition (for Intel Rapid Start technology)').
part_type_guid('f4019732-066e-4e12-8273-346c5641494f', sys_sony_boot, 'Sony boot partition').
part_type_guid('bfbfafe7-a34f-448a-9a5b-6213eb736c22', sys_lenovo_boot, 'Lenovo boot partition').

% Windows
part_type_guid('e3c9e316-0b5c-4db8-817d-f92df00215ae', win_msr, 'Microsoft Reserved Partition (MSR)').
part_type_guid('ebd0a0a2-b9e5-4433-87c0-68b6b72699c7', win_data, 'Basic data partition').
part_type_guid('5808c8aa-7e8f-42e0-85d2-e1e90434cfb3', win_ldm_metadata, 'Logical Disk Manager (LDM) metadata partition').
part_type_guid('af9b60a0-1431-4f62-bc68-3311714a69ad', win_ldm_data, 'Logical Disk Manager data partition').
part_type_guid('de94bba4-06d1-4d40-a16a-bfd50179d6ac', win_recovery, 'Windows Recovery Environment').
part_type_guid('37affc90-ef7d-4e96-91c3-2d7ae055b174', win_gpfs, 'IBM General Parallel File System (GPFS) partition').
part_type_guid('e75caf8f-f680-4cee-afa3-b001e56efc2d', win_storage_space, 'Storage Spaces partition').
part_type_guid('558d43c5-a1ac-43c0-aac8-d1472b2923d1', win_storage_replica, 'Storage Replica partition').

% HP-UX
part_type_guid('75894c1e-3aeb-11d3-b7c1-7b03a0000000', hp_ux_data, 'Data partition').
part_type_guid('e2a1e728-32e3-11d6-a682-7b03a0000000', hp_ux_service, 'Service partition').

% Linux
part_type_guid('0fc63daf-8483-4772-8e79-3d69d8477de4', linux_data, 'Linux filesystem data').
part_type_guid('a19d880f-05fc-4d3b-a006-743f0f84911e', linux_raid, 'RAID partition').
part_type_guid('44479540-f297-41b2-9af7-d131d5f0458a', linux_root_x86, 'Root partition (x86)').
part_type_guid('4f68bce3-e8cd-4db1-96e7-fbcaf984b709', linux_root_x86_64, 'Root partition (x86-64)').
part_type_guid('69dad710-2ce4-4e3c-b16c-21a1d49abed3', linux_root_arm_32, 'Root partition (32-bit ARM)').
part_type_guid('b921b045-1df0-41c3-af44-4c6f280d3fae', linux_root_arm_64, 'Root partition (64-bit ARM/AArch64)').
part_type_guid('bc13c2ff-59e6-4262-a352-b275fd6f7172', linux_boot, '/boot partition').
part_type_guid('0657fd6d-a4ab-43c4-84e5-0933c84b4f4f', linux_swap, 'Swap partition').
part_type_guid('e6d6d379-f507-44c2-a23c-238f2a3df928', linux_lvm, 'Logical Volume Manager (LVM) partition').
part_type_guid('933ac7e1-2eb4-4f13-b844-0e14e2aef915', linux_home, '/home partition').
part_type_guid('3b8f8425-20e0-4f3b-907f-1a25a76f98e8', linux_srv, '/srv (server data) partition').
part_type_guid('7ffec5c9-2d00-49b7-8941-3ea10a5586b7', linux_dm_crypt, 'Plain dm-crypt partition').
part_type_guid('ca7d7ccb-63ed-4c53-861c-1742536059cc', linux_luks, 'LUKS partition').
part_type_guid('8da63339-0007-60c0-c436-083ac8230908', linux_reserved, 'Reserved').

% GNU/Hurd
part_type_guid('0fc63daf-8483-4772-8e79-3d69d8477de4', gnu_data, 'Linux filesystem data').
part_type_guid('0657fd6d-a4ab-43c4-84e5-0933c84b4f4f', gnu_swap, 'Linux Swap partition').

% FreeBSD
part_type_guid('83bd6b9d-7f41-11dc-be0b-001560b84f0f', freebds_boot, 'Boot partition').
part_type_guid('516e7cb4-6ecf-11d6-8ff8-00022d09712b', freebds_disklabel, 'BSD disklabel partition').
part_type_guid('516e7cb5-6ecf-11d6-8ff8-00022d09712b', freebds_swap, 'Swap partition').
part_type_guid('516e7cb6-6ecf-11d6-8ff8-00022d09712b', freebds_ufs, 'Unix File System (UFS) partition').
part_type_guid('516e7cb8-6ecf-11d6-8ff8-00022d09712b', freebds_vinum, 'Vinum volume manager partition').
part_type_guid('516e7cba-6ecf-11d6-8ff8-00022d09712b', freebds_zfs, 'ZFS partition').
part_type_guid('74ba7dd9-a689-11e1-bd04-00e081286acf', freebds_nandfs, 'nandfs partition').

% MacOS
part_type_guid('48465300-0000-11aa-aa11-00306543ecac', macos_hfs, 'Hierarchical File System Plus (HFS+) partition').
part_type_guid('7c3457ef-0000-11aa-aa11-00306543ecac', macos_apfs, 'Apple APFS container. APFS FileVault volume container').
part_type_guid('55465300-0000-11aa-aa11-00306543ecac', macos_ufs, 'Apple UFS container').
part_type_guid('6a898cc3-1dd2-11b2-99a6-080020736631', macos_zfs, 'ZFS').
part_type_guid('52414944-0000-11aa-aa11-00306543ecac', macos_raid, 'Apple RAID partition').
part_type_guid('52414944-5f4f-11aa-aa11-00306543ecac', macos_raid_offline, 'Apple RAID partition, offline').
part_type_guid('426f6f74-0000-11aa-aa11-00306543ecac', macos_recovery, 'Apple Boot partition (Recovery HD)').
part_type_guid('4c616265-6c00-11aa-aa11-00306543ecac', macos_label, 'Apple Label').
part_type_guid('5265636f-7665-11aa-aa11-00306543ecac', macos_recovery_tv, 'Apple TV Recovery partition').
part_type_guid('53746f72-6167-11aa-aa11-00306543ecac', macos_hfs_filevault, 'Apple Core Storage Container. HFS+ FileVault volume container').
part_type_guid('69646961-6700-11aa-aa11-00306543ecac', macos_apfs_preboot, 'Apple APFS Preboot partition').
part_type_guid('52637672-7900-11aa-aa11-00306543ecac', macos_apfs_recovery, 'Apple APFS Recovery partition').

% Solaris/Illumos
part_type_guid('6a82cb45-1dd2-11b2-99a6-080020736631', solaris_boot, 'Boot partition').
part_type_guid('6a85cf4d-1dd2-11b2-99a6-080020736631', solaris_root, 'Root partition').
part_type_guid('6a87c46f-1dd2-11b2-99a6-080020736631', solaris_swap, 'Swap partition').
part_type_guid('6a8b642b-1dd2-11b2-99a6-080020736631', solaris_backup, 'Backup partition').
part_type_guid('6a898cc3-1dd2-11b2-99a6-080020736631', solaris_usr, '/usr partition').
part_type_guid('6a8ef2e9-1dd2-11b2-99a6-080020736631', solaris_var, '/var partition').
part_type_guid('6a90ba39-1dd2-11b2-99a6-080020736631', solaris_home, '/home partition').
part_type_guid('6a9283a5-1dd2-11b2-99a6-080020736631', solaris_alternate, 'Alternate sector').
part_type_guid('6a945a3b-1dd2-11b2-99a6-080020736631', solaris_reserverd_1, 'Reserved partition').
part_type_guid('6a9630d1-1dd2-11b2-99a6-080020736631', solaris_reserverd_2, 'Reserved partition').
part_type_guid('6a980767-1dd2-11b2-99a6-080020736631', solaris_reserverd_3, 'Reserved partition').
part_type_guid('6a96237f-1dd2-11b2-99a6-080020736631', solaris_reserverd_4, 'Reserved partition').
part_type_guid('6a8d2ac7-1dd2-11b2-99a6-080020736631', solaris_reserverd_5, 'Reserved partition').

% NetBSD
part_type_guid('49f48d32-b10e-11dc-b99b-0019d1879648', netbsd_swap, 'Swap partition').
part_type_guid('49f48d5a-b10e-11dc-b99b-0019d1879648', netbsd_ffs, 'FFS partition').
part_type_guid('49f48d82-b10e-11dc-b99b-0019d1879648', netbsd_lfs, 'LFS partition').
part_type_guid('49f48daa-b10e-11dc-b99b-0019d1879648', netbsd_raid, 'RAID partition').
part_type_guid('2db519c4-b10f-11dc-b99b-0019d1879648', netbsd_concatenated, 'Concatenated partition').
part_type_guid('2db519ec-b10f-11dc-b99b-0019d1879648', netbsd_encrypted, 'Encrypted partition').

% ChromeOS
part_type_guid('fe3a2a5d-4f32-41a7-b725-accc3285a309', chromeos_kernel, 'ChromeOS kernel').
part_type_guid('3cb8e202-3b7e-47dd-8a3c-7ff2a13cfcec', chromeos_rootfs, 'ChromeOS rootfs').
part_type_guid('cab6e88e-abf3-4102-a07a-d4bb9be3c1d3', chromeos_firmware, 'ChromeOS firmware').
part_type_guid('2e0a753d-9e48-43b0-8337-b15192cb1b5e', chromeos_future_use, 'ChromeOS future use').
part_type_guid('09845860-705f-4bb5-b16c-8a8a099caf52', chromeos_minios, 'ChromeOS miniOS').
part_type_guid('3f0f8318-f146-4e6b-8222-c28c8f02e0d5', chromeos_hibernate, 'ChromeOS hibernate').

% CoreOS
part_type_guid('5dfbf5f4-2848-4bac-aa5e-0d9a20b745a6', coreos_usr, '/usr partition (coreos-usr)').
part_type_guid('3884dd41-8582-4404-b9a8-e9b84f2df50e', coreos_resize, 'Resizable rootfs (coreos-resize)').
part_type_guid('c95dc21a-df0e-4340-8d7b-26cbfa9a03e0', coreos_reserved, 'OEM customizations (coreos-reserved)').
part_type_guid('be9067b9-ea49-4f15-b4f6-f36f8c9e1818', coreos_root_raid, 'Root filesystem on RAID (coreos-root-raid)').

% Haiku
part_type_guid('42465331-3ba3-10f1-802a-4861696b7521', haiku_bfs, 'Haiku BFS').

% MidnightBSD
part_type_guid('85d5e45e-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_boot, 'Boot partition').
part_type_guid('85d5e45a-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_data, 'Data partition').
part_type_guid('85d5e45b-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_swap, 'Swap partition').
part_type_guid('0394ef8b-237e-11e1-b4b3-e89a8f7fc3a7', midnightbsd_ufs, 'Unix File System (UFS) partition').
part_type_guid('85d5e45c-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_vinum, 'Vinum volume manager partition').
part_type_guid('85d5e45d-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_zfs, 'ZFS partition').

% Ceph
part_type_guid('45b0969e-9b03-4f30-b4c6-b4b80ceff106', ceph_journal, 'Journal').
part_type_guid('45b0969e-9b03-4f30-b4c6-5ec00ceff106', dm_crypt_journal, 'dm-crypt journal').
part_type_guid('4fbd7e29-9d25-41b8-afd0-062c0ceff05d', ceph_osd, 'OSD').
part_type_guid('4fbd7e29-9d25-41b8-afd0-5ec00ceff05d', dm_crypt_osd, 'dm-crypt OSD').
part_type_guid('89c57f98-2fe5-4dc0-89c1-f3ad0ceff2be', ceph_in_creation, 'Disk in creation').
part_type_guid('89c57f98-2fe5-4dc0-89c1-5ec00ceff2be', dm_crypt_in_creation, 'dm-crypt disk in creation').
part_type_guid('cafecafe-9b03-4f30-b4c6-b4b80ceff106', ceph_block, 'Block').
part_type_guid('30cd0809-c2b2-499c-8879-2d6b78529876', ceph_block_db, 'Block DB').
part_type_guid('5ce17fce-4087-4169-b7ff-056cc58473f9', ceph_block_wal, 'Block write-ahead log').
part_type_guid('fb3aabf9-d25f-47cc-bf5e-721d1816496b', dm_crypt_lockbox, 'Lockbox for dm-crypt keys').
part_type_guid('4fbd7e29-8ae0-4982-bf9d-5a8d867af560', ceph_multipath_osd, 'Multipath OSD').
part_type_guid('45b0969e-8ae0-4982-bf9d-5a8d867af560', ceph_multipath_jornal, 'Multipath journal').
part_type_guid('cafecafe-8ae0-4982-bf9d-5a8d867af560', ceph_multipath_block1, 'Multipath block').
part_type_guid('7f4a666a-16f3-47a2-8445-152ef4d03f6c', ceph_multipath_block2, 'Multipath block').
part_type_guid('ec6d6385-e346-45dc-be91-da2a7c8b3261', ceph_multipath_block_db, 'Multipath block DB').
part_type_guid('01b41e1b-002a-453c-9f17-88793989ff8f', ceph_multipath_block_wal, 'Multipath block write-ahead log').
part_type_guid('cafecafe-9b03-4f30-b4c6-5ec00ceff106', dm_crypt_block, 'dm-crypt block').
part_type_guid('93b0052d-02d9-4d8a-a43b-33a3ee4dfbc3', dm_crypt_block_db, 'dm-crypt block DB').
part_type_guid('306e8683-4fe2-4330-b7c0-00a917c16966', dm_crypt_block_wal, 'dm-crypt block write-ahead log').
part_type_guid('45b0969e-9b03-4f30-b4c6-35865ceff106', dm_crypt_luks_journal, 'dm-crypt LUKS journal').
part_type_guid('cafecafe-9b03-4f30-b4c6-35865ceff106', dm_crypt_luks_block, 'dm-crypt LUKS block').
part_type_guid('166418da-c469-4022-adf4-b30afd37f176', dm_crypt_luks_block_db, 'dm-crypt LUKS block DB').
part_type_guid('86a32090-3647-40b9-bbbd-38d8c573aa86', dm_crypt_luks_block_wal, 'dm-crypt LUKS block write-ahead log').
part_type_guid('4fbd7e29-9d25-41b8-afd0-35865ceff05d', dm_crypt_luks_osd, 'dm-crypt LUKS OSD').

% OpenBSD
part_type_guid('824cc7a0-36a8-11e3-890a-952519ad3f61', openbsd_data, 'Data partition').

% QNX
part_type_guid('cef5a9ad-73bc-4601-89f3-cdeeeee321a1', qnx, 'Power-safe (QNX6) file system').

% Plan 9
part_type_guid('c91818f9-8025-47af-89d2-f030d7000c2c', plan9, 'Plan 9 partition').

% VMware ESX
part_type_guid('9d275380-40ad-11db-bf97-000c2911d1b8', vmware_coredump, 'vmkcore (coredump partition)').
part_type_guid('aa31e02a-400f-11db-9590-000c2911d1b8', vmware_fs, 'VMFS filesystem partition').
part_type_guid('9198effc-31c0-11db-8f78-000c2911d1b8', vmware_reserved, 'VMware Reserved').

% Android-IA
part_type_guid('2568845d-2332-4675-bc39-8fa5a4748d15', android_bootloader, 'Bootloader').
part_type_guid('114eaffe-1552-4022-b26e-9b053604cf84', android_bootloader2, 'Bootloader2').
part_type_guid('49a4d17f-93a3-45c1-a0de-f50b2ebe2599', android_boot, 'Boot').
part_type_guid('4177c722-9e92-4aab-8644-43502bfd5506', android_recovery, 'Recovery').
part_type_guid('ef32a33b-a409-486c-9141-9ffb711f6266', android_misc, 'Misc').
part_type_guid('20ac26be-20b7-11e3-84c5-6cfdb94711e9', android_metadata, 'Metadata').
part_type_guid('38f428e6-d326-425d-9140-6e0ea133647c', android_system, 'System').
part_type_guid('a893ef21-e428-470a-9e55-0668fd91a2d9', android_cache, 'Cache').
part_type_guid('dc76dda9-5ac1-491c-af42-a82591580c0d', android_data, 'Data').
part_type_guid('ebc597d0-2053-4b15-8b64-e0aac75f4db1', android_persistent, 'Persistent').
part_type_guid('c5a0aeec-13ea-11e5-a1b1-001e67ca0c3c', android_vendor, 'Vendor').
part_type_guid('bd59408b-4514-490d-bf12-9878d963f378', android_config, 'Config').
part_type_guid('8f68cc74-c5e5-48da-be91-a0c8c15e9c80', android_factory, 'Factory').
part_type_guid('9fdaa6ef-4b3f-40d2-ba8d-bff16bfb887b', android_factory_alt, 'Factory (alt)').
part_type_guid('767941d0-2085-11e3-ad3b-6cfdb94711e9', android_fastboot, 'Fastboot / Tertiary').
part_type_guid('ac6d7924-eb71-4df8-b48d-e267b27148ff', android_oem, 'OEM').

% Android 6.0+ Arm
part_type_guid('19a710a2-b3ca-11e4-b026-10604b889dcf', android_meta, 'Android Meta').
part_type_guid('193d1ea4-b3ca-11e4-b075-10604b889dcf', android_ext, 'Android EXT').

% Open Network Install environment (onie)
part_type_guid('7412f7d5-a156-4b13-81dc-867174929325', onie_boot, 'Boot').
part_type_guid('d4e6e2cd-4469-46f3-b5cb-1bff57afc149', onie_config, 'Config').

% PowerPC
part_type_guid('9e1a2d38-c612-4316-aa26-8b49521e5a8b', powerpc_boot, 'PReP boot').

% freedesktop.org
part_type_guid('bc13c2ff-59e6-4262-a352-b275fd6f7172', freedesktop, 'Shared boot loader configuration').

% Atari TOS
part_type_guid('734e5afe-f61a-11e6-bc64-92361f002671', atari, 'Basic data partition (GEM, BGM, F32)').

% VeraCrypt
part_type_guid('8c8f8eff-ac95-4770-814a-21994f2dbc8f', veracrypt, 'Encrypted data partition').

% OS/2
part_type_guid('90b6ff38-b98f-4358-a21f-48f35b4a8ad3', os2, 'ArcaOS Type 1').

% Storage Performance development kit (spdk)
part_type_guid('7c5222bd-8f5d-4087-9c00-bf9843c7b58c', spdk, 'SPDK block device').

% barebox bootloader
part_type_guid('4778ed65-bf42-45fa-9c5b-287a1dc4aab1', barebox, 'barebox-state').

% U-Boot bootloader
part_type_guid('3de21764-95bd-54bd-a5c3-4abe786f38a8', uboot, 'U-Boot environment').

% SoftRAID
part_type_guid('b6fa30da-92d2-4a9a-96f1-871ec6486200', softraid_status, 'SoftRAID_Status').
part_type_guid('2e313465-19b9-463f-8126-8a7993773801', softraid_scratch, 'SoftRAID_Scratch').
part_type_guid('fa709c7e-65b1-4593-bfd5-e71d61de9b02', softraid_volume, 'SoftRAID_Volume').
part_type_guid('bbba6df5-f46f-4a89-8f59-8765b2727503', softraid_cache, 'SoftRAID_Cache').

% Fuchsia standard
part_type_guid('fe8a2634-5e2e-46ba-99e3-3a192091a350', fuchsia_bootloader, 'Bootloader (slot A/B/R)').
part_type_guid('d9fd4535-106c-4cec-8d37-dfc020ca87cb', fuchsia_dm_encrypted, 'Durable mutable encrypted system data').
part_type_guid('a409e16b-78aa-4acc-995c-302352621a41', fuchsia_dm_bootloader, 'Durable mutable bootloader data (including A/B/R metadata)').
part_type_guid('f95d940e-caba-4578-9b93-bb6c90f29d3e', fuchsia_fpr_system, 'Factory-provisioned read-only system data').
part_type_guid('10b8dbaa-d2bf-42a9-98c6-a7c5db3701e7', fuchsia_fpr_bootloader, 'Factory-provisioned read-only bootloader data').
part_type_guid('49fd7cb8-df15-4e73-b9d9-992070127f0f', fuchsia_vm, 'Fuchsia Volume Manager').
part_type_guid('421a8bfc-85d9-4d85-acda-b64eec0133e9', fuchsia_vbm, 'Verified boot metadata (slot A/B/R)').
part_type_guid('9b37fff6-2e58-466a-983a-f7926d0b04e0', fuchsia_zircon_boot, 'Zircon boot image (slot A/B/R)').

% Fuchsia legacy
part_type_guid('c12a7328-f81f-11d2-ba4b-00a0c93ec93b', fuchsia_esp, 'fuchsia-esp').
part_type_guid('606b000b-b7c7-4653-a7d5-b737332c899d', fuchsia_system, 'fuchsia-system').
part_type_guid('08185f0c-892d-428a-a789-dbeec8f55e6a', fuchsia_data, 'fuchsia-data').
part_type_guid('48435546-4953-2041-494e-5354414c4c52', fuchsia_install, 'fuchsia-install').
part_type_guid('2967380e-134c-4cbb-b6da-17e7ce1ca45d', fuchsia_blob, 'fuchsia-blob').
part_type_guid('41d0e340-57e3-954e-8c1e-17ecac44cff5', fuchsia_fvm, 'fuchsia-fvm').
part_type_guid('de30cc86-1f4a-4a31-93c4-66f147d33e05', fuchsia_zircon_boot_a, 'Zircon boot image (slot A)').
part_type_guid('23cc04df-c278-4ce7-8471-897d1a4bcdf7', fuchsia_zircon_boot_b, 'Zircon boot image (slot B)').
part_type_guid('a0e5cf57-2def-46be-a80c-a2067c37cd49', fuchsia_zircon_boot_r, 'Zircon boot image (slot R)').
part_type_guid('4e5e989e-4c86-11e8-a15b-480fcf35f8e6', fuchsia_sys_config, 'sys-config').
part_type_guid('5a3a90be-4c86-11e8-a15b-480fcf35f8e6', fuchsia_factory_config, 'factory-config').
part_type_guid('5ece94fe-4c86-11e8-a15b-480fcf35f8e6', fuchsia_bootloader, 'bootloader').
part_type_guid('8b94d043-30be-4871-9dfa-d69556e8c1f3', fuchsia_guid-test, 'guid-test').
part_type_guid('a13b4d9a-ec5f-11e8-97d8-6c3be52705bf', fuchsia_vbm_a, 'Verified boot metadata (slot A)').
part_type_guid('a288abf2-ec5f-11e8-97d8-6c3be52705bf', fuchsia_vbm_b, 'Verified boot metadata (slot B)').
part_type_guid('6a2460c3-cd11-4e8b-80a8-12cce268ed0a', fuchsia_vbm_r, 'Verified boot metadata (slot R)').
part_type_guid('1d75395d-f2c6-476b-a8b7-45cc1c97b476', fuchsia_misc, 'misc').
part_type_guid('900b0fc5-90cd-4d4f-84f9-9f8ed579db88', fuchsia_emmc-boot1, 'emmc-boot1').
part_type_guid('b2b2e8d1-7c10-4ebc-a2d0-4614568260ad', fuchsia_emmc-boot2, 'emmc-boot2').

