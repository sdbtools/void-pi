% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023-2024 Sergey Sikorskiy, released under the GNU GPLv2 license.

% https://en.wikipedia.org/wiki/GUID_Partition_Table
% os_gpt_part_type(GUID, type, descr).
%
os_gpt_part_type('00000000-0000-0000-0000-000000000000', sys_unused, 'Unused entry').
os_gpt_part_type('024dee41-33e7-11d3-9d69-0008c781f39f', sys_mbr, 'MBR partition scheme').
os_gpt_part_type('c12a7328-f81f-11d2-ba4b-00a0c93ec93b', sys_efi, 'EFI System partition').
os_gpt_part_type('21686148-6449-6e6f-744e-656564454649', sys_bios_boot, 'BIOS boot partition').
os_gpt_part_type('d3bfe2de-3daf-11df-ba40-e3a556d89593', sys_iffs, 'Intel Fast Flash (iFFS) partition (for Intel Rapid Start technology)').
os_gpt_part_type('f4019732-066e-4e12-8273-346c5641494f', sys_sony_boot, 'Sony boot partition').
os_gpt_part_type('bfbfafe7-a34f-448a-9a5b-6213eb736c22', sys_lenovo_boot, 'Lenovo boot partition').

os_gpt_part_type('e3c9e316-0b5c-4db8-817d-f92df00215ae', win_msr, 'Microsoft Reserved Partition (MSR)').
os_gpt_part_type('ebd0a0a2-b9e5-4433-87c0-68b6b72699c7', win_data, 'Basic data partition').
os_gpt_part_type('5808c8aa-7e8f-42e0-85d2-e1e90434cfb3', win_ldm_metadata, 'Logical Disk Manager (LDM) metadata partition').
os_gpt_part_type('af9b60a0-1431-4f62-bc68-3311714a69ad', win_ldm_data, 'Logical Disk Manager data partition').
os_gpt_part_type('de94bba4-06d1-4d40-a16a-bfd50179d6ac', win_recovery, 'Windows Recovery Environment').
os_gpt_part_type('37affc90-ef7d-4e96-91c3-2d7ae055b174', win_gpfs, 'IBM General Parallel File System (GPFS) partition').
os_gpt_part_type('e75caf8f-f680-4cee-afa3-b001e56efc2d', win_storage_space, 'Storage Spaces partition').
os_gpt_part_type('558d43c5-a1ac-43c0-aac8-d1472b2923d1', win_storage_replica, 'Storage Replica partition').

% HP-UX
os_gpt_part_type('75894c1e-3aeb-11d3-b7c1-7b03a0000000', hp_ux_data, 'Data partition').
os_gpt_part_type('e2a1e728-32e3-11d6-a682-7b03a0000000', hp_ux_service, 'Service partition').

% Linux
os_gpt_part_type('0fc63daf-8483-4772-8e79-3d69d8477de4', linux_data, 'Linux filesystem data').
os_gpt_part_type('a19d880f-05fc-4d3b-a006-743f0f84911e', linux_raid, 'RAID partition').
os_gpt_part_type('44479540-f297-41b2-9af7-d131d5f0458a', linux_root_x86, 'Root partition (x86)').
os_gpt_part_type('4f68bce3-e8cd-4db1-96e7-fbcaf984b709', linux_root_x86_64, 'Root partition (x86-64)').
os_gpt_part_type('69dad710-2ce4-4e3c-b16c-21a1d49abed3', linux_root_arm_32, 'Root partition (32-bit ARM)').
os_gpt_part_type('b921b045-1df0-41c3-af44-4c6f280d3fae', linux_root_arm_64, 'Root partition (64-bit ARM/AArch64)').
os_gpt_part_type('bc13c2ff-59e6-4262-a352-b275fd6f7172', linux_boot, '/boot partition').
os_gpt_part_type('0657fd6d-a4ab-43c4-84e5-0933c84b4f4f', linux_swap, 'Swap partition').
os_gpt_part_type('e6d6d379-f507-44c2-a23c-238f2a3df928', linux_lvm, 'Logical Volume Manager (LVM) partition').
os_gpt_part_type('933ac7e1-2eb4-4f13-b844-0e14e2aef915', linux_home, '/home partition').
os_gpt_part_type('3b8f8425-20e0-4f3b-907f-1a25a76f98e8', linux_srv, '/srv (server data) partition').
os_gpt_part_type('7ffec5c9-2d00-49b7-8941-3ea10a5586b7', linux_dm_crypt, 'Plain dm-crypt partition').
os_gpt_part_type('ca7d7ccb-63ed-4c53-861c-1742536059cc', linux_luks, 'LUKS partition').
os_gpt_part_type('8da63339-0007-60c0-c436-083ac8230908', linux_reserved, 'Reserved').

% GNU/Hurd
os_gpt_part_type('0fc63daf-8483-4772-8e79-3d69d8477de4', gnu_data, 'Linux filesystem data').
os_gpt_part_type('0657fd6d-a4ab-43c4-84e5-0933c84b4f4f', gnu_swap, 'Linux Swap partition').

% FreeBSD
os_gpt_part_type('83bd6b9d-7f41-11dc-be0b-001560b84f0f', freebds_boot, 'Boot partition').
os_gpt_part_type('516e7cb4-6ecf-11d6-8ff8-00022d09712b', freebds_disklabel, 'BSD disklabel partition').
os_gpt_part_type('516e7cb5-6ecf-11d6-8ff8-00022d09712b', freebds_swap, 'Swap partition').
os_gpt_part_type('516e7cb6-6ecf-11d6-8ff8-00022d09712b', freebds_ufs, 'Unix File System (UFS) partition').
os_gpt_part_type('516e7cb8-6ecf-11d6-8ff8-00022d09712b', freebds_vinum, 'Vinum volume manager partition').
os_gpt_part_type('516e7cba-6ecf-11d6-8ff8-00022d09712b', freebds_zfs, 'ZFS partition').
os_gpt_part_type('74ba7dd9-a689-11e1-bd04-00e081286acf', freebds_nandfs, 'nandfs partition').

% MacOS
os_gpt_part_type('48465300-0000-11aa-aa11-00306543ecac', macos_hfs, 'Hierarchical File System Plus (HFS+) partition').
os_gpt_part_type('7c3457ef-0000-11aa-aa11-00306543ecac', macos_apfs, 'Apple APFS container. APFS FileVault volume container').
os_gpt_part_type('55465300-0000-11aa-aa11-00306543ecac', macos_ufs, 'Apple UFS container').
os_gpt_part_type('6a898cc3-1dd2-11b2-99a6-080020736631', macos_zfs, 'ZFS').
os_gpt_part_type('52414944-0000-11aa-aa11-00306543ecac', macos_raid, 'Apple RAID partition').
os_gpt_part_type('52414944-5f4f-11aa-aa11-00306543ecac', macos_raid_offline, 'Apple RAID partition, offline').
os_gpt_part_type('426f6f74-0000-11aa-aa11-00306543ecac', macos_recovery, 'Apple Boot partition (Recovery HD)').
os_gpt_part_type('4c616265-6c00-11aa-aa11-00306543ecac', macos_label, 'Apple Label').
os_gpt_part_type('5265636f-7665-11aa-aa11-00306543ecac', macos_recovery_tv, 'Apple TV Recovery partition').
os_gpt_part_type('53746f72-6167-11aa-aa11-00306543ecac', macos_hfs_filevault, 'Apple Core Storage Container. HFS+ FileVault volume container').
os_gpt_part_type('69646961-6700-11aa-aa11-00306543ecac', macos_apfs_preboot, 'Apple APFS Preboot partition').
os_gpt_part_type('52637672-7900-11aa-aa11-00306543ecac', macos_apfs_recovery, 'Apple APFS Recovery partition').

% Solaris/Illumo
os_gpt_part_type('6a82cb45-1dd2-11b2-99a6-080020736631', solaris_boot, 'Boot partition').
os_gpt_part_type('6a85cf4d-1dd2-11b2-99a6-080020736631', solaris_root, 'Root partition').
os_gpt_part_type('6a87c46f-1dd2-11b2-99a6-080020736631', solaris_swap, 'Swap partition').
os_gpt_part_type('6a8b642b-1dd2-11b2-99a6-080020736631', solaris_backup, 'Backup partition').
os_gpt_part_type('6a898cc3-1dd2-11b2-99a6-080020736631', solaris_usr, '/usr partition').
os_gpt_part_type('6a8ef2e9-1dd2-11b2-99a6-080020736631', solaris_var, '/var partition').
os_gpt_part_type('6a90ba39-1dd2-11b2-99a6-080020736631', solaris_home, '/home partition').
os_gpt_part_type('6a9283a5-1dd2-11b2-99a6-080020736631', solaris_alternate, 'Alternate sector').
os_gpt_part_type('6a945a3b-1dd2-11b2-99a6-080020736631', solaris_reserverd_1, 'Reserved partition').
os_gpt_part_type('6a9630d1-1dd2-11b2-99a6-080020736631', solaris_reserverd_2, 'Reserved partition').
os_gpt_part_type('6a980767-1dd2-11b2-99a6-080020736631', solaris_reserverd_3, 'Reserved partition').
os_gpt_part_type('6a96237f-1dd2-11b2-99a6-080020736631', solaris_reserverd_4, 'Reserved partition').
os_gpt_part_type('6a8d2ac7-1dd2-11b2-99a6-080020736631', solaris_reserverd_5, 'Reserved partition').

% NetBSD
os_gpt_part_type('49f48d32-b10e-11dc-b99b-0019d1879648', netbsd_swap, 'Swap partition').
os_gpt_part_type('49f48d5a-b10e-11dc-b99b-0019d1879648', netbsd_ffs, 'FFS partition').
os_gpt_part_type('49f48d82-b10e-11dc-b99b-0019d1879648', netbsd_lfs, 'LFS partition').
os_gpt_part_type('49f48daa-b10e-11dc-b99b-0019d1879648', netbsd_raid, 'RAID partition').
os_gpt_part_type('2db519c4-b10f-11dc-b99b-0019d1879648', netbsd_concatenated, 'Concatenated partition').
os_gpt_part_type('2db519ec-b10f-11dc-b99b-0019d1879648', netbsd_encrypted, 'Encrypted partition').

% ChromeOS
os_gpt_part_type('fe3a2a5d-4f32-41a7-b725-accc3285a309', chromeos_kernel, 'ChromeOS kernel').
os_gpt_part_type('3cb8e202-3b7e-47dd-8a3c-7ff2a13cfcec', chromeos_rootfs, 'ChromeOS rootfs').
os_gpt_part_type('cab6e88e-abf3-4102-a07a-d4bb9be3c1d3', chromeos_firmware, 'ChromeOS firmware').
os_gpt_part_type('2e0a753d-9e48-43b0-8337-b15192cb1b5e', chromeos_future_use, 'ChromeOS future use').
os_gpt_part_type('09845860-705f-4bb5-b16c-8a8a099caf52', chromeos_minios, 'ChromeOS miniOS').
os_gpt_part_type('3f0f8318-f146-4e6b-8222-c28c8f02e0d5', chromeos_hibernate, 'ChromeOS hibernate').

% CoreOS
os_gpt_part_type('5dfbf5f4-2848-4bac-aa5e-0d9a20b745a6', coreos_usr, '/usr partition (coreos-usr)').
os_gpt_part_type('3884dd41-8582-4404-b9a8-e9b84f2df50e', coreos_resize, 'Resizable rootfs (coreos-resize)').
os_gpt_part_type('c95dc21a-df0e-4340-8d7b-26cbfa9a03e0', coreos_reserved, 'OEM customizations (coreos-reserved)').
os_gpt_part_type('be9067b9-ea49-4f15-b4f6-f36f8c9e1818', coreos_root_raid, 'Root filesystem on RAID (coreos-root-raid)').

% Haiku
os_gpt_part_type('42465331-3ba3-10f1-802a-4861696b7521', haiku_bfs, 'Haiku BFS').

% MidnightBSD
os_gpt_part_type('85d5e45e-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_boot, 'Boot partition').
os_gpt_part_type('85d5e45a-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_data, 'Data partition').
os_gpt_part_type('85d5e45b-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_swap, 'Swap partition').
os_gpt_part_type('0394ef8b-237e-11e1-b4b3-e89a8f7fc3a7', midnightbsd_ufs, 'Unix File System (UFS) partition').
os_gpt_part_type('85d5e45c-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_vinum, 'Vinum volume manager partition').
os_gpt_part_type('85d5e45d-237c-11e1-b4b3-e89a8f7fc3a7', midnightbsd_zfs, 'ZFS partition').

% Ceph
os_gpt_part_type('45b0969e-9b03-4f30-b4c6-b4b80ceff106', ceph_journal, 'Journal').
os_gpt_part_type('45b0969e-9b03-4f30-b4c6-5ec00ceff106', dm_crypt_journal, 'dm-crypt journal').
os_gpt_part_type('4fbd7e29-9d25-41b8-afd0-062c0ceff05d', ceph_osd, 'OSD').
os_gpt_part_type('4fbd7e29-9d25-41b8-afd0-5ec00ceff05d', dm_crypt_osd, 'dm-crypt OSD').
os_gpt_part_type('89c57f98-2fe5-4dc0-89c1-f3ad0ceff2be', ceph_in_creation, 'Disk in creation').
os_gpt_part_type('89c57f98-2fe5-4dc0-89c1-5ec00ceff2be', dm_crypt_in_creation, 'dm-crypt disk in creation').
os_gpt_part_type('cafecafe-9b03-4f30-b4c6-b4b80ceff106', ceph_block, 'Block').
os_gpt_part_type('30cd0809-c2b2-499c-8879-2d6b78529876', ceph_block_db, 'Block DB').
os_gpt_part_type('5ce17fce-4087-4169-b7ff-056cc58473f9', ceph_block_wal, 'Block write-ahead log').
os_gpt_part_type('fb3aabf9-d25f-47cc-bf5e-721d1816496b', dm_crypt_lockbox, 'Lockbox for dm-crypt keys').
os_gpt_part_type('4fbd7e29-8ae0-4982-bf9d-5a8d867af560', ceph_multipath_osd, 'Multipath OSD').
os_gpt_part_type('45b0969e-8ae0-4982-bf9d-5a8d867af560', ceph_multipath_jornal, 'Multipath journal').
os_gpt_part_type('cafecafe-8ae0-4982-bf9d-5a8d867af560', ceph_multipath_block1, 'Multipath block').
os_gpt_part_type('7f4a666a-16f3-47a2-8445-152ef4d03f6c', ceph_multipath_block2, 'Multipath block').
os_gpt_part_type('ec6d6385-e346-45dc-be91-da2a7c8b3261', ceph_multipath_block_db, 'Multipath block DB').
os_gpt_part_type('01b41e1b-002a-453c-9f17-88793989ff8f', ceph_multipath_block_wal, 'Multipath block write-ahead log').
os_gpt_part_type('cafecafe-9b03-4f30-b4c6-5ec00ceff106', dm_crypt_block, 'dm-crypt block').
os_gpt_part_type('93b0052d-02d9-4d8a-a43b-33a3ee4dfbc3', dm_crypt_block_db, 'dm-crypt block DB').
os_gpt_part_type('306e8683-4fe2-4330-b7c0-00a917c16966', dm_crypt_block_wal, 'dm-crypt block write-ahead log').
os_gpt_part_type('45b0969e-9b03-4f30-b4c6-35865ceff106', dm_crypt_luks_journal, 'dm-crypt LUKS journal').
os_gpt_part_type('cafecafe-9b03-4f30-b4c6-35865ceff106', dm_crypt_luks_block, 'dm-crypt LUKS block').
os_gpt_part_type('166418da-c469-4022-adf4-b30afd37f176', dm_crypt_luks_block_db, 'dm-crypt LUKS block DB').
os_gpt_part_type('86a32090-3647-40b9-bbbd-38d8c573aa86', dm_crypt_luks_block_wal, 'dm-crypt LUKS block write-ahead log').
os_gpt_part_type('4fbd7e29-9d25-41b8-afd0-35865ceff05d', dm_crypt_luks_osd, 'dm-crypt LUKS OSD').

% OpenBSD
os_gpt_part_type('824cc7a0-36a8-11e3-890a-952519ad3f61', openbsd_data, 'Data partition').

% QNX
os_gpt_part_type('cef5a9ad-73bc-4601-89f3-cdeeeee321a1', qnx, 'Power-safe (QNX6) file system').

% Plan 9
os_gpt_part_type('c91818f9-8025-47af-89d2-f030d7000c2c', plan9, 'Plan 9 partition').

% VMware ESX
os_gpt_part_type('9d275380-40ad-11db-bf97-000c2911d1b8', vmware_coredump, 'vmkcore (coredump partition)').
os_gpt_part_type('aa31e02a-400f-11db-9590-000c2911d1b8', vmware_fs, 'VMFS filesystem partition').
os_gpt_part_type('9198effc-31c0-11db-8f78-000c2911d1b8', vmware_reserved, 'VMware Reserved').

% Android-IA
os_gpt_part_type('2568845d-2332-4675-bc39-8fa5a4748d15', android_bootloader, 'Bootloader').
os_gpt_part_type('114eaffe-1552-4022-b26e-9b053604cf84', android_bootloader2, 'Bootloader2').
os_gpt_part_type('49a4d17f-93a3-45c1-a0de-f50b2ebe2599', android_boot, 'Boot').
os_gpt_part_type('4177c722-9e92-4aab-8644-43502bfd5506', android_recovery, 'Recovery').
os_gpt_part_type('ef32a33b-a409-486c-9141-9ffb711f6266', android_misc, 'Misc').
os_gpt_part_type('20ac26be-20b7-11e3-84c5-6cfdb94711e9', android_metadata, 'Metadata').
os_gpt_part_type('38f428e6-d326-425d-9140-6e0ea133647c', android_system, 'System').
os_gpt_part_type('a893ef21-e428-470a-9e55-0668fd91a2d9', android_cache, 'Cache').
os_gpt_part_type('dc76dda9-5ac1-491c-af42-a82591580c0d', android_data, 'Data').
os_gpt_part_type('ebc597d0-2053-4b15-8b64-e0aac75f4db1', android_persistent, 'Persistent').
os_gpt_part_type('c5a0aeec-13ea-11e5-a1b1-001e67ca0c3c', android_vendor, 'Vendor').
os_gpt_part_type('bd59408b-4514-490d-bf12-9878d963f378', android_config, 'Config').
os_gpt_part_type('8f68cc74-c5e5-48da-be91-a0c8c15e9c80', android_factory, 'Factory').
os_gpt_part_type('9fdaa6ef-4b3f-40d2-ba8d-bff16bfb887b', android_factory_alt, 'Factory (alt)').
os_gpt_part_type('767941d0-2085-11e3-ad3b-6cfdb94711e9', android_fastboot, 'Fastboot / Tertiary').
os_gpt_part_type('ac6d7924-eb71-4df8-b48d-e267b27148ff', android_oem, 'OEM').

% Android 6.0+ A
os_gpt_part_type('19a710a2-b3ca-11e4-b026-10604b889dcf', android_meta, 'Android Meta').
os_gpt_part_type('193d1ea4-b3ca-11e4-b075-10604b889dcf', android_ext, 'Android EXT').

% Open Network Il environment (onie)
os_gpt_part_type('7412f7d5-a156-4b13-81dc-867174929325', onie_boot, 'Boot').
os_gpt_part_type('d4e6e2cd-4469-46f3-b5cb-1bff57afc149', onie_config, 'Config').

% PowerPC
os_gpt_part_type('9e1a2d38-c612-4316-aa26-8b49521e5a8b', powerpc_boot, 'PReP boot').

% freedesktop.or
os_gpt_part_type('bc13c2ff-59e6-4262-a352-b275fd6f7172', freedesktop, 'Shared boot loader configuration').

% Atari TOS
os_gpt_part_type('734e5afe-f61a-11e6-bc64-92361f002671', atari, 'Basic data partition (GEM, BGM, F32)').

% VeraCrypt
os_gpt_part_type('8c8f8eff-ac95-4770-814a-21994f2dbc8f', veracrypt, 'Encrypted data partition').

% OS/2
os_gpt_part_type('90b6ff38-b98f-4358-a21f-48f35b4a8ad3', os2, 'ArcaOS Type 1').

% Storage Perfor development kit (spdk)
os_gpt_part_type('7c5222bd-8f5d-4087-9c00-bf9843c7b58c', spdk, 'SPDK block device').

% barebox bootlo
os_gpt_part_type('4778ed65-bf42-45fa-9c5b-287a1dc4aab1', barebox, 'barebox-state').

% U-Boot bootloa
os_gpt_part_type('3de21764-95bd-54bd-a5c3-4abe786f38a8', uboot, 'U-Boot environment').

% SoftRAID
os_gpt_part_type('b6fa30da-92d2-4a9a-96f1-871ec6486200', softraid_status, 'SoftRAID_Status').
os_gpt_part_type('2e313465-19b9-463f-8126-8a7993773801', softraid_scratch, 'SoftRAID_Scratch').
os_gpt_part_type('fa709c7e-65b1-4593-bfd5-e71d61de9b02', softraid_volume, 'SoftRAID_Volume').
os_gpt_part_type('bbba6df5-f46f-4a89-8f59-8765b2727503', softraid_cache, 'SoftRAID_Cache').

% Fuchsia standa
os_gpt_part_type('fe8a2634-5e2e-46ba-99e3-3a192091a350', fuchsia_bootloader, 'Bootloader (slot A/B/R)').
os_gpt_part_type('d9fd4535-106c-4cec-8d37-dfc020ca87cb', fuchsia_dm_encrypted, 'Durable mutable encrypted system data').
os_gpt_part_type('a409e16b-78aa-4acc-995c-302352621a41', fuchsia_dm_bootloader, 'Durable mutable bootloader data (including A/B/R metadata)').
os_gpt_part_type('f95d940e-caba-4578-9b93-bb6c90f29d3e', fuchsia_fpr_system, 'Factory-provisioned read-only system data').
os_gpt_part_type('10b8dbaa-d2bf-42a9-98c6-a7c5db3701e7', fuchsia_fpr_bootloader, 'Factory-provisioned read-only bootloader data').
os_gpt_part_type('49fd7cb8-df15-4e73-b9d9-992070127f0f', fuchsia_vm, 'Fuchsia Volume Manager').
os_gpt_part_type('421a8bfc-85d9-4d85-acda-b64eec0133e9', fuchsia_vbm, 'Verified boot metadata (slot A/B/R)').
os_gpt_part_type('9b37fff6-2e58-466a-983a-f7926d0b04e0', fuchsia_zircon_boot, 'Zircon boot image (slot A/B/R)').

% Fuchsia legacy
os_gpt_part_type('c12a7328-f81f-11d2-ba4b-00a0c93ec93b', fuchsia_esp, 'fuchsia-esp').
os_gpt_part_type('606b000b-b7c7-4653-a7d5-b737332c899d', fuchsia_system, 'fuchsia-system').
os_gpt_part_type('08185f0c-892d-428a-a789-dbeec8f55e6a', fuchsia_data, 'fuchsia-data').
os_gpt_part_type('48435546-4953-2041-494e-5354414c4c52', fuchsia_install, 'fuchsia-install').
os_gpt_part_type('2967380e-134c-4cbb-b6da-17e7ce1ca45d', fuchsia_blob, 'fuchsia-blob').
os_gpt_part_type('41d0e340-57e3-954e-8c1e-17ecac44cff5', fuchsia_fvm, 'fuchsia-fvm').
os_gpt_part_type('de30cc86-1f4a-4a31-93c4-66f147d33e05', fuchsia_zircon_boot_a, 'Zircon boot image (slot A)').
os_gpt_part_type('23cc04df-c278-4ce7-8471-897d1a4bcdf7', fuchsia_zircon_boot_b, 'Zircon boot image (slot B)').
os_gpt_part_type('a0e5cf57-2def-46be-a80c-a2067c37cd49', fuchsia_zircon_boot_r, 'Zircon boot image (slot R)').
os_gpt_part_type('4e5e989e-4c86-11e8-a15b-480fcf35f8e6', fuchsia_sys_config, 'sys-config').
os_gpt_part_type('5a3a90be-4c86-11e8-a15b-480fcf35f8e6', fuchsia_factory_config, 'factory-config').
os_gpt_part_type('5ece94fe-4c86-11e8-a15b-480fcf35f8e6', fuchsia_bootloader, 'bootloader').
os_gpt_part_type('8b94d043-30be-4871-9dfa-d69556e8c1f3', fuchsia_guid_test, 'guid-test').
os_gpt_part_type('a13b4d9a-ec5f-11e8-97d8-6c3be52705bf', fuchsia_vbm_a, 'Verified boot metadata (slot A)').
os_gpt_part_type('a288abf2-ec5f-11e8-97d8-6c3be52705bf', fuchsia_vbm_b, 'Verified boot metadata (slot B)').
os_gpt_part_type('6a2460c3-cd11-4e8b-80a8-12cce268ed0a', fuchsia_vbm_r, 'Verified boot metadata (slot R)').
os_gpt_part_type('1d75395d-f2c6-476b-a8b7-45cc1c97b476', fuchsia_misc, 'misc').
os_gpt_part_type('900b0fc5-90cd-4d4f-84f9-9f8ed579db88', fuchsia_emmc_boot1, 'emmc-boot1').
os_gpt_part_type('b2b2e8d1-7c10-4ebc-a2d0-4614568260ad', fuchsia_emmc_boot2, 'emmc-boot2').

os_gpt_part_type(_, unknown, '').

% https://en.wikipedia.org/wiki/Partition_type
% os_gpt_part_type(ID, [t4(type, group, company, descr)]).
os_mbr_part_type('0x00', [t4(unused, free, 'IBM', 'Unused entry')]).
os_mbr_part_type('0x01', [t4(fs_fat12, fs, 'IBM', 'FAT12 as primary partition')]).
os_mbr_part_type('0x02', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x03', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x04', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x05', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x06', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x07', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x08', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x09', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x0a', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x0b', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x0c', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x0e', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x0f', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x11', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x12', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x14', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x15', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x16', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x17', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x18', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x19', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x1b', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x1c', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x1e', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x1f', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x20', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x21', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x22', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x23', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x24', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x27', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x2a', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x2b', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x30', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x31', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x33', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x34', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x35', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x36', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x38', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x39', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x3a', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x3b', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x3c', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x3d', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x40', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x41', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x42', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x43', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x44', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x45', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x46', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x47', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x4a', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x4c', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x4d', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x4e', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x4f', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x50', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x51', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x52', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x53', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x54', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x55', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x56', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x57', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x59', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x5c', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x61', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x63', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x64', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x65', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x66', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x67', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x68', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x69', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x6c', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x70', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x71', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x72', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x73', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x74', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x75', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x76', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x77', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x78', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x79', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x7a', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x7b', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x7c', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x7d', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x7e', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x7f', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x80', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x81', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x82', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x83', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x84', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x85', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x86', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x87', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x88', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x8a', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x8b', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x8c', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x8d', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x8e', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x90', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x91', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x92', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x93', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x94', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x95', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x96', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x97', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x98', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x99', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x9a', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x9b', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0x9e', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0x9f', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa0', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa1', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa2', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa3', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa4', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa5', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa6', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa7', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa8', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xa9', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xaa', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xab', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xac', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xad', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xae', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xaf', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb0', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb1', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb2', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb3', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb4', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xb6', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb7', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xb8', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xbb', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xbc', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xbd', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xbe', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xbf', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc0', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc1', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc2', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc3', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc4', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc5', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc6', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc7', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc8', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xc9', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xca', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xcb', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xcc', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xcd', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xce', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xcf', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xd0', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xd1', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xd4', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xd5', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xd6', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xd8', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xda', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xdb', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xdd', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xde', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xdf', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xe0', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xe1', [t4(unknown, unknown, '', '')]).
% ...
os_mbr_part_type('0xe3', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xe4', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xe5', [t4(unknown, unknown, '', '')]).
os_mbr_part_type('0xe6', [t4(fs_fat16_ro, fs, 'Linux Unified Key Setup', 'Read-only FAT16')]).
% ...
os_mbr_part_type('0xe8', [t4(linux_luks, svc, 'Linux', 'Linux Unified Key Setup')]).
% ...
os_mbr_part_type('0xeb', [t4(beos_fs, fs, 'Be Inc.', 'BFS')]).
os_mbr_part_type('0xec', [t4(skyos_fs, fs, 'Robert Szeleney', 'SkyFS')]).
os_mbr_part_type('0xed', [t4(sprytix_edc_loader, svc, 'Matthias R. Paul', 'EDC loader'), t4(sys_efi, svc_fs, 'Hewlett-Packard', 'Was proposed for GPT hybrid MBR')]).
os_mbr_part_type('0xee', [t4(sys_efi, blocker, 'Microsoft', 'GPT protective MBR')]).
os_mbr_part_type('0xef', [t4(sys_efi, svc_fs, 'Intel', 'EFI System partition')]).
os_mbr_part_type('0xf0', [t4(linux_pa_risc_boot_loader, svc, '', 'Linux')]).
os_mbr_part_type('0xf1', [t4(sys_SpeedStor, fs, 'Storage Dimensions', 'Storage Dimensions')]).
os_mbr_part_type('0xf2', [t4(dos_secondary_part, fs, 'Unisys', 'DOS 3.3+ secondary partition')]).
% ...
os_mbr_part_type('0xf4', [t4(fs_fat16, fs, 'Storage Dimensions', 'FAT16B (corresponds to 0x06')]).
os_mbr_part_type('0xf5', [t4(cont_prologue, cont, '', 'MD0-MD9 multi volume partition for NGF or TwinFS')]).
os_mbr_part_type('0xf6', [t4(fs_fat16_ro, fs, 'Storage Dimensions', 'Read-only FAT16B (corresponds to 0xF4)')]).
os_mbr_part_type('0xf7', [t4(fs_efat, fs, 'Natalia Portillo', ''), t4(fs_x1, fs, 'DDRdrive', 'Solid State file system')]).
os_mbr_part_type('0xf8', [t4(arm_EBBR, svc, 'Arm', 'Protective partition for the area containing system firmware')]).
os_mbr_part_type('0xf9', [t4(linux_cache, cache, 'Linux', 'pCache ext2/ext3 persistent cache')]).
% ...
os_mbr_part_type('0xfb', [t4(vmware_vmfs, fs, 'VMware', 'VMware VMFS file system partition')]).
os_mbr_part_type('0xfc', [t4(vmware_swap, swap, 'VMware', 'VMware swap')]).
os_mbr_part_type('0xfd', [t4(linux_raid, svc, 'Linux', 'Linux RAID superblock with auto-detect')]).
os_mbr_part_type('0xfe', [t4(linux_lvm_old, svc, 'Linux', 'Old Linux LVM'), t4(ibm_ps2_fs, svc_fs, 'IBM', 'PS/2 recovery partition (FAT12 reference disk floppy image)'), t4(ibm_ps2_svc, svc, 'IBM', 'PS/2 IML partition')]).
os_mbr_part_type('0xff', [t4(xenix_bbt, svc, 'Microsoft', 'XENIX bad block table')]).

os_mbr_part_type(_, [t4(unkn4own, unknown, '', '')]).

