% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

% pvs -o pv_name --no-heading
lvm_pvs(L) :-
	os_shell_lines_codes('pvs -o pv_name,vg_name --readonly --noheading --separator "\t" --units b --nosuffix 2>/dev/null', CL),
	% os_shell_lines_codes('pvs --readonly --noheading --separator "\t" --units b --nosuffix 2>/dev/null', CL),
	CL \= [],
	maplist(lvm_pvs_convert_, CL, L),
	true.

lvm_lvs(L) :-
	os_shell_lines_codes('lvs -o vg_name,lv_name,lv_dm_path --noheading --separator "\t" 2>/dev/null', CL),
	CL \= [],
	maplist(lvm_lvs_convert_, CL, L),
	true.

lvm_pvs_convert_(L, pv(PV,VG)) :-
	split_list(L, "\t", SL0),
	SL0 = [[32,32|H]|T],
	SL = [H|T],
	maplist(codes_atom, SL, [PV,VG]),
	true.

lvm_lvs_convert_(L, lv(VG,LV,Path)) :-
	split_list(L, "\t", SL0),
	SL0 = [[32,32|H]|T],
	SL = [H|T],
	maplist(codes_atom, SL, [VG,LV,Path]),
	true.

% PV - physical volume.
lvm_pvcreate_unsafe(PV) :-
	% writenl([lvm_pvcreate, PV]),
	os_shell2([pvcreate, '--yes', PV, '1>/dev/null', '2>/dev/null']),
	% os_shell2([pvcreate, PV, '1>/dev/null']),
	% os_call2([pvcreate, '--yes', PV]),
	true.

lvm_pvremove(PV) :-
	% Remove from a VG.
	os_shell2([pvremove, '-y', PV, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgcreate(VG, PVL) :-
	% os_shell2([vgcreate, VG, PVL]),
	os_shell2([vgcreate, VG, PVL, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgremove('') :- !. % There is no VG.
lvm_vgremove(VG) :-
	os_shell2([vgremove, VG, '1>/dev/null', '2>/dev/null']),
	true.

lvm_vgremove_unsafe('') :- !. % There is no VG.
lvm_vgremove_unsafe(VG) :-
	os_shell2([vgremove, '-y', VG, '1>/dev/null', '2>/dev/null']),
	true.

% Remove PV from VG. Remove VG if there is no more PVs left in it.
% PV - long device name
lvm_pvremove_unsafe(VG, PV) :-
	% list all PVs
	lvm_pvs(L),
	% get all PVs in VG
	findall(PV1, member(pv(PV1,VG), L), PVL),
	lvm_vgremove_unsafe_(VG, PV, PVL),
	true.

% PV - long device name
lvm_vgremove_unsafe_(_VG, _PV, []) :- !. % There are no PVs in VG
lvm_vgremove_unsafe_(VG, PV, [PV]) :- !, % This is the last device in VG.
	% Remove all LV in VG.
	lvm_lvremove_unsafe(VG),
	% Remove whole VG.
	lvm_vgremove(VG).
lvm_vgremove_unsafe_(VG, PV, _) :- !,
	lvm_vgreduce(VG, PV).

lvm_vgreduce('', _PV) :- !.
lvm_vgreduce(VG, PV) :-
	os_call2([vgreduce, VG, PV]),
	true.

% with the rest of capacity
lvm_lvcreate_unsafe(VG, LV) :-
	lvm_lvcreate_unsafe(VG, LV, '100%FREE').

lvm_lvcreate_unsafe(VG, LV, '') :- !,
	lvm_lvcreate_unsafe(VG, LV, '100%FREE').
lvm_lvcreate_unsafe(VG, LV, SZ) :-
	% os_shell2([lvcreate, '--yes', '--extents', SZ, VG, '-n', LV]),
	os_shell2([lvcreate, '--yes', '--extents', SZ, '--name', LV, VG, '1>/dev/null', '2>/dev/null']),
	true.

% Remove all LVs the specified VG.
lvm_lvremove_unsafe('') :- !. % There is no LV.
lvm_lvremove_unsafe(VG) :-
	% os_call2([lvremove, '--yes', VG]),
	os_shell2([lvremove, '--yes', VG, '1>/dev/null', '2>/dev/null']),
	true.

lvm_lvremove_unsafe(VG, LV) :-
	format_to_atom(A, '~w/~w', [VG, LV]),
	% os_call2([lvremove, '--yes', A]),
	os_call2([lvremove, '--yes', A, '1>/dev/null', '2>/dev/null']),
	true.

% Logical Volume Fields
lvm_opt(lv_all                 , 'All fields in this section.').
lvm_opt(lv_uuid                , 'Unique identifier.').
lvm_opt(lv_name                , 'Name.  LVs created for internal use are enclosed in brackets.').
lvm_opt(lv_full_name           , 'Full name of LV including its VG, namely VG/LV.').
lvm_opt(lv_path                , 'Full pathname for LV. Blank for internal LVs.').
lvm_opt(lv_dm_path             , 'Internal device-mapper pathname for LV (in /dev/mapper directory).').
lvm_opt(lv_parent              , 'For LVs that are components of another LV, the parent LV.').
lvm_opt(lv_layout              , 'LV layout.').
lvm_opt(lv_role                , 'LV role.').
lvm_opt(lv_initial_image_sync  , 'Set if mirror/RAID images underwent initial resynchronization.').
lvm_opt(lv_image_synced        , 'Set if mirror/RAID image is synchronized.').
lvm_opt(lv_merging             , 'Set if snapshot LV is being merged to origin.').
lvm_opt(lv_converting          , 'Set if LV is being converted.').
lvm_opt(lv_allocation_policy   , 'LV allocation policy.').
lvm_opt(lv_allocation_locked   , 'Set if LV is locked against allocation changes.').
lvm_opt(lv_fixed_minor         , 'Set if LV has fixed minor number assigned.').
lvm_opt(lv_skip_activation     , 'Set if LV is skipped on activation.').
lvm_opt(lv_when_full           , 'For thin pools, behavior when full.').
lvm_opt(lv_active              , 'Active state of the LV.').
lvm_opt(lv_active_locally      , 'Set if the LV is active locally.').
lvm_opt(lv_active_remotely     , 'Set if the LV is active remotely.').
lvm_opt(lv_active_exclusively  , 'Set if the LV is active exclusively.').
lvm_opt(lv_major               , 'Persistent major number or -1 if not persistent.').
lvm_opt(lv_minor               , 'Persistent minor number or -1 if not persistent.').
lvm_opt(lv_read_ahead          , 'Read ahead setting in current units.').
lvm_opt(lv_size                , 'Size of LV in current units.').
lvm_opt(lv_metadata_size       , 'For thin and cache pools, the size of the LV that holds the metadata.').
lvm_opt(seg_count              , 'Number of segments in LV.').
lvm_opt(origin                 , 'For snapshots and thins, the origin device of this LV.').
lvm_opt(origin_uuid            , 'For snapshots and thins, the UUID of origin device of this LV.').
lvm_opt(origin_size            , 'For snapshots, the size of the origin device of this LV.').
lvm_opt(lv_ancestors           , 'LV ancestors ignoring any stored history of the ancestry chain.').
lvm_opt(lv_full_ancestors      , 'LV ancestors including stored history of the ancestry chain.').
lvm_opt(lv_descendants         , 'LV descendants ignoring any stored history of the ancestry chain.').
lvm_opt(lv_full_descendants    , 'LV descendants including stored history of the ancestry chain.').
lvm_opt(raid_mismatch_count    , 'For RAID, number of mismatches found or repaired.').
lvm_opt(raid_sync_action       , 'For RAID, the current synchronization action being performed.').
lvm_opt(raid_write_behind      , 'For RAID1, the number of outstanding writes allowed to writemostly devices.').
lvm_opt(raid_min_recovery_rate , 'For RAID1, the minimum recovery I/O load in kiB/sec/disk.').
lvm_opt(raid_max_recovery_rate , 'For RAID1, the maximum recovery I/O load in kiB/sec/disk.').
lvm_opt(move_pv                , 'For pvmove, Source PV of temporary LV created by pvmove.').
lvm_opt(move_pv_uuid           , 'For pvmove, the UUID of Source PV of temporary LV created by pvmove.').
lvm_opt(convert_lv             , 'For lvconvert, Name of temporary LV created by lvconvert.').
lvm_opt(convert_lv_uuid        , 'For lvconvert, UUID of temporary LV created by lvconvert.').
lvm_opt(mirror_log             , 'For mirrors, the LV holding the synchronisation log.').
lvm_opt(mirror_log_uuid        , 'For mirrors, the UUID of the LV holding the synchronisation log.').
lvm_opt(data_lv                , 'For thin and cache pools, the LV holding the associated data.').
lvm_opt(data_lv_uuid           , 'For thin and cache pools, the UUID of the LV holding the associated data.').
lvm_opt(metadata_lv            , 'For thin and cache pools, the LV holding the associated metadata.').
lvm_opt(metadata_lv_uuid       , 'For thin and cache pools, the UUID of the LV holding the associated metadata.').
lvm_opt(pool_lv                , 'For thin volumes, the thin pool LV for this volume.').
lvm_opt(pool_lv_uuid           , 'For thin volumes, the UUID of the thin pool LV for this volume.').
lvm_opt(lv_tags                , 'Tags, if any.').
lvm_opt(lv_profile             , 'Configuration profile attached to this LV.').
lvm_opt(lv_lockargs            , 'Lock args of the LV used by lvmlockd.').
lvm_opt(lv_time                , 'Creation time of the LV, if known').
lvm_opt(lv_time_removed        , 'Removal time of the LV, if known').
lvm_opt(lv_host                , 'Creation host of the LV, if known.').
lvm_opt(lv_modules             , 'Kernel device-mapper modules required for this LV.').
lvm_opt(lv_historical          , 'Set if the LV is historical.').
   
% Logical Volume Device Info Fields
lvm_opt(lv_all                 , 'All fields in this section.').
lvm_opt(lv_kernel_major        , 'Currently assigned major number or -1 if LV is not active.').
lvm_opt(lv_kernel_minor        , 'Currently assigned minor number or -1 if LV is not active.').
lvm_opt(lv_kernel_read_ahead   , 'Currently-in-use read ahead setting in current units.').
lvm_opt(lv_permissions         , 'LV permissions.').
lvm_opt(lv_suspended           , 'Set if LV is suspended.').
lvm_opt(lv_live_table          , 'Set if LV has live table present.').
lvm_opt(lv_inactive_table      , 'Set if LV has inactive table present.').
lvm_opt(lv_device_open         , 'Set if LV device is open.').

% Logical Volume Device Status Fields
lvm_opt(lv_all                 , 'All fields in this section.').
lvm_opt(data_percent           , 'For snapshot, cache and thin pools and volumes, the percentage full if LV is active.').
lvm_opt(snap_percent           , 'For snapshots, the percentage full if LV is active.').
lvm_opt(metadata_percent       , 'For cache and thin pools, the percentage of metadata full if LV is active.').
lvm_opt(copy_percent           , 'For Cache, RAID, mirrors and pvmove, current percentage in-sync.').
lvm_opt(sync_percent           , 'For Cache, RAID, mirrors and pvmove, current percentage in-sync.').
lvm_opt(cache_total_blocks     , 'Total cache blocks.').
lvm_opt(cache_used_blocks      , 'Used cache blocks.').
lvm_opt(cache_dirty_blocks     , 'Dirty cache blocks.').
lvm_opt(cache_read_hits        , 'Cache read hits.').
lvm_opt(cache_read_misses      , 'Cache read misses.').
lvm_opt(cache_write_hits       , 'Cache write hits.').
lvm_opt(cache_write_misses     , 'Cache write misses.').
lvm_opt(kernel_cache_settings  , 'Cache settings/parameters as set in kernel, including default values (cached segments only).').
lvm_opt(kernel_cache_policy    , 'Cache policy used in kernel.').
lvm_opt(kernel_metadata_format , 'Cache metadata format used in kernel.').
lvm_opt(lv_health_status       , 'LV health status.').
lvm_opt(kernel_discards        , 'For thin pools, how discards are handled in kernel.').
lvm_opt(lv_check_needed        , 'For thin pools and cache volumes, whether metadata check is needed.').
lvm_opt(lv_merge_failed        , 'Set if snapshot merge failed.').
lvm_opt(lv_snapshot_invalid    , 'Set if snapshot LV is invalid.').

% Logical Volume Device Info and Status Combined Fields
lvm_opt(lv_all                 , 'All fields in this section.').
lvm_opt(lv_attr                , 'Various attributes - see man page.').

% Physical Volume Label Fields
lvm_opt(pv_all                 , 'All fields in this section.').
lvm_opt(pv_fmt                 , 'Type of metadata.').
lvm_opt(pv_uuid                , 'Unique identifier.').
lvm_opt(dev_size               , 'Size of underlying device in current units.').
lvm_opt(pv_name                , 'Name.').
lvm_opt(pv_major               , 'Device major number.').
lvm_opt(pv_minor               , 'Device minor number.').
lvm_opt(pv_mda_free            , 'Free metadata area space on this device in current units.').
lvm_opt(pv_mda_size            , 'Size of smallest metadata area on this device in current units.').
lvm_opt(pv_ext_vsn             , 'PV header extension version.').

% Physical Volume Fields
lvm_opt(pv_all                 , 'All fields in this section.').
lvm_opt(pe_start               , 'Offset to the start of data on the underlying device.').
lvm_opt(pv_size                , 'Size of PV in current units.').
lvm_opt(pv_free                , 'Total amount of unallocated space in current units.').
lvm_opt(pv_used                , 'Total amount of allocated space in current units.').
lvm_opt(pv_attr                , 'Various attributes - see man page.').
lvm_opt(pv_allocatable         , 'Set if this device can be used for allocation.').
lvm_opt(pv_exported            , 'Set if this device is exported.').
lvm_opt(pv_missing             , 'Set if this device is missing in system.').
lvm_opt(pv_pe_count            , 'Total number of Physical Extents.').
lvm_opt(pv_pe_alloc_count      , 'Total number of allocated Physical Extents.').
lvm_opt(pv_tags                , 'Tags, if any.').
lvm_opt(pv_mda_count           , 'Number of metadata areas on this device.').
lvm_opt(pv_mda_used_count      , 'Number of metadata areas in use on this device.').
lvm_opt(pv_ba_start            , 'Offset to the start of PV Bootloader Area on the underlying device in current units.').
lvm_opt(pv_ba_size             , 'Size of PV Bootloader Area in current units.').
lvm_opt(pv_in_use              , 'Set if PV is used.').
lvm_opt(pv_duplicate           , 'Set if PV is an unchosen duplicate.').

% Volume Group Fields
lvm_opt(vg_all                 , 'All fields in this section.').
lvm_opt(vg_fmt                 , 'Type of metadata.').
lvm_opt(vg_uuid                , 'Unique identifier.').
lvm_opt(vg_name                , 'Name.').
lvm_opt(vg_attr                , 'Various attributes - see man page.').
lvm_opt(vg_permissions         , 'VG permissions.').
lvm_opt(vg_extendable          , 'Set if VG is extendable.').
lvm_opt(vg_exported            , 'Set if VG is exported.').
lvm_opt(vg_partial             , 'Set if VG is partial.').
lvm_opt(vg_allocation_policy   , 'VG allocation policy.').
lvm_opt(vg_clustered           , 'Set if VG is clustered.').
lvm_opt(vg_shared              , 'Set if VG is shared.').
lvm_opt(vg_size                , 'Total size of VG in current units.').
lvm_opt(vg_free                , 'Total amount of free space in current units.').
lvm_opt(vg_sysid               , 'System ID of the VG indicating which host owns it.').
lvm_opt(vg_systemid            , 'System ID of the VG indicating which host owns it.').
lvm_opt(vg_lock_type           , 'Lock type of the VG used by lvmlockd.').
lvm_opt(vg_lock_args           , 'Lock args of the VG used by lvmlockd.').
lvm_opt(vg_extent_size         , 'Size of Physical Extents in current units.').
lvm_opt(vg_extent_count        , 'Total number of Physical Extents.').
lvm_opt(vg_free_count          , 'Total number of unallocated Physical Extents.').
lvm_opt(max_lv                 , 'Maximum number of LVs allowed in VG or 0 if unlimited.').
lvm_opt(max_pv                 , 'Maximum number of PVs allowed in VG or 0 if unlimited.').
lvm_opt(pv_count               , 'Number of PVs in VG.').
lvm_opt(vg_missing_pv_count    , 'Number of PVs in VG which are missing.').
lvm_opt(lv_count               , 'Number of LVs.').
lvm_opt(snap_count             , 'Number of snapshots.').
lvm_opt(vg_seqno               , 'Revision number of internal metadata.  Incremented whenever it changes.').
lvm_opt(vg_tags                , 'Tags, if any.').
lvm_opt(vg_profile             , 'Configuration profile attached to this VG.').
lvm_opt(vg_mda_count           , 'Number of metadata areas on this VG.').
lvm_opt(vg_mda_used_count      , 'Number of metadata areas in use on this VG.').
lvm_opt(vg_mda_free            , 'Free metadata area space for this VG in current units.').
lvm_opt(vg_mda_size            , 'Size of smallest metadata area for this VG in current units.').
lvm_opt(vg_mda_copies          , 'Target number of in use metadata areas in the VG.').

% Logical Volume Segment Fields
lvm_opt(seg_all                , 'All fields in this section.').
lvm_opt(segtype                , 'Type of LV segment.').
lvm_opt(stripes                , 'Number of stripes or mirror/raid1 legs.').
lvm_opt(data_stripes           , 'Number of data stripes or mirror/raid1 legs.').
lvm_opt(reshape_len            , 'Size of out-of-place reshape space in current units.').
lvm_opt(reshape_len_le         , 'Size of out-of-place reshape space in logical extents.').
lvm_opt(data_copies            , 'Number of data copies.').
lvm_opt(data_offset            , 'Data offset on each image device.').
lvm_opt(new_data_offset        , 'New data offset after any reshape on each image device.').
lvm_opt(parity_chunks          , 'Number of (rotating) parity chunks.').
lvm_opt(stripe_size            , 'For stripes, amount of data placed on one device before switching to the next.').
lvm_opt(region_size            , 'For mirrors/raids, the unit of data per leg when synchronizing devices.').
lvm_opt(chunk_size             , 'For snapshots, the unit of data used when tracking changes.').
lvm_opt(thin_count             , 'For thin pools, the number of thin volumes in this pool.').
lvm_opt(discards               , 'For thin pools, how discards are handled.').
lvm_opt(cache_metadata_format  , 'For cache, metadata format in use.').
lvm_opt(cache_mode             , 'For cache, how writes are cached.').
lvm_opt(zero                   , 'For thin pools and volumes, if zeroing is enabled.').
lvm_opt(transaction_id         , 'For thin pools, the transaction id and creation transaction id for thins.').
lvm_opt(thin_id                , 'For thin volume, the thin device id.').
lvm_opt(seg_start              , 'Offset within the LV to the start of the segment in current units.').
lvm_opt(seg_start_pe           , 'Offset within the LV to the start of the segment in physical extents.').
lvm_opt(seg_size               , 'Size of segment in current units.').
lvm_opt(seg_size_pe            , 'Size of segment in physical extents.').
lvm_opt(seg_tags               , 'Tags, if any.').
lvm_opt(seg_pe_ranges          , 'Ranges of Physical Extents of underlying devices in command line format (deprecated, use seg_le_ranges for common format).').
lvm_opt(seg_le_ranges          , 'Ranges of Logical Extents of underlying devices in command line format.').
lvm_opt(seg_metadata_le_ranges , 'Ranges of Logical Extents of underlying metadata devices in command line format.').
lvm_opt(devices                , 'Underlying devices used with starting extent numbers.').
lvm_opt(metadata_devices       , 'Underlying metadata devices used with starting extent numbers.').
lvm_opt(seg_monitor            , 'Dmeventd monitoring status of the segment.').
lvm_opt(cache_policy           , 'The cache policy (cached segments only).').
lvm_opt(cache_settings         , 'Cache settings/parameters (cached segments only).').

% Physical Volume Segment Fields
lvm_opt(pvseg_all              , 'All fields in this section.').
lvm_opt(pvseg_start            , 'Physical Extent number of start of segment.').
lvm_opt(pvseg_size             , 'Number of extents in segment.').

% Special Fields
lvm_opt(selected               , 'Set if item passes selection criteria.').

