% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

st_root_fs(TL, FS) :-
	memberchk(state(root_fs, ctx_rfs(_PTT, FS, _B, _TT)), TL),
	!.
st_root_fs(_TL, ext4).

st_part_tmpl(TL, PTN) :-
	memberchk(state(make_part_tmpl, ctx_part(_PTT, _B, _FS, PTN, _D4L)), TL),
	!.
st_part_tmpl(_TL, root).

st_skip_till([state(C, CTX)|T], C, [state(C, CTX)|T]) :- !.
st_skip_till([_|T], C, L) :- !,
	st_skip_till(T, C, L), !.
st_skip_till([], _C, []).

st_retrieve_ctx(state(bootloader, ctx_bl(B)), L, [bl(B)|L]) :- !.
st_retrieve_ctx(state(template, ctx_tmpl(_B, TT)), L, [tmpl(TT)|L]) :- !.
st_retrieve_ctx(state(used_d7, ctx_used(UL)), L, [used(UL)|L]) :- !.
st_retrieve_ctx(state(bootloader_dev, ctx_bld7(_B, BD, _D7L)), L, [bld7(BD)|L]) :- !.
st_retrieve_ctx(state(root_fs, ctx_rfs(_PTT, FS, _B, _TT)), L, [fs(FS)|L]) :- !.
st_retrieve_ctx(state(make_part_tmpl, ctx_part(_PTT, _B, _FS, PTN, _D4L)), L, [part_tmpl(PTN)|L]) :- !.
st_retrieve_ctx(state(soft, ctx_soft(_FS, _B, SL)), L, [soft(SL)|L]) :- !.
st_retrieve_ctx(_, L, L).

