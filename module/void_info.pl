% vi: noexpandtab:tabstop=4:ft=gprolog
% Copyright (c) 2023 Sergey Sikorskiy, released under the GNU GPLv2 license.

version :-
	writenl('version 0.6.3').

source_dep_module('void-live', filesystem(zfs), [zfs, lz4]).
source_dep_module('void-live', filesystem(f2fs), [lz4]).
source_dep_module('void-live', template(_), [gptfdisk]).
source_dep_module('void-live', template(gpt_luks1), [lz4]).
source_dep_module('void-live', template(gpt_luks1_lvm), [lz4]).
source_dep_module('void-live', template(gpt_raid), [mdadm]).
source_dep_module('void-live', inst_method(rootfs), [xz]).

% Directory to store installation key files and detached headers
config_dir(instkey, '/boot').
% Directory to store boot key files and detached headers
config_dir(bootkey, '/root').
% Intended mounting point of EFI System Partition
config_dir(efi, '/boot/efi').

config_file('CRYPTTAB', '/etc/crypttab').
config_file('DHCPCD', '/etc/dhcpcd.conf').
config_file('DNSCRYPT-PROXY', '/etc/dnscrypt-proxy.toml').
config_file('DRACUT', '/etc/dracut.conf.d').
config_file('EFI-STARTUP', F) :-
	config_dir(efi,D),
	format_to_atom(F, '%s/startup.nsh', D).
config_file(fstab, '/etc/fstab').
config_file(grub_default, '/etc/default/grub').
config_file(grub_linux, '/etc/grub.d/10_linux').
config_file(hosts, '/etc/hosts').
config_file(locales, '/etc/default/libc-locales').
config_file(openresolv, '/etc/resolvconf.conf').
config_file(openssh_daemon, '/etc/ssh/sshd_config').
config_file(openssh_moduli, '/etc/ssh/moduli').
config_file(pam, '/etc/pam.d/passwd').
config_file(rc, '/etc/rc.conf').
config_file(securetty, '/etc/securetty').
config_file(sudoers, '/etc/sudoers').
config_file(sysctl, '/etc/sysctl.d/99-sysctl.conf').

% ISO-639 language names for locales
get_lng_name(LC, N) :-
	iso639(LC, N), !.
get_lng_name(LC, LC).

iso639(aa, 'Afar').
iso639(af, 'Afrikaans').
iso639(an, 'Aragonese').
iso639(ar, 'Arabic').
iso639(ast, 'Asturian').
iso639(be, 'Belgian').
iso639(bg, 'Bulgarian').
iso639(bhb, 'Bhili').
iso639(br, 'Breton').
iso639(bs, 'Bosnian').
iso639(ca, 'Catalan').
iso639(cs, 'Czech').
iso639(cy, 'Welsh').
iso639(da, 'Danish').
iso639(de, 'German').
iso639(el, 'Greek').
iso639(en, 'English').
iso639(es, 'Spanish').
iso639(et, 'Estonian').
iso639(eu, 'Basque').
iso639(fi, 'Finnish').
iso639(fo, 'Faroese').
iso639(fr, 'French').
iso639(ga, 'Irish').
iso639(gd, 'Scottish Gaelic').
iso639(gl, 'Galician').
iso639(gv, 'Manx').
iso639(he, 'Hebrew').
iso639(hr, 'Croatian').
iso639(hsb, 'Upper Sorbian').
iso639(hu, 'Hungarian').
iso639(id, 'Indonesian').
iso639(is, 'Icelandic').
iso639(it, 'Italian').
iso639(iw, 'Hebrew').
iso639(ja, 'Japanese').
iso639(ka, 'Georgian').
iso639(kk, 'Kazakh').
iso639(kl, 'Kalaallisut').
iso639(ko, 'Korean').
iso639(ku, 'Kurdish').
iso639(kw, 'Cornish').
iso639(lg, 'Ganda').
iso639(lt, 'Lithuanian').
iso639(lv, 'Latvian').
iso639(mg, 'Malagasy').
iso639(mi, 'Maori').
iso639(mk, 'Macedonian').
iso639(ms, 'Malay').
iso639(mt, 'Maltese').
iso639(nb, 'Norwegian Bokmål').
iso639(nl, 'Dutch').
iso639(nn, 'Norwegian Nynorsk').
iso639(oc, 'Occitan').
iso639(om, 'Oromo').
iso639(pl, 'Polish').
iso639(pt, 'Portugese').
iso639(ro, 'Romanian').
iso639(ru, 'Russian').
iso639(sk, 'Slovak').
iso639(sl, 'Slovenian').
iso639(so, 'Somali').
iso639(sq, 'Albanian').
iso639(st, 'Southern Sotho').
iso639(sv, 'Swedish').
iso639(tcy, 'Tulu').
iso639(tg, 'Tajik').
iso639(th, 'Thai').
iso639(tl, 'Tagalog').
iso639(tr, 'Turkish').
iso639(uk, 'Ukrainian').
iso639(uz, 'Uzbek').
iso639(wa, 'Walloon').
iso639(xh, 'Xhosa').
iso639(yi, 'Yiddish').
iso639(zh, 'Chinese').
iso639(zu, 'Zulu').

% ISO-3166 country codes for locales
get_country_name(LC, C) :-
	iso3166(LC, C), !.
get_country_name(LC, LC).

iso3166('AD', 'Andorra').
iso3166('AE', 'United Arab Emirates').
iso3166('AL', 'Albania').
iso3166('AR', 'Argentina').
iso3166('AT', 'Austria').
iso3166('AU', 'Australia').
iso3166('BA', 'Bonsia and Herzegovina').
iso3166('BE', 'Belgium').
iso3166('BG', 'Bulgaria').
iso3166('BH', 'Bahrain').
iso3166('BO', 'Bolivia').
iso3166('BR', 'Brazil').
iso3166('BW', 'Botswana').
iso3166('BY', 'Belarus').
iso3166('CA', 'Canada').
iso3166('CH', 'Switzerland').
iso3166('CL', 'Chile').
iso3166('CN', 'China').
iso3166('CO', 'Colombia').
iso3166('CR', 'Costa Rica').
iso3166('CY', 'Cyprus').
iso3166('CZ', 'Czech Republic').
iso3166('DE', 'Germany').
iso3166('DJ', 'Djibouti').
iso3166('DK', 'Denmark').
iso3166('DO', 'Dominican Republic').
iso3166('DZ', 'Algeria').
iso3166('EC', 'Ecuador').
iso3166('EE', 'Estonia').
iso3166('EG', 'Egypt').
iso3166('ES', 'Spain').
iso3166('FI', 'Finland').
iso3166('FO', 'Faroe Islands').
iso3166('FR', 'France').
iso3166('GB', 'Great Britain').
iso3166('GE', 'Georgia').
iso3166('GL', 'Greenland').
iso3166('GR', 'Greece').
iso3166('GT', 'Guatemala').
iso3166('HK', 'Hong Kong').
iso3166('HN', 'Honduras').
iso3166('HR', 'Croatia').
iso3166('HU', 'Hungary').
iso3166('ID', 'Indonesia').
iso3166('IE', 'Ireland').
iso3166('IL', 'Israel').
iso3166('IN', 'India').
iso3166('IQ', 'Iraq').
iso3166('IS', 'Iceland').
iso3166('IT', 'Italy').
iso3166('JO', 'Jordan').
iso3166('JP', 'Japan').
iso3166('KE', 'Kenya').
iso3166('KR', 'Korea, Republic of').
iso3166('KW', 'Kuwait').
iso3166('KZ', 'Kazakhstan').
iso3166('LB', 'Lebanon').
iso3166('LT', 'Lithuania').
iso3166('LI', 'Liechtenstein').
iso3166('LU', 'Luxembourg').
iso3166('LV', 'Latvia').
iso3166('LY', 'Libya').
iso3166('MA', 'Morocco').
iso3166('MG', 'Madagascar').
iso3166('MK', 'Macedonia').
iso3166('MT', 'Malta').
iso3166('MX', 'Mexico').
iso3166('MY', 'Malaysia').
iso3166('NI', 'Nicaragua').
iso3166('NL', 'Netherlands').
iso3166('NO', 'Norway').
iso3166('NZ', 'New Zealand').
iso3166('OM', 'Oman').
iso3166('PA', 'Panama').
iso3166('PE', 'Peru').
iso3166('PH', 'Philippines').
iso3166('PL', 'Poland').
iso3166('PR', 'Puerto Rico').
iso3166('PT', 'Portugal').
iso3166('PY', 'Paraguay').
iso3166('QA', 'Qatar').
iso3166('RO', 'Romania').
iso3166('RU', 'Russian Federation').
iso3166('SA', 'Saudi Arabia').
iso3166('SC', 'Seychelles').
iso3166('SD', 'Sudan').
iso3166('SE', 'Sweden').
iso3166('SG', 'Singapore').
iso3166('SI', 'Slovenia').
iso3166('SK', 'Slovakia').
iso3166('SO', 'Somalia').
iso3166('SV', 'El Salvador').
iso3166('SY', 'Syria').
iso3166('TH', 'Thailand').
iso3166('TJ', 'Tajikistan').
iso3166('TN', 'Tunisia').
iso3166('TR', 'Turkey').
iso3166('TW', 'Taiwan').
iso3166('UA', 'Ukraine').
iso3166('UG', 'Uganda').
iso3166('US', 'United States of America').
iso3166('UY', 'Uruguay').
iso3166('UZ', 'Uzbekistan').
iso3166('VE', 'Venezuela').
iso3166('YE', 'Yemen').
iso3166('ZA', 'South Africa').
iso3166('ZW', 'Zimbabwe').

