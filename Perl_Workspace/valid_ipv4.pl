use warnings;
use v5.14; # Implicitly uses strict pragma.

use constant OCTET => '\b(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\b';

while( <> )
{
	die if /^\s*$/;
	chomp and say 'IP is ', /^(${\OCTET}\.){3}${\OCTET}$/ ? 'GOOD' : 'BAD';
}
