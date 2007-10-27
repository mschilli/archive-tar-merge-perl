######################################################################
# Test suite for Archive::Tar::Merge
# by Mike Schilli <cpan@perlmeister.com>
######################################################################

use warnings;
use strict;

use Test::More qw(no_plan);
BEGIN { use_ok('Archive::Tar::Merge') };

ok(1);
like("123", qr/^\d+$/);
