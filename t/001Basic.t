######################################################################
# Test suite for Archive::Tar::Merge
# by Mike Schilli <cpan@perlmeister.com>
######################################################################
use warnings;
use strict;

use Test::More qw(no_plan);
use Archive::Tar::Merge;
use Archive::Tar::Wrapper;
use File::Temp qw(tempdir);
use Sysadm::Install qw(slurp);

my $tmpdir = tempdir(CLEANUP => 1);
my $dsttar = "$tmpdir/foo.tgz";

my $DATA = "data";
$DATA = "t/data" unless -d $DATA;

  # No conflict
my $merger = Archive::Tar::Merge->new(
    source_tarballs => ["$DATA/tar1.tgz", "$DATA/tar2.tgz"],
    dest_tarball    => $dsttar,
);
$merger->merge();

my $tar = Archive::Tar::Wrapper->new();
$tar->read($dsttar);

my @all_files = @{$tar->list_all()};

is(scalar @all_files, 3, "All files contained in tarball");

is(slurp($tar->locate("tar1/a")), "This is a.\n", "content of a");
is(slurp($tar->locate("tar1/b")), "This is b.\n", "content of b");
is(slurp($tar->locate("tar2/c")), "This is c.\n", "content of c");
