###########################################
package Archive::Tar::Merge;
###########################################

use strict;
use warnings;
use Archive::Tar::Wrapper;

our $VERSION = "0.01";

###########################################
sub new {
###########################################
    my($class, %options) = @_;

    my $self = {
        dest_tarball    => undef,
        source_tarballs => [],
        hook            => undef,
        %options,
    };

    bless $self, $class;
}

######################################
sub merge {
######################################
    my($self) = @_; 

    my @merged_files = ();

    DEBUG "Merging ", join(', ', @{$p{source_tarballs}}),
          " into $p{dest_tarball}";

    my $paths     = {};
        # Build the following data structure:
        # rel/path1:
        #   digests => digest1 => abs/path1
        #              digest2 => abs/path2
        #   paths   => [abs/path1, abs/path2]
        # rel/path3:
        #   digests => digest3 => abs/path3
        #   paths   => [abs/path3]
        # ...

    for my $dir (@{$p{source_dirs}}) {
        find(sub {
            return unless -f;
            my $rel = abs2rel($File::Find::name, $dir);

            if(! $p{filter}->($rel, $File::Find::name)) {
                DEBUG "$rel: Blocked by filter";
                return;
            }

            my $digest = md5me($File::Find::name);
    
            if(!exists $paths->{$rel} or
               !exists $paths->{$rel}->{digests}->{$digest}) {
                $paths->{$rel}->{digests}->{$digest} = $File::Find::name;
                push @{$paths->{$rel}->{paths}}, $File::Find::name;
            }
        }, $dir);
    }

        # Traverse and figure out conflicts
    for my $relpath (keys %$paths) {

        my $target_dir = "$p{target_dir}/" . dirname($relpath);
        my @digests    = keys %{$paths->{$relpath}->{digests}};
    
        mkd $target_dir unless -d $target_dir;

            # The *last* src wins by default
        my $src = $paths->{$relpath}->{paths}->[-1];

        my $dst = $target_dir . "/" . basename($src);

        if(@digests == 1) {
            # A unique file. Take it as-is

        } else {
            ERROR "C $relpath";
            for my $digest (@digests) {
                ERROR "  $paths->{$relpath}->{digests}->{$digest}";
            }
            if($p{force}) {
                ERROR "Resolved with force";
            } else {
                my $src =
                  pick("Pick the winner", [
                    @{$paths->{$relpath}->{paths}} ],
                    scalar @{$paths->{$relpath}->{paths}});
            }
        }

        if(-l $src) {
            # It's a symlink!
            DEBUG "Symlinking $src to $dst";
            symlink(readlink($src), $dst) or
                LOGDIE("symlinking $dst failed: $!");
        } else {
            cp($src, $dst);
                # File::Copy doesn't copy permissions correctly, fix that.
            perm_cp($src, $dst);
        }

        push @merged_files, $dst;
    }

    return \@merged_files;
}

######################################
sub md5me {
######################################
    my($filename) = @_; 

    open FH, "<$filename" or LOGDIE "Cannot open $filename";
    local $/ = undef;
    my $data = <FH>;
    my $digest = md5_hex($data);
    close FH;
    return $digest;
}


1;

__END__

=head1 NAME

Archive::Tar::Merge - Merge two or more tarballs into one

=head1 SYNOPSIS

    use Archive::Tar::Merge;

    my $merger = Archive::Tar::Merge->new(
        source_tarballs => ["a.tgz", "b.tgz"],
        dest_tarball    => "c.tgz"
    );

    if(! $merger->merge()) {
        print "Merge failed: ", $merger->error(), "\n";
    }

=head1 DESCRIPTION

C<Archive::Tar::Merge> takes two or more tarballs, merges their files
and writes the resulting directory/file structure out to a destination
tarball.

In the easiest case, there's no overlap between files and the resulting
tarball is just a union of all files in all source tarballs.

If there's overlap, but the overlapping files are identical in the source
and destination tarballs, there's no conflict and C<Archive::Tar::Merge>
just puts the file into the destination tarball without further ado.

=head2 Deciders

If there is a conflict, e.g. two files under the same path in two source
tarballs which have different content or permissions, a decision needs
to be made which of the source files is going to be used in the destination
tarball. The C<decider> parameter takes a reference to a function, which 
receives both the logical path and the physical locations of the two
(or more) conflicting files. Based on this, it decides what to do:

=over 4

=item *

Return the array index of the winning file. If the decider gets a
reference to an array of 2 source tarballs, returning

    return { index => 0 }

will pick the first file while

    return { index => 1 }

will pick the second one.

=item *

Return the content of the winning file. Typically, the decider applies
a filter to one or more of the source files, derives the content of the
destination file and returns its content as a string.

    return { content => "content of destination file" }

=item *

Ignore the file. Leave it out of the destination tarball:

    return { action => "ignore" };

=item *

Throw an error:

    return { action => "error" };

=back

As parameters, the decider receives the file's logical source path in the
tarball and a list of paths to the source file candidates. Here's an
example of a decider that resolves conflicts by prioritizing files from
the second source tarball (C<b.tgz>):

    my $merger = Archive::Tar::Merge->new(
        source_tarballs => ["a.tgz", "b.tgz"],
        dest_tarball    => "c.tgz"
        decider         => \&decider,
    );

      # If there's a conflict, let the source file of the
      # last candidate win.
    sub decider {
        my($logical_src_path, @candidate_phyical_paths) = @_;
          
          # Return the index of the last candidate
        return { index => $candidate_pysical_paths[-1] };
    }

    $merger->merge();

If the decider wants to make a decision based on the content of one or 
more of the source files, it has to open and read them.

=head2 Hooks

Sometimes not all of the source files are supposed to be merged to the
destination. A hook, called with the logical source path and a list of 
source file candidate physical paths, can act as a filter to suppress files,
modify their content, or store them under a different location.

    my $merger = Archive::Tar::Merge->new(
        source_tarballs => ["a.tgz", "b.tgz"],
        dest_tarball    => "c.tgz"
        hook            => \&hook,
    );

      # Keep only "bin/*" files
    sub hook {
        my($logical_src_path, @candidate_paths) = @_;
          
        if($logical_src_path =~ /^bin/) {
            return { action => "keep" };
        }

        return { action => "ignore" };
    }

Just like deciders, filters receive the file's logical source path in the
tarball and a list of paths to the source file candidates.
Filters I<differ> from hooks in that they are called with I<every> file
that is merged and not just with overlapping files.



=head1 TODO

* Copy directories?
* First decider, then hook?

=head1 LEGALESE

Copyright 2007 by Mike Schilli, all rights reserved.
This program is free software, you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

2007, Mike Schilli <cpan@perlmeister.com>