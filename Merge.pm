###########################################
package Archive::Tar::Merge;
###########################################

use strict;
use warnings;
use Archive::Tar::Wrapper;
use File::Temp qw(tempdir);
use Log::Log4perl qw(:easy);
use File::Spec::Functions qw(abs2rel);
use File::Spec;
use File::Find;
use Digest::MD5;
use File::Basename;
use Sysadm::Install qw(mkd);

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

    if(@{ $self->{source_tarballs} } == 0) {
        LOGDIE "Need at least one tarball to merge";
    }

    for my $tarball (@{ $self->{source_tarballs} }) {
        if(! -f $tarball) {
            LOGDIE "Tarball not found: $tarball";
        }
    }

    bless $self, $class;

    $self->unpack_sources();
    return $self;
}

###########################################
sub unpack_sources {
###########################################
    my($self) = @_; 

    for my $source_tarball (@{ $self->{source_tarballs} }) {
        my($tmpdir) = tempdir(CLEANUP => 1);

        my %source = ();

        $source{dir} = $tmpdir;

        my $arch = Archive::Tar::Wrapper->new(tmpdir => $tmpdir);
        $arch->read($source_tarball);

        $source{archive} = $arch;
        $source{tarball} = $source_tarball;

        push @{ $self->{sources} }, \%source;
    }
}

######################################
sub merge {
######################################
    my($self) = @_; 

    my $out_dir = tempdir(CLEANUP => 1);
    my $out_tar = Archive::Tar::Wrapper->new(tmpdir => $out_dir);

    DEBUG "Merging ", join(', ', @{ $self->{source_tarballs} }),
          " into $self->{dest_tarball}";

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
    for my $source (@{ $self->{sources} }) {
        my $dir = $source->{dir};
        find(sub {
            return unless -f;
            my $rel = abs2rel($File::Find::name, $dir);
              # Two down
            $rel =~ s#.*?/.*?/##;

            # Hook
            if(defined $self->{hook}) {
                if(! $self->{hook}->($File::Find::name, $rel, $source)) {
                    DEBUG "$rel: Blocked by hook";
                    return;
                }
            }

            my $digest = file_hash($File::Find::name);
    
              # avoid autovivification
            if(!exists $paths->{$rel} or
               !exists $paths->{$rel}->{digests}->{$digest}) {
                  # create the data structure shown above
                $paths->{$rel}->{digests}->{$digest} = $File::Find::name;
                push @{$paths->{$rel}->{paths}}, $File::Find::name;
            }
        }, $dir);
    }

        # Traverse and figure out conflicts
    for my $relpath (keys %$paths) {

        my $dst_dir = File::Spec->catfile($out_dir, dirname($relpath));
        my @digests = keys %{$paths->{$relpath}->{digests}};
    
        my $dst_entry = File::Spec->catfile($dst_dir,
                                            basename($relpath));
        my $dst_content;

        mkd $dst_dir unless -d $dst_dir;

        my $src_entry = $paths->{$relpath}->{paths}->[0];

        if(-l $src_entry) {
            DEBUG "Symlinking $src_entry to $dst_entry";
            symlink(readlink($src_entry), $dst_entry) or
                LOGDIE("symlinking $dst_entry failed: $!");
            next;
        }

        if(@digests == 1) {
            # A unique file. Take it as-is

        } else {

              # Several different versions of the file, call the
              # decider to pick one
            if(defined $self->{decider}) {
                my $decision = $self->{decider}->(
                    $relpath, 
                    @{ $paths->{$relpath}->{paths} },
                );

                if(0) {
                } elsif(defined $decision->{action}) {
                    if($decision->{action} eq "ignore") {
                        DEBUG "Ignoring $relpath per decider";
                        next;
                    } else {
                        LOGDIE "Unknown action from decider: ",
                               "$decision->{action}";
                    }
                } elsif(defined $decision->{index}) {
                    $src_entry = 
                        $paths->{$relpath}->{paths}->[ $decision->{index} ];

                } elsif(defined $decision->{content}) {
                    $dst_content = $decision->{content};
                } else {
                    LOGDIE "Decider failed to return decision";
                }
            } else {
                LOGDIE "Conflict: $relpath (and no decider defined)";
            }
        }

        if(defined $dst_content) {
            $out_tar->add($relpath, \$dst_content);
        } else {
            $out_tar->add($relpath, $src_entry);
        }
    }

    my $compress = 0;
    if($self->{dest_tarball} =~ /gz$/) {
        $compress = 1;
    }
    return $out_tar->write($self->{dest_tarball}, $compress);
}

######################################
sub file_hash {
######################################
    my($filename) = @_; 

    open FH, "<$filename" or LOGDIE "Cannot open $filename";

    my $ctx = Digest::MD5->new();
    $ctx->addfile(*FH);
    my $digest = $ctx->hexdigest;

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

In the easiest case, there's no overlap between files. Then
the result is just a union of all files in all source tarballs.

If there's overlap, but the overlapping files are identical in the source
and destination tarballs, there's no conflict and C<Archive::Tar::Merge>
just puts the files into the destination tarball without further ado.

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

Return the content of the winning file. How it comes up with this content
is up to the decider, typically, it applies
a filter to one or more of the source files, derives the content of the
destination file from them and returns its content as a string.

    return { content => "content of destination file" }

=item *

Ignore the file. Leave it out of the destination tarball:

    return { action => "ignore" };

=item *

Throw an error:

    return { action => "error" };

=back

As parameters, the decider receives the file's logical source path in the
tarball and a list of paths to the unpacked source file candidates. Here's an
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
        my($logical_src_path, @candidate_physical_paths) = @_;
          
          # Always return the index of the last candidate
        return { index => $candidate_pysical_paths[-1] };
    }

    $merger->merge();

If a decider wants to make a decision based on the content of one or 
more of the source files, it has to open and read them. A somewhat
contrived example would be a decider which appends the content of all
conflicting source files and adds them under the given logical path
to the destination tarball:

    use File::Slurp;

    sub decider {
        my($logical_src_path, @candidate_physical_paths) = @_;
          
        my $content;

        for my $path (@candidate_physical_paths) {
            $content .= read_file($path);
        }

        return { content => $content };
    }

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
            return { action => "first" };
        }

        return { action => "ignore" };
    }

Just like deciders, hooks receive the file's logical source path in the
tarball and a list of paths to the source file candidates.
Hooks I<differ> from deciders in that they are called with I<every> file
that is merged and not just with conflicting files.

=head1 TODO

* Copy directories?
* First decider, then hook?
* What if an entry is a symlink in one tarball and a file in another?
* Permissions of target files created by content
* N different symlinks (hash dst file)

=head1 LEGALESE

Copyright 2007 by Mike Schilli, all rights reserved.
This program is free software, you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

2007, Mike Schilli <cpan@perlmeister.com>
