######################################################################
# Makefile.PL for Archive::Tar::Merge
# 2007, Mike Schilli <cpan@perlmeister.com>
######################################################################
use ExtUtils::MakeMaker;
WriteMakefile(
    'NAME'         => 'Archive::Tar::Merge',
    'VERSION_FROM' => 'lib/Archive/Tar/Merge.pm', # finds $VERSION
    'PREREQ_PM'    => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM => 'lib/Archive/Tar/Merge.pm',
       AUTHOR     => 'Mike Schilli <cpan@perlmeister.com>') : ()),
);
