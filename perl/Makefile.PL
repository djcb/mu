# mu - perl interface to mu

use ExtUtils::MakeMaker;
do "./lib/mup.pm";

sub SRC { "lib/@_" }
sub DST { '$(INST_LIBDIR)/'."@_" }
sub PAIR { ( SRC(@_) => DST(@_) ) }

print STDERR "Welcome to mup $mup::VERSION\n\n";
WriteMakefile(
    AUTHOR => 'attila <attila@stalphonsos.com>',
    ABSTRACT => 'perl interface to mu',
    NAME => 'mup',
    VERSION_FROM => 'lib/mup.pm',
    PREREQ_PM => {
	'Data::SExpression' => 0,
	'Moose' => 0,
    },
    PM => {
	PAIR('mup.pm'),
    }
);

sub MY::postamble {
    return <<__MyStuff__;

check:

__MyStuff__
}
