#!/usr/bin/perl


# Declaration of pragmas
use strict;
use utf8;
use open qw/:std :utf8/;

# Call of external modules
use Encode qw(is_utf8);
use File::Copy;
use Getopt::Long;

# Looking for programme name => usage
my ($programme) = $0 =~ m|^(?:.*/)?(.+)|;
my  $substitut  = " " x length($programme);

my $usage = "Usage : \n" .
            "    $programme  -i input_file -o output_file.(json|xml) -c cluster_number \n" .
            "    $substitut [ -d document_threshold ] [ -t term_threshold ] [ -m metadata ] \n" .
            "    $substitut [ -s script_path ] [ -f frequency ] [ -jk ]\n" .
            "    $programme  -h \n";

my $arguments = join(" ", @ARGV);

my $version     = "1.2.0";
my $dateModif   = "September 26th, 2019";

# Initialization of the globals variables
# needed to read the options
my $help       = undef;
my $input      = undef;
my $json       = undef;
my $keep       = undef;
my $number     = undef;
my $metadata   = undef;
my $output     = undef;
my $scriptPath = undef;
# Variables with predefined values
my $docThreshold = 0.3;
my $kwThreshold  = 1.0;
my $minimum      = 2;

eval        {
        $SIG{__WARN__} = sub {usage(1);};
        GetOptions(
                "clusterNumber=i"  => \$number,
                "docThreshold=f"   => \$docThreshold,
                "frequency=i"      => \$minimum,
                "help"             => \$help,
                "inputFile=s"      => \$input,
                "json"             => \$json,
                "keep"             => \$keep,
                "metadat=s"        => \$metadata,
                "outputFile=s"     => \$output,
                "scriptPath=s"     => \$scriptPath,
                "termThreshold=f"  => \$kwThreshold,
                );
        };
$SIG{__WARN__} = sub {warn $_[0];};

if ( $help ) {
        print "\nProgramme: \n";
        print "   “$programme”, version $version ($dateModif)\n";
        print "    Clustering tool applying the axial k-means algorithm \n";
        print "    followed by PCA on a “document × term” datafile. \n";
        print "    \n";
        print "$usage\n";
        print "Options: \n";
        print "    -c  indicate the number of clusters (from 2 to 200)\n";
        print "    -d  give the threshold value for a document to be included \n";
        print "        in a cluster (default value: 0.3) \n";
        print "    -f  give the minimum frequency (number of documents) of a \n";
        print "        term (default value: 2) \n";
        print "    -h  display this help \n";
        print "    -i  give the name of the input file \n";
        print "    -j  set the output file format to JSON (even if the extension \n";
        print "        of the output file says otherwise) \n";
        print "    -k  keep the k-means result files (deleted by default) \n";
        print "    -m  give the name of the metadata file (in TSV format) \n";
        print "    -o  give the name of the output file with the file extension \n";
        print "        indicating the file format: JSON or XML (XML by default) \n";
        print "    -s  give the path to the directory with the Matlab/Octave \n";
        print "        scripts \n";
        print "    -t  give the threshold value for a term to be included \n";
        print "        in a cluster (default value: 1.0) \n";
        print "        \n";

        exit 0;
        }

# usage(2) if not $number or not $input or not $output;
if ( not $number or not $input or not $output ) {
        usage(2);
        }

if ( $number < 2 or $number > 200 ) {
        print STDERR " \n";
        print STDERR "$programme: the argument of option “-c” must be an integer number ";
        print STDERR "with a value between 2 and 200 \n";
        exit (3);
        }

if ( $minimum < 1 ) {
        print STDERR " \n";
        print STDERR "$programme: the argument of option “-f” must be an integer number ";
        print STDERR "greater than 1 \n";
        exit (3);
        }

# Checking programmes and scripts
my $error = 0;
my $path  = undef;
my @programmes = qw(IndocInitMat IndocKmeansAx octave acc);
foreach my $item (@programmes) {
        my $path = qx(which $item 2> /dev/null);
        if ( $? ) {
                print STDERR "$programme: cannot find programme “$item”. ";
                print STDERR "Make sure it is in one of the directories of “\$\{PATH\}”. \n";
                $error ++;
                }
        }

my @scripts = qw(IndocMatlabAcp IndocVecMat);
if ( not defined $scriptPath ) {
        if ( defined $ENV{'MATLABPATH'} ) {
                $scriptPath = $ENV{'MATLABPATH'};
                }
        else    {
                $scriptPath = '.';
                }
        }
foreach my $item (@scripts) {
        if ( not -f "$scriptPath/$item.m" ) {
                if ( $scriptPath eq '.' ) {
                        print STDERR "$programme: cannot find Matlab/Octave script “$item.m” in current directory. ";
                        }
                else    {
                        print STDERR "$programme: cannot find Matlab/Octave script “$item.m” in directory “$scriptPath”. ";
                        }
                print STDERR "Make sure to indicate the scripts directory with option “-s” or with variable “\$\{MATLABPATH\}”. \n";
                $error ++;
                }
        }

if ( $error ) {
        print STDERR "$programme: please correct the above error";
        print STDERR $error == 1 ? "" : "s";
        print STDERR " before running the programme again. \n";
        exit 4;
        }

# Global variables for data processing
my $acc       = undef;
my $current   = undef;
my $dir       = undef;
my $format    = 'xml';
my $key       = undef;
my $num       = 0;
my @acc       = ();
my %frequency = ();
my %file      = ();
my %hash      = ();
my %key       = ();
my %list      = ();
my %metadata  = ();
my %number    = ();
my %term      = ();
my %tmp       = ();

# Checking output directory
if ( $output =~ m|^(.*)/(.+)| ) {
        my $tmp = $1;
        $dir = $tmp if $tmp !~ m|^\./+\z|;
        }

# If the medata file is present ...
if ( $metadata ) {
        my %content = ();
        open(MTD, "<:utf8", $metadata) or die "$!,";
        my $header = <MTD>;
        chomp($header);
        $header =~ s/\r//go;
        $header =~ s/^\x{FEFF}//o;
        my @fields = split(/\t/, $header);
        for ( my $column = 0 ; $column <= $#fields ; $column ++ ) {
                if ( $fields[$column] =~ /^Filename\z/io ) {
                        $content{$column} = 'Filename';
                        }
                elsif ( $fields[$column] =~ /^Istex\s*Id\z/io ) {
                        $content{$column} = 'IstexId';
                        }
                elsif ( $fields[$column] =~ /^ARK\z/io ) {
                        $content{$column} = 'ARK';
                        }
                elsif ( $fields[$column] =~ /^DOI\z/io ) {
                        $content{$column} = 'DOI';
                        }
                elsif ( $fields[$column] =~ /^Title\z/io ) {
                        $content{$column} = 'Title';
                        }
                elsif ( $fields[$column] =~ /^Source\z/io ) {
                        $content{$column} = 'Source';
                        }
                elsif ( $fields[$column] =~ /^Publication\s*date\z/io ) {
                        $content{$column} = 'Publication date';
                        }
                elsif ( $fields[$column] =~ /^Authors?\z/io ) {
                        $content{$column} = 'Author';
                        }
                }
        while(<MTD>) {
                chomp;
                s/\r//go;
                my $filename = undef;
                my %values = ();
                @fields = split(/\t/);
                for ( my $column = 0 ; $column <= $#fields ; $column ++ ) {
                        next if $fields[$column] =~ /^\s*\z/o;
                        if ( defined $content{$column} ) {
                                if ( $content{$column} eq 'Filename' ) {
                                        $filename = $fields[$column];
                                        }
                                else    {
                                        $values{$content{$column}} = $fields[$column];
                                        if ( $content{$column} eq 'Source' ) {
                                                $number{'Source'}{$fields[$column]} ++;
                                                }
                                        elsif ( $content{$column} eq 'Author' ) {
                                                foreach my $author (split(/ ; /, $fields[$column])) {
                                                        $number{'Author'}{$author} ++;
                                                        }
                                                }
                                        }
                                }
                        }
                if ( defined $filename ) {
                        $metadata{$filename} = \%values;
                        }
                }
        close MTD;
        }

# Output format 
if ( $json or $output =~ /\.json\z/io ) {
        $format = 'json';
        }

# Signal handling
$SIG{'HUP'} = 'clean';
$SIG{'INT'} = 'clean';
$SIG{'TERM'} = 'clean';

# Stem of temporary filenames
my $tag = "ndoc$$";
if ( $tag eq 'ndoc1' ) {
        my $tmp = int(rand 9990) + 10;
        $tag = "ndoc$tmp";
        }

# Reading input file, first round
open(INP, "<:utf8", $input) or die "$!,";
while(<INP>) {
        chomp;
        s/\r//o;        # just in case ...
        my ($file, $term) = split(/\t/);
        if ( defined $current and $current ne $file ) {
                my @tmp = sort keys %tmp;
                foreach my $item (@tmp) {
                        $frequency{$item} ++;
                        }
                $hash{$current} = \@tmp;
                %tmp = ();
                }
        $current = $file;
        $tmp{$term} ++;
        }
close INP;

if ( defined $current ) {
        my @tmp = sort keys %tmp;
        foreach my $item (@tmp) {
                $frequency{$item} ++;
                }
        $hash{$current} = \@tmp;
        %tmp = ();
        }

# Creating file index
my @files = sort keys %hash;
my $nbFiles = $#files + 1;
foreach my $file (@files) {
        my $key = sprintf("%06d", $num);
        # print OUT "$key\t$file\n";
        $key{'file'}{$file} = $key;
        $file{$key} = $file;
        $num ++;
        }

# Creating term index
$num = 0;
my @terms = sort grep {$frequency{$_} >= $minimum} keys %frequency;
my $nbTerms = $#terms + 1;
foreach my $term (@terms) {
        my $key = sprintf("%06d", $num);
        $key{'term'}{$term} = $key;
        $term{$key} = $term;
        $num ++;
        }

# Checking document frequency
foreach my $file (keys %hash) {
        my @tmp = grep {defined $key{'term'}{$_}} @{$hash{$file}};
        if (@tmp) {
                foreach my $term (@tmp) {
                        push @{$list{$key{'term'}{$term}}}, $key{'file'}{$file};
                        }
                $hash{$file} = \@tmp;
                }
        else    {
                $hash{$file} = undef;
                $nbFiles --;
                }
        }

# Resetting variables
$key     = undef;
$current = undef;
%tmp     = ();

# Creating K-means input file
open(KMN, ">:raw", $tag."InputFile.txt") or die "$!,";
printf KMN "%07d%07d\n", $nbTerms, $nbFiles;

# Reading input file, second round
open(INP, "<:utf8", $input) or die "$!,";
while(<INP>) {
        chomp;
        s/\r//o;        # just in case ...
        my ($file, $term) = split(/\t/);
        if ( $key{'file'}{$file} ) {
                $key = $key{'file'}{$file};
                }
        else    {
                print STDERR "Error for file \"$file\"\n";
                clean(6);
                }
        if ( $key ne $current ) {
                if ( defined $current ) {
                        my @tmp = sort keys %tmp;
                        if ( @tmp ) {
                                printf KMN "%s     %07d\n", $current, $#tmp + 1;
                                foreach my $item (@tmp) {
                                        printf KMN "%s\n", $item;
                                        }
                                }
                        }
                %tmp = ();
                }
        $current = $key;
        if ( $key{'term'}{$term} ) {
                $tmp{$key{'term'}{$term}} ++;
                }
        }
close INP;

# Let's not forget the last file content
if ( defined $current ) {
        my @tmp = sort keys %tmp;
        printf KMN "%s     %07d\n", $current, $#tmp + 1;
        foreach my $item (@tmp) {
                printf KMN "%s\n", $item;
                }
        }
close KMN;

# Axial K-maeans: matrix initialization
my $code = system "IndocInitMat -d ${tag}InputFile.txt -i ${tag}Init.txt -n $number -h -g 1";
if ( $code ) {
        if ( $code == -1 ) {
                print STDERR "$programme: cannot find programme “IndocInitMat”. \n";
                print STDERR "Make sure it is in one of the directories of “\$\{PATH\}”. \n";
                clean(4);
                }
        my $return = $code >> 8;
        my $signal = $code & 0X7F;
        my $dump   = $code & 0X80;
        print STDERR "$programme: programme “IndocInitMat” exited with code $return";
        if ( $signal ) {
                print STDERR " from signal $signal";
                }
        if ( $dump ) {
                print STDERR " (core dumped)";
                }
        print ". \n";
        clean(4);
        }

# Axial K-means: computation
$code = system "IndocKmeansAx -a 1 -d ${tag}InputFile.txt -i ${tag}Init.txt -op ./ -os $tag -sd $docThreshold -sk $kwThreshold";
if ( $code ) {
        if ( $code == -1 ) {
                print STDERR "$programme: cannot find programme “IndocKmeansAx”. \n";
                print STDERR "Make sure it is in one of the directories of “\$\{PATH\}”. \n";
                clean(4);
                }
        my $return = $code >> 8;
        my $signal = $code & 0X7F;
        my $dump   = $code & 0X80;
        print STDERR "$programme: programme “IndocKmeansAx” exited with code $return";
        if ( $signal ) {
                print STDERR " from signal $signal";
                }
        if ( $dump ) {
                print STDERR " (core dumped)";
                }
        print ". \n";
        clean(4);
        }

# Building results 
my %cluster = ();
if ( -f "des.$tag" ) {
        open(DES, "<:raw", "des.$tag") or die "$!,";
        while(<DES>) {
                chomp;
                s/\r//o;        # just in case ...
                my ($cluster, $value) = split(/\t/);
                my ($weight, $term) = split(/:\s*/, $value);
                $cluster = sprintf("%06d", $cluster);
                $term    = sprintf("%06d", $term);
                $weight  =~ s/^\s+//o;
                my $record = {};
                $record->{'term'} = $term;
                $record->{'weight'} = $weight;
                push(@{$cluster{$cluster}{'terms'}}, $record);
                }
        close DES;
        # unlink "des.$tag";
        # Sorting terms in each cluster
        foreach my $cluster (sort {$a <=> $b} keys %cluster) {
                my @tmp = sort {$b->{'weight'} <=> $a-> {'weight'} or 
                                $a->{'term'} cmp $b->{'term'}} @{$cluster{$cluster}{'terms'}};
                @{$cluster{$cluster}{'terms'}} = @tmp;
                }
        }
else    {
        print STDERR "$programme: cannot find expected result file “des.$tag”\n";
        clean(5);
        }

if ( -f "doc.$tag" ) {
        open(DOC, "<:raw", "doc.$tag") or die "$!,";
        while(<DOC>) {
                chomp;
                s/\r//o;        # just in case ...
                my ($cluster, $value) = split(/\t/);
                my ($weight, $doc) = split(/:\s*/, $value);
                $cluster = sprintf("%06d", $cluster);
                $doc     = sprintf("%06d", $doc);
                $weight  =~ s/^\s+//o;
                 my $record = {};
                $record->{'doc'} = $doc;
                $record->{'weight'} = $weight;
                push(@{$cluster{$cluster}{'docs'}}, $record);
                }
        close DOC;
        #unlink "doc.$tag";
        # Sorting docs in each cluster
        foreach my $cluster (sort {$a <=> $b} keys %cluster) {
                my @tmp = sort {$b->{'weight'} <=> $a-> {'weight'} or 
                                $a->{'doc'} cmp $b->{'doc'}} @{$cluster{$cluster}{'docs'}};
                @{$cluster{$cluster}{'docs'}} = @tmp;
                }
        }
else    {
        print STDERR "$programme: cannot find expected result file “doc.$tag”\n";
        clean(5);
        }

if ( -f "iner.$tag" ) {
        open(INR, "<:raw", "iner.$tag") or die "$!,";
        while(<INR>) {
                chomp;
                s/\r//o;        # just in case ...
                my ($cluster, $inertia) = split(/\t\s*/);
                $cluster = sprintf("%06d", $cluster);
                $cluster{$cluster}{'inertia'} = $inertia;
                }
        close INR;
        #unlink "iner.$tag";
        }
else    {
        print STDERR "$programme: cannot find expected result file “iner.$tag”\n";
        clean(5);
        }

if ( -f "card.$tag" ) {
        open(INR, "<:raw", "card.$tag") or die "$!,";
        while(<INR>) {
                chomp;
                s/\r//o;        # just in case ...
                my ($cluster, $cardinal) = split(/\t\s*/);
                $cluster = sprintf("%06d", $cluster);
                $cluster{$cluster}{'cardinal'} = $cardinal;
                }
        close INR;
        #unlink "card.$tag";
        }
else    {
        print STDERR "$programme: cannot find expected result file “card.$tag”\n";
        clean(5);
        }

if ( -f "entete.$tag" ) {
        open(ENT, "<:raw", "entete.$tag") or die "$!,";
        chomp(my $nbClusters = <ENT>);
        chomp($nbTerms = <ENT>);
        close ENT;
        # print STDERR "Nb clusters : \"$nbClusters\"\n";
        # print STDERR "Nb termes   : \"$nbTerms\"\n";
        if ( -f "pds.$tag" ) {
                open(ACC, "acc -c $nbClusters -k $nbTerms -f pds.$tag |") or die "$!,";
                @acc = <ACC>;
                close ACC;
                my $code = system "cat entete.$tag pds.$tag > ${tag}Data.txt";
                if ( $code ) {
                        print STDERR "$programme: cannot create input file for Octave script\n";
                        clean(6);
                        }
                # unlink "pds.$tag";
                }
        else    {
                print STDERR "$programme: cannot find expected result file “pds.$tag”\n";
                clean(5);
                }
        # unlink "entete.$tag";
        }
else    {
        print STDERR "$programme: cannot find expected result file “entete.$tag”\n";
        clean(5);
        }

# Principal Components Analysis (using Octave software)
open(OCT, "| octave --quiet --no-history > /dev/null") or die "$!,";
print OCT "addpath $scriptPath\n";
print OCT "load ${tag}Data.txt;\n";
print OCT "[x,valP]=IndocMatlabAcp(${tag}Data);\n";
print OCT "save ${tag}Data.map.txt x;\n";
print OCT "save ${tag}Data.stat.txt valP;\n";
close OCT;

if ( -f "${tag}Data.map.txt" ) {
        open(MAP, "<:raw", "${tag}Data.map.txt") or die "$!,";
        $num = "000000";
        while(<MAP>) {
                next if /^\s*\#/o;
                next if /^\s*$/o;
                chomp;
                s/\r//o;        # just in case ...
                s/^\s*//o;
                my ($x, $y) = split(/\s+/);
                $cluster{$num}{'geo'} = sprintf "%.3f,%.3f", $x, $y;
                $num ++;
                }
        close MAP;
        # unlink "${tag}Data.map.txt";
        }

# Writing output file
open(OUT, ">:utf8", $output) or die "$!,";

if ( $format eq 'json' ) {
        # First, the XML declaration and the root start-tag
        print OUT "{\n";
        my $nbClusters = keys %cluster;
        print OUT "  \"Total\": $nbClusters,\n";
        print OUT "  \"Clusters\": \[\n";

        # then, the clusters
        foreach my $cluster (sort {$a <=> $b} keys %cluster) {
                $nbClusters --;
                print OUT "    \{\n";
                printf OUT "      \"Id\": \"%06d\",\n", $cluster;
                # List of terms and list of documents
                # my @terms = @{$cluster{$cluster}{'terms'}};
                my @terms = grep {$#{$list{$_->{'term'}}} > 0} @{$cluster{$cluster}{'terms'}};
                my @docs = @{$cluster{$cluster}{'docs'}};
                %tmp = map {$_->{'doc'} => 1} @docs;
                # Looking for the most important term
                my $record = $terms[0];
                print OUT "      \"Name\": \"$term{$record->{'term'}}\",\n";
                print OUT "      \"Coordinates\": \"$cluster{$cluster}{'geo'}\",\n";
                print OUT "      \"Cardinal\": $cluster{$cluster}{'cardinal'},\n";
                print OUT "      \"Inertia\": $cluster{$cluster}{'inertia'},\n";
                # Processing the terms ...
                $nbTerms = $#terms + 1;
                print OUT "      \"Number of terms\": $nbTerms,\n";
                print OUT "      \"Terms\": \[\n";
                foreach $record (@terms) {
                        $nbTerms --;
                        print OUT "        \{\n";
                        print OUT "          \"Term\": \"$term{$record->{'term'}}\",\n";
                        print OUT "          \"Reference\": \"$record->{'term'}\",\n";
                        print OUT "          \"Weight\": $record->{'weight'},\n";
                        my $total = $#{$list{$record->{'term'}}} + 1;
                        my @list = sort grep {$tmp{$_}} @{$list{$record->{'term'}}};
                        my $nb = $#list + 1;
                        print OUT "          \"Frequency\": \"$nb/$total\",\n";
                        print OUT "          \"List\": \"", join(",", @list), "\"\n";
                        print OUT "        \}", $nbTerms ? "," : "", "\n";
                        }
                print OUT "      \],\n";
                # ... then the documents
                my $nbDocs = $#docs + 1;
                print OUT "      \"Number of documents\": $nbDocs,\n";
                print OUT "      \"Documents\": \[\n";
                # Collecting data on journals
                my %author = ();
                my %source = ();
                foreach $record (@docs) {
                        $nbDocs --;
                        my $filename = $file{$record->{'doc'}};
                        if ( defined $metadata{$filename}{'Author'} ) {
                                foreach my $author (split(/ ; /, $metadata{$filename}{'Author'})) {
                                        push @{$author{$author}}, $record->{'doc'};
                                        }
                                }
                        if ( defined $metadata{$filename}{'Source'} ) {
                                push @{$source{$metadata{$filename}{'Source'}}}, $record->{'doc'};
                                }
                        print OUT "        \{\n";
                        print OUT "          \"Filename\": \"$filename\",\n";
                        if ( defined $metadata{$filename} ) {
                                if ( defined $metadata{$filename}{'Title'} ) {
                                        my $title = $metadata{$filename}{'Title'};
                                        print OUT "          \"Title\": \"$title\",\n";
                                        }
                                if ( defined $metadata{$filename}{'Author'} ) {
                                        my $author = $metadata{$filename}{'Author'};
                                        print OUT "          \"Author\": \"$author\",\n";
                                        }
                                if ( defined $metadata{$filename}{'Source'} ) {
                                        my $source = $metadata{$filename}{'Source'};
                                        print OUT "          \"Source\": \"$source\",\n";
                                        }
                                if ( defined $metadata{$filename}{'Publication date'} ) {
                                        my $date = $metadata{$filename}{'Publication date'};
                                        print OUT "          \"Publication date\": \"$date\",\n";
                                        }
                                if ( defined $metadata{$filename}{'IstexId'} ) {
                                        my $istexId = $metadata{$filename}{'IstexId'};
                                        print OUT "          \"Istex Id\": \"$istexId\",\n";
                                        }
                                if ( defined $metadata{$filename}{'ARK'} ) {
                                        my $ark = $metadata{$filename}{'ARK'};
                                        print OUT "          \"ARK\": \"$ark\",\n";
                                        }
                                if ( defined $metadata{$filename}{'DOI'} ) {
                                        my $doi = $metadata{$filename}{'DOI'};
                                        print OUT "          \"DOI\": \"$doi\",\n";
                                        }
                                }
                        print OUT "          \"Reference\": \"$record->{'doc'}\",\n";
                        print OUT "          \"Weight\": $record->{'weight'}\n";
                        print OUT "        \}", $nbDocs ? "," : "", "\n";
                        }
                if ( %source ) {
                        %tmp = ();
                        my $nbSources = 0;
                        foreach my $source (keys %source) {
                                $nbSources ++;
                                $tmp{$source} = $#{$source{$source}} + 1;
                                }
                        print OUT "      \],\n";
                        print OUT "      \"Number of sources\": $nbSources,\n";
                        print OUT "      \"Sources\": \[\n";
                        foreach my $source (sort {$tmp{$b} <=> $tmp{$a} or $a cmp $b} keys %tmp) {
                                $nbSources --;
                                my @tmp = sort @{$source{$source}};
                                print OUT "        \{\n";
                                print OUT "          \"Title\": \"$source\",\n";
                                print OUT "          \"Frequency\": \"$tmp{$source}/$number{'Source'}{$source}\",\n";
                                print OUT "          \"List\": \"", join(",", @tmp), "\"\n";
                                print OUT "        \}", $nbSources ? "," : "", "\n";
                                }
                        }
                if ( %author ) {
                        %tmp = ();
                        my $nbAuthors = 0;
                        foreach my $author (keys %author) {
                                $nbAuthors ++;
                                $tmp{$author} = $#{$author{$author}} + 1;
                                }
                        print OUT "      \],\n";
                        print OUT "      \"Number of authors\": $nbAuthors,\n";
                        print OUT "      \"Authors\": \[\n";
                        foreach my $author (sort {$tmp{$b} <=> $tmp{$a} or $a cmp $b} keys %tmp) {
                                $nbAuthors --;
                                my @tmp = sort @{$author{$author}};
                                print OUT "        \{\n";
                                print OUT "          \"Name\": \"$author\",\n";
                                print OUT "          \"Frequency\": \"$tmp{$author}/$number{'Author'}{$author}\",\n";
                                print OUT "          \"List\": \"", join(",", @tmp), "\"\n";
                                print OUT "        \}", $nbAuthors ? "," : "", "\n";
                                }
                        }
                print OUT "      \]\n";
                print OUT "    \}", $nbClusters ? "," : "", "\n";
                }

        # ACC
        if ( @acc ) {
                print OUT "  \],\n";
                chomp(@acc);
                my @levels = grep(/^\d+/, @acc);
                print OUT "  \"Links\": \[\n";
                while (my $level = shift @levels) {
                        chomp($level);
                        print OUT "    \[\n";
                        my @links = split(/:/, $level);
                        while (my $link = shift @links) {
                                my ($c1, $c2, $strength) = split(/,/, $link);
                                print OUT "      \{\n";
                                print OUT "        \"Link\": \"$c2 - $c1\",\n";
                                print OUT "        \"Strength\": $strength\n";
                                print OUT "      \}", @links ? "," : "", "\n";
                                }
                        print OUT "    \]", @levels ? "," : "", "\n";
                        }
                }

        # Finally, root end tag
        print OUT "  \]\n";
        print OUT "\}\n";

        close OUT;
        }
elsif ( $format eq 'xml' ) {
        # First, the XML declaration and the root start-tag
        print OUT "<?xml version=\"1.0\"?>\n";
        print OUT "<clusters>\n";
        
        # then, the clusters
        foreach my $cluster (sort {$a <=> $b} keys %cluster) {
                print OUT "    <cluster>\n";
                printf OUT "        <id>%06d</id>\n", $cluster;
                # List of terms and list of documents
                my @terms = grep {$#{$list{$_->{'term'}}} > 0} @{$cluster{$cluster}{'terms'}};
                my @docs = @{$cluster{$cluster}{'docs'}};
                %tmp = map {$_->{'doc'} => 1} @docs;
                # Looking for the most important term 038079
                my $record = $terms[0];
                print OUT "        <name>$term{$record->{'term'}}</name>\n";
                print OUT "        <geo>$cluster{$cluster}{'geo'}</geo>\n";
                print OUT "        <cardinal>$cluster{$cluster}{'cardinal'}</cardinal>\n";
                print OUT "        <inertia>$cluster{$cluster}{'inertia'}</inertia>\n";
                # list of terms
                print OUT "        <terms>\n";
                printf OUT "            <number>%d</number>\n", $#terms + 1;
                foreach $record (@terms) {
                        print OUT "            <term>\n";
                        print OUT "                <reference>$record->{'term'}</reference>\n";
                        print OUT "                <keyword>$term{$record->{'term'}}</keyword>\n";
                        print OUT "                <weight>$record->{'weight'}</weight>\n";
                        my $total = $#{$list{$record->{'term'}}} + 1;
                        my @list = sort grep {$tmp{$_}} @{$list{$record->{'term'}}};
                        my $nb = $#list + 1;
                        print OUT "                <frequency>$nb/$total</frequency>\n";
                        print OUT "                <list>", join(",", @list), "</list>\n";
                        print OUT "            </term>\n";
                        }
                print OUT "        </terms>\n";
                # Looking for the documents
                my @docs = @{$cluster{$cluster}{'docs'}};
                print OUT "        <documents>\n";
                printf OUT "            <number>%d</number>\n", $#docs + 1;
                # Collecting data on journals
                my %author = ();
                my %source = ();
                foreach $record (@docs) {
                        my $filename = conform($file{$record->{'doc'}});
                        if ( defined $metadata{$filename}{'Author'} ) {
                                foreach my $author (split(/ ; /, $metadata{$filename}{'Author'})) {
                                        push @{$author{$author}}, $record->{'doc'};
                                        }
                                }
                        if ( defined $metadata{$filename}{'Source'} ) {
                                push @{$source{$metadata{$filename}{'Source'}}}, $record->{'doc'};
                                }
                        print OUT "            <document>\n";
                        print OUT "                <reference>$record->{'doc'}</reference>\n";
                        print OUT "                <filename>$filename</filename>\n";
                        if ( defined $metadata{$filename} ) {
                                if ( defined $metadata{$filename}{'Title'} ) {
                                        my $title = conform($metadata{$filename}{'Title'});
                                        print OUT "                <title>$title</title>\n";
                                        }
                                if ( defined $metadata{$filename}{'Author'} ) {
                                        my $author = conform($metadata{$filename}{'Author'});
                                        print OUT "                <author>$author</author>\n";
                                        }
                                if ( defined $metadata{$filename}{'Source'} ) {
                                        my $source = conform($metadata{$filename}{'Source'});
                                        print OUT "                <source>$source</source>\n";
                                        }
                                if ( defined $metadata{$filename}{'Publication date'} ) {
                                        my $date = conform($metadata{$filename}{'Publication date'});
                                        print OUT "                <Date>$date</Date>\n";
                                        }
                                if ( defined $metadata{$filename}{'IstexId'} ) {
                                        my $istexId = conform($metadata{$filename}{'IstexId'});
                                        print OUT "                <istexId>$istexId</istexId>\n";
                                        }
                                if ( defined $metadata{$filename}{'ARK'} ) {
                                        my $ark = conform($metadata{$filename}{'ARK'});
                                        print OUT "                <ark>$ark</ark>\n";
                                        }
                                if ( defined $metadata{$filename}{'DOI'} ) {
                                        my $doi = conform($metadata{$filename}{'DOI'});
                                        print OUT "                <doi>$doi</doi>\n";
                                        }
                                }
                        print OUT "                <weight>$record->{'weight'}</weight>\n";
                        print OUT "            </document>\n";
                        }
                print OUT "        </documents>\n";
                if ( %source ) {
                        print OUT "        <sources>\n";
                        %tmp = ();
                        my $nbSources = 0;
                        foreach my $source (keys %source) {
                                $nbSources ++;
                                $tmp{$source} = $#{$source{$source}} + 1;
                                }
                        printf OUT "            <number>%d</number>\n", $nbSources;
                        foreach my $source (sort {$tmp{$b} <=> $tmp{$a} or $a cmp $b} keys %tmp) {
                                $nbSources --;
                                my @tmp = sort @{$source{$source}};
                                print OUT "            <source>\n";
                                print OUT "                <title>", conform($source), "</title>\n";
                                print OUT "                <frequency>$tmp{$source}/$number{'Source'}{$source}</frequency>\n";
                                print OUT "                <list>", join(",", @tmp), "</list>\n";
                                print OUT "            </source>\n";
                                }
                        print OUT "        </sources>\n";
                        }
                if ( %author ) {
                        print OUT "        <authors>\n";
                        %tmp = ();
                        my $nbAuthors = 0;
                        foreach my $author (keys %author) {
                                $nbAuthors ++;
                                $tmp{$author} = $#{$author{$author}} + 1;
                                }
                        printf OUT "            <number>%d</number>\n", $nbAuthors;
                        foreach my $author (sort {$tmp{$b} <=> $tmp{$a} or $a cmp $b} keys %tmp) {
                                $nbAuthors --;
                                my @tmp = sort @{$author{$author}};
                                print OUT "            <author>\n";
                                print OUT "                <name>", conform($author), "</name>\n";
                                print OUT "                <frequency>$tmp{$author}/$number{'Source'}{$author}</frequency>\n";
                                print OUT "                <list>", join(",", @tmp), "</list>\n";
                                print OUT "            </author>\n";
                                }
                        print OUT "        </authors>\n";
                        }
                print OUT "    </cluster>\n";
                }
        
        # Finally, root end tag
        print OUT "</clusters>\n";
        
        close OUT;
        }

clean(0);

exit 0;


sub usage
{
print STDERR $usage;

exit shift;
}

sub clean
{
my $signal = shift;

if ( not $keep ) {
        while(<$tag*.*>) {
                unlink;
                }
        while(<*.$tag>) {
                unlink;
                }
        }
elsif ( $dir ) {
        while(<$tag*.*>) {
                move($_, "$dir/");
                }
        while(<*.$tag>) {
                move($_, "$dir/");
                }
        }

if ( $signal =~ /^\d+\z/ ) {
        exit $signal;
        }
if ( $signal ) {
        print STDERR "Signal SIG$signal détecté\n";
        exit 9;
        }
else    {
        exit 0;
        }

}

sub conform
{
my $string = shift;

$string =~ s/&/&amp;/go;
$string =~ s/</&lt;/go;
$string =~ s/>/&gt;/go;

return $string;
}
