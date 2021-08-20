#!/usr/bin/perl -w
use POSIX;
use bignum (p => -2);
# Jonathan 06/12/2021

no warnings 'all';

$outputname = "high.txt";
$winsize = 50;
$start = 48724000;
$end = 48729000;
my $literal_string = "NC_000017.11"."\t".substr($start, 0, 4);

$filename = "file_name";

for($i=0;$i<=floor(($end-$start)/$winsize);$i++)
{
	$header[$i] = $start + $i*$winsize;
}

$header_string = join("\t", @header);

open(OUTPUTFILE, ">>".$outputname);
print OUTPUTFILE "$filename\t$header_string\n";
close (OUTPUTFILE);


my @files = <*.cov>;
foreach my $file (@files) 
{
	for($i=0;$i<=floor(($end-$start)/$winsize);$i++)
	{
		$methy_array[$i] = 0;
		$nomethy_array[$i] = 0;
	}
	
	open(INPUTFILE, $file);
	while($datline = <INPUTFILE>)
	{
		if ($datline =~ /^\Q$literal_string\E/)
		{
			chomp($datline);
			@line = split(" ",$datline);

			$location = $line[1];
			$methy = $line[4];
			$nomethy = $line[5];	

			$mindex = floor(($location-$start)/$winsize);

			if ($location>=$start && $location<=$end)
			{
				$methy_array[$mindex] = $methy_array[$mindex]+$methy;
				$nomethy_array[$mindex] = $nomethy_array[$mindex]+$nomethy;
			}
		}
	}
	close(INPUTFILE);

	for($i=0;$i<=floor(($end-$start)/$winsize);$i++)
	{
		$methy_rate[$i] = $methy_array[$i]/($methy_array[$i]+$nomethy_array[$i]);
	}

	$methy_rate_string = join("\t", @methy_rate);

	open(OUTPUTFILE, ">>".$outputname);
	print OUTPUTFILE "$file\t$methy_rate_string\n";
	close (OUTPUTFILE);
}