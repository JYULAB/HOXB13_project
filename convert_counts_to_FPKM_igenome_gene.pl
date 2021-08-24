my @files = <*ReadsPerGene.out.tab>;

$inputname1 = 'igenome_gene_name_length.txt';
$inputname2 = 'sample_read_mapping.txt';

my %gene_hash;
my %mapping_hash;
my $mreads;

open(INPUTFILE1, $inputname1);
while($datline1 = <INPUTFILE1>)
{
	chomp($datline1);
	my @line1 = split("\t", $datline1);  
#gid is actually gene_name, since it is unique, lazy to change code, gene_name used twice
	my $gid = $line1[0];
	my $genenamelength = "$line1[0]\t$line1[1]"; 
	$gene_hash{$gid} = $genenamelength;
}
close(INPUTFILE1);

open(INPUTFILE2, $inputname2);
while($datline2 = <INPUTFILE2>)
{
	chomp($datline2);
	my @line2 = split("\t", $datline2);  
	my $filename = $line2[0];
	my $mappedreads = $line2[2]; 
	$mapping_hash{$filename} = $mappedreads;
}
close(INPUTFILE2);

foreach $file (@files)
{
	$fname = substr $file, 0, index($file, 'ReadsPerGene');
	if(exists($mapping_hash{$fname}))
	{
		$mreads = $mapping_hash{$fname};
	}
	
	open(OUTPUTFILE, '>', $fname.'_FPKM.txt');
	open(INPUTFILE, $file); 
	
	while($datline = <INPUTFILE>)
	{
		next if 1..4;
		chomp($datline);
		my @line = split("\t", $datline);  
		my $rgid = $line[0];
		my $rawcounts = $line[1]; 
		if(exists($gene_hash{$rgid}))
		{
			@gene = split("\t", $gene_hash{$rgid});
			$genename = $gene[0];			
			$genelength = $gene[1];
			my $FPKM = sprintf "%.7f", ($rawcounts * 1000000000)/($mreads * $genelength);
			print OUTPUTFILE "$rgid\t$FPKM\n";
		}
		else
		{
			print OUTPUTFILE "$rgid\t\n";
		}
	}
	close(INPUTFILE);
	close(OUTPUTFILE);
}
