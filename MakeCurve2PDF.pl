#!/usr/bin/perl
#------
#------
#------
#------
sub RunPerl; 
sub GetColumns;
sub CreateCurve;
sub MaxInTable;
use FindBin qw($Bin);   #get the directory of this perl Script
$CurDir= $Bin;
$CurDir4R=$CurDir;
$pathDataR=$CurDir."/DataR/";
$pathRaw=$CurDir."/Input/";
$outPath=$CurDir."/PDF/";
$tmpRfile=$pathDataR."tmpRScript.R";
$ChIPseqListFile =$CurDir."/ChIPseqList.txt";
$patternFilter=".txt";
#-------create directories if not exist----------
chdir($CurDir)
   or die("Can't change to dir $CurDir: $!\n");
unless (-d $pathRaw) {
		unless(mkdir "Input") {
			die "Unable to create directory\n";
		}
	}
	unless (-d $pathDataR) {
		unless(mkdir "DataR") {
			die "Unable to create directory\n";
		}
	}
	unless (-d $outPath) {
		unless(mkdir "PDF") {
			die "Unable to create directory\n";
		}
	}
	

@myRawFileList=GetFileList($pathRaw,$patternFilter);
#-------------Check whether input files are ready----------------
if (scalar(@myRawFileList)<1) {
  print "\nPlease copy your input files to $pathRaw\n";
  print "\n\tInput file - a tab-delimited txt file (*.txt).\n";
  exit;
}
#--------------------delete all tmp files----------------------------
if (not Is_folder_empty($pathDataR)) {
chdir ($pathDataR);
unlink <*.*>;
}
#-------------------process all txt files in .\Input-----------------------------------

	foreach $FileIn(@myRawFileList){
    print "\t",$FileIn,"...\n";

		open(FILE1, $pathRaw.$FileIn) or die "Couldn't open $pathRaw.$FileIn for reading: $!";
		 @Data =();
		 @Data = <FILE1>;
		close FILE1;
       #----------------------
		foreach $line(@Data) {
				$HeaderLine=$line;  
				#print $HeaderLine
				last ;
		 }
       #----------------------
        @items=();
		@items = split("\t",$HeaderLine);
		$count=0;
		$Column='';
	 $Flag='';		
	 $DistanceFlag=0;
	 foreach $ID(@items) { 
			$count++;
			#print $ID;
			 if (($ID=~/Distance/) and ($DistanceFlag ==0)){
				# print "Found \" Distance \"\n";
				$DistanceFlag=1;
				$Column.=$count;
				$NewHeaderLine="Distance";
			 }

			  if ($ID=~/Coverage/) {
				  $Column.=",".$count;
				  #print $ID." \n";
					#histogram -- "/TagDir/831/ Coverage
				   $temp = $ID;
				   @ChIPs=split("\/",$ID);
				     #print "\n\t$temps[$#ChIPs-1]\t$temps[$#ChIPs-2]\n";
					 $NewHeaderLine.="\t".(substr $ChIPs[$#ChIPs-1],0);
					  $Flag ="Hist.R";
			 }
			 
			 if ($ID=~/total sites/) {
				  $Column.=",".$count;
				  #print $ID." \n";
					#histogram -- "/TagDir/831/ Coverage
				   $temp = $ID;
				   @ChIPs=split("\/",$ID);
				     #print "\n\t$temps[$#ChIPs-1]\t$temps[$#ChIPs-2]\n";
					 $NewHeaderLine.="\t".(substr $ChIPs[$#ChIPs-1],0);
					  $Flag ="Hist.R";
			 }
		}

		#print 		" \n". $NewHeaderLine." \n";
		#print 		" \n". $Column." \n";
        @returnData=();
		@returnData= GetColumns($Column, @Data) ;
		splice @returnData, 0, 0,  $NewHeaderLine."\n"; #Insert $NewHeaderLine to the 0 position of @returndata
		push @yMax, MaxInTable("T", @returnData);
		#print "\n@yMax\n";
        $fileData=$pathDataR.$FileIn.".dataR.txt";
		open (FILE2, ">".$fileData) or die "Couldn't open ".$FileIn.".dataR.txt for writing: $!";
		foreach $tmpX (@returnData) {
			print FILE2 $tmpX;
		}
		close FILE2;
		#-------------createCurve----------------------

		chop($MotifList) ;   #delete last comma
	   $TemplateRfile= $CurDir."/".$Flag;
		do CreateCurve($TemplateRfile,$CurDir4R,$FileIn.".dataR.txt",$ChIPinfo[1],$MotifList);
		 #CreateCurve ($TemplateR,$DataDir,$FileName,$Antibody,$Motifs)=@_;
}
$Max= max(@yMax);
#-----------------------------------------
#------------
$patternFilter=".dataR.txt.R";
my @myRFileList2=GetFileList($pathDataR,$patternFilter);
	foreach $FileIn(@myRFileList2){
    # print "\t",$FileIn,"...\n";
	open(TEMPLATE, $pathDataR.$FileIn) or die "Couldn't open $pathDataR.$FileIn for reading: $!";
	open (RScript, ">".$pathDataR.$FileIn.".m") or die "Couldn't open file for writing: $!";
	$x=$y=0;
	while(<TEMPLATE>)
	{
			$line=$_;	 
			if ($_=~/yMax/) {
					#print $line;
					$x++;
					if ($x==2) {
						$line ="yMax<-".$Max."\n";
					 } 					
					 if ($x==3) { 
						 $line ="#".$line;
					 }
			  }
             if ($line=~/pdfOutFile/) {
				$y++;
				if ($y==1) {
					$line ="pdfOutFile=paste(OpenFile,\".max.pdf\",sep=\"\")\n";
				  }		  
			   }
			   print RScript $line;
		}
	close TEMPLATE;
     close RScript;
	#set the location of R 
	$cmd= 'C:\Program Files\R\R-3.4.0\bin\Rscript.exe';
	@args = ($cmd, $pathDataR.$FileIn.".m");
	system(@args) == 0
	or die "system @args failed: $?";
	}
#----------------------------------------------------
print "\tAll files have been processed!\n";
print  "\n\tRaw files in [$pathRaw]";
print  "\n\tR files in [$pathDataR]";
print  "\n\tPDF files in [",$CurDir,"/PDF/]\n";
#-------------------process all txt files in \Raw---------------------------
exit;

sub RunPerl {
	my($perlScript,$parameter) = @_; 
	#set the location of perl
$cmd= 'C:\perl\bin\perl.exe';
@args = ($cmd, $perlScript,$parameter);
system(@args) == 0
or die "system @args failed: $?";
}

sub GetColumns {
	#--------GetColumns------------------
	# Extract columns from @data
	#---------------------------------------
	   my ($Column, @Data);    # Variables known only to this subroutine (preferred)
		my @ExtractColumns;
		 @Data=(); @ExtractData=(); @ExtractColumns =(); #empty out array

	     $Column=$_[0];            #First parameter
		 @Data = @_[1..$#_];   #Second parameter
		@ExtractColumns = split(',',$Column);	

      foreach $myline(@Data) {
		 chomp ($myline);
		  next if ($myline=~/Distance/) ;  #skip the first line of Homer results generated by annotatePeak.pl
		  next if ($myline=~/[#]/);    #skip comments
		 @items = split("\t",$myline);
		 $strTmp=''; 

			#-------------------------------------
			foreach $temp (@ExtractColumns) {

				$temp1=$temp-1;
				if ($strTmp eq '') {
					$strTmp = $items[$temp1];
					#$tmp = "peak_$count"; 
				} else {
					$strTmp .="\t".$items[$temp1];
			   } #End if
	       } #End foreach2
			#-------------------------------------
         # print $strTmp."\n";
		push @ExtractData, $strTmp."\n";
   }#End foreach1
   return (@ExtractData);
}


sub GetFileList{
	 my ($dir, $RegExpFilter)= @_; 
	 my @files;
	opendir(DIR, $dir);
	@files = grep(/$RegExpFilter/,readdir(DIR));
	closedir(DIR);
	return (@files);
 }
sub CreateCurve {
my($TemplateR,$DataDir,$FileName)=@_;

open(TEMPLATE, $TemplateR) or die "Couldn't open $TemplateR for reading: $!";
$tmpRfile=$pathDataR.$FileName.".R";

open (RScript, ">".$tmpRfile) or die "Couldn't open ".$tmpRfile." for writing: $!";
while(<TEMPLATE>)
{
	    $line=$_;	 
		if ($_=~/\$\$/) {   # not empty
			#print $line;
			$line =~ s/\$\$DataDir/$DataDir/;
			$line =~ s/\$\$FileName/$FileName/;
			#print "\t".$line."\n";
		  }
	   print RScript $line;
	}
close(TEMPLATE);
close (RScript);

}


sub MaxInTable{
my ($Header,@Data)=@_;
use List::Util qw[min max];
 $Header=$_[0];            #First parameter
 @Data = @_[1..$#_];   #Second parameter
my @tmpData,@tempArray, $tmpLine;
$n=0;
		foreach $tmpLine (@Data) {
			$n++;
            chomp ($tmpLine);
			@tempArray=split ("\t",$tmpLine);
			if ($Header eq "T"){
			     next if ($n==1);    #skip Header line
                shift @tempArray;  #skip first column
			};
			push @tmpData, @tempArray;
		}
return (max(@tmpData));
}

sub Is_folder_empty {
    my $dirname = shift;
    opendir(my $dh, $dirname) or die "Not a directory";
    return scalar(grep { $_ ne "." && $_ ne ".." } readdir($dh)) == 0;
}