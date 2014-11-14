BEGIN {
	RS=",";
	for(i=1;i<=9;i++) {
		for(j=1;j<=9;j++) {
			result[i][j] = ".";
		}
	}
	 sf = 10;
	 f = 0;
}

{
	gsub("\\[","");
	gsub("\\]","");
	gsub("\"","");
	result[$2][$3] = $4
}

/FLUSH/ {
	f++;

	if (f%sf==0) {
		f==0;
		#clear screen
		printf "\x1b\x5b\x48\x1b\x5b\x32\x4a";
		for(i=1;i<=9;i++) {
			if ((i-1) % 3 == 0) {
				print "-----------------------";
			}
			for(j=1;j<=9;j++) {
				if (((j) % 3 == 0) && (j>1)) {
					sep = " | ";
				} else {
					sep = " ";
				}
				printf result[j][i]sep;
			}
			printf "\n";
		}
		print "-----------------------";
	}

	for(i=1;i<=9;i++) {
		for(j=1;j<=9;j++) {
				result[i][j] = ".";
		}
	}
}

END {
	for(i=1;i<=9;i++) {
		if ((i-1) % 3 == 0) {
			print "-----------------------";
		}
		for(j=1;j<=9;j++) {
			if (((j) % 3 == 0) && (j>1)) {
				sep = " | ";
			} else {
				sep = " ";
			}
			printf result[j][i]sep;
		}
		printf "\n";
	}
	print "-----------------------";
}
