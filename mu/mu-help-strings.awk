## Copyright (C) 2012-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


## convert text blobs statements into c-strings

BEGIN {
	in_def=0;
	in_string=0;
	print "/* Do not edit - auto-generated. */"
}


/^#BEGIN/ {
	printf "\tHelp {\n\t\t" $2 ","    # e.g., MU_CONFIG_CMD_ADD
	in_def=1
}

/^#STRING/ {
	if (in_def== 1) {
		if (in_string==1) {
			print ",";
		}
		in_string=1
	}
}

/^#END/ {
	if (in_string==1) {
		in_string=0;
	}
	in_def=0;
	printf "\n\t},\n"
}


!/^#/  {
	if (in_string==1) {
		printf "\n\t\t\"" $0 "\\n\""
	}
}

END {
	print "/* the end */"
}
