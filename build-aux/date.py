#!/bin/env python
#The MacOS 'date' is not quite up to GNU standards
import sys
from datetime import datetime
date=datetime.strptime(sys.argv[1],'%Y-%m-%d')
print(date.strftime(sys.argv[2]))
#
#  ./date.py 2023-10-14 "The year-month is %y %m"
#
