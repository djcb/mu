#!/usr/bin/env python3

"""
Script to get date strings, since the MacOS 'date' is not quite up to GNU
standards

E.g..
  date.py 2023-10-14 "The year-month is %y %m"
"""

import sys
from datetime import datetime

date=datetime.strptime(sys.argv[1],'%Y-%m-%d')
print(date.strftime(sys.argv[2]))
