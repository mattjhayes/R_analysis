#!/usr/bin/python

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Data Pre-Processing for TC timely experiements prior to
ingestion by R

Pass it full path to directory that contains test result
files to pre-process

"""

import os, sys

import time
from datetime import datetime
from datetime import timedelta
import pytz
from tzlocal import get_localzone

import re

FILENAME_IPERF_START = 'iperf_starttime.txt'
FILENAME_FLOWUPDATES = 'sw1.example.com-OF-snooping.txt' 

treatment_time = 0

utc = pytz.utc

#*** Must have 1 parameter passed to it (first parameter is script)
assert len(sys.argv) == 2

#*** Get parameters from command line
TEST_DIR = sys.argv[1]

#*** Read in the Iperf start time:
FILENAME = os.path.join(TEST_DIR, FILENAME_IPERF_START)

with open(FILENAME, 'r') as f:
    IPERF_STARTTIME = f.readline()

print "IPERF_STARTTIME is", IPERF_STARTTIME
iperf_starttime_datetime = datetime.fromtimestamp(float(IPERF_STARTTIME))
print "iperf_starttime_datetime is ", iperf_starttime_datetime

# get local timezone    
local_tz = get_localzone() 
print "local_tz =", local_tz

iperf_starttime_datetime_tz = local_tz.localize(iperf_starttime_datetime)
print "iperf_starttime_datetime_tz is ", iperf_starttime_datetime_tz

#*** Read in the TC Flow Entry Install Time:
FILENAME = os.path.join(TEST_DIR, FILENAME_FLOWUPDATES)

with open(FILENAME, 'r') as f:
    for line in f.readlines():
        #*** date time is in group 1:
        of_snoop_match = re.match("^(\d+\-\d+\-\d+\s+\d+\:\d+\:\d+\.\d+).*", line)
        if of_snoop_match:
            print "Matched date time ", of_snoop_match.group(1)
            treatment_match = re.search("actions\=set_queue\:1\,goto\_table\:5", line)
            if treatment_match:
                print "matched treatment on line ", line
                if not treatment_time:
                    treatment_time = of_snoop_match.group(1)
                    print "treatment_time is ", treatment_time
                    pattern = '%Y-%m-%d %H:%M:%S.%f'
                    treatment_time1 = datetime.strptime(treatment_time, pattern)
                    print "treatment_time1 is ", treatment_time1
                    treatment_time_tz = utc.localize(treatment_time1)
                    print "treatment_time_tz is ", treatment_time_tz 

#*** Non-DST dependant...
delta = treatment_time_tz - iperf_starttime_datetime_tz 
print "delta is ", delta

print "delta seconds is ", delta.total_seconds()


#2016-04-13 08:29:16.591: OFPT_FLOW_MOD (OF1.3) (xid=0xcd67b0fb): ADD table:4 priority=1,tcp,nw_src=10.1.0.1,nw_dst=10.1.0.2,tp_src=35898,tp_dst=5555 idle:30 out_port:0 actions=set_queue:1,goto_table:5
    
#https://pymotw.com/2/re/

#print "IPERF_STARTTIME is", IPERF_STARTTIME

