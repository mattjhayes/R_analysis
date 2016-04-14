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

import re

FILENAME_IPERF_START = 'iperf_starttime.txt'
FILENAME_FLOWUPDATES = 'sw1.example.com-OF-snooping.txt' 


#*** Must have 1 parameter passed to it (first parameter is script)
assert len(sys.argv) == 2

#*** Get parameters from command line
TEST_DIR = sys.argv[1]

#*** Read in the Iperf start time:
FILENAME = os.path.join(TEST_DIR, FILENAME_IPERF_START)

with open(FILENAME, 'r') as f:
    IPERF_STARTTIME = f.readline()

print "IPERF_STARTTIME is", IPERF_STARTTIME

#*** Read in the TC Flow Entry Install Time:
FILENAME = os.path.join(TEST_DIR, FILENAME_FLOWUPDATES)

with open(FILENAME, 'r') as f:
    IPERF_STARTTIME = f.readline()
    
#https://pymotw.com/2/re/

#print "IPERF_STARTTIME is", IPERF_STARTTIME

