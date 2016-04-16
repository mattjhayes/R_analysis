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

Example:

./tc_timely_data_pre_processing.py ~/results/timeliness/statistical \
     /20160414211230/nmeta2-constrained-bw-iperf/20160414211648

"""
#*** For writing to file:
from __future__ import print_function

import os, sys

#*** Time-related imports:
from datetime import datetime
import pytz
from tzlocal import get_localzone

#*** Regular Expressions import:
import re

#*** Constants for filenames to process:
FILENAME_IPERF_START = 'iperf_starttime.txt'
FILENAME_FLOWUPDATES = 'sw1.example.com-OF-snooping.txt'
FILENAME_FLOWTABLE = 'sw1.example.com-flows.txt'

#*** File to write traffic treatment time to:
FILENAME_TT = 'post_process_treatment_time_delta.txt'
#*** File to write errors to:
FILENAME_ERROR = 'error.txt'

#*** Timezones (for conversions to UTC):
UTC = pytz.utc
LOCAL_TZ = get_localzone()

#*** Must have 1 parameter passed to it (first parameter is script)
assert len(sys.argv) == 2

#*** Get parameters from command line
TEST_DIR = sys.argv[1]

def main():
    """
    Main function
    """
    #*** Get the time for when the Iperf test was started:
    iperf_starttime = get_iperf_starttime()

    #*** Get the time when treatment was applied on the switch:
    treatment_time = get_treatment_time()

    #*** Calculate the delta between Iperf start and traffic treatment:
    if iperf_starttime and treatment_time:
        delta = treatment_time - iperf_starttime
        print("delta seconds is ", delta.total_seconds())
        #*** Write result to file:
        treatment_result_filename = os.path.join(TEST_DIR, FILENAME_TT)
        with open(treatment_result_filename, 'w') as f:
            print(delta.total_seconds(), file=f)
    else:
        #*** Uh-oh, something must have gone wrong... Lets write
        #*** something to file for triage later:
        error_filename = os.path.join(TEST_DIR, FILENAME_ERROR)
        with open(error_filename, 'a') as f:
            print("Error iperf_starttime=",
                    iperf_starttime, "treatment_time=",
                    treatment_time, file=f)
        sys.exit("Uh-oh, something must have gone wrong...")

def get_iperf_starttime():
    """
    Return the Iperf test start time, or 0 if error
    """
    iperf_st = 0
    #*** Read in the Iperf start time:
    filename = os.path.join(TEST_DIR, FILENAME_IPERF_START)
    with open(filename, 'r') as f:
        iperf_st = f.readline()
    if iperf_st:
        iperf_st = datetime.fromtimestamp(float(iperf_st))
        iperf_st_tz = LOCAL_TZ.localize(iperf_st)
        print("Iperf was started at", iperf_st_tz)
        return iperf_st_tz
    else:
        print("Error finding Iperf start time")
        return 0

def get_treatment_time():
    """
    Return the time when treatment was applied on the switch,
    or 0 if error
    """
    treatment_time = 0
    #*** Read in the TC Flow Entry (Treatment) Install Time on Switch:
    filename = os.path.join(TEST_DIR, FILENAME_FLOWUPDATES)
    with open(filename, 'r') as f:
        for line in f.readlines():
            if not treatment_time:
                #*** Call function to check the line to see if it is
                #***  a treatment and if so return time in UTC timezone:
                treatment_time = check_snoop_line(line, UTC)
    if treatment_time:
        return treatment_time
    else:
        print("WARNING: failed to find a treatment entry")
        return 0

def check_snoop_line(snoop_line, timezone):
    """
    Passed a line from the OVS snooping file and determine if it is
    the treatment being applied. If treatment found, return the
    time in usable format (datetime object with correct timezone).
    """
    #*** Extract date/time from the line:
    #*** Example line start: 2016-04-14 09:14:38.592: OFPT_FLOW_MOD...
    #*** date time is in group 1:
    of_snoop_match = \
                re.match(r"^(\d+\-\d+\-\d+\s+\d+\:\d+\:\d+\.\d+).*",
                snoop_line)
    if of_snoop_match:
        #*** Now see if line contains pattern for a treatment:
        treatment_match = \
                re.search(r"actions\=set_queue\:1\,goto\_table\:5",
                snoop_line)
        if treatment_match:
            print("matched treatment on line ", snoop_line)
            tt_datetime = of_snoop_match.group(1)
            pattern = '%Y-%m-%d %H:%M:%S.%f'
            tt_datetime2 = datetime.strptime(tt_datetime, pattern)
            tt_datetime2_tz = timezone.localize(tt_datetime2)
            print("treatment_time_tz is ", tt_datetime2_tz)
            return tt_datetime2_tz
    return 0

if __name__ == '__main__':
    main()


