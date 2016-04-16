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
#*** File to write DPAE packet count to:
FILENAME_DPAE_PKTS = 'post_process_dpae_pkts.txt'
#*** File to write errors to:
FILENAME_ERROR = 'error.txt'

#*** For finding flow entry that sends packets to DPAE:
FT_TC = 3
FT_DPAE_PRIORITY = 1

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
        #*** Write result to file:
        write_result(FILENAME_TT, delta.total_seconds())
    else:
        #*** Uh-oh, something must have gone wrong... Lets write
        #*** something to file for triage later:
        write_error("Error: iperf_starttime=" + str(iperf_starttime) + \
                    " treatment_time=" + str(treatment_time))

    #*** Get the number of packets sent to the DPAE:
    packets_to_dpae = get_packets_to_dpae()
    if packets_to_dpae:
        #*** Write to file:
        write_result(FILENAME_DPAE_PKTS, packets_to_dpae)
    else:
        write_error("Error: packets_to_dpae is zero")

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

def get_packets_to_dpae():
    """
    Return the number of packets sent to DPAE for classification
    """
    pkts2dpae = 0
    #*** Read in the flow table:
    filename = os.path.join(TEST_DIR, FILENAME_FLOWTABLE)
    with open(filename, 'r') as f:
        for line in f.readlines():
            if not pkts2dpae:
                #*** Call function to check the line to see if it is
                #***  sending packets to DPAE:
                pkts2dpae = check_dpae_pkts(line)
    if pkts2dpae:
        return pkts2dpae
    else:
        print("WARNING: failed to find a packets to DPAE entry")
        return 0

def check_dpae_pkts(ft_line):
    """
    Passed a line from the flow table dump and determine if it
    is the one that sends packets to DPAE for checking. If
    it is then return the number of packets, otherwise 0).
    """
    #*** Extract date/time from the line:
    #*** Example line:
    #  cookie=0x0, duration=23.421s, table=3, n_packets=4, n_bytes=344, priority=1 actions=output:6
    dpae_match = \
                re.search(r"table\=(\d+)\,\s+n_packets\=(\d+)\,\s+n_bytes\=(\d+)\,\s+priority\=(\d+)",
                ft_line)
    if dpae_match:
        flow_table = int(dpae_match.group(1))
        packets = int(dpae_match.group(2))
        bytes_ = int(dpae_match.group(3))
        priority = int(dpae_match.group(4))
        if flow_table == FT_TC and priority == FT_DPAE_PRIORITY:
            return packets
    return 0

def write_result(filename, value):
    """
    Write a result value to a file (overwrites)
    """
    print("Writing result", value, "to file", filename)
    result_filename = os.path.join(TEST_DIR, filename)
    with open(result_filename, 'w') as f:
        print(value, file=f)

def write_error(parameter):
    """
    Append an error message to error file
    """
    print(parameter)
    error_filename = os.path.join(TEST_DIR, FILENAME_ERROR)
    with open(error_filename, 'a') as f:
        print(parameter, file=f)

if __name__ == '__main__':
    main()


