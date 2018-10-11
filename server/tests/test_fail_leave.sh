#! /usr/bin/env sh

# Test a sequence of events

LOCALHOST=localhost
PORT=9000
EVENTS=fail_leave.nc

cat $EVENTS | nc $LOCALHOST $PORT
