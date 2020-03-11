#!/bin/bash

# please make sure that postgres is properly installed!!!!

# start the database. Log stuff in a logfile in the same directory as script
echo "Starting database"
postgres -D ./mock/db/ >logfile &
echo "Database started"