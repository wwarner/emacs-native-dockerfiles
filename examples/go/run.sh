#!/usr/bin/bash
set -eu -o pipefail

./server/server &
export SERVER_PID=$!
trap "trap - SIGTERM && kill -- $SERVER_PID" SIGINT SIGTERM EXIT
sleep 1
./loader/loader
