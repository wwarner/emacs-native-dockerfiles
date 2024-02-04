#!/usr/bin/bash
# Copyright 2024 William Warner
# SPDX-License-Identifier: BSD-3-Clause

set -eu -o pipefail

./server/server &
export SERVER_PID=$!
trap "trap - SIGTERM && kill -- $SERVER_PID" SIGINT SIGTERM EXIT
sleep 1
./loader/loader
