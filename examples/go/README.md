# A Go Demo

The purpose of this demo app is to illustrate how this development
environment can work with Go.

# Usage

    M-x multi-vterm
	# make run
    rm server/server loader/loader
    go test -v demo/server
    === RUN   TestStatusHandler
    --- PASS: TestStatusHandler (0.00s)
	...
    PASS
    ok      demo/server
    go build -o server/server demo/server
    go build -o loader/loader demo/loader
    ./run.sh
    2024/01/26 03:07:06 I:       8 |-------■------|
    2024/01/26 03:07:07 I:    4805 |-------■■■----|
    2024/01/26 03:07:08 I:    8677 |-------■■■■■--|
    2024/01/26 03:07:09 I:   11779 |-------■■■■■■-|
    2024/01/26 03:07:10 I:   14060 |-------■■■■■■■|
    2024/01/26 03:07:11 I:   15634 |-------■■■■■■■■
    2024/01/26 03:07:12 I:   16409 |-------■■■■■■■■
    2024/01/26 03:07:13 I:   16330 |-------■■■■■■■■
    2024/01/26 03:07:14 I:   15550 |-------■■■■■■■■
    2024/01/26 03:07:15 I:   14042 |-------■■■■■■■|
    2024/01/26 03:07:16 I:   11859 |-------■■■■■■-|
    2024/01/26 03:07:17 I:    8831 |-------■■■■■--|
    2024/01/26 03:07:18 I:    4837 |-------■■■----|
    2024/01/26 03:07:19 I:     103 |-------■------|
    2024/01/26 03:07:20 I:   -4522 |-----■■■------|
    2024/01/26 03:07:21 I:   -8242 |----■■■■------|
    2024/01/26 03:07:22 I:  -11492 |--■■■■■■------|
    2024/01/26 03:07:23 I:  -13981 |-■■■■■■■------|
    2024/01/26 03:07:24 I:  -15604 |■■■■■■■■------|
    2024/01/26 03:07:25 I:  -16418 |■■■■■■■■------|
    2024/01/26 03:07:26 I:  -16345 |■■■■■■■■------|
    2024/01/26 03:07:27 I:  -15797 |■■■■■■■■------|
    2024/01/26 03:07:28 I:  -14373 |-■■■■■■■------|
    2024/01/26 03:07:29 I:  -12125 |--■■■■■■------|
    2024/01/26 03:07:30 I:   -8971 |---■■■■■------|
    2024/01/26 03:07:31 I:   -5166 |-----■■■------|
    2024/01/26 03:07:32 I:    -492 |-------■------|
