// Copyright 2024 William Warner
// SPDX-License-Identifier: BSD-3-Clause

package main

import (
	"context"
	"demo/common"
	"encoding/json"
	"log"
	"net"
	"net/http"
	"net/http/pprof"
	"os"
	"runtime"
	"strconv"
	"sync"
	"time"
)

type storage struct {
	I int
	m *sync.Mutex
}

func (s *storage) add(i int) {
	s.m.Lock()
	defer s.m.Unlock()
	s.I += i
}

func newStorage() *storage {
	return &storage{
		m: &sync.Mutex{},
	}
}

func DecrementHandler(s *storage) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		s.add(-1)
	}
}

func IncrementHandler(s *storage) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		s.add(1)
	}
}

const (
	httpContentTypeHeader = "Content-Type"
	httpContentTypeJson   = "application/json"
)

func StatusHandler(s *storage) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set(httpContentTypeHeader, httpContentTypeJson)
		w.WriteHeader(http.StatusOK)
		s.m.Lock()
		defer s.m.Unlock()
		if err := json.NewEncoder(w).Encode(s); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
	}
}

const (
	ProfilingDisable = iota
	// Enables at 100% sample rate
	ProfilingEnable
)

func ProfilingHandler(setRate func(int), token string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		setRate(ProfilingEnable)
		defer setRate(ProfilingDisable)
		pprof.Handler(token).ServeHTTP(w, r)
	}
}

const (
	point = '■'
	tic   = '|'
	axis  = '-'
)

func makeBar(n, max int) string {
	barlen := 16
	cols := os.Getenv("COLUMNS")
	if cols != "" {
		var err error
		barlen, err = strconv.Atoi(cols)
		if err != nil {
			panic(err)
		}
	}
	barbuf := make([]rune, barlen)

	// normed is the normalized value, i.e. n projected onto our
	// little status bar.
	normed := int(float64(n) * float64(barlen) / 2.0 / float64(max))

	// for every position on the bar, leave either an axis
	// mark, a tic mark or a point mark
	for i := 0; i < barlen; i += 1 {
		// x ranges from -barlen/2 to barlen/2-1
		x := -barlen/2 + i
		// the default mark
		mark := axis
		if x == -barlen/2 || x == 0 || x == barlen/2-1 {
			// when x is at the most negative, zero or the
			// most positive, leave a tic mark
			mark = tic
		}
		if normed == x {
			// normed is our plot value, so leave a point
			// mark here
			mark = point
		}
		// if x between normed and zero, leave a point mark,
		// which creates a bar of height normed
		if normed < 0 && x > normed && x <= 0 {
			mark = point
		}
		if normed > 0 && x < normed && x >= 0 {
			mark = point
		}
		// save the mark
		barbuf[i] = mark
	}
	return string(barbuf)
}

func Heartbeat(ctx context.Context, _range int) {
	h := time.NewTicker(1000 * time.Millisecond)
	for {
		select {
		case <-ctx.Done():
			return
		case <-h.C:
			resp, err := http.Get("http://localhost/status")
			if err != nil {
				log.Printf("err: %v\n", err.Error())
				continue
			}
			s := storage{}
			err = json.NewDecoder(resp.Body).Decode(&s)
			if err != nil {
				panic(err)
			}
			resp.Body.Close()

			// mutexCount, _ := runtime.MutexProfile(nil)
			bar := makeBar(s.I, _range)
			log.Printf("I:%8d %v\n", s.I, bar)
		}
	}
}

func setMutextProfileFraction(i int) {
	runtime.SetMutexProfileFraction(i)
}

func main() {
	s := newStorage()
	mux := http.NewServeMux()
	mux.HandleFunc("/inc", IncrementHandler(s))
	mux.HandleFunc("/dec", DecrementHandler(s))
	mux.HandleFunc("/status", StatusHandler(s))

	mux.HandleFunc("/debug/pprof/", pprof.Index)
	mux.HandleFunc("/debug/pprof/cmdline", pprof.Cmdline)
	mux.HandleFunc("/debug/pprof/profile", pprof.Profile)
	mux.HandleFunc("/debug/pprof/symbol", pprof.Symbol)
	mux.HandleFunc("/debug/pprof/trace", pprof.Trace)

	mux.HandleFunc("/debug/pprof/mutex", ProfilingHandler(setMutextProfileFraction, "mutex"))
	mux.HandleFunc("/debug/pprof/block", ProfilingHandler(runtime.SetBlockProfileRate, "block"))

	listener, err := net.Listen("tcp", ":80")
	if err != nil {
		log.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer func() {
		cancel() // stop the heartbeat
	}()

	// If you loak at the loader, it starts with 0 decrementers
	// and `max` incrementers, then goes to 1 decr and max-1 incr
	// finally ending with max decr and 0 incr. s.I is max on the
	// first pass, max+(max-2) on step two, max+(max-2)+(max-4) on
	// step three. Midway the term is (max/2-max/2)=0. The
	// difference between two successive values is max-2i, a
	// straight line with slope 2. Add up all those differences
	// and we know the maximum value that n will take.
	//
	// max_n is sum(max-2i), 1 <= i <= max/2
	//          == max*max/2 - 2(max/2+1)(max/2)/2
	//          == max/2(max - (max/2+1))
	//          == max/2(max/2+1)
	//
	// 💡 notice that since the difference between each value
	// falls on a straight line, it means that the curve formed is
	// a parabola.

	max_n := common.CONCURRENCY / 2 * (common.CONCURRENCY/2 + 1)
	go Heartbeat(ctx, max_n)
	http.Serve(listener, mux)
}
