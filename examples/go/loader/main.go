package main

import (
	"demo/common"
	"log"
	"net/http"
	"sync"
)

// Load the server with CONCURRENCY simultaneous requests, an
// unbalanced mixture of /dec and /inc
func Load(decrs int) {
	url := func(i int) string {
		if i < decrs {
			return "http://localhost/dec"
		}
		return "http://localhost/inc"
	}
	wg := &sync.WaitGroup{}
	wg.Add(common.CONCURRENCY)
	for i := 0; i < common.CONCURRENCY; i += 1 {
		go func(i int) {
			defer wg.Done()
			_, err := http.Get(url(i))
			if err != nil {
				log.Println(err)
			}
		}(i)
	}
	wg.Wait()
}

func main() {
	for {
		for decrs := 0; decrs <= common.CONCURRENCY; decrs += 1 {
			Load(decrs)
		}
		for decrs := common.CONCURRENCY; decrs >= 0; decrs -= 1 {
			Load(decrs)
		}
	}
}
