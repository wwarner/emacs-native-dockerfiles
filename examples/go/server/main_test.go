package main

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestStatusHandler(t *testing.T) {
	resp := httptest.NewRecorder()
	statusHandler := StatusHandler(newStorage())
	statusHandler(resp, nil)
	require.Equal(t, `{"I":0}`+"\n", resp.Body.String())
}

func TestDecrementHandler(t *testing.T) {
	resp := httptest.NewRecorder()
	s := newStorage()
	decHandler := DecrementHandler(s)
	decHandler(resp, nil)
	require.Equal(t, http.StatusOK, resp.Code)
	require.Equal(t, "", resp.Body.String())
	resp = httptest.NewRecorder()
	statusHandler := StatusHandler(s)
	statusHandler(resp, nil)
	require.Equal(t, `{"I":-1}`+"\n", resp.Body.String())
}

func TestIncrementHandler(t *testing.T) {
	resp := httptest.NewRecorder()
	s := newStorage()
	decHandler := IncrementHandler(s)
	decHandler(resp, nil)
	require.Equal(t, http.StatusOK, resp.Code)
	require.Equal(t, "", resp.Body.String())
	resp = httptest.NewRecorder()
	statusHandler := StatusHandler(s)
	statusHandler(resp, nil)
	require.Equal(t, `{"I":1}`+"\n", resp.Body.String())
}

func TestMakeBar(t *testing.T) {
	tests := map[string]struct {
		N        int
		Max      int
		Expected string
	}{
		"min max8":     {-8, 8, "■■■■■■■■■------|"},
		"0 max8":       {0, 8, "|-------■------|"},
		"max max8":     {7, 8, "|-------■■■■■■■■"},
		"min max128":   {-128, 128, "■■■■■■■■■------|"},
		"0 max128":     {0, 128, "|-------■------|"},
		"max max128":   {127, 128, "|-------■■■■■■■■"},
		"min max1M":    {-1024 * 1024, 1024 * 1024, "■■■■■■■■■------|"},
		"0 max1M":      {0, 1024 * 1024, "|-------■------|"},
		"max maxM":     {1024*1024 - 1, 1024 * 1024, "|-------■■■■■■■■"},
		"min max23":    {-23, 23, "■■■■■■■■■------|"},
		"0 max23":      {0, 23, "|-------■------|"},
		"max max23":    {22, 23, "|-------■■■■■■■■"},
		"tooneg max23": {-50, 23, "■■■■■■■■■------|"},
		"toopos max23": {22, 23, "|-------■■■■■■■■"},
	}
	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			got := makeBar(test.N, test.Max)
			require.Equal(t, test.Expected, got)
		})
	}
}
