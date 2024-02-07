build: server/server loader/loader

server/server:
	go test -v demo/server
	go build -o server/server demo/server

loader/loader:
	go build -o loader/loader demo/loader

run: clean build
	./run.sh

clean:
	rm -f server/server loader/loader
