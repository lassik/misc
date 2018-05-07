package main

import (
	"log"
	"net"
	"net/http"
)

func check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

type hello struct{}

func (_ hello) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte("<h1>Hello world</h1>"))
}

func main() {
	var listener net.Listener
	var err error
	listener, err = net.Listen("tcp", "127.0.0.1:0")
	check(err)
	log.Print("Serving on http://", listener.Addr())
	check(http.Serve(listener, hello{}))
}
