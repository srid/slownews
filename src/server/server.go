package main

import (
	"log"
	"net/http"
	"os"
)

// handleData ...
func handleData(w http.ResponseWriter, r *http.Request) {
	links, err := getSubredditLinks("programming")
	if err != nil {
		httpFatal(w, err)
	} else {
		httpRespondJSON(w, links)
	}
}

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		log.Fatal("PORT must be set")
	}
	http.HandleFunc("/data", handleData)
	fs := http.FileServer(http.Dir("static"))
	http.Handle("/", fs)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}
