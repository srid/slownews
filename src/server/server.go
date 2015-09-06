package main

import (
	"log"
	"net/http"
	"os"
)

// handleData ...
func handleData(w http.ResponseWriter, r *http.Request) {
	var site *Site
	var err error

	if store.IsStale("r/programming") {
		site, err = getSubredditLinks("programming")
		if err != nil {
			httpFatal(w, err)
			return
		}
		store.Put(site)
	}

	httpRespondJSON(w, []*Site{site})
}

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		log.Fatal("PORT must be set")
	}
	http.HandleFunc("/data", handleData)
	fs := http.FileServer(http.Dir("."))
	http.Handle("/", fs)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}
