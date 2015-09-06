package main

import (
	"log"
	"net/http"
	"os"
	"strings"
)

var SITES []string

func init() {
	sitesString := os.Getenv("SITES")
	if sitesString == "" {
		sitesString = "r/programming:r/haskell:r/elm"
	}
	SITES = strings.Split(sitesString, ":")
	log.Printf("SITES = %+v", SITES)
}

// handleData ...
func handleData(w http.ResponseWriter, r *http.Request) {
	sites := store.GetOrFetchMultiple(SITES)
	log.Printf("Responding with %d sites", len(sites))
	httpRespondJSON(w, sites)
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
