package main

import (
	"log"
	"os"
	"time"
)

type Store struct {
	Sites map[string]*Site
}

var CACHE_DURATION time.Duration
var store *Store

func init() {
	store = new(Store)
	store.Sites = make(map[string]*Site)

	cacheDuration := os.Getenv("CACHE_DURATION")
	if cacheDuration == "" {
		cacheDuration = "30m"
	}
	var err error
	CACHE_DURATION, err = time.ParseDuration(cacheDuration)
	if err != nil {
		log.Fatalf("Invalid cache duration: %v", err)
	}
}

// (s *store) IsStale ...
func (s *Store) IsStale(siteName string) bool {
	if site, ok := s.Sites[siteName]; ok {
		return time.Now().Sub(site.LastFetched) > CACHE_DURATION
	}
	return true
}

func (s *Store) Put(site *Site) {
	s.Sites[site.Name] = site
}
