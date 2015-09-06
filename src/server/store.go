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

func (s *Store) Get(siteName string) *Site {
	if site, ok := s.Sites[siteName]; ok {
		return site
	}
	panic("not possible")
}

func (s *Store) GetOrFetch(siteName string) (*Site, error) {
	if store.IsStale(siteName) {
		return getSubredditSite(siteName)
	} else {
		return store.Get(siteName), nil
	}
}

func (s *Store) GetOrFetchMultiple(siteNames []string) []*Site {
	sites := make([]*Site, 0, len(siteNames))
	for _, siteName := range siteNames {
		site, err := s.GetOrFetch(siteName)
		if err != nil {
			log.Printf("ERROR fetching %v: %v", siteName, err)
		} else {
			sites = append(sites, site)
		}
	}
	return sites
}
