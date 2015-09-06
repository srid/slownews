package main

import (
	"time"
)

type Site struct {
	Name        string    `json:"name"`
	Links       []Link    `json:"links"`
	LastFetched time.Time `json:"last_fetched"`
}

func NewSite(name string, links []Link) *Site {
	return &Site{name, links, time.Now()}
}
