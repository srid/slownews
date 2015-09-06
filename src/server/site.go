package main

import (
	"time"
)

type Site struct {
	Name        string
	Links       []Link
	LastFetched time.Time
}

func NewSite(name string, links []Link) *Site {
	return &Site{name, links, time.Now()}
}
