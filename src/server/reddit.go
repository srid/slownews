package main

import (
	"fmt"
)

type RedditListing struct {
	Kind string `json:"kind"`
	Data struct {
		Children []RedditT3 `json:"children"`
	} `json:"data"`
}

type RedditT3 struct {
	Kind string `json:"kind"`
	Data struct {
		Title   string  `json:"title"`
		Url     string  `json:"url"`
		Created float64 `json:"created_utc"`
	} `json:"data"`
}

func getSubredditSite(subreddit string) (*Site, error) {
	var response RedditListing
	url := fmt.Sprintf("https://www.reddit.com/%s/top/.json?sort=top&t=week", subreddit)
	err := httpGetJSON(url, &response)
	if err != nil {
		return nil, err
	}
	links := normalizeRedditResponse(response)
	return NewSite(subreddit, links), nil
}

// normalizeRedditResponse ...
func normalizeRedditResponse(listing RedditListing) []Link {
	links := make([]Link, 0, len(listing.Data.Children))
	for _, child := range listing.Data.Children {
		links = append(links,
			Link{Title: child.Data.Title, Url: child.Data.Url, Time: int(child.Data.Created)})
	}
	return links
}
