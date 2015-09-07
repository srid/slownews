package main

import (
	"fmt"
	"log"
)

const HNCOUNT = 25
const HNSITENAME = "hn"

func getHNSite() (*Site, error) {
	var ids []int
	var links []Link
	url := "https://hacker-news.firebaseio.com/v0/beststories.json"
	err := httpGetJSON(url, &ids)
	if err != nil {
		return nil, err
	}
	for _, id := range ids[:HNCOUNT] {
		link, err := getItem(id)
		if err != nil {
			log.Printf("ERROR fetching HN id %v: %v", id, err)
		} else {
			links = append(links, link)
		}
	}
	return NewSite(HNSITENAME, links), nil
}

func getItem(itemId int) (Link, error) {
	var link Link
	url := fmt.Sprintf("https://hacker-news.firebaseio.com/v0/item/%d/.json", itemId)
	err := httpGetJSON(url, &link)
	return link, err
}
