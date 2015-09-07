package main

type Link struct {
	Title string `json:"title"`
	Url   string `json:"url"`
	Time  int    `json:"time"` // UTC
}
