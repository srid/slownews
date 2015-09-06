package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

// httpGetJSON ...
func httpGetJSON(url string, value interface{}) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return err
	}
	return json.Unmarshal(body, value)
}

func httpRespondJSON(w http.ResponseWriter, value interface{}) {
	data, err := json.Marshal(value)
	if err != nil {
		httpFatal(w, err)
	} else {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprintf(w, "%s", string(data))
	}
}

func httpFatal(w http.ResponseWriter, err error) {
	http.Error(w, err.Error(), 500)
}
