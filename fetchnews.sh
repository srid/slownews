#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

OUTPUTDIR=$1
mkdir -p ${OUTPUTDIR}

function reddit_url {
    SUBREDDIT=$1
    echo "https://www.reddit.com/r/${SUBREDDIT}/top/.json?sort=top&t=week"
}

echo "Fetching reddit"
http get $(reddit_url programming) | jq '.data.children | map({title: .data.title, url: .data.url})' > ${OUTPUTDIR}/reddit.programming.json

echo "Fetching HN"
HNAPI=https://hacker-news.firebaseio.com/v0
HNCOUNT=10
for ID in `http get ${HNAPI}/beststories.json | jq -r '.[]' | head -n ${HNCOUNT}`; do
    http get ${HNAPI}/item/${ID}.json | jq '{title: .title, url: .url}';
done | jq -s '.' > ${OUTPUTDIR}/hn.json

ls -l ${OUTPUTDIR}
