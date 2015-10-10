config :slownews,
  hackernews_maxlinks: 20

  config :quantum, cron: [
    "0 * * * *": &Slownews.Crawler.fetchAll/0
  ]
