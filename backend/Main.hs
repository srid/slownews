{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat ["<h1>SlowNews, ", " index page WIP</h1>"]
  get "/data" $ do
    text [r|
           [{"url":"https://www.blog.google/topics/next-billion-users/building-india-first-products-and-features/","title":"Google for India: Building India-first products and features","site":"hn/india#max=1","meta_url":"https://news.ycombinator.com/item?id=15851238","created":1512475689},{"url":"https://news.ycombinator.com/item?id=15853374","title":"AMA: NY AG Schneiderman on net neutrality and protecting our voice in government","site":"hn#max=7","meta_url":"https://news.ycombinator.com/item?id=15853374","created":1512494704}]"
           |]
