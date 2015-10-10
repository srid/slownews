defprotocol Slownews.Site do
  @doc "Fetch content as list of links with :title, :url, :meta_url"
  def fetch(site)
end
