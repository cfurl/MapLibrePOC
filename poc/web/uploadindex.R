library(paws)

local_html <- paste0(
  "C:/Users/cfurl/OneDrive - Edwards Aquifer Authority/",
  "r/MapLibrePOC/poc/web/index.html"
)

s3 <- paws::s3(
  config = list(
    region = "us-east-2"
  )
)

s3$put_object(
  Bucket = "cfhydromet-site-prod",
  Key = "map-poc/index.html",
  Body = local_html,
  ContentType = "text/html",
  CacheControl = "no-cache"
)

cat(
  "Uploaded:\n",
  "s3://cfhydromet-site-prod/map-poc/index.html\n"
)