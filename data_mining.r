setwd(
    "/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA"
)
library(RedditExtractoR)

rh_url <-
    find_thread_urls(
        keywords = "frank redhot",
        sort_by = "relevance",
        period = "all"
    )

rh_content <- get_thread_content(rh_url$url)

rh_threads <- rh_content$threads
rh_comments <- rh_content$comments

save(rh_url,
    rh_content,
    rh_threads,
    rh_comments,
    file = "redhot_raw.RData"
)
