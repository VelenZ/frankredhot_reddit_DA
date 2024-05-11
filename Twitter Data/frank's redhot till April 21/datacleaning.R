library(jsonlite)

# The dataset has three outputs
# output.json contains the tweets that matched your search queries.
# This should be the main dataset for analysis when you study the difference between Reddit and Twitter.
# Output-tweets.json: this is related tweets. e.g. the tweet in the output.json may reply to another message.
# This dataset contains all the related tweets.
# Output-users.json contains all info about users.

frh_421_tweet <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/frank's redhot till April 21/output-tweets.json")
frh_421_user <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/frank's redhot till April 21/output-users.json")
frh_421 <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/frank's redhot till April 21/output.json")

fs_421_tweet <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/Frank's sauce till April 21/output-tweets.json")
fs_421_user <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/Frank's sauce till April 21/output-users.json")
fs_421 <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/Frank's sauce till April 21/output.json")

f_427_tweet <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/franks batch 2 till April 27/output-tweets.json")
f_427_user <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/franks batch 2 till April 27/output-users.json")
f_427 <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/franks batch 2 till April 27/output.json")

frh_427_tweet <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/FranksRedHot till April 27/output-tweets.json")
frh_427_user <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/FranksRedHot till April 27/output-users.json")
frh_427 <- fromJSON("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA/FranksRedHot till April 27/output.json")




