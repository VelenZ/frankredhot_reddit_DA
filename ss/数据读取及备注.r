setwd("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA")

load("rawdata_reddit&x.RData")

# 关于Reddit数据
# 以rh为开头的都是reddit数据，具体使用方法可以参考头几节课的代码

# 关于推特数据
# The dataset has three outputs
# output.json contains the tweets that matched your search queries.
# This should be the main dataset for analysis when you study the difference between Reddit and Twitter.
# Output-tweets.json: this is related tweets. e.g. the tweet in the output.json may reply to another message.
# This dataset contains all the related tweets.
# Output-users.json contains all info about users.
# frh_421对应frank's redhot till April 21文件夹
# fs_421对应Frank's sauce till April 21文件夹
# f_427对应franks batch 2 till April 27文件夹
# frh_427对应FranksRedHot till April 27文件夹
# 这几个文件夹只是查找关键字不同，建议每段时间用一组数据作为代表即可
