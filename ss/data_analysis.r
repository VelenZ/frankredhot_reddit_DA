load("redhot_raw.RData")
library(dplyr)

summary(rh_comments)
summary(rh_threads)

# 查看独特值出现的次数，初衷是找subreddit的个数
rh_comments_unique <- rh_comments %>% summarise_all(n_distinct)
rh_threads_unique <- rh_threads %>% summarise_all(n_distinct)

# 一些思路，想到哪里写到哪里
# 关键词方面，上一个文档已经写了，略，虽然需要考虑到后续调整的可能，也许可以多组关键词？
# 关键的数据
# threads：
# url，匹配帖子和评论的工具
# date，和广告播出时间做对比
# title，判断积极/消极/中性/混合的依据
# text，作为对title的补充
# subreddit，对内容进行分类（asmr？你们玩得挺花哈？）
# socre/up_ratio，判断帖子热门程度，可以标准化后作为权重使用
# comments，同样是判断帖子热门程度，可能需要筛掉一些没有回复的帖子
# comments：
# url，匹配帖子和评论的工具
# date，和广告播出时间做对比
# comment，判断积极/消极/中性/混合的依据
# upvotes，浏览者对于帖子的赞成程度，可惜的是我们没有浏览量数据
# comment_id：我在想一种办法来把这个数据利用起来，例如，上层说了一个反面观点，下层回答“Yes”，那它就加强了上层的观点；
# 然而，某些程度上upvotes也做了类似的事情，因此可能需要找些其他用法