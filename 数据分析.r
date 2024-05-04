setwd("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA")

load("rawdata_reddit&x.RData")

library(textclean)
library(dplyr)
library(sentimentr)
library(data.table)
library(textcat)
library(zoo)
library(transforEmotion)
library(reshape2)
library(ggplot2)
library(tm)
library(wordcloud)
library(textstem)
library(RColorBrewer)
library(lda)
library(topicmodels)

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

# 姑且是思路
# 先看x数据，使用f_427及fs_421作为主数据集
# 数据清洗
x_421 <- fs_421$text[fs_421$lang == "en"]
x_427 <- f_427$text[f_427$lang == "en"]

x_421 <- x_421 %>%
    gsub("^RT", "", .) %>%
    tolower() %>%
    replace_html() %>%
    replace_url() %>%
    replace_hash() %>%
    replace_emoji() %>%
    replace_emoticon() %>%
    replace_tag() %>%
    replace_non_ascii() %>%
    replace_symbol()
x_427 <- x_427 %>%
    gsub("^RT", "", .) %>%
    tolower() %>%
    replace_html() %>%
    replace_url() %>%
    replace_hash() %>%
    replace_emoji() %>%
    replace_emoticon() %>%
    replace_tag() %>%
    replace_non_ascii() %>%
    replace_symbol()


# Emotion analysis
setup_miniconda()
# use transformer model for emotion analysis
x_421_bart <- transformer_scores(
    text = x_421,
    classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
    transformer = "facebook-bart"
)

x_421_bart_mean <- x_421_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()

barplot(x_421_bart_mean)

x_427_bart <- transformer_scores(
    text = x_427,
    classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
    transformer = "facebook-bart"
)

x_427_bart_mean <- x_427_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()

combined_data <- cbind(
    matrix(x_421_bart_mean, ncol = 1, dimnames = list(names(x_421_bart_mean), NULL)),
    matrix(x_427_bart_mean, ncol = 1, dimnames = list(names(x_427_bart_mean), NULL))
)
colnames(combined_data) <- c("Apr 14-21", "Apr 20-27")

# Converting matrix to data frame
data_df <- as.data.frame(combined_data)
rownames(data_df) <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
data_df$Emotion <- rownames(data_df)

# Melting the data frame to long format
data_long <- melt(data_df, id.vars = "Emotion", variable.name = "Dataset", value.name = "Value")

# Creating the bar plot
ggplot(data_long, aes(x = Emotion, y = Value, fill = Dataset)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = c("#6BAED6", "#FD8D3C")) +
    labs(
        title = "Comparison of Emotions Across Two Weeks",
        x = "Emotion Types",
        y = "Percentage",
        fill = "Dataset"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.text.x = element_text(
        margin = margin(t = 0, b = 10)
    )) +
    theme(axis.text.y = element_text(
        margin = margin(r = 0, l = 10)
    ))


# 整理文本并输出词云
create_word_cloud <- function(text) {
    text_cld <- text %>%
        VectorSource() %>%
        VCorpus() %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(stripWhitespace) %>%
        tm_map(content_transformer(lemmatize_strings)) %>%
        DocumentTermMatrix() %>%
        as.matrix() %>%
        colSums() %>%
        sort(decreasing = TRUE)

    text_cld <- data.table(words = names(text_cld), freq = text_cld)

    par(mar = c(1.5, 1.5, 1.5, 1.5))
    wordcloud(
        words = text_cld$words, freq = text_cld$freq,
        scale = c(5, 1),
        min.freq = 5,
        max.words = 200,
        random.order = FALSE,
        rot.per = 0.1,
        colors = rev(brewer.pal(9, "Set1"))
    )
}

set.seed(2)
create_word_cloud(x_421)
set.seed(2)
create_word_cloud(x_427)


# 整理文本并输出词袋
create_word_bag <- function(text, k) {
    text_bag <- text %>%
        VectorSource() %>%
        VCorpus() %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(stripWhitespace) %>%
        tm_map(content_transformer(lemmatize_strings)) %>%
        DocumentTermMatrix() %>%
        dtm2ldaformat()

    # k <- k
    n <- 1000
    text_bag_result <- lda.collapsed.gibbs.sampler(
        text_bag$documents,
        k,
        text_bag$vocab,
        n,
        alpha = 1 / k,
        eta = 0.1,
        compute.log.likelihood = TRUE
    )
    par(mar = c(3, 3, 3, 3))
    plot(text_bag_result$log.likelihoods[1, ], type = "o")
    r <- top.topic.words(text_bag_result$topics, 10, by.score = TRUE)

    return(r)
}

set.seed(2)
create_word_bag(x_421, 7)
set.seed(2)
create_word_bag(x_427, 7)
