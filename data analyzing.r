################################################################################
# README_CN
# 1.本数据集主要是演示我所使用的得出结果的代码，并不完全代表我的工作流程。
# 2.部分代码在其他文件中运行，包括json文件转档及拉取Reddit数据等。
# 3.鉴于部分代码运行时间较长，我在.RData文件中存储了所有变量以便代码审查。
# 4.尽管我控制了部分代码的随机数，但是我无法保证结果相同。因此结论仅供参考。
# 5.连续运行transformer_scores函数可能导致灾难性的令R崩溃的内存分配问题（特别是RStudio），
#   暂无除了运行rm(list = ls())外的解决方案。
# 6.代码托管于https://github.com/VelenZ/frankredhot_reddit_DA，欢迎提出issues！
################################################################################

################################################################################
# README_EN
# 1. This dataset primarily demonstrates the code I used to obtain results
#    and does not fully represent my workflow.
# 2. Some of the code runs in other files,
#    including converting JSON files and pulling data from Reddit, etc.
# 3. Given that some code takes a long time to run,
#    I have stored all variables in an .RData file for code review purposes.
# 4. Although I have controlled the randomness in some of the code,
#    I cannot guarantee identical results.
#    Therefore, the conclusions are for reference only.
# 5. Continuously running the transformer_scores function
#    may lead to catastrophic memory allocation issues that cause R to crash
#    (especially in RStudio),
#    currently no solution other than running rm(list = ls()).
# 6. The code is hosted at https://github.com/VelenZ/frankredhot_reddit_DA,
#    and issues are welcome!
################################################################################


################################################################################
# ENVIRONMENT SETUP
# Change to your root directory
setwd("/Users/velen/Documents/文稿-iCloud/Learning/JHU/Sping II/Social Media Analytics/Final/frankredhot_reddit_DA")

# 加载.RData文件
# 里面有些没删干净的临时变量，不影响运行
load("frank_da_work_space.RData")
# load("rawdata_reddit&x.RData")
# load("workspace.RData")
# load("sampled_comments_bart.RData")

# About Reddit Data
# Data starting with rh/ta/tp are from Reddit,
# using the same classification logic as classroom example code.

# About Twitter Data
# frh_421 corresponds to the "frank's redhot till April 21" folder.
# fs_421 corresponds to the "Frank's sauce till April 21" folder.
# f_427 corresponds to the "franks batch 2 till April 27" folder.
# frh_427 corresponds to the "FranksRedHot till April 27" folder.
# The following code uses f_427 and fs_421 as the main datasets
# because their content is more complete.

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
library(RedditExtractoR)
library(reticulate)
################################################################################


################################################################################
# PART ONE - X Data Organization and Analysis
# Comparison of sentiment graphs over two weeks and word clouds/word bags

# Using f_427 and fs_421 as the main datasets
# Data Cleaning
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

# Combine data for plotting sentiment graphs
x_combined_data <- cbind(
    matrix(x_421_bart_mean, ncol = 1, dimnames = list(names(x_421_bart_mean), NULL)),
    matrix(x_427_bart_mean, ncol = 1, dimnames = list(names(x_427_bart_mean), NULL))
)
colnames(x_combined_data) <- c("Apr 14-20", "Apr 21-27")

# Function for outputting sentiment bar graphs
create_emotion_plot <- function(emotion_data, colors, title) {
    # The built-in plot has poor support for multiple datasets,
    # switching to use ggplot2 instead
    # Converting matrix to data frame
    data_df <- as.data.frame(emotion_data)
    rownames(data_df) <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
    data_df$Emotion <- rownames(data_df)
    # Melting the data frame to long format
    data_long <- melt(data_df, id.vars = "Emotion", variable.name = "Dataset", value.name = "Value")
    # Creating the bar plot
    plot <- ggplot(data_long, aes(x = Emotion, y = Value, fill = Dataset)) + # nolint
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_fill_manual(values = colors) +
        labs(
            title = title,
            x = "Emotion Types",
            y = "Percentage",
            fill = "Legend"
        ) +
        theme_minimal() +
        # Center the title
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        # Adjust the x-axis spacing
        theme(axis.text.x = element_text(margin = margin(t = 0, b = 10))) +
        # Adjust the y-axis spacing
        theme(axis.text.y = element_text(margin = margin(r = 0, l = 10)))
    return(plot)
}

# Plotting sentiment comparison
create_emotion_plot(
    x_combined_data,
    c("#6BAED6", "#FD8D3C"),
    "Comparison of Emotions Across Two Weeks"
)


# Function for organizing text and outputting word clouds
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
        # Square root is applied to frequencies here,
        # considering the large disparity between
        # high-frequency and low-frequency words.
        words = text_cld$words, freq = floor(sqrt(text_cld$freq)),
        scale = c(5, 1),
        # min.freq = 5,
        max.words = 200,
        random.order = FALSE,
        rot.per = 0.1,
        colors = brewer.pal(10, "Reds")
    )
}

# Output word clouds for two sets of data separately
# Since there is no significant difference between the two word clouds,
# only one is outputted
set.seed(2)
create_word_cloud(c(x_421, x_427))


# Function for organizing text and outputting word bags
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

# Combine and output seven sets of word bags
set.seed(3)
word_bag <- create_word_bag(c(x_421, x_427), 8)
word_bag
################################################################################


################################################################################
# PART TWO - Reddit Data Organization and Analysis
# Sentiment analysis before and after the Super Bowl,
# competitor sentiment comparison analysis

# Change time format
rh_threads$date <- as.Date(rh_threads$date, format = "%Y-%m-%d")
rh_comments$date <- as.Date(rh_comments$date, format = "%Y-%m-%d")

# Split data before and after the Super Bowl
rh_threads_bp <- rh_threads[rh_threads$date < as.Date("2024-02-08"), ]
rh_threads_ap <- rh_threads[rh_threads$date >= as.Date("2024-02-08"), ]

rh_comments_bp <- rh_comments[rh_comments$date < as.Date("2024-02-08"), ]
rh_comments_ap <- rh_comments[rh_comments$date >= as.Date("2024-02-08"), ]

# Function for cleaning Reddit text
clean_reddit_text <- function(input) {
    input <- input %>%
        iconv(from = "Windows-1252", to = "UTF-8") %>%
        gsub("\\[deleted\\]", "", .) %>% # nolint
        gsub("\\[removed\\]", "", .) %>% # nolint
        # Subreddit tags like r/reddit have not been processed
        # as I consider it's meaningful
        replace_html() %>%
        replace_url() %>%
        replace_emoji() %>%
        replace_emoticon() %>%
        replace_non_ascii() %>%
        replace_symbol() %>%
        replace_emoticon() %>%
        replace_non_ascii() %>%
        tolower()
    input <- input[nzchar(input)]
    return(input)
}

# Clean the required data and perform sentiment analysis
# Analyzed both the title and post text together
rh_threads_bp_text <- c(
    clean_reddit_text(rh_threads_bp$title),
    clean_reddit_text(rh_threads_bp$text)
)
rh_threads_ap_text <- c(
    clean_reddit_text(rh_threads_ap$title),
    clean_reddit_text(rh_threads_ap$text)
)
rh_comments_bp_text <- clean_reddit_text(rh_comments_bp$comment)
rh_comments_ap_text <- clean_reddit_text(rh_comments_ap$comment)

rh_comments_bp_text_bart <- transformer_scores(
    text = rh_comments_bp_text,
    classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
    transformer = "facebook-bart"
)
rh_comments_bp_text_bart_mean <- rh_comments_bp_text_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()

rh_comments_ap_text_bart <- transformer_scores(
    text = rh_comments_ap_text,
    classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
    transformer = "facebook-bart"
)
rh_comments_ap_text_bart_mean <- rh_comments_ap_text_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()

# Only used comment data for analysis, as some posts are meaningless
rh_comments_combined_data <- cbind(
    matrix(rh_comments_bp_text_bart_mean,
        ncol = 1,
        dimnames = list(names(rh_comments_bp_text_bart_mean), NULL)
    ),
    matrix(rh_comments_ap_text_bart_mean,
        ncol = 1,
        dimnames = list(names(rh_comments_ap_text_bart_mean), NULL)
    )
)
colnames(rh_comments_combined_data) <- c("Before 02-08-2024", "After 02-08-2024")
create_emotion_plot(
    rh_comments_combined_data,
    c("#A1D99B", "#BCBDDC"),
    "Comparison of Emotions Before and After Jason Kelce Becomes a Spokesperson (Comments Only)"
)

# rh_threads_bp_text_bart <- transformer_scores(
#     text = rh_threads_bp_text,
#     classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
#     transformer = "facebook-bart"
# )
# rh_threads_bp_text_bart_mean <- rh_threads_bp_text_bart %>%
#     as.data.table() %>%
#     transpose() %>%
#     setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
#     colMeans()

# rh_threads_ap_text_bart <- transformer_scores(
#     text = rh_threads_ap_text,
#     classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
#     transformer = "facebook-bart"
# )
# rh_threads_ap_text_bart_mean <- rh_threads_ap_text_bart %>%
#     as.data.table() %>%
#     transpose() %>%
#     setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
#     colMeans()

# ta_url <-
#     find_thread_urls(
#         keywords = "tapatio sauce",
#         sort_by = "relevance",
#         period = "all"
#     )
# ta_content <- get_thread_content(ta_url$url)
# ta_threads <- ta_content$threads
# ta_comments <- ta_content$comments

# tp_url <-
#     find_thread_urls(
#         keywords = "texas pete",
#         sort_by = "relevance",
#         period = "year"
#     )
# tp_content <- get_thread_content(tp_url$url)
# tp_threads <- tp_content$threads
# tp_comments <- tp_content$comments

# Process competitor data
ta_comments_text <- clean_reddit_text(ta_comments$comment)
tp_comments_text <- clean_reddit_text(tp_comments$comment)

# Used sampling to reduce workload, though it sacrifices accuracy
set.seed(2)
ta_sampled_comments <- sample(ta_comments_text,
    size = floor(length(ta_comments_text) / 10)
)
set.seed(2)
tp_sampled_comments <- sample(tp_comments_text,
    size = floor(length(tp_comments_text) / 10)
)

# Competitor sentiment analysis
ta_comments_text_bart <- transformer_scores(
    text = ta_sampled_comments,
    classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
    transformer = "facebook-bart"
)
ta_comments_text_bart_mean <- ta_comments_text_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()
tp_comments_text_bart <- transformer_scores(
    text = tp_sampled_comments,
    classes = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
    transformer = "facebook-bart"
)
tp_comments_text_bart_mean <- tp_comments_text_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()

# Process Frank's sentiment data, simple combination
rh_comments_text_bart <- c(rh_comments_bp_text_bart, rh_comments_ap_text_bart)
rh_comments_text_bart_mean <- rh_comments_text_bart %>%
    as.data.table() %>%
    transpose() %>%
    setNames(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    colMeans()

# Plot sentiment graphs for Frank and competitors
all_comments_combined_data <- cbind(
    matrix(rh_comments_text_bart_mean,
        ncol = 1,
        dimnames = list(names(rh_comments_bp_text_bart_mean), NULL)
    ),
    matrix(ta_comments_text_bart_mean,
        ncol = 1,
        dimnames = list(names(rh_comments_ap_text_bart_mean), NULL)
    ),
    matrix(tp_comments_text_bart_mean,
        ncol = 1,
        dimnames = list(names(rh_comments_ap_text_bart_mean), NULL)
    )
)
colnames(all_comments_combined_data) <- c("Frank's RedHot", "Tapatio Hot Sauce", "Texas Pete")
create_emotion_plot(
    all_comments_combined_data,
    c("#B22222", "#FDD0A2", "#F08080"),
    "Comparison of Emotions Across Three Hot Sauce Brands"
)
################################################################################


################################################################################
# Save the work environment
save.image(file = "frank_da_work_space.RData")
################################################################################
