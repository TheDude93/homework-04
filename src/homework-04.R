#########################
## Házi feladat 4       #
## Programozás I.       #
## 2016/17 II. félév    #
## Takács Bence         #
## 2017-05-02           #
#########################

# II. Feladat
# csv. behívása
tweets <- read.csv2(file = "data/clinton_trump_tweets.csv",
                    stringsAsFactors = F)

# hány tweet származik Hillarytől, és Trumptól?
source("src/homework-04-functions.R")
count(tweets$handle, "realDonaldTrump")
count(tweets$handle, "HillaryClinton")
who_tweeted <-
  c(count(tweets$handle, "realDonaldTrump"),
    count(tweets$handle, "HillaryClinton"))
pol_names <- c("Donald Trump", "Hillary Clinton")
plotDf <- data.frame(pol_names, who_tweeted)

# ábra - ki, mennyit tweetelt
library(ggplot2)
cbPalette <- c("blue2", "red2")
ggplot(plotDf,
       aes(
         x = pol_names,
         y = who_tweeted,
         fill = plotDf$pol_names
       )) +
  xlab(" ") + ylab("Tweet frequency") +
  ggtitle("Candidate Tweets") +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()) +
  scale_fill_manual(values = cbPalette,
                    name = "Candidates")

# nyelv ellenőrzése
table(tweets$lang)

tweets$text[tweets$lang == "et"]
tweets$text[tweets$lang == "fr"]
tweets$text[tweets$lang == "tl"]
tweets$text[tweets$lang == "fi"]
tweets$text[tweets$lang == "da"]

tweets$lang[tweets$lang == "et"] <- c("en")
tweets$lang[tweets$lang == "fr"] <- c("en")
tweets$lang[tweets$lang == "tl"] <- c("en")
tweets$lang[tweets$lang == "fi"] <- c("en")
tweets$lang[tweets$lang == "da"] <- c("en")

# ábra nyelvhasználat
colours <- c("darkgrey", "cornflowerblue")
ggplot(tweets, aes(x = handle, fill = lang)) +
  xlab(" ") + ylab("Tweet frequency") +
  ggtitle("Language of Tweets") +
  geom_bar(stat = "count", position = position_dodge()) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()) +
  scale_fill_manual(
    values = colours,
    name = "Language",
    labels = c("English", "Spanish")
  ) +
  scale_x_discrete(label = c("Hillary Clinton", "Donald Trump"))

# név szerinti tweetek function
source("src/homework-04-functions.R")
give_tweets("Hillary Clinton", 10)
give_tweets("Donald Trump", 15)


# III. Feladat
# hiphop1 plot
library(fivethirtyeight)
data("hiphop_cand_lyrics")
cvpalett <-
  c(
    "yellowgreen",
    "seagreen",
    "goldenrod2",
    "lightskyblue2",
    "yellow1",
    "tomato1",
    "lightpink1",
    "violetred1"
  )
ggplot(hiphop_cand_lyrics,
       aes(x = album_release_date, fill = candidate)) +
  xlab(" ") + ylab(" ") +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs") +
  geom_bar() +
  geom_bar(
    stat = "count",
    width = 1,
    colour = "grey60",
    show.legend = F) +
  scale_fill_manual(
    values = cvpalett,
    breaks = c(
      "Donald Trump",
      "Hillary Clinton",
      "Jeb Bush",
      "Chris Christie",
      "Mike Huckabee",
      "Bernie Sanders",
      "Ben Carson",
      "Ted Cruz"
    ),
    labels = c(
      "Trump",
      "Clinton",
      "Bush",
      "Christie",
      "Huckabee",
      "Sanders",
      "Carson",
      "Cruz"
    ),
    drop = F
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# hiphop2 plot
ggplot(
  transform(hiphop_cand_lyrics, sentiment = factor(
    sentiment, levels = c("positive", "negative", "neutral")
  )),
  aes(album_release_date, fill = candidate)
) +
  geom_bar() +
  geom_bar(
    stat = "count",
    width = 1,
    colour = "grey60",
    show.legend = F
  ) + facet_grid(. ~ sentiment) +
  scale_fill_manual(
    values = cvpalett,
    breaks = c(
      "Donald Trump",
      "Hillary Clinton",
      "Jeb Bush",
      "Chris Christie",
      "Mike Huckabee",
      "Bernie Sanders",
      "Ben Carson",
      "Ted Cruz"
    ),
    labels = c(
      "Trump",
      "Clinton",
      "Bush",
      "Christie",
      "Huckabee",
      "Sanders",
      "Carson",
      "Cruz"
    ),
    drop = F
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    legend.box = "horizontal"
  ) +
  ggtitle("Candidate mentions, by sentiment")

# saját plot
ggplot(
  transform(hiphop_cand_lyrics, sentiment = factor(
    sentiment, levels = c("positive", "negative", "neutral")
  )),
  aes(album_release_date, fill = theme)
) +
  geom_bar() +
  geom_bar(
    stat = "count",
    width = 1,
    colour = "white",
    show.legend = F
  ) + facet_grid(. ~ sentiment) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    legend.box = "horizontal"
  ) +
  ggtitle("Themes, by sentiment")

# IV. Feladat
# 1. részfeladat
ggplot(
  transform(tweets, text_sentiment = factor(
    text_sentiment, levels = c("positive", "negative", "neutral")
  )),
  aes(text_sentiment, fill = text_emotion)
) +
  geom_bar() +
  geom_bar(
    stat = "count",
    width = 1,
    colour = "white",
    show.legend = F
  ) + facet_grid(. ~ handle) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    legend.box = "horizontal"
  ) +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Sentiment & emotion of tweets")

# IV/2. feladat
trump_source <- tweets[((tweets$source_url == "http://twitter.com/download/iphone") |
                         (tweets$source_url == "http://twitter.com/download/android") &
                         (tweets$handle == "realDonaldTrump")), ]

levels(trump_source$source_url)
trump_source$source_url <- droplevels(trump_source$source_url, "http://instagram.com",                        
           "http://twitter.com" ,                         
           "http://twitter.com/#!/download/ipad"  ,       
                    
           "https://about.twitter.com/products/tweetdeck",
           "https://mobile.twitter.com"         ,         
           "https://studio.twitter.com")
levels(trump_source$source_url) <- c("Android", "iPhone")
ggplot(
  trump_source,
  aes(text_sentiment, fill = text_emotion)
) +
  geom_bar() +
  geom_bar(
    stat = "count",
    width = 1,
    colour = "white",
    show.legend = F
  ) + facet_grid(. ~ source_url) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    legend.box = "horizontal"
  ) +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Sentiment & emotion of tweets")
