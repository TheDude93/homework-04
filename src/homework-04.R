#########################
## Házi feladat 4       #
## Programozás I.       #
## 2016/17 II. félév    #
## Takács Bence         #
## 2017-05-02           #
#########################

# II. Feladat
# csv. behívása
tweets <- read.csv2(
  file = "data/clinton_trump_tweets.csv",
  stringsAsFactors = F
)

# hány tweet származik Hillarytől, és Trumptól? 
source("src/homework-04-functions.R")
count(tweets$handle, "realDonaldTrump")
count(tweets$handle, "HillaryClinton")
who_tweeted <- c(count(tweets$handle, "realDonaldTrump"), count(tweets$handle, "HillaryClinton"))
pol_names <- c("Donald Trump", "Hillary Clinton")
plotDf <- data.frame(pol_names, who_tweeted)

# ábra - ki, mennyit tweetelt
library(ggplot2)
cbPalette <- c("blue2", "red2")
ggplot(plotDf, aes(x = pol_names, y = who_tweeted, fill = plotDf$pol_names))+
  xlab(" ") + ylab("Tweet frequency")+
  ggtitle("Candidate Tweets")+
  geom_bar(stat = "identity")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_manual(values=cbPalette,
                    name="Candidates")

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
ggplot(tweets, aes(x = handle, fill = lang))+
  xlab(" ") + ylab("Tweet frequency")+
  ggtitle("Language of Tweets")+
  geom_bar(stat = "count", position = position_dodge())+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_manual(values=colours,
                    name="Language",
                    labels=c("English", "Spanish"))+
  scale_x_discrete(label=c("Hillary Clinton", "Donald Trump"))

# név szerinti tweetek function
source("src/homework-04-functions.R")
give_tweets("Hillary Clinton", 10)
give_tweets("Donald Trump", 15)


# III. Feladat
# hiphop1 plot
library(fivethirtyeight)
data("hiphop_cand_lyrics")
ggplot(hiphop_cand_lyrics,
       aes(x = album_release_date, fill = candidate)) +
  xlab(" ") + ylab(" ") +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs") +
  geom_bar(stat = "count") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) 
