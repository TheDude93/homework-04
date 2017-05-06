# Functions

# count tweets
count <- function(x, n) {
  length((which(x == n)))
}

# II. feladat/4. függvény
give_tweets <- function(candidate = "Donald Trump", number = 20) {
  if (candidate == "Donald Trump") {
    tweets$sumcol <- tweets$retweet_count + tweets$favorite_count
    trump_tweets <- tweets[tweets$handle == "realDonaldTrump",]
    trump_tweets <-
      trump_tweets[order(trump_tweets$sumcol, decreasing = T),]
    print(trump_tweets[1:number, "text"])
  } else if (candidate == "Hillary Clinton") {
    hillary_tweets <- tweets[tweets$handle == "HillaryClinton",]
    hillary_tweets <-
      hillary_tweets[order(hillary_tweets$sumcol, decreasing = T),]
    print(hillary_tweets[1:number, "text"])
  } else {
    print("Rossz név, próbáld újra!")
  }
}
