rm(list=ls())


#functions
rightchars <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
leftchars <- function(x, n){
  substr(x, 1, n)
}
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
#rvest is for the scraping, scales is for the ordinal function
library(rvest)
library(scales)


TeamAbbr <- "WSN"
TeamMascot <- "Nationals"
League <- "NL"
Division <- "East"

#scrape the data fron Baseball Reference####
webpage <- read_html("http://www.baseball-reference.com/")
tbls <- html_nodes(webpage, "table")
if(League == "AL"){
LEAGUE <- webpage %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)
} else {
  LEAGUE <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
}

LEAGUE <- as.data.frame(LEAGUE)
#just get the division data
if (Division == "East"){
  LEAGUE <- LEAGUE[2:6,]
} else if (Division == "Central"){
  LEAGUE <- LEAGUE[8:12,]
}else {
  LEAGUE <- LEAGUE[14:18,]
}

LEAGUE <- LEAGUE[c(1:4, 6)]
#get the team name so I dont have to deal with the division winner symbols
colnames(LEAGUE)[1] <- "LEAGUE"

LEAGUE$Team <- rightchars(LEAGUE$LEAGUE, 3)
LEAGUE$DivClinch <- ifelse(leftchars(LEAGUE$LEAGUE, 1)=="y", "Y", 0)
LEAGUE$PlayoffClinch <- ifelse(leftchars(LEAGUE$LEAGUE, 1)=="x", "Y", 0)
LEAGUE$GB <- ifelse(LEAGUE$GB == "--", 0, LEAGUE$GB)
LEAGUE$rank <- rank(as.numeric(LEAGUE$GB), ties.method = "min")
#get rid of the silly skull and crossbones stuff for eliminated
LEAGUE$E. <- ifelse(LEAGUE$E. == "â " , "PE",LEAGUE$E.)


#scrape BR for the LEAGUE Wildcard table
if(League == "AL"){
  LEAGUEWC <- webpage %>%
    html_nodes("table") %>%
    .[3] %>%
    html_table(fill = TRUE)
} else {
  LEAGUEWC <- webpage %>%
    html_nodes("table") %>%
    .[4] %>%
    html_table(fill = TRUE)
}

LEAGUEWC <- as.data.frame(LEAGUEWC)
colnames(LEAGUEWC)[1] <- "LEAGUE"

LEAGUEWC$GB <- ifelse(LEAGUEWC$GB == "--", 0, LEAGUEWC$GB)
LEAGUEWC$Team <- rightchars(LEAGUEWC$LEAGUE, 3)
#fix the GB column so I have real numbers and no symbols
LEAGUEWC$GB <- ifelse(leftchars(LEAGUEWC$GB, 1)=="+", as.numeric(mid(LEAGUEWC$GB, 2, nchar(LEAGUEWC$GB)))*1, as.numeric(LEAGUEWC$GB)*-1)
#get the first place wild card team GB number
LEAGUEWCLeadGB <- min( LEAGUEWC$GB[LEAGUEWC$GB>0] )
#get rid of the division leaders by getting oLEAGUEy the teams in the wildcard race
LEAGUEWC <- subset(LEAGUEWC, LEAGUEWC$GB<=LEAGUEWCLeadGB)
LEAGUEWC$rank <- rank(-LEAGUEWC$GB, ties.method = "min")


####get the variables to be used in the tweets####


TeamDivPlace <- ordinal(as.numeric(LEAGUE$rank[match(TeamAbbr, LEAGUE$Team)]))
TeamWin <- as.numeric(LEAGUE$W[match(TeamAbbr, LEAGUE$Team)])
TeamLoss <- as.numeric(LEAGUE$L[match(TeamAbbr, LEAGUE$Team)])
TeamGB <- as.numeric(LEAGUE$GB[match(TeamAbbr, LEAGUE$Team)])
#if the team has clinched a playoff spot, don't say anything, if they're leading the division, dont say anything, if they've been eliminated, say so, else, show what the elimination number is

TeamDivElim <- ifelse(LEAGUE$DivClinch[match(TeamAbbr, LEAGUE$Team)] == "Y" | LEAGUE$PlayoffClinch[match(TeamAbbr, LEAGUE$Team)] == "Y" | LEAGUE$E.[match(TeamAbbr, LEAGUE$Team)]=="--", "", 
                      ifelse(LEAGUE$E.[match(TeamAbbr, LEAGUE$Team)]=="PE" | TeamAbbr %!in% LEAGUEWC$Team , paste0("The ", TeamMascot, " have been eliminated from the playoff race. "),
                            ifelse(LEAGUE$E.[match(TeamAbbr, LEAGUE$Team)]=="E", paste0("The ", TeamMascot, " have been eliminated from the division race. "),
                                  ifelse(LEAGUE$E.[match(TeamAbbr, LEAGUE$Team)]!="--", paste0("Their division elimination number is ", LEAGUE$E.[match(TeamAbbr, LEAGUE$Team)], ". ")))))
                                    
TeamGamesPlayed <- TeamWin + TeamLoss
TeamGamesLeft <- 162- (TeamWin + TeamLoss)
TeamDivClinch <- ifelse(LEAGUE$DivClinch[match(TeamAbbr, LEAGUE$Team)]=="Y", paste0("The ", TeamMascot, " have clinched the division!"), "")
TeamWCRank <-ordinal(as.numeric(LEAGUEWC$rank[match(TeamAbbr, LEAGUEWC$Team)]))
TeamWCGB <- as.numeric(LEAGUEWC$GB[match(TeamAbbr, LEAGUEWC$Team)])
TeamWCElim <- ifelse( TeamAbbr %!in% LEAGUEWC$Team, "",  
                      ifelse(LEAGUEWC$E.[match(TeamAbbr, LEAGUEWC$Team)]!="--", paste0("Their wild card elimination number is ", LEAGUEWC$E.[match(TeamAbbr, LEAGUEWC$Team)], ". "), ""))
TeamPlayoffClinch <- ifelse(LEAGUE$PlayoffClinch[match(TeamAbbr, LEAGUE$Team)]=="Y", paste0("The ", TeamMascot, " have clinched a spot in the playoffs!"), "")


TweetDiv <- paste0("Through ", TeamGamesPlayed, " games, the ", TeamMascot, " are ", TeamWin, " and ", TeamLoss, ", good for ", TeamDivPlace, " in the ",  League, " ", Division, ". ")
TweetDivClinch <- TeamDivClinch
TweetDivElim <- TeamDivElim
TweetWC <- ifelse(TeamDivPlace == "1st" |LEAGUE$E.[match(TeamAbbr, LEAGUE$Team)]=="Eliminated" |  TeamAbbr %!in% LEAGUEWC$LEAGUE , "", paste0("In the Wild Card, the ", TeamMascot, " are in ", TeamWCRank, " place. ") )
TweetWCElim <- TeamWCElim

Tweet <- paste0(TweetDiv,TweetDivClinch, TweetDivElim, TweetWC,TweetWCElim, TeamPlayoffClinch )
nchar(Tweet)
Tweet



###Tweet####
 if(TeamGamesPlayed<=162 & TeamGamesPlayed >=1){

library(rtweet)


is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  !(nchar(.x) <= n)   # here's the fix
}


assignInNamespace("is_tweet_length", is_tweet_length, ns = "rtweet")

twitter_token <- create_token(
  app = "APP_NAME",
  consumer_key = "CONSUMER_KEY",
  access_token = "ACCESS_TOKEN",
  consumer_secret = "CONSUMER_SECRET",
  access_secret = "ACCESS_SECRET")
post_tweet(Tweet)
 }

Tweet
