---
title: "Win probability, expected points, and EPA player value in football"
author: "@wiscostretford"
date: "2019-04-20"
output: 
  html_document:
    keep_md: yes
---

(See also my [full blog here](https://wiscostret.wordpress.com/2019/04/20/win-probability-expected-points-and-epa-player-value-in-football/))

**Introduction**

Win probabilities and associated player value are at the frontier of sports analytics in leagues like the NFL and the NBA. Some of the finest contributions to reproducible data analytics in those sports - packages like [nflscrapR](https://www.rdocumentation.org/packages/nflscrapR/versions/1.4.0) and [nbastatR](https://www.rdocumentation.org/packages/nbastatR/versions/0.1.10131) - have win probability and player value measures at the core of their propositions.

Yet these concepts seem (to me anyway) surprisingly absent from the broader discussions of football (soccer) analytics. (Admittedly, I am not the person most in-sync with the analytics community.) Of course, [there](https://www.americansocceranalysis.com/home/2016/6/19/goodbye-expected-goals-hello-expected-points) [are](https://www.rowzreport.com/single-post/2017/10/01/Match-Odds-and-Expected-Points-xP) [discussions](https://help.smarkets.com/hc/en-gb/articles/115001457989-How-to-calculate-Poisson-distribution-for-football-betting) [around](https://arxiv.org/abs/1807.07536). And sites like [Between the Posts](https://betweentheposts.net/) now regularly include 'match odds' based on expected outcome simulations in their match plots.

Still, I found there is less out there than I had expected on this. And this is despite the rapid uptake and spread in football analytics of advanced analyses and measures coming from sports like American football and basketball, such as 'expected' stats, which underlie many 'win probability' models. Furthermore, the explosion of available data sources in football analytics offers fertile ground to produce this kind of analysis.

So, in that context, here is my (very small) first contribution on these topics. I plan a couple of posts on this. To start, I will discuss some simple concepts and measures of win (and draw) probability in football, associated player value, and provide some applied coding structure.

&nbsp;

**Concepts**

*Win probabilities* are, simply, the chance of a given team winning a given game, at a given time. Most advanced win probability models used in other sports include variables like field position, minute by minute events, team strength, and so forth. 

In football, most of these kinds of advanced analytics tend to be based on shot events, since they are relatively easy to record and offer much explanatory value given their importance in the context of football games. This is, for instance, what Between the Posts use to simulate 'match odds', which we can conceptualise as the *ex-post* win probabilities for each team (plus a 'draw' probability). This can be done in the most basic way by applying a Poisson distribution to team xG's, from which the probability of each possible result can be derived (as explained [here](https://www.rowzreport.com/single-post/2017/10/01/Match-Odds-and-Expected-Points-xP)). However, as I will get into below, we can also calculate the win probabilities continuously throughout the game based on match events.

*Expected points* (xPts) are an extension of win probability, converted to points by simply multiplying each expected outcome probability with the points total achieved for that outcome. For instance, if the probabilities for a given game are 45% for a home win, 35% for a draw, and 20% for an away win, then the expected points for the home team is 1.7, and for the away team 0.95.

Turning again to the example of footblal, xPts can be calculated, and often is, ex-post, based on the total team xG (or actual goals). But again as I will get into below, we can do this continuously as well.

*Expected points added* (EPA) is a player value measure that is based on the contribution made by the player to the expected points of the player's team. In short, EPA is simply the change in xPts resulting from a match event, for instance a shot. In that sense, EPA is quite simple to understand and simple to apply player value concept.

Let's say the xG score in the 41st minute of a game is 0.65 to 0.30. Applying a Poisson distribution, the basic match outcome probabilities at that point are 36% for a home win, 46% for a draw, and 14% for an away win. Consequently, the home team xPts is 1.54. If the home team then in the 42nd minute have big chance, with a shot on goal valued at 0.6 xG, the xG score is now 1.25 to 0.30. Now the match outcome probabilities are 61%, 30% and 8%, and the home team xPts is 2.13. The EPA of that shot is then 2.13 minus 1.45 = 0.59.

&nbsp;

**Data**

For this post, I will use data from understat, which provides basic info for all big league matches, as well as (and importantly for the purposes at hand) detailed data for all shot events in big leagues matches. Here I will take the example of a couple of Premier League games from this season.

The data set, which I have manipulated slightly for the purposes at hand, looks like so:




```r
head(usdata)
```

```
##   X.1     id minute      result    X    Y         xG         player h_a
## 1   1 232811      1 BlockedShot 86.3 71.1 0.03996211 Alexis Sánchez   h
## 2   2 232812      2        Goal 88.5 50.0 0.76116884     Paul Pogba   h
## 3   3 232818     39   SavedShot 72.4 65.5 0.01839577     Paul Pogba   h
## 4   4 232819     40   SavedShot 88.0 65.3 0.08121494      Luke Shaw   h
## 5   5 232821     55   SavedShot 78.1 33.0 0.02830883 Matteo Darmian   h
## 6   6 232822     64 MissedShots 81.3 47.6 0.07612572      Juan Mata   h
##   player_id situation season  shotType match_id            h_team
## 1       498  OpenPlay   2018 RightFoot     9197 Manchester United
## 2      1740   Penalty   2018 RightFoot     9197 Manchester United
## 3      1740  OpenPlay   2018 RightFoot     9197 Manchester United
## 4      1006  OpenPlay   2018 RightFoot     9197 Manchester United
## 5       557  OpenPlay   2018 RightFoot     9197 Manchester United
## 6       554  OpenPlay   2018  LeftFoot     9197 Manchester United
##      a_team h_goals a_goals                date player_assisted lastAction
## 1 Leicester       2       1 2018-08-10 22:00:00       Luke Shaw       Pass
## 2 Leicester       2       1 2018-08-10 22:00:00            <NA>   Standard
## 3 Leicester       2       1 2018-08-10 22:00:00  Alexis Sánchez       Pass
## 4 Leicester       2       1 2018-08-10 22:00:00       Juan Mata    Chipped
## 5 Leicester       2       1 2018-08-10 22:00:00  Alexis Sánchez       Pass
## 6 Leicester       2       1 2018-08-10 22:00:00  Alexis Sánchez       Pass
##   id.1 isResult            datetime h.id           h.title h.short_title
## 1 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 2 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 3 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 4 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 5 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
## 6 9197     TRUE 2018-08-10 22:00:00   89 Manchester United           MUN
##   a.id   a.title a.short_title goals.h goals.a   xG.h    xG.a forecast.w
## 1   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 2   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 3   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 4   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 5   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
## 6   75 Leicester           LEI       2       1 1.5137 1.73813     0.2812
##   forecast.d forecast.l              team   X2   Y2
## 1     0.3275     0.3913 Manchester United 86.3 71.1
## 2     0.3275     0.3913 Manchester United 88.5 50.0
## 3     0.3275     0.3913 Manchester United 72.4 65.5
## 4     0.3275     0.3913 Manchester United 88.0 65.3
## 5     0.3275     0.3913 Manchester United 78.1 33.0
## 6     0.3275     0.3913 Manchester United 81.3 47.6
```

&nbsp;

**Analysis**

Let's take the example of the recent Liverpool vs. Chelsea game (14th April 2019), which the home team won 2 to 0. The xG score was 1.31 to 0.87.

&nbsp;

*Shotmap*

Using the [ggsoccer](https://github.com/Torvaney/ggsoccer) package, we can start out by simply plotting the game shotmap:


```r
library(dplyr)
library(ggsoccer)
library(ggplot2)

shotmap <- usdata %>% filter(h.short_title=="LIV",a.short_title=="CHE")

ggplot() + 
  annotate_pitch() + 
  theme_pitch() +
  geom_point(data=shotmap,aes(x=X2,y=Y2,colour=team,size=xG)) +
  scale_colour_manual(values=c("blue","red"),guide=F)
```

![](https://i.imgur.com/VtqywBI.png)<!-- -->

&nbsp;

*Ex-post outcome probability and expected points*

Knowing the total team xG's, we can use the basic Poisson distribution method to calculate the win probabilities for each team, and a draw probability. Leveraging R's 'ppois' function, I've set up a simple function called 'poissonwp' that calculates the Poisson distribution for all possible scoring outcomes (from 0-10), based on the team xG, and then sums up the probabilities for home win, away win, and draw, based on these scoring outcomes. We can feed this the team xGs as follows:




```r
winprobs <- poissonwp(1.31,0.87)

winprobs
```

```
##    xpo outcome
## 1 0.45 homewin
## 2 0.24 awaywin
## 3 0.29    draw
```

So, this tells us that, given the number and quality of chances (from xG), we would expect Liverpool to win the game 45% of the time, Chelsea to win the game 24% of the time, and the teams to draw 29% of the time. 

We can plot this nicely in a stacked bar to illustrate:


```r
library(ggplot2)

ggplot(winprobs,aes(x=1,y=xpo,fill=outcome)) + 
  geom_col(position="stack") + 
  theme_void() + 
  coord_flip() + 
  scale_fill_manual(values=c("blue","grey","red")) +
  xlim(0,2)
```

![](https://i.imgur.com/PKgOvfV.png)<!-- -->

We can also convert this to xPts for each team, with a simple calculation:


```r
# Liverpool:

3 * winprobs$xpo[1] + 1 * winprobs$xpo[3]
```

```
## [1] 1.64
```

```r
# Chelsea:
3 * winprobs$xpo[2] + 1 * winprobs$xpo[3]
```

```
## [1] 1.01
```

&nbsp;

*Continuous outcome probability and expected points*

What is even more interesting than ex-post calculations, I think, are analysing how these measures evolve continuously throughout the game based on match events.

Let's take outcome probabilities to start with. Since we have continuous xG data for each shot event as the game progresses, we can calculate and plot the outcome probabilities at each point during the game, as follows:


```r
library(tidyr)
```


```r
## creating the basic data structure for our expected outcomes (xpo) progress plot

xpoprog <- shotmap %>% 
  select(minute,xG,player,h_a) %>% # selecting out the columns of interest
  arrange(minute) %>% 
  mutate(hcumxG = cumsum(ifelse(h_a=="h",xG,0))) %>% # fist we generate cumulative xG sums, to have the total team xG at any point in the game
  mutate(acumxG = cumsum(ifelse(h_a=="a",xG,0))) %>% 
  full_join(.,data.frame(minute=0:90),by="minute") %>% # then we add in the remaining minutes (where there were no events) and fill the voids
  arrange(minute) %>%
  fill(hcumxG,acumxG)
  
## calculating the poisson outcome probabilities for the home-away xG score for each minute

wpsx <- data.frame()

for (i in 1:nrow(xpoprog)){
  wps <- data.frame(t(poissonwp(xpoprog$hcumxG[i],xpoprog$acumxG[i])$xpo))
  wpsx <- rbind(wpsx,wps)
}

colnames(wpsx) <- c(as.character(unique(shotmap$h.title)),as.character(unique(shotmap$a.title)),"Draw")

## combining the two data frames, we need to adjust for instances where there was more than one shot in a single minute:

xpoprog2 <- cbind(xpoprog,wpsx) %>% add_count(minute)

for (i in 1:nrow(xpoprog2)){
  ifelse(xpoprog2$n[i] > 1 & xpoprog2$minute[i] == xpoprog2$minute[i-1],
  xpoprog2$minute[i] <- xpoprog2$minute[i-1]+0.1,
  "")
}

## finally, we reorder (from wide to long) for plotting

xpoprog3 <- 
  xpoprog2 %>%
  gather(team,as.character(unique(shotmap$h.title)),as.character(unique(shotmap$a.title)),"Draw",value="value")
```

And then we plot:


```r
library(directlabels)
```


```r
xpoprog3 %>% 
  ggplot(aes(x=minute,y=value,colour=team)) +
  geom_path(size=1.5) +
  theme_bw() +
  scale_colour_manual(values=c("blue","grey","red"),guide=F) +
  scale_y_continuous(labels = scales::percent_format(), limits=c(0,1)) +
  geom_dl(aes(label=team),method="smart.grid") +
  labs(title="Outcome probabilities based on xG", x="Minute",y="",caption="Data: understat | @wiscostretford")
```

![](https://i.imgur.com/QCLg4z0.png)<!-- -->

Converting the minute-by-minute outcome probabilities to xPts, we can plot a similar figure:


```r
xptsprog <- xpoprog2 %>% 
  mutate(LiverpoolxP = Liverpool*3 + Draw*1) %>% 
  mutate(ChelseaxP = Chelsea*3 + Draw*1) %>% 
  gather(team,LiverpoolxP,ChelseaxP,value="value")
  
xptsprog %>% 
  ggplot(aes(x=minute,y=value,colour=team)) +
  geom_path(size=1.5) +
  theme_bw() +
  scale_colour_manual(values=c("blue","red"),guide=F) +
  geom_dl(aes(label=team),method="smart.grid") +
  labs(title="Expected points based on xG", x="Minute",y="",caption="Data: understat | @wiscostretford")
```

![](https://i.imgur.com/JrMtxdl.png)<!-- -->

&nbsp;

*Player value and EPA*

As a final analytical step, we can use our continuous expected points calculations to assess 'expected points added' for each player with a shot in the game:


```r
## creating the epa variable

playerepa <- xptsprog %>% mutate(epa=0)

for (i in 2:nrow(playerepa)){
  playerepa$epa[i] <- playerepa$value[i] - playerepa$value[i-1]
}

#summarizing at the player level

playerepa %>% 
  filter(epa > 0) %>% 
  group_by(player) %>% 
  summarize(totalepa = sum(epa), totalxG = sum(xG)) %>% 
  arrange(-totalepa)
```

```
## # A tibble: 10 x 3
##    player                 totalepa totalxG
##    <fct>                     <dbl>   <dbl>
##  1 Sadio Mané               0.85    0.787 
##  2 Eden Hazard              0.71    0.656 
##  3 Willian                  0.190   0.126 
##  4 Mohamed Salah            0.160   0.128 
##  5 Jordan Henderson         0.07    0.0308
##  6 N&#039;Golo Kanté        0.0700  0.0911
##  7 Naby Keita               0.06    0.0632
##  8 Trent Alexander-Arnold   0.0500  0.0180
##  9 Roberto Firmino          0.02    0.0349
## 10 Fabinho                  0.02    0.0294
```

What this tells us is that Sadio Mané was the player whose shots contributed most to his team's expected points throughout the game, producing a total of 0.85 EPA. He's followed by Eden Hazard on 0.71 EPA.

As we can see, this correlates strongly with total xG - naturally, given that xG is the ultimate basis for the EPA calculation. However, the will inevitably be divergence, for instance where high-xG shots (big chances) occur when the team is leading big. In those cases, the EPA will be low, since it doesn't add much to expected points. This is one strength of EPA over raw xG as a player performance evaluator.

Moreover, EPA is scalable to the macro level, looking across whole seasons or multiple seasons, as is commonplace in other sports.

&nbsp;

**Conclusion**

So, that concludes this initial look at win probability, expected points and player value with EPA in football. I think these concepts provide much promise, simple access to relatively advanced and useful analytics ideas. As I hope to have shown above, these analyses are not hard to grasp nor complete. In further posts, I plan to look at this in more detail, so keep an eye out.

Comments and feedback are welcome. Any questions, go find me on Twitter [wiscostretford](http://www.twitter.com/wiscostretford)
