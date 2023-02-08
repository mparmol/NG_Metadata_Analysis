
## HowLongToBeat method accuracy

We analyze the accuracy of the HowLongToBeat methodology. Each hit to
the database is related to a similarity index.

First we need to know the number of games to which we have found a
result in the database when calling their name with the API. This data
is analyzed from the “Full” metadata table.

``` r
print(dim(datos[!is.na(datos$V3) &  !is.na(datos$V4),])[1])
```

    ## [1] 2870

From the total number we can see the similarity distribution.

``` r
ggplot(datos,aes(V5)) + geom_bar()
```

![](Library_Metadata_Analysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Being the total number of exact matches (similarity = 1)

``` r
print(dim(datos[datos$V5==1,])[1])
```

    ## [1] 2588

Focusing on the games that aren’t similaryt=1 we can highlight the
“problems”. Lets focus on each case an their closest result

``` r
datos[datos$V5<1,c(1,2,5)][1:20,]
```

    ##                                            V1
    ## 14                                 Magic 2015
    ## 23       Plants vs. Zombies: Game of the Year
    ## 25        The Whispered World Special Edition
    ## 35         Darksiders II Deathinitive Edition
    ## 37                            Battle vs Chess
    ## 40               Hellblade: Senua's Sacrifice
    ## 59                                    Saviors
    ## 66                    Jotun: Valhalla Edition
    ## 70                Wasteland 2: Director's Cut
    ## 79           Monkey Island 2: Special Edition
    ## 89       Broken Sword 3 - the Sleeping Dragon
    ## 90                       DuckTales Remastered
    ## 91                  Resident Evil Revelations
    ## 93                                 Hell Yeah!
    ## 94             Dead Island Definitive Edition
    ## 98              Shovel Knight: Treasure Trove
    ## 103     Planescape: Torment: Enhanced Edition
    ## 104                                99 Spirits
    ## 106     Sherlock Holmes: The Devil's Daughter
    ## 110 Injustice: Gods Among Us Ultimate Edition
    ##                                                         V2   V5
    ## 14  Magic: The Gathering - Duels of the Planeswalkers 2015 0.19
    ## 23                                      Plants vs. Zombies 0.50
    ## 25                                     The Whispered World 0.54
    ## 35                     Darksiders II: Deathinitive Edition 0.97
    ## 37                                        Battle vs. Chess 0.94
    ## 40                            Hellblade: Senua's Sacrifice 0.96
    ## 59               The Ninja Saviors: Return of the Warriors 0.20
    ## 66                                                   Jotun 0.22
    ## 70                                             Wasteland 2 0.41
    ## 79                      Monkey Island 2: LeChuck's Revenge 0.56
    ## 89                       Broken Sword: The Sleeping Dragon 0.97
    ## 90                                   DuckTales: Remastered 0.95
    ## 91                              Resident Evil: Revelations 0.96
    ## 93                     Hell Yeah! Wrath of the Dead Rabbit 0.29
    ## 94                         Dead Island: Definitive Edition 0.97
    ## 98                                           Shovel Knight 0.45
    ## 103                                    Planescape: Torment 0.95
    ## 104                                            Tsukumogami 0.00
    ## 106                  Sherlock Holmes: The Devil's Daughter 0.97
    ## 110            Injustice: Gods Among Us - Ultimate Edition 0.95

So as we can see, most “problems” are derived from typos, such as “:” o
romanic values

## Recomended and forbiden games for achivement collectors

If you are a completionist you would like to play games that are easy to
100% complete. This is not only the shorttest games, but the games that
could be easily completed just playing (avoiding in the majority of
cases farming/grinding games). The representation of the time to beat
against the time t beat at 100% could shade lights on this question

``` r
datos_lim<-datos[datos[,3]!="No time registered yet",] # We remove the entries with not time regisrered yet
datos_lim[,3]<-as.numeric(datos_lim[,3]) 
datos_lim[,4]<-as.numeric(datos_lim[,4])

datos_lim<-datos_lim[datos_lim[,3]>0,] # We remove the 0 hours game
datos_lim<-datos_lim[datos_lim[,4]>0,]

ggplot(datos_lim, aes(V4,V3,label=V1)) + geom_point() + theme_bw() + geom_text(hjust=0, vjust=0) + ylab("Tiempo pasartelo (h)") + xlab("Tiempo completarlo 100% (h)")
```

![](Library_Metadata_Analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

A lot of sport simulation games, concretely Football Manager games, and
Aura Kingdom are the games that show the most biased distribution when
comparing both parameters, based in farming and repetition. Football
Manager games surround 1000h of game play to complete it at a 100%,
while Aura Kingdom, a MMO, is up to 9000h to finish it. It is easy to
think on these as farming related achievements that will take an insane
amount of time.

But we can see much using this kind of approach, so we better create a
index to save the games that are more biased according to the difference
beeteen finished and 100% games

``` r
datos_lim$time_index<-datos_lim$V4/datos_lim$V3

datos_lim <- datos_lim[order(as.factor(datos_lim$time_index),decreasing = T),]
datos_lim$V1 <- reorder(datos_lim$V1, -datos_lim$time_index)

#time_rel<-datos_lim[order(datos_lim$time_index,decreasing=TRUE),]

#ggplot(time_rel[1:5,],aes(V1,time_index)) + geom_bar()

ggplot(datos_lim[1:30,], aes(V1,time_index,label=V1)) + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
```

![](Library_Metadata_Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

If we remove these games we could focus on the next most biased games

``` r
datos_lim<-datos_lim[!grepl("Football Manager",datos_lim[,1]) & datos_lim[,1]!="Aura Kingdom",]

ggplot(datos_lim, aes(V4,V3,label=V1)) + geom_point() + theme_bw() + geom_text(hjust=0, vjust=0) + ylab("Tiempo pasartelo (h)") + xlab("Tiempo completarlo 100% (h)")
```

![](Library_Metadata_Analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Even after removing those games, there are still plenty of games biased
to the 100% completion rate. Now we can see othe sport games, such as
NBA and Our of the park games, in company with MMO titles like Black
Desert, on the top of the figure. After removing them we can see other
games. These games are the more time demanding game, for finishing and
completion, but not the most biased games. To get the names of the most
based game we should focus on the games that have a really low time to
complete (less than 5h) but insane time for 100% finishing.

``` r
datos_lim<-datos_lim[datos_lim$V3<=5,]

ggplot(datos_lim, aes(V4,V3,label=V1)) + geom_point() + theme_bw() + geom_text(hjust=0, vjust=0) + ylab("Tiempo pasartelo (h)") + xlab("Tiempo completarlo 100% (h)")
```

![](Library_Metadata_Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

This representation is not the best option for lloking at the data,
let’s represent it as barplots

``` r
datos_lim<-datos_lim[datos_lim$V3<=1,]

ggplot(datos_lim, aes(V3,V4,label=V1)) + geom_jitter(position = position_jitter(seed = 1)) + geom_text(position = position_jitter(seed = 1)) + theme_bw()
```

![](Library_Metadata_Analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
This way you can take a closer look at games that will only take an hour
to finish, most of them Arcade games, but will take up to 80 hours to
finish completely, in the case of King of Fighters ’98 Ultimate Match
Final Edition. (ESTO ES MEJOR HACER BoxPlot y mostrar solo los puntos
que sean outlier…cómo?)

################################# Analyze Steam_library

datos\<-read.delim(“Steam_Library_Metadata_marko_pakete.txt”,header = T)

###### N?mero de juegos, tiempo total de juego, tiempo medio de juego, conteo de generos, puntuaci?n media y desviaci?n, n?mero de juegos por develop y publisher

\#Number of entries

print(dim(datos)\[1\])

# Time to finish library

datos_lim\<-datos\[datos\[,3\]!=“No time registered yet” &
!is.na(datos\[,3\]),\] datos_lim\[,3\]\<-as.numeric(datos_lim\[,3\])
datos_lim\[,4\]\<-as.numeric(datos_lim\[,4\])

datos_lim\<-datos_lim\[datos_lim\[,3\]\>0,\]
datos_lim\<-datos_lim\[datos_lim\[,4\]\>0,\]

print(paste0(((sum(datos_lim\$V3)/24)/31)/12,” years”))

# Time to COMPLETE library

datos_lim\<-datos\[datos\[,4\]!=“No time registered yet” &
!is.na(datos\[,4\]),\] datos_lim\[,3\]\<-as.numeric(datos_lim\[,3\])
datos_lim\[,4\]\<-as.numeric(datos_lim\[,4\])

datos_lim\<-datos_lim\[datos_lim\[,3\]\>0,\]
datos_lim\<-datos_lim\[datos_lim\[,4\]\>0,\]

print(paste0(((sum(datos_lim\$V4)/24)/31)/12,” years”))

\######Tiempo medio

datos_lim\<-datos\[datos\[,8\]!=“No time registered yet” &
!is.na(datos\[,8\]),\] datos_lim\[,8\]\<-as.numeric(datos_lim\[,8\])
datos_lim\<-datos_lim\[datos_lim\[,8\]\>0,\]

mean(datos_lim\[,8\])

######## Pensar c?mo representar valoraci?n de los juegos por compa??a y por a?o

factor(datos\$V14)

heatmap(datos$V15,datos$V14)

#################### 

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Library_Metadata_Analysis_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
