Library_Metadata_Analysis
================
2023-02-07

\#HowLongToBeat method accuracy

We analyze the accuracy of the HowLongToBeat methodology. Each hit to
the database is related to a similarity index. If we take the total
number of hits

\############################Analyze Full

\#Number of entires with HowLong data

print(dim(datos\[!is.na(datos$V3) & !is.na(datos$V4),\])\[1\])

# Number exact matcher HowLongToBeat

print(dim(datos\[datos\$V5==1,\])\[1\])

# Barplot similarity

ggplot(datos,aes(V5)) + geom_bar()

# Relation time to finish and complete

datos_lim\<-datos\[datos\[,3\]!=“No time registered yet”,\]
datos_lim\[,3\]\<-as.numeric(datos_lim\[,3\])
datos_lim\[,4\]\<-as.numeric(datos_lim\[,4\])

datos_lim\<-datos_lim\[datos_lim\[,3\]\>0,\]
datos_lim\<-datos_lim\[datos_lim\[,4\]\>0,\]

ggplot(datos_lim, aes(V4,V3,label=V1)) + geom_point() + theme_bw() +
geom_text(hjust=0, vjust=0) + ylab(“Tiempo pasartelo (h)”) +
xlab(“Tiempo completarlo 100% (h)”)

# A lot of sport sim games and aura kingdom (qu? es, describirlo)

datos_lim\<-datos_lim\[!grepl(“Football Manager”,datos_lim\[,1\]) &
datos_lim\[,1\]!=“Aura Kingdom”,\]

ggplot(datos_lim, aes(V4,V3,label=V1)) + geom_point() + theme_bw() +
geom_text(hjust=0, vjust=0) + ylab(“Tiempo pasartelo (h)”) +
xlab(“Tiempo completarlo 100% (h)”)

# Still a lot, now we cas see RPGs and service under demand game (ejemplos)

datos_lim\<-datos_lim\[datos_lim$V3<150 & datos_lim$V4\<300,\]

ggplot(datos_lim, aes(V4,V3,label=V1, col=V5)) + geom_point() +
theme_bw() + scale_colour_viridis_c() + geom_text(hjust=0, vjust=0) +
ylab(“Tiempo pasartelo (h)”) + xlab(“Tiempo completarlo 100% (h)”)

# Highlight the “problems”. Lets focus on each case an their closest result

datos_lim\[datos_lim\$V5\<1,c(1,2,5)\]\[1:20,\]

# Everything is fine

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