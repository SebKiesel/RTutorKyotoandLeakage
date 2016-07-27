## A function to get the summary statistics of a single sector, only with the variable 'dk'
## As input you need only the wanted sector, between 1 and 12.
sector = function (cat){
  library(lfe)
  library(stargazer)
  library(dplyr)
  felm_limp=felm(limp~dk+fta+wto+eu | sid+year+importer+exporter, data=filter(dat, sector==cat))
  felm_lbeim=felm(lbeim~dk+fta+wto+eu | sid+year+importer+exporter, data=filter(dat, sector==cat))
  felm_lint=felm(lint~dk+fta+wto+eu | sid+year+importer+exporter, data=filter(dat, sector==cat))
  felm_lant=felm(lant~dk | sid+year+importer+exporter, data=filter(dat, sector==cat))
  stargazer(felm_limp, felm_lbeim, felm_lint, felm_lant, type="html", keep=c("dk"))
}

## A function to plot the changes between the period before and after the start of the Kyoto Protocol
## As input you need the wanted variable as a character and the dataset of the variable

changes = function(car,data){
 library(ggplot2)
 library(dplyr)
 library(lfe)
   dat1=filter(data, year>=1997 & year<=2000)
   dat2=filter(data, year>=2004 & year<=2007)
   dat1_minus=filter(dat1, expin==1)
   dat1_null=filter(dat1, bothin==1 | nonein==1)
   dat1_plus=filter(dat1, impin==1)
   dat2_minus=filter(dat2, expin==1)
   dat2_null=filter(dat2, bothin==1 | nonein==1)
   dat2_plus=filter(dat2, impin==1)
   dk=c(-1,0,1)
   AA=car
  switch(AA, 
         "limp"={
           felm_imp=felm(limp~dk+fta+wto+eu | sid+year+importer+exporter, data=data)
           in_limp=c(mean(dat2_minus$limp)-mean(dat1_minus$limp),mean(dat2_null$limp)-mean(dat1_null$limp),mean(dat2_plus$limp)-mean(dat1_plus$limp))
           data.limp=data.frame(dk,in_limp )
           p1<<-ggplot(data.limp, aes(x = dk, y = in_limp)) + stat_summary(fun.y=mean,geom="bar", aes(width=0.5)) + scale_y_continuous("Changes in imports",limits=c(0,1),breaks=seq(-1, 1, .1)) + geom_abline(aes(intercept=mean(dat2$limp)-mean(dat1$limp), slope=coef(felm_imp)[1]), color="red", size=1.5) + ggtitle("Change in Imports after Kyoto") + theme_bw()
           p1
           return(p1)
         },
         "lint"={
           felm_int=felm(lint~dk+fta+wto+eu | sid+year+importer+exporter, data=data)
           in_lint=c(mean(dat2_minus$lint)-mean(dat1_minus$lint),mean(dat2_null$lint)-mean(dat1_null$lint),mean(dat2_plus$lint)-mean(dat1_plus$lint))
           data.lint=data.frame(dk,in_lint )
           p2<<-ggplot(data.lint, aes(x = dk, y = in_lint)) + stat_summary(fun.y=mean,geom="bar", aes(width=0.5)) + scale_y_continuous("Changes in intensity",limits=c(-1,0),breaks=seq(-1, 1, .1)) + geom_abline(aes(intercept=mean(dat2$lint)-mean(dat1$lint), slope=coef(felm_int)[1]), color="red", size=1.5) + ggtitle("Change in Carbon Intensity of the Imports") + theme_bw()
           p2
           return(p2)
         },
         "lbeim"={
           felm_beim=felm(lbeim~dk+fta+wto+eu | sid+year+importer+exporter, data=data)
           in_lbeim=c(mean(dat2_minus$lbeim)-mean(dat1_minus$lbeim),mean(dat2_null$lbeim)-mean(dat1_null$lbeim),mean(dat2_plus$lbeim)-mean(dat1_plus$lbeim))
           data.lbeim=data.frame(dk,in_lbeim )
           p3<<-ggplot(data.lbeim, aes(x = dk, y = in_lbeim)) + stat_summary(fun.y=mean,geom="bar", aes(width=0.5)) + scale_y_continuous("Changes in content",limits=c(-0.3,0.7),breaks=seq(-1, 1, .1)) + geom_abline(aes(intercept=mean(dat2$lbeim)-mean(dat1$lbeim), slope=coef(felm_beim)[1]), color="red", size=1.5) + ggtitle("Change in Carbon Content of the Imports") + theme_bw()
           p3
           return(p3)
         },
         "lant"={
           felm_ant=felm(lant~dk | sid+year+importer+exporter, data=data)
           dat1_minus=filter(dat1_minus, lant!="NA")
           dat1_null=filter(dat1_null, lant!="NA")
           dat1_plus=filter(dat1_plus, lant!="NA")
           dat2_minus=filter(dat2_minus, lant!="NA")
           dat2_null=filter(dat2_null, lant!="NA")
           dat2_plus=filter(dat2_plus, lant!="NA")
           in_lant=c(mean(dat2_minus$lant)-mean(dat1_minus$lant),mean(dat2_null$lant)-mean(dat1_null$lant),mean(dat2_plus$lant)-mean(dat1_plus$lant))
           data.lant=data.frame(dk,in_lant )
           p4<<-ggplot(data.lant, aes(x = dk, y = in_lant)) + stat_summary(fun.y=mean,geom="bar", aes(width=0.5)) + scale_y_continuous("Changes of lant",limits=c(-0.5,0.5),breaks=seq(-1, 1, .1)) + geom_abline(aes(intercept=data.lant$in_lant[2], slope=coef(felm_ant)[1]), color="red", size=1.5) + ggtitle("Change in 'lant'") + theme_bw()
           p4
           return(p4)
         },
         {
           print('default')
         }
  )
}

## A function to plot a world map with 2 colours red and green, for the situation before and after the start of the Kyoto Protocol
## As input you need the grouped dataset with columns importer and val and the title.

map2col=function(data,car){
  library(rworldmap)
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  a1=joinCountryData2Map(data, joinCode="NAME", nameJoinColumn="importer")
  op=palette(c('red','green'))
  cutVector=quantile(a1@data[["val"]],na.rm=TRUE)
  a1@data[["valcat"]]=cut(a1@data[["val"]], c(0,0.5,1), include.lowest=TRUE)
  levels(a1@data[["valcat"]])=c('not committed','committed')
  mapCountryData(a1, nameColumnToPlot='valcat',catMethod='categorial',mapTitle=car, colourPalette='palette',oceanCol='lightblue',missingCountryCol='white')
  
}

## A function to plot a world map with 4 colours red, orange, yellow and green, for the situation before and after the start of the Kyoto Protocol
## As input you need the grouped dataset with columns importer and val and the title.
map4col=function(data, car){
  library(rworldmap)
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  a1=joinCountryData2Map(data, joinCode="NAME", nameJoinColumn="importer")
  op=palette(c('green','yellow','orange','red'))
  cutVector=quantile(a1@data[["val"]],na.rm=TRUE)
  a1@data[["valcat"]]=cut(a1@data[["val"]], cutVector, include.lowest=TRUE)
  levels(a1@data[["valcat"]])=c('low','med','high','very high')
  mapCountryData(a1, nameColumnToPlot='valcat',catMethod='categorial',mapTitle=car, colourPalette='palette',oceanCol='lightblue',missingCountryCol='white')
}

