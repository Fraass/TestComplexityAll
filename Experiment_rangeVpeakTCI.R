resolution<-0.1
genus.tci<-matrix(NA,
                  nrow=length(time.mean(test.complex.index,
                                        morph$origin,
                                        morph$extin,
                                        resolution)[,1]
                  ),ncol={length(unique(morph$genus))+1})
as.data.frame(genus.tci)->genus.tci
as.character(unique(morph$genus))->colnames(genus.tci)[2:{length(unique(morph$genus))+1}]
colnames(genus.tci)[1]<-'midstage'
species.in.genus<-NA
time.mean(test.complex.index,
          morph$origin,
          morph$extin,
          0.1)[,1]->genus.tci[,1]

for(i in unique(morph$genus)){
  which(morph$genus == i)->species.in.genus
  time.mean(test.complex.index[species.in.genus],
            morph$origin[species.in.genus],
            morph$extin[species.in.genus],
            resolution)->holding.obj
  rev(holding.obj[,2])->holding.obj[,2]
  holding.obj[,2]-holding.obj[2,2]->holding.obj[,2]
  which(round(holding.obj[1,1],digits=1) == round(genus.tci[,1],digits=1))->start
  genus.tci[start:{start+length(holding.obj[,2])-1},i]<-holding.obj[,2]
}


rm(species.in.genus)

plot(genus.tci[,1],
     genus.tci[,2],
     xlim=c(170,0),
     ylim=c(min(genus.tci[,2:{length(unique(morph$genus))+1}],na.rm=T),
            max(genus.tci[,2:{length(unique(morph$genus))+1}],na.rm=T)),
     type='l')
sample(colors(),{length(unique(morph$genus))+1})->genus.color
for(i in "Globigerinelloides"){
  
  lines(genus.tci[,1],
        genus.tci[,i],
        col = genus.color[which(colnames(genus.tci) == i)])
}

genus.tci.naomit<-genus.tci[,2:length(genus.tci[1,])]
for(i in 1:length(genus.tci.naomit[1,])){
  genus.tci.naomit[!is.na(genus.tci.naomit[,i]),i]->holding.obj
  rep(NA,length(genus.tci.naomit[,i]))->genus.tci.naomit[,i]
  c(holding.obj,
    rep(NA,length(genus.tci.naomit[,i])-length(holding.obj))
  )->genus.tci.naomit[,i]
  
  genus.tci.naomit[,i]-genus.tci.naomit[1,i]->genus.tci.naomit[,i]
}

plot(#genus.tci[,1],
  na.omit(genus.tci.naomit[,1]),
  xlim=c(1000,0),
  ylim=c(-15,
         15),
  type='l')
sample(colors(),{length(unique(morph$genus))+1})->genus.color
for(i in unique(morph$genus)){
  
  lines(#genus.tci[,1],
    na.omit(genus.tci.naomit[,i]),
    col = genus.color[which(colnames(genus.tci) == i)])
}
for(i in 1:length(unique(morph$genus))){
  text(range[i],end.tci[i],
       unique(morph$genus)[i],
       col=genus.color[which(colnames(genus.tci) == unique(morph$genus)[i])])
}


##I'm concerned that something is messed up, since I don't remember Globigerinelloides 
#oh, it's a K taxon. Goddammit, I keep getting it mixed up w/ Globigerinoides
#let's check anyway
max(morph$origin[which(morph$genus == "Globigerinelloides")])
min(morph$extin[which(morph$genus == "Globigerinelloides")])
#welp, yeah, it's right
#done checking


#idea
range<-NA;end.tci<-NA;peak.tci<-NA;peak.max.tci<-NA;peak.min.tci<-NA
for(i in 1:length(genus.tci.naomit[1,])){
  range[i]<-max(which(is.na(genus.tci.naomit[,i]) == F))
  end.tci[i]<-genus.tci.naomit[range[i],i]
  peak.max.tci[i]<-max(genus.tci.naomit[,i],na.rm=T)
  peak.min.tci[i]<-abs(min(genus.tci.naomit[,i],na.rm=T))
  peak.tci[i]<-max(c(peak.max.tci[i],peak.min.tci[i]))
}

plot(range,end.tci,col='black',pch=16,cex=1)
text(range,end.tci,unique(morph$genus))
#highlight modern & K-extin
unique(morph$genus[which(morph$extin <= 0)])->mod.gen
unique(morph$genus[which(morph$extin <= 66 & morph$extin >= 65)])->endk.gen
points(range[which(unique(morph$genus) == mod.gen)],
       end.tci[which(unique(morph$genus) == mod.gen)]
       ,col='grey',pch=16,lwd=2)
points(range[which(unique(morph$genus) %in% endk.gen)],
       end.tci[which(unique(morph$genus) %in% endk.gen)]
       ,col='lightblue',pch=16,lwd=2)


plot(range,abs(end.tci),col='black',pch=16,cex=.75)
text(range,abs(end.tci),unique(morph$genus))

plot(range,peak.tci,col='black',pch=16,cex=.75)
text(range,peak.tci,unique(morph$genus))
#end idea





lines(rowMeans(genus.tci.naomit,na.rm=T),lwd=3)
#plot of the mean TCI scores for a genus through time. Average is pretty steady.
abline(h=0)

matrix(NA,nrow=10000,ncol=ncol(genus.tci.naomit))->genus.tci.scaled

for(i in 1:ncol(genus.tci.naomit)){
  round(10000/length(na.omit(genus.tci.naomit[,i])))->rep.times
  for(o in 1:length(na.omit(genus.tci.naomit[,i]))){
    min(which(is.na(genus.tci.scaled) == T))->start
    if(10000-start >= rep.times){
      rep(genus.tci.naomit[o,i],rep.times)->genus.tci.scaled[start:{start+rep.times-1},i]
    }
    if(10000-start < rep.times){
      rep(genus.tci.naomit[o,i],length(start:10000))->genus.tci.scaled[start:10000,i]
    }
  }
}
