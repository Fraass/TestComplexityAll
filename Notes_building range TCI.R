plot(#genus.tci[,1],
     na.omit(genus.tci[,2]),
     xlim=c(40,0),
     ylim=c(min(genus.tci[,2:{length(unique(morph$genus))+1}],na.rm=T),
            max(genus.tci[,2:{length(unique(morph$genus))+1}],na.rm=T)),
     type='l')
sample(colors(),{length(unique(morph$genus))+1})->genus.color
for(i in unique(morph$genus)){
  
  lines(#genus.tci[,1],
        na.omit(genus.tci[,i]),
        col = genus.color[which(colnames(genus.tci) == i)])
}
genus.tci.naomit<-genus.tci[,2:length(genus.tci[1,])]
for(i in 1:length(genus.tci.naomit[1,])){
  genus.tci.naomit[!is.na(genus.tci.naomit[,i]),i]->holding.obj
  rep(NA,length(genus.tci.naomit[,i]))->genus.tci.naomit[,i]
  c(holding.obj,
    rep(NA,length(genus.tci.naomit[,i])-length(holding.obj))
    )->genus.tci.naomit[,i]
}
lines(rowMeans(genus.tci.naomit,na.rm=T),lwd=3)



#plot of the mean TCI scores for a genus through time. Average is pretty steady.



