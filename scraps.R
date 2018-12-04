time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)->time.TCI
plot(0,0,
     type='n',
     xlim=c(170,0),
     xlab="Time (Ma)",
     ylab="Mean of Test Complexity",
     ylim=c(min(temp[,2],na.rm=T),
            max(temp[,2],na.rm=T))
)

which(timeresolution.dataframe$sig.origin == 'y')->sig.origin
which(timeresolution.dataframe$sig.extin == 'y')->sig.extin

rect(timeresolution.dataframe$end[sig.origin],.8,
          timeresolution.dataframe$start[sig.origin],1.3,
          col='blue',border=F)
rect(timeresolution.dataframe$end[sig.extin],1.3,
     timeresolution.dataframe$start[sig.extin],1.8,
     col='red',border=F)

lines(midstage,occurtot/100+.9,col='grey',lwd=2)

lines(time.TCI[,1],time.TCI[,2],lwd=3,col='black')

morph$family->family.ID
range.line.plot(test.complex.index,
                "Test Complexity Index",
                family.ID,
                morph$origin,
                morph$extin
)
lines(time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe),lwd=2)
lines(time.median(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe),lwd=2,col="red")
time.var(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)->temp
lines(temp,lwd=2,col='blue')

range.line.plot<-function(var.A,y.axis.label,family.ID,origin,extin){
  #Setting the spacer to make the vioplots visible
  spacer<-4.5
  #setting colors
  #these are based on the colors set in the other function (vioplot.pca)
  c(brewer.pal(12,'Set3'),brewer.pal(8,"Set2"),brewer.pal(9,'Set1'),'black','grey','black','brown')->col.temp
  fam.col<-rep(NA,length(family.ID))
  droplevels(family.ID)->family.ID
  col.code<-rep(NA,times=length(levels(family.ID)))
  for(i in 1:length(levels(family.ID))){
    levels(family.ID)[i]->temp.ID
    if(temp.ID == "Candeinidae"){col.code[i]<-1}
    if(temp.ID == "Cassigerinellidae"){col.code[i]<-3}
    if(temp.ID == "Chiloguembelinidae"){col.code[i]<-4}
    if(temp.ID == "Favusellidae"){col.code[i]<-5}
    if(temp.ID == "Globigerinelloididae"){col.code[i]<-6}
    if(temp.ID == "Globigerinidae"){col.code[i]<-7}
    if(temp.ID == "Globoquadrinidae"){col.code[i]<-8}
    if(temp.ID == "Globorotaliidae"){col.code[i]<-9}
    if(temp.ID == "Globotruncanidae"){col.code[i]<-10}
    if(temp.ID == "Globuligerinidae"){col.code[i]<-11}
    if(temp.ID == "Guembelitriidae"){col.code[i]<-13}
    if(temp.ID == "Hantkeninidae"){col.code[i]<-14}
    if(temp.ID == "Hastigerinidae"){col.code[i]<-15}
    if(temp.ID == "Hedbergellidae"){col.code[i]<-16}
    if(temp.ID == "Heterohelicidae"){col.code[i]<-19}
    if(temp.ID == "Planomalinidae"){col.code[i]<-21}
    if(temp.ID == "Rotaliporidae"){col.code[i]<-22}
    if(temp.ID == "Rugoglobigerinidae"){col.code[i]<-23}
    if(temp.ID == "Schackoinidae"){col.code[i]<-27}
    if(temp.ID == "Truncorotaloididae"){col.code[i]<-31}
    if(temp.ID == "UNKNOWN"){col.code[i]<-32}
    if(temp.ID == "Paraticinellinae"){col.code[i]<-33}
  }
  col.key<-cbind(levels(family.ID),col.code) #col.key is now a 2-col obj with family (c1) and color.index (c2)
  for(i in 1:nrow(col.key)){
    col.key[i,2]<-col.temp[as.numeric(col.key[i,2])]
  }
  1->counter
  for(i in levels(family.ID)){ #This portion takes colkey and subs in the color value from RColorBrewer
    fam.col[which(family.ID == i)]<-col.key[which(col.key[,1] == i),2]
    counter+1->counter
  }
  plot(0,0,type='n',
       xlim=c(max(as.vector(origin),na.rm=T),-{spacer*length(levels(family.ID))*.9}),
       ylim=c(min(var.A,na.rm=T),max(var.A,na.rm=T)),
       ylab=paste(y.axis.label),
       xlab='Time (Ma)'
  )
  1->counter
  aggregate(var.A,by=list(family.ID),FUN=median,na.rm=T)->a.order
  order(a.order[,2])->a.order.1
  a.order[a.order.1,1]->a.order#setting an order for the vioplots
  for(i in a.order){
    if(all(is.na(var.A[which(family.ID == i)])) != T){
      segments(as.vector(origin[which(family.ID == i)]),
               var.A[which(family.ID == i)],
               as.vector(extin[which(family.ID == i)]),
               var.A[which(family.ID == i)],
               col=col.key[which(col.key[,1] == i),2],
               lwd=2
      )
      vioplot(na.omit(var.A[which(family.ID == i)]),
              horizontal=F,
              at=-{counter-0.5}*spacer,
              add=T,
              border=F,
              col=col.key[which(col.key[,1] == i),2],
              wex=spacer,
              na.rm=T)
      text(-{counter-0.5}*spacer,
           2.5,
           labels=i,
           col=col.key[which(col.key[,1] == i),2],
           srt=90
           
      )
    }
    counter+1->counter
  }
}

