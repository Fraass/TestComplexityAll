plot(occurtot[],time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)[,2],
     xlab="diversity",ylab="Mean Test Complexity")
segments(occurtot[1:83],time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)[1:83,2],
         occurtot[2:84],time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)[2:84,2],
         col='grey95',lwd=4)
occurtot[]->occurtemp
time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)[,1]->time.temp
time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)[,2]->mean.TCI.temp
brewer.pal(8,'BrBG')->timescale.pallet
timescale.pallet[1]<-	"#FFF2AE"
timescale.pallet[2]<-	"#FFFF00"
timescale.pallet[3]<-	"#FDC07A"
timescale.pallet[4]<-	"#FDB46C"
timescale.pallet[5]<-	"#FDA75F"
timescale.pallet[6]<-	"#A6D84A"
timescale.pallet[7]<-	"#8CCD57"
timescale.pallet[8]<-	"#B3E3EE"
point.size<-1.75
#PP
points(occurtot[which(time.temp<5.33)],
       mean.TCI.temp[which(time.temp<5.33)],
       pch=16,col=timescale.pallet[1],cex=point.size)
points(occurtot[which(time.temp<5.33)],
       mean.TCI.temp[which(time.temp<5.33)],cex=point.size)
#Mio
points(occurtot[which(time.temp>=5.33 & time.temp <23.03)],
       mean.TCI.temp[which(time.temp>=5.33& time.temp <23.03)],
       pch=16,col=timescale.pallet[2],cex=point.size)
points(occurtot[which(time.temp>=5.33& time.temp <23.03)],
       mean.TCI.temp[which(time.temp>=5.33& time.temp <23.03)],cex=point.size,col='red')
#Olig
points(occurtot[which(time.temp>=23.03 & time.temp <33.9)],
       mean.TCI.temp[which(time.temp>=23.03& time.temp <33.9)],
       pch=16,col=timescale.pallet[3],cex=point.size)
points(occurtot[which(time.temp>=23.03& time.temp <33.9)],
       mean.TCI.temp[which(time.temp>=23.03& time.temp <33.9)],cex=point.size,col="dodgerblue",lwd=3)
#Eocene
points(occurtot[which(time.temp>=33.9 & time.temp <55.8)],
       mean.TCI.temp[which(time.temp>=33.9& time.temp <55.8)],
       pch=16,col=timescale.pallet[4],cex=point.size)
points(occurtot[which(time.temp>=33.9& time.temp <55.8)],
       mean.TCI.temp[which(time.temp>=33.9& time.temp <55.8)],cex=point.size,col='white')
#Paleocene
points(occurtot[which(time.temp>=55.8 & time.temp <65.5)],
       mean.TCI.temp[which(time.temp>=55.8& time.temp <65.5)],
       pch=16,col=timescale.pallet[5],cex=point.size)
points(occurtot[which(time.temp>=55.8& time.temp <65.5)],
       mean.TCI.temp[which(time.temp>=55.8& time.temp <65.5)],cex=point.size)
#Upper K
points(occurtot[which(time.temp>=65.5 & time.temp <100.5)],
       mean.TCI.temp[which(time.temp>=65.5& time.temp <100.5)],
       pch=16,col=timescale.pallet[6],cex=point.size)
points(occurtot[which(time.temp>=65.5& time.temp <100.5)],
       mean.TCI.temp[which(time.temp>=65.5& time.temp <100.5)],cex=point.size,col='white')
#Lower K
points(occurtot[which(time.temp>=100.5 & time.temp <145.5)],
       mean.TCI.temp[which(time.temp>=100.5& time.temp <145.5)],
       pch=16,col=timescale.pallet[7],cex=point.size)
points(occurtot[which(time.temp>=100.5& time.temp <145.5)],
       mean.TCI.temp[which(time.temp>=100.5& time.temp <145.5)],cex=point.size)
#Jurassic
points(occurtot[which(time.temp >=145.5)],
       mean.TCI.temp[which(time.temp >=145.5)],
       pch=16,col=timescale.pallet[8],cex=point.size)
points(occurtot[which(time.temp >=145.5)],
       mean.TCI.temp[which(time.temp >=145.5)],cex=point.size)

