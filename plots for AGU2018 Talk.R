plot(0,0,
     type='n',
     xlim=c(170,0),
     ylim=c(min(morph$expans,na.rm=T),
            max(morph$expans,na.rm=T)),
     xlab='Time (Ma)',
     ylab="Expansion Rate")
segments(morph$origin,
         morph$expans,
         morph$extin,
         morph$expans)

#finding examples
paste(morph$genus[order(morph$expans)],morph$species[order(morph$expans )],morph$expans[order(morph$expans)])

paste(morph$origin[order(morph$expans)],morph$extin[order(morph$expans )])
abline(v=c(72))


plot(0,0,
     type='n',
     xlim=c(170,0),
     ylim=c(min(build.tci$expans,na.rm=T),
            max(build.tci$expans,na.rm=T)),
     xlab='Time (Ma)',
     ylab="Expansion Rate")
segments(build.tci$origin,
         build.tci$expans,
         build.tci$extin,
         build.tci$expans)



plot(q[2:84],d.t.mean,
     xlab="q(1)",ylab="first diff Mean in Test Complexity",pch=16,col="grey")
cor.test(q[2:84],d.t.mean)
points(q[sig.extin+1],d.t.mean[sig.extin],pch=3,lwd=2,col='red')
points(q[sig.origin+1],d.t.mean[sig.origin],pch=4,lwd=2,col='blue')