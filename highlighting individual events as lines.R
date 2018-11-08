time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)->temp
time.div(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe)->temp.div
cbind(p,q)->rates
cbind(rates,matrix(rep(NA,times=6*91),ncol=6))->rates
colnames(rates)[c(3,4,5,6,7,8)]<-c('o.1','o.2','e.1','e.2','dif.o','dif.e')
plot(0,0,
     type='n',
     xlim=c(170,0),
     xlab="Time (Ma)",
     ylab="Mean TCI/Diversity",
     ylim=c(0.013,
            .3)
)

lines(temp[,1],temp[,2]/temp.div[,2],lwd=1,col='grey')
which(timeresolution.dataframe$sig.origin == 'y')->sig.origin
which(timeresolution.dataframe$sig.extin == 'y')->sig.extin

points(temp[sig.extin,1],temp[sig.extin,2]/temp.div[sig.extin,2],
       pch=16,
       cex=.75,col='red')
for(i in 1:length(sig.extin)){
  
  lines(temp[c(sig.extin[i],sig.extin[i]-1),1],
        temp[c(sig.extin[i],sig.extin[i]-1),2]/
          temp.div[c(sig.extin[i],sig.extin[i]-1),2],lwd=4,col='red')}
  for(i in 1:length(p)){
  temp[c(i,i-1),2]/
    temp.div[c(i,i-1),2]->rates[i,c('e.1','e.2')]
  }

points(temp[sig.origin,1],temp[sig.origin,2]/temp.div[sig.origin,2],
       pch=16,cex=.75,col='blue')
for(i in 1:length(sig.origin)){
  lines(temp[c(sig.origin[i],sig.origin[i]+1),1],
        temp[c(sig.origin[i],sig.origin[i]+1),2]/
          temp.div[c(sig.origin[i],sig.origin[i]+1),2],lwd=2,col='blue')}
  for(i in 1:length(p)){
  temp[c(i,i+1),2]/
    temp.div[c(i,i+1),2]->rates[i,c('o.1','o.2')]
}


geotimescale(.017,.005)
#qred,pblue
as.data.frame(rates)->rates
par(mfcol=c(1,2))
plot(temp[1:84,1],rates$o.1[1:84]-rates$o.2[1:84],col='grey',pch=16)
points(p[sig.origin],rates$o.1[sig.origin]-rates$o.2[sig.origin],col='blue',pch=16)
abline(lm({rates$o.1[1:84]-rates$o.2[1:84]}~p[1:84]),col='grey')
summary(lm({rates$o.1[1:84]-rates$o.2[1:84]}~p[1:84]))
abline(lm({rates$o.1[sig.origin]-rates$o.2[sig.origin]}~p[sig.origin]),col='blue')
summary(lm({rates$o.1[sig.origin]-rates$o.2[sig.origin]}~p[sig.origin]))
#BAD P VALUES

plot(temp[1:84,1],rates$e.1[1:84]-rates$e.2[1:84],col='grey',pch=16)
points(q[sig.extin],rates$e.1[sig.extin]-rates$e.2[sig.extin],col='red',pch=16)

abline(lm({rates$e.2[1:84]-rates$e.1[1:84]}~q[1:84]),col='grey')
summary(lm({rates$e.2[1:84]-rates$e.1[1:84]}~q[1:84]))
abline(lm({rates$e.2[sig.extin %w/o% 54]-rates$e.1[sig.extin%w/o% 54]}~q[sig.extin %w/o% 54]),col='red')
summary(lm({rates$e.2[sig.extin%w/o% 54]-rates$e.1[sig.extin%w/o% 54]}~q[sig.extin%w/o% 54]))


