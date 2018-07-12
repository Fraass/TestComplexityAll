par(mfcol=c(3,1))
unique(morph$genus[which(morph$extin <= 0)])->mod.gen
unique(morph$genus[which(morph$extin <= 66 & morph$extin >= 65)])->endk.gen
which(unique(morph$genus) == mod.gen)->mod.gen.id
which(unique(morph$genus) %in% endk.gen)->endk.gen.id

plot(range,abs(end.tci),
     col='grey',pch=16,cex=1,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,720),
     main="non-Modern,non-K"
)
points(range[1:length(range) %w/o% c(mod.gen.id,endk.gen.id)],
     abs(end.tci[1:length(range) %w/o% c(mod.gen.id,endk.gen.id)]),
     col='black',pch=16,cex=1.2,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,700),
     main="non-Modern,non-K"
     )

#text(range,end.tci,unique(morph$genus))
#highlight modern & K-extin
plot(range,abs(end.tci),
     col='grey',pch=16,cex=1,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,720),
     main="Modern"
)
points(range[which(unique(morph$genus) == mod.gen)],
       abs(end.tci[which(unique(morph$genus) == mod.gen)]),
       col='blue',pch=16,cex=1.2,
       xlab='range',ylab="Change in complexity",
       ylim=c(0,12),xlim=c(0,700),
       main="Modern")
plot(range,abs(end.tci),
     col='grey',pch=16,cex=1,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,720),
     main="End K"
)
points(range[which(unique(morph$genus) %in% endk.gen)],
      abs(end.tci[which(unique(morph$genus) %in% endk.gen)]),
     col='red',pch=16,cex=1.2,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,700),
     main="End-K")






par(mfcol=c(3,1))
unique(morph$genus[which(morph$extin <= 24)])->neo.gen
unique(morph$genus[which(morph$extin >= 65.5)])->m.gen
unique(morph$genus[which(morph$extin >= 24 & morph$extin <= 65.5)])->paleo.gen
#definition of being a neo or paleo genus is based on having an extinction in that interval
#thus, a genus can be both a neogene and a paleogene genus (e.g., Dentoglobigerina)

which(unique(morph$genus) %in% neo.gen)->neo.gen.id
which(unique(morph$genus) %in% m.gen)->m.gen.id
which(unique(morph$genus) %in% paleo.gen)->paleo.gen.id

plot(range,abs(end.tci),
     col='grey',pch=16,cex=1,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,720),
     main="Mesozoic"
)
points(range[m.gen.id],
       abs(end.tci[m.gen.id]),
       col='forestgreen',pch=16,cex=1.2,
)

#text(range,end.tci,unique(morph$genus))
#highlight modern & K-extin
plot(range,abs(end.tci),
     col='grey',pch=16,cex=1,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,720),
     main="Paleogene"
)
points(range[paleo.gen.id],
       abs(end.tci[paleo.gen.id]),
       col='brown',pch=16,cex=1.2,
)
abline(v=310)
plot(range,abs(end.tci),
     col='grey',pch=16,cex=1,
     xlab='range',ylab="Change in complexity",
     ylim=c(0,12),xlim=c(0,720),
     main="Neogene"
)
points(range[neo.gen.id],
       abs(end.tci[neo.gen.id]),
       col='blue',pch=16,cex=1.2,
       )
abline(v=230)
