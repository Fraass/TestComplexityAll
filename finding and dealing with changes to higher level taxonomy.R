morph$family->family.ID
range.line.plot(test.complex.index,
                "Test Complexity Index",
                family.ID,
                morph$origin,
                morph$extin
)
lines(time.mean(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe),lwd=2)
lines(time.median(test.complex.index,morph$origin,morph$extin,timeresolution.dataframe),lwd=2,col="red")

#old higher-level tax
if(temp.ID == "Globoquadrinidae"){col.code[i]<-8}
if(temp.ID == "Globuligerinidae"){col.code[i]<-11}
if(temp.ID == "Paraticinellinae"){col.code[i]<-33}
