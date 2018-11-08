which(foram.dataframe$origin > 65.5 & foram.dataframe$extin < 65.5)->survivors
paste(foram.dataframe$genus[survivors], foram.dataframe$species[survivors])

