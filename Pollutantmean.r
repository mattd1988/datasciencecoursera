pollutantmean <- function(specdata, pollutant, id = 1:332) {
        specdata <- "/Users/matthew_dorst/Desktop/Coursera/specdata/"
        data=numeric()
        for(i in id)
                {newRead=read.csv(paste(specdata,formatC(i,width=3,flag="0"),".csv",sep=""))
		data=c(data,newRead[[pollutant]])
		}
        return(print(mean(data,na.rm=TRUE),digits=4))
        }