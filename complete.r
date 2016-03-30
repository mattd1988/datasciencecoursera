complete <- function(specdata, id = 1:332) {
        specdata <- "/Users/matthew_dorst/Desktop/Coursera/specdata/"
        nobs = numeric()
        for(i in id)
                {newRead=read.csv(paste(specdata,formatC(i,width=3,flag="0"),".csv",sep=""))
		nobs=c(nobs,sum(complete.cases(newRead)))
		}
	return(data.frame(id,nobs))
}