corr <- function(specdata, threshold = 0) {
        specdata <- "/Users/matthew_dorst/Desktop/Coursera/specdata/"
        df = complete(specdata)
        ids = df[df["nobs"] > threshold, ]$id
        corrr = numeric()
        for (i in ids) {
                
                newRead = read.csv(paste(specdata, "/", formatC(i, width = 3, flag = "0"), 
                                         ".csv", sep = ""))
                dff = newRead[complete.cases(newRead), ]
                corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
        }
        return(corrr)
}