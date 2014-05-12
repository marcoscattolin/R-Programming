corr <- function(directory, threshold = 0) {
        
        id <- 1:332
        directory <- paste(directory,"/", sep="")
        filepaths <- rep("",times=length(id)) 

        
        for (i in seq_along(id)){
                if (id[i] < 10){
                        filepaths[i] <- paste(directory,"00",id[i],".csv",sep="")
                } else if (id[i] < 100) {
                        filepaths[i] <- paste(directory,"0",id[i],".csv",sep="")
                } else {
                        filepaths[i] <- paste(directory,id[i],".csv",sep="")
                }
        }
        
        correl <- numeric(0)
        
        for (i in seq_along(id)){
                nobs <- complete(directory,id[i])
                if(nobs[1,2] > threshold){
                        data <- read.csv(filepaths[i])
                        compl <- data[complete.cases(data),]
                        tmp <- cor(compl[,"nitrate"],compl[,"sulfate"])
                        correl <- c(correl,tmp)
                } 
                
        }
        
        correl
        
}
