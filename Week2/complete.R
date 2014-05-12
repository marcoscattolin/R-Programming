complete <- function(directory, id = 1:332) {
        
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
        
                
        result <- matrix(rep(0, 2*length(id)), ncol=2)
        colnames(result) <- c("id","nobs")
         
        for (i in seq_along(id)){
                data <- read.csv(filepaths[i])
                obs <- sum(complete.cases(data))
                result[i,"id"] <- id[i]
                result[i,"nobs"] <- obs
        }
         
         data.frame(result)
        
}

comp <- complete("specdata", 1)
