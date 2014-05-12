pollutantmean <- function(directory, pollutant, id = 1:332) {
        
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
        
        vals <- numeric()
        for (i in seq_along(filepaths)){
                data <- read.csv(filepaths[i])
                vals <- c(vals,data[!is.na(data[pollutant]),pollutant])
        }
        
        mean(vals)
        
}
