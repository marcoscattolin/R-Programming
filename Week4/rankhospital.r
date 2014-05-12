rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- data.frame(read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available"))
        
        
        ## Check that state and outcome are valid
        if(sum(data$State == state) == 0){
                stop("invalid state")
        }
        
        valid_outcomes = c("heart attack","heart failure","pneumonia")
        index <- c(11,17,23)
        
        if(sum(valid_outcomes == outcome) == 0){
                stop("invalid outcome")
        }
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        col <- index[which(valid_outcomes == outcome)]
        ## Return hospital name in that state with lowest 30-day death rate
        subset <- data[data$State == state,c(2,col)]
        subset[,2] <- as.numeric(subset[,2])
        subset <- subset[complete.cases(subset),]
        subset <- subset[order(subset[,1]),]
        subset <- cbind(subset, rank(subset[,2], ties.method = "first"))
        
        if(num == "best"){
                num = min(subset[,3])
        }
        
        if(num == "worst"){
                num = max(subset[,3])
        }
        
        if(num > max(subset[,3])){
                res <- NA
        } 
        else {
                res <- subset[subset[,3] == num,1]
        }
        
        res       
}
