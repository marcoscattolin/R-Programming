best <- function(state, outcome) {
        
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
        
        col <- index[which(valid_outcomes == outcome)]
        ## Return hospital name in that state with lowest 30-day death rate
        subset <- data[data$State == state,c(2,col)]
        subset[,2] <- as.numeric(subset[,2])
        subset <- subset[complete.cases(subset),]
        
        
        top_score <- min(subset[,2])
        res <- subset[subset[,2] == top_score,1]
}
