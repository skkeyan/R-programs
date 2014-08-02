## Function to return the hospital in the state for the given outcome and for the num provided - Heart Attack, Heart Failure, Pneumonia
## Important columns - Column 2: Hospital Name, Column 7: State, Column 11 - 30 Day death rate for Heart Attack, Column 17 - 30 Day death rate for Heart 
## Failure, Column 23 - 30 Day death rate for Pneumonia              

rankall <- function(outcome,num = "best"){
    ## Read outcome data with the relevant columns
    
    outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
    
    ## Coerce all the key columns into numeric values
    
    outcome_df[ ,3] <- as.numeric(outcome_df[ ,3])  
    outcome_df[ ,4] <- as.numeric(outcome_df[ ,4])
    outcome_df[ ,5] <- as.numeric(outcome_df[ ,5])

    ## Check that the state and outcome are valid
    
    if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
    }
  
    if(class(num) == "character"){
        if (! (num == "best" || num == "worst")){
            stop("invalid number")
        }
    }

    ## Create a new data frame based on the outcome parameter

    if(outcome == "heart attack") {
        outcome_hospitals = outcome_df[,c(1,2,3)]
    } else if(outcome == "heart failure") {
        outcome_hospitals = outcome_df[,c(1,2,4)]
    } else if(outcome == "pneumonia") {
        outcome_hospitals = outcome_df[,c(1,2,5)]
    }
    names(outcome_hospitals)[3] = "Rates"
    outcome_hospitals[ ,3] = suppressWarnings( as.numeric(outcome_hospitals[, 3]))
     
    outcome_hospitals <- outcome_hospitals[!is.na(outcome_hospitals$Rates),]   # Remove NA rows
    
    outcome_hospitals_split <- split(outcome_hospitals,outcome_hospitals$State)
    #print(outcome_hospitals_split)
    
    final_list <- lapply(outcome_hospitals_split,function(x,num){
        # Order by Death Rates and then by hospital names
        
        x = x[order(x$Rates,x$Hospital.Name),]
        #print(x)
        
        if(class(num) == "character"){
            if(num == "best"){
                #print(x$Hospital.Name[1])
                return(x$Hospital.Name[1])
            }
            else if(num == "worst"){
                #print(x$Hospital.Name[nrow(x)])
                return(x$Hospital.Name[nrow(x)])
            }
        }
        else{
                #print(x$Hospital.Name[num])
                return(x$Hospital.Name[num])
            }
        },num)
    #print(final_list)
    
    return(data.frame(hospital = unlist(final_list),state = names(final_list)))
}