## Function to return the best hospital in the state for the given outcome - Heart Attack, Heart Failure, Pneumonia
## Important columns - Column 2: Hospital Name, Column 7: State, Column 11 - 30 Day death rate for Heart Attack, Column 15 - No.of patients treated for Heart ## Attack, Column 17 - 30 Day death rate for Heart Failure, Column 21 - No.of patients treated for Heart Failure, Column 23 - 30 Day death rate for Pneumonia
## Column 27 - Number of patients treated for Pneumonia                  

best <- function(state,outcome){
    ## Read outcome data
    
    outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Coerce all the key columns into numeric values
    
    outcome_df[ ,11] <- as.numeric(outcome_df[ ,11])  
    outcome_df[ ,17] <- as.numeric(outcome_df[ ,17])
    outcome_df[ ,23] <- as.numeric(outcome_df[ ,23])
    
    # Get the states present in the file
    states_in_df <- unique(outcome_df[,7]) # Stores all the states present in the input file
    input_state <- state
    
    if (is.na(match(input_state,states_in_df))){
        #print("Error in best(input_state,outcome) : invalid state")
        stop("invalid state")
    }
    else {                                        # State is valid. Then check for outcome argument to find the column to be evaluated
        if (outcome == "heart attack"){
            column_to_be_evaluated <- 11
        }
        else if(outcome == "heart failure"){
            column_to_be_evaluated <- 17
        }
        else if(outcome == "pneumonia"){
            column_to_be_evaluated <- 23
        }
        else{
            #print("Error in best(input_state,outcome) : invalid outcome")
            stop("invalid outcome")
            #return()
        }
        
        filter_vector <- !is.na(outcome_df[column_to_be_evaluated]) & outcome_df[7] == input_state    # Subset vector 
        outcome_df_2 <- outcome_df[filter_vector, ]                                                   # Subset the data frame with the subset vector
        index_of_min_value <- which.min(outcome_df_2[ ,column_to_be_evaluated])            # Get the index value corresponding to minimum mortality rate
        best_hospital <- as.vector(outcome_df_2[index_of_min_value,2])                     # Use the index to find the best hospital
        print(best_hospital)
    }
}