complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        files_full <- list.files(directory, full.names = TRUE) # Creates the list of files by appending the directory to it
        
        monitor_obs <- data.frame() # Creates an empty data frame to hold the observations in each monitor
        
        for (i in id){
            monitor_obs <- rbind(monitor_obs,read.csv(files_full[i]))   # Binds the data from all the monitors, passed as a parameter
        }
         
        obs_complete <- complete.cases(monitor_obs)  # Create a boolean vector that has TRUE for only complete cases, i.e. no NA values
        monitor_obs_complete <- monitor_obs[obs_complete,] # Subset the monitor observations data frame to get only complete cases
        
        num_obs <- data.frame() # Create a data frame to hold the number of observations
        
        for (j in id){
            monitor_obs_for_ids <- monitor_obs_complete[monitor_obs_complete$ID == j,]  # Creates a vector that has observations only for id
            num_obs <- rbind(num_obs,c(j,nrow(monitor_obs_for_ids))) # Data frame created with number of observations for each id
        }
        
        col_names <- c("id","nobs")                                 # Meaningful column names
        names(num_obs) <- col_names                                 # Column names fixed on the number of observations data frame
        return(num_obs)
}