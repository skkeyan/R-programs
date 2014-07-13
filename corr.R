corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        
        #num_obs <- complete(directory,1:332)
        files_full <- list.files(directory, full.names = TRUE) # Creates the list of files by appending the directory to it
        
        monitor_obs <- data.frame() # Creates an empty data frame to hold the observations in each monitor
        
        for (i in 1:332){
            monitor_obs <- rbind(monitor_obs,read.csv(files_full[i]))   # Binds the data from all the monitors, passed as a parameter
        }
         
        obs_complete <- complete.cases(monitor_obs)  # Create a boolean vector that has TRUE for only complete cases, i.e. no NA values
        monitor_obs_complete <- monitor_obs[obs_complete,] # Subset the monitor observations data frame to get only complete cases
        
        num_obs <- data.frame() # Create a data frame to hold the number of observations
        
        for (j in 1:332){
            monitor_obs_for_ids <- monitor_obs_complete[monitor_obs_complete$ID == j,]  # Creates a vector that has observations only for id
            num_obs <- rbind(num_obs,c(j,nrow(monitor_obs_for_ids))) # Data frame created with number of observations for each id
        }
                
        
        
        col_names <- c("id","nobs")                                 # Meaningful column names
        names(num_obs) <- col_names                                 # Column names fixed on the number of observations data frame
        num_obs <- num_obs[(num_obs["nobs"] > threshold),]          # Has id and observations from monitors where the no.of observations exceed the threshold
        len <- nrow(num_obs)                                        # Number of rows that exceeds the threshold
        
        corr_sulfate_nitrate <- data.frame()                        # Create a new data frame to hold the id and correlation values
        
        if(len > 0){
            for (k in 1:len){
                id_to_check <- num_obs[k,1]
                monitor_sulfate <- monitor_obs_complete[monitor_obs_complete$ID == id_to_check,"sulfate"]  # Get the sulfate values correspoding to id
                monitor_nitrate <- monitor_obs_complete[monitor_obs_complete$ID == id_to_check,"nitrate"]  # Get the nitrate values corresponding to id 
                temp_corr_vector <- c(id_to_check,cor(monitor_sulfate,monitor_nitrate))
                corr_sulfate_nitrate <- rbind(corr_sulfate_nitrate,temp_corr_vector)
                }        
        col_names <- c("id","correl")
        names(corr_sulfate_nitrate) <- col_names
        }  
        return(corr_sulfate_nitrate$correl)      
}