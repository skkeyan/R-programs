pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        files_full <- list.files(directory, full.names = TRUE) # Creates the list of files by appending the directory to it
        
       all_monitors_data <- data.frame() # Creates an empty data frame to hold all the monitors data
        
        for (i in id){
            all_monitors_data <- rbind(all_monitors_data,read.csv(files_full[i]))   # Binds the data from all the monitors, passed as a parameter
        }
                
        return(mean(all_monitors_data[,pollutant],na.rm=TRUE)) # Calculates and returns the mean for the pollutant specified after removing NA values.
        #print(mean(all_monitors_data[,pollutant],na.rm=TRUE))
}