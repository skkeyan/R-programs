# Manipulating weather data:
weather <- read.csv("weather.csv") # Has weather details for all locations
weather$idfield <- paste(weather$Place,weather$joinfield,sep=":")
library("sqldf")
weather_new <- sqldf("select min(rowid) row_names,idfield,* from weather group by idfield",row.names = TRUE)
weather <- weather_new[,c(-2,-3,-4,-10)] # Removing redundant columns


# Read the full training set
training <- read.csv("CA_Training.csv")
training <- training[,c(1,7,16,25,31,33,49)]   # Taking only the relevant columns

# Creating the join field in training set
training$CRSDepTime <- as.integer(training$CRSDepTime / 100)
training$temp <- paste(training$Origin,training$FlightDate,sep=":")
training$joinfield <- paste(training$temp,training$CRSDepTime,sep=":")
training <- training[,c(9,3,4,6)]# Has only relevant fields - JoinField, Origin, Destination and Departure Delay

# Creating the complete Delay (Training set) and Weather data
delayweather <- merge(training,weather,by.x="joinfield",by.y="idfield")
names(delayweather) <- c("id","origin","destination","delaymts","temperature","dewpoint","humidity","visibility","conditions")

delayweather$humidity <- as.numeric(delayweather$humidity)
write.csv(delayweather,file="delayweather.csv",row.names=F)

# Build the Model
fol <- formula(delaymts ~ origin + destination + temperature + dewpoint + humidity + visibility + conditions)
model <- tree(fol,method="class",data=delayweather)

# Load Test Data
test <- read.csv("CA_Public_Test.csv")
test <- test[,c(1,7,16,25,31)]

# Creating the join field in test set
test$CRSDepTime <- as.integer(test$CRSDepTime / 100)
test$temp <- paste(test$Origin,test$FlightDate,sep=":")
test$joinfield <- paste(test$temp,test$CRSDepTime,sep=":")
test <- test[,c(1,7,3,4)]

# Creating the complete Test set by combining with Weather data

delayweather_test <- merge(test,weather,by.x="joinfield",by.y="idfield")
names(delayweather_test) <- c("id","obsid","origin","destination","temperature","dewpoint","humidity","visibility","conditions")
delayweather_test$humidity <- as.numeric(delayweather_test$humidity)


# If Linear Regression is required:

#linear_model <- lm(formula = delaymts ~ origin + destination + temperature + dewpoint + humidity + visibility, data = delayweather)
linear_model <- lm(formula = delaymts ~ dewpoint + humidity + visibility, data = delayweather)
predictions <- predict.lm(linear_model, newdata = delayweather_test)

