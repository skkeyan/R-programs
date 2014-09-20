# Set the working directory by using the setwd() command
# The following code assumes that the input file is present in the working directory

library(sqldf)
f <- file("household_power_consumption.txt")

power <- sqldf("select * from f",dbname = tempfile(),file.format = list(header=T,row.names=F,sep=";"))
power_subset <- power[power$Date == "1/2/2007" | power$Date == "2/2/2007" ,]
power_subset$Timestamp = strptime(paste(power_subset$Date, power_subset$Time), format = "%d/%m/%Y %H:%M:%S")

# Plot 1 - Drawing a histogram

with(power_subset,hist(Global_active_power,main="Global Active Power",col="red",xlab="Global Active Power (kilowatts)"))
dev.copy(png, file = "plot1.png")

# Plot 2
with(power_subset,plot(Timestamp,Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)"))
dev.copy(png, file = "plot2.png")

# Plot 3
with(power_subset,plot(Timestamp,Sub_metering_1,type="l",col="black",xlab="",ylab="Energy sub metering"))
with(power_subset,lines(Timestamp,Sub_metering_2,type="l",col="red"))
with(power_subset,lines(Timestamp,Sub_metering_3,type="l",col="blue"))
legend("topright", lwd = 1,col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))
dev.copy(png, file = "plot3.png")

#Plot 4
par(mfrow=c(2,2),mar=c(4,4,0.5,0.5),cex=0.64)
with(power_subset,{
	plot(Timestamp,Global_active_power,type="l",xlab="",ylab="Global Active Power")
	plot(Timestamp,Voltage,type="l",xlab="datetime",ylab="Voltage")
	plot(Timestamp,Sub_metering_1,type="l",col="black",xlab="",ylab="Energy sub metering")
	lines(Timestamp,Sub_metering_2,type="l",col="red")
	lines(Timestamp,Sub_metering_3,type="l",col="blue")
	legend("top", bty="n",lwd=1,col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),text.font=2)
	plot(Timestamp,Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")
})
dev.copy(png, file = "plot4.png")