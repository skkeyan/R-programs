# The files are assumed to be in the working directory

# Read the National Emissions Inventory file
NEI <- readRDS("summarySCC_PM25.rds")
# Read the Source Classification code table
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 1 - Compute the emissions by year
library(sqldf)
emissions_by_year <- sqldf("select year,sum(Emissions) from NEI group by year")
names(emissions_by_year) <- c("year","emissions")
emissions_by_year[,2] = emissions_by_year[,2]/1000
with(emissions_by_year,plot(year,emissions,type="l",xlab="Year",ylab="Total Emissions (in '000 tons)",main="Total Emissions by year"))
dev.copy(png, file = "plot1.png")
dev.off()

# Answer 1 - Total emissions show a decreasing trend by year

# Plot 2 - Total Emissions in Baltimore city
library(sqldf)
emissions_by_year_baltimore <- sqldf("select year,sum(Emissions) from NEI where fips == '24510' group by year")
names(emissions_by_year_baltimore) <- c("year","emissions")
with(emissions_by_year_baltimore,plot(year,emissions,type="l",xlab="Year",ylab="Total Emissions (in tons)",main="Total Emissions by Year in Baltimore"))
dev.copy(png, file = "plot2.png")
dev.off()

# Answer 2 - Total emissions increases in 2005 and then decreases in 2008

# Plot 3 - Total Emissions in Baltimore city by type
library(sqldf)
library(ggplot2)
emissions_year_type_baltimore <- sqldf("select year,type,sum(Emissions) from NEI where fips == '24510' group by year,type")
names(emissions_year_type_baltimore) <- c("year","type","emissions")
emissions_year_type_baltimore$type <- as.factor(emissions_year_type_baltimore$type)

# Setup ggplot with data frame

g <- ggplot(emissions_year_type_baltimore,aes(year,emissions))

# Add layers to the plot

g <- g + geom_line() + facet_wrap( ~ type, ncol=2) + labs(x = "Year") + labs(y = "Emissions (in tons)") + labs(title = "Emissions in Baltimore by Type")

print(g)
dev.copy(png, file = "plot3.png")
dev.off()

# Plot 4 - Emissions from coal combustion related sources
library(sqldf)

# Prepare the required SCC data by including only coal related combustion scc codes

scc_subset <- SCC[,c(1,4)]
scc_subset[,1] <- as.character(scc_subset[,1])
scc_subset[,2] <- as.character(scc_subset[,2])
names(scc_subset) <- c("scc","sector")
scc_subset <- sqldf("select * from scc_subset where sector like '%Coal%'")

# Prepare the required emissions data along with SCC codes
emissions_by_year_with_scc <- sqldf("select SCC,year,sum(Emissions) from NEI group by SCC,year")
names(emissions_by_year_with_scc) <- c("scc","year","emissions")

# Merge the datasets to create coal emissions by year
coal_emissions_by_year <- merge(emissions_by_year_with_scc,scc_subset, by.x="scc",by.y="scc")
coal_emissions_by_year <- sqldf("select year,sum(emissions) as emissions from coal_emissions_by_year group by year")

# Develop the plot

with(coal_emissions_by_year,plot(year,emissions,type="l",xlab="Year",ylab="Total Coal Emissions (in tons)",main="Total Coal Emissions by year"))

dev.copy(png, file = "plot4.png")
dev.off()

# Plot 5 - Emissions from motor vehicle related sources in Baltimore city
library(sqldf)

# Prepare the required SCC data by including only motor vehicle related scc codes
scc_subset <- SCC[,c(1,2)]
scc_subset[,1] <- as.character(scc_subset[,1])
scc_subset[,2] <- as.character(scc_subset[,2])
names(scc_subset) <- c("scc","category")
scc_subset <- sqldf("select scc from scc_subset where category ='Onroad'")

# Prepare the required emissions data along with SCC codes
emissions_in_baltimore_with_scc <- sqldf("select SCC,year,sum(Emissions) from NEI where fips == '24510' group by SCC,year")
names(emissions_in_baltimore_with_scc) <- c("scc","year","emissions")

# Merge the datasets to create motor vehicles emissions by year in Baltimore
motor_emissions_in_baltimore <- merge(emissions_in_baltimore_with_scc,scc_subset, by.x="scc",by.y="scc")
motor_emissions_in_baltimore <- sqldf("select year,sum(emissions) as emissions from motor_emissions_in_baltimore group by year")

# Develop the plot

with(motor_emissions_in_baltimore,plot(year,emissions,type="l",xlab="Year",ylab="Total Emissions (in tons)",main="Total Motor Vehicle Emissions by year in Baltimore"))

dev.copy(png, file = "plot5.png")
dev.off()

# Plot 6 - Comparing emissions from motor vehicle related sources in Baltimore city and Los Angeles county
library(sqldf)
library(ggplot2)

# Prepare the required SCC data by including only motor vehicle related scc codes
scc_subset <- SCC[,c(1,2)]
scc_subset[,1] <- as.character(scc_subset[,1])
scc_subset[,2] <- as.character(scc_subset[,2])
names(scc_subset) <- c("scc","category")
scc_subset <- sqldf("select scc from scc_subset where category ='Onroad'")

# Prepare the required emissions data along with SCC codes
motor_emissions_in_baltimore_la <- sqldf("select SCC as scc,year,fips,sum(Emissions) as emissions from NEI where fips IN('24510','06037') group by SCC,year,fips")

# Merge the datasets to create motor vehicles emissions by year in Baltimore and LA
motor_emissions_in_baltimore_la <- merge(motor_emissions_in_baltimore_la,scc_subset, by.x="scc",by.y="scc")
motor_emissions_in_baltimore_la <- sqldf("select year,fips,sum(emissions) as emissions from motor_emissions_in_baltimore_la group by year,fips")

# motor_emissions_in_baltimore_la$fips <- as.factor(motor_emissions_in_baltimore_la$fips)

motor_emissions_in_baltimore_la$place = ifelse(motor_emissions_in_baltimore_la$fips == "06037","Los Angeles",   ifelse(motor_emissions_in_baltimore_la$fips == "24510", 'Baltimore', 'Not Known'))

# Setup the plot

qplot(year,emissions,data = motor_emissions_in_baltimore_la,color=place,geom=c("line"),xlab="Year",ylab="Total Motor Emissions (in tons)",main="Motor Emissions by Location")

dev.copy(png, file = "plot6.png")
dev.off()