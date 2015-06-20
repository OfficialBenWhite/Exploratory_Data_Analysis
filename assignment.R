#
# - Exploratory Data Analysis - Course Project 2
#   
# - Data: PM2.5 Emissions Data
#    Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database 
#    is known as the National Emissions Inventory (NEI).
#
#  

library("dplyr")
library("ggplot2")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Step 1. Read data to memory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

file <- "data.zip"
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
setwd("/Users/bw/RCourse/C4W3")

# - checks if file is downloaded and downloads it
if(!file.exists(file)){   
    download.file(url,file, method="curl")
}

# - checks if file is unzipped and unzips it
if(!file.exists("Source_Classification_Code.rds")){
    unzip(file)
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Plot 1. Total emissions by year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot1 <- group_by(NEI, year)
plot1a <- summarise(plot1, total_emissions = sum(Emissions))
png(filename='plot1.png')
plot(plot1a, main = "Total emissions by year")
abline(lm(plot1a$total_emissions ~ plot1a$year))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Plot 2. Total emissions by year in Baltimore City, Maryland
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot2 <- group_by(NEI[NEI$fips == "24510",], year)
plot2a <- summarise(plot2, total_emissions = sum(Emissions))
png(filename='plot2.png')
plot(plot2a, main = "Total emissions by year in Baltimore City, Maryland")
abline(lm(plot2a$total_emissions ~ plot2a$year))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Plot 3. which of the four sources have seen decreases in emissions 
#   from 1999–2008 for Baltimore City?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot3 <- group_by(NEI[NEI$fips == "24510",], year, type)
plot3a <- summarise(plot3, total_emissions = sum(Emissions))
png(filename='plot3.png')
ggplot(plot3a, aes(x=year, y=total_emissions, color=type)) + 
    geom_point(shape=1) +
    scale_colour_hue(l=50) +       # - Use a slightly darker palette than normal
    geom_smooth(method=lm,         # - Add linear regression lines
                se=FALSE,          # - Don't add shaded confidence region
                fullrange=TRUE) +  # - Extend regression lines
    ggtitle("Which of the four sources have seen decreases in emissions\n from 1999–2008 for Baltimore City?") 
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Plot 4. Across the United States, how have emissions from coal 
#   combustion-related sources changed from 1999–2008?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# - get SCC's of any coal combustion related souces
coal_pollutant_ids <- SCC[grepl(pattern = "Fuel Comb(.*?)[Cc]oal",
                                x = SCC$EI.Se),"SCC"]
plot4  <- group_by(NEI[NEI$SCC %in% coal_pollutant_ids,], year)
plot4a <- summarise(plot4, total_coal_emissions = sum(Emissions))
png(filename='plot4.png')
ggplot(plot4a, aes(x=year, y=total_coal_emissions)) + 
    geom_point(shape=1) +
    scale_colour_hue(l=50) +       # - Use a slightly darker palette than normal
    geom_smooth(method=lm,
                se=FALSE) +        # - Don't add shaded confidence region
    ggtitle("Across the United States, how have emissions from\n coal combustion-related sources changed from 1999–2008?") 
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Plot 5. How have emissions from motor vehicle sources changed from 1999–2008
#   in Baltimore City?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# - get SCC's of any motor vehicle
vehicle_source_ids <- SCC[grepl(pattern = "[Vv]ehicles",
                                x = SCC$SCC.Level.Two),]
plot5  <- group_by(NEI[NEI$SCC %in% coal_pollutant_ids & NEI$fips == "24510",],
                   year)
plot5a <- summarise(plot5, total_vehicle_emissions = sum(Emissions))
png(filename='plot5.png')
ggplot(plot5a, aes(x=year, y=total_vehicle_emissions)) + 
    geom_point(shape=1) +
    scale_colour_hue(l=50) +       # - Use a slightly darker palette than normal
    geom_smooth(method=lm,
                se=FALSE) +        # - Don't add shaded confidence region
    ggtitle("How have emissions from motor vehicle sources changed\n from 1999–2008 in Baltimore City?") 
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# - Plot 6. Compare emissions from motor vehicle sources in Baltimore City with 
#   emissions from motor vehicle sources in Los Angeles County, California
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# - get SCC's of any motor vehicle
vehicle_source_ids <- SCC[grepl(pattern = "[Vv]ehicles",
                                x = SCC$SCC.Level.Two),]
plot6  <- group_by(NEI[NEI$SCC %in% coal_pollutant_ids & NEI$fips %in% c("24510","06037"),],
                   year, fips)
plot6a <- summarise(plot6, total_vehicle_emissions = sum(Emissions))
colnames(plot6a) <- c("year", "city", "total_vehicle_emissions")
plot6a$city <- gsub(pattern = "24510", replacement = "Baltimore", x = plot6a$city)
plot6a$city <- gsub(pattern = "06037", replacement = "LA", x = plot6a$city)
png(filename='plot6.png')
ggplot(plot6a, aes(x=year, y=total_vehicle_emissions, color=city)) + 
    geom_point(shape=1) +
    scale_colour_hue(l=50) +       
    geom_smooth(method=lm,
                se=FALSE,
                fullrange = TRUE) +        
    ggtitle("Compare emissions from motor vehicle sources\n in Baltimore City with emissions from motor vehicle sources in\n Los Angeles County") 
dev.off()
