# Reading RDS object into dataframe
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEI <- transform(NEI, year = factor(year))
boxplot(Emissions ~ year, NEI, xlab = "Year", ylab = "Emissions")

# Plot 1
totals = aggregate(Emissions ~ year, NEI, sum)

png("plot1.png", width=480, height=480)
barplot(totals$Emissions, main = "PM2.5 Total Emissions per year", xlab = "Year", ylab="PM2.5 Total Emissions", names.arg = totals$year)
dev.off()


# Plot 2
baltimore_totals = aggregate(Emissions ~ year, data = subset(NEI, fips == "24510"), sum)

png("plot2.png", width=480, height=480)
barplot(baltimore_totals$Emissions, main = "PM2.5 Total Emissions per year in Baltimore", xlab = "Year", ylab="PM2.5 Total Emissions in Baltimore", names.arg = baltimore_totals$year)
dev.off()

# Plot 3
baltimore_by_type = aggregate(Emissions ~ year + type, data = subset(NEI, fips == "24510"), sum)

png("plot3.png", width=480, height=480)
qplot(year, Emissions, data=baltimore_by_type,	color=type, geom = c("line", "point"), method="loess")
dev.off()