# Reading RDS object into dataframe
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEI <- transform(NEI, year = factor(year))
#boxplot(Emissions ~ year, NEI, xlab = "Year", ylab = "Emissions")

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
qplot(year, Emissions, data=baltimore_by_type,	color=type, geom = c("line", "point"), method="loess") +
  labs(title="PM2.5 Emission in Baltimore by Type ",
       y="Total PM2.5 emission", x="Year")
dev.off()

# Plot 4
combustion = as.vector(SCC[grep("combustion",SCC$SCC.Level.One,ignore.case=T) & grep("coal",SCC$SCC.Level.Three,ignore.case=T),1])
NEI_comb = NEI[NEI$SCC %in% combustion,]
totals_comb = aggregate(Emissions ~ year, NEI_comb, sum)
png("plot4.png", width=480, height=480)
qplot(year, Emissions, data=totals_comb) +
  labs(title="PM2.5 Emission from coal combustion-related sources ",
       y="Total PM2.5 emission each year", x="Year")

dev.off()


