##############################################################
## Coursera Data Science Specialization ######################
## Exploratory Data Analysis: Project 1 ######################
##############################################################

## Description goes here


# Load the data set, which should be contained in the wording directory:
hpc <- read.table("household_power_consumption.txt", header=TRUE, sep=c(";"))

# Check to see if there is any missing data
nans <- which(hpc=="?")

# Clean the data by dentifying the complete cases and 
# saving the relevant rows into a new variable
cc <- complete.cases(hpc)
data <- hpc[cc,]

# Verify that no incomplete data remains and remove the relevant index variables
nans <- which(data=="?")
rm(nans)
rm(cc)

# Identify and subset the data from the two relevant days
dr1get <- which(data$Date=="1/2/2007")
d1 <- data$Date[dr1get[1]]
d1data <- data[which(data$Date==d1),]

dr2get <- which(data$Date=="2/2/2007")
d2 <- data$Date[dr2get[1]]
d2data <- data[which(data$Date==d2),]

# Combine the subsets from these two days and remove the no longer useful variables
tdd <- rbind.data.frame(d1data, d2data)
rm("dr1get", "d1", "d1data", "dr2get", "d2", "d2data")

# Combine the date and time into one variable for ease of reference
tdd$myDT1 <- paste(tdd$Date, tdd$Time)
tdd$myDT2 <- strptime(tdd$myDT1, "%d/%m/%Y %H:%M:%S")
tdd$myDT3 <- as.POSIXct(strptime(tdd$myDT1, "%d/%m/%Y %H:%M:%S"))

# Remove columns produced as part of processing
tdd <- subset(tdd, select = -c(10,11) )


#################
# Creating Plot 1
#################

# Convert the variable for plot one to numeric and match the scale 
# by dividing the original variable by 477 (for some strange reason...)
tdd$Global_active_power <- as.numeric(tdd$Global_active_power)
tdd$GAPkw <- tdd$Global_active_power/477

# First, create a basic set of graphical parameter setting to return to between plots
basic.par <- par(mar = c(0, 0, 0, 0))

# Ensure that no device has been inadvertantly left open
dev.off()

# Open a device to take plot 1, with the appropriate filetype and dimensions
png(file = "plot1.png", width = 480, height = 480, units = "px")

# Create the plot, edit the x axis and close the device 
with(tdd, hist(GAPkw, col="red", mex = 0.5 ,xaxt='n', font.axis=1, font.lab=1, font.main=2, main='Global Active Power', xlim=c(0,8), xlab = "Global Active power (kilowatts)", ylab="Frequency"))
axis(side=1, at=seq(0,6,2), labels=seq(0,6,2))
dev.off()

#################
# Creating Plot 2
#################
# Open a device to take plot 2 
# First clear the graphical parameter settings
par(basic.par)

# Open a device to take plot 2, with the appropriate filetype and dimensions
png(file = "plot2.png", width = 480, height = 480, units = "px")

# Create the basic plot outline without the data first, then add the data and edit the y axis
with(tdd, plot(x=myDT3, y=GAPkw, type="n", xlab="", ylab="Global Active power (kilowatts)", yaxt="n", ylim=c(0,8)))
with(tdd, lines(myDT3, GAPkw, type="l"))
axis(side=2, at=seq(0,6,2), labels=seq(0,6,2), lwd=2)

# Close the screen device
dev.off()


#################
# Creating Plot 3
#################
# The variables are not all zeroed out. Fix this before plotting
# Although some of the variables are somewhat differently scaled
# in the example plots, it did not seem appropriate at this stage
# to perform a complex transformation on the data to get the 
# scales to match. 

tdd$SM1 <- as.numeric(tdd$Sub_metering_1)
# range(tdd$SM1) # optionally check that the variable is not zeroed
tdd$SM1 <- tdd$SM1-2

tdd$SM2 <- as.numeric(tdd$Sub_metering_2)
# range(tdd$SM2) # optionally check that the variable is not zeroed
tdd$SM2 <- tdd$SM2-2

tdd$SM3 <- as.numeric(tdd$Sub_metering_3)
# range(tdd$SM3) # optionally verify that the variable is zeroed

# Reset the graphical parameter settings, then open a device to take plot 2 
# with the appropriate filetype and dimensions
par(basic.par)
png(file = "plot3.png", width = 480, height = 480, units = "px")

with(tdd, plot(x=myDT3, y=Sub_metering_1, type = "n", xlab="", ylab="Energy sub metering",yaxt="n",  ylim=c(0,40)))
axis(side=2, at=seq(0,30,10), labels=seq(0,30,10), lwd=2)
with(tdd, lines(myDT3, SM1, type="l", col="black"))
with(tdd, lines(myDT3, SM2, type="l", col="red"))
with(tdd, lines(myDT3, SM3, type="l", col="blue"))
legend("topright", lty=1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Close the screen device
dev.off()


#################
# Creating Plot 4
#################
# First make the relevant variable numeric since it is currently a factor
tdd$Voltage <- as.numeric(tdd$Voltage)

# Also, rescale Global_reactive_power to match the example 
# in the last of the four charts on this plot since it is 
# relatively straightforward to do
tdd$Global_reactive_power <- as.numeric(tdd$Global_reactive_power)
# range(tdd$Global_reactive_power) # optionally check to see the original range
tdd$grp <- tdd$Global_reactive_power/460
# range(tdd$grp) # optionally check to see if the new range is appropriate

# Clear the graphical parameter settings then open a device to take plot 4
par(basic.par)

# Create the plot structure first, such that four individual plots 
# can be added to the same device 
png(file = "plot4.png", width = 480, height = 480, units = "px")
par(mfrow=c(2,2))

# then create the axes/labels for each individual plot, add the data and edit axes as appropriate
with(tdd, 
     
     # Top-left plot first
     {plot(x=myDT3, y=GAPkw, type="n", xlab="", ylab="Global Active power", yaxt="n", ylim=c(0,8))
        with(tdd, lines(myDT3, GAPkw, type="l"))
        axis(side=2, at=seq(0,6,2), labels=seq(0,6,2), lwd=2)

      # Then the top-right plot 
      plot(x=myDT3, y=Voltage, xlab="datetime", type="n")
        with(tdd, lines(myDT3, Voltage, type="l"))
        axis(side=2, at=seq(234,246), labels=seq(234,246), lwd=2)
        
      # Next the bottom-left plot
      plot(x=myDT3, y=Sub_metering_1, type = "n", xlab="", ylab="Energy sub metering",yaxt="n",  ylim=c(0,40))
        axis(side=2, at=seq(0,30,10), labels=seq(0,30,10), lwd=2)
        with(tdd, lines(myDT3, SM1, type="l", col="black"))
        with(tdd, lines(myDT3, SM2, type="l", col="red"))
        with(tdd, lines(myDT3, SM3, type="l", col="blue"))
        legend("topright", lty=1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty="n")

        with(tdd, plot(x=myDT3, y=grp, xlab="datetime", type="n", ylab="Global_reactive_power"))
        with(tdd, lines(myDT3, grp, type="l"))
        axis(side=2, at=seq(0,0.5, 0.1), lwd=2)
     }
     )

# Finally, close the graphics device
dev.off()

#########################################
#########################################