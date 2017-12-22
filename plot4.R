# Plot 4
#
# Author: Kai Hirsinger (kai.hirsinger@gmail.com)
# Since: December 2017
#
# Reproduces Plot 4 for Exploratory Data Analysis Project 1.


#############
# Constants #
#############

HOUSEHOLD_POWER_CONSUMPTION_DATA <- './data/household_power_consumption.txt'
OUTPUT_FILE <- 'plot4.png'


##########################
# Data Loading Functions #
##########################

load_household_power_consumption_data <- function(file, date_from, date_to) {
    field_separator <- ';'
    missing_value   <- '?'
    data <- read.table(
        file,
        header=TRUE,
        sep=field_separator,
        na.strings=missing_value
    )
    data$DateTime <- strptime(paste(data$Date, data$Time), '%d/%m/%Y %H:%M:%S')
    data$Date <- strptime(data$Date, '%d/%m/%Y')
    data <- data[(data$Date >= date_from) & (data$Date <= date_to),]
    data
}


#####################
# Plotting Function #
#####################

make_plot_from <- function(dataset, save_to) {
    png(filename=save_to)
    par(mfrow=c(2, 2))
    with(
        dataset, {
            plot(
                x=DateTime,
                y=Global_active_power,
                type='l',
                ylab='Global Active Power',
                xlab='',
            )
            plot(
                x=DateTime,
                y=Voltage,
                type='l',
                ylab='Voltage',
                xlab='datetime',
            )
            plot(
                x=DateTime,
                y=Sub_metering_1,
                col='black',
                type='l',
                xlab='',
                ylab='Energy sub metering',
            )
            lines(
                DateTime,
                Sub_metering_2,
                col='red',
            )
            lines(
                DateTime,
                Sub_metering_3,
                col='blue',
            )
            legend(
                'topright',
                lwd=1,
                bty='n',
                col=c('black', 'red', 'blue'),
                legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
            )
            plot(
                x=DateTime,
                y=Global_reactive_power,
                type='l',
                # pch=4,
                # cex=0.1,
                ylab='Global_reactive_power',
                xlab='datetime',
            )
        }
    )
    dev.off()
}


####################
# Main Script Code #
####################

dataset <- load_household_power_consumption_data(
    file=HOUSEHOLD_POWER_CONSUMPTION_DATA,
    date_from='2007-02-01',
    date_to='2007-02-02'
)
print(head(dataset))
make_plot_from(dataset, save_to=OUTPUT_FILE)

