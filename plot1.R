# Plot 1
#
# Author: Kai Hirsinger (kai.hirsinger@gmail.com)
# Since: December 2017
#
# Reproduces Plot 1 for Exploratory Data Analysis Project 1.


#############
# Constants #
#############

HOUSEHOLD_POWER_CONSUMPTION_DATA <- './data/household_power_consumption.txt'
OUTPUT_FILE <- 'plot1.png'


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
    data$Date <- strptime(data$Date, '%d/%m/%Y')
    data <- data[(data$Date >= date_from) & (data$Date <= date_to),]
    data
}


#####################
# Plotting Function #
#####################

make_plot_from <- function(dataset, save_to) {
    png(filename=save_to)
    with(
        dataset,
        hist(
            Global_active_power,
            col='red',
            xlab='Global Active Power (kilowatts)',
            ylab='Frequency',
            main='Global Active Power'
        )
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
make_plot_from(dataset, save_to=OUTPUT_FILE)

