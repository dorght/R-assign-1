## pollutantmean(directory, pollutant, id = 1:332)

## Returns the mean of the pollutant across all monitors listed
## in the 'id' vector (ignoring NA values)

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files (no trailing "/")

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which mean is calculated;
## either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

pollutantmean <- function(directory, pollutant, id = 1:332) {
    sumpollutant <- 0
    observationcount <- 0

    for (n in id) {
        # generate filenames of type dir/###.csv e.g. 001.csv or 332.csv
        monitorfilename <- paste(directory, "/", sprintf("%03i", n), ".csv", sep = "")
        monitordata <- read.csv(monitorfilename)
        # really need some error trapping here to ensure file opened
        
        # find non-NA entries for pollutant and subtotal them
        good <- !is.na(monitordata[[pollutant]])
        subtotal <- sum(monitordata[[pollutant]][good])

        # update running total of pollutant and count of observations
        sumpollutant <- sumpollutant + subtotal
        observationcount <- observationcount + sum(good)
    }

    # calculate and return mean for all observations of pollutant
    mean <- sumpollutant / observationcount
    mean
}