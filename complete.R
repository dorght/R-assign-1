## complete(directory, id = 1:332)

## Returns a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files (no trailing "/")

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

complete <- function(directory, id = 1:332) {
    #initialize records with names
    records <- data.frame(id = integer(), nobs = integer())

    for (n in id) {
        # generate filenames of type dir/###.csv e.g. 001.csv or 332.csv
        monitorfilename <- paste(directory, "/", sprintf("%03i", n), ".csv", sep = "")
        monitordata <- read.csv(monitorfilename)
        # really need some error trapping here to ensure file opened
        
        # find each complete case of data and count them
        good <- complete.cases(monitordata)
        ngood <- sum(good)

        # add new row for this monitor to records data frame
        newrow <- data.frame(id = n, nobs = ngood)
        records <- rbind(records, newrow)
    }

    # return the records data frame
    records
}