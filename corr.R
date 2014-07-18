## corr(directory, threshold = 0)

## Returns a numeric vector of correlations between nitrate and sulfate
## observations meeting the required threshold

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files (no trailing "/")

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## calls complete.R

corr <- function(directory, threshold = 0) {
    corrs <- numeric()
    
    # find number of complete observations for each monitor
    # use default of all monitor ids
    records <- complete(directory)              
    
    for (n in records$id) {

        # if number of complete cases meets or exceeds the threshold calc correlation
        if (records$nobs[n] > threshold) {
            # generate filenames of type dir/###.csv e.g. 001.csv or 332.csv
            monitorfilename <- paste(directory, "/", sprintf("%03i", n), ".csv", sep = "")
            monitordata <- read.csv(monitorfilename)
            # really need some error trapping here to ensure file opened

            # calculate correlation (default pearson) between sulfate and nitrate
            monitorcorr <- cor(monitordata$sulfate, monitordata$nitrate, use = "complete.obs")

            # add this monitor's correlation to corrs vector
            corrs <- c(corrs, monitorcorr)
        }
    }
    #return correlation vector
    corrs
}