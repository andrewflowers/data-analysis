# Set your working directory
setwd("~/Documents/538editing/Carl/percentmatch/data sets")

# Converts a SPSS data file to a Stata data file

#install.packages(c("Hmisc", "foreign", "memisc"))

require(Hmisc)
require(foreign)
require(xlsx)

fileName <- "Dataset - Pew Forum on Religion & Public Life 2011 National Survey of Mormons.sav"  # Enter file name with .sav extension

data <- spss.get(fileName, use.value.labels=TRUE) # This may take some time

write.dta(data, sub(".sav", ".dta", fileName)) # Will write .dta file with same fileName

# If you get an error that reads like below:
    # Error in read.spss(file, use.value.labels = use.value.labels, to.data.frame = to.data.frame,  : 
    #                        error reading portable-file dictionary
# Then do this:

require(memisc)

data <- as.data.set(spss.portable.file(fileName)) # This may take some time

write.dta(as.data.frame(data), sub(".sav", ".dta", fileName)) # Will write .dta file with same fileName




