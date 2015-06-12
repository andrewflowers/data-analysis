# Carl poll fraud 

setwd("~/Documents/538editing/Carl/percentmatch")

require(Hmisc)
require(foreign)
require(xlsx)

rawDataFiles  <- list.files(path="~/Documents/538editing/Carl/percentmatch/raw_data_sets")
rawPath  <- '~/Documents/538editing/Carl/percentmatch/raw_data_sets/'
cleanPath <- '~/Documents/538editing/Carl/percentmatch/clean_data_sets/'
finalPath <- '~/Documents/538editing/Carl/percentmatch/final_data_sets/'
resultsPath  <- '~/Documents/538editing/Carl/percentmatch/percentmatch_results/'

# Convert all raw files into .csv files
# # .csv
# for(f in grep(".csv$", rawDataFiles)){
#     file.copy(from=paste0(rawPath, rawDataFiles[f]), to=paste0(cleanPath,rawDataFiles[f]))
# }
# 
# # .dta
# for(f in grep(".dta$", rawDataFiles)){
#     data <- read.dta(file=paste0(rawPath, rawDataFiles[f])) 
#     write.csv(data, file=paste0(cleanPath, sub(".dta",".csv", rawDataFiles[f])))
# }
# 
# # .xls
# # for(f in grep(".xls$", rawDataFiles)){
# #     data <- read.xlsx(paste0(rawPath, rawDataFiles[f]), sheetIndex=1) 
# #     write.csv(data, file=paste0(cleanPath, sub(".xls",".csv", rawDataFiles[f])))
# # }
# 
# # .sav
# for(f in grep(".sav$", rawDataFiles)){
#     data <- spss.get(paste0(rawPath, rawDataFiles[f])) 
#     write.csv(data, file=paste0(cleanPath, sub(".sav",".csv", rawDataFiles[f])))
# }

# Convert all raw .csv files into .dta files
cleanDataFiles  <- list.files(path=cleanPath)

for (f in grep(".csv$", cleanDataFiles)){
    data  <- read.csv(paste0(cleanPath, cleanDataFiles[f]), header=T, fileEncoding='latin1')    
    write.dta(data, paste0(finalPath, sub(".csv",".dta", cleanDataFiles[f])))
}

# Convert final data files into database of .csv files of histograms
finalDataFiles  <- list.files(path=finalPath)

for (f in grep(".dta$", finalDataFiles)){
    data  <- read.dta(file=paste0(finalPath, finalDataFiles[f]))    
    write.csv(data, paste0(resultsPath, sub(".dta",".csv", finalDataFiles[f])))
}












