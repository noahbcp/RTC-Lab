{
## Dependencies
rm(list = ls())
tidyverse_req <- require(tidyverse) 
    if (tidyverse_req == FALSE) {install.packages('tidyverse')}
    library(tidyverse)
tools_req <- require(tools) 
    if (tools_req == FALSE) {install.packages('tools')} 
    library(tools)
crayon_req <- require(crayon)
    if (crayon_req == FALSE) {install.packages('crayon')}
    library(crayon)

## Parses MARS exports (after saving as .csv) into tibbles
data <- as.character(readline(prompt = 'Enter the pathname of your data in csv format: '))
format_check <- file_ext(data)
    if (format_check != 'csv') {
        while (format_check != 'csv') {
            if (format_check == 'xlsx') {cat(red(bold("That's an Excel file! I need a csv.")))}
            data <- readline(prompt = 'Enter the pathname of your data in csv format: ')
            format_check <- file_ext(data)
        }
    }
data <- read_csv(data)
view(data)
pos_row <- as.integer(readline(prompt = 'Enter the row # of well A1: '))
pos_col <- as.integer(readline(prompt = 'Enter the column # of well A1: '))
cycles <- as.integer(readline(prompt = 'Enter how many cycles were run: '))
## rawdata is a list that holds n (equal to cycle) lists (i.e. list of lists). 
## Each sublist includes one timepoints lumi & fluor data
rawdata <- list(1:14)
## Loop to fetch and sort data
i <- 1
    while (i <= cycles){
        ## In csv, lumi & fluor at the same timepoint have one empty line between whilst each timepoint is separated by a double line.
        lumi_raw <- data[(pos_row:(pos_row + 7)), (pos_col:(pos_col + 11))] %>%
            replace(is.na(.), 0) %>%
            unname()
        fluor_raw <- data[((pos_row + 11):((pos_row + 11) + 7)), (pos_col:(pos_col + 11))] %>%
            replace(is.na(.), 0) %>%
            unname()
        rawdata_sub <- list(lumi_raw, fluor_raw)
        rawdata[[i]] <- rawdata_sub
        pos_row <- (pos_row + 23)
        i <- (i + 1)
    }
## Loop to calculate BRET2 ratio
i <- 1
    while (i <= cycles){
        bret2 <- (rawdata)
    }
}
