{
## Dependencies
rm(list = ls())
tidyverse_req <- require(tidyverse) 
if (tidyverse_req == FALSE) {install.packages('tidyverse')}
library(tidyverse)
tools_req <- require(tools) 
if (tools_req == FALSE) {install.packages('tools')} 
library(tools)
hablar_req <- require(hablar)
if(hablar_req == FALSE) {install.packages('hablar')}
library(hablar)

## Set crayon styles
alert <- crayon::combine_styles('bold', 'red')
greenlight <- crayon::combine_styles('bold', 'green')

## Select directory to look for .csv
data_path_folder <- as.character(readline(prompt = 'Enter the pathname of your data folder: ')) %>% file.path()
data_path_files <- list_files_with_exts(data_path_folder, exts = 'csv')
while (length(data_path_files) == 0) {
    cat(alert('There are no `.csv` files in that folder.'))
    data_path_folder <- as.character(readline(prompt = 'Enter the pathname of your data: ')) %>% file.path()
    data_path_files <- list_files_with_exts(data_path_folder, exts = 'csv')
}
## Select csv file to pull data from
print(data_path_files)
file_prompt <- as.integer(readline(prompt = 'Which file? (Enter the corresponding number): '))
data_path <- unlist(data_path_files[file_prompt])
## Suppress error message
options(readr.show_col_types = FALSE)
## Read csv
data <- read_csv(data_path, col_names = FALSE)
## Open data so user can check well position
View(data)
pos_row <- as.integer(readline(prompt = 'Row # of first well in triplicate: '))
pos_col <- as.integer(readline(prompt = 'Col # of first well in triplcate: '))
n_cycles <- as.integer(readline(prompt = 'Number of cycles: '))
i <- 1
datalist <- list(1:n_cycles)
while (i <= n_cycles){
    ## In csv, lumi & fluor at the same timepoint have one empty line between 
    ## whilst each timepoint is separated by two empty lines.
    bret <- data[(pos_row:(pos_row + 7)), (pos_col:(pos_col + 2))]
    bret <- as.vector(t(bret))
    pos_row <- (pos_row + 23)
    datalist[[i]] <- bret
    i <- i + 1
}
drug <- as.character(readline(prompt = 'Drug: '))
savepath <- paste0(data_path, '_', drug, '.csv')
datalist <- as.data.frame(do.call(rbind, datalist))
write_csv(datalist, savepath)
}
