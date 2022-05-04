{ 
#-----Dependencies-----
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

#-----Crayon Styles-----
alert <- crayon::combine_styles('bold', 'red')
greenlight <- crayon::combine_styles('bold', 'green')

#-----Code-----
#Select files
data_path_folder <- as.character(readline(prompt = 'Enter the pathname of your data folder: ')) %>% file.path()
data_path_files <- list_files_with_exts(data_path_folder, exts = 'csv')
while (length(data_path_files) == 0) {
    cat(alert('There are no `.csv` files in that folder.'))
    data_path_folder <- as.character(readline(prompt = 'Enter the pathname of your data folder: ')) %>% file.path()
    data_path_files <- list_files_with_exts(data_path_folder, exts = 'csv')
}
print(data_path_files)
#Offer batch Process
if (length(data_path_files) > 1) {
    file_prompt <- toupper(as.character(readline(prompt = 'Batch process files? (Y/N): ')))
    if (file_prompt == 'N') {
        batch.process <- FALSE
        file_prompt <- as.integer(readline(prompt = 'Which file should be processed? (Enter the corresponding number): '))
    } else {
        file_prompt <- 1
        batch.process <- TRUE
    }
} else {
    batch.process <- FALSE
    file_prompt <- 1
    }
#Ask user to define position of data in CSV and experiment setup
data_path <- unlist(data_path_files[1])
options(readr.show_col_types = FALSE) #Suppresses error message
data <- read_csv(data_path_files[file_prompt], col_names = FALSE)
View(data)
pos_row <- as.integer(readline(prompt = 'Row # of first well in triplicate: '))
pos_col <- as.integer(readline(prompt = 'Col # of first well in triplcate: '))
n_cycles <- as.integer(readline(prompt = 'Number of cycles: '))
drug <- as.character(readline(prompt = 'Compound (determines filename): '))
#Begin batch loop
batch.i <- 1
while (batch.i <= length(data_path_files)) {
if (batch.process == FALSE) {
    data_path <- data_path_files[file_prompt]
    data <- read_csv(data_path, col_names = FALSE)
} else {
    data_path <- data_path_files[batch.i]
    data <- read_csv(data_path, col_names = FALSE)
}
    #Loop to fetch FI data
    datalist <- list(1:n_cycles)
    i <- 1
    pos_row_A <- pos_row
    pos_col_A <- pos_col
    while (i <= n_cycles){
        fi_data <- data[(pos_row_A:(pos_row_A + 7)), (pos_col_A:(pos_col_A + 2))]
        fi_data <- as.vector(t(fi_data))
        pos_row_A <- (pos_row_A + 12)
        datalist[[i]] <- as.numeric(fi_data)
        i <- i + 1
    }
    savepath <- paste0(stringr::str_replace_all(data_path, '.csv', ''), '_', drug, '_processed.csv')
    datalist <- as.data.frame(do.call(rbind, datalist))
    write_csv(datalist, savepath)
    #Escapes batch loop if user does not opt for batch process
    if (batch.process == TRUE) {
        batch.i <- batch.i + 1
    } else {batch.i <- (length(data_path_files) + 1)} #Have to increase variable by 1 to escape loop.
}
}
