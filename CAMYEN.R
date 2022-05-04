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
} else {batch.process <- FALSE}
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
    #Loop to fetch wavelength A
    datalist_wavelength_A <- list(1:n_cycles)
    i <- 1
    pos_row_A <- pos_row
    pos_col_A <- pos_col
    while (i <= n_cycles){
        wavelength_A <- data[(pos_row_A:(pos_row_A + 7)), (pos_col_A:(pos_col_A + 2))]
        wavelength_A <- as.vector(t(wavelength_A))
        pos_row_A <- (pos_row_A + 23)
        datalist_wavelength_A[[i]] <- as.numeric(wavelength_A)
        i <- i + 1
    }
    #Loop to fetch wavelength B
    datalist_wavelength_B <- list(1:n_cycles)
    i <- 1
    pos_row_B <- pos_row
    pos_col_B <- pos_col
    while (i <= n_cycles){
        wavelength_B<- data[((pos_row_B + 11):((pos_row_B + 7) + 11)), (pos_col_B:(pos_col_B + 2))]
        wavelength_B <- as.vector(t(wavelength_B))
        pos_row_B <- (pos_row_B + 23)
        datalist_wavelength_B[[i]] <- as.numeric(wavelength_B)
        i <- i + 1
    }
    #Calculate BRET ratio (wavelength A / wavelength B)
    datalist <- list(1:n_cycles)
    i <- 1
    while (i <= n_cycles){
        datalist[[i]] <- datalist_wavelength_A[[i]] / datalist_wavelength_B[[i]]
        i <- i + 1
    }
    savepath <- paste0(stringr::str_replace_all(data_path, '.csv', ''), '_', drug, '_processed.csv')
    datalist <- as.data.frame(do.call(rbind, datalist))
    write_csv(datalist, savepath)
    #Escapes batch loop if user does not opt for batch process
    if (batch.process == TRUE) {
        batch.i <- batch.i + 1
    } else {batch.i <- (length(data_path_files) + 1)} #Have to increase var by length + 1 to escape batch loop
}
}
