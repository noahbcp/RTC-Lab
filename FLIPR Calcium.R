{
#---Dependencies---
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
#Pick file
file_prompt <- as.integer(readline(prompt = 'Which file should be processed? (Enter the corresponding number): '))
#Ask if all quadrants should be parsed at once
batch_quadrant <- toupper(as.character(readline(prompt = 'Batch process all quadrants? (Y/N): ')))
if (batch_quadrant == 'N') {
    batch.process <- FALSE
    quadrant <- as.integer(readline(prompt = 'Which quadrant are you interested in?: '))
} else {
    batch.process <- TRUE
}
batch.i <- 1
while(batch.i <= 4) {
    if (batch.process == TRUE) {
        quadrant <- batch.i
    }
    data_path <- unlist(data_path_files[1])
    options(readr.show_col_types = FALSE) #Suppress error message
    data <- read_csv(data_path_files[file_prompt], col_names = FALSE)
    #Generate a sequence that is the start of a quadrant plus 24 up to the total well number
    #Create a list from this sequence that then includes a range of each seq number plus 6
    #Concatenate all of that into a vector that can then be used to select the data
    pos_start <- seq((2+((quadrant - 1)*6)), 385, 24)
    data_pos <- list()
    i <- 1
    while (i <= length(pos_start)) {
        data_pos[[i]] <- pos_start[i] + 0:5
        i <- i + 1
    }
    data_pos <- c(1, unlist(data_pos))
    quadrant_data <- data[data_pos,5:615]
    #Sort by cell line
    cell_line <- list()
    i <- 1
    while (i <= 4) {
        if (i %in% c(1,2)) {
            cell_line[[i]] <- t(quadrant_data[c(1, seq.default(from = 2 + (i - 1), to = 97, by = 2)[rep(rep(c(TRUE,FALSE), each = 3))]), ])
            
        } else {
            cell_line[[i]] <- t(quadrant_data[c(1, seq.default(from = 8 + (i - 3), to = 97, by = 2)[rep(rep(c(TRUE,FALSE), each = 3))]), ])
        }
        write_csv(as.data.frame(cell_line[[i]]), paste0(stringr::str_replace_all(data_path, '.csv', ''), '_quadrant_', quadrant, '_cell_line_', i, '.csv'), col_names = FALSE)
        i <- i + 1
    }
    #Escapes batch loop if user does not opt for batch process
    if (batch.process == TRUE) {
        batch.i <- batch.i + 1
    } else {batch.i <- 999}
}
rm(list = ls())
}
