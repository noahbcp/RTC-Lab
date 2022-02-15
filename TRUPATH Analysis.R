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

## Parses MARS exports (after saving as .csv) into tibbles
data_path <- as.character(readline(prompt = 'Enter the pathname of your data in csv format: '))
format_check <- file_ext(data_path)
    if (format_check != 'csv') {
        while (format_check != 'csv') {
            if (format_check == 'xlsx') {cat(crayon::red(crayon::bold("That's an Excel file! I need a csv.")))}
            data <- readline(prompt = 'Enter the pathname of your data in csv format: ')
            format_check <- file_ext(data_path)
        }
    }
data <- read_csv(data_path, col_names = FALSE)
## Opens data so user can check well position
View(data)
pos_row <- as.integer(readline(prompt = 'Enter the row # of well A1: '))
pos_col <- as.integer(readline(prompt = 'Enter the column # of well A1: '))
cycles <- as.integer(readline(prompt = 'Enter how many cycles were run: '))
## rawdata is a list that holds n (equal to cycle) lists (i.e. list of lists). 
## Each sublist includes one timepoints lumi & fluor data
rawdata <- list(1:cycles)
## Loop to fetch and sort data
i <- 1
    while (i <= cycles){
        ## In csv, lumi & fluor at the same timepoint have one empty line between 
        ## whilst each timepoint is separated by a double line.
        lumi_raw <- data[(pos_row:(pos_row + 7)), (pos_col:(pos_col + 11))] %>%
            replace(is.na(.), 0)
        lumi_names <- colnames(lumi_raw)
        lumi_raw <- lumi_raw %>% 
            data.frame() %>%
            convert(int(all_of(lumi_names))) %>%
            unname()
        fluor_raw <- data[((pos_row + 11):((pos_row + 11) + 7)), (pos_col:(pos_col + 11))] %>%
            replace(is.na(.), 0)
        fluor_names <- colnames(fluor_raw)
        fluor_raw <- fluor_raw %>% 
            data.frame() %>%
            convert(int(all_of(fluor_names))) %>%
            unname()
        rawdata_sub <- list(lumi_raw, fluor_raw)
        rawdata[[i]] <- rawdata_sub
        pos_row <- (pos_row + 23)
        i <- (i + 1)
    }
## Loop to calculate BRET2 ratio (GFP2 [515 nm] emissions divided by RLuc8 [410 nm]) 
## at each timepoint and feed it into the list `bret2` at position i
i <- 1
bret2 <- list(1:cycles)
    while (i <= cycles){
        ## Pulls data from each sublist within the rawdata list, then unlists it 
        ## and converts it to a matrix to allow binary operators
        fluor_raw <- matrix(unlist(rawdata[[i]][1]), nrow = 8, ncol = 12)
        lumi_raw <- matrix(unlist(rawdata[[i]][2]), nrow = 8, ncol = 12)
        bret2[i] <- tibble(fluor_raw / lumi_raw)
        i <- (i + 1)
    }
## Baseline correction
## Wells are corrected to the respective well mean of a given set of timepoints
bl_prompt <- readline(prompt = 'Normalise data? (Y/N): ') %>% toupper()
    if (bl_prompt == 'Y') {
        bl_start <- as.integer(readline(prompt = 'What timepoint should baseline start?: '))
        bl_end <- as.integer(readline(prompt = 'What timepoint should baseline end?: '))
        bl_list <- bret2[bl_start:bl_end]
        i <- bl_start
        ## Creates an empty matrix `bl_summed`
        bl_summed <- matrix(nrow = 8, ncol = 12) %>% replace(is.na(.), 0)
        ## Loop to fill `bl_summed` with sums of baseline timepoints
        ## Had to do this as `mean()` required too much fudging
        while (i <= bl_end) {
            bl_summed <- bl_summed + matrix(unlist(bl_list[i]), nrow = 8, ncol = 12)
            i <- i + 1
        }
        ## `bl_summed` divided by the number of timepoints to give the mean baseline for each well
        baseline <- (bl_summed / length(bl_start:bl_end))
        ## Loop to give baseline corrected bret2
        i <- 1
        bl_bret2 <- list(1:cycles)
        while (i <= cycles){
            bl_bret2[i] <- (matrix(unlist(bret2[i]), nrow = 8, ncol = 12) / baseline) %>% tibble()
            i <- (i + 1)
        }
    }   
}
