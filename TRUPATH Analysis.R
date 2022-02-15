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
cat(greenlight('BRET2 ratios calculation complete.'))
## Baseline correction
## Wells are corrected to the respective well mean of a given set of timepoints
bl_prompt <- readline(prompt = 'Normalise data to a baseline? (Y/N): ') %>% toupper()
    if (bl_prompt == 'Y') {
        bl_start <- as.integer(readline(prompt = 'What timepoint should baseline start?: '))
            ## Failsafe if user inputs 0 as the baseline timepoint
            if (bl_start == 0) {bl_start <- 1}
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
        cat(greenlight('Baseline correction complete.'))
    }
## Allow user to review data prior to saving
view_prompt <- readline(prompt = 'Do you want to review your data? (Y/N): ') %>% toupper()
    if (view_prompt == 'Y') {
        if (bl_prompt == 'Y') {
            bl_prompt2 <- readline(prompt = 'Do you want to review baseline corrected? (Y/N): ') %>% toupper()
        }
        warn_prompt <- 'N'
        if (cycles >= 20) {
            cat(alert('This will print a lot of data!'))
            warn_prompt <- readline(prompt =  'Continue? (Y/N): ') %>% toupper()
        } else warn_prompt <- 'Y'
        if (warn_prompt == 'Y' & bl_prompt2 == 'Y') {
            i <- 1
            ## warn prompt changed to 'N' to prevent non-normalised data being printed too
            warn_prompt <- 'N'
            while (i <= cycles) {
                View(bl_bret2[[i]][1:8, 1:12])
                i <- (i + 1)
            }
        }
        if (warn_prompt == 'Y') {
            i <- 1
            while (i <= cycles) {
                View(bret2[[i]][1:8, 1:12])
                i <- (i + 1)
            }
        }
    }
## Export
save_prompt <- readline(prompt = 'Do you want to export your data? (Y/N): ') %>% toupper()
    if (save_prompt == 'Y') {
        dest_path <- 
        save_prompt2 <- readline(prompt = 'Export to same folder as input? (Y/N): ') %>% toupper()
        if (save_prompt2 == 'N') {
            dest_path <- readline(prompt = 'Enter pathname of destination folder: ')
        } else dest_path <- data_path_folder
        save_path <- str_glue(str_remove(data_path, '.csv'), '_bret2.csv')
        write.csv(bret2, file = save_path)
        if (bl_prompt == 'Y') {
            bl_save_path <- str_glue(str_remove(data_path, '.csv'), '_baseline_corrected_bret2.csv')
            write.csv(bl_bret2, file = bl_save_path)
        }
        save_check <- list_files_with_exts(dest_path, exts = 'csv')
        if (bl_prompt == 'Y') {
            if (bl_save_path %in% save_check && save_path %in% save_check) {
                cat(greenlight('Export succesful!'))
            } else
                cat(alert('Export failed.'))
            
        } else
            if (save_path %in% save_check) {
                cat(greenlight('Export succesful!'))
            } else
                cat(alert('Export failed.'))
    }
}
