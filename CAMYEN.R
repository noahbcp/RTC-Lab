#----Dependencies----
if (require(tidyverse) == FALSE) {
    install.packages("tidyverse")
    }
if (require(tools) == FALSE) {
    install.packages("tools")
    }
if (require(hablar) == FALSE) {
    install.packages("hablar")
    }
library(tidyverse)
library(tools)
library(hablar)
options(readr.show_col_types = FALSE)
#----Styles----
styles <- new.env()
styles$alert <- crayon::combine_styles("bold", "red")
styles$greenlight <- crayon::combine_styles("bold", "green")
#----Closures----
filehandler <- local({
    files <- c()
    list(
        fetch_files = function() {
            files <<- list_files_with_exts(
                file.path(
                    readline("Enter the pathname of your data folder: ")),
                exts = "csv")
            while (length(files) == 0) {
                cat(styles$alert("There are no `.csv` files in that location."))
                files <<- list_files_with_exts(
                    file.path(
                        readline("Enter the pathname of your data folder: ")),
                        exts = "csv")
            }
        },
        filepaths = function() {
            return(files)
        },
        basenames = function(silent = FALSE) {
            file_basenames <- basename(files)
            if (silent == FALSE) {
                cat(styles$greenlight("The following files were found:\n"))
                print(file_basenames)
                return(file_basenames)
            } else {
                return(file_basenames)
                }
        },
        save_files = function(x) {
            #Creates a new directory in the parent directory
            savepath <- file.path(paste0(dirname(files[batcher$file_integer()]), "/Processed"))
            dir.create(savepath, showWarnings = FALSE)
            savepath <- file.path(paste0(savepath, "/", filehandler$basenames(TRUE)[batcher$file_integer()]))
            write_csv(x, savepath)
        }
    )
})
batcher <- local({
    file_int <- c()
    batch_process <- logical()
    list(
        offer_batch_process = function() {
            if (length(filehandler$filepaths()) > 1) {
                batch_prompt <- toupper(as.character(readline(prompt = "Batch process files? (Y/N): ")))
                if (batch_prompt == "N") {
                    batch_process <<- FALSE
                    file_int <<- as.integer(readline(prompt = "Which file should be processed? (Enter the corresponding number): "))
                    cat(styles$greenlight("The following file will be processed:\n"))
                    print(basename(filehandler$filepaths()[file_int]))
                } else {
                    batch_process <<- TRUE
                    file_int <<- 1
                }
            } else {
                batch_process <<- FALSE
                file_int <<- 1
            }
        },
        batch_status = function() {
            return(batch_process)
        },
        file_integer = function() {
            return(file_int)
        },
        increment_file_integer = function(x) {
            file_int <<- file_int + 1
            return(file_int)
        }
    )
})
datahandler <- local({
    data <- list()
    pos_row <- c()
    pos_col <- c()
    n_cycles <- c()
    list(
        view_data = function(silent = FALSE) {
            data <<- read_csv(
                filehandler$filepaths()[batcher$file_integer()],
                col_names = c(as.character(1:13)), #Rename columns
                skip = 14) #Skip `.csv` header
            if (silent == FALSE) {
                View(data, filehandler$basenames(TRUE)[batcher$file_integer()])
            }
        },
        fetch_exp = function(silent = FALSE) {
            if (batcher$batch_status() == FALSE) {
                pos_row <<- 3 #Assumes data begins in row 3.
                pos_col <<- datahandler$find_triplicate()
            } else {
                pos_row <<- 3 #Assumes data begins in row 3.
                pos_col <<- datahandler$find_triplicate()
            }
            if (silent == FALSE) {
                n_cycles <<- as.numeric(readline("How many cycles?: "))
            }
        },
        find_triplicate = function() {
            #Gives a vector of the triplicate to be passed to `pos_col`
            #[2] is called as the first col is the plate coordinate.
            return(which(!is.na(data[3, 1:13]))[2]) 
        },
        row_position = function() {
            return(pos_row)
        },
        column_position = function() {
            return(pos_col)
        },
        calculate_bret = function(batch = FALSE) {
            #Loop to fetch wavelength A
            datalist_wavelength_A <- list()
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
            datalist_wavelength_B <- list()
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
            datalist <- as.data.frame(do.call(rbind, datalist))
            return(datalist)
        }
    )
})
#----Script----
filehandler$fetch_files()
filehandler$basenames()
batcher$offer_batch_process()
datahandler$view_data()
datahandler$fetch_exp()
if (batcher$batch_status() == TRUE) {
    while (batcher$file_integer() <= length(filehandler$filepaths())) {
        datahandler$view_data(silent = TRUE)
        datahandler$fetch_exp(silent = TRUE)
        filehandler$save_files(datahandler$calculate_bret())
        batcher$increment_file_integer()
    }
}  else {
    filehandler$save_files(datahandler$calculate_bret())
}
cat(styles$greenlight('Export completed!'))