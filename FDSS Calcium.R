# Dependencies

if (require(openxlsx) == FALSE) {
    install.packages('openxlsx')
}
if (require(stringr) == FALSE) {
    install.packages('stringr')
}
if (require(tools) == FALSE) {
    install.packages('tools')
}
library(openxlsx)
library(stringr)
library(tools)

# Definitions

getTxtFiles <- function(dirpath) {
    
    # Takes a dir path as arg and returns a list with all `.txt` files
    # in the directory.
    
    fl = tools::list_files_with_exts(dirpath, c('txt', 'TXT'))
    return(fl)
}

parseTabDelimited <- function(filepath) {
    
    # Takes a file path pointing to a tab delimited file as arg and 
    # returns a data frame object with file contents.
    # Assumes that all input files have 7 header rows and 96 data rows.
    
    df <- read.csv(filepath, sep = '\t', header = TRUE, skip = 7, nrows = 96)
    return(df)
}

fileint <- NULL
dir <- file.path(readline(prompt = 'Directory with `.txt` files: '))
files <- getTxtFiles(dir)
print(files)
batch <- toupper(readline(prompt = 'Process all files? (Y/N): '))
if (batch == 'N') {
    fileint <- as.integer(readline(prompt = 'Which file?: '))
    files <- files[fileint]
}
for (file in files) {
    df <- parseTabDelimited(file)
    data <- list()
    
    for (i in c(1, 4, 7, 10)) { # First row of each starting triplicate
        rows <- c(i, i + 1, i + 2)
        rows <- c(rows, rows + 12, rows + 24, rows + 36, rows + 48, rows + 60, rows + 72, rows + 84)
        data <- append(data, df[rows, ])
    }
    
    data <- as.data.frame(do.call(rbind, data))
    write.csv(data, file = file.path(dir, paste0(basename(file), '-processed.csv')))
}