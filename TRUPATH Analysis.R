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
    if (crayon == FALSE) {install.packages('crayon')}
    library(crayon)

## Parses MARS exports (after saving as .csv) into tibbles of platesize
data <- readline(prompt = 'Enter the pathname of your data in csv format: ')
format_check <- file_ext(data)
    if (format_check != 'csv') {
        while (format_check != 'csv') {
            if (format_check == 'csv')
                
            data <- readline(prompt = 'Enter the pathname of your data in csv format: ')
            format_check <- file_ext(data)
        }
    }
data <- read_csv(data) %>%
    print()
    
}
