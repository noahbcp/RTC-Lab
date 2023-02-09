library(magrittr)
library(openxlsx)
library(stringr)

GetFilepath <- function() {
    directory <- file.path(readline(prompt = "Enter Filepath: "))
    print(tools::list_files_with_exts(directory, 'xlsx', full.names = FALSE))
    file_select <- as.integer(readline(prompt = "Which File?: "))
    filepath <- file.path(tools::list_files_with_exts(directory, 'xlsx', full.names = TRUE)[file_select])
    return(filepath)
}
ParseExcel <- function(xlsx_file) {
    data <- openxlsx::read.xlsx(xlsx_file, colNames = FALSE, skipEmptyRows = FALSE, sheet = 1)
    colnames(data) <- c(as.character(1:13))
    return(data)
}
FindRows <- function(datatable, wavelength) {
    # Returns the first row that each cycle's plate representation starts at
    # Plates occupy a 12*8 grid
    # Wavelength dictates which sequence of rows is returned
    if (wavelength == "A") {
        return(which(datatable$`1` == "A", arr.ind = TRUE)[c(TRUE, FALSE)])
    }
    if (wavelength == "B") {
        return(which(datatable$`1` == "A", arr.ind = TRUE)[c(FALSE, TRUE)])
    }
}
CalculateBret <- function(datatable, a_rows, b_rows, where) {
    n_cycles <- length(a_rows)
    datalist_a <- list()
    for (i in 1:n_cycles) {
        row_position <- a_rows[i]
        if (where == 1) {
            wavelength_a <- datatable[row_position:(row_position + 1),2:13] # Rows A-B
        }
        if (where == 2) {
            wavelength_a <- datatable[(row_position + 2):(row_position + 3),2:13] # Rows C-D
        }
        if (where == 3) {
            wavelength_a <- datatable[(row_position + 4):(row_position + 5),2:13] # Rows E-F
        }
        datalist_a[[i]] <- as.numeric(as.matrix(wavelength_a))
    }
    datalist_b <- list()
    for (i in 1:n_cycles) {
        row_position <- b_rows[i]
        if (where == 1) {
            wavelength_b <- datatable[row_position:(row_position + 1),2:13] # Rows A-B
        }
        if (where == 2) {
            wavelength_b <- datatable[(row_position + 2):(row_position + 3),2:13] # Rows C-D
        }
        if (where == 3) {
            wavelength_b <- datatable[(row_position + 4):(row_position + 5),2:13] # Rows E-F
        }
        datalist_b[[i]] <- as.numeric(as.matrix(wavelength_b))
    }
    # Calculate BRET ratio
    calculated_bret <- list()
    for (i in 1:n_cycles) {
        calculated_bret[[i]] <- datalist_a[[i]] / datalist_b[[i]]
    }
    return(calculated_bret)
}
ExportExcel <- function(calculated_bret_values, savepath) {
    for_export <- do.call(rbind.data.frame, calculated_bret_values)
    colnames(for_export) <- as.character(1:(dim(for_export)[2]))
    savepath <- paste0(stringr::str_remove(savepath, ".xlsx"), "-processed.xlsx")
    return(
        openxlsx::saveWorkbook(wb = openxlsx::buildWorkbook(for_export), file = savepath, overwrite = TRUE)
    )
}
path <- GetFilepath()
data <- ParseExcel(path)
where <- as.numeric(readline(prompt = "Which duplicate?: "))
processed_data <- CalculateBret(data, FindRows(data, "A"), FindRows(data, "B"), where = where)
ExportExcel(processed_data, path)
