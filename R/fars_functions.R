#' Read FARS file
#'
#' This function checks if the file exists in the work directory.
#' If does, it load it to a data frame.
#'
#' @param filename The file name that has the data that should be loaded.
#'
#' @return This functions returns a data frame with file data loaded, if it exists.
#'    If the file doesn't exist, it stops the function and return a message telling that the file doesn't exist.
#'
#' @details This function uses read_csv function from readr package.
#'    In order to load the file, it should be in the working directory.
#'
#' @examples
#' \dontrun{
#' fars_read("data/accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        full_path <- paste0(system.file("extdata", package = "fars"), "/", filename)
        if(!file.exists(full_path))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(full_path, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates the name of file
#'
#' This function builds a string with the name of file that will be loaded based on the year of the required information.
#'
#' @param year The year of the required information.
#'
#' @return This function returns the file name that should be loaded.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Selects all months in each file loaded (year)
#'
#' This function creates a list of all months in the files.
#'
#' @param years a vector with all years that should be loaded
#'
#' @return This function returns a list with the column month of each file.
#'    if the year is invalid a warning will be generated to inform it.
#'
#' @details This function uses mutate and select functions from dplyr package.
#'    It also calls make_filename function in order to create a string with the filename and
#'    fars_read function to load the files in a dataframe of all years specified in the parameter.
#'    In case the year required doesn't have a correspondent file it will generate a warning.
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' fars_read_years(c(2014))
#' fars_read_years(c(2013, 2015))
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize how many accidents happend in each month by year
#'
#' This function summarizes the number of occurrencies in each month by year.
#'
#' @param years a vector with all years that should be loaded
#'
#' @return This function returns a dataframe summarizing the numer of accidents in each month of each year.
#'    if the year required is invalid a warning will be generated to inform it.
#'
#' @details This function uses bind_rows, group_by and summarize functions from dplyr package
#'    and spread function from tidyr package.
#'    It also calls fars_read_years function in order to select only month column from each year.
#'    In case the year required doesn't have a correspondent file it will generate a warning.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' fars_summarize_years(c(2014))
#' fars_summarize_years(c(2013, 2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot a graphic indicating the places of each accident that happend in the required year
#'
#' This function plots a map of a state showing the places of each accident that happend in
#' the required year.
#'
#' @param state.num The state id of the required information.
#' @param year The year of the required information.
#'
#' @return This function returns a map of the state indicating the places of each accident that happend
#'    in the required year. It also return an error message if there is no file for the year informed
#'    in the parameter year or if the state id is invalid.
#'    If there no accident has happend in the informed state in the informed year, it will print
#'    a message to inform that there is nothing to plot.
#'
#' @details This function uses filter functions from dplyr package, map function from maps package
#'    and points function from graphics package.
#'    It also calls make_filename function in order to create a string with the filename, based on the
#'    year parameter and fars_read function to load the correspondent file with required year in a
#'    dataframe.
#'    In case of the year required doesn't have a correspondent file, it will stop running and print a
#'    message informing that there is no file for the year.
#'    In case of the state ID be invalid, it will stop running and print a message informing that the
#'    ID informed in the parameter is invalid.
#'    If there is no accidents to plot for state ID informed it will print a message to inform that.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(33, 2014)
#' fars_map_state(40, 2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
