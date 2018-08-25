#' Read csv file
#'
#' \code{fars_read} reads in the csv file containing the Fatality Analysis Reporting System data for a particular year
#' specified by the filename argument and returns it as a tibble.
#' Make sure there is a corresponding file for the year of interest
#'
#' @param filename path to csv file to be read.
#'
#' @return tibble with the FARS data for a particular year.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        filename <- system.file("extdata", filename, package = "BuildingRPackagesAssignment")
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make filename out of year specification
#'
#' \code{make_filename} Given a year, creates the approriate name for the file that contains the data for that year
#'
#' @param year An integer or a string that can be coerced to integer specifying the year of interest.
#'
#' @return A string giving the name of the file that contains the year's data.
#'
#' @examples
#' make_filename(2013)
#' make_filename("2013")
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read data for multiple years
#'
#' \code{fars_read_years} Reads in the data files for the years specified by the input. If any of the input years do not
#' have a corresponding data file, stops with error.
#'
#' @param years Vector of years for which we want to read data.
#'
#' @return List of tibbles with two columns containing the month and year.
#'
#' @examples
#' fars_read_years(c(2013, 2014))
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~ year) %>%
                                dplyr::select_(.dots = c("MONTH", "year"))
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize observations
#'
#' \code{fars_summarize_years} Summarizes the number of observations by month for the years specified in input
#'
#' @inheritParams  fars_read_years
#'
#' @return A summary tibble which gives the no. of observations in each month for every year.
#'
#' @examples
#' fars_summarize_years(c(2013, 2014))
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~ year, ~ MONTH) %>%
                dplyr::summarize_(n = ~ n()) %>%
                tidyr::spread_(key_col = 'year', value_col = 'n')
}

#' Plot accidents
#'
#' \code{fars_map_state} Plots the coordinates of the accidents in a particular state for a specific year
#' If the numeric code for the state does not exist or there isn't a corresponding data file for the year of interest,
#' will not work
#'
#' @param state.num Numeric code of state of interest
#' @param year Year of interest
#'
#' @return A plot of the accidnts in a state for a given year.
#'
#' @examples
#' fars_map_state(1, 2013)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
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
