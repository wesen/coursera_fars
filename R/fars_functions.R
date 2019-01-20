#' Reads a FARS accident table
#'
#' This function reads in a CSV file containing FARS accidents for a single year,
#' and returns a dplyr tibble. The function `stop`s if the file does not exist.
#'
#' @param filename The filename to read
#'
#' @return A tibble containing the FARS accidents contained in the given file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read(make_filename(2013))
#' }
fars_read <- function(filename) {
    if (!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' Returns the accident table's filename for a specific year
#'
#' @param year The requested year as an integer
#'
#' @return A string containing the filename of the CSV containing the requested year's information
#'
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' Read in the FARS accident reports for a range of years
#'
#' This function reads in the report accidents for the given years,
#' reads the corresponding files into tibbles, and keeps only the month and year of a given accident.
#' It then returns a list of the cleaned up data as a list.
#'
#' @note
#' An invalid year will generate a NULL element, and a warning message will be printed.
#'
#' @param years A list or vector of years
#'
#' @return A list of cleaned up dataframes containing the `MONTH` and the `year` of each accident
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014))
#' }
#'
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>% dplyr::select("MONTH", "year")
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' Return a monthly summary of the FARS for a range of years
#'
#' This function returns a monthly comparison of the number of accidents in each year.
#'
#' @details
#' The accident report CSV for each year is read using \link{fars_read_yars},
#' a summary of the counts is created using \link{dplyr::summarize},
#' and the data is then converted into a wide table for easier comparison using
#' \link{tidyr::spread}.
#'
#' @note
#' Invalid years will be ignored, and a warning message will be printed.
#'
#' @param years A list or vector of years to compare
#'
#' @return A dataframe of the accident counts for each month of each request year.
#' There is a column per year, and a row for each month.
#'
#' @importFrom dplyr bind_rows summarize group_by
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by_("year", "MONTH") %>%
        dplyr::summarize_(n = ~n()) %>%
        tidyr::spread_("year", "n")
}

#' Plot a map showing accident location in a state on a given year
#'
#' This function plots the location of accidents in a given state for a given year,
#' using data read from the FARS database.
#'
#' @param state.num The state to be plotted, as an integer. You can lookup the state codes
#' in the \href{https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812559}{FARS code book}.
#' @param year The year of interest
#'
#' @return None
#'
#' @note
#' This function will fail on invalid states or years, and will create an empty plot
#' if no accidents were found.
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if (!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if (nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE), xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
