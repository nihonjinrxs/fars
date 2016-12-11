#' Read FARS Data from a single file
#'
#' @description
#' This function reads data from the FARS CSV file (compressed or uncompressed) having the filename provided
#' and returns a data frame of the data. FARS is the US National Traffic Safety Commission's
#' \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}.
#'
#' @note
#' This function is usually used with the \code{\link{make_filename}} function.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note
#' This function checks that the given filename exists in the working directory,
#' and will exit, generating an error, if the file does not exist.
#'
#' @param filename The filename of the data file to load
#'
#' @return This function returns the data frame of data loaded from the CSV.
#'
#' @examples
#' \dontrun{fars_read(make_filename(2014))}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Construct FARS filename from year
#'
#' This function constructs a filename for a FARS CSV file using the data year provided.
#' FARS is the US National Traffic Safety Commission's
#' \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}.
#'
#' @param year The numeric year value for the data for which you need a filename
#'
#' @return This function returns a character string containing the filename of the
#'    FARS data file for the provided data year.
#'
#' @examples
#' make_filename(2014)
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        system.file("extdata", sprintf("accident_%d.csv.bz2", year), package="fars")
}

#' Read FARS Data contents for several years
#'
#' @description
#' This function reads data from the FARS CSV files for the provided years,
#' and returns a list of data frames containing the month and year of each row
#' in the data for the given years.
#'
#' @details
#' FARS is the US National Traffic Safety Commission's
#' \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}.
#'
#' @seealso \code{\link{make_filename}}, \code{\link{fars_read}}
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @note
#' This function checks that the files for the requested years exist in the working directory,
#' and will generate warnings for each of those files that does not exist.
#'
#' @param years A vector or list of numeric year values for which to load data.
#'
#' @return This function returns a list of data frames containing the
#'    columns \code{MONTH} and \code{year} for the requested years.
#'
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014))}
#' \dontrun{fars_read_years(list(2013, 2014))}
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

#' Summarize FARS Data for several years
#'
#' @description
#' This function provides a summary table of fatal accident count by month from
#' FARS data for the requested years.
#'
#' @details
#' FARS is the US National Traffic Safety Commission's
#' \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}.
#'
#' @seealso \code{\link{fars_read_years}}
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @note
#' This function checks that the files for the requested years exist in the working directory,
#' and will generate warnings for each of those files that does not exist.
#'
#' @param years A vector or list of numeric year values for which to load data.
#'
#' @return This function returns a summary table with rows for each \code{MONTH}
#'    and columns for each \code{year} providing count of fatal accidents for each
#'    \code{(MONTH, year)} pair.
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013, 2014))}
#' \dontrun{fars_summarize_years(list(2013, 2014))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Draw a map of fatal accidents in a given US state and year from FARS data
#'
#' @description
#' This function generates a map of fatal accidents in the given state from
#' FARS data for the requested years.
#'
#' @details
#' FARS is the US National Traffic Safety Commission's
#' \href{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}.
#'
#' @seealso \code{\link{make_filename}}, \code{\link{fars_read}}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note
#' This function checks that the files for the requested years exist in the working directory,
#' and will exit, generating an error, if the file does not exist.  It also checks in the loaded
#' data for the validity of the state number given, and will generate an error if the state number
#' is invalid, or a message if there is no data for that state during that year.
#'
#' @param state.num The state number for the state for which to map accident data. State numbers can be found
#'    in the \href{https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812315}{FARS Analytical User's Manual},
#'    page 26: "C1/V1/D1/PC1/P1/NM1 State Number" section.
#' @param year The numeric year value for the data to map.
#'
#' @return This function does not return a value (i.e., returns \code{NULL}).  As a side effect, this
#'    function causes a scatterplot map of the fatal accidents in the given state during the given year
#'    to be plotted together with the state boundary.
#'
#' @examples
#' \dontrun{fars_map_state(22, 2014)     # Louisiana, 2014 data}
#' \dontrun{fars_map_state(11, 2015)     # District of Columbia, 2015 data}
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
