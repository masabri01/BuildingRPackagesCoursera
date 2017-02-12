#' Read data from a CSV (Comma Separated Value) file
#'
#' This functions reads a flat file (text file encoded in ASCII) in the current
#'      working directory and returns it as an object of the "tbl_df" class. If
#'      the file does not exist in the current directory, this function stops
#'      execution with an error message. The progress while the file is being
#'      read is not displayed. Similarly, messages are also suppressed.
#'      It is an internal (supporting) function used by other functions in this
#'      package.
#'
#' @details This function uses the \code{read_csv()} function from the
#'          \code{readr} package, with \code{progress = FALSE} parameter.
#'          After reading the data from the csv file, the \code{tbl_df()}
#'          function from the \code{dplyr} package is called to convert the
#'          data into the tabl_df format before returing it.
#'          This function is also exported, so users will have direct access to
#'          it when they load the package conataining this function.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note The file passed as a parameter (\code{filename}) must exist in the
#'        current working directory. Otherwise, this function stops execution
#'        with an error message: "file '\code{filename}' does not exist".
#'
#' @param filename A character string containing the name of the csv file to be
#'        read, residing in the current working directory. CSV file can also be
#'        compressed as allowed by the \code{readr} package's \code{read_csv()}
#'        function.
#'
#' @return This function returns an object of class "tbl_df" (table dataframe)
#'          from the \code{dplyr} package.
#'
#' @examples
#' fars_read("accident_2014.csv.bz2") # file in current working directory
#' fars_read("abc123xyz.csv") # Error: File 'abc123xyz.csv' does not exist.
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}
#' Generate the filename
#'
#' This function generates the filename for a FARS (Fatality Analysis Reporting
#'     System) datafile for the year passed as the calling parameter.
#'     It is an internal (supporting) function used by other functions in this
#'     package.
#'
#' @details This function uses the year, input parameter, to generate the file
#'          name for the FARS data file. It converts the year into an integer
#'          and then uses the sprintf() function to format the filename by
#'          prefixing "accident_" to the year and siffixing ".csv.bz2".
#'
#' @note No other input validation is performed. It requires no other packages
#'          to be imported.
#'
#' @param year An integer, or a value coercible to an integer. If not
#'        coercible, then an invalid filename will be generated containing "NA"
#'        as the year in the file name -- accident_NA.csv.bz2 -- which doesn't
#'        exist in the FARS database.
#'
#' @return This function returns a character string representing the filename.
#'
#' @examples
#' make_filename(2016) # generates "accident_2016.csv.bz2"
#' make_filename("2016") # generates "accident_2016.csv.bz2"
#' make_filename(2016.5) # generates "accident_2016.csv.bz2"
#' make_filename("2016.5") # generates "accident_2016.csv.bz2"
#' make_filename("DEC-2016") # generates "accident_NA.csv.bz2"
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' Read FARS Data for Multiple Years
#'
#' This function takes a vector of years as its only parameter, reads the FARS
#'    files for those years and returns the MONTH and year columns as a list.
#'    It is an internal (supporting) function used by other functions in this
#'    package.
#'
#' @details This function processes each element of a vector or list of years
#'          as follows:
#'          - calls the supporting function, \code{make_filename()}, to
#'            generate a filename with the year,
#'          - calls the supporting function, \code{fars_read()}, to read the
#'            FARS data file with the generated filename,
#'          - if the file exists: creates a new column, year, selects the
#'            MONTH and year columns, and adds them to an R list.
#'          - If the file does not exit: gives a warning message, and
#'            adds nothing (NULL) to the list.
#'          - Returns the R list to the calling function.
#'
#' @note Following functions need to be imported:
#'       \code{dplyr} package: \code{select()} and \code{mutate()}
#'       Catches any execution error in a generic \code{error()} function and
#'       ignores that invalid year by returing a NULL.
#'
#' @importFrom dplyr mutate select
#'
#' @param years A vector of years
#'
#' @return This function returns an R list comprising of vectors (columns) of
#'         MONTH and year.
#'
#' @examples
#' fars_read_years(c(2014, 2015, 2016))
#' fars_read_years(2014:2016)
#' fars_read_years(list(2014, 2015, 2016))
#'
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
#' Summarize FARS data for multiple years into columns
#'
#' This function can be used to get a "tidy" data frame of FARS data for
#'    multiple years. Data is summarized by each month of each year with
#'    each year in its own column.
#'
#' @details This function processes each element of a vector or list of years
#'          as follows:
#'          - calls the supporting function, \code{fars_read_years()}, to
#'            generate a list of MONTH and year,
#'          - calls the \code{bind_rows()} function from the \code{dplyr}
#'            package to merge the list elements (MONTH and year) into a
#'            single dataframe,
#'          - calls the \code{group_by()} function from the \code{dplyr}
#'            package on the dataframe to group them by year and MONTH,
#'          - calls the \code{summarize()} function from the \code{dplyr}
#'            package on the grouped data to count the number of occurrences
#'            of fatalities for each month within each year, and finally
#'          - calls the \code{spread()} function from the \code{tidyr}
#'            package on the summarized data to create a column for each year.
#'          - Returns the tidy dataframe to the calling function.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @note Following functions need to be imported:
#'       \code{dplyr} package: \code{bind_rows()}, \code{group_by()}, and
#'                             \code{summarize()}.
#'       \code{tidyr} package: \code{spread}.
#'       No error handling is performed in this function.
#'
#' @param years A vector of years
#'
#' @return This function returns a dataframe comprising of years (provided as
#'         the input parameter) as columns with the fatalities summarized by
#'         each month.
#'
#' @examples
#' fars_summarize_years(c(2014, 2015, 2016))
#' fars_summarize_years(c(2014:2016))
#' fars_summarize_years(list(2014, 2015, 2016))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
#' Plot a map of fatalities in a State
#'
#' This function can be used to create a map of fatalities in a US State
#'    during a year as reported in the FARS database.
#'
#' @details This function takes the state number and a year as inputs and
#'          performs the following operations on them:
#'          - calls the supporting function, \code{make_filename()}, with
#'            the year to generate a list of MONTH and year,
#'          - calls the supporting function, \code{fars_read()}, to read the
#'            FARS data file with the generated filename,
#'          - If the state number (provided as the input parameter) is NOT
#'            found in the FARS data file read: stops the execution with a
#'            message, "invalid STATE number: xxx".
#'          - For a valid STATE number:
#'          - calls the \code{filter()} function from the \code{dplyr}
#'            package to extract the data for the STATE number provided,
#'          - If no data exists for the state number: prints a message,
#'            "no accidents to plot" and returns NULL.
#'          - Otherwise:
#'          - sets the invalid data for longitude (values greater than 900)
#'            and latitude(values greater than 90), if any, to NA, and
#'          - with the cleaned, mappable data:
#'          - calls the \code{map()} function of the \code{maps} package to
#'            draw the map of the State, and
#'          - calls the \code{points()} function of the base \code{graphics}
#'            package to draw points on the longitude, latitude coordinates
#'            as documented in the FARS dataset for that year in that State.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#'
#' @note Following functions need to be imported:
#'       \code{dplyr} package: \code{filter()}.
#'       \code{maps} package: \code{map()}.
#'       \code{graphics} base package: \code{points()}.
#'       Some validaiton is performed in this function as described in the
#'       "Details" section.
#'
#' @param state.num State number as coded in the \code{maps} package.
#' @param year A scalar year.
#'
#' @return This function draws a map of the State and places points on it --
#'         one point for EACH fatality that occurred in that State.
#'
#' @examples
#' fars_map_state(1, 2014) # Fatalities map for State #1 during 2014
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
#'
#' Raw data files can be found in the \code{inst/extdata/} folder of this
#' package using the following command:
#' system.file("extdata",
#'             "accident_yyyy.csv.bz2",
#'              package = "BuildPackageWeek2")
#' Where \code{yyyy} is the year in (2013, 2014, 2015).
#'
