#' Convert Circle K Excel transaction file info CSV for GnuCash
#' 
#' This extracts relevant columns and writes CSV file
#'
#' @param file Character variable containing input file name (XLSXfile)
#' @param outfile Character variable containing output file name or NULL to
#' create a sensible name in the same directory as the input file
#' @param overwrite logical variable to control whether an existing output file
#' of the same name will be overwritten
#' @param window integer containing maximum age of transactions to import
#'
#' @author Tim Perkins
#' @export
#' @examples
#' convert_circlek("~/Desktop/transactions.xlsx")

convert_circlek <- function(file, outfile = NULL, overwrite = TRUE, window) {
    if (!file.exists(file)) {
        stop(
            "Required input Circle K transactions file ",
            file,
            " not found"
        )
    }
    # Read in just relevant columns in the file and skip the errors we would get converting text to date
    suppressWarnings(
        input <- readxl::read_xlsx(
            file,
            skip = 3,
            col_types = c(
                "date",
                "skip",
                "text",
                "text",
                "skip",
                "skip",
                "numeric",
                "skip"
            )
        )
    )
    # Delete any lines without a date
    del <- is.na(input$Datum)
    input <- input[!del, ]
    
    # Create the essential outputs from input
    output <- data.frame(
        Date = as.Date(input$Datum),
        Amount = input$Belopp * -1,
        Description = input$Specifikation,
        Memo = input$Ort
    )
    
    # Skip any blank memos
    output$Memo[is.na(output$Memo)] <- ""
    
    # Sort by date order
    output <- output[order(output$Date),]

    # Apply transaction window
    if (!missing(window)) output <- apply_window(output, window)

    # Default output filename in same directory as input file
    # and has format CircleK-{YYYYMMDD}.csv
    if (is.null(outfile)) {
        outfile <- file.path(
            dirname(file),
            paste0(
                "Circle-K-",
                format(Sys.Date(), "%Y%m%d"),
                ".csv"
            )
        )
    }

    # Are we allowed to overwrite?

    if (!overwrite && file.exists(outfile)) {
        stop(
            "Output file ",
            outfile,
            " already exists and overwrite is FALSE"
        )
    }

    # Write file
    write.csv(output, file = outfile, na = " ", row.names = FALSE)
    message("Wrote Circle K output to ", outfile)
}