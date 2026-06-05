# Convert Bank Norwegian Excel file to csv
# Tim perkins
# 28 May 2026
#
#' Convert Bank Norwegian Excel transaction file to CSV
#'
#' This creates an output with same same and directory as input file but
#' with CSV extension and overwrites any existing file of the same name
#'
#' @param file xlsx format input file to process
#' @param outfile Character variable containing Output file name or NULL to
#' create a sensible name in the same directory as the input file
#' @param overwrite logical variable to control whether an existing output file
#' of the same name will be overwritten
#' @param window integer containing maximum age of transactions to import
#'
#' @export
#' @examples
#' convert_norwegian("~/Desktop/Statements.xlsx", window = 30)

convert_norwegian <- function(file, outfile = NULL, overwrite = TRUE, window) {
    library(readxl)
    if (!file.exists(file)) {
        stop(
            "Required input Norwegian transactions file ",
            file,
            " not found"
        )
    }
    input <- readxl::read_xlsx(file)
 
    # Error checking
    if (names(input)[1] != "TransactionDate") {
        stop("Input file ", file, " seems to be in the wrong format for Norwegian")
    }
    
    # Skip just reserved transactions
    input <- input[input$Type != "Reserverat",]

    # Create the essential outputs from input
    output <- data.frame(
        Date = as.Date(input$TransactionDate),
        Amount = input$Amount,
        Description = input$Text,
        Memo = paste(input$`Merchant Category`, input$`Merchant Area`)
    )
    
    # Update memo for payments
    payments <- input$Type == "Betalning"
    output$Memo[payments] <- "Betalning"
    

    # Apply transaction window
    if (!missing(window)) output <- apply_window(output, window)

    # Output file name
    # Default output filename in same directory as input file
    # and has format Bank_Norwegian-{YYYYMMDD}.csv
    if (is.null(outfile)) {
        outfile <- file.path(
            dirname(file),
            paste0(
                "Bank-Norwegian-",
                format(Sys.Date(), "%Y%m%d"),
                ".csv"
            )
        )
    }

    # Are we allowed to overwrite?
    {if (!overwrite && file.exists(outfile)) {
        stop(
            "Output file ",
            outfile,
            " already exists and overwrite is FALSE"
        )
    }}

    # Write file
    write.csv(output, file = outfile, na = " ", row.names = FALSE)
    message("Wrote Bank Norwegian output to ", outfile)
}
