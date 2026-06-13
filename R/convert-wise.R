#' Convert a Wise statement to a CSV file suitable for import into GnuCash
#'
#' You need to export a statement from the WISE website (and not download transactions).
#' This is found under your name on the top right. Use Create a statement, choose the dates
#' and currency and select CSV format and English (UK) as the Statement language. Do not select
#' Display transactions with fees separately. Then click Generate and Download the resulting
#' statement.
#'
#' @param file Character variable containing input file name (CSV file)
#' @param outfile Character variable containing output file name or NULL to
#' create a sensible name in the same directory as the input file
#' @param overwrite logical variable to control whether an existing output file
#' of the same name will be overwritten
#' @param window integer containing maximum age of transactions to import
#'
#' @examples
#' convert_wise("~/Desktop/statement_15924_SEK_2026-01-01_2026-05-29.csv", overwrite = FALSE)
#' convert_wise("~/Desktop/statement_15924_SEK_2026-01-01_2026-05-29.csv", outfile = "TMP.csv")

convert_wise <- function(file, outfile = NULL, overwrite = TRUE, window) {
    if (!file.exists(file)) {
        stop(
            "Required input WISE transactions file ",
            file,
            " not found"
        )
    }
    input <- read.table(file = file, header = TRUE, sep = ",")
    
    if(names(input)[1] != "TransferWise.ID") {
        stop("Input file ", file, " seems to be in the wrong format for WISE")
    }

    # Apply transaction window
    if (!missing(window)) output <- apply_window(input, window)

    # Date in correct order
    output <- data.frame(
        Date = as.Date(input$Date, format = "%d-%m-%Y"),
        Amount = input$Amount,
        Description = input$Merchant,
        Memo = input$Description
    )

    # Put correct payer into Description for payments
    plus <- grepl("^DEPOSIT", input$Transaction.Details.Type)
    output$Description[plus] <- input$Payer.Name[plus]

    # Fix Description for money added
    added <- grepl("^MONEY_ADDED", input$Transaction.Details.Type)
    output$Description[added] <- "Money added"
    
    # Fix transfer payments
    payee <- !is.na(input$Payee.Name) & input$Payee.Name != ""
    output$Description[payee] <- paste(input$Payee.Name[payee], input$Payment.Reference)[payee]
    
    # Deal with cashback
    accrual <- grepl(
        "^ACCRUAL|^BALANCE_CASHBACK",
        input$TransferWise.ID
    )
    output$Description[accrual] <- output$Memo[accrual]

    # Mark Transfer correctly
    internal <- grepl(
        "^BALANCE-|^CONVERSION_ORDER",
        input$TransferWise.ID
    )
    output$Description[internal] <- "Transfer"

    # Get matching account name
    output$Description[internal] <- paste(
        output$Description[internal],
        sub(
            "^Moved.* (GBP .*)$",
            "\\1",
            output$Memo[internal]
        )
    )

    # Default output filename in same directory as input file
    # and has format WISE-{CUR}-{YYYYMMDD}.csv
    if (is.null(outfile)) {
        outfile <- file.path(
            dirname(file),
            paste0(
                "WISE-",
                names(which.max(table(input$Currency))),
                "-",
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

    write.csv(output, file = outfile, row.names = FALSE)
    message("Wrote WISE output to ", outfile)
}
