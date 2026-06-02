#' Convert American Express qfx file for GnuCash
#' 
#' This is actually a very trivial conversion as the qfx file is nicely formatted. However,
#' the currency is currently specified as EUR for Swedish accounts so this function just changes
#' that into SEK, which allows the file to be loaded into GnuCash
#'
#' @param file Character variable containing input file in qfx format
#' @param outfile Character variable containing output file name or NULL to
#' create a sensible name in the same directory as the input file
#' @param overwrite logical variable to control whether an existing output file
#' of the same name will be overwritten
#'
#' @export
#' @examples 
#' convert_amex("~/Desktop/activity.qfx")
#' convert_amex("activity.qfx", outfile = "NEW.qfx", overwrite = FALSE)
#' 
convert_amex <- function(file, outfile = NULL, overwrite = TRUE) {
    if (!file.exists(file)) {
        stop(
            "Required input AmEx transactions file ",
            file,
            " not found"
        )
    }
    suppressWarnings(input <- readLines(file))
    
    # Check the contents a bit
    if(!grepl("<?xml version=\"1.0\" standalone=\"no\"?>", input[1], fixed = TRUE)) {
        stop("Input file ", file, " seems to be in the wrong format for AmEx")
    }
    output <- sub("EUR", "SEK", input)

    # Default output filename in same directory as input file
    # and has format AmEx-{YYYYMMDD}.qfx
    if (is.null(outfile)) {
        outfile <- file.path(
            dirname(file),
            paste0(
                "AmEx-",
                format(Sys.Date(), "%Y%m%d"),
                ".qfx"
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
    writeLines(output, outfile)
    message("Wrote AmeEx output to ", outfile)
}