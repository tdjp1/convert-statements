#' Filter out transactions older than window 
#' 
#' Remove any transactions older than window days from the input data frame
#'
#' @param input data frame with transactions for export
#' @param window integer variable containing maximum age of transaction in days
#'
#' @returns output data frame with filtered transactions
#'
#' @export
#' @examples
#' apply_window(input, 90)

apply_window <- function (input, window) {
    # Deal with transaction window
    output <- input
    oldest <- min(output$Date)
    window <- as.integer(window)
    limit <- Sys.Date() - window
    if (limit >= oldest) {
        output <- output[output$Date >= limit,]
        message("Ignoring transactions older than ", limit)
    }

    return(output)
}
