#' Convert bank statements in specified directory
#'
#' @param path character str4ing containg directory to process
#' 
#' @export
#' @examples
#' process_statements()

process_statements <- function(path= ".") {
    # Just find qfx, csv and xlsx files
    files <- list.files(path = path, full.names = TRUE, pattern = "\\.qfx|.csv|.xlsx")
    
    # Check we git some files
    if (length(files) == 0) {
        warning("No statement files found in directory ", path)
        return()
    }
    
    df <- data.frame(Full= files, File = basename(files), Function = NA)

    # Loop through available statement types
    for (i in 1:nrow(known_statements)) {
        f <- grepl(known_statements$Pattern[i], df$File)
        df$Function[f] <- known_statements$Function[i]
    }
    df <- df[!is.na(df$Function),]
    
    # Process each file
    for (i in 1:nrow(df)) {
        message("Processing ", df$Full[i], " with ", df$Function[i], "...")
        do.call(df$Function[i], list(file = df$Full[i]))
    }

}