known_statements <- read.table(header = TRUE, sep = ",", text ="
Pattern,Function
^activity,convert_amex
^Statement.xlsx,convert_norwegian
^statement_[0-9]+,convert_wise
^transactions-20[0-9]+,convert_circlek
")

usethis::use_data(known_statements, overwrite = TRUE)
