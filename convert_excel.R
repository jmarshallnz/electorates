#' Simple script for grabbing table data out of an NZ stats excel sheet.
#' 
#' format seems to be:
#' <header junk>
#' <table header>
#' <outer header>
#' <inner header>
#' <inner table rows>
#' ...
#' <outer header>
#' <inner header>
#' <inner table rows>
#' ...
#' <footer junk>
#'
#' 1. Find inner tables (complete rows)
#' 2. Find headers and apply to each table.
#' 3. Remove headers
#' 4. Repeat from 2.
#' 

library(readxl)
library(dplyr)
library(reshape2)

sheets <- excel_sheets("2013-census-electorate-tables.xls")

all <- list()

# ignore the first sheet of contents
for (sheet_num in 2:length(sheets)) {
  
  sheet <- as.data.frame(read_excel("2013-census-electorate-tables.xls", sheet=sheet_num))
  
  
  #' remove all blank rows
  blanks <- apply(sheet, 1, function(x) { all(is.na(x)) })
  sheet <- sheet[!blanks,]
  
  #' grab the variable names - first two rows contain this
  inner_var <- sheet[1,1]
  level_var <- str_trim(unlist(str_split(sheet[2,1], ",")))
  level_var <- str_trim(gsub("(By|and)(.*)", "\\2", level_var))
  
  #' doesn't seem plausible to assign these to columns unfortunately
  
  #' save the data columns, as we'll be adding more
  data_cols <- 2:ncol(sheet)
  
  # Righto, we repeat from here...
  level <- 0
  names(sheet)[1] <- paste0("level", level)
  while (TRUE) {
  
    #' We assume that a table is a contiguous block with no
    #' missing entries which are all the same size
    table_rows <- apply(sheet, 1, function(x) { all(!is.na(x)) })
  
    tr.rle <- rle(table_rows)
  
    if (sum(tr.rle$values) == 1) {
      # only one group left - break out
      break
    }
  
    table_entries <- tr.rle$values
    table_entries[table_entries] <- 1:sum(table_entries)
    
    tr.rle$values <- table_entries
    
    table_rows <- inverse.rle(tr.rle)
    
    # pull out header of each repeated table from the sheet.
    # this will be the rows just before each table starts
    hr.rle <- rle(table_rows > 0)
    
    header_rows <- cumsum(hr.rle$lengths)[!hr.rle$value]
    header_rows <- header_rows[-length(header_rows)]
    
    # grab the table names
    table_names <- sheet[header_rows,1]
    
    # apply the table names to each of the tables beneath them
    sheet <- cbind(sheet, NA)
    sheet[table_rows > 0, ncol(sheet)] <- table_names[table_rows[table_rows>0]]
    
    # now remove the header rows and any blanks
    sheet <- sheet[-header_rows,]
    
    # and name the column
    level <- level + 1
    names(sheet)[length(names(sheet))] <- paste0("level", level)
  }
  
  # names of the columns can now be found from the top
  table_rows <- apply(sheet, 1, function(x) { all(!is.na(x)) })
  name_rows <- rle(table_rows)$length[1]
  
  # column names are then above the first header row, skipping blanks
  data_col_names <- rep(NA, length(data_cols))
  for (i in rev(seq_len(name_rows))) {
    needed_cols <- is.na(data_col_names)
    data_col_names[needed_cols] <- as.character(sheet[i,data_cols[needed_cols]])
    if (!any(is.na(data_col_names))) {
      break;
    }
  }
  
  # fill in the column names
  names(sheet)[data_cols] <- data_col_names
  
  # and kill any rows that aren't complete
  complete <- apply(sheet, 1, function(x) { all(!is.na(x)) })
  sheet <- sheet[complete,]
  
  # convert fields to numeric
  for (i in data_cols) {
    sheet[,i] <- as.numeric(sheet[,i])
  }
  
  # convert to long format
  sheet <- melt(sheet, value.name="count")
  
  # kill anything with 'total' or 'median' in it
  data_cols <- (1:ncol(sheet))[names(sheet) != "count"]
  for (i in data_cols) {
    kill_rows <- grepl("^(total|median).*", sheet[,i], ignore.case=TRUE)
    sheet <- sheet[!kill_rows,]
  }
  
  names(sheet)[names(sheet) == "variable"] <- inner_var
  
  # can infer other variables with knowledge of the table only
  # unfortuantely. We'll add them to the filename
  csv_name <- paste0(inner_var, " by ", paste(level_var, collapse=", "), ".csv")
  write.csv(sheet, file.path("csv", csv_name), row.names=FALSE)
  
  all[[sheet_num]] <- sheet
}
