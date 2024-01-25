# Function to merge two longitudinal variables
Merge_Vble: Function to search in the Body-Weight dataset and match 
[id-animal plus BW-date] with [id-animal plus bleed-date]. 
A window in weeks is required.
If no BW-date (window = +/- 2 weeks) is found insert NA in data2$bw,
otherwise insert closest BW-date body weight in data2$bw.
merge_Vble <- function(data1,
                          data2,
                          id,
                          date,
                          threshold = c("weeks" = Inf),
                          vars = NULL,
                          where = "both",
                          suffixes = c(".1", ".2"),
                          clean_vars = TRUE) {

  # prep data1 and data2 -------------------------------------------------------
  # convert data1 and data2 to data.tables
  dt1 <- data.table::as.data.table(data1)
  dt2 <- data.table::as.data.table(data2)
  
  # manually add suffixes to identify which column came from which dataframe
  names(dt1) <- paste0(names(dt1), suffixes[1])
  names(dt2) <- paste0(names(dt2), suffixes[2])
  
  # create id and date vars if length id = 1 or 2 -----------------------------
  # do this for consistency of function regardless of date or id input
  # create corresponding id vars
  if (length(id) == 1) {
    id1 <- paste0(id, suffixes[1])
    id2 <- paste0(id, suffixes[2])
  } else if (length(id) == 2) {
    id1 <- paste0(id[1], suffixes[1])
    id2 <- paste0(id[2], suffixes[2])
  } else {
    stop("id must be length 1 or 2")
  }
  
  # create corresponding date vars
  if (length(date) == 1) {
    date1 <- paste0(date, suffixes[1])
    date2 <- paste0(date, suffixes[2])
  } else if (length(date) == 2) {
    date1 <- paste0(date[1], suffixes[1])
    date2 <- paste0(date[2], suffixes[2])
  } else {
    stop("date must be length 1 or 2")
  }
  
  # if vars specified ----------------------------------------------------------
  # if vars are specified ensure complete cases for those vars
  if (is.null(vars)) {
    dt2 <- dt2
  } else if (!is.null(vars)) {
    vars_suf <- paste0(vars, suffixes[2])
    complete_cases <- stats::complete.cases(dt2[, vars_suf, with = FALSE])
    ndt2 <- dt2[complete_cases]
  }
  
  # merge and create dif -------------------------------------------------------
  dtm <- merge(dt1,
               dt2,
               by.x = id1,
               by.y = id2,
               all.x = TRUE,
               allow.cartesian = TRUE)
  # dif tells use time difference and direction
  dtm$dif <- difftime(dtm[[date2]], dtm[[date1]], units = names(threshold))
  
  # set dif_ef (effective dif) depending upon where we are looking for nearest
  # measurement
  if (where == "both") {
    dtm$dif_ef <- abs(dtm$dif)
  } else if (where == "before") {
    dtm$dif_ef <- dtm$dif * -1
  } else if (where == "after") {
    dtm$dif_ef <- dtm$dif
  } else {
    stop("where must be both, before, or after")
  }
  
  data.table::setorderv(dtm, cols = c(id1, "dif_ef"))
  
  dtm <- base::unique(dtm, by = c(eval(id1), eval(date1)))
  
  # threshold
  get_class <- function(col) class(dtm[[col]])
  dt2_cols <- names(dtm)[grepl(paste0(suffixes[2], "$"), names(dtm))]
  dt2_cols_not_date <- dt2_cols[sapply(dt2_cols, get_class) != "Date"]
  set_na_row <- which(dtm$dif_ef > threshold[[1]])
  data.table::set(dtm,
                  i = eval(set_na_row),
                  j = dt2_cols_not_date,
                  value = NA
  )
  
  # clean vars -----------------------------------------------------------------
  if (isTRUE(clean_vars)) {
    suf1_regx <- paste0(suffixes[1], "$")
    suf2_regx <- paste0(suffixes[2], "$")
    col1 <- names(dtm)[grepl(suf1_regx, names(dtm))]
    col2 <- names(dtm)[grepl(suf2_regx, names(dtm))]
    
    col1_clean <- gsub(suf1_regx, replacement = "", x = col1)
    col2_clean <- gsub(suf2_regx, replacement = "", x = col2)
    
    if (length(date) == 1) {
      col1_clean <- gsub(date, replacement = date1, x = col1_clean)
      col2_clean <- gsub(date, replacement = date2, x = col2_clean)
    } else if (length(date) == 2) {
      col1_clean <- gsub(date[1], replacement = date1, x = col1_clean)
      col2_clean <- gsub(date[2], replacement = date2, x = col2_clean)
    }
    
    col2_var_keep <- col2[!(col2_clean %in% col1_clean)]
    col2_clean_var_keep <- col2_clean[!(col2_clean %in% col1_clean)]
    var_keep <- c(col1, col2_var_keep, "dif")
    
    dtm <- dtm[, .SD, .SDcols = var_keep]
    names(dtm) <- c(col1_clean,
                    col2_clean_var_keep,
                    paste0("dif_", names(threshold)))
  }
  
  # return as.data.frame-------------------------------------------------------
  as.data.frame(dtm)
}
