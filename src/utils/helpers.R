DistinctCalls <- function(mcalls) {
  # Distinct 211 calls by `cat.name`, popular
  # categories will prevail.
  # Args:
  #   mcalls - data frame of all calls where calls might be in multiple categories
  #            (showing as duplicate rows)
  count.cat.code <- count(mcalls, cat.code)
  mcalls %>%
    filter(!is.na(cat.code)) %>%
    left_join(count.cat.code, by = "cat.code") %>%
    rename(cat.total = n) %>%
    arrange(id, desc(cat.total)) %>%
    distinct(id, .keep_all = TRUE)
}

#
# Cleaning referral resources
#
CleanUselessCols <- function(dat) {
  # Clean useless columns
  #   - columns with >99% observations being NA
  #   - columns with only one single unique value
  # Args:
  #   dat: a tibble
  # find whether each column are NA
  conames <- colnames(dat)
  i <- 0
  useless.cols <- sapply(conames, function(coname) {
    if (coname == "version") return(FALSE)
    co <- dat[, coname]
    uniq.var <- unique(co)
    if (nrow(uniq.var) == 1) {
      if (!is.na(uniq.var)) {
        message(sprintf("Removing \`%s\` \t \"%s\"", coname, uniq.var))
      }
      return(TRUE)
    }
    # if 99% rows are NAs, this column has no value
    length(which(is.na(co))) / nrow(co) > 0.999
    # return(FALSE)
  })
  # message("droping columns:")
  # message(str_c(conames[useless.cols], collapse = ", "))
  dat[, !useless.cols]
}

MergeDedup <- function(x, collapse='; ', sep=' *[,;\\*] *') {
  # Merge strings separated by `sep`
  # remove duplicate and empty items
  # Args:
  #   x - a vector of strings, possibly contains `collapse`
  #       indicating a list of items
  str_c(x, collapse = collapse) %>%
    str_split(sep) %>%
    map_chr(function(x) {
      # remove empty string so to clean the end output
      x <- unique(x) %>% .[. != '']
      if (length(x) > 0) {
        x <- str_c(x, collapse = collapse)
      } else {
        x <- NA
      }
      x
    })
}

FirstNonNA <- function(...) {
  # Use first non NA item in a list of vectors
  items <- list(...)
  if (length(items) < 2) {
    return(unlist(items))
  }
  first_item <- items[[1]]
  if_else(is.na(first_item), do.call(FirstNonNA, items[2:length(items)]), first_item)
}

FirstNonNAItem <- function(x) {
  x[first(which(!is.na(x)))]
}
