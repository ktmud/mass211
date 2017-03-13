# ======= Define cats functions ================

ReadTaxonomy <- function(file = "data/Taxonomy.htm") {
  tax <- read_html(file)
  tax.name <- xml_find_all(tax, "//table//td[position()=1]") %>% xml_text()
  tax.desc <- xml_find_all(tax, "//table//td[position()=2]") %>% xml_text()
  tax.code <- xml_find_all(tax, "//table//td[position()=3]") %>% xml_text()
  data.frame(code = tax.code, tname = tax.name, desc = tax.desc,
             stringsAsFactors = FALSE)
}

TaxonomyMerge <- function(tax, ...) {
  lapply(list(...), function(tax2) {
    tax <<- rbind(tax, tax2[-which(tax2$code %in% tax$code), ])
  })
  tax
}

TaxonomyLevels <- function(code, missing.as.na = TRUE, as.df = TRUE) {
  # convert a taxonomy code to a list of code levels
  # Args:
  #   code: Taxonomy code in the format of "BV-8900.9300"
  #   missing.as.na: whether a missing level is treated as NA,
  #      otherwise we will complete it with code of the previous level
  # Return: a character vector of all levels of this code.
  #         e.g. c("B", "BV", "BV-8900", "BV-8900.9300")
  tlevels <- str_match(code, "^([A-Z])([A-Z])?(-[0-9]+)?(\\.[0-9]+)?(-[0-9]+)?") %>%
    apply(1, function(parts) {
      parts <- parts[-1]  # remove the first matching group
      full.length <- length(parts)
      ok.length <- length(na.omit(parts))
      cats <- sapply(1:ok.length, function(i) {
        paste(parts[1:i], collapse = "")
      })
      # fill lower levels with existing upper levels
      if (ok.length < full.length) {
        if (missing.as.na) {
          cats[(ok.length + 1):full.length] <- NA
        } else {
          cats[(ok.length + 1):full.length] <-
            rep(last(cats), full.length - ok.length)
        }
      }
      cats
    }) %>% t()
  if (as.df) {
    tlevels %<>% as_tibble()
    names(tlevels) <- str_c("L",  1:5, sep = "")
  }
  tlevels
}

ReadAllTax <- function() {
  # Taxonomy searched with "a/e/i/o/u", from
  # https://211longisland.communityos.org/zf/taxonomy/getftitem
  t1 <- ReadTaxonomy()
  t2 <- ReadTaxonomy("data/Taxonomy2.htm")
  t3 <- ReadTaxonomy("data/Taxonomy3.htm")
  t4 <- ReadTaxonomy("data/Taxonomy4.htm")
  t5 <- ReadTaxonomy("data/Taxonomy5.htm")
  t <- TaxonomyMerge(t1, t2, t3, t4, t5)
  t$pathString <- TaxonomyLevels(t$code, as.df = FALSE) %>%
    apply(1, function(x) {
      str_c(na.omit(x), collapse = "/")
    })
  t$pathString <- str_c("Taxonomy", t$pathString, sep = "/")
  t
}

CountCats <- function(m, ...) {
  # Split taxonomy code into 5 levels,
  # and do the counting for each level 
  # Args:
  #   m - the Mass 211 data set
  # Return: a tibble with cat name, desc and call count
  tlevels <- TaxonomyLevels(m$cat.code)
  # append cate name and description
  cat.count <- tlevels %>%
    unlist() %>%
    na.omit() %>%
    table() %>%
    as_tibble()
  names(cat.count) <- c("code", "n")
  cat.count %>% left_join(full.cats, by = "code")
}


# ==== Generate Cats Info ==============

if (!exists("full.cats")) {
  # All categories as scraped from Taxonomy site
  full.cats <- ReadAllTax()
}

cats.tree <- CountCats(mcalls)
cats.tree$code <- NULL  # because it is in the path already
cats.tree <- as.Node(cats.tree)

