
uniqsort <- function(x) {
  unique(x) %>% sort()
}

ToSankeyLinks <- function(x, nodes, group = NULL) {
  # Convert a nodes mapping 
  names(x) <- c("source", "target", "value")
  # -1 because D3 indexes are zero-based
  x$source <- match(x$source, nodes$name) - 1
  x$target <- match(x$target, nodes$name) - 1
  if (!is.null(group)) {
    x$group = group
  }
  # convert to data frame because tibble will complain
  # if `$source`` is a column name
  x %>% as.data.frame()
}

DatSankey <- function(mcalls) {
  # Prepare data for the main sankey plot
  
  popular.agency <- mcalls %>%
    count(agency.simple, sort = TRUE) %>%
    .[1:20, "agency.simple"] %>%
    unlist()
  
  mcalls %<>% mutate(agency.simple2 = ifelse(
    agency.simple %in% popular.agency,
    agency.simple,
    "Other Agency"
  ))
  mcalls %<>% mutate(agency.simple3 = ifelse(
    is.na(agency.simple2), "Unknown Agency", agency.simple2
  ))
  mcalls %<>% mutate(airs.cat3 = ifelse(is.na(airs.cat), "Unknown Service", airs.cat))
  
  count1 <- mcalls %>% count(called.before, how.heard)
  count2 <- mcalls %>% count(how.heard, airs.cat3)
  count3 <- mcalls %>% count(airs.cat3, agency.simple3)
  
  nodes <- data.frame(
    name = c(
      uniqsort(mcalls$called.before),
      uniqsort(mcalls$how.heard),
      uniqsort(mcalls$airs.cat3),
      uniqsort(mcalls$agency.simple3)
    ),
    group = c(
      rep("Called Before", uniqsort(mcalls$called.before) %>% length()),
      rep("How Did You Heard", uniqsort(mcalls$how.heard) %>% length()),
      rep("AIRS Category", uniqsort(mcalls$airs.cat3) %>% length()),
      rep("Referrals Made", uniqsort(mcalls$agency.simple3) %>% length())
    )
  )
  links <- bind_rows(
    ToSankeyLinks(count1, nodes, "a"),
    ToSankeyLinks(count2, nodes, "b"),
    ToSankeyLinks(count3, nodes, "c")
  )
  list(nodes = nodes, links = links)
}
