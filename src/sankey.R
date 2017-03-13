library(networkD3)


popular.agency <- mcalls %>%
  count(agency.simple, sort = TRUE) %>%
  .[1:20, "agency.simple"] %>%
  unlist()

mcalls %<>% mutate(
  agency.simple2 = ifelse(agency.simple %in% popular.agency, agency.simple, "Other Agency")
)

count1 <- mcalls %>% count(called.before, how.heard)
count2 <- mcalls %>% count(how.heard, airs.cat)
count3 <- mcalls %>% count(airs.cat, agency.simple2)

uniqsort <- function(x) {
  unique(x) %>% sort()
}
ToSankeyLinks <- function(x, nodes, group = NULL) {
  names(x) <- c("source", "target", "value")
  x$source <- match(x$source, nodes$name) - 1
  x$target <- match(x$target, nodes$name) - 1
  if (!is.null(group)) {
    x$group = group
  }
  # convert to data frame because tibble will complain
  # if `$source`` is a column name
  x %>% as.data.frame()
}
sk.nodes <- data.frame(
  name = c(
    uniqsort(mcalls$called.before),
    uniqsort(mcalls$how.heard),
    uniqsort(mcalls$airs.cat),
    uniqsort(mcalls$agency.simple2)
  ),
  group = c(
    rep("Called Before", uniqsort(mcalls$called.before) %>% length()),
    rep("How Did You Heard", uniqsort(mcalls$how.heard) %>% length()),
    rep("AIRS Category", uniqsort(mcalls$airs.cat) %>% length()),
    rep("Referrals Made", uniqsort(mcalls$agency.simple2) %>% length())
  )
)

sk.links <- bind_rows(
  ToSankeyLinks(count1, sk.nodes, "a"),
  ToSankeyLinks(count2, sk.nodes, "b"),
  ToSankeyLinks(count3, sk.nodes, "c")
)

