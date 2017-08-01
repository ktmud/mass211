library(lubridate)

safe_val <- function(val, accepted_vals = NULL, default_val = NULL) {
  # Get input value matching one of the acceptable values
  # otherwise returns NULL
  if (is.null(val) ||
      !is.null(accepted_vals) && !(val %in% accepted_vals)) {
    if (!is.null(default_val)) {
      val <- default_val
    } else {
      val <- NULL
    }
  }
  return(val)
}

week_start <- function(x) {
  # Get the start of a week
  as.Date(x - days(wday(x) - 1))
}
month_start <- function(x) {
  as.Date(x - days(mday(x) - 1))
}

RangeSelector <- function(...) {
  args <- list(...)
  if (length(args) == 1) {
    mindate <- min(args[[1]])
    maxdate <- max(args[[1]])
  } else {
    mindate <- args[[1]]
    maxdate <- args[[2]]
  }
  btns <- list()
  diffdays <- as.double(maxdate - mindate, units = "days")
  if (diffdays > 30 * 4) {
    btns[[length(btns) + 1]] <- list(
      count = 3,
      label = "3 mo",
      step = "month",
      stepmode = "backward"
    )
  }
  if (diffdays > 30 * 7) {
    btns[[length(btns) + 1]] <- list(
      count = 6,
      label = "6 mo",
      step = "month",
      stepmode = "backward"
    )
  }
  if (diffdays > 400) {
    btns[[length(btns) + 1]] <- list(
      count = 1,
      label = "1 yr",
      step = "year",
      stepmode = "backward"
    )
  }
  if (diffdays > 365 * 2.5) {
    btns[[length(btns) + 1]] <- list(
      count = 2,
      label = "2 yr",
      step = "year",
      stepmode = "backward"
    )
  }
  btns <<- btns
  if (length(btns) > 0) {
    btns[[length(btns) + 1]] <- list(step = "all")
    list(buttons = btns)
  } else {
    NULL    
  }
}

FillEmptyWeeks <-
  function(dat,
           mindate = NULL,
           maxdate = NULL,
           fillwith = 0) {
    if (nrow(dat) == 0)
      return(dat)
    if (any(is.na(dat$week))) {
      # ignore data with NA's
      return(dat)
    }
    if (is.null(mindate))
      mindate = min(dat$week)
    if (is.null(maxdate))
      maxdate = max(dat$week)
    dat.full <- data.frame(week = seq(mindate, maxdate, 7)) %>%
      full_join(dat, by = "week")
    dat.full[is.na(dat.full)] <- fillwith
    dat.full
  }
CollapseOthers <- function(dat, keycol, aggcol = "week", valcol = "count",
                           keep_n = 7, others = "<i>others</i>") {
  # Collapse small values of `keycol`, based on the sum of
  # `valcol` aggregated by `aggcol`
  dat$keycol <- dat[[keycol]]
  dat$aggcol <- dat[[aggcol]]
  dat$valcol <- dat[[valcol]]
  top_items <- dat %>%
    group_by(keycol) %>%
    summarize(n = sum(valcol)) %>%
    # we need this diff rate to filter out
    # very small contributors
    mutate(p = n / max(n)) %>%
    arrange(desc(n)) %>%
    filter(p > .1) %>%
    head(keep_n)
  dat.top <- dat %>%
    filter(keycol %in% top_items$keycol)
  dat <- dat %>%
    filter(!(keycol %in% top_items$keycol)) %>%
    group_by(aggcol) %>%
    # count of others authors
    summarise(valcol = sum(valcol)) %>%
    mutate(keycol = "<i>others</i>") %>%
    bind_rows(dat.top, .)
  # revert to original column names
  dat[[keycol]] <- dat$keycol
  dat[[aggcol]] <- dat$aggcol
  dat[[valcol]] <- dat$valcol
  dat$keycol <- NULL
  dat$aggcol <- NULL
  dat$valcol <- NULL
  dat
}

CountByTime <- function(dat, units = "day") {
  unit.fun <- switch(units,
                     day = as.Date,
                     week = week_start,
                     month = month_start)
  if (is.null(unit.fun)) {
    stop("Unknown time unit.")
  }
  dat %>%
    mutate(called_at = unit.fun(call.start)) %>%
    count(airs.cat, called_at) %>%
    rename(count = n) %>%
    CollapseOthers("airs.cat", "called_at")
}

PlotMainTimeline <- function(dat, units = "week") {
  units <- safe_val(units, c("day", "week", "month"), "week")
  dat <- CountByTime(dat, units)
  p <- plot_ly(dat, x = ~called_at, y = ~count, opacity = 0.6,
               color = ~airs.cat, type = "bar")
  p %<>% layout(
    barmode = "stack",
    yaxis = list(
      title = "Count",
      fixedrange = TRUE
    ),
    xaxis = list(
      title = str_to_title(units),
      rangemode = "nonnegetive",
      rangeselector = RangeSelector(dat$called_at)
    )) %>%
    config(displayModeBar = F)
  p
}

