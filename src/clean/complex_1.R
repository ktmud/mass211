# Clean iCarolExport raw format data
if (!exists("call_reports")) {
  call_reports <-  read_csv(
    "data/211/20170209/CallReports.csv",
    na = c("", "NA", "(No feedback was needed for this call.)"),
    col_types = cols(
      CallDateAndTimeEnd = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      CallDateAndTimeStart = col_datetime(format = "%Y-%m-%d %H:%M:%S")
    ),
    skip = 2
  )
}
if (!exists("met_unmet")) {
  met_unmet <- read_csv(
    "data/211/20170209/MetUnmet.csv",
    col_types = cols(
      DateOfCall = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    skip = 2
  )
}
if (!exists("custom_fields")) {
  custom_fields <- read_csv(
    "data/211/20170209/CustomFields.csv",
    col_types = cols(
      DateOfCall = col_datetime(format = "%Y-%m-%d %H:%M:%S")
    ),
    skip = 2
  )
}

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

SplitCalls <- function(calls) {
  # split calls by ReportVersion
  # A ReportVersion is a type of data source, the data
  # fields are normally vastly different
  ret <- list()
  unique(calls$version) %>%
    sapply(function(ver) {
      message(sprintf("--- Cleaning %s ------", ver))
      ret[[ver]] <<- calls %>%
        filter(version == ver) %>%
        CleanUselessCols()
      message(sprintf("> New tbl_df has %s columns ---", ncol(ret[[ver]])))
    })
  ret
}

RenameColumns <- function(dat) {
  dat %>%
    dplyr::rename(
      id = CallReportNum,
      version = ReportVersion,
      call.start = CallDateAndTimeStart,
      call.end = CallDateAndTimeEnd,
      call.length = CallLength,
      worker.num = PhoneWorkerNum,
      worker.name = PhoneWorkerName,
      enter.worker.num = EnteredByWorkerNum,
      enter.worker.name = EnteredByName,
      enter.time = EnteredOn,
      feedback.status = FeedbackStatus,
      feedback.from = FeedbackFromPhoneWorkerNum,
      feedback.from.name = FeedbackFromPhoneWorkerName,
      
      caller.num = CallerNum,
      caller.name = CallerName,
      caller.lastname = CallerLastName,
      address = CallerAddress,
      city = CityName,
      county = CountyName,
      state = StateProvince,
      country = CountryName,
      zip = PostalCode,
      phone = PhoneNumberFull,
      
      comment = Narrative,
      
      age = `Caller Data - Age Range of the primary person needing services`,
      gender = `Caller Data - Gender of the Primary Person Needing Services`,
      
      caller.pin.relation = `Caller Data - Caller to Person in Need Relationship (PIN)`,
      dd.caller.pin.relation = `Data Collection - Caller to Person in Need Relationship (PIN)`,
      
      call.type = `Caller Data - Call Type`,
      origin = `Caller Data - Call Origination`,
      military = `Caller Data - Military Status of Primary Person needing Services`,
      dd.military = `Data Collection - Military Status of Primary Person needing Services`,
      
      how.heard = `Data Collection - How did you Learn about Mass211`,
      how.heard2 = `Caller Data - How Heard about Call2Talk?`,
      how.heard3 = `Caller Data - How did you Learn about Call2Talk`,
      how.heard4 = `Marketing - How did you learn about Call2Talk?`,
      how.heard5 = `Marketing - How did you learn about Mass211?`,
      
      called.before = `Data Collection - Have You Called Mass211 Before?`,
      called.before2 = `Caller Data - Have You Called Call2Talk Before?`,
      called.before3 = `Marketing - Have you called Mass211 before?`,
      called.before4 = `Marketing - Have you called Call2Talk before?`,
      
      cc.household_id = `Child Care Financial Assistance - Household ID`,
      cc.has_voucher = `Child Care Financial Assistance - Has Voucher/Other Financial Assistance?`,
      cc.waitlisted = `Child Care Financial Assistance - Is Parent/Guardian on Waitlist?`,
      
      cc.need.pay = `Child Care Needs (0-18 years) - Need Help Paying for Child Care?`,
      cc.need.find = `Child Care Needs (0-18 years) - Needs Help Finding Child Care?`,
      cc.notes = `Child Care Needs (0-18 years) - Child Care Notes`,
      
      cra.6to8 = `CRA - Concerns about 6 to 18 year old?`,
      cra.age = `Child Requiring Assistance (6-18 Years) Old - Age of Youth Needing Service`,
      cra.caller.relation = `Child Requiring Assistance (6-18 Years) Old - Callers Relationship to Youth`,
      cra.concern.re = `Child Requiring Assistance (6-18 Years) Old - Concerns re:  6-18 year old?`,
      cra.meds.rx = `Child Requiring Assistance (6-18 Years) Old - Psychiatric Meds Rx`,
      cra.treatment = `Child Requiring Assistance (6-18 Years) Old - Psychiatric Treatment or Counseling?`,
      cra.drug.abuse = `Child Requiring Assistance (6-18 Years) Old - Substance Abuse Treatment?`,
      cra.youth.insured = `Child Requiring Assistance (6-18 Years) Old - Youth has Health Insurance?`,
      cra.youth.living.with = `Child Requiring Assistance (6-18 Years) Old - Youth Living with`,
      cra.notes = `Child Requiring Assistance (6-18 Years) Old - CRA Notes`,
      
      dd.age = `Data Collection - Age Range of the primary person needing services`,
      dd.gender = `Data Collection - Gender of the Primary Person Needing Services`,
      dd.call.type = `Data Collection - Call Type`,
      dd.hh.everyone.insured = `Data Collection - Everyone in the Household covered by Insurance?`,
      
      change.of.anxiety = `Assessment - Anxiety Level After Call`,
      risk.assess = `Assessment - Risk Assessment`,
      
      change.of.anxiety = `Assessment - Anxiety Level After Call`,
      risk.assess = `Assessment - Risk Assessment`,
      
      lang = `Data Collection - Primary Family Language`,
      interp.used = `Data Collection - Language Interpretation used in this call?`,
      cra.lang = `DCF / Probation / CRA - Primary Family Language`,
      
      dd.source.of.income = `Data Collection - What is your source of income?`,
      age.of.child = `Family Issues - Ages of the children in the home?`,
      family.size = `Family Issues - Family Size`,
      cc.dev.concern = `Help Finding Child Care - Developmental Concerns?`,
      cc.type.needed = `Help Finding Child Care - Type of Child Care Needed`,
      call.reason = `Issues - Reason for Call`
    ) %>%
    mutate(
      # Fall backs
      called.before = FirstNonNA(called.before, called.before2, called.before3, called.before4),
      how.heard = FirstNonNA(how.heard, how.heard2, how.heard3, how.heard4),
      lang = FirstNonNA(lang, cra.lang),
      caller.pin.relation = FirstNonNA(caller.pin.relation, dd.caller.pin.relation),
      age = FirstNonNA(age, dd.age),
      gender = FirstNonNA(gender, dd.gender),
      military = FirstNonNA(military, dd.military),
      
      date = call.start,
      caller.name = CollapsedText(caller.name, caller.lastname, sep = " "),
      caller.lastname = NULL,
      comment = CollapsedText(comment, cc.notes, cra.notes, sep = " \n "),
      cc.notes = NULL,
      cra.notes = NULL
    )
}

CollapsedText <- function(..., sep = " ") {
  # Merge multiple text fields into one
  # Args:
  #   ... - arbituary number of text vectors
  #   sep - seperator when combining text
  cols <- list(...) %>%
    # replace NA's to empty string
    lapply(str_replace_na, replacement = "")
  names(cols) <- NULL
  do.call(function(...) str_c(..., sep = sep), args = cols) %>%
    str_trim()
}

AppendCustomFields <- function(calls) {
  # custom fields
  cf <- custom_fieds %>%
    rename(
      id = CallReportNum,
      field = SubCategoryName,
      # Answer and Textanswer never co-exists
      value = ifelse(is.na(Answer), TextAnswer, Answer)
    ) %>%
    mutate(
      # remove question mark
      field = str_replace_all(field, "?", "")
    )
  cf
}

AllCalls <- function(call_reports) {
  all.calls <- call_reports %>%
    RenameColumns() %>%
    SplitCalls()
  all.calls
}

# ------- Clean Raw data -----------
calls <- AllCalls(call_reports)