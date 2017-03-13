names(m) <- c(
  "date", "id", "type", "cat.code", "cat.name", "city",
  "uw.region", "zip", "caller.relation", "age",
  "lang",  # Primary family language
  "lang.interp.used",  # Language interpretation used in this call?
  "gender",  # Gender of Primary Person Needing Service (PPNS)
  "military",  # Military status of PPNS
  "household.insured",  # Household covered by insurance
  "source.of.income",
  "channel",  # How did you learn about Mass211
  "called.before", "family.size", "children.age"
)
# remove rows with bad IDs
m <- m[!is.na(m$id), ]

# untouched values stay here
mass211 <- m

m <- cbind(m[, 1:4],
           big.cat.code = as.factor(gsub("([A-Z]+).*", "\\1", m$cat.code)),
           m[, 5:ncol(m)])

m$type <- mapvalues(
  m$type, unique(m$type),
  # "Save me from required fields" -> "Unknown"
  c("Mass211", "Disaster", "SaveMe", "Chat", "Text", "")
)
m$date <- as.POSIXct(
  m$date, tz = "America/New_York", format = "%m/%d/%Y %H:%M:%S"
)
m$uw.region <- str_replace_all(
  m$uw.region,
  "( *)(.*?)( *)United Way( of)?( *)(.*)( *)$",
  "\\2\\6"
)
m$caller.relation <- mapvalues(
  m$caller.relation, unique(m$caller.relation),
  # "Professional" =
  #    Professional Calling for Client
  # "Family member" =
  #    Caller contacting for family member living elsewhere or friend
  c("Self/Household", "Professional", "Family/Friend Elsewhere", "")
)
m$age <- mapvalues(
  m$age, unique(m$age),
  c("45-64", "18-24", "25-44", ">65", "Unknown", "<18", "", "Declined to Answer")
)
m$lang.interp.used <- mapvalues(
  m$lang.interp.used, unique(m$lang.interp.used),
  # "Not Applicable" -> ""
  # Staff = "Staff Provided"
  # Other = "Someone else provided"
  # Service = "Translation Service"
  c("", "Staff", "Other", "", "Service")
)
m$gender <- revalue(m$gender, c(Male="M", Female="F"))
m$military <- mapvalues(
  m$military, unique(m$military),
  c("None", "Veteran", "", "Unknown", "Declined", "Active Duty")
)
m$household.insured <- mapvalues(
  m$household.insured, unique(m$household.insured),
  # -1 means unknown
  c(0, NA, -1, 1)
)

# housing and mental health related issues
# m[grep("(^| )([hH]ous|[sS]helter|[hH]ome|[mM]ental)", m$cat.name), ]

xt.channel <- xtabs(~ big.cat.code + channel, m) %>% as.data.frame()
xt.channel2 <- xtabs(~ cat.name + channel, m) %>% as.data.frame()