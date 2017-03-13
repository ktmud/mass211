# ============= Regression ===============

fit <- lm(p.call ~
            White * MedHouseIncome,
          count.zip)
summary(fit)
broom::tidy(fit)

ByZip <- function(dat) {
  # Count and run linear regression by zipcode
  fit <- lm(p.call ~
              SexRatio + AgeU5 + Age517 +
              White + Black + Asian +
              MedHouseIncome + GINI + UnempRate,
            aci.zip)
  summary(fit)
}