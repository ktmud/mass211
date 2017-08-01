# Health Status Indicators analysis
library(broom)

hsi <- read_csv("data/health-indicators/hsi.csv")

count.town.hsi <- count.town %>%
  left_join(hsi, "name")


HSIFit <- function(dat) {
  fit <- glm(p_call_childcare ~ moms_pub_care, dat)
  summary(fit)
  fit <- lm(p_call_childcare ~ black, dat)
  summary(fit)
  fit <- lm(p_call_health ~ chronic_death, dat)
  summary(fit)
  fit <- lm(p_call_health ~ black, dat)
  summary(fit)
  fit <- lm(p_call_employment ~ UnempRate, dat)
  summary(fit)
  fit <- lm(p_call_employment ~ Black, dat)
  summary(fit)
  fit <- lm(p_call_mental ~ substance_abuse, dat)
  summary(fit)
  fit <- lm(p_call_mental ~ MedHouseIncome, dat)
  summary(fit)
  
  n1 <- ggplot(dat, aes(moms_pub_care, p_call_childcare)) +
    geom_point(size=1)
  n2 <- ggplot(dat, aes(chronic_death, p_call_health)) +
    geom_point(size=1)
  n3 <- ggplot(dat, aes(substance_abuse, p_call_mental)) +
    geom_point(size=1)
  n4 <- ggplot(dat, aes(UnempRate, p_call_employment)) +
    geom_point(size=1)
  r1 <- ggplot(dat, aes(Black, p_call_childcare)) +
    geom_point(size=1)
  r2 <- ggplot(dat, aes(Black, p_call_health)) +
    geom_point(size=1)
  r3 <- ggplot(dat, aes(Black, p_call_mental)) +
    geom_point(size=1)
  r4 <- ggplot(dat, aes(Black, p_call_employment)) +
    geom_point(size=1)
  i1 <- ggplot(dat, aes(MedHouseIncome, p_call_childcare)) +
    geom_point(size=1)
  i2 <- ggplot(dat, aes(MedHouseIncome, p_call_health)) +
    geom_point(size=1)
  i3 <- ggplot(dat, aes(MedHouseIncome, p_call_mental)) +
    geom_point(size=1)
  i4 <- ggplot(dat, aes(MedHouseIncome, p_call_employment)) +
    geom_point(size=1)
  grid.arrange(n1, r1, i1, n2, r2, i2, n3, r3, i3, n4, r4, i4, ncol=3)
}

library(gridExtra)
HSIFit(count.town.hsi)
