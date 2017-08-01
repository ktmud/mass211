# ============= Regression ===============
library(pscl)
library(boot)
library(MASS)

glm.nb <- MASS::glm.nb
zeroinfl <- pscl::zeroinfl

summary(m1 <- glm.nb(p_call * 1000 ~ Black + MedHouseIncome, data = count.town))
summary(m2 <- lm(I(p_call * 1000) ~ Black + MedHouseIncome, data = count.town))


glm.RR <- function(GLM.RESULT, digits = 2) {
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    LABEL <- 'NB'  # negative binomial
    # stop("Not logistic or Poisson model")
  }
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  colnames(TABLE.EXP)[1] <- LABEL
  TABLE.EXP
}

zip.geo.data <- zip.geo@data
# summary(m1 <- glm(n_call ~
#                     White + EthHet +
#                     PubAssistYes +
#                     PopDen +
#                     AtLeastBachelor,
#                   offset = log(TotalPop),
#                   family = poisson(link = 'log'),
#                   data = zip.geo.data))
glm.RR(m1)

zip.geo.data <- zip.geo@data
town.geo.data <- town.geo@data
summary(m1 <- lm(p_call ~
                    White + Black + Hispanic + Asian + EthHet +
                    MedHouseIncome + GINI + PubAssist + UnempRate + BelowPoverty +
                    PopDen +
                    AgeU5 + Age1824 + Age2534 + Age3544 + Age4554 +
                    AtLeastBachelor,
                  data = town.geo.data))
summary(m1 <- glm.nb(n_call_homeless ~
                       White + Black + Hispanic + Asian + TwoOrMore + EthHet +
                       MedHouseIncome +
                       PopDen + p.urbanpop +
                       # AgeU5 + Age1824 + Age2534 + Age3544 + Age4554 +
                       # Age5559 + I(Age6061 + Age6264 + Age6574) +
                       AtLeastBachelor,
                     data = zip.geo.data))

summary(m1 <- glm(n_call/TotalPop ~
                    White + Black + Hispanic + Asian + EthHet +
                    MedHouseIncome + GINI + PubAssist + UnempRate + BelowPoverty +
                    PopDen +
                    AgeU5 + Age1824 + Age2534 + Age3544 + Age4554 +
                    Age5559 + I(Age6061 + Age6264) +
                    I(Age6574 + Age75P) +
                    LessThanHS +
                    AtLeastBachelor +
                    SameHouse1YearAgo + 
                    Nonfamily + 
                    RenterOccupied,
                  data = town.geo.data,
                  weights = TotalPop,
                  family = quasipoisson))

summary(m1 <- glm.nb(n_call ~
                    White + Black + Hispanic + Asian + EthHet +
                    MedHouseIncome +
                    #GINI + PubAssist + UnempRate + BelowPoverty +
                    SameHouse1YearAgo +
                    Nonfamily +
                    RenterOccupied +
                    PopDen +
                    AgeU5 + Age1824 + Age2534 + Age3544 + Age4554 +
                    Age5559 + I(Age6061 + Age6264) +
                    I(Age6574 + Age75P) +
                    AtLeastBachelor +
                    offset(log(TotalPop)),
                  data = town.geo.data))


summary(m1 <- zeroinfl(n_call ~
                       White + EthHet +
                       MedHouseIncome + GINI + PubAssist + UnempRate +
                       PopDen + p.urbanpop +
                       AgeU5 + Age1824 + Age2534 + Age3544 + Age4554 +
                       Age5559 + I(Age6061 + Age6264) +
                       AtLeastBachelor | 1,
                     data = zip.geo.data,
                     weights = TotalPop))
glm.RR(m1)
