library(plyr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(stringr)
library(readxl)
library(xml2)
library(data.tree)

if (!exists("mass211.raw")) {
  mass211.raw <- read_csv("data/211/mass211.csv")
}
m <- mass211.raw

source("src/solidify.R")
source("src/clean_raw.R")
source("src/prepare.R")
source("src/metrics.R")
source("src/treemap.R")
#source("src/cats.R")
source("src/map.R")

