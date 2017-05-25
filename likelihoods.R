source("helper_functions.R")
library(dplyr)

possibilities.raw <- read.csv("Assignment a la Gili_sheet1.csv", stringsAsFactors = FALSE,
                              header = FALSE)
possibilities <- data.frame(possibilities.raw[11:164, c(1:13)], row.names = NULL)
colnames(possibilities) <- possibilities.raw[10,c(1:13)]

probabilities.raw <- read.csv("Assignment a la Gili_sheet2_modified.csv",
                              stringsAsFactors = FALSE,
                              header = FALSE)
probabilities <- data.frame(probabilities.raw[4:135,1:10], row.names = NULL)
colnames(probabilities) <- as.character(probabilities.raw[3, 1:10])

# check that probabilities sum to 1
probs.check <- probabilities %>%
  group_by(Locus) %>%
  summarize(Gidron = sum(as.numeric(Gidron)),
            Sheizaf = sum(as.numeric(Sheizaf)),
            Zeelim = sum(as.numeric(Zeelim)),
            Saif = sum(as.numeric(Saif)),
            `Ein Gedi` = sum(as.numeric(`Ein Gedi`)),
            Zofar = sum(as.numeric(Zofar)),
            Peres = sum(as.numeric(Peres)),
            Hemar = sum(as.numeric(Hemar)))


