source("helper_functions.R")

possibilities.raw <- read.csv("Assignment a la Gili_sheet1.csv", stringsAsFactors = FALSE)
possibilities <- data.frame(possibilities.raw[10:nrow(possibilities.raw), c(1:13)])
colnames(possibilities) <- possibilities.raw[9,c(1:13)]

probabilities.raw <- read.csv("Assignment a la Gili_sheet2.csv", stringsAsFactors = FALSE)
probabilities <- data.frame(probabilities.raw[3:134,1:10])

# note: set stringsAsFactors to FALSE to convert this row to a character vector
colnames(probabilities) <- as.character(probabilities.raw[2, 1:10])
