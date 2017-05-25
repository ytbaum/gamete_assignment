source("helper_functions.R")

possibilities.raw <- read.csv("Assignment a la Gili_sheet1.csv", stringsAsFactors = FALSE,
                              header = FALSE)
possibilities <- data.frame(possibilities.raw[11:164, c(1:13)])
colnames(possibilities) <- possibilities.raw[10,c(1:13)]

probabilities.raw <- read.csv("Assignment a la Gili_sheet2.csv", stringsAsFactors = FALSE,
                              header = FALSE)
probabilities <- data.frame(probabilities.raw[4:134,1:10])

# note: set stringsAsFactors to FALSE to convert this row to a character vector
colnames(probabilities) <- as.character(probabilities.raw[2, 1:10])
