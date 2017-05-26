source("helper_functions.R")
library(dplyr)

possibilities.raw <- read.csv("Assignment a la Gili_sheet1.csv", stringsAsFactors = FALSE,
                              header = FALSE)
possibilities <- data.frame(possibilities.raw[11:254, c(1:13)], row.names = NULL)
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

# table with sample, population average probabilities, ratio of (smallest to
# second-smallest negative log prob), name of area with smallest log prob, name of area
# with second-smallest log prob
output <- data.frame()
mother.row.indices <- get.mother.row.indices(possibilities)
for (i in 1:(length(mother.row.indices))) {
  cur.mother.row <- mother.row.indices[i]
  if (i < length(mother.row.indices)) {
    next.mother.row <- mother.row.indices[i+1]
    var.row.indices <- seq(cur.mother.row + 1, next.mother.row - 1)
  } else {
    # we are on the last mother row index
    var.row.indices <- seq(cur.mother.row + 1, nrow(possibilities))
  }

  var.area.probs <- sapply(var.row.indices,
                           function(var.idx) {
                             get.var.area.probs(probabilities,
                                                possibilities[var.idx, allele.columns])})
  area.probs <- apply(var.area.probs, 1, mean)
  area.probs <- -log(area.probs)
  highest.prob.area <- sort(area.probs)[1]
  second.highest.prob.area <- sort(area.probs)[2]
  ratio <- highest.prob.area / second.highest.prob.area

  cur.output.row <- nrow(output) + 1
  output[cur.output.row,"Sample"] <- possibilities[cur.mother.row, "Sample"]
  output[cur.output.row,"Mother"] <- possibilities[cur.mother.row, "Mother"]
  for (area in names(area.probs)) {
    output[cur.output.row, area] <- area.probs[area]
  }
  output[cur.output.row, "Highest"] <- names(highest.prob.area)
  output[cur.output.row, "Second"] <- names(second.highest.prob.area)
  output[cur.output.row, "Ratio"] <- ratio

}

write.csv(output, "results.csv")
