area.names <- c("Gidron", "Sheizaf", "Zeelim", "Saif", "Ein Gedi",
                "Zofar", "Peres", "Hemar")

is.possibility <- function(row) {
  length(row["Mother"]) == 0
}

get.locus <- function(locus.name) {
  return(strsplit(locus.name, "-")[[1]][2])
}

# function to extract the rows having the given alleles in the given loci
# from the probabilities data frame
# probs: a data-frame containing probabilities
# variation: a one-row data-frame containing a list of which alleles appear at which loci
get.var.prob.rows <- function(probs, variation) {
  output.df <- data.frame()
  for (locus.name in names(variation)){
    locus <- get.locus(locus.name)
    allele <- as.character(variation[locus.name])
    row <- probs[which(probs$Locus == locus & probs$Allele == allele), area.names]
    output.df <- rbind(output.df, row)
  }

  return(output.df)
}

# function to get the log probability of seeing the given alleles in the given loci
get.area.log.prob <- function(allele.probs) {
  allele.probs <- as.numeric(allele.probs)
  very.small.prob <- 0.0001
  allele.probs[allele.probs == 0] <- very.small.prob
  area.prob <- prod(allele.probs)
  log.prob <- -log(area.prob)
  return(log.prob)
}

# function to identify which rows are variations, and not mother rows
# (and also to identify which mother they belong to)
code.var.rows <- function(possibilities)
{
  mother.rows <- which(nchar(mother) > 0)
  for (i in 1:(length(mother.rows)-1)) {
    cur.mother.row <- mother.rows[i]
    next.mother.row <- mother.rows[i+1]
    if (next.mother.row - cur.mother.row > 1) {
      prefix <- possibilities[cur.mother.row, "Mother"]
      var.code <- paste0(prefix, "_var")
      var.rows <- seq(cur.mother.row + 1, next.mother.row - 1)
      possibilities[var.rows, "Mother"] <- var.code
    }
  }

  return(possibilities)
}
