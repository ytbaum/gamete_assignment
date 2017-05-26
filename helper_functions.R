area.names <- c("Gidron", "Sheizaf", "Zeelim", "Saif", "Ein Gedi",
                "Zofar", "Peres", "Hemar")

allele.columns <- 3:13

is.possibility <- function(row) {
  length(row["Mother"]) == 0
}

# function to extract the locus number from the locus name
# note: the locus number is returned as a string
get.locus <- function(locus.name) {
  return(strsplit(locus.name, "-")[[1]][2])
}

# function to create a code to identify the mother tree of a particular genotype variant
get.var.code <- function(mother) {
  paste0(mother, "_var")
}

# get indices of rows that have a value for "Mother"
get.mother.row.indices <- function(possibilities) {
  which(nchar(possibilities$Mother) > 0)
}

# get a list of "Mother" labels
get.mothers <- function(possibilities) {
  indices <- get.mother.row.indices(possibilities)
  mothers <- possibilities$Mother[indices]
  return(mothers)
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
get.area.prob <- function(allele.probs) {
  allele.probs <- as.numeric(allele.probs)
  very.small.prob <- 0.0001
  allele.probs[allele.probs == 0] <- very.small.prob
  area.prob <- prod(allele.probs)
  return(area.prob)
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

# get the per-area probabilities of seeing a certain variation
get.var.area.probs <- function(probs, var)
{
  prob.rows <- get.var.prob.rows(probs, var)
  var.area.probs <- apply(prob.rows, 2, get.area.prob)
  return(var.area.probs)
}

# function to get the average probability, per location, for all genetic variants
# from a particular mother tree
get.mother.avg.probs <- function(possibilities, mother, probs)
{
  var.code <- get.var.code(mother)
  var.rows <- possibilities[which(possibilities$Mother == var.code),3:13]
  var.area.probs <- apply(var.rows, 1, function(var) get.var.area.probs(probs, var))
  return(var.area.probs)
}
