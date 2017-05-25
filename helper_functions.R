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
# alleles: a one-row data-frame containing a list of which alleles appear at which loci
get.prob.rows <- function(probs, alleles) {
  output.df <- data.frame()
  for (locus.name in names(alleles)){
    locus <- get.locus(locus.name)
    allele <- as.character(alleles[locus.name])
    row <- probs[which(probs$Locus == locus & probs$Allele == allele), area.names]
    output.df <- rbind(output.df, row)
  }

  return(output.df)
}
