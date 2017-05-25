is.possibility <- function(row) {
  length(row["Mother"]) == 0
}

get.locus <- function(locus.name) {
  return(strsplit(locus.name, "-")[[1]][2])
}

get.prob.rows <- function(probs, alleles) {
  output.df <- data.frame()
  for (locus.name in names(alleles)){
    locus <- get.locus(locus.name)
    allele <- as.character(alleles[locus.name])
    row <- probs[which(probs$Locus == locus & probs$Allele == allele),]
    output.df <- rbind(output.df, row)
  }

  return(output.df)
}
