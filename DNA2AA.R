#' @param DNA alignment file in FASTA format
#' @param startcodon Numeric osition in the alignment file corresponding to the first nucleotide in position 1 readframe (e.g., position 1 of the start codon)
#' @param prefix Character string for the output file prefix
#' @return FASTA file of the amino acid sequence 

convertDNA2AA<-function(fasta,startcodon=1,prefix="AminoAcid"){
  require(seqinr)
  source("./convertAA.R")
  AAtable<-read.csv("./AminoAcidTable.csv")
  sequence<-read.fasta(fasta,forceDNAtolower = F)
  no.AA<-floor(length(startcodon:length(sequence[[1]]))/3)
  lastsite<-no.AA*3+(startcodon-1)
  AA_sequences<-lapply(sequence, function(x) convertAA(x,startcodon,lastsite))
  write.fasta(AA_sequences,names(AA_sequences),paste0(prefix,".fasta"),open = "w")
}
