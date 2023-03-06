# convertAA script for use with DNA2AA.R
convertAA<-function(x,startcodon,lastsite){
  codons<-unlist(strsplit(paste0(unlist(x[startcodon:lastsite]),collapse = ""), 
                          "(?<=.{3})", perl = TRUE))
  missing<-which(!codons %in% AAtable$Codon)
  AAstring<-unlist(sapply(codons, function(x) AAtable$AminoAcid[which(x==AAtable$Codon)]))
  if (length(missing)>0){
    for (i in 1:length(missing)){
      if (missing[i]==1){
        AAstring<-c("-",AAstring)
      } else if (missing[i]==length(codons)){
        AAstring<-c(AAstring[1:(missing[i]-1)],"-")
      } else {
        AAstring<-c(AAstring[1:(missing[i]-1)],"-",AAstring[missing[i]:length(AAstring)])
      }
    }
  }
  AAstring<-paste0(AAstring,collapse = "")
  return(AAstring)
}
