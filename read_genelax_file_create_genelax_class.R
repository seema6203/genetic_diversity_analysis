setwd("C:/Users/RIZWAN/Documents/genepop/")
install.packages("remotes")
remotes::install_github("douglasgscofield/readGenalex")
library(readGenalex)
excel_class<-readGenalexExcel("a.xlsx",worksheet = 1)
writeGenepop(excel_class,check.annotation = FALSE,file = "gene_pop.txt")

bruvo.boot(nauch1, replen = rep_nauch, cutoff = 50, quiet = TRUE)

library(genepop)




