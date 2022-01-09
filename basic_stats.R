library(poppr)
nou<-read.genalex("nou_org.csv",genclone = FALSE)
a<-find.clusters(nou)
a
setwd("../Downloads/")
nou_3<-read.genalex("3_Pop_pools.csv",genclone = FALSE)
nou_4<-read.genalex("4_pop_poops.csv",genclone = FALSE)
class(nou_3)


library(hierfstat)
?hierfstat::Ho
Ho(nou)
Hs(nou)
nou_3$pop
basic.stats(nou,)
?basic.stats
?poppr
poppr(nou_3)
?adegenet::summary(nou)
library(pegas)
amova_3 <- poppr.amova(nou_3, ~Pop)
amova_3

amova_3
set.seed(1999)
amova_3_sig<- randtest(amova_3, nrepet = 999)
