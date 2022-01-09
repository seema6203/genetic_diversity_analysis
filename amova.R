library(poppr)
library(hierfstat)
nou<-read.genalex("DNA+RNA final.csv",genclone = FALSE)
nou_3<-read.genalex("3_Pop_pools.csv",genclone = FALSE)
nou_4<-read.genalex("pop4.csv",genclone = FALSE)
setwd("../Downloads/")
amova_3 <- poppr.amova(nou_3, ~Pop)
set.seed(1999)
amova_3_sig<- randtest(amova_3, nrepet = 999)
a<-seq(27)
strata(nou_3)$Subpop<-a[nou$pop]
amova_subpop_3<-poppr.amova(nou_3,~Pop/Subpop)
amova_rand<-randtest(amova_subpop_3,nrepet = 999)
pop4<-basic.stats(nou_4)
pop4$overall
nou_4
poppr_basic<-poppr(nou_4)
write.csv(poppr_basic,"po4_stat.csv")
pairwise_fst<-hierfstat::pairwise.WCfst(genind2hierfstat(nou_4))
write.csv(pairwise_fst,"pairwise_fst_WC
          .csv")
pairwise_fst<- pairwise.neifst(genind2hierfstat(nou_4))       
write.csv(pairwise_fst,"pairwise_fst_nei.csv")
