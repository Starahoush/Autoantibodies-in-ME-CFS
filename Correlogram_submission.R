install.packages("qgraph")
library(qgraph)
library(ggplot2)

mainDir <- ("blob")
outDir <- ("blob")

Cor2Control=read.table(file.path(mainDir, "Cor2_Healthy.txt"), sep = '\t', header = T)
Cor2Pchc=read.table(file.path(mainDir, "Cor2_Asymptomatic.txt"), sep = '\t', header = T)
Cor2NonME=read.table(file.path(mainDir, "Cor2_Long_COVID.txt"), sep = '\t', header = T)
Cor2ME=read.table(file.path(mainDir, "Cor2_CFSME.txt"), sep = '\t', header = T)

Cor2Control[2] <- NULL
rownames(Cor2Control) <- Cor2Control[,1]
Cor2Control[1] <- NULL
colnames(Cor2Control) <- paste0("A",1:20)

Cor2Pchc[2] <- NULL
rownames(Cor2Pchc) <- Cor2Pchc[,1]
Cor2Pchc[1] <- NULL
colnames(Cor2Pchc) <- paste0("A",1:20)

Cor2NonME[2] <- NULL
rownames(Cor2NonME) <- Cor2NonME[,1]
Cor2NonME[1] <- NULL
colnames(Cor2NonME) <- paste0("A",1:20)

Cor2ME[2] <- NULL
rownames(Cor2ME) <- Cor2ME[,1]
Cor2ME[1] <- NULL
colnames(Cor2ME) <- paste0("A",1:20)

png(file = file.path(outDir, "corr_circle.png"), bg = "transparent", width = 1250, height = 1250, units = "px", res = 300)
par(mfrow=c(2,2))

cormat_hc=cor(Cor2Control, method="spearman")
write.table(cormat_hc, file=file.path(outDir, "control_network.csv"), sep = ",", quote = F)
qgraph(cormat_hc,shape="circle", minimum= 0.59, posCol="darkblue", negCol="darkred",#FOR SOME REASON 0.6 WAS RESULTING IN WAIVER()
       layout="groups", vsize=7,label.cex = 1.5, labels =  colnames(Cor2Control), title = "HC", title.cex = 0.8)

cormat_pchc=cor(Cor2Pchc, method="spearman") 
write.table(cormat_pchc, file=file.path(outDir, "PCHC_network.csv"), sep = ",", quote = F)
qgraph(cormat_pchc,shape="circle", minimum= 0.6, posCol="darkblue", negCol="darkred",
       layout="groups", vsize=7,label.cex = 1.5, labels =  colnames(Cor2Pchc), title = "PCHC", title.cex = 0.8)

cormat_nonme=cor(Cor2NonME, method="spearman") 
write.table(cormat_nonme, file=file.path(outDir, "nonME_network.csv"), sep = ",", quote = F)
qgraph(cormat_nonme,shape="circle", minimum= 0.6, posCol="darkblue", negCol="darkred",
       layout="groups", vsize=7,label.cex = 1.5, labels =  colnames(Cor2NonME), title = "PCS/non-ME/CFS", title.cex = 0.8)

cormat_me=cor(Cor2ME, method="spearman")
write.table(cormat_me, file=file.path(outDir, "ME_network.csv"), sep = ",", quote = F)
qgraph(cormat_me,shape="circle", minimum= 0.6, posCol="darkblue", negCol="darkred",
       layout="groups", vsize=7,label.cex = 1.5, labels =  colnames(Cor2ME), title = "PCS/ME/CFS", title.cex = 0.8)

dev.off()