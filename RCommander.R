
exh.1 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment1/1_exhOPRAVA2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
dek.1 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment1/1_dekOPRAVA.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
jiz.1 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment1/1_jizOPRAVA.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
nak.1 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment1/1_nakOPRAVA.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
zas.1 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment1/1_zasOPRAVA2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)

experiment1 <- rbind(exh.1, dek.1, jiz.1, nak.1, zas.1)

exh.2 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment2/2_exhOPRAVA2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
dek.2 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment2/2_dekOPRAVA.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
jiz.2 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment2/2_jizOPRAVA.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
nak.2 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment2/2_nakOPRAVA2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
zas.2 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment2/2_zasOPRAVA2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)
ref.2 <- readXL("D:/tumajer/#Brizy/Mereni/vystupy/experiment2/2_refOPRAVA2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Cell", stringsAsFactors=TRUE)

experiment2 <- rbind(exh.2, dek.2, jiz.2, nak.2, zas.2, ref.2)


###################################################################

AGREGACE <- function(vstup, save=F, kod="") {

    # Kody jednotlivych stromu, vyskovych urovni
	vstup$TreeId <- substr(vstup$SampleId, 1, 5)
	vstup$LevelId <- substr(vstup$SampleId, 1, 6)

   attach(vstup)

    # Vypocet prumernych hodnot za serii
	vstup.serie <- aggregate(vstup[c(4:18)], by=list(SampleId=SampleId, Year=Year), FUN=mean)
	vstup.serie$Deformation <- substr(vstup.serie$SampleId, 3, 5)
	vstup.serie <- vstup.serie[order(vstup.serie$Deformation, vstup.serie$SampleId, vstup.serie$Year), ]

    # Vypocet prumernych hodnot za vyskovou uroven
	vstup.level <- aggregate(vstup[c(4:18)], by=list(LevelId=LevelId, Year=Year), FUN=mean)
	vstup.level$Deformation <- substr(vstup.level$LevelId, 3, 5)
	vstup.level <- vstup.level[order(vstup.level$Deformation, vstup.level$LevelId, vstup.level$Year), ]


    # Vypocet prumernych hodnot za serii
	vstup.strom <- aggregate(vstup[c(4:18)], by=list(TreeId=TreeId, Year=Year), FUN=mean)
	vstup.strom$Deformation <- substr(vstup.strom$TreeId, 3, 5)
	vstup.strom <- vstup.strom[order(vstup.strom$Deformation, vstup.strom$TreeId, vstup.strom$Year), ]

if (save==T) {
	write.table(vstup.serie, paste("D:/tumajer/#Brizy/Rskript/vystupy/", kod, "series.txt", sep=""), sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(vstup.level, paste("D:/tumajer/#Brizy/Rskript/vystupy/", kod, "level.txt", sep=""), sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(vstup.strom, paste("D:/tumajer/#Brizy/Rskript/vystupy/", kod, "tree.txt", sep=""), sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")}

  return(list(Series=vstup.serie, Level=vstup.level, Tree=vstup.strom))

}

##################################################################



experiment1.ag <- AGREGACE(experiment1, save=T, kod="1_")
experiment2.ag <- AGREGACE(experiment2, save=T, kod="2_")



##################################################################

# Pocet zmerenych cev
exh.1.ag <- AGREGACE(exh.1, save=T, kod="1exh_")
jiz.1.ag <- AGREGACE(jiz.1, save=T, kod="1jiz_")
dek.1.ag <- AGREGACE(dek.1, save=T, kod="1dek_")
nak.1.ag <- AGREGACE(nak.1, save=T, kod="1nak_")
zas.1.ag <- AGREGACE(zas.1, save=T, kod="1zas_")
exh.2.ag <- AGREGACE(exh.2, save=T, kod="2exh_")
jiz.2.ag <- AGREGACE(jiz.2, save=T, kod="2jiz_")
dek.2.ag <- AGREGACE(dek.2, save=T, kod="2dek_")
nak.2.ag <- AGREGACE(nak.2, save=T, kod="2nak_")
zas.2.ag <- AGREGACE(zas.2, save=T, kod="2zas_")
ref.2.ag <- AGREGACE(ref.2, save=T, kod="2ref_")

N.vessels <- data.frame(Exhumace=c(nrow(exh.1),nrow(exh.2)),
			 Zasypani=c(nrow(zas.1),nrow(zas.2)), 
			 Jizva=c(nrow(jiz.1),nrow(jiz.2)),
			 Dekapitace=c(nrow(dek.1),nrow(dek.2)),
			 Nakloneni=c(nrow(nak.1),nrow(nak.2)),
			 Referencni=c(0,nrow(ref.2))	)

colSums(N.vessels); rowSums(N.vessels); sum(rowSums(N.vessels))

##################################################
# Modely

Data.1 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/1.xlsx", rownames=FALSE, header=TRUE, na="", sheet="series", stringsAsFactors=TRUE) # Vetsina datovych podkladu
cambial.age <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/1.xlsx", rownames=FALSE, header=TRUE, na="", sheet="camb.age2", stringsAsFactors=TRUE) # Kambialni stari jednotlivych letokruhu
Data.1 <- merge(Data.1, cambial.age, by=c("SampleId", "Year"))

Data.2 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="series", stringsAsFactors=TRUE) # Vetsina datovych podkladu
cambial.age <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="camb.age2", stringsAsFactors=TRUE) # Kambialni stari jednotlivych letokruhu
Data.2 <- merge(Data.2, cambial.age, by=c("SampleId", "Year"))


# Dilci deformace
Data.Dek.1 <- subset(Data.1, subset=(Data.1$Deformation=="dek"|Data.1$Deformation=="dex"))
Data.Exh.1 <- subset(Data.1, subset=(Data.1$Deformation=="exh"|Data.1$Deformation=="exx"))
Data.Jiz.1 <- subset(Data.1, subset=(Data.1$Deformation=="jiz"))
Data.Nak.1 <- subset(Data.1, subset=(Data.1$Deformation=="nak"|Data.1$Deformation=="nax"))
Data.Zas.1 <- subset(Data.1, subset=(Data.1$Deformation=="zas"))

Data.Dek.2 <- subset(Data.2, subset=(Data.2$Deformation=="dek"))
Data.Exh.2 <- subset(Data.2, subset=(Data.2$Deformation=="exh"))
Data.Jiz.2 <- subset(Data.2, subset=(Data.2$Deformation=="jiz"))
Data.Nak.2 <- subset(Data.2, subset=(Data.2$Deformation=="nak"))
Data.Zas.2 <- subset(Data.2, subset=(Data.2$Deformation=="zas"))
Data.Ref.2 <- subset(Data.2, subset=(Data.2$Deformation=="ref"))

##################################################
################# MODELY #########################
##################################################

library(lme4)
library(piecewiseSEM)

MODEL <- function(input.data) {

	LM <- lmer(LumenArea ~ 1 + CA + (1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level), data=input.data)
	print(summary(LM))
	print(fixef(LM)); print(ranef(LM, condVar=T))
	anova(LM)	

	limit.osy <- max(max(na.omit(predict(LM))), max(na.omit(input.data$LumenArea)))
	plot(predict(LM)~input.data$LumenArea, xlim=c(0, limit.osy), ylim=c(0, limit.osy), xlab="observation", ylab="model"); abline(lm(predict(LM)~input.data$LumenArea), col="red")
	
	print("Marginal a Conditional R2:"); sem.model.fits(LM)

}

MODEL(Data.Exh.1)
MODEL(Data.Zas.1)
MODEL(Data.Dek.1)
MODEL(Data.Nak.1)
MODEL(Data.Jiz.1)

MODEL(Data.Exh.2)
MODEL(Data.Zas.2)
MODEL(Data.Dek.2)
MODEL(Data.Nak.2)
MODEL(Data.Jiz.2)
MODEL(Data.Ref.2)

MODEL(rbind(Data.Exh.1, Data.Exh.2))
MODEL(rbind(Data.Zas.1, Data.Zas.2))
MODEL(rbind(Data.Dek.1, Data.Dek.2))
MODEL(rbind(Data.Nak.1, Data.Nak.2))
MODEL(rbind(Data.Jiz.1, Data.Jiz.2))



