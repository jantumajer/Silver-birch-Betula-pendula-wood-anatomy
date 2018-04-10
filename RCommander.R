
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
 
    # Dopocet hydraulic conductivity (Kh, [kg*m*Mpa-1*s-1]) jednotlivych bunek
    # Viz Scholz (2013) a Gebauer, Volarik (2013)
    #         prevod um->m   plochalumenu2      hustota H20         viskozita vody
	vstup$Kh <- (1e-24) * (vstup$LumenArea^2) * 998.205 / (8*pi* 1.002e-9)

    # Vypocet prumernych hodnot parametru za serii
	vstup.serie <- aggregate(vstup[c(4:18)], by=list(SampleId=SampleId, Year=Year), FUN=mean)
	vstup.serie$Deformation <- substr(vstup.serie$SampleId, 3, 5)
	vstup.serie <- vstup.serie[order(vstup.serie$Deformation, vstup.serie$SampleId, vstup.serie$Year), ]

    # Vypocet poctu cev v letokruhu za serii
	vstup.serie2 <- aggregate(vstup[5], by=list(SampleId=SampleId, Year=Year), FUN=NROW)
	colnames(vstup.serie2) <- c("SampleId", "Year", "NVessel")
	vstup.serie <- merge(vstup.serie, vstup.serie2, by=c("SampleId", "Year"))

    # Vypocet merene plochy v ramci letokruhu za serii (pomoci nize definovane funkce)
	plocha <- PLOCHA.LETOKRUHU(vstup)
	vstup.serie <- merge(vstup.serie, plocha, by=c("SampleId", "Year"))

    # Vypocet celkove hydraulic conductivity vsech bunek ...
	vstup.serie3 <- aggregate(vstup["Kh"], by=list(SampleId=SampleId, Year=Year), FUN=sum)
	colnames(vstup.serie3) <- c("SampleId", "Year", "Kh")
	vstup.serie <- merge(vstup.serie, vstup.serie3, by=c("SampleId", "Year"))
    
    # ...a specific hydraulic conductivity (sum(Kh)/plocha, [kg*m-1*Mpa-1*s-1])
    # VE VYPOCTU ZVETSENA PLOCHA O 10% !!!!!
	vstup.serie$Ks <- vstup.serie$Kh / (1.1 * vstup.serie$MeasuredArea * 1e-12)

##############################################################################################

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


##########################################
#### Urceni plochy mereneho polygonu #####
##########################################
library(concaveman); library(sp)

##########################################

Plocha1 <- PLOCHA.LETOKRUHU(experiment1)
Plocha2 <- PLOCHA.LETOKRUHU(experiment2)

PLOCHA.LETOKRUHU <- function (VSTUP) {

VYSTUP <- data.frame(Id=unique(paste(VSTUP$SampleId, VSTUP$Year)), SampleId=NA, Year=NA, MeasuredArea=NA) # Priprava vystupniho datoveho souboru
VYSTUP["SampleId"] <- substring(VYSTUP$Id, 1, 8); VYSTUP["Year"] <- as.numeric(substring(VYSTUP$Id, 10, 13))
VYSTUP <- VYSTUP[c("SampleId", "Year", "MeasuredArea")]

for (vzorek in unique(as.character(VSTUP$SampleId))) {

	subset.1 <- subset(VSTUP, subset=VSTUP$SampleId==vzorek)
	
	for (rok in unique(subset.1$Year)) {
		
		rozloha <- NA
		subset.2 <- subset(subset.1, subset=subset.1$Year==rok) 

		body <- data.matrix(subset.2[c("HorizontalPosition", "VerticalPosition")])
		obrys <- concaveman(body, concavity=10) # Pomoci funkce vyberu obrysove body podel okraju merene casti letokruhu

		Ps <- Polygon(obrys, hole=F)
		Ps1 <- SpatialPolygons(list(Polygons(list(Ps), ID = "a"))) # Vytvorim SpatialPolygon

		if ((Ps@area == Ps1@polygons[[1]]@area) & (Ps@area == Ps1@polygons[[1]]@Polygons[[1]]@area)) {rozloha <- Ps@area} # Ze spatial polygon vytahnu udaj o rozloze
		ulozit <- VYSTUP$SampleId==vzorek & VYSTUP$Year==rok # Kam se ma vypoctena hodnota rozlohy ulozit
		VYSTUP[ulozit, 3] <- rozloha

		#plot(body); title(paste("Vzorek", vzorek, "v roce", rok)) # Vykresleni a ulozeni grafu
		#plot(Ps1, axes=T, add=T)
		#dev.copy(png, filename=paste("D:/tumajer/#Brizy/Rskript/Obrazky_letokruhu/2/", vzorek, "y", rok, ".png", sep=""))
		#dev.off()

		}
	}

return(VYSTUP)
}


##################################################################



experiment1.ag <- AGREGACE(experiment1, save=T, kod="1_")
experiment2.ag <- AGREGACE(experiment2, save=T, kod="2_")


######################################################
# TreeClim 

library(treeclim); library(dplR); library(dendroTools)

temp <- readXL("D:/tumajer/#Brizy/Rskript/Kopisty_HB.xlsx", rownames=FALSE, header=TRUE, na="", sheet="teploty", stringsAsFactors=TRUE) # Vetsina datovych podkladu
prec <- readXL("D:/tumajer/#Brizy/Rskript/Kopisty_HB.xlsx", rownames=FALSE, header=TRUE, na="", sheet="srazky", stringsAsFactors=TRUE) # Vetsina datovych podkladu
klima <- list(temp=temp, prec=prec)

climate.1 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/1.xlsx", rownames=TRUE, header=TRUE, na="", sheet="treeclim", stringsAsFactors=TRUE) 
detrend.1 <- detrend(climate.1, method="Mean", pos.slope=T, make.plot=T)
chron.1 <- chron(detrend.1[c(1:14),]) # Zjistuji klimaticky signal pouze pro obdobi pred disturbanci
dcc(chron.1, prec, boot="exact", selection=c(-8:9))
dcc(chron.1, temp, boot="exact", selection=c(-8:9))

climate.2 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=TRUE, header=TRUE, na="", sheet="treeclim", stringsAsFactors=TRUE) 
detrend.2 <- detrend(climate.2, method="Mean", pos.slope=T, make.plot=T)
chron.2 <- chron(detrend.2[c(1:15),]) # Zjistuji klimaticky signal pouze pro obdobi pred disturbanci
chron.ref <- chron(detrend.2[,c(1:16)])
dcc(chron.2, prec, boot="exact", selection=c(-8:9))
dcc(chron.2, temp, boot="exact", selection=c(-8:9))

dcc(chron.ref, prec, boot="exact", selection=c(-8:9))
dcc(chron.ref, temp, boot="exact", selection=c(-8:9))

plot(chron.1)
plot(chron.2)
plot(chron.ref)

######################################################
# dendroTools 

chron.1 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/1.xlsx", rownames=TRUE, header=TRUE, na="", sheet="dendroTools", stringsAsFactors=TRUE)
chron.1 <- rbind(chron.1, data.frame(VLA=NA, TVA=NA, VD=NA, Ks=NA)); rownames(chron.1) <- c(1999:2016)
chron.2 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=TRUE, header=TRUE, na="", sheet="dendroTools", stringsAsFactors=TRUE)

vla <- cbind(chron.1["VLA"], chron.2["VLA"])
tva <- cbind(chron.1["TVA"], chron.2["TVA"])
vd <- cbind(chron.1["VD"], chron.2["VD"])
Ks <- cbind(chron.1["Ks"], chron.2["Ks"])

prec.daily <- readXL("D:/tumajer/#Brizy/Rskript/Kopisty_klim_2000-2016.xlsx", rownames=TRUE, header=TRUE, na="", sheet="srazky", stringsAsFactors=TRUE)
temp.daily <- readXL("D:/tumajer/#Brizy/Rskript/Kopisty_klim_2000-2016.xlsx", rownames=TRUE, header=TRUE, na="", sheet="teploty", stringsAsFactors=TRUE)

vla.prec <- daily_response(response=vla[c(1:14),], env_data=prec.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)
tva.prec <- daily_response(response=tva[c(1:14),], env_data=prec.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)
vs.prec <- daily_response(response=vd[c(1:14),], env_data=prec.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)
Ks.prec <- daily_response(response=Ks[c(1:14),], env_data=prec.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)

# vla.temp <- daily_response(response=vla[c(1:14),], env_data=temp.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)
# tva.temp <- daily_response(response=tva[c(1:14),], env_data=temp.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)
# vs.temp <- daily_response(response=vd[c(1:14),], env_data=temp.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)
# Ks.temp <- daily_response(response=Ks[c(1:14),], env_data=temp.daily, lower_limit = 21, upper_limit = 270, method="lm", previous_year=T, row_names_subset=T, remove_insignificant=T, alpha=0.05, temporal_stability_check="progressive", k=5)


dr <- vla.prec
dr <- tva.prec
dr <- vs.prec
dr <- Ks.prec

	dr$plot_extreme # nejvyssi korelace
	dr$plot_heatmap # barevny graf
	# dr$plot_specific
	dr$optimized_return
	dr$temporal_stability



# Do modelu pouzit - srazky curAUG + ? curAPR-curMAY
#		   - teploty prevAug-prevOCT + ? prevNOV-prevDEC
##################################################
# Modely

klimadata <- readXL("D:/tumajer/#Brizy/Rskript/Kopisty_HB.xlsx", rownames=FALSE, header=TRUE, na="", sheet="MODEL", stringsAsFactors=TRUE) # klimaticke prumery

Data.1 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/1.xlsx", rownames=FALSE, header=TRUE, na="", sheet="series", stringsAsFactors=TRUE) # Vetsina datovych podkladu
cambial.age <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/1.xlsx", rownames=FALSE, header=TRUE, na="", sheet="camb.age2", stringsAsFactors=TRUE) # Kambialni stari jednotlivych letokruhu
Data.1 <- merge(Data.1, cambial.age, by=c("SampleId", "Year"))
Data.1 <- merge(Data.1, klimadata, by=c("Year"), all.x=T)


Data.2 <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="series", stringsAsFactors=TRUE) # Vetsina datovych podkladu
cambial.age <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="camb.age2", stringsAsFactors=TRUE) # Kambialni stari jednotlivych letokruhu
Data.2 <- merge(Data.2, cambial.age, by=c("SampleId", "Year"))
Data.2 <- merge(Data.2, klimadata, by=c("Year"), all.x=T)


plot(Data.1$Ks ~ Data.1$LumenArea)

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

with(Data.1, Hist(Ks, groups=Deformation, scale="frequency", breaks="Sturges", col="darkgray"))
with(Data.2, Hist(Ks, groups=Deformation, scale="frequency", breaks="Sturges", col="darkgray"))


##################################################
################# MODELY #########################
##################################################

library(lme4)
library(piecewiseSEM)

input.data <- rbind(Data.Exh.1, Data.Exh.2)


MODEL <- function(input.data) {

#####################################################################################
# Vypocet modelu a jejich statistik - dale se pracuje pouze s modelem Ks

   	LM.la <- lmer(LumenArea ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level) + (1|Experiment), data=input.data)
   	LM.tva <- lmer(TVA_perc ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level) + (1|Experiment), data=input.data)
   	LM.vd <- lmer(VD_mm2 ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level) + (1|Experiment), data=input.data)
   	LM.ks <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level) + (1|Experiment), data=input.data)

	ranf <- merge(data.frame(ranef(LM.la)), data.frame(ranef(LM.tva)), by=c("grpvar", "term", "grp"), all=T)
	ranf <- merge(ranf, data.frame(ranef(LM.vd)), by=c("grpvar", "term", "grp"), all=T)
	ranf <- merge(ranf, data.frame(ranef(LM.ks)), by=c("grpvar", "term", "grp"), all=T)
	colnames(ranf) <- c("grpvar", "term", "grp", "LA", "TVA", "VD", "Ks")

	fixf <- cbind(data.frame(fixef(LM.la)), data.frame(fixef(LM.tva)), data.frame(fixef(LM.vd)), data.frame(fixef(LM.ks)))
	colnames(fixf) <- c("LA", "TVA", "VD", "Ks")

	R2 <- cbind(t(sem.model.fits(LM.la)), t(sem.model.fits(LM.tva)), t(sem.model.fits(LM.vd)), t(sem.model.fits(LM.ks)))
	colnames(R2) <- c("LA", "TVA", "VD", "Ks")


#####################################################################################
# Vykresleni grafu

	limit.osy.la <- max(max(na.omit(predict(LM.la))), max(na.omit(input.data$LumenArea))) 
	limit.osy.tva <- max(max(na.omit(predict(LM.tva))), max(na.omit(input.data$TVA_perc)))
	limit.osy.vd <- max(max(na.omit(predict(LM.vd))), max(na.omit(input.data$VD_mm2))) 
	limit.osy.ks <- max(max(na.omit(predict(LM.ks))), max(na.omit(input.data$Ks)))
	
	par(mfrow=c(2,2))
	plot(predict(LM.la)~input.data$LumenArea, xlim=c(0, limit.osy.la), ylim=c(0, limit.osy.la), xlab="observation", ylab="model"); abline(lm(predict(LM.la)~input.data$LumenArea), col="red"); title("Mean Lumen Area [um2]")
	plot(predict(LM.tva)~input.data$TVA_perc, xlim=c(0, limit.osy.tva), ylim=c(0, limit.osy.tva), xlab="observation", ylab="model"); abline(lm(predict(LM.tva)~input.data$TVA_perc), col="red"); title("Total Lumen Area [% or mm2/mm2]")
	plot(predict(LM.vd)~input.data$VD_mm2, xlim=c(0, limit.osy.vd), ylim=c(0, limit.osy.vd), xlab="observation", ylab="model"); abline(lm(predict(LM.vd)~input.data$VD_mm2), col="red"); title("Vessel Density [1/mm2]")
	plot(predict(LM.ks)~input.data$Ks, xlim=c(0, limit.osy.ks), ylim=c(0, limit.osy.ks), xlab="observation", ylab="model"); abline(lm(predict(LM.ks)~input.data$Ks), col="red"); title("Specific conductivity [kg*m-1*Mpa-1*s-1]")


#####################################################################################
# 1] Vypocet lokalnich predictor effects podle Peugh (2010): Journal of School Psychology 48: 85-112
# 2] Vypocet zmeny pseudo-R2 pri vypusteni prediktoru

	effects <- data.frame(Variable=c("CA", "PREC", "TEMP", "TREE", "PHASE", "ORIENTATION", "LEVEL","PREC+TEMP", "PHASE+ORIENT+LEVEL", "EXPERIMENT"), Type=c("fixed", "fixed", "fixed", "random", "random", "random", "random", "2fixed", "3random", "random"), Effect=NA, pseudoR2=NA)

 	LM.la_CA <- lmer(Ks ~ 1 +  PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level)  + (1|Experiment), data=input.data)
 	LM.la_PREC <- lmer(Ks ~ 1 + CA  + TEMP_pAUG_pOCT + (1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level)  + (1|Experiment), data=input.data)
 	LM.la_TEMP <- lmer(Ks ~ 1 + CA + PREC_AUG + (1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level)  + (1|Experiment), data=input.data)
 	LM.la_TREE <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level) + (1|Experiment), data=input.data)
 	LM.la_PHASE <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Orientation) + (1|Level) + (1|Experiment), data=input.data)
 	LM.la_ORIENTATION <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Phase) + (1|Phase:Level) + (1|Experiment) , data=input.data)
 	LM.la_LEVEL <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Phase) + (1|Phase:Orientation)  + (1|Experiment), data=input.data)

 	LM.la_CLIMATE <- lmer(Ks ~ 1 + CA + (1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level)  + (1|Experiment), data=input.data)
	LM.la_GEOMORPH <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Experiment), data=input.data)

   	LM.la_EXPERIMENT <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) + (1|Phase) + (1|Phase:Orientation) + (1|Phase:Level), data=input.data)

	
	pseudoR2_full <- cor(predict(LM.ks), input.data$Ks)^2
	pseudoR2_CA <- cor(predict(LM.la_CA), input.data$Ks)^2
	pseudoR2_PREC <- cor(predict(LM.la_PREC), input.data$Ks)^2
	pseudoR2_TEMP <- cor(predict(LM.la_TEMP), input.data$Ks)^2
	pseudoR2_TREE <- cor(predict(LM.la_TREE), input.data$Ks)^2
	pseudoR2_PHASE <- cor(predict(LM.la_PHASE), input.data$Ks)^2
	pseudoR2_ORIENTATION <- cor(predict(LM.la_ORIENTATION), input.data$Ks)^2
	pseudoR2_LEVEL <- cor(predict(LM.la_LEVEL), input.data$Ks)^2
	pseudoR2_CLIMATE <- cor(predict(LM.la_CLIMATE), input.data$Ks)^2
	pseudoR2_GEOMORPH <- cor(predict(LM.la_GEOMORPH), input.data$Ks)^2
	pseudoR2_EXPERIMENT <- cor(predict(LM.la_EXPERIMENT), input.data$Ks)^2




	Var_full <- data.frame(VarCorr(LM.ks)); colnames(Var_full) <- c("factor", "var1", "var2", "Variance_FULL", "Std.Dev_FULL")
	Var_CA <- data.frame(VarCorr(LM.la_CA)); colnames(Var_CA) <- c("factor", "var1", "var2", "Variance_CA", "Std.Dev_CA")
	Var_PREC <- data.frame(VarCorr(LM.la_PREC)); colnames(Var_PREC) <- c("factor", "var1", "var2", "Variance_PREC", "Std.Dev_PREC")
	Var_TEMP <- data.frame(VarCorr(LM.la_TEMP)); colnames(Var_TEMP) <- c("factor", "var1", "var2", "Variance_TEMP", "Std.Dev_TEMP")

	Var_TREE <- data.frame(VarCorr(LM.la_TREE)); colnames(Var_TREE) <- c("factor", "var1", "var2", "Variance_TREE", "Std.Dev_TREE")
	Var_PHASE <- data.frame(VarCorr(LM.la_PHASE)); colnames(Var_PHASE) <- c("factor", "var1", "var2", "Variance_PHASE", "Std.Dev_PHASE")
	Var_ORIENTATION <- data.frame(VarCorr(LM.la_ORIENTATION)); colnames(Var_ORIENTATION) <- c("factor", "var1", "var2", "Variance_ORIENTATION", "Std.Dev_ORIENTATION")
	Var_LEVEL <- data.frame(VarCorr(LM.la_LEVEL)); colnames(Var_LEVEL) <- c("factor", "var1", "var2", "Variance_LEVEL", "Std.Dev_LEVEL")

	Var_CLIMATE <- data.frame(VarCorr(LM.la_CLIMATE)); colnames(Var_CLIMATE) <- c("factor", "var1", "var2", "Variance_CLIMATE", "Std.Dev_CLIMATE")
	Var_GEOMORPH <- data.frame(VarCorr(LM.la_GEOMORPH)); colnames(Var_GEOMORPH) <- c("factor", "var1", "var2", "Variance_GEOMORPH", "Std.Dev_GEOMORPH")
	Var_EXPERIMENT <- data.frame(VarCorr(LM.la_EXPERIMENT )); colnames(Var_EXPERIMENT) <- c("factor", "var1", "var2", "Variance_EXPERIMENT", "Std.Dev_EXPERIMENT")


	# Problem s nepresne prirazenymi NA radky
	Var_full <- cbind(Var_full[-c(3)], Var_CA[c(4:5)], Var_PREC[c(4:5)], Var_TEMP[c(4:5)], rbind(NA, Var_TREE[c(4:5)]), rbind(NA, Var_PHASE[c(4:5)]), rbind(NA, Var_ORIENTATION[c(4:5)]), rbind(NA, Var_LEVEL[c(4:5)]), Var_CLIMATE[c(4:5)], rbind(NA,NA,NA, Var_GEOMORPH[c(4:5)]), rbind(NA,Var_EXPERIMENT[c(4:5)]))
	effects[1,3] <- 100 * (Var_full[5, "Variance_CA"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_CA"]  
	effects[2,3] <- 100 * (Var_full[5, "Variance_PREC"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_PREC"]  
	effects[3,3] <- 100 * (Var_full[5, "Variance_TEMP"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_TEMP"] 

	effects[4,3] <- 100 * (Var_full[5, "Variance_TREE"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_TREE"]
	effects[5,3] <- 100 * (Var_full[5, "Variance_PHASE"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_PHASE"]
	effects[6,3] <- 100 * (Var_full[5, "Variance_ORIENTATION"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_ORIENTATION"]
	effects[7,3] <- 100 * (Var_full[5, "Variance_LEVEL"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_LEVEL"]

	effects[8,3] <- 100 * (Var_full[5, "Variance_CLIMATE"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_CLIMATE"]
	effects[9,3] <- 100 * (Var_full[5, "Variance_GEOMORPH"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_GEOMORPH"]
	effects[10,3] <- 100 * (Var_full[5, "Variance_EXPERIMENT"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_EXPERIMENT"]


	effects[1,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_CA) / pseudoR2_full
	effects[2,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_PREC) / pseudoR2_full
	effects[3,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_TEMP) / pseudoR2_full

	effects[4,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_TREE) / pseudoR2_full
	effects[5,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_PHASE) / pseudoR2_full
	effects[6,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_ORIENTATION) / pseudoR2_full
	effects[7,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_LEVEL) / pseudoR2_full

	effects[8,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_CLIMATE) / pseudoR2_full
	effects[9,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_GEOMORPH) / pseudoR2_full
	effects[10,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_EXPERIMENT) / pseudoR2_full


#####################################################################################
# Tabulka pozorovanych a modelovanych hodnot (pro plny model i modely s vypustenymi promennymi)

	modely.vystup <- cbind(input.data[c("Year", "SampleId", "Ks")], FULL=predict(LM.ks), CA=predict(LM.la_CA), PREC=predict(LM.la_PREC), TEMP=predict(LM.la_TEMP), TREE=predict(LM.la_TREE), PHASE=predict(LM.la_PHASE), ORIENTATION=predict(LM.la_ORIENTATION), LEVEL=predict(LM.la_LEVEL), CLIMATE=predict(LM.la_CLIMATE), GEOMORPH=predict(LM.la_GEOMORPH), EXPERIMENT=predict(LM.la_EXPERIMENT))	
	modely.vystup.agg <- aggregate(modely.vystup, by=list(Year1=modely.vystup$Year), FUN=mean)

	par(mfrow=c(1,1))
	plot(modely.vystup.agg$Ks~modely.vystup.agg$Year, ylim=c(0,30), xlab="Rok", ylab="Ks")
	colors <- rainbow(11)
	lines(modely.vystup.agg$Ks~modely.vystup.agg$Year, lwd=3)
	lines(modely.vystup.agg$FULL~modely.vystup.agg$Year, col=colors[1], lwd=3)
	lines(modely.vystup.agg$CA~modely.vystup.agg$Year, col=colors[2])
	lines(modely.vystup.agg$PREC~modely.vystup.agg$Year, col=colors[3])
	lines(modely.vystup.agg$TEMP~modely.vystup.agg$Year, col=colors[4])
	lines(modely.vystup.agg$TREE~modely.vystup.agg$Year, col=colors[5])
	lines(modely.vystup.agg$PHASE~modely.vystup.agg$Year, col=colors[6])
	lines(modely.vystup.agg$ORIENTATION~modely.vystup.agg$Year, col=colors[7])
	lines(modely.vystup.agg$LEVEL~modely.vystup.agg$Year, col=colors[8])
	lines(modely.vystup.agg$CLIMATE~modely.vystup.agg$Year, col=colors[9])
	lines(modely.vystup.agg$GEOMORPH~modely.vystup.agg$Year, col=colors[10])
	lines(modely.vystup.agg$EXPERIMENT~modely.vystup.agg$Year, col=colors[11])


	legend(range(modely.vystup.agg$Year), c(0,30), c("FULL", "CA", "PREC", "TEMP", "TREE", "PHASE", "ORIENT", "LEVEL", "CLIMATE", "GEOMORPH", "EXPERIMENT"), cex=0.8, col=colors, lty=1, title="MODEL")

#####################################################################################
# Ulozeni vysledku

	write.table(fixf, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/fixf.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
	write.table(ranf, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/ranf.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(R2, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/R2.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(modely.vystup, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/modely.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(effects, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/effects.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")


return(list(fixf=fixf, ranf=ranf, determination=R2, modely=modely.vystup, effects=effects))

}



MODEL(rbind(Data.Exh.1, Data.Exh.2))
MODEL(rbind(Data.Zas.1, Data.Zas.2))
MODEL(rbind(Data.Dek.1, Data.Dek.2))
MODEL(rbind(Data.Nak.1, Data.Nak.2))
MODEL(rbind(Data.Jiz.1, Data.Jiz.2))


########## varianta modelu pro Referencni stromy
#####################################################################################
#####################################################################################
#####################################################################################


Data.2.prekodovanoREF <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2_prekodovanoREF.xlsx", rownames=FALSE, header=TRUE, na="", sheet="series", stringsAsFactors=TRUE) # Vetsina datovych podkladu
cambial.age.prekodovanoREF <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2_prekodovanoREF.xlsx", rownames=FALSE, header=TRUE, na="", sheet="camb.age2", stringsAsFactors=TRUE) # Kambialni stari jednotlivych letokruhu
Data.2.prekodovanoREF <- merge(Data.2.prekodovanoREF, cambial.age.prekodovanoREF, by=c("SampleId", "Year"))
Data.2.prekodovanoREF <- merge(Data.2.prekodovanoREF, klimadata, by=c("Year"), all.x=T)

input.data <- Data.2.prekodovanoREF

input.data$Orientation
#####################################################################################
# Vypocet modelu a jejich statistik - dale se pracuje pouze s modelem Ks

   	LM.la <- lmer(LumenArea ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) + (1|Orientation)  , data=input.data)
   	LM.tva <- lmer(TVA_perc ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) +  (1|Orientation) , data=input.data)
   	LM.vd <- lmer(VD_mm2 ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) +  (1|Orientation)  , data=input.data)
   	LM.ks <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) +  (1|Orientation)  , data=input.data)

	ranf <- merge(data.frame(ranef(LM.la)), data.frame(ranef(LM.tva)), by=c("grpvar", "term", "grp"), all=T)
	ranf <- merge(ranf, data.frame(ranef(LM.vd)), by=c("grpvar", "term", "grp"), all=T)
	ranf <- merge(ranf, data.frame(ranef(LM.ks)), by=c("grpvar", "term", "grp"), all=T)
	colnames(ranf) <- c("grpvar", "term", "grp", "LA", "TVA", "VD", "Ks")

	fixf <- cbind(data.frame(fixef(LM.la)), data.frame(fixef(LM.tva)), data.frame(fixef(LM.vd)), data.frame(fixef(LM.ks)))
	colnames(fixf) <- c("LA", "TVA", "VD", "Ks")

	R2 <- cbind(t(sem.model.fits(LM.la)), t(sem.model.fits(LM.tva)), t(sem.model.fits(LM.vd)), t(sem.model.fits(LM.ks)))
	colnames(R2) <- c("LA", "TVA", "VD", "Ks")

#####################################################################################
# 1] Vypocet lokalnich predictor effects podle Peugh (2010): Journal of School Psychology 48: 85-112
# 2] Vypocet zmeny pseudo-R2 pri vypusteni prediktoru

	effects <- data.frame(Variable=c("CA", "PREC", "TEMP", "TREE", "PHASE", "ORIENTATION", "LEVEL","PREC+TEMP", "PHASE+ORIENT+LEVEL", "EXPERIMENT"), Type=c("fixed", "fixed", "fixed", "random", "random", "random", "random", "2fixed", "3random", "random"), Effect=NA, pseudoR2=NA)

 	LM.la_CA <- lmer(Ks ~ 1 +  PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Orientation)  , data=input.data)
 	LM.la_PREC <- lmer(Ks ~ 1 + CA  + TEMP_pAUG_pOCT + (1|Tree) + (1|Orientation)  , data=input.data)
 	LM.la_TEMP <- lmer(Ks ~ 1 + CA + PREC_AUG + (1|Tree) + (1|Orientation)   , data=input.data)
 	LM.la_TREE <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +  (1|Orientation)  , data=input.data)
 	LM.la_PHASE <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Orientation)  , data=input.data)
 	LM.la_ORIENTATION <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree)   , data=input.data)
 	LM.la_LEVEL <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) + (1|Orientation)  , data=input.data)

 	LM.la_CLIMATE <- lmer(Ks ~ 1 + CA + (1|Tree) + (1|Orientation) + (1|Phase)  , data=input.data)
	LM.la_GEOMORPH <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT + (1|Tree) , data=input.data)

   	LM.la_EXPERIMENT <- lmer(Ks ~ 1 + CA + PREC_AUG + TEMP_pAUG_pOCT +(1|Tree) +  (1|Orientation) , data=input.data)

	
	pseudoR2_full <- cor(predict(LM.ks), input.data$Ks)^2
	pseudoR2_CA <- cor(predict(LM.la_CA), input.data$Ks)^2
	pseudoR2_PREC <- cor(predict(LM.la_PREC), input.data$Ks)^2
	pseudoR2_TEMP <- cor(predict(LM.la_TEMP), input.data$Ks)^2
	pseudoR2_TREE <- cor(predict(LM.la_TREE), input.data$Ks)^2
	pseudoR2_PHASE <- cor(predict(LM.la_PHASE), input.data$Ks)^2
	pseudoR2_ORIENTATION <- cor(predict(LM.la_ORIENTATION), input.data$Ks)^2
	pseudoR2_LEVEL <- cor(predict(LM.la_LEVEL), input.data$Ks)^2
	pseudoR2_CLIMATE <- cor(predict(LM.la_CLIMATE), input.data$Ks)^2
	pseudoR2_GEOMORPH <- cor(predict(LM.la_GEOMORPH), input.data$Ks)^2
	pseudoR2_EXPERIMENT <- cor(predict(LM.la_EXPERIMENT), input.data$Ks)^2




	Var_full <- data.frame(VarCorr(LM.ks)); colnames(Var_full) <- c("factor", "var1", "var2", "Variance_FULL", "Std.Dev_FULL")
	Var_CA <- data.frame(VarCorr(LM.la_CA)); colnames(Var_CA) <- c("factor", "var1", "var2", "Variance_CA", "Std.Dev_CA")
	Var_PREC <- data.frame(VarCorr(LM.la_PREC)); colnames(Var_PREC) <- c("factor", "var1", "var2", "Variance_PREC", "Std.Dev_PREC")
	Var_TEMP <- data.frame(VarCorr(LM.la_TEMP)); colnames(Var_TEMP) <- c("factor", "var1", "var2", "Variance_TEMP", "Std.Dev_TEMP")

	Var_TREE <- data.frame(VarCorr(LM.la_TREE)); colnames(Var_TREE) <- c("factor", "var1", "var2", "Variance_TREE", "Std.Dev_TREE")
	Var_PHASE <- data.frame(VarCorr(LM.la_PHASE)); colnames(Var_PHASE) <- c("factor", "var1", "var2", "Variance_PHASE", "Std.Dev_PHASE")
	Var_ORIENTATION <- data.frame(VarCorr(LM.la_ORIENTATION)); colnames(Var_ORIENTATION) <- c("factor", "var1", "var2", "Variance_ORIENTATION", "Std.Dev_ORIENTATION")
	Var_LEVEL <- data.frame(VarCorr(LM.la_LEVEL)); colnames(Var_LEVEL) <- c("factor", "var1", "var2", "Variance_LEVEL", "Std.Dev_LEVEL")

	Var_CLIMATE <- data.frame(VarCorr(LM.la_CLIMATE)); colnames(Var_CLIMATE) <- c("factor", "var1", "var2", "Variance_CLIMATE", "Std.Dev_CLIMATE")
	Var_GEOMORPH <- data.frame(VarCorr(LM.la_GEOMORPH)); colnames(Var_GEOMORPH) <- c("factor", "var1", "var2", "Variance_GEOMORPH", "Std.Dev_GEOMORPH")
	Var_EXPERIMENT <- data.frame(VarCorr(LM.la_EXPERIMENT )); colnames(Var_EXPERIMENT) <- c("factor", "var1", "var2", "Variance_EXPERIMENT", "Std.Dev_EXPERIMENT")


	# Problem s nepresne prirazenymi NA radky
	Var_full <- cbind(Var_full[-c(3)], Var_CA[c(4:5)], Var_PREC[c(4:5)], Var_TEMP[c(4:5)], rbind(NA, Var_TREE[c(4:5)]), Var_PHASE[c(4:5)], rbind(NA, Var_ORIENTATION[c(4:5)]), Var_LEVEL[c(4:5)], Var_CLIMATE[c(3:5),c(4:5)], rbind(NA, Var_GEOMORPH[c(4:5)]), Var_EXPERIMENT[c(4:5)])
	effects[1,3] <- 100 * (Var_full[5, "Variance_CA"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_CA"]  
	effects[2,3] <- 100 * (Var_full[5, "Variance_PREC"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_PREC"]  
	effects[3,3] <- 100 * (Var_full[5, "Variance_TEMP"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_TEMP"] 

	effects[4,3] <- 100 * (Var_full[5, "Variance_TREE"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_TREE"]
	effects[5,3] <- 100 * (Var_full[5, "Variance_PHASE"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_PHASE"]
	effects[6,3] <- 100 * (Var_full[5, "Variance_ORIENTATION"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_ORIENTATION"]
	effects[7,3] <- 100 * (Var_full[5, "Variance_LEVEL"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_LEVEL"]

	effects[8,3] <- 100 * (Var_full[5, "Variance_CLIMATE"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_CLIMATE"]
	effects[9,3] <- 100 * (Var_full[5, "Variance_GEOMORPH"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_GEOMORPH"]
	effects[10,3] <- 100 * (Var_full[5, "Variance_EXPERIMENT"] - Var_full[5, "Variance_FULL"]) / Var_full[5, "Variance_EXPERIMENT"]


	effects[1,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_CA) / pseudoR2_full
	effects[2,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_PREC) / pseudoR2_full
	effects[3,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_TEMP) / pseudoR2_full

	effects[4,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_TREE) / pseudoR2_full
	effects[5,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_PHASE) / pseudoR2_full
	effects[6,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_ORIENTATION) / pseudoR2_full
	effects[7,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_LEVEL) / pseudoR2_full

	effects[8,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_CLIMATE) / pseudoR2_full
	effects[9,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_GEOMORPH) / pseudoR2_full
	effects[10,c("pseudoR2")] <- 100 * (pseudoR2_full - pseudoR2_EXPERIMENT) / pseudoR2_full


#####################################################################################
# Tabulka pozorovanych a modelovanych hodnot (pro plny model i modely s vypustenymi promennymi)

	modely.vystup <- cbind(input.data[c("Year", "SampleId", "Ks")], FULL=predict(LM.ks), CA=predict(LM.la_CA), PREC=predict(LM.la_PREC), TEMP=predict(LM.la_TEMP), TREE=predict(LM.la_TREE), PHASE=predict(LM.la_PHASE), ORIENTATION=predict(LM.la_ORIENTATION), LEVEL=predict(LM.la_LEVEL), CLIMATE=predict(LM.la_CLIMATE), GEOMORPH=predict(LM.la_GEOMORPH), EXPERIMENT=predict(LM.la_EXPERIMENT))	
	modely.vystup.agg <- aggregate(modely.vystup, by=list(Year1=modely.vystup$Year), FUN=mean)

	par(mfrow=c(1,1))
	plot(modely.vystup.agg$Ks~modely.vystup.agg$Year, ylim=c(0,30), xlab="Rok", ylab="Ks")
	colors <- rainbow(11)
	lines(modely.vystup.agg$Ks~modely.vystup.agg$Year, lwd=3)
	lines(modely.vystup.agg$FULL~modely.vystup.agg$Year, col=colors[1], lwd=3)
	lines(modely.vystup.agg$CA~modely.vystup.agg$Year, col=colors[2])
	lines(modely.vystup.agg$PREC~modely.vystup.agg$Year, col=colors[3])
	lines(modely.vystup.agg$TEMP~modely.vystup.agg$Year, col=colors[4])
	lines(modely.vystup.agg$TREE~modely.vystup.agg$Year, col=colors[5])
	lines(modely.vystup.agg$PHASE~modely.vystup.agg$Year, col=colors[6])
	lines(modely.vystup.agg$ORIENTATION~modely.vystup.agg$Year, col=colors[7])
	lines(modely.vystup.agg$LEVEL~modely.vystup.agg$Year, col=colors[8])
	lines(modely.vystup.agg$CLIMATE~modely.vystup.agg$Year, col=colors[9])
	lines(modely.vystup.agg$GEOMORPH~modely.vystup.agg$Year, col=colors[10])
	lines(modely.vystup.agg$EXPERIMENT~modely.vystup.agg$Year, col=colors[11])


	legend(range(modely.vystup.agg$Year), c(0,30), c("FULL", "CA", "PREC", "TEMP", "TREE", "PHASE", "ORIENT", "LEVEL", "CLIMATE", "GEOMORPH", "EXPERIMENT"), cex=0.8, col=colors, lty=1, title="MODEL")

#####################################################################################
effects

# Ulozeni vysledku

	write.table(fixf, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/fixf.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
	write.table(ranf, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/ranf.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(R2, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/R2.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(modely.vystup, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/modely.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
	write.table(effects, "D:/tumajer/#Brizy/Rskript/vystupy/LMM/effects.txt", sep="\t", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")

