
Dataset <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="series", stringsAsFactors=TRUE)

Dataset.ca <- readXL("D:/tumajer/#Brizy/Rskript/vystupy/2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="cambial.age", stringsAsFactors=TRUE)

Dataset <- Dataset[c("SampleId", "Year", "Tree")]

merge <- Dataset
merge$SampleId2 <- substr(merge$SampleId, 1, 6)

# Pro skupinu 1
for (i in c("1_dekD", "1_dekH", "1_dexD", "1_dexH", "1_exhD", "1_exhH", "1_exxD", "1_exxH", "1_jizD", "1_jizH", "1_jizJ", "1_nakD", "1_nakH", "1_zasD", "1_zasH", "2_dekD", "2_dekH", "2_dexD", "2_dexH", "2_exhD", "2_exhH", "2_jizD", "2_jizH", "2_jizJ", "2_nakD", "2_nakH", "2_naxD", "2_naxH", "2_zasD", "2_zasH", "3_dekD", "3_dekH", "3_dexD", "3_dexH", "3_exhD", "3_exhH", "3_jizD", "3_jizH", "3_jizJ", "3_nakD", "3_nakH", "3_zasD", "3_zasH", "4_dekD", "4_dekH", "4_jizD", "4_jizH", "4_jizJ", "4_nakD", "4_nakH", "4_zasD", "4_zasH", "5_jizD", "5_jizH", "5_jizJ", "5_zasD", "5_zasH")) {
# Pro skupinu 2
for (i in c("1_dekD", "1_dekH", "1_exhD", "1_exhH", "1_jizD", "1_jizH", "1_jizJ", "1_nakD", "1_nakH", "1_ref_", "1_zasD", "1_zasH", "2_dekD", "2_dekH", "2_exhD", "2_exhH", "2_jizD", "2_jizH", "2_jizJ", "2_nakD", "2_nakH", "2_ref_", "2_zasD", "2_zasH", "3_dekD", "3_dekH", "3_exhD", "3_exhH", "3_jizD", "3_jizH", "3_jizJ", "3_nakD", "3_nakH", "3_ref_", "3_zasD", "3_zasH", "4_dekD", "4_dekH", "4_exhD", "4_exhH", "4_jizD", "4_jizH", "4_jizJ", "4_ref_", "4_zasD", "4_zasH", "5_dekD", "5_dekH", "5_exhD", "5_exhH", "5_jizD", "5_jizH", "5_jizJ", "5_nakD", "5_nakH", "5_zasD", "5_zasH", "6_dekD", "6_dekH", "6_jizD", "6_jizH", "6_jizJ", "6_zasD", "6_zasH")) {
	vyber <- Dataset.ca[c("Year", paste("X",i, sep=""))]
	vyber$Tree <- i
	merge <- merge(merge, vyber, by.y=c("Tree", "Year"), by.x=c("SampleId2", "Year"), all.x=T) }
write.table(merge, "D:/tumajer/#Brizy/Rskript/CambialAge2.txt", sep="\t", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")

