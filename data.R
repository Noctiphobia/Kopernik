library(dplyr)
library(sqldf)

ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi","nie pamieta"))
obserwacje = read.csv('obserwacje.csv', na.strings = c("", " ", "NA"))

###Cleaning - ankieta

for (i in 1:dim(obserwacje)[2]){
	obserwacje[,i] = trimws(obserwacje[,i])
}

#Order levels
ankieta$klasyfikacja_z = ordered(ankieta$klasyfikacja_z,
																 levels = c("calkowicie niezwiazany z science",
																 					 "posrednio zwiazany z science",
																 					 "bezposrdnio zwiazany z science"))
time_frequency = c(21, 38:49)
for (i in time_frequency)
{
	ankieta[, i] = ordered(ankieta[, i],
												 levels = c("nigdy albo rzadko (raz w roku)", "kilka razy w roku",
												 					 "raz lub dwa razy w miesiacu", "raz w tygodniu",
												 					 "prawie codziennie"))
}

agreement_levels = c(50:60)
time_frequency = c(21, 38:49)
for (i in time_frequency)
{
	ankieta[, i] = ordered(ankieta[, i],
												 levels = c("zdecydowanie sie nie zgadzam", "raczej sie nie zgadzam",
												 					 "ani sie zgadzam ani sie nie zgadzam", 
												 					 "raczej sie zgadzam", "zdecydowanie sie zgadzam"))
}

ankieta$p_16_2_4 = ordered(ankieta$p_16_2_4,
													 levels = c("nie ma ani jednej", "kilka (mniej niz 20)",
													 					 "duzo (od 20 do 50)", "bardzo duzo (wiecej niz 50 do 100)",
													 					 "cale mnostwo (wiecej niz 100)"))


###Cleaning - obserwacje
for (i in 1:dim(ankieta)[2]){
	ankieta[,i] = trimws(ankieta[,i])
}

obserwacje[is.na(obserwacje$galeria),"galeria"] = "<brak>"

levels(obserwacje$eksponat) = c(levels(obserwacje$eksponat), "PRZERWA")
obserwacje[obserwacje$kategorie==1,]$eksponat = "PRZERWA"
obserwacje[obserwacje$eksponat=="INNE - PLANETARIUM",]$eksponat <- "PLANETARIUM"

oldw <- getOption("warn")
options(warn = -1)
for (i in 13:21){
	obserwacje[,i] = as.numeric(as.character(obserwacje[,i]))
	obserwacje[,i][obserwacje[,i]<100] = NA
}
options(warn=oldw)


# DZIECI

## Liczba dzieci, dla ktorych jest zarowno ankieta, jak i obserwacje
library(sqldf)
sqldf("select count(distinct id_ucznia) from ankieta join obserwacje on id_ucznia = ID")

## Czy wiedza o rodzicach jest zalezna od plci?
### STUDIA
#### DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_studies_none <- nrow(subset(data_girls, is.na(data_girls$studia_m) & is.na(data_girls$studia_t)))
data_girls_know_studies_both <- nrow(subset(data_girls, !is.na(data_girls$studia_m) & !is.na(data_girls$studia_t)))
data_girls_know_studies_one <- nrow(data_girls) - data_girls_know_studies_both - data_girls_know_studies_none

data_boys <- subset(ankieta, ankieta$plec != "dziewczyna")
data_boys_know_studies_none <- nrow(subset(data_boys, is.na(data_boys$studia_m) & is.na(data_boys$studia_t)))
data_boys_know_studies_both <- nrow(subset(data_boys, !is.na(data_boys$studia_m) & !is.na(data_boys$studia_t)))
data_boys_know_studies_one <- nrow(data_boys) - data_boys_know_studies_both - data_boys_know_studies_none

num_girls = nrow(data_girls)
num_boys = nrow(data_boys)

vgirls = c(rep(0, data_girls_know_studies_none), rep(1, data_girls_know_studies_one), rep(2, data_girls_know_studies_both))
vboys = c(rep(0, data_boys_know_studies_none), rep(1, data_boys_know_studies_one), rep(2, data_boys_know_studies_both))
print("Wyniki testu statystycznej istotności większej wiedzy dziewczynek na temat studiów rodziców")
print(wilcox.test(vgirls, vboys, alternative = "greater", correct = FALSE))


#### WYKRES SLUPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_studies_none/num_girls,1),round(100*data_boys_know_studies_none/num_boys,1)),
											 Jeden=c(round(100*data_girls_know_studies_one/num_girls,1),round(100*data_boys_know_studies_one/num_boys,1)),
											 Tak=c(round(100*data_girls_know_studies_both/num_girls,1),round(100*data_boys_know_studies_both/num_boys,1))),
									.Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedza o studiach rodzicow?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "chlopcy"), col=colors)

### PRACA
#### DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_work_none <- nrow(subset(data_girls, is.na(data_girls$praca_m) & is.na(data_girls$praca_o)))
data_girls_know_work_both <- nrow(subset(data_girls, !is.na(data_girls$praca_m) & !is.na(data_girls$praca_o)))
data_girls_know_work_one <- nrow(data_girls) - data_girls_know_work_both - data_girls_know_work_none

data_boys <- subset(ankieta, ankieta$plec != "dziewczyna")
data_boys_know_work_none <- nrow(subset(data_boys, is.na(data_boys$praca_m) & is.na(data_boys$praca_o)))
data_boys_know_work_both <- nrow(subset(data_boys, !is.na(data_boys$praca_m) & !is.na(data_boys$praca_o)))
data_boys_know_work_one <- nrow(data_boys) - data_boys_know_work_both - data_boys_know_work_none

num_girls = nrow(data_girls)
num_boys = nrow(data_boys)

vgirls = c(rep(0, data_girls_know_work_none), rep(1, data_girls_know_work_one), rep(2, data_girls_know_work_both))
vboys = c(rep(0, data_boys_know_work_none), rep(1, data_boys_know_work_one), rep(2, data_boys_know_work_both))
print("Wyniki testu statystycznej istotności większej wiedzy dziewczynek na temat pracy rodziców")
print(wilcox.test(vgirls, vboys, alternative = "greater", correct = FALSE))

#### WYKRES SLUPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_work_none/num_girls,1),round(100*data_boys_know_work_none/num_boys,1)),
											 Jeden=c(round(100*data_girls_know_work_one/num_girls,1),round(100*data_boys_know_work_one/num_boys,1)),
											 Tak=c(round(100*data_girls_know_work_both/num_girls,1),round(100*data_boys_know_work_both/num_boys,1))),
									.Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedza o pracy rodzicow?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "chlopcy"), args.legend = list(x ='topleft'), col=colors)

# OCENY
ankieta_clr = read.csv('ankieta_clr.csv')

## BOXPLOT
notes<-data.frame(Matematyka=ankieta_clr$ocena_matematyka,Polski=ankieta_clr$ocena_jezyk_polski,Przyroda=ankieta_clr$ocena_przyroda)
boxplot(notes, ylab ="Oceny", xlab ="Przedmiot", main="Oceny z przedmiotow", varwidth=TRUE)

## Oceny dzieci vs. studia rodzicow
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
notes_studying<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jezyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jezyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_studying[,"Matematyka"]))
notes_studying_num[,"Polski"]<-as.numeric(as.character(notes_studying[,"Polski"]))
notes_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_studying[,"Przyroda"]))
mean_studying <- rowMeans(notes_studying_num, na.rm = TRUE)

not_studying <- subset(ankieta, (ankieta$studia_m == "nie" & ankieta$studia_t=="nie") | (is.na(ankieta$studia_m) & ankieta$studia_t=="nie") | (ankieta$studia_m == "nie" & is.na(ankieta$studia_t)))
notes_not_studying<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jezyk_polski,Przyroda=not_studying$ocena_przyroda)
notes_not_studying_num<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jezyk_polski,Przyroda=not_studying$ocena_przyroda)
notes_not_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_not_studying[,"Matematyka"]))
notes_not_studying_num[,"Polski"]<-as.numeric(as.character(notes_not_studying[,"Polski"]))
notes_not_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_not_studying[,"Przyroda"]))
mean_not_studying <- rowMeans(notes_not_studying_num, na.rm = TRUE)

lmts <- range(mean_studying,mean_not_studying, na.rm = TRUE)

par(mfrow = c(1, 2))
boxplot(mean_studying,ylim=lmts, ylab ="Srednia ocen", xlab ="Ze studiami", varwidth=TRUE)
boxplot(mean_not_studying,ylim=lmts, xlab ="Bez studiow", varwidth=TRUE)
title(main="Czy studia rodzicow wplywaja na oceny dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

print("Wyniki testu hipotezy: dzieci, których rodzice studiowali mają lepsze oceny")
print(t.test(mean_studying, mean_not_studying, alternative = "greater"))


## Oceny dzieci vs. doping rodzicow

zd_tak<-subset(ankieta, ankieta$p_19_b_4=="zdecydowanie sie zgadzam")
notes_zd_tak<-data.frame(Matematyka=zd_tak$ocena_matematyka,Polski=zd_tak$ocena_jezyk_polski,Przyroda=zd_tak$ocena_przyroda)
notes_zd_tak_num<-data.frame(Matematyka=zd_tak$ocena_matematyka,Polski=zd_tak$ocena_jezyk_polski,Przyroda=zd_tak$ocena_przyroda)
notes_zd_tak_num[,"Matematyka"]<-as.numeric(as.character(notes_zd_tak[,"Matematyka"]))
notes_zd_tak_num[,"Polski"]<-as.numeric(as.character(notes_zd_tak[,"Polski"]))
notes_zd_tak_num[,"Przyroda"]<-as.numeric(as.character(notes_zd_tak[,"Przyroda"]))
mean_zd_tak <- rowMeans(notes_zd_tak_num, na.rm = TRUE)

tak<-subset(ankieta, ankieta$p_19_b_4=="raczej sie zgadzam")
notes_tak<-data.frame(Matematyka=tak$ocena_matematyka,Polski=tak$ocena_jezyk_polski,Przyroda=tak$ocena_przyroda)
notes_tak_num<-data.frame(Matematyka=tak$ocena_matematyka,Polski=tak$ocena_jezyk_polski,Przyroda=tak$ocena_przyroda)
notes_tak_num[,"Matematyka"]<-as.numeric(as.character(notes_tak[,"Matematyka"]))
notes_tak_num[,"Polski"]<-as.numeric(as.character(notes_tak[,"Polski"]))
notes_tak_num[,"Przyroda"]<-as.numeric(as.character(notes_tak[,"Przyroda"]))
mean_tak <- rowMeans(notes_tak_num, na.rm = TRUE)

tak_nie<-subset(ankieta, ankieta$p_19_b_4=="ani sie zgadzam ani sie nie zgadzam")
notes_tak_nie<-data.frame(Matematyka=tak_nie$ocena_matematyka,Polski=tak_nie$ocena_jezyk_polski,Przyroda=tak_nie$ocena_przyroda)
notes_tak_nie_num<-data.frame(Matematyka=tak_nie$ocena_matematyka,Polski=tak_nie$ocena_jezyk_polski,Przyroda=tak_nie$ocena_przyroda)
notes_tak_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_tak_nie[,"Matematyka"]))
notes_tak_nie_num[,"Polski"]<-as.numeric(as.character(notes_tak_nie[,"Polski"]))
notes_tak_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_tak_nie[,"Przyroda"]))
mean_tak_nie <- rowMeans(notes_tak_nie_num, na.rm = TRUE)

nie<-subset(ankieta, ankieta$p_19_b_4=="raczej sie nie zgadzam")
notes_nie<-data.frame(Matematyka=nie$ocena_matematyka,Polski=nie$ocena_jezyk_polski,Przyroda=nie$ocena_przyroda)
notes_nie_num<-data.frame(Matematyka=nie$ocena_matematyka,Polski=nie$ocena_jezyk_polski,Przyroda=nie$ocena_przyroda)
notes_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_nie[,"Matematyka"]))
notes_nie_num[,"Polski"]<-as.numeric(as.character(notes_nie[,"Polski"]))
notes_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_nie[,"Przyroda"]))
mean_nie <- rowMeans(notes_nie_num, na.rm = TRUE)

zd_nie<-subset(ankieta, ankieta$p_19_b_4=="zdecydowanie sie nie zgadzam")
notes_zd_nie<-data.frame(Matematyka=zd_nie$ocena_matematyka,Polski=zd_nie$ocena_jezyk_polski,Przyroda=zd_nie$ocena_przyroda)
notes_zd_nie_num<-data.frame(Matematyka=zd_nie$ocena_matematyka,Polski=zd_nie$ocena_jezyk_polski,Przyroda=zd_nie$ocena_przyroda)
notes_zd_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_zd_nie[,"Matematyka"]))
notes_zd_nie_num[,"Polski"]<-as.numeric(as.character(notes_zd_nie[,"Polski"]))
notes_zd_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_zd_nie[,"Przyroda"]))
mean_zd_nie <- rowMeans(notes_zd_nie_num, na.rm = TRUE)


lmts <- range(notes_zd_tak_num,notes_tak_num,notes_tak_nie_num,notes_nie_num,notes_zd_nie_num, na.rm = TRUE)

par(mfrow = c(1, 5))
boxplot(mean_zd_tak,ylim=lmts, ylab ="Srednia ocen", xlab ="Zdecydowanie sie zgadzam", varwidth=TRUE)
boxplot(mean_tak,ylim=lmts, xlab ="Raczej sie zgadzam", varwidth=TRUE)
boxplot(mean_tak_nie,ylim=lmts, xlab ="Ani sie zgadzam, ani sie nie zgadzam", varwidth=TRUE)
boxplot(mean_nie,ylim=lmts, xlab ="Raczej sie nie zgadzam", varwidth=TRUE)
boxplot(mean_zd_nie,ylim=lmts, xlab ="Zdecydowanie sie nie zgadzam", varwidth=TRUE)
title(main="Czy doping rodzicow wplywa na oceny dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Kapital dzieci vs. studia rodzicow
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
k_studying<-data.frame(Kapital=studying$k_sum_4)
studies_points<-as.numeric(as.character(studying[,"k_studia_m"]))+as.numeric(as.character(studying[,"k_studia_t"]))
k_studying[,"Kapital"]<-as.numeric(as.character(k_studying[,"Kapital"]))-studies_points

not_studying <- subset(ankieta, (ankieta$studia_m == "nie" & ankieta$studia_t=="nie") | (is.na(ankieta$studia_m) & ankieta$studia_t=="nie") | (ankieta$studia_m == "nie" & is.na(ankieta$studia_t)))
k_not_studying<-data.frame(Kapital=not_studying$k_sum_4)
studies_points<-as.numeric(as.character(not_studying[,"k_studia_m"]))+as.numeric(as.character(not_studying[,"k_studia_t"]))
k_not_studying[,"Kapital"]<-as.numeric(as.character(k_not_studying[,"Kapital"]))-studies_points

lmts <- range(k_studying,k_not_studying, na.rm = TRUE)

par(mfrow = c(1, 2))
boxplot(k_studying,ylim=lmts, ylab ="Kapital naukowy", xlab ="Ze studiami", varwidth=TRUE)
boxplot(k_not_studying,ylim=lmts, xlab ="Bez studiow", varwidth=TRUE)
title(main="Czy studia rodzicow wplywaja na kapital naukowy dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Kapital (bez ocen) vs. oceny
kapital<-data.frame(Kapital=ankieta$k_sum_4)
kapital[,"Kapital"]<-as.numeric(as.character(kapital[,"Kapital"]))

notes_studying<-data.frame(Matematyka=ankieta$ocena_matematyka,Polski=ankieta$ocena_jezyk_polski,Przyroda=ankieta$ocena_przyroda)
notes_studying_num<-data.frame(Matematyka=ankieta$ocena_matematyka,Polski=ankieta$ocena_jezyk_polski,Przyroda=ankieta$ocena_przyroda)
notes_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_studying[,"Matematyka"]))
notes_studying_num[,"Polski"]<-as.numeric(as.character(notes_studying[,"Polski"]))
notes_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_studying[,"Przyroda"]))

sum_studying <- rowSums(notes_studying_num, na.rm = TRUE)
mean_studying <- rowMeans(notes_studying_num, na.rm = TRUE)

kapital <- as.numeric(unlist(as.vector(kapital - sum_studying)))

plot(kapital, mean_studying, main="Kapital vs Oceny", xlab="Kapital (bez ocen) ", ylab="Srednia ocen")

## Oceny vs. Liczba ksiazek
more_100<-subset(ankieta, ankieta$p_16_2_4=="cale mnostwo (wiecej niz 100)")
notes_more_100<-data.frame(Matematyka=more_100$ocena_matematyka,Polski=more_100$ocena_jezyk_polski,Przyroda=more_100$ocena_przyroda)
notes_more_100_num<-data.frame(Matematyka=more_100$ocena_matematyka,Polski=more_100$ocena_jezyk_polski,Przyroda=more_100$ocena_przyroda)
notes_more_100_num[,"Matematyka"]<-as.numeric(as.character(notes_more_100[,"Matematyka"]))
notes_more_100_num[,"Polski"]<-as.numeric(as.character(notes_more_100[,"Polski"]))
notes_more_100_num[,"Przyroda"]<-as.numeric(as.character(notes_more_100[,"Przyroda"]))
mean_more_100 <- rowMeans(notes_more_100_num, na.rm = TRUE)

bduzo<-subset(ankieta, ankieta$p_16_2_4=="bardzo duzo (wiecej niz 50 do 100)")
notes_bduzo<-data.frame(Matematyka=bduzo$ocena_matematyka,Polski=bduzo$ocena_jezyk_polski,Przyroda=bduzo$ocena_przyroda)
notes_bduzo_num<-data.frame(Matematyka=bduzo$ocena_matematyka,Polski=bduzo$ocena_jezyk_polski,Przyroda=bduzo$ocena_przyroda)
notes_bduzo_num[,"Matematyka"]<-as.numeric(as.character(notes_bduzo[,"Matematyka"]))
notes_bduzo_num[,"Polski"]<-as.numeric(as.character(notes_bduzo[,"Polski"]))
notes_bduzo_num[,"Przyroda"]<-as.numeric(as.character(notes_bduzo[,"Przyroda"]))
mean_bduzo <- rowMeans(notes_bduzo_num, na.rm = TRUE)

duzo<-subset(ankieta, ankieta$p_16_2_4=="duzo (od 20 do 50)")
notes_duzo<-data.frame(Matematyka=duzo$ocena_matematyka,Polski=duzo$ocena_jezyk_polski,Przyroda=duzo$ocena_przyroda)
notes_duzo_num<-data.frame(Matematyka=duzo$ocena_matematyka,Polski=duzo$ocena_jezyk_polski,Przyroda=duzo$ocena_przyroda)
notes_duzo_num[,"Matematyka"]<-as.numeric(as.character(notes_duzo[,"Matematyka"]))
notes_duzo_num[,"Polski"]<-as.numeric(as.character(notes_duzo[,"Polski"]))
notes_duzo_num[,"Przyroda"]<-as.numeric(as.character(notes_duzo[,"Przyroda"]))
mean_duzo <- rowMeans(notes_duzo_num, na.rm = TRUE)

kilka<-subset(ankieta, ankieta$p_16_2_4=="kilka (mniej niz 20)")
notes_kilka<-data.frame(Matematyka=kilka$ocena_matematyka,Polski=kilka$ocena_jezyk_polski,Przyroda=kilka$ocena_przyroda)
notes_kilka_num<-data.frame(Matematyka=kilka$ocena_matematyka,Polski=kilka$ocena_jezyk_polski,Przyroda=kilka$ocena_przyroda)
notes_kilka_num[,"Matematyka"]<-as.numeric(as.character(notes_kilka[,"Matematyka"]))
notes_kilka_num[,"Polski"]<-as.numeric(as.character(notes_kilka[,"Polski"]))
notes_kilka_num[,"Przyroda"]<-as.numeric(as.character(notes_kilka[,"Przyroda"]))
mean_kilka <- rowMeans(notes_kilka_num, na.rm = TRUE)

zero<-subset(ankieta, ankieta$p_16_2_4=="nie ma ani jednej")
notes_zero<-data.frame(Matematyka=zero$ocena_matematyka,Polski=zero$ocena_jezyk_polski,Przyroda=zero$ocena_przyroda)
notes_zero_num<-data.frame(Matematyka=zero$ocena_matematyka,Polski=zero$ocena_jezyk_polski,Przyroda=zero$ocena_przyroda)
notes_zero_num[,"Matematyka"]<-as.numeric(as.character(notes_zero[,"Matematyka"]))
notes_zero_num[,"Polski"]<-as.numeric(as.character(notes_zero[,"Polski"]))
notes_zero_num[,"Przyroda"]<-as.numeric(as.character(notes_zero[,"Przyroda"]))
mean_zero <- rowMeans(notes_zero_num, na.rm = TRUE)

lmts <- range(mean_more_100,mean_bduzo,mean_duzo,mean_kilka,mean_zero, na.rm = TRUE)

par(mfrow = c(1, 5))
boxplot(mean_more_100,ylim=lmts, ylab ="Srednia ocen", xlab ="Cale mnostwo (wiecej niz 100)", varwidth=TRUE)
boxplot(mean_bduzo,ylim=lmts, xlab ="Bardzo duzo (wiecej niz 50 do 100)", varwidth=TRUE)
boxplot(mean_duzo,ylim=lmts, xlab ="Duzo (od 20 do 50)", varwidth=TRUE)
boxplot(mean_kilka,ylim=lmts, xlab ="Kilka (mniej niz 20)", varwidth=TRUE)
boxplot(mean_zero,ylim=lmts, xlab ="Nie ma ani jednej", varwidth=TRUE)
title(main="Czy ilosc ksiazek w domu wplywa na oceny dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Oceny vs. Praca rodzicow
ankieta_clr_r<-subset(ankieta_clr, (ankieta$zawod_matki!="Inne" & ankieta$zawod_ojciec!="Inne") & (!is.na(ankieta$zawod_matki) & !is.na(ankieta$zawod_ojciec)))

###TECHNICZNE
techniczne_r <- subset(ankieta_clr_r, ankieta_clr_r$zawod_matki == "Techniczny" | ankieta_clr_r$zawod_ojciec=="Techniczny")
notes_techniczne<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_techniczne[,"Matematyka"]))
mean_techniczne <- colMeans(notes_techniczne_num, na.rm = TRUE)

not_techniczne_r <- subset(ankieta_clr_r, (ankieta_clr_r$zawod_matki != "Techniczny" & ankieta_clr_r$zawod_ojciec!="Techniczny") | (is.na(ankieta_clr_r$zawod_matki) & ankieta_clr_r$zawod_ojciec=="Techniczny") | (ankieta_clr_r$zawod_matki == "Techniczny" & is.na(ankieta_clr_r$zawod_ojciec)))
notes_not_techniczne<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_not_techniczne[,"Matematyka"]))
mean_not_techniczne <- colMeans(notes_not_techniczne_num, na.rm = TRUE)

###HUMANISTYCZNE
humanistyczne_r <- subset(ankieta_clr_r, ankieta_clr_r$zawod_matki == "Humanistyczny" | ankieta_clr_r$zawod_ojciec=="Humanistyczny")
notes_humanistyczne<-data.frame(Polski=humanistyczne_r$ocena_jezyk_polski)
notes_humanistyczne_num<-data.frame(Polski=humanistyczne_r$ocena_jezyk_polski)
notes_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_humanistyczne[,"Polski"]))
mean_humanistyczne <- colMeans(notes_humanistyczne_num, na.rm = TRUE)

not_humanistyczne_r <- subset(ankieta_clr_r, (ankieta_clr_r$zawod_matki != "Humanistyczny" & ankieta_clr_r$zawod_ojciec!="Humanistyczny") | (is.na(ankieta_clr_r$zawod_matki) & ankieta_clr_r$zawod_ojciec=="Humanistyczny") | (ankieta_clr_r$zawod_matki == "Humanistyczny" & is.na(ankieta_clr_r$zawod_ojciec)))
notes_not_humanistyczne<-data.frame(Polski=not_humanistyczne_r$ocena_jezyk_polski)
notes_not_humanistyczne_num<-data.frame(Polski=not_humanistyczne_r$ocena_jezyk_polski)
notes_not_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_not_humanistyczne[,"Polski"]))
mean_not_humanistyczne <- colMeans(notes_not_humanistyczne_num, na.rm = TRUE)

###PRZYRODNICZE
przyrodnicze_r <- subset(ankieta_clr_r, ankieta_clr_r$zawod_matki == "Przyrodniczy" | ankieta_clr_r$zawod_ojciec=="Przyrodniczy")
notes_przyrodnicze<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_przyrodnicze[,"Przyroda"]))
mean_przyrodnicze <- colMeans(notes_przyrodnicze_num, na.rm = TRUE)

not_przyrodnicze_r <- subset(ankieta_clr_r, (ankieta_clr_r$zawod_matki != "Przyrodniczy" & ankieta_clr_r$zawod_ojciec!="Przyrodniczy") | (is.na(ankieta_clr_r$zawod_matki) & ankieta_clr_r$zawod_ojciec=="Przyrodniczy") | (ankieta_clr_r$zawod_matki == "Przyrodniczy" & is.na(ankieta_clr_r$zawod_ojciec)))
notes_not_przyrodnicze<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_not_przyrodnicze[,"Przyroda"]))
mean_not_przyrodnicze <- colMeans(notes_not_przyrodnicze_num, na.rm = TRUE)

#### WYKRES SLUPKOWY
data <- structure(list(Matematyka=c(round(mean_techniczne,2),round(mean_not_techniczne,2)),
											 Polski=c(round(mean_humanistyczne,2),round(mean_not_humanistyczne,2)),
											 Przyroda=c(round(mean_przyrodnicze,2),round(mean_not_przyrodnicze,2))),
									.Names = c("Matematyka", "Jezyk polski", "Przyroda"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("green", "purple")
barplot(as.matrix(data), main="Czy oceny dzieci sa zalezne od typu pracy rodzicow?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Srednia ocen", ylim=c(0,6), legend=c("W zawodzie", "Poza zawodem"), col=colors)

## Oceny vs. Wymarzona praca
ankieta_clr_d<-subset(ankieta_clr, ankieta$zawod_u!="Inne" & !is.na(ankieta$zawod_u))

###TECHNICZNE
techniczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawod_u == "Techniczny")
notes_techniczne<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_techniczne[,"Matematyka"]))
mean_techniczne <- colMeans(notes_techniczne_num, na.rm = TRUE)

not_techniczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawod_u != "Techniczny")
notes_not_techniczne<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_not_techniczne[,"Matematyka"]))
mean_not_techniczne <- colMeans(notes_not_techniczne_num, na.rm = TRUE)

###HUMANISTYCZNE
humanistyczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawod_u == "Humanistyczny")
notes_humanistyczne<-data.frame(Polski=humanistyczne_r$ocena_jezyk_polski)
notes_humanistyczne_num<-data.frame(Polski=humanistyczne_r$ocena_jezyk_polski)
notes_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_humanistyczne[,"Polski"]))
mean_humanistyczne <- colMeans(notes_humanistyczne_num, na.rm = TRUE)

not_humanistyczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawod_u != "Humanistyczny")
notes_not_humanistyczne<-data.frame(Polski=not_humanistyczne_r$ocena_jezyk_polski)
notes_not_humanistyczne_num<-data.frame(Polski=not_humanistyczne_r$ocena_jezyk_polski)
notes_not_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_not_humanistyczne[,"Polski"]))
mean_not_humanistyczne <- colMeans(notes_not_humanistyczne_num, na.rm = TRUE)

###PRZYRODNICZE
przyrodnicze_r <- subset(ankieta_clr_d, ankieta_clr_d$zawod_u == "Przyrodniczy")
notes_przyrodnicze<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_przyrodnicze[,"Przyroda"]))
mean_przyrodnicze <- colMeans(notes_przyrodnicze_num, na.rm = TRUE)

not_przyrodnicze_r <- subset(ankieta_clr_d, ankieta_clr_d$zawod_u != "Przyrodniczy")
notes_not_przyrodnicze<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_not_przyrodnicze[,"Przyroda"]))
mean_not_przyrodnicze <- colMeans(notes_not_przyrodnicze_num, na.rm = TRUE)

#### WYKRES SLUPKOWY
data <- structure(list(Matematyka=c(round(mean_techniczne,2),round(mean_not_techniczne,2)),
											 Polski=c(round(mean_humanistyczne,2),round(mean_not_humanistyczne,2)),
											 Przyroda=c(round(mean_przyrodnicze,2),round(mean_not_przyrodnicze,2))),
									.Names = c("Matematyka", "Jezyk polski", "Przyroda"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("green", "purple")
barplot(as.matrix(data), main="Czy oceny dzieci sa zalezne od typu pracy, ktora chcialyby wykonywac?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Srednia ocen", ylim=c(0,6), legend=c("Do zawodu", "Poza zawodem"), col=colors)

#### CLUSTER

##### BOOKS VS GRADES
data<-data.frame(Matematyka=ankieta$ocena_matematyka,Polski=ankieta$ocena_jezyk_polski,Przyroda=ankieta$ocena_przyroda)
notes_num<-data.frame(Matematyka=ankieta$ocena_matematyka,Polski=ankieta$ocena_jezyk_polski,Przyroda=ankieta$ocena_przyroda)
notes_num[,"Matematyka"]<-as.numeric(as.character(data[,"Matematyka"]))
notes_num[,"Polski"]<-as.numeric(as.character(data[,"Polski"]))
notes_num[,"Przyroda"]<-as.numeric(as.character(data[,"Przyroda"]))
mean <- rowMeans(notes_num, na.rm = TRUE)
books <- as.vector(data.frame(Ksiazki=ankieta$p_16_2_4))
data[,"Mean"] = mean
data[,"Ksiazki"] = books
data <- na.omit(data)
mean <- data[,"Mean"]
levels(data$Ksiazki) <- c(levels(data$Ksiazki), "1")
data[data$Ksiazki=="nie ma ani jednej",]=1
levels(data$Ksiazki) <- c(levels(data$Ksiazki), "2")
data[data$Ksiazki=="kilka (mniej niz 20)",]=2
levels(data$Ksiazki) <- c(levels(data$Ksiazki), "3")
data[data$Ksiazki=="duzo (od 20 do 50)",]=3
levels(data$Ksiazki) <- c(levels(data$Ksiazki), "4")
data[data$Ksiazki=="bardzo duzo (wiecej niz 50 do 100)",]=4
levels(data$Ksiazki) <- c(levels(data$Ksiazki), "5")
data[data$Ksiazki=="cale mnostwo (wiecej niz 100)",]=5
data$Ksiazki<-droplevels(data$Ksiazki)
data[,"Mean"]<-mean

data_no_notes<-data.frame(Mean=data$Mean)
data_no_notes[,"Ksiazki"] = data$Ksiazki

ratio_ss=rep(0,7)
for (k in 1:7) {
  data_km<-kmeans(data, k, nstart=20)
  ratio_ss[k]<-data_km$tot.withinss/data_km$totss
}
plot(ratio_ss,type="b",xlab="k")
abline(h=0.2)

##### Z powyższego k=4
data_km<-kmeans(data, 4, nstart=20)

ratio_ss=rep(0,7)
for (k in 1:7) {
  data_no_notes_km<-kmeans(data_no_notes, k, nstart=20)
  ratio_ss[k]<-data_no_notes_km$tot.withinss/data_no_notes_km$totss
}
plot(ratio_ss,type="b",xlab="k")
abline(h=0.2)

##### Z powyższego k=6
data_km<-kmeans(data, 6, nstart=20)


par(mfrow = c(1, 2))
plot(data$Mean,data$Ksiazki,xlab="", ylab="Ilość książek", main="Klaster z ocenami", col=data_km$cluster, pch=16)
plot(data_no_notes$Mean,data_no_notes$Ksiazki, xlab="", ylab="", main="Klaster bez ocen",col=data_no_notes_km$cluster, pch=16)
title(sub="Średnia ocen", line=-2, outer=TRUE)
par(mfrow = c(1, 1))



# EKSPONATY

#Najpopularniejsze
istotne = filter(obserwacje, kategorie==2)

#galerie
liczba_odwiedzen_galerii = function(){
	top_galerie = arrange(count(istotne, galeria), desc(n))
	eksponaty_galerie = arrange(count(distinct(select(istotne, eksponat, galeria)), galeria), desc(n))
	joined = inner_join(top_galerie, eksponaty_galerie, by="galeria")
	joined$count_normalized = joined$n.x/joined$n.y
	names(joined) = c("Galeria", "Liczba odwiedzen", "Liczba eksponatow", "Znormalizowana liczba odwiedzen")
	rn = joined$Galeria
	joined = as.data.frame(joined[,2:4])
	row.names(joined) = rn
	
	joined_scaled = joined[,-2]
	scale = max(joined_scaled$`Liczba odwiedzen`) / max(joined_scaled$`Znormalizowana liczba odwiedzen`)
	joined_scaled$`Znormalizowana liczba odwiedzen` = joined_scaled$`Znormalizowana liczba odwiedzen` * scale
	joined_scaled = joined_scaled[order(joined_scaled$`Znormalizowana liczba odwiedzen`, decreasing = TRUE),]
	
	barplot(t(as.matrix(joined_scaled)), beside = TRUE, ylim=c(0, 4000), yaxt = "n", col = c("Red", "Green"), las=2)
	axis(2, seq(0, 4000, by=1000), col="Red")
	axis(4, pretty(seq(0, 4000, by=1000) / scale) * scale, pretty(seq(0, 4000, by=1000) / scale), col="Green" )
	legend("topright", legend = c("Nieprzetworzona", "Znormalizowana"), fill = c("Red", "Green"))
	title("Liczba odwiedzen galerii")
}

#Eksponaty
top_eksponaty_ever = function(){
	top_eksponaty = as.data.frame(head(arrange(count(istotne, eksponat, galeria), desc(n)), 6)) #top eksponaty
	names(top_eksponaty) = c("Eksponat", "Galeria", "Liczba odwiedzen")
	print(top_eksponaty)
}

#Eksponaty w obrebie galerii
top_eksponaty_w_galeriach = function(){
	top_eksponaty = arrange(count(istotne, eksponat, galeria), desc(n))
	names(top_eksponaty) = c("Eksponat", "Galeria", "Liczba odwiedzen")
	top_eksponaty_galerie = aggregate(.~Galeria, top_eksponaty, FUN = head, 1)
	top_eksponaty_galerie$`Liczba odwiedzen` = as.numeric(as.character(top_eksponaty_galerie$`Liczba odwiedzen`))
	print(arrange(top_eksponaty_galerie, desc(`Liczba odwiedzen`)))
}

#Najbardziej absorbujace eksponaty
m = 10

avg = function(x) { mean(as.numeric(as.character(x)), na.rm = TRUE)}

#Wg czasu
avg_czas_all = avg(istotne$czas_w_sek)
top_czas = arrange(summarize(group_by(istotne, eksponat, galeria), sredni_czas = mean(as.numeric(as.character(czas_w_sek))), n = length(czas_w_sek)), desc(sredni_czas))
top_czas_over_m = arrange(filter(top_czas, n>m), desc(sredni_czas))
top_czas_galerie = aggregate(.~galeria, top_czas_over_m, FUN = head, 1)[,-4]

istotne$zach[istotne$zach==2] = 1
istotne$zach[istotne$zach==3] = 2
istotne$zach[istotne$zach==4] = 3

avg_zaangazowanie_all = avg(istotne$zach)
top_zaangazowanie = arrange(summarize(group_by(istotne, eksponat, galeria), 
																			srednie_zaangazowanie = mean(as.numeric(as.character(zach))), n = length(zach)), desc(srednie_zaangazowanie))
top_zaangazowanie_over_m = arrange(filter(top_zaangazowanie, n>m), desc(srednie_zaangazowanie))
top_zaangazowanie_not_brak = head(arrange(filter(top_zaangazowanie, n>m, galeria != "<brak>"), desc(srednie_zaangazowanie)), 10)
top_zaangazowanie_galerie = aggregate(.~galeria, top_zaangazowanie_over_m, FUN = head, 1)[,-4]


avg_opis_all = avg(istotne$opis)
top_opis = arrange(summarize(group_by(istotne, eksponat, galeria), 
														 sredni_opis = mean(as.numeric(as.character(opis))), n = length(opis)), desc(sredni_opis))
top_opis_over_m = arrange(filter(top_opis, n>m), desc(sredni_opis))
top_opis_not_brak = head(arrange(filter(top_opis, n>m, galeria != "<brak>"), desc(sredni_opis)), 10)
top_opis_galerie = aggregate(.~galeria, top_opis_over_m, FUN = head, 1)[,-4]

avg_animator_all = avg(istotne$animator)
top_animator = arrange(summarize(group_by(istotne, eksponat, galeria), 
																 sredni_animator = mean(as.numeric(as.character(animator))), n = length(animator)), desc(sredni_animator))
top_animator_over_m = arrange(filter(top_animator, n>m), desc(sredni_animator))
top_animator_not_brak = head(arrange(filter(top_animator, n>m, galeria != "<brak>"), desc(sredni_animator)), 10)
top_animator_galerie = aggregate(.~galeria, top_animator_over_m, FUN = head, 1)[,-4]

#top eksponaty dla najwyzszych i najnizszych kapitalow
top_kapital = sqldf("select * from ankieta where exists (select 1 from istotne where ID = id_ucznia limit 1) order by k_sum_4 desc limit 20")
bottom_kapital = sqldf("select * from ankieta where exists (select 1 from istotne where ID = id_ucznia limit 1) order by k_sum_4 limit 20")

top_kapital_obserwacje = inner_join(top_kapital, istotne, by = c("id_ucznia" = "ID"))
bottom_kapital_obserwacje = inner_join(bottom_kapital, istotne, by = c("id_ucznia" = "ID"))

ankieta_obserwacje = inner_join(ankieta, obserwacje, by = c("id_ucznia" = "ID"))

top_eksponaty_top_kapital = as.data.frame(head(arrange(count(top_kapital_obserwacje, eksponat, galeria), desc(n)), 6))
top_eksponaty_bottom_kapital = as.data.frame(head(arrange(count(bottom_kapital_obserwacje, eksponat, galeria), desc(n)), 6))

#wielokrotnie odwiedzane
#wszystkie
liczba_odwiedzen_all = sqldf("select id_ucznia, eksponat, count(*) as liczba_odwiedzen from ankieta_obserwacje group by id_ucznia, eksponat")
srednia_liczba_odwiedzen_all = mean(liczba_odwiedzen_all$liczba_odwiedzen)

##top kapital
liczba_odwiedzen_top = sqldf("select id_ucznia, eksponat, count(*) as liczba_odwiedzen from top_kapital_obserwacje group by id_ucznia, eksponat")
srednia_liczba_odwiedzen_top = mean(liczba_odwiedzen_top$liczba_odwiedzen)

##bottom kapital
liczba_odwiedzen_bottom = sqldf("select id_ucznia, eksponat, count(*) as liczba_odwiedzen from bottom_kapital_obserwacje group by id_ucznia, eksponat")
srednia_liczba_odwiedzen_bottom = mean(liczba_odwiedzen_bottom$liczba_odwiedzen)

##top dzieci rzadziej odwiedzaja wiecej niz raz od wszystkich
wilcox.test(liczba_odwiedzen_all$liczba_odwiedzen, liczba_odwiedzen_top$liczba_odwiedzen, alternative = "greater")$p.value < 0.05

#bottom dzieci nie odwiedzaja czesciej od wszystkich
wilcox.test(liczba_odwiedzen_all$liczba_odwiedzen, liczba_odwiedzen_bottom$liczba_odwiedzen, alternative = "less")$p.value < 0.05

#unikalne odwiedziny
unikalne_eksponaty_all = distinct(ankieta_obserwacje, id_ucznia, eksponat)
unikalne_odwiedzenia_all = count(unikalne_eksponaty_all, id_ucznia)
srednie_unikalne_odwiedzenia_all = mean(unikalne_odwiedzenia_all$n)

unikalne_eksponaty_top = distinct(top_kapital_obserwacje, id_ucznia, eksponat)
unikalne_odwiedzenia_top = count(unikalne_eksponaty_top, id_ucznia)
srednie_unikalne_odwiedzenia_top = mean(unikalne_odwiedzenia_top$n)

unikalne_eksponaty_bottom = distinct(bottom_kapital_obserwacje, id_ucznia, eksponat)
unikalne_odwiedzenia_bottom = count(unikalne_eksponaty_bottom, id_ucznia)
srednie_unikalne_odwiedzenia_bottom = mean(unikalne_odwiedzenia_bottom$n)

#nie ma statystycznie istotnych roznic w liczbie unikalnych odwiedzonych
t.test(unikalne_odwiedzenia_all$n, unikalne_odwiedzenia_top$n, alternative = "less")$p.value < 0.05
t.test(unikalne_odwiedzenia_all$n, unikalne_odwiedzenia_bottom$n, alternative = "greater")$p.value < 0.05
t.test(unikalne_odwiedzenia_top$n, unikalne_odwiedzenia_bottom$n, alternative = "greater")$p.value < 0.05


#klasteryzacja eksponatow
numerize = function(x){ as.numeric(as.character(x)) }

summed = summarize(group_by(istotne, eksponat, ID), czas = sum(numerize(czas_w_sek)), zach = mean(numerize(zach)), n = n())
summed$ID = numerize(summed$ID)
summed = summed[!is.na(summed$czas) & !is.na(summed$zach),]

eksponaty = summarize(group_by(summed, eksponat), czas = mean(czas), zach = mean(zach), n = sum(n))
eksponaty = filter(eksponaty, n>10)

eksponaty_klastry = kmeans(eksponaty[,"czas","zach"], 4)

###Creating dataframe groups
obserwacje2 = obserwacje
bad_records = c()
for (i in 1:dim(obserwacje)[1])
{
  children = obserwacje[i, c(2, 13:20)]
  children = children[!is.na(children)]
  if (length(children) - 1 != obserwacje[i, "ILE_OSTOW"])
  {
    bad_records = c(bad_records, i)
  }
  children = sort(children)
  group_id = paste(children, collapse = ' ')
  obserwacje2[i, "grupka_id"] = group_id
  
}

obserwacje2 = obserwacje2[-bad_records, ]
obserwacje_obciete = distinct(obserwacje2, grupka_id, eksponat)
grupki = count(obserwacje_obciete, grupka_id)
for (i in 1:dim(grupki)[1])
{
  children_ids = unlist(strsplit(unlist(grupki[i, 'grupka_id']), " "))
  grupki[i, "liczba_dzieci"] = length(children_ids)
  children = filter(ankieta, id_ucznia %in% children_ids)
  d = grupki[i, "liczba_dziewczynek"] = count(filter(children, plec == "dziewczyna"))
  c = grupki[i, "liczba_chlopcow"] = count(filter(children, plec != "dziewczyna"))
  grupki[i, "procent_dziewczynek"] = d/(c + d)
}

grupki_wszystkie = grupki
grupki_jednoosobowe = filter(grupki_wszystkie, liczba_dzieci == 1)
grupki_jednoosobowe$grupka_id = as.numeric(grupki_jednoosobowe$grupka_id)
grupki = filter(grupki_wszystkie, liczba_dzieci > 1)

rozklad_licznosci_grupek = function()
{
  licznosci = summarize(group_by(grupki_wszystkie, liczba_dzieci), sum(n))
  barplot(unlist(licznosci[, 2]), names.arg = 1:9)
  title("Rozklad grupek roznych wielkosci")
}

liczba_eksponatow_podczas_wizyty = function()
{
  liczba_eksponatow = count(obserwacje, ID)
  obserwacje_ankieta = inner_join(obserwacje, ankieta, c("ID" = "id_ucznia"))
  liczba_eksponatow_dziewczynki = count(filter(obserwacje_ankieta, plec == "dziewczyna"), ID)
  liczba_eksponatow_chlopcy = count(filter(obserwacje_ankieta, plec != "dziewczyna"), ID)
  par(mfrow = c(3, 1))
  lim = c(10, 130) 
  boxplot(liczba_eksponatow[, "n"], ylim = lim, ylab ="Wszyscy", horizontal = TRUE)
  boxplot(liczba_eksponatow_dziewczynki[, "n"], ylim = lim, ylab ="Dziewczynki", varwidth=TRUE, horizontal = TRUE)
  boxplot(liczba_eksponatow_chlopcy[, "n"], ylim=lim, ylab ="Chlopcy", varwidth=TRUE, horizontal = TRUE)
  title(main="Liczba odwiedzen eksponatow", outer=TRUE)
  par(mfrow = c(1, 1))
  
}

razem_czy_osobno = function()
{
  count(distinct(obserwacje, ID))
  count(grupki_jednoosobowe)
  distinct(filter(left_join(filter(obserwacje, kategorie == 2), grupki_jednoosobowe, by=c("ID" = "grupka_id")), is.na(n)), ID)
  sqldf("select distinct ID from obserwacje ob where not exists(select * from obserwacje ob2 where ob.ID = ob2.ID and ILE_OSTOW > 0)")
}

grupki_plec = function()
{
  grupki_po_plci = unlist(c(count(filter(grupki, procent_dziewczynek == 1)),
                            count(filter(grupki, procent_dziewczynek == 0)),
                            count(filter(grupki, procent_dziewczynek != 1 & procent_dziewczynek != 0))))
  pie(grupki_po_plci, main="Liczba grupek ze wzgledu na plec", labels=c("dziewczece", "chlopiece", "mieszane"), col = c("pink", "lightblue", "white"))
}

trwale_grupki = function()
{
  print("Wszystkie grupki")
  print(arrange(grupki_wszystkie, desc(n))[1:15, c("n", "liczba_dzieci", "procent_dziewczynek")])
  grupki_wyczyszczone = arrange(filter(grupki, liczba_dzieci == liczba_dziewczynek + liczba_chlopcow), desc(n))
  print("Tylko grupki zawierajace co najmniej dwie osoby i da sie okreslic plec wszystkich dzieci")
  print(grupki_wyczyszczone[1:15, c("n", "liczba_dzieci", "procent_dziewczynek")])
}

