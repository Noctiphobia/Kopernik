#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi","nie pami?ta"))
obserwacje = read.csv('obserwacje.csv', na.strings = c("", " ", "NA"))

###Cleaning - ankieta

for (i in 1:dim(obserwacje)[2]){
	obserwacje[,i] = trimws(obserwacje[,i])
}

#Order levels
ankieta$klasyfikacja_z = ordered(ankieta$klasyfikacja_z,
                                  levels = c("ca?kowicie niezwi?zany z science",
                                             "po?rednio zwi?zany z science",
                                             "bezpo?rdnio zwi?zany z science"))
time_frequency = c(21, 38:49)
for (i in time_frequency)
{
  ankieta[, i] = ordered(ankieta[, i],
                   levels = c("nigdy albo rzadko (raz w roku)", "kilka razy w roku",
                              "raz lub dwa razy w miesi?cu", "raz w tygodniu",
                              "prawie codziennie"))
}

agreement_levels = c(50:60)
time_frequency = c(21, 38:49)
for (i in time_frequency)
{
  ankieta[, i] = ordered(ankieta[, i],
                         levels = c("zdecydowanie si? nie zgadzam", "raczej si? nie zgadzam",
                                    "ani si? zgadzam ani si? nie zgadzam", 
                                    "raczej si? zgadzam", "zdecydowanie si? zgadzam"))
}

ankieta$p_16_2_4 = ordered(ankieta$p_16_2_4,
                           levels = c("nie ma ani jednej", "kilka (mniej ni? 20)",
                                      "du?o (od 20 do 50)", "bardzo du?o (wi?cej ni? 50 do 100)",
                                      "ca?e mn?stwo (wi?cej ni? 100)"))


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

## Czy wiedza o rodzicach jest zale?na od p?ci?
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

#### WYKRESY KO?OWE
pie_chart_data_girls <- c(data_girls_know_studies_none, data_girls_know_studies_one, data_girls_know_studies_both)
percentlabels<- round(100*pie_chart_data_girls/num_girls, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_girls))
pie(pie_chart_data_girls, main="Czy dziewczynki wiedz? o studiach rodzic?w?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

pie_chart_data_boys <- c(data_boys_know_studies_none, data_boys_know_studies_one, data_boys_know_studies_both)
percentlabels<- round(100*pie_chart_data_boys/num_boys, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_boys))
pie(pie_chart_data_boys, main="Czy ch?opcy wiedz? o studiach rodzic?w?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

#### WYKRES S?UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_studies_none/num_girls,1),round(100*data_boys_know_studies_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_studies_one/num_girls,1),round(100*data_boys_know_studies_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_studies_both/num_girls,1),round(100*data_boys_know_studies_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz? o studiach rodzic?w?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch?opcy"), col=colors)

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

#### WYKRESY KO?OWE
pie_chart_data_girls <- c(data_girls_know_work_none, data_girls_know_work_one, data_girls_know_work_both)
percentlabels<- round(100*pie_chart_data_girls/num_girls, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_girls))
pie(pie_chart_data_girls, main="Czy dziewczynki wiedz? o pracy rodzic?w?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

pie_chart_data_boys <- c(data_boys_know_work_none, data_boys_know_work_one, data_boys_know_work_both)
percentlabels<- round(100*pie_chart_data_boys/num_boys, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_boys))
pie(pie_chart_data_boys, main="Czy ch?opcy wiedz? o pracy rodzic?w?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

#### WYKRES S?UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_work_none/num_girls,1),round(100*data_boys_know_work_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_work_one/num_girls,1),round(100*data_boys_know_work_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_work_both/num_girls,1),round(100*data_boys_know_work_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz? o pracy rodzic?w?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch?opcy"), args.legend = list(x ='topleft'), col=colors)

# OCENY
ankieta_clr = read.csv('ankieta_clr.csv')

## BOXPLOT
notes<-data.frame(Matematyka=ankieta_clr$ocena_matematyka,Polski=ankieta_clr$ocena_j?zyk_polski,Przyroda=ankieta_clr$ocena_przyroda)
boxplot(notes, ylab ="Oceny", xlab ="Przedmiot", main="Oceny z przedmiot?w", varwidth=TRUE)

## Oceny dzieci vs. studia rodzic?w
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
notes_studying<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jêzyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jêzyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_studying[,"Matematyka"]))
notes_studying_num[,"Polski"]<-as.numeric(as.character(notes_studying[,"Polski"]))
notes_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_studying[,"Przyroda"]))
mean_studying <- rowMeans(notes_studying_num, na.rm = TRUE)
cbind(notes_studying, Mean = mean_studying)

not_studying <- subset(ankieta, (ankieta$studia_m == "nie" & ankieta$studia_t=="nie") | (is.na(ankieta$studia_m) & ankieta$studia_t=="nie") | (ankieta$studia_m == "nie" & is.na(ankieta$studia_t)))
notes_not_studying<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jêzyk_polski,Przyroda=not_studying$ocena_przyroda)
notes_not_studying_num<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jêzyk_polski,Przyroda=not_studying$ocena_przyroda)
notes_not_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_not_studying[,"Matematyka"]))
notes_not_studying_num[,"Polski"]<-as.numeric(as.character(notes_not_studying[,"Polski"]))
notes_not_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_not_studying[,"Przyroda"]))
mean_not_studying <- rowMeans(notes_not_studying_num, na.rm = TRUE)
cbind(notes_not_studying, Mean = mean_not_studying)

lmts <- range(mean_studying,mean_not_studying, na.rm = TRUE)

par(mfrow = c(1, 2))
boxplot(mean_studying,ylim=lmts, ylab ="Œrednia ocen", xlab ="Ze studiami", varwidth=TRUE)
boxplot(mean_not_studying,ylim=lmts, xlab ="Bez studiów", varwidth=TRUE)
title(main="Czy studia rodziców wp³ywaj¹ na oceny dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Oceny dzieci vs. doping rodzic?w

zd_tak<-subset(ankieta, ankieta$p_19_b_4=="zdecydowanie siê zgadzam")
notes_zd_tak<-data.frame(Matematyka=zd_tak$ocena_matematyka,Polski=zd_tak$ocena_jêzyk_polski,Przyroda=zd_tak$ocena_przyroda)
notes_zd_tak_num<-data.frame(Matematyka=zd_tak$ocena_matematyka,Polski=zd_tak$ocena_jêzyk_polski,Przyroda=zd_tak$ocena_przyroda)
notes_zd_tak_num[,"Matematyka"]<-as.numeric(as.character(notes_zd_tak[,"Matematyka"]))
notes_zd_tak_num[,"Polski"]<-as.numeric(as.character(notes_zd_tak[,"Polski"]))
notes_zd_tak_num[,"Przyroda"]<-as.numeric(as.character(notes_zd_tak[,"Przyroda"]))
mean_zd_tak <- rowMeans(notes_zd_tak_num, na.rm = TRUE)
cbind(notes_zd_tak, Mean = mean_zd_tak)

tak<-subset(ankieta, ankieta$p_19_b_4=="raczej siê zgadzam")
notes_tak<-data.frame(Matematyka=tak$ocena_matematyka,Polski=tak$ocena_jêzyk_polski,Przyroda=tak$ocena_przyroda)
notes_tak_num<-data.frame(Matematyka=tak$ocena_matematyka,Polski=tak$ocena_jêzyk_polski,Przyroda=tak$ocena_przyroda)
notes_tak_num[,"Matematyka"]<-as.numeric(as.character(notes_tak[,"Matematyka"]))
notes_tak_num[,"Polski"]<-as.numeric(as.character(notes_tak[,"Polski"]))
notes_tak_num[,"Przyroda"]<-as.numeric(as.character(notes_tak[,"Przyroda"]))
mean_tak <- rowMeans(notes_tak_num, na.rm = TRUE)
cbind(notes_tak, Mean = mean_tak)

tak_nie<-subset(ankieta, ankieta$p_19_b_4=="ani siê zgadzam ani siê nie zgadzam")
notes_tak_nie<-data.frame(Matematyka=tak_nie$ocena_matematyka,Polski=tak_nie$ocena_jêzyk_polski,Przyroda=tak_nie$ocena_przyroda)
notes_tak_nie_num<-data.frame(Matematyka=tak_nie$ocena_matematyka,Polski=tak_nie$ocena_jêzyk_polski,Przyroda=tak_nie$ocena_przyroda)
notes_tak_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_tak_nie[,"Matematyka"]))
notes_tak_nie_num[,"Polski"]<-as.numeric(as.character(notes_tak_nie[,"Polski"]))
notes_tak_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_tak_nie[,"Przyroda"]))
mean_tak_nie <- rowMeans(notes_tak_nie_num, na.rm = TRUE)
cbind(notes_tak_nie, Mean = mean_tak_nie)

nie<-subset(ankieta, ankieta$p_19_b_4=="raczej siê nie zgadzam")
notes_nie<-data.frame(Matematyka=nie$ocena_matematyka,Polski=nie$ocena_jêzyk_polski,Przyroda=nie$ocena_przyroda)
notes_nie_num<-data.frame(Matematyka=nie$ocena_matematyka,Polski=nie$ocena_jêzyk_polski,Przyroda=nie$ocena_przyroda)
notes_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_nie[,"Matematyka"]))
notes_nie_num[,"Polski"]<-as.numeric(as.character(notes_nie[,"Polski"]))
notes_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_nie[,"Przyroda"]))
mean_nie <- rowMeans(notes_nie_num, na.rm = TRUE)
cbind(notes_nie, Mean = mean_nie)

zd_nie<-subset(ankieta, ankieta$p_19_b_4=="zdecydowanie siê nie zgadzam")
notes_zd_nie<-data.frame(Matematyka=zd_nie$ocena_matematyka,Polski=zd_nie$ocena_jêzyk_polski,Przyroda=zd_nie$ocena_przyroda)
notes_zd_nie_num<-data.frame(Matematyka=zd_nie$ocena_matematyka,Polski=zd_nie$ocena_jêzyk_polski,Przyroda=zd_nie$ocena_przyroda)
notes_zd_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_zd_nie[,"Matematyka"]))
notes_zd_nie_num[,"Polski"]<-as.numeric(as.character(notes_zd_nie[,"Polski"]))
notes_zd_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_zd_nie[,"Przyroda"]))
mean_zd_nie <- rowMeans(notes_zd_nie_num, na.rm = TRUE)
cbind(notes_zd_nie, Mean = mean_zd_nie)


lmts <- range(zd_tak,tak,tak_nie,nie,zd_nie, na.rm = TRUE)

par(mfrow = c(1, 5))
boxplot(mean_zd_tak,ylim=lmts, ylab ="Œrednia ocen", xlab ="Zdecydowanie siê zgadzam", varwidth=TRUE)
boxplot(mean_tak,ylim=lmts, xlab ="Raczej siê zgadzam", varwidth=TRUE)
boxplot(mean_tak_nie,ylim=lmts, xlab ="Ani siê zgadzam, ani siê nie zgadzam", varwidth=TRUE)
boxplot(mean_nie,ylim=lmts, xlab ="Raczej siê nie zgadzam", varwidth=TRUE)
boxplot(mean_zd_nie,ylim=lmts, xlab ="Zdecydowanie siê nie zgadzam", varwidth=TRUE)
title(main="Czy doping rodziców wp³ywa na oceny dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Kapita³ dzieci vs. studia rodziców
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
k_studying<-data.frame(Kapita³=studying$k_sum_4)
studies_points<-as.numeric(as.character(studying[,"k_studia_m"]))+as.numeric(as.character(studying[,"k_studia_t"]))
k_studying[,"Kapita³"]<-as.numeric(as.character(k_studying[,"Kapita³"]))-studies_points

not_studying <- subset(ankieta, (ankieta$studia_m == "nie" & ankieta$studia_t=="nie") | (is.na(ankieta$studia_m) & ankieta$studia_t=="nie") | (ankieta$studia_m == "nie" & is.na(ankieta$studia_t)))
k_not_studying<-data.frame(Kapita³=not_studying$k_sum_4)
studies_points<-as.numeric(as.character(not_studying[,"k_studia_m"]))+as.numeric(as.character(not_studying[,"k_studia_t"]))
k_not_studying[,"Kapita³"]<-as.numeric(as.character(k_not_studying[,"Kapita³"]))-studies_points

lmts <- range(k_studying,k_not_studying, na.rm = TRUE)

par(mfrow = c(1, 2))
boxplot(k_studying,ylim=lmts, ylab ="Kapita³ naukowy", xlab ="Ze studiami", varwidth=TRUE)
boxplot(k_not_studying,ylim=lmts, xlab ="Bez studiów", varwidth=TRUE)
title(main="Czy studia rodziców wp³ywaj¹ na kapita³ naukowy dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Kapita³ dzieci (bez ocen) vs. studia rodziców
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
k_studying<-data.frame(Kapita³=studying$k_sum_4)
studies_points<-as.numeric(as.character(studying[,"k_studia_m"]))+as.numeric(as.character(studying[,"k_studia_t"]))
k_studying[,"Kapita³"]<-as.numeric(as.character(k_studying[,"Kapita³"]))-studies_points

notes_studying<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jêzyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jêzyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_studying[,"Matematyka"]))
notes_studying_num[,"Polski"]<-as.numeric(as.character(notes_studying[,"Polski"]))
notes_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_studying[,"Przyroda"]))

sum_studying <- rowSums(notes_studying_num, na.rm = TRUE)
cbind(notes_studying, Sum = sum_studying)
k_studying <- k_studying - sum_studying

not_studying <- subset(ankieta, (ankieta$studia_m == "nie" & ankieta$studia_t=="nie") | (is.na(ankieta$studia_m) & ankieta$studia_t=="nie") | (ankieta$studia_m == "nie" & is.na(ankieta$studia_t)))
k_not_studying<-data.frame(Kapita³=not_studying$k_sum_4)
studies_points<-as.numeric(as.character(not_studying[,"k_studia_m"]))+as.numeric(as.character(not_studying[,"k_studia_t"]))
k_not_studying[,"Kapita³"]<-as.numeric(as.character(k_not_studying[,"Kapita³"]))-studies_points

notes_not_studying<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jêzyk_polski,Przyroda=not_studying$ocena_przyroda)
notes_not_studying_num<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jêzyk_polski,Przyroda=not_studying$ocena_przyroda)
notes_not_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_not_studying[,"Matematyka"]))
notes_not_studying_num[,"Polski"]<-as.numeric(as.character(notes_not_studying[,"Polski"]))
notes_not_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_not_studying[,"Przyroda"]))

sum_not_studying <- rowSums(notes_not_studying_num, na.rm = TRUE)
cbind(notes_not_studying, Sum = sum_not_studying)
k_not_studying <- k_not_studying - sum_not_studying


lmts <- range(k_studying,k_not_studying, na.rm = TRUE)

par(mfrow = c(1, 2))
boxplot(k_studying,ylim=lmts, ylab ="Kapita³ naukowy", xlab ="Ze studiami", varwidth=TRUE)
boxplot(k_not_studying,ylim=lmts, xlab ="Bez studiów", varwidth=TRUE)
title(main="Czy studia rodziców wp³ywaj¹ na kapita³ naukowy(bez ocen) dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Kapita³ (bez ocen) vs. oceny
kapital<-data.frame(Kapita³=ankieta$k_sum_4)
kapital[,"Kapita³"]<-as.numeric(as.character(kapital[,"Kapita³"]))

notes_studying<-data.frame(Matematyka=ankieta$ocena_matematyka,Polski=ankieta$ocena_jêzyk_polski,Przyroda=ankieta$ocena_przyroda)
notes_studying_num<-data.frame(Matematyka=ankieta$ocena_matematyka,Polski=ankieta$ocena_jêzyk_polski,Przyroda=ankieta$ocena_przyroda)
notes_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_studying[,"Matematyka"]))
notes_studying_num[,"Polski"]<-as.numeric(as.character(notes_studying[,"Polski"]))
notes_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_studying[,"Przyroda"]))

sum_studying <- rowSums(notes_studying_num, na.rm = TRUE)
mean_studying <- rowMeans(notes_studying_num, na.rm = TRUE)

kapital <- as.numeric(unlist(as.vector(kapital - sum_studying)))

plot(kapital, mean_studying, main="Kapita³ vs Oceny", xlab="Kapita³ (bez ocen) ", ylab="Œrednia ocen")

## Oceny vs. Liczba ksi¹¿ek
more_100<-subset(ankieta, ankieta$p_16_2_4=="ca³e mnóstwo (wiêcej ni¿ 100)")
notes_more_100<-data.frame(Matematyka=more_100$ocena_matematyka,Polski=more_100$ocena_jêzyk_polski,Przyroda=more_100$ocena_przyroda)
notes_more_100_num<-data.frame(Matematyka=more_100$ocena_matematyka,Polski=more_100$ocena_jêzyk_polski,Przyroda=more_100$ocena_przyroda)
notes_more_100_num[,"Matematyka"]<-as.numeric(as.character(notes_more_100[,"Matematyka"]))
notes_more_100_num[,"Polski"]<-as.numeric(as.character(notes_more_100[,"Polski"]))
notes_more_100_num[,"Przyroda"]<-as.numeric(as.character(notes_more_100[,"Przyroda"]))
mean_more_100 <- rowMeans(notes_more_100_num, na.rm = TRUE)
cbind(notes_more_100, Mean = mean_more_100)

bduzo<-subset(ankieta, ankieta$p_16_2_4=="bardzo du¿o (wiêcej ni¿ 50 do 100)")
notes_bduzo<-data.frame(Matematyka=bduzo$ocena_matematyka,Polski=bduzo$ocena_jêzyk_polski,Przyroda=bduzo$ocena_przyroda)
notes_bduzo_num<-data.frame(Matematyka=bduzo$ocena_matematyka,Polski=bduzo$ocena_jêzyk_polski,Przyroda=bduzo$ocena_przyroda)
notes_bduzo_num[,"Matematyka"]<-as.numeric(as.character(notes_bduzo[,"Matematyka"]))
notes_bduzo_num[,"Polski"]<-as.numeric(as.character(notes_bduzo[,"Polski"]))
notes_bduzo_num[,"Przyroda"]<-as.numeric(as.character(notes_bduzo[,"Przyroda"]))
mean_bduzo <- rowMeans(notes_bduzo_num, na.rm = TRUE)
cbind(notes_bduzo, Mean = mean_bduzo)

duzo<-subset(ankieta, ankieta$p_16_2_4=="du¿o (od 20 do 50)")
notes_duzo<-data.frame(Matematyka=duzo$ocena_matematyka,Polski=duzo$ocena_jêzyk_polski,Przyroda=duzo$ocena_przyroda)
notes_duzo_num<-data.frame(Matematyka=duzo$ocena_matematyka,Polski=duzo$ocena_jêzyk_polski,Przyroda=duzo$ocena_przyroda)
notes_duzo_num[,"Matematyka"]<-as.numeric(as.character(notes_duzo[,"Matematyka"]))
notes_duzo_num[,"Polski"]<-as.numeric(as.character(notes_duzo[,"Polski"]))
notes_duzo_num[,"Przyroda"]<-as.numeric(as.character(notes_duzo[,"Przyroda"]))
mean_duzo <- rowMeans(notes_duzo_num, na.rm = TRUE)
cbind(notes_duzo, Mean = mean_duzo)

kilka<-subset(ankieta, ankieta$p_16_2_4=="kilka (mniej niÅ¼ 20)")
notes_kilka<-data.frame(Matematyka=kilka$ocena_matematyka,Polski=kilka$ocena_jêzyk_polski,Przyroda=kilka$ocena_przyroda)
notes_kilka_num<-data.frame(Matematyka=kilka$ocena_matematyka,Polski=kilka$ocena_jêzyk_polski,Przyroda=kilka$ocena_przyroda)
notes_kilka_num[,"Matematyka"]<-as.numeric(as.character(notes_kilka[,"Matematyka"]))
notes_kilka_num[,"Polski"]<-as.numeric(as.character(notes_kilka[,"Polski"]))
notes_kilka_num[,"Przyroda"]<-as.numeric(as.character(notes_kilka[,"Przyroda"]))
mean_kilka <- rowMeans(notes_kilka_num, na.rm = TRUE)
cbind(notes_kilka, Mean = mean_kilka)

zero<-subset(ankieta, ankieta$p_16_2_4=="nie ma ani jednej")
notes_zero<-data.frame(Matematyka=zero$ocena_matematyka,Polski=zero$ocena_jêzyk_polski,Przyroda=zero$ocena_przyroda)
notes_zero_num<-data.frame(Matematyka=zero$ocena_matematyka,Polski=zero$ocena_jêzyk_polski,Przyroda=zero$ocena_przyroda)
notes_zero_num[,"Matematyka"]<-as.numeric(as.character(notes_zero[,"Matematyka"]))
notes_zero_num[,"Polski"]<-as.numeric(as.character(notes_zero[,"Polski"]))
notes_zero_num[,"Przyroda"]<-as.numeric(as.character(notes_zero[,"Przyroda"]))
mean_zero <- rowMeans(notes_zero_num, na.rm = TRUE)
cbind(notes_zero, Mean = mean_zero)

lmts <- range(mean_more_100,mean_bduzo,mean_duzo,mean_kilka,mean_zero, na.rm = TRUE)

par(mfrow = c(1, 5))
boxplot(mean_more_100,ylim=lmts, ylab ="Œrednia ocen", xlab ="Ca³e mnóstwo (wiêcej ni¿ 100)", varwidth=TRUE)
boxplot(mean_bduzo,ylim=lmts, xlab ="Bardzo du¿o (wiêcej ni¿ 50 do 100)", varwidth=TRUE)
boxplot(mean_duzo,ylim=lmts, xlab ="Du¿o (od 20 do 50)", varwidth=TRUE)
boxplot(mean_kilka,ylim=lmts, xlab ="Kilka (mniej ni¿ 20)", varwidth=TRUE)
boxplot(mean_zero,ylim=lmts, xlab ="Nie ma ani jednej", varwidth=TRUE)
title(main="Czy iloœæ ksi¹¿ek w domu wp³ywa na oceny dzieci?", line = -2, outer=TRUE)
par(mfrow = c(1, 1))

## Oceny vs. Praca rodziców
ankieta_clr_r<-subset(ankieta_clr, (ankieta$zawód_matki!="Inne" & ankieta$zawód_ojciec!="Inne") & (!is.na(ankieta$zawód_matki) & !is.na(ankieta$zawód_ojciec)))

###TECHNICZNE
techniczne_r <- subset(ankieta_clr_r, ankieta_clr_r$zawód_matki == "Techniczny" | ankieta_clr_r$zawód_ojciec=="Techniczny")
notes_techniczne<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_techniczne[,"Matematyka"]))
mean_techniczne <- colMeans(notes_techniczne_num, na.rm = TRUE)

not_techniczne_r <- subset(ankieta_clr_r, (ankieta_clr_r$zawód_matki != "Techniczny" & ankieta_clr_r$zawód_ojciec!="Techniczny") | (is.na(ankieta_clr_r$zawód_matki) & ankieta_clr_r$zawód_ojciec=="Techniczny") | (ankieta_clr_r$zawód_matki == "Techniczny" & is.na(ankieta_clr_r$zawód_ojciec)))
notes_not_techniczne<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_not_techniczne[,"Matematyka"]))
mean_not_techniczne <- colMeans(notes_not_techniczne_num, na.rm = TRUE)

###HUMANISTYCZNE
humanistyczne_r <- subset(ankieta_clr_r, ankieta_clr_r$zawód_matki == "Humanistyczny" | ankieta_clr_r$zawód_ojciec=="Humanistyczny")
notes_humanistyczne<-data.frame(Polski=humanistyczne_r$ocena_jêzyk_polski)
notes_humanistyczne_num<-data.frame(Polski=humanistyczne_r$ocena_jêzyk_polski)
notes_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_humanistyczne[,"Polski"]))
mean_humanistyczne <- colMeans(notes_humanistyczne_num, na.rm = TRUE)

not_humanistyczne_r <- subset(ankieta_clr_r, (ankieta_clr_r$zawód_matki != "Humanistyczny" & ankieta_clr_r$zawód_ojciec!="Humanistyczny") | (is.na(ankieta_clr_r$zawód_matki) & ankieta_clr_r$zawód_ojciec=="Humanistyczny") | (ankieta_clr_r$zawód_matki == "Humanistyczny" & is.na(ankieta_clr_r$zawód_ojciec)))
notes_not_humanistyczne<-data.frame(Polski=not_humanistyczne_r$ocena_jêzyk_polski)
notes_not_humanistyczne_num<-data.frame(Polski=not_humanistyczne_r$ocena_jêzyk_polski)
notes_not_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_not_humanistyczne[,"Polski"]))
mean_not_humanistyczne <- colMeans(notes_not_humanistyczne_num, na.rm = TRUE)

###PRZYRODNICZE
przyrodnicze_r <- subset(ankieta_clr_r, ankieta_clr_r$zawód_matki == "Przyrodniczy" | ankieta_clr_r$zawód_ojciec=="Przyrodniczy")
notes_przyrodnicze<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_przyrodnicze[,"Przyroda"]))
mean_przyrodnicze <- colMeans(notes_przyrodnicze_num, na.rm = TRUE)

not_przyrodnicze_r <- subset(ankieta_clr_r, (ankieta_clr_r$zawód_matki != "Przyrodniczy" & ankieta_clr_r$zawód_ojciec!="Przyrodniczy") | (is.na(ankieta_clr_r$zawód_matki) & ankieta_clr_r$zawód_ojciec=="Przyrodniczy") | (ankieta_clr_r$zawód_matki == "Przyrodniczy" & is.na(ankieta_clr_r$zawód_ojciec)))
notes_not_przyrodnicze<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_not_przyrodnicze[,"Przyroda"]))
mean_not_przyrodnicze <- colMeans(notes_not_przyrodnicze_num, na.rm = TRUE)

#### WYKRES S£UPKOWY
data <- structure(list(Matematyka=c(round(mean_techniczne,2),round(mean_not_techniczne,2)),
                       Polski=c(round(mean_humanistyczne,2),round(mean_not_humanistyczne,2)),
                       Przyroda=c(round(mean_przyrodnicze,2),round(mean_not_przyrodnicze,2))),
                  .Names = c("Matematyka", "Jêzyk polski", "Przyroda"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("green", "purple")
barplot(as.matrix(data), main="Czy oceny dzieci s¹ zale¿ne od typu pracy rodziców?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Œrednia ocen", ylim=c(0,6), legend=c("W zawodzie", "Poza zawodem"), col=colors)

## Oceny vs. Wymarzona praca
ankieta_clr_d<-subset(ankieta_clr, ankieta$zawód_u!="Inne" & !is.na(ankieta$zawód_u))

###TECHNICZNE
techniczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawód_u == "Techniczny")
notes_techniczne<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num<-data.frame(Matematyka=techniczne_r$ocena_matematyka)
notes_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_techniczne[,"Matematyka"]))
mean_techniczne <- colMeans(notes_techniczne_num, na.rm = TRUE)

not_techniczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawód_u != "Techniczny")
notes_not_techniczne<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num<-data.frame(Matematyka=not_techniczne_r$ocena_matematyka)
notes_not_techniczne_num[,"Matematyka"]<-as.numeric(as.character(notes_not_techniczne[,"Matematyka"]))
mean_not_techniczne <- colMeans(notes_not_techniczne_num, na.rm = TRUE)

###HUMANISTYCZNE
humanistyczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawód_u == "Humanistyczny")
notes_humanistyczne<-data.frame(Polski=humanistyczne_r$ocena_jêzyk_polski)
notes_humanistyczne_num<-data.frame(Polski=humanistyczne_r$ocena_jêzyk_polski)
notes_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_humanistyczne[,"Polski"]))
mean_humanistyczne <- colMeans(notes_humanistyczne_num, na.rm = TRUE)

not_humanistyczne_r <- subset(ankieta_clr_d, ankieta_clr_d$zawód_u != "Humanistyczny")
notes_not_humanistyczne<-data.frame(Polski=not_humanistyczne_r$ocena_jêzyk_polski)
notes_not_humanistyczne_num<-data.frame(Polski=not_humanistyczne_r$ocena_jêzyk_polski)
notes_not_humanistyczne_num[,"Polski"]<-as.numeric(as.character(notes_not_humanistyczne[,"Polski"]))
mean_not_humanistyczne <- colMeans(notes_not_humanistyczne_num, na.rm = TRUE)

###PRZYRODNICZE
przyrodnicze_r <- subset(ankieta_clr_d, ankieta_clr_d$zawód_u == "Przyrodniczy")
notes_przyrodnicze<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num<-data.frame(Przyroda=przyrodnicze_r$ocena_przyroda)
notes_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_przyrodnicze[,"Przyroda"]))
mean_przyrodnicze <- colMeans(notes_przyrodnicze_num, na.rm = TRUE)

not_przyrodnicze_r <- subset(ankieta_clr_d, ankieta_clr_d$zawód_u != "Przyrodniczy")
notes_not_przyrodnicze<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num<-data.frame(Przyroda=not_przyrodnicze_r$ocena_przyroda)
notes_not_przyrodnicze_num[,"Przyroda"]<-as.numeric(as.character(notes_not_przyrodnicze[,"Przyroda"]))
mean_not_przyrodnicze <- colMeans(notes_not_przyrodnicze_num, na.rm = TRUE)

#### WYKRES S£UPKOWY
data <- structure(list(Matematyka=c(round(mean_techniczne,2),round(mean_not_techniczne,2)),
                       Polski=c(round(mean_humanistyczne,2),round(mean_not_humanistyczne,2)),
                       Przyroda=c(round(mean_przyrodnicze,2),round(mean_not_przyrodnicze,2))),
                  .Names = c("Matematyka", "Jêzyk polski", "Przyroda"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("green", "purple")
barplot(as.matrix(data), main="Czy oceny dzieci s¹ zale¿ne od typu pracy, któr¹ chcia³yby wykonywaæ?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Œrednia ocen", ylim=c(0,6), legend=c("Do zawodu", "Poza zawodem"), col=colors)


# EKSPONATY

#Najpopularniejsze
library(dplyr)
istotne = filter(obserwacje, kategorie==2)

#galerie
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


#Eksponaty
top_eksponaty = as.data.frame(head(arrange(count(istotne, eksponat, galeria), desc(n)), 6)) #top eksponaty
names(top_eksponaty) = c("Eksponat", "Galeria", "Liczba odwiedzen")
top_eksponaty

#Eksponaty w obrebie galerii
top_eksponaty = arrange(count(istotne, eksponat, galeria), desc(n))
names(top_eksponaty) = c("Eksponat", "Galeria", "Liczba odwiedzen")
top_eksponaty_galerie = aggregate(.~Galeria, top_eksponaty, FUN = head, 1)
top_eksponaty_galerie$`Liczba odwiedzen` = as.numeric(as.character(top_eksponaty_galerie$`Liczba odwiedzen`))
arrange(top_eksponaty_galerie, desc(`Liczba odwiedzen`))

#Najbardziej absorbujÄ…ce eksponaty
m = 10

avg = function(x) { mean(as.numeric(as.character(x)), na.rm = TRUE)}

#Wg czasu
avg_czas_all = avg(istotne$czas_w_sek)
top_czas = arrange(summarize(group_by(istotne, eksponat, galeria), sredni_czas = mean(as.numeric(as.character(czas_w_sek))), n = length(czas_w_sek)), desc(sredni_czas))
top_czas_over_m = arrange(filter(top_czas, n>m), desc(sredni_czas))
top_czas_galerie = aggregate(.~galeria, top_czas_over_m, FUN = head, 1)[,-4]


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
library(sqldf)
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
mean(liczba_odwiedzen_all$liczba_odwiedzen)

##top kapital
liczba_odwiedzen_top = sqldf("select id_ucznia, eksponat, count(*) as liczba_odwiedzen from top_kapital_obserwacje group by id_ucznia, eksponat")
mean(liczba_odwiedzen_top$liczba_odwiedzen)

##bottom kapital
liczba_odwiedzen_bottom = sqldf("select id_ucznia, eksponat, count(*) as liczba_odwiedzen from bottom_kapital_obserwacje group by id_ucznia, eksponat")
mean(liczba_odwiedzen_bottom$liczba_odwiedzen)

##top dzieci rzadziej odwiedzaja wiecej niz raz od wszystkich
t.test(liczba_odwiedzen_all$liczba_odwiedzen, liczba_odwiedzen_top$liczba_odwiedzen, alternative = "greater")$p.value < 0.05

#bottom dzieci nie odwiedzaja czesciej od wszystkich
t.test(liczba_odwiedzen_all$liczba_odwiedzen, liczba_odwiedzen_bottom$liczba_odwiedzen, alternative = "less")$p.value < 0.05

#unikalne odwiedziny
unikalne_eksponaty_all = distinct(ankieta_obserwacje, id_ucznia, eksponat)
unikalne_odwiedzenia_all = count(unikalne_eksponaty_all, id_ucznia)
mean(unikalne_odwiedzenia_all$n)

unikalne_eksponaty_top = distinct(top_kapital_obserwacje, id_ucznia, eksponat)
unikalne_odwiedzenia_top = count(unikalne_eksponaty_top, id_ucznia)
mean(unikalne_odwiedzenia_top$n)

unikalne_eksponaty_bottom = distinct(bottom_kapital_obserwacje, id_ucznia, eksponat)
unikalne_odwiedzenia_bottom = count(unikalne_eksponaty_bottom, id_ucznia)
mean(unikalne_odwiedzenia_bottom$n)

#nie ma statystycznie istotnych roznic w liczbie unikalnych odwiedzonych
t.test(unikalne_odwiedzenia_all$n, unikalne_odwiedzenia_top$n, alternative = "less")$p.value < 0.05
t.test(unikalne_odwiedzenia_all$n, unikalne_odwiedzenia_bottom$n, alternative = "greater")$p.value < 0.05
t.test(unikalne_odwiedzenia_top$n, unikalne_odwiedzenia_bottom$n, alternative = "greater")$p.value < 0.05

###Creating dataframe groups
library(dplyr)
library(sqldf)
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

rozk³ad_licznosci_grupek = function()
{
  licznosci = summarize(group_by(grupki_wszystkie, liczba_dzieci), sum(n))
  barplot(unlist(licznosci[, 2]), names.arg = 1:9)
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
  boxplot(liczba_eksponatow_chlopcy[, "n"], ylim=lim, ylab ="Ch³opcy", varwidth=TRUE, horizontal = TRUE)
  title(main="Liczba odwiedzeñ eksponatów", outer=TRUE)
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
  pie(grupki_po_plci, main="Liczba grupek ze wzglêdu na p³eæ", labels=c("dziewczêce", "ch³opiêce", "mieszane"), col = c("pink", "lightblue", "white"))
}

trwale_grupki = function()
{
  print("Wszystkie grupki")
  arrange(grupki_wszystkie, desc(n))
  grupki_wyczyszczone = arrange(filter(grupki, liczba_dzieci == liczba_dziewczynek + liczba_chlopcow), desc(n))
  print("Tylko grupki zawieraj¹ce co najmniej dwie osoby i da siê okreœliæ p³eæ wszystkich dzieci")
  print(grupki_wyczyszczone)
}

