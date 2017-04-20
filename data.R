setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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

## Czy wiedza o rodzicach jest zale¿na od p³ci?
### STUDIA
#### DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_studies_none <- nrow(subset(data_girls, is.na(data_girls$studia_m) & is.na(data_girls$studia_t)))
data_girls_know_studies_both <- nrow(subset(data_girls, !is.na(data_girls$studia_m) & !is.na(data_girls$studia_t)))
data_girls_know_studies_one <- nrow(data_girls) - data_girls_know_studies_both - data_girls_know_studies_none

data_boys <- subset(ankieta, ankieta$plec == "ch?opak")
data_boys_know_studies_none <- nrow(subset(data_boys, is.na(data_boys$studia_m) & is.na(data_boys$studia_t)))
data_boys_know_studies_both <- nrow(subset(data_boys, !is.na(data_boys$studia_m) & !is.na(data_boys$studia_t)))
data_boys_know_studies_one <- nrow(data_boys) - data_boys_know_studies_both - data_boys_know_studies_none

num_girls = nrow(data_girls)
num_boys = nrow(data_boys)

#### WYKRESY KO£OWE
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

#### WYKRES S£UPKOWY
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

data_boys <- subset(ankieta, ankieta$plec == "ch?opak")
data_boys_know_work_none <- nrow(subset(data_boys, is.na(data_boys$praca_m) & is.na(data_boys$praca_o)))
data_boys_know_work_both <- nrow(subset(data_boys, !is.na(data_boys$praca_m) & !is.na(data_boys$praca_o)))
data_boys_know_work_one <- nrow(data_boys) - data_boys_know_work_both - data_boys_know_work_none

num_girls = nrow(data_girls)
num_boys = nrow(data_boys)

#### WYKRESY KO£OWE
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

#### WYKRES S£UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_work_none/num_girls,1),round(100*data_boys_know_work_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_work_one/num_girls,1),round(100*data_boys_know_work_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_work_both/num_girls,1),round(100*data_boys_know_work_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz? o pracy rodzic?w?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch?opcy"), args.legend = list(x ='topleft'), col=colors)

# OCENY
ankieta_clr = read.csv('ankieta_clr.csv')

## BOXPLOT
notes<-data.frame(Matematyka=ankieta_clr$ocena_matematyka,Polski=ankieta_clr$ocena_jêzyk_polski,Przyroda=ankieta_clr$ocena_przyroda)
boxplot(notes, ylab ="Oceny", xlab ="Przedmiot", main="Oceny z przedmiotów", varwidth=TRUE)

## Oceny dzieci vs. studia rodziców
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
notes_studying<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jêzyk_polski,Przyroda=studying$ocena_przyroda)
notes_studying_num[,"Matematyka"]<-as.numeric(as.character(notes_studying[,"Matematyka"]))
notes_studying_num[,"Polski"]<-as.numeric(as.character(notes_studying[,"Polski"]))
notes_studying_num[,"Przyroda"]<-as.numeric(as.character(notes_studying[,"Przyroda"]))
mean_studying <- rowMeans(notes_studying_num, na.rm = TRUE)
cbind(notes_studying, Mean = mean_studying)

not_studying <- subset(ankieta, (ankieta$studia_m == "nie" & ankieta$studia_t=="nie") | (is.na(ankieta$studia_m) & ankieta$studia_t=="nie") | (ankieta$studia_m == "nie" & is.na(ankieta$studia_t)))
notes_not_studying<-data.frame(Matematyka=not_studying$ocena_matematyka,Polski=not_studying$ocena_jêzyk_polski,Przyroda=not_studying$ocena_przyroda)
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

## Oceny dzieci vs. doping rodziców

zd_tak<-subset(ankieta, ankieta$p_19_b_4=="zdecydowanie siê zgadzam")
notes_zd_tak<-data.frame(Matematyka=zd_tak$ocena_matematyka,Polski=zd_tak$ocena_jêzyk_polski,Przyroda=zd_tak$ocena_przyroda)
notes_zd_tak_num[,"Matematyka"]<-as.numeric(as.character(notes_zd_tak[,"Matematyka"]))
notes_zd_tak_num[,"Polski"]<-as.numeric(as.character(notes_zd_tak[,"Polski"]))
notes_zd_tak_num[,"Przyroda"]<-as.numeric(as.character(notes_zd_tak[,"Przyroda"]))
mean_zd_tak <- rowMeans(notes_zd_tak_num, na.rm = TRUE)
cbind(notes_zd_tak, Mean = mean_zd_tak)

tak<-subset(ankieta, ankieta$p_19_b_4=="raczej siê zgadzam")
notes_tak<-data.frame(Matematyka=tak$ocena_matematyka,Polski=tak$ocena_jêzyk_polski,Przyroda=tak$ocena_przyroda)
notes_tak_num[,"Matematyka"]<-as.numeric(as.character(notes_tak[,"Matematyka"]))
notes_tak_num[,"Polski"]<-as.numeric(as.character(notes_tak[,"Polski"]))
notes_tak_num[,"Przyroda"]<-as.numeric(as.character(notes_tak[,"Przyroda"]))
mean_tak <- rowMeans(notes_tak_num, na.rm = TRUE)
cbind(notes_tak, Mean = mean_tak)

tak_nie<-subset(ankieta, ankieta$p_19_b_4=="ani siê zgadzam ani siê nie zgadzam")
notes_tak_nie<-data.frame(Matematyka=tak_nie$ocena_matematyka,Polski=tak_nie$ocena_jêzyk_polski,Przyroda=tak_nie$ocena_przyroda)
notes_tak_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_tak_nie[,"Matematyka"]))
notes_tak_nie_num[,"Polski"]<-as.numeric(as.character(notes_tak_nie[,"Polski"]))
notes_tak_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_tak_nie[,"Przyroda"]))
mean_tak_nie <- rowMeans(notes_tak_nie_num, na.rm = TRUE)
cbind(notes_tak_nie, Mean = mean_tak_nie)

nie<-subset(ankieta, ankieta$p_19_b_4=="raczej siê nie zgadzam")
notes_nie<-data.frame(Matematyka=nie$ocena_matematyka,Polski=nie$ocena_jêzyk_polski,Przyroda=nie$ocena_przyroda)
notes_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_nie[,"Matematyka"]))
notes_nie_num[,"Polski"]<-as.numeric(as.character(notes_nie[,"Polski"]))
notes_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_nie[,"Przyroda"]))
mean_nie <- rowMeans(notes_nie_num, na.rm = TRUE)
cbind(notes_nie, Mean = mean_nie)

zd_nie<-subset(ankieta, ankieta$p_19_b_4=="zdecydowanie siê nie zgadzam")
notes_zd_nie<-data.frame(Matematyka=zd_nie$ocena_matematyka,Polski=zd_nie$ocena_jêzyk_polski,Przyroda=zd_nie$ocena_przyroda)
notes_zd_nie_num[,"Matematyka"]<-as.numeric(as.character(notes_zd_nie[,"Matematyka"]))
notes_zd_nie_num[,"Polski"]<-as.numeric(as.character(notes_zd_nie[,"Polski"]))
notes_zd_nie_num[,"Przyroda"]<-as.numeric(as.character(notes_zd_nie[,"Przyroda"]))
mean_zd_nie <- rowMeans(notes_zd_nie_num, na.rm = TRUE)
cbind(notes_zd_nie, Mean = mean_zd_nie)

par(mfrow = c(1, 5))
boxplot(mean_zd_tak,ylim=lmts, ylab ="Œrednia ocen", xlab ="Zdecydowanie siê zgadzam", varwidth=TRUE)
boxplot(mean_tak,ylim=lmts, xlab ="Raczej siê zgadzam", varwidth=TRUE)
boxplot(mean_tak_nie,ylim=lmts, xlab ="Ani siê zgadzam, ani siê nie zgadzam", varwidth=TRUE)
boxplot(mean_nie,ylim=lmts, xlab ="Raczej siê nie zgadzam", varwidth=TRUE)
boxplot(mean_zd_nie,ylim=lmts, xlab ="Zdecydowanie siê nie zgadzam", varwidth=TRUE)
title(main="Czy doping rodziców wp³ywa na oceny dzieci?", line = -2, outer=TRUE)

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

## Kapita³ dzieci (bez ocen) vs. studia rodziców
studying <- subset(ankieta, ankieta$studia_m == "tak" | ankieta$studia_t=="tak")
k_studying<-data.frame(Kapita³=studying$k_sum_4)
studies_points<-as.numeric(as.character(studying[,"k_studia_m"]))+as.numeric(as.character(studying[,"k_studia_t"]))
k_studying[,"Kapita³"]<-as.numeric(as.character(k_studying[,"Kapita³"]))-studies_points

notes_studying<-data.frame(Matematyka=studying$ocena_matematyka,Polski=studying$ocena_jêzyk_polski,Przyroda=studying$ocena_przyroda)
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



