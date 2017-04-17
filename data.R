setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi","nie pamięta"))
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
                           levels = c("nie ma ani jednej", "kilka (mniej niż 20)",
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

# Czy wiedza o rodzicach jest zale?na od p?ci?
# STUDIA
# DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_studies_none <- nrow(subset(data_girls, is.na(data_girls$studia_m) & is.na(data_girls$studia_t)))
data_girls_know_studies_both <- nrow(subset(data_girls, !is.na(data_girls$studia_m) & !is.na(data_girls$studia_t)))
data_girls_know_studies_one <- nrow(data_girls) - data_girls_know_studies_both - data_girls_know_studies_none

data_boys <- subset(ankieta, ankieta$plec == "ch?opak")
data_boys_know_studies_none <- nrow(subset(data_boys, is.na(data_boys$studia_m) & is.na(data_boys$studia_t)))
data_boys_know_studies_both <- nrow(subset(data_boys, !is.na(data_boys$studia_m) & !is.na(data_boys$studia_t)))
data_boys_know_studies_one <- nrow(data_boys) - data_boys_know_studies_both - data_boys_know_studies_none

num_girls = sum(pie_chart_data_girls)
num_boys = sum(pie_chart_data_boys)

# WYKRESY KO?OWE
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

# WYKRES S?UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_studies_none/num_girls,1),round(100*data_boys_know_studies_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_studies_one/num_girls,1),round(100*data_boys_know_studies_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_studies_both/num_girls,1),round(100*data_boys_know_studies_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz? o studiach rodzic?w?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch?opcy"), col=colors)

# PRACA
# DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_work_none <- nrow(subset(data_girls, is.na(data_girls$praca_m) & is.na(data_girls$praca_o)))
data_girls_know_work_both <- nrow(subset(data_girls, !is.na(data_girls$praca_m) & !is.na(data_girls$praca_o)))
data_girls_know_work_one <- nrow(data_girls) - data_girls_know_work_both - data_girls_know_work_none

data_boys <- subset(ankieta, ankieta$plec == "ch?opak")
data_boys_know_work_none <- nrow(subset(data_boys, is.na(data_boys$praca_m) & is.na(data_boys$praca_o)))
data_boys_know_work_both <- nrow(subset(data_boys, !is.na(data_boys$praca_m) & !is.na(data_boys$praca_o)))
data_boys_know_work_one <- nrow(data_boys) - data_boys_know_work_both - data_boys_know_work_none

num_girls = sum(pie_chart_data_girls)
num_boys = sum(pie_chart_data_boys)

# WYKRESY KO?OWE
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

# WYKRES S?UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_work_none/num_girls,1),round(100*data_boys_know_work_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_work_one/num_girls,1),round(100*data_boys_know_work_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_work_both/num_girls,1),round(100*data_boys_know_work_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz? o pracy rodzic?w?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch?opcy"), args.legend = list(x ='topleft'), col=colors)

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

barplot(t(as.matrix(joined_scaled)), beside = TRUE, ylim=c(0, 4000), yaxt = "n", col = c("Red", "Blue"), las=2)
axis(2, seq(0, 4000, by=1000), col="Red")
axis(4, pretty(seq(0, 4000, by=1000) / scale) * scale, pretty(seq(0, 4000, by=1000) / scale), col="Blue" )
legend("topright", legend = c("Nieprzetworzona", "Znormalizowana"), fill = c("Red", "Blue"))
title("Liczba odwiedzeń galerii")


#Eksponaty
top_eksponaty = as.data.frame(head(arrange(count(istotne, eksponat, galeria), desc(n)), 6)) #top eksponaty
names(top_eksponaty) = c("Eksponat", "Galeria", "Liczba odwiedzeń")
top_eksponaty

#Eksponaty w obrebie galerii
top_eksponaty = arrange(count(istotne, eksponat, galeria), desc(n))
names(top_eksponaty) = c("Eksponat", "Galeria", "Liczba odwiedzeń")
top_eksponaty_galerie = aggregate(.~Galeria, top_eksponaty, FUN = head, 1)
top_eksponaty_galerie$`Liczba odwiedzeń` = as.numeric(as.character(top_eksponaty_galerie$`Liczba odwiedzeń`))
arrange(top_eksponaty_galerie, desc(`Liczba odwiedzeń`))








