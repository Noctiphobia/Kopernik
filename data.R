setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi","nie pamiÄ™ta"))
obserwacje = read.csv('obserwacje.csv', na.strings = c("", " ", "NA"))

###Cleaning - ankieta

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
                           levels = c("nie ma ani jednej", "kilka (mniej niÅ¼ 20)",
                                      "du?o (od 20 do 50)", "bardzo du?o (wi?cej ni? 50 do 100)",
                                      "ca?e mn?stwo (wi?cej ni? 100)"))


###Cleaning - obserwacje
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

###Interesting variables
children_count = count(distinct(ankieta, id_ucznia))

# DZIECI

# Czy wiedza o rodzicach jest zale¿na od p³ci?
# STUDIA
# DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_studies_none <- nrow(subset(data_girls, is.na(data_girls$studia_m) & is.na(data_girls$studia_t)))
data_girls_know_studies_both <- nrow(subset(data_girls, !is.na(data_girls$studia_m) & !is.na(data_girls$studia_t)))
data_girls_know_studies_one <- nrow(data_girls) - data_girls_know_studies_both - data_girls_know_studies_none

data_boys <- subset(ankieta, ankieta$plec == "ch³opak")
data_boys_know_studies_none <- nrow(subset(data_boys, is.na(data_boys$studia_m) & is.na(data_boys$studia_t)))
data_boys_know_studies_both <- nrow(subset(data_boys, !is.na(data_boys$studia_m) & !is.na(data_boys$studia_t)))
data_boys_know_studies_one <- nrow(data_boys) - data_boys_know_studies_both - data_boys_know_studies_none

num_girls = sum(pie_chart_data_girls)
num_boys = sum(pie_chart_data_boys)

# WYKRESY KO£OWE
pie_chart_data_girls <- c(data_girls_know_studies_none, data_girls_know_studies_one, data_girls_know_studies_both)
percentlabels<- round(100*pie_chart_data_girls/num_girls, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_girls))
pie(pie_chart_data_girls, main="Czy dziewczynki wiedz¹ o studiach rodziców?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

pie_chart_data_boys <- c(data_boys_know_studies_none, data_boys_know_studies_one, data_boys_know_studies_both)
percentlabels<- round(100*pie_chart_data_boys/num_boys, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_boys))
pie(pie_chart_data_boys, main="Czy ch³opcy wiedz¹ o studiach rodziców?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

# WYKRES S£UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_studies_none/num_girls,1),round(100*data_boys_know_studies_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_studies_one/num_girls,1),round(100*data_boys_know_studies_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_studies_both/num_girls,1),round(100*data_boys_know_studies_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz¹ o studiach rodziców?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch³opcy"), col=colors)

# PRACA
# DANE
data_girls <- subset(ankieta, ankieta$plec == "dziewczyna")
data_girls_know_work_none <- nrow(subset(data_girls, is.na(data_girls$praca_m) & is.na(data_girls$praca_o)))
data_girls_know_work_both <- nrow(subset(data_girls, !is.na(data_girls$praca_m) & !is.na(data_girls$praca_o)))
data_girls_know_work_one <- nrow(data_girls) - data_girls_know_work_both - data_girls_know_work_none

data_boys <- subset(ankieta, ankieta$plec == "ch³opak")
data_boys_know_work_none <- nrow(subset(data_boys, is.na(data_boys$praca_m) & is.na(data_boys$praca_o)))
data_boys_know_work_both <- nrow(subset(data_boys, !is.na(data_boys$praca_m) & !is.na(data_boys$praca_o)))
data_boys_know_work_one <- nrow(data_boys) - data_boys_know_work_both - data_boys_know_work_none

num_girls = sum(pie_chart_data_girls)
num_boys = sum(pie_chart_data_boys)

# WYKRESY KO£OWE
pie_chart_data_girls <- c(data_girls_know_work_none, data_girls_know_work_one, data_girls_know_work_both)
percentlabels<- round(100*pie_chart_data_girls/num_girls, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_girls))
pie(pie_chart_data_girls, main="Czy dziewczynki wiedz¹ o pracy rodziców?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

pie_chart_data_boys <- c(data_boys_know_work_none, data_boys_know_work_one, data_boys_know_work_both)
percentlabels<- round(100*pie_chart_data_boys/num_boys, 1)
pielabels<- paste(percentlabels, "%", sep="")
cols=rainbow(length(pie_chart_data_boys))
pie(pie_chart_data_boys, main="Czy ch³opcy wiedz¹ o pracy rodziców?", col=cols, labels=pielabels, cex=0.8)
legend("topright", c("Nie","O jednym","Tak"), cex=0.8, fill=cols)

# WYKRES S£UPKOWY
data <- structure(list(Nie=c(round(100*data_girls_know_work_none/num_girls,1),round(100*data_boys_know_work_none/num_boys,1)),
                       Jeden=c(round(100*data_girls_know_work_one/num_girls,1),round(100*data_boys_know_work_one/num_boys,1)),
                       Tak=c(round(100*data_girls_know_work_both/num_girls,1),round(100*data_boys_know_work_both/num_boys,1))),
                  .Names = c("Nie", "O jednym", "Tak"), class = "data.frame", row.names = c(NA, -2L))
colors <- c("red", "blue")
barplot(as.matrix(data), main="Czy dzieci wiedz¹ o pracy rodziców?", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, ylab = "Procent", ylim=c(0,100), legend=c("Dziewczynki", "Ch³opcy"), args.legend = list(x ='topleft'), col=colors)

###Creating dataframe groups
library(dplyr)
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
  c = grupki[i, "liczba_chlopcow"] = count(filter(children, plec == "ch³opak"))
  grupki[i, "procent_dziewczynek"] = d/(c + d)
}

grupki_wszystkie = grupki
grupki_jednoosobowe = filter(grupki_wszystkie, liczba_dzieci == 1)
grupki = filter(grupki_wszystkie, liczba_dzieci > 1)

count(grupki_jednoosobowe)
children_count

procent_dziewczynek = unlist(grupki[!is.na(grupki[, "procent_dziewczynek"]), "procent_dziewczynek"])

mean(procent_dziewczynek)
median(procent_dziewczynek)

#Grupki dziewczêce, ch³opiêce i mieszane
grupki_po_plci = unlist(c(count(filter(grupki, procent_dziewczynek == 1)),
                  count(filter(grupki, procent_dziewczynek == 0)),
                  count(filter(grupki, procent_dziewczynek != 1 & procent_dziewczynek != 0))))
barplot(grupki_po_plci, names.arg = c("dziewczêce", "ch³opiêce", "mieszane"))

liczba_dzieci = grupki_wszystkie[, "liczba_dzieci"]
liczba_liczb_dzieci = count(liczba_dzieci, liczba_dzieci)

#Najbardziej nieroz³¹czne grupki
arrange(grupki_wszystkie, desc(n))
grupki_wyczyszczone = arrange(filter(grupki, liczba_dzieci == liczba_dziewczynek + liczba_chlopcow), desc(n))

#Liczba grupek o poszczególnych rozmiarach 
barplot(unlist(liczba_liczb_dzieci[, "n"]), names.arg = 1:9)
