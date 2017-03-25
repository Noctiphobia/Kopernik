setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi"))
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
                           levels = c("nie ma ani jednej", "kilka (mniej niż 20)",
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
