setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi"))
obserwacje = read.csv('obserwacje.csv', na.strings = c("", " ", "NA"))

###Cleaning - ankieta

###Order levels
ankieta$klasyfikacja_z = ordered(ankieta$klasyfikacja_z,
                                  levels = c("ca³kowicie niezwi¹zany z science",
                                             "poœrednio zwi¹zany z science",
                                             "bezpoœrdnio zwi¹zany z science"))
time_frequency = c(21, 38:49)
for (i in time_frequency)
{
  ankieta[, i] = ordered(ankieta[, i],
                   levels = c("nigdy albo rzadko (raz w roku)", "kilka razy w roku",
                              "raz lub dwa razy w miesi¹cu", "raz w tygodniu",
                              "prawie codziennie"))
}

agreement_levels = c(50:60)
time_frequency = c(21, 38:49)
for (i in time_frequency)
{
  ankieta[, i] = ordered(ankieta[, i],
                         levels = c("zdecydowanie siê nie zgadzam", "raczej siê nie zgadzam",
                                    "ani siê zgadzam ani siê nie zgadzam", 
                                    "raczej siê zgadzam", "zdecydowanie siê zgadzam"))
}

ankieta$p_16_2_4 = ordered(ankieta$p_16_2_4,
                           levels = c("nie ma ani jednej", "kilka (mniej niÅ¼ 20)",
                                      "du¿o (od 20 do 50)", "bardzo du¿o (wiêcej ni¿ 50 do 100)",
                                      "ca³e mnóstwo (wiêcej ni¿ 100)"))


###Cleaning - obserwacje
levels(obserwacje$eksponat) = c(levels(obserwacje$eksponat), "PRZERWA")
obserwacje[obserwacje$kategorie==1,]$eksponat = "PRZERWA"
obserwacje[obserwacje$eksponat=="INNE - PLANETARIUM",]$eksponat <- "PLANETARIUM"

for (i in 13:21){
  obserwacje[,i] = as.numeric(as.character(obserwacje[,i]))
  obserwacje[,i][obserwacje[,i]<100] = NA
}
