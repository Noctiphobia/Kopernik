setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv', na.strings=c(""," ","NA","nie wie","nie wiem","nieczytelne pismo", "dualizm odpowiedzi","brak odpowiedzi"))
obserwacje = read.csv('obserwacje.csv', na.strings = c("", " ", "NA"))

###Cleaning - ankieta



###Cleaning - obserwacje

for (i in 13:21){
	obserwacje[,i] = as.numeric(as.character(obserwacje[,i]))
	obserwacje[,i][obserwacje[,i]<100] = NA
}