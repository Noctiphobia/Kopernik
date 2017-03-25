setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ankieta = read.csv('ankieta.csv')
obserwacje = read.csv('obserwacje.csv')

ankieta$klasyfikacja_z = replace(ankieta$klasyfikacja_z, 
                                 ankieta$klasyfikacja_z == "bezpoœrdnio zwi¹zany z science",
                                 "bezpoœerdnio zwi¹zany z science")
ankieta$klasyfikacja_z = ordered(ankieta$klasyfikacja_z,
                                  levels = c("ca³kowicie niezwi¹zany z science",
                                             "poœrednio zwi¹zany z science",
                                             "bezpoœrednio zwi¹zany z science"))


