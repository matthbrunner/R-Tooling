### ---
# Created on 06.12.2016
# 
# @author: Matthias Brunner
# 
# @summary:
# 
# @version: 0.1
# 
# @change:
# 
# 
# ---

# Hinzufuegen von Library
library(foreign)

# Laden Sie die Daten http://www.farys.org/daten/ebay.dta. Es handelt sich um 
# Ebaydaten von Mobiltelefonauktionen.
ebay <- read.dta("http://www.farys.org/daten/ebay.dta")

# Speichern der Daten lokal
# write.dta(ebay, "C:/Workspace/Weiterbildung/Tooling Und Datenmanagement/TakeHomeExame/Daten/ebay.dta")

# Laden der Lokalen daten
ebay <- read.dta("C:/Workspace/Weiterbildung/Tooling Und Datenmanagement/TakeHomeExame/Daten/ebay.dta")
# Betrachten der Daten
View(ebay)



