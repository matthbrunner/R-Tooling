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
library(dplyr)

# Laden Sie die Daten http://www.farys.org/daten/ebay.dta. Es handelt sich um 
# Ebaydaten von Mobiltelefonauktionen.
ebay <- read.dta("http://www.farys.org/daten/ebay.dta")

# Speichern der Daten lokal
# write.dta(ebay, "C:/Workspace/Weiterbildung/Tooling Und Datenmanagement/TakeHomeExame/Daten/ebay.dta")

# Laden der Lokalen daten
ebay <- read.dta("C:/Workspace/Weiterbildung/Tooling Und Datenmanagement/TakeHomeExam/Daten/ebay.dta")
# Betrachten der Daten
View(ebay)

# 2. 
# Sie interessieren sich dafür, ob Mobiltelefone bei Auktionen einen höheren 
# Preis erzielen, wenn der Verkäufer des Geräts eine gute Bewertung hat. 
# Erzeugen Sie eine Variable rating, die den Anteil positiver Bewertungen an 
# den Gesamtbewertungen misst. Schliessen Sie Fälle aus den Daten aus, für die 
# weniger als 12 positive Bewertungen vorliegen.

pricing <- ebay %>%
  select(model = subcat,
         price = price) %>%
  mutate(rating = ebay$sepos - ebay$seneg) %>% 
  filter(ebay$sepos >= 12, !is.na(price)) %>% 
  arrange(model, desc(rating))

View(pricing)

# 3.
# Bilden Sie zudem eine Variable makellos (TRUE/FALSE), ob der Verkäufer ein 
# makelloses Rating (>98% positive Bewertungen) hat oder nicht.
# pricing <- ebay %>%
#   select(model = subcat,
#          price = price,
#          feedback_p = sepos,
#          feedback_n = seneg) %>%
#   mutate(rating = ebay$sepos - ebay$seneg) %>%
#   mutate(per = rating/ebay$sepos) %>% 
#   mutate(makellos = ifelse((rating/ebay$sepos) > 0.98, TRUE, FALSE)) %>% 
#   filter(ebay$sepos >= 12, !is.na(price)) %>% 
#   arrange(model, desc(rating))
pricing <- ebay %>%
  select(model = subcat,
         price = price) %>%
  mutate(rating = ebay$sepos - ebay$seneg) %>%
  mutate(makellos = ifelse((rating/ebay$sepos) > 0.98, TRUE, FALSE)) %>% 
  filter(ebay$sepos >= 12, !is.na(price)) %>% 
  arrange(model, desc(rating))

# 4.
# Zeichnen Sie einen farblich geschichteten Boxplot: Y-Achse=Preis, 
# X-Achse=Gerätetyp, farblich geschichtet nach Bewertung 
# (makellos=grün sonst=rot). Orientieren Sie sich z.B. am Vorschlag von 
# "Roger Bivand" im Helpfile zu boxplot(). Exportieren Sie die Grafik als PDF. 
# Erzielen (rein optisch) die Verkäufer mit makellosem Rating einen höheren 
# Verkaufspreis?
?boxplot

boxplot(pricing$price ~ pricing$model)





