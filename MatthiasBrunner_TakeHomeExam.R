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

rm(list = ls())
# Hinzufuegen von Library
library(foreign)
library(dplyr)
library(data.table)
library(magrittr)
library(ggplot2)
library(stringr)
library(broom)

# Laden Sie die Daten http://www.farys.org/daten/ebay.dta. Es handelt sich um 
# Ebaydaten von Mobiltelefonauktionen.
ebay <- read.dta("http://www.farys.org/daten/ebay.dta")

ebay_df <- read.dta("http://www.farys.org/daten/ebay.dta") %>% as.data.table()

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

library(xlsx)
write.xlsx(pricing, "c:/temp/pricing.xlsx")
write.xlsx(ebay, "c:/temp/ebay.xlsx")

# 3.
# Bilden Sie zudem eine Variable makellos (TRUE/FALSE), ob der Verkäufer ein 
# makelloses Rating (>98% positive Bewertungen) hat oder nicht.
pricing <- ebay %>%
  select(model = subcat,
         price = price) %>%
  mutate(rating = ebay$sepos - ebay$seneg) %>%
  mutate(makellos = ifelse((rating/ebay$sepos) > 0.98, TRUE, FALSE)) %>% 
  mutate(categorie = str_replace(model, "\\ \\(\\d+\\)", "")) %>% 
  filter(ebay$sepos >= 12, !is.na(price)) %>% 
  arrange(model, desc(rating))

pricing[order(pricing$categorie),]

# 4.
# Zeichnen Sie einen farblich geschichteten Boxplot: Y-Achse=Preis, 
# X-Achse=Gerätetyp, farblich geschichtet nach Bewertung 
# (makellos=grün sonst=rot). Orientieren Sie sich z.B. am Vorschlag von 
# "Roger Bivand" im Helpfile zu boxplot(). Exportieren Sie die Grafik als PDF. 
# Erzielen (rein optisch) die Verkäufer mit makellosem Rating einen höheren 
# Verkaufspreis?
# Variante 1:
boxplot(pricing$price ~ pricing$categorie,
        boxwex = 0.25, at = 1:7 - 0.2,
        data = pricing,
        subset = pricing$makellos == TRUE,
        main = "Mobile phone",
        col = "green",
        xlim = c(0.5, 7.5), 
        ylim = c(50, 400),
        yaxs = "i",
        ylab = "Price",
        las=3,
        yaxt = "n"
        )
boxplot(pricing$price ~ pricing$categorie,
        boxwex = 0.25, at = 1:7 + 0.2,
        data = pricing,
        subset = pricing$makellos == FALSE,
        col = "red",
        add = TRUE,
        xaxt = "n"
        )
legend(5.5, 350, c("TRUE", "FALSE"),
       fill = c("green", "red") 
       )

# Variante 2:
ggplot(pricing, aes(x = pricing$model, y = pricing$price))+
  geom_boxplot()








