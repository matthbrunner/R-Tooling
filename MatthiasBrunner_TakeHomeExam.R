### ---
# Created on 06.01.2017
# 
# @author: Matthias Brunner
# 
# @summary: TakeHomeExam für das Modul Tooling und Datenmanagement
# 
# @version: 1.0
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
library(xlsx)
library(stargazer)
library(rvest)

# 22 Take-Home-Exam:

# 22.1 Ebay-Auktionen
# 1. Laden Sie die Daten http://www.farys.org/daten/ebay.dta. Es handelt sich um 
# Ebaydaten von Mobiltelefonauktionen.
ebay <- read.dta("http://www.farys.org/daten/ebay.dta")

# Speichern der Daten lokal
# write.dta(ebay, "C:/Workspace/Weiterbildung/Tooling Und Datenmanagement/TakeHomeExame/Daten/ebay.dta")

# Laden der Lokalen daten
# ebay <- read.dta("C:/Workspace/Weiterbildung/Tooling Und Datenmanagement/TakeHomeExam/Daten/ebay.dta")
# Betrachten der Daten
View(ebay)

# Speichern der Daten in Excel um Levels zu überprüfen!
# write.xlsx(pricing, "c:/temp/pricing.xlsx")
# write.xlsx(ebay, "c:/temp/ebay.xlsx")
# Löschen der überflüssigen Levels!
ebay <- droplevels(ebay)

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

head(pricing)

# 3.
# Bilden Sie zudem eine Variable makellos (TRUE/FALSE), ob der Verkäufer ein 
# makelloses Rating (>98% positive Bewertungen) hat oder nicht.
pricing <- ebay %>%
  select(model = subcat,
         price = price) %>%
  mutate(rating = (ebay$sepos - ebay$seneg)/ebay$sepos) %>%
  mutate(makellos = ifelse((rating) > 0.98, TRUE, FALSE)) %>% 
  mutate(categorie = str_replace(model, "\\ \\(\\d+\\)", "")) %>% 
  filter(ebay$sepos >= 12, !is.na(price)) %>% 
  arrange(model, desc(rating))

head(pricing)

# 4.
# Zeichnen Sie einen farblich geschichteten Boxplot: Y-Achse=Preis, 
# X-Achse=Gerätetyp, farblich geschichtet nach Bewertung 
# (makellos=grün sonst=rot). Orientieren Sie sich z.B. am Vorschlag von 
# "Roger Bivand" im Helpfile zu boxplot(). Exportieren Sie die Grafik als PDF. 
# Erzielen (rein optisch) die Verkäufer mit makellosem Rating einen höheren 
# Verkaufspreis?
# Variante 1 (basis plot):
pdf("mobil_v1.pdf")
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
par(cex.axis=0.6)
dev.off()

# Variante 2 (ggplot):
ggplot(pricing, aes(x = categorie, y = price))+
  geom_boxplot(aes(fill = makellos), position = position_dodge(0.8))+
  scale_fill_manual(values = c("green", "red"), name = "Makellos") +
  labs(x = "Kategorie", y="Preis")+
  ggtitle("Mobile phone")+
  #theme_classic()
  theme(legend.position=c(.9, .85),
        axis.text=element_text(size=6))

# Speichern des Plots mit ggsave
ggsave("mobil_v2.pdf")

# Antwort: Es ist keine signifikante Unterschied zwischen festzustellen!

# 5. Rechnen Sie zwei kleine Regressionsmodelle für den Preis von verkauften 
# Geräten. 


# DataFrame umbauen damit die das Attribut listpic mit verarbeitet werden kann.
pricing <- ebay %>%
  select(model = subcat,
         price = price,
         listpic = listpic,
         pos = sepos,
         neg = seneg) %>%
  mutate(rating = (ebay$sepos - ebay$seneg)/ebay$sepos) %>%
  mutate(makellos = ifelse((rating) > 0.98, TRUE, FALSE)) %>% 
  mutate(categorie = str_replace(model, "\\ \\(\\d+\\)", "")) %>%
  filter(ebay$sepos >= 12, !is.na(price)) %>% 
  arrange(model, desc(rating))

# Modell 1 soll als Prädiktoren den Modelltyp und das Rating beinhalten.   
model.1 <- lm(price ~ categorie + rating, data = pricing)
summary(model.1)
coef(model.1)


# Modell 2 soll zusätzlich die Variable listpic beinhalten.
model.2 <- lm(price ~ categorie + rating + listpic, data = pricing)
summary(model.2)
coef(model.2)

# Haben das Rating und die Thumbnails einen Einfluss auf den Verkaufspreis? 
# Antwort: Ja, die Verkaufspreise beeinflussen den verkaufspreis mit dem
#          Koeffizienzintervall von 6.73 

# Exportieren Sie eine Regressionstabelle, die beide Modelle beinhaltet.
stargazer(model.1, model.2, type = "html", style = "qje", out = "model.htm")

# 22.2 Webscraping / Tidying

# 1. Betrachten Sie den Wikipedia-Eintrag zum Klima in Bern: 
# https://de.wikipedia.org/wiki/Bern#Klima. Lesen Sie die Tabelle "Monatliche
# Durchschnittstemperaturen und -niederschläge für Bern 1981-2010" ein.
# Verwenden Sie hierfür die Tools aus dem Kapitel "Datenimport aus HTML/XML"",
# z.B. das Package rvest.
url <- "https://de.wikipedia.org/wiki/Bern#Klima"
roh<-read_html(url)
tabelle <- html_table(roh, fill = TRUE, header = TRUE)[[6]]

# 2. Konzentrieren Sie sich auf die ersten drei Zeilen 
# (Monat, Max. Temperatur, Min. Temperatur) und säubern Sie die Daten 
# (vgl. Kapitel 15.1.), um auf folgende (oder hübschere) Tabelle zu kommen:

# filtern der benötigten Daten
tabelle.short <- tabelle[1:2,2:13]
# Dezimaltrennzeichen von "," auf "." ändern.
swap <- lapply(tabelle.short, gsub, pattern = ",", replacement = ".", fixed = TRUE)

# Formatieren jedes einzelnen Werten mit 3 Nachkommastellen.
swap <- as.data.frame(lapply(swap, function(.col){
     if (is.numeric(as.numeric(as.character(.col)))) 
       return(sprintf("%.2f", as.numeric(as.character(.col))))
     else return(.col)
 }))

# Drehen der Daten
swap <- t(swap)

# rownames in Kolonne abfüllen
swap <- cbind(Row.Names = rownames(swap), swap)
# Löschen der rownames
rownames(swap) <- NULL
# Kolonnennamen vergeben
colnames(swap) <- c("Monat", "Max", "Min")
# Ausgeben der Tabelle mit Stargazer
stargazer(swap, 
          type = "html", 
          style = "qje",
          rownames = TRUE,
          title = "Monatliche Durchschnittstemperaturen und -niederschläge für Bern 1981-2010",
          out = "Temperatur.htm")






