# RU - R Analysis Library

Gemensamt bibliotek med återanvändbara R-script för samhällsanalys inom Regional utveckling.

## Struktur

Inga script ska läggas i roten, utan helst klassificeras till ämne/tema eller alternativ namngiven mapp.

- `demographics/` – demografiska analyser
- `rmi/` – analyser baserat på SCB:s regionala matchningsindikatorer

## Användning

Script i detta repository fungerar som gemensamma original för analyser som kan återanvändas i flera R-produkter.

I den R-produkt där scriptet används kopieras det till `R/shared/` i aktuell R-produkt. 
Mappen `shared` markerar just att scriptet har sitt original i detta repository.
