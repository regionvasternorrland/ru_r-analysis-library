# RU - R Analysis Library

Gemensamt bibliotek med återanvändbara R-script för samhällsanalys inom Regional utveckling.

## Struktur

Inga script ska läggas i roten, utan helst klassificeras till ämne/tema eller alternativ namngiven mapp.
Exempel på ämne/teman:

- `demographics/` – demografiska analyser
- `rmi/` – analyser baserat på SCB:s regionala matchningsindikatorer

## Filnamn

Alla script inleds med källan, ex. `SCB_x.R`.
Vidare innehåller filnamnet även ämnesområdet, ex. för `demographics` skrivs filnamnet: `SCB_demo_x.R`

- bindestreck kan användas för att förtydliga vad scriptet gör
- understeck används för mer tydlig separering

## Användning

Script i detta repository fungerar som gemensamma original för analyser som kan återanvändas i flera R-produkter - med andra ord är script i detta repo "förvaltningslagda".

I den R-produkt där scriptet används kopieras scriptet till sökvägen `R/shared/` i aktuell R-produkt - samtliga R-script bör ligga i en katalog kallad `R` i aktuell produkt/mapp.
Undermappen `shared` markerar just att scriptet har sitt original i detta repository - och är förvaltningslagd. Script som enbart ligger i `R` i aktuell produkt/mapp är inte förvaltningslagda utan är lokala R-script.
