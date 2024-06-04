# cohortSurvivalAnalysis

For information in English please see below.

## Kirjeldus

Käesolev R-pakett valmis bakalaureusetöö raames 2024. aastal. Bakalaureusetöö keskendus elukestusanalüüsi tööriista loomisele _Observational Medical Outcomes Partnership_ (OMOP) ühtse andmemudeli kujul terviseandmetele. R-paketi töövoog sisaldab andmebaasipäringute loomist ja teostamist. Saadud tulemusi saab vaadelda kasutajaliideses. R-paketi sisenditeks on OHDSI _Phenotype Library_ kohortide ID väärtused.

## Kasutamine

### Eeldused

1. Olemasolev R töökoht
2. Ligipääs OMOP CDM kujul andmebaasile

### Juhis

Kasutamiseks tuleb sisestada alamkasutas /extras faili run.R andmebaasi kasutamiseks vajalikud väärtused. Töövoo käivitamiseks on võimalik kasutada /extras/run.R faili.

Töövahendi jaoks vajalikud paketid saab laadida.

```ruby
renv::restore()
```

Funktsioon _cohortVisualAnalysis_ on töövoo keskseks osaks. Funktsiooni argumendid _absorbingState_ ja _targetCohorts_ eeldavad sisendina kohortide ID-sid kujul cx.json, kus x on uuritava kohordi ID _Phenotype Library_-st .

## Introduction

The cohortSurvivalAnalysis R package was developed as part of a Bachelor's thesis in 2024. The thesis focused on creating a survival analysis tool for healthcare data in the format of the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM). The R package's workflow includes creating and executing database queries, and the results can be viewed in a user interface. The input for the R package is the cohort IDs from the OHDSI Phenotype Library.

## Getting started

### Requirements

1. An existing R work environment
2. Access to a database in the OMOP CDM format

### Guide

User needs to enter the necessary values for database access in the /extras/run.R file. The workflow can be initiated by using the /extras/run.R file.

Missing packages used can be loaded with renv command.

```ruby
renv::restore()
```

Function cohortVisualAnalysis contains the main code of the package. Function arguments absorbingState and targetCohorts accept Phenotype Library cohortIDs with the following format cx.json, where x is to be replaced by cohort ID.
