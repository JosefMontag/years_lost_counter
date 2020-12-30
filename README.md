# Years lost counter - Replication package

This package complements the web app [smikula.shinyapps.io/lostyears](https://smikula.shinyapps.io/lostyears/), 
which facilitates computations of years lost due to COVID-19 deaths. It allows to explicitly specify and vary the
assumption of the risk group to which an average dying person would have belonged. The basic explanation of
how the app works, its functionalities, and the output estimates is available on the app's website (in Czech).

## Contents of this package

* Document [Computing_years_lost.pdf](https://github.com/JosefMontag/years_lost_counter/blob/main/Computing_years_lost.pdf)
describes how the risk groups are defined and how risk group-specific years lost are computed from the actuarial tables.
* Folder [Input_data](https://github.com/JosefMontag/years_lost_counter/tree/main/Input_data) contains:
    * CSV files with Czech actuarial tables from the Czech Statistical Office (see https://www.czso.cz/csu/czso/umrtnostni_tabulky, 
    for the original files and the documentation.
    * Daily exports of individual-level data on COVID-19 hospitalizations (patient's gender, age, week of death if the patient died)
    from the Institute of Health Information and Statistics (ÚZIS). The full original data is not publicly available but can be 
    accessed upon request, see Czech Ministry of Health's COVID-19 data website https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19.
* R script [lost_years_counter.R](https://github.com/JosefMontag/years_lost_counter/blob/main/lost_years_counter.R) computes
the risk group-specific years lost for individual years of death using the actuarial tables from the Czech Statistical Office.
* The CSV [lost_years_by_risk_group.csv](https://github.com/JosefMontag/years_lost_counter/blob/main/lost_years_by_risk_group.csv)
contains the estimates of years lost for each age at death and risk group computed by the script.
* R script [lostyears_shinyapp.R](https://github.com/JosefMontag/years_lost_counter/blob/main/lostyears_shinyapp.R) is the code behind 
the Shiny web app. It uses the risk group-specific years lost estimates and the data on hospitalizations to produce real-time estimates
of years lost due to COVID-19 deaths in the Czech Republic.

## Availability for other uses

* The app can be easily adapted to data from other countries or to other diseases.
* The code can be freely used, but proper credits should be given.

## Authors and feedback

Štěpán Mikula, PhD  
Department of Economics  
Faculty of Economics and Administration  
Masaryk University

Josef Montag, PhD  
Department of Economics  
Faculty of Law  
Charles University

We greatly appreciate any feedback. 

With queries or feedback regarding:

* the web app contact Štěpán Mikula at stepan.mikula@econ.muni.cz.
* the computation of risk group-specific years lost josef.montag@gmail.com.


