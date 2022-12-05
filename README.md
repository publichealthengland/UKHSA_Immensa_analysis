<img src="images/ukhsa-logo.png" alt="drawing" width="150"/>

# Epidemiological impact of a large number of false-negative SARS-CoV-2 test results in South West England during September and October 2021

Authors: Hounsome, L.<sup>1</sup>, Herr, D.<sup>1</sup>, Bryant, R.<sup>1</sup>, Smith, R.<sup>1</sup>, Loman, L.<sup>1</sup>, Harris, J.<sup>1</sup>, Youhan, U.<sup>1</sup>, Dzene, E.<sup>1</sup>, Hadjipantelis, P.<sup>1</sup>, Long, H.<sup>1</sup>, Laurence, T.<sup>1</sup>, Riley, S.<sup>2</sup>, Cumming, F.<sup>1</sup>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>1</sup> Advanced Analytics Team. Data, Analytics and Surveillance. UKHSA

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Director General. Data, Analytics and Surveillance. UKHSA


## Abstract

>**Background** In England, free testing for coronavirus (COVID-19) was widely available from early in the pandemic until 1 April 2022. Based on apparent differences in the rate of positive polymerase chain reaction (PCR) tests at a single laboratory compared to the rest of the laboratory network, we hypothesised that a substantial number of UK PCR tests processed during September and October 2021 may have been incorrectly reported as negative, compared with the rest of the laboratory network. We investigate the epidemiological impact of this incident.
>
>**Methods** We estimate the additional number of COVID-19 cases that would have been reported had the sensitivity of the laboratory test procedure not dropped for the period 2 September to 12 October. In addition, by making comparisons between the most affected 
local areas and comparator populations, we estimate the number of additional infections, cases, hospitalisations and deaths that could have occurred as a result of increased transmission due to the misclassification of tests.
>
>**Results** We estimate that around 39,000 tests may have been incorrectly classified during this period and, as a direct result of this incident, the most affected areas in the South West could have experienced between 6,000 and 34,000 additional reportable cases, with a central estimate of around 24,000 additional reportable cases. Using modelled relationships between key variables, we estimate that this central estimate could have translated to approximately 55,000 additional infections, which means that each incorrect negative test likely led to just over 2 additional infections. In those same geographical areas, our results also suggest an increased number of admissions and deaths.
>
>**Conclusion** The incident is likely to have had a measurable impact on cases and infections in the affected areas in the South West of England.

### **Serious Untoward Incident** 
The Serious Untoward Incident Investigation report can be found by accessing the following [link](https://www.gov.uk/government/publications/serious-untoward-incident-investigation-immensa-health-clinic-limited)

## Repository Organization


    │── data               <- Holds data files
    │
    ├── outputs
    │ └── report           <- Where the Rmarkdown preprint will be outputted       
    │ └── visuals          <- Plots and visualisations will be created within this folder
    │
    │── main_script.R      <- Runs the analysis and produces the preprint
    │
    ├── src                <- Source code for use in this project
    │ └── example_foo.R    <- Example R file containing functions used in scripts.
    │
    ├── .Rproj             <- R project for analysis
    │
    ├── SECURITY.md        <- Security terms and conditions
    │
    └── README.md          <- The top-level README for developers using this project



Replication
----------
This repository contains the code & data for the preprint publication (INSERT LINK HERE). The code is written in R, and data stored in csv format and a shape file will be called from the ONS api and stored as a geojson after the first run.

- All plots and tables from the paper can be reproduced using the functions in the `src` folder.

- Data, from a variety of sources described in the publication, can be found in the `data` folder. A description of datasets is in the readme in the folder.
 
- Outputs, including visualisations and the publication, can be found in the `outputs` folder.

A master script `main_script.R` can be found in the main folder. To replicate the analysis and preprint run the script.

This will run all of the scripts for each separate analysis discussed in the paper (e.g. descriptive plots, causal impact ... ).

Extensions
----------
Data and code from this project have been published in the spririt of collaboration. Please feel free to replicate this work, or to adapt & develop the work further. When using the data and code from this repository please cite using the following.

> Hounsome, L., et al. (2022). Epidemiological impact of a large number of incorrect negative SARS-CoV-2 test results in South West England during September and October 2021. https://doi.org/10.1101/2022.11.30.22282922 

To get in contact to discuss this work, please email: luke.hounsome@ukhsa.gov.uk or raise an issue on the repository.
