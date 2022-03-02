## Project Description / Background

This project is part of CalEPA's racial equity work group (REWG) data sub-team, and aims to explore the demographics of California's state government workforce. The project is regularly evolving to fit the data needs of CalEPA's REWG workforce equity sub-team. There are several different analyses within the project, each supporting the larger goal of operationalizing equity throughout all phases of workforce development and supporting a culture across CalEPA where all feel they belong. CalEPA's REWG is guided by equity principles from the Government Alliance on Racial Equity (GARE). (we can potentially add more information about the project here, e.g., a description of the project background and goals, documentation of where the raw/source data came from, instructions for running the scripts, description of intermediate and final outputs, etc.).

## Source Data

The workforce data used in this project comes from [CalHR's Statewide 5102 report](https://www.calhr.ca.gov/pages/statewide-reports.aspx). A cleaned and compiled version of that data is available on the California Open Data Portal at: <https://data.ca.gov/dataset/calhr-civil-rights-data-for-gare-capital-cohort-2019>

### Dataset Description

Description of the dataset coming soon...

## Instructions

If using RStudio, open the `workforce_data.Rproj` file to open the project within RStudio. Otherwise, set your working directory in R to the folder that contains this readme file.

### Exploratory Data Analysis

To run the exploratory data analysis script, run the `workforce_data_exploration.R` script in the `01_scripts` folder.

### Create Visualizations For Your Agency

To create graphs visualizing your department's workforce demographics, use the script in the `06_reports` folder. Your results will be saved locally in the `07_slides` folder.

## Reports / Slides

- [2020 Workforce Data Analysis slides](https://caleparacialequity.github.io/ca-state-workforce-data/07_slides/2021-05-14/workforce_data_summary_2021-05-14.html)
   
- [5102 Data Analysis by Agency / Department](https://caleparacialequity.github.io/ca-state-workforce-data/06_reports/workforce_metrics_5102.html)
- PowerBI prototypes and older data viz tools:
   - CalHR statewide data tools (includes up to 2020 for 5102 data. https://bit.ly/ca_workforce_2020
   - CalEPA interactive tool for 2011 to 2020 CalHR 5102 data: https://bit.ly/CalEPA_workforcedemo_PeterH
