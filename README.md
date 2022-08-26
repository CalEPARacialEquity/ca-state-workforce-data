## Project Description / Background

This project is part of CalEPA's racial equity work group (REWG) data sub-team, and aims to explore the demographics of California's state government workforce. The project is regularly evolving to fit the data needs of CalEPA's REWG workforce equity sub-team. There are several different analyses within the project, each supporting the larger goal of operationalizing equity throughout all phases of workforce development and supporting a culture across CalEPA where all feel they belong. CalEPA's REWG is guided by equity principles from the Government Alliance on Racial Equity (GARE). (we can potentially add more information about the project here, e.g., a description of the project background and goals, documentation of where the raw/source data came from, instructions for running the scripts, description of intermediate and final outputs, etc.).

## Source Data

The workforce data used in this project comes from [CalHR's Statewide 5102 report](https://www.calhr.ca.gov/pages/statewide-reports.aspx). A cleaned and compiled version of that data is available on the California Open Data Portal at: <https://data.ca.gov/dataset/calhr-civil-rights-data-for-gare-capital-cohort-2019>

### Dataset Description

Description of the dataset coming soon...

## Instructions

If using RStudio, open the `workforce_data.Rproj` file to open the project within RStudio. Otherwise, set your working directory in R to the folder that contains this readme file.

### Updating 5102 Dataset

To update the compiled 5102 dataset with data for an additional year:

1.  Get the raw data for the new year from CalHR.
2.  Save the new data to an excel file named "calhr-5102-statewide-YYYY.xlsx" (where YYYY is the year the dataset covers), and put that file in the `02_data_raw/5102` folder.
3.  Run the `01_scripts/data_processing.R` script. This script compiles the data from each of the individual years' data files (assuming they are saved with the file naming convention described in step 2), and saves the compiled dataset to a zipped csv file in the `03_data_processed` folder. It also updates the [compiled dataset on the CA Open Data Portal](https://data.ca.gov/dataset/calhr-civil-rights-data-for-gare-capital-cohort-2019/resource/aba87ad9-f6b0-4a7e-a45e-d1452417eb7f) (the script assumes that you have a data portal key saved to your local environment, in a variable named `data_portal_key`; as an alternative to using this script to update the data portal, you can manually update the portal by extracting the zipped csv in the `03_data_processed` folder and loading it to the portal).

### Exploratory Data Analysis

To run the exploratory data analysis script, run the `workforce_data_exploration.R` script in the `01_scripts` folder.

### Create Visualizations For Your Agency

To create graphs visualizing your department's workforce demographics, use the script in the `06_reports` folder. Your results will be saved locally in the `07_slides` folder.

## Reports / Slides

-   [2020 Workforce Data Analysis slides](https://caleparacialequity.github.io/ca-state-workforce-data/07_slides/2021-05-14/workforce_data_summary_2021-05-14.html)

-   [5102 Data Analysis by Agency / Department](https://caleparacialequity.github.io/ca-state-workforce-data/06_reports/workforce_metrics_5102.html)

-   PowerBI prototypes and older data viz tools:

    -   CalHR statewide data tools (includes up to 2020 for 5102 data. <https://bit.ly/ca_workforce_2020>
    -   CalEPA interactive tool for 2011 to 2020 CalHR 5102 data: <https://bit.ly/CalEPA_workforcedemo_PeterH>
