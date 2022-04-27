# Time Series Analysis on U.S. Immigration Data

## Introduction
Refer to the following documents for more for more detail:
- Full Thesis Report: Fabio_Thesis_Final.pdf
- Abstract: Fabio_Thesis_Abstract.pdf

### Goal
The goal is to detect trends and outliers from immigration data to monitor the effects of newly implemented immigration policies and of economic events. From monitoring these trends and outliers, immigration flow can be better managed in the U.S.

### Research Questions
This study accomplishes the goal described above by answering the following research questions:
1.	What political and economic factors significantly impact U.S. immigration?
2.	How does the implementation of U.S. immigration policies and the occurrence of economic events change immigration in the short term and in the long term?
3.	What are significant emigration trends from sending countries?

## Data
Intervention Event Analysis:
- Monthly Apprehension (TSA/monthly_app_cbp.csv)
- Annual Apprehension (TSA/app.csv)
- Annual Lawful Permanent Resident (LPR) Status (TSA/lawful_res.csv)

Time Series Clustering:
- Annual Apprehension by Region and Country of Birth (TSA/app_region_country_2011_2020.csv)
- Annual LPR by Region and Country of Birth (TSA/lpr_region_country_2011_2020.csv)
- Annual Naturalization by Region and Country of Birth (TSA/nat_region_country_2011_2020.csv)
- Annual Nonimmigrant Admission by Region and Country of Birth (TSA/nonimmigrant_region_country_2011_2020.csv)

## Methods
Preprocessing, modeling, and visualizations is performed on R (https://www.r-project.org/) along with intervention event analysis and time series clustering.

### Intervention Events Analysis
The R scripts are listed below:
- Monthly Apprehension (TSA/monthly_apprehension_ts.R)
- Annual Apprehension (TSA/app_ts.R)
- Annual Lawful Permanent Resident (LPR) Status (TSA/lawful_res_ts.R)

Make sure FUNCTIONS_Intervention_Events.R is in the same folder to perform intervention analysis automatically when called the R above R files.

#### Events
![image](https://user-images.githubusercontent.com/70343375/159962744-232308d8-88a5-437a-8318-588a217dfdff.png)


### Time Series Clustering
#### Parameters
![image](https://user-images.githubusercontent.com/70343375/156901777-a3ad058f-913c-4b87-9065-70634e697091.png)

The R scripts are listed below:
- Annual Apprehension by Region and Country of Birth (app_tsclust.R)
- Annual LPR by Region and Country of Birth (lpr_tsclust.R)
- Annual Naturalization by Region and Country of Birth (nat_tsclust.R)
- Annual Nonimmigrant Admission by Region and Country of Birth (non_tsclust.R)

#### Clustering Assignment
Refer to the following files for clustering assignment for each method:
- Apprehension (TSA/app_cluster_id.csv)
- LPR (TSA/lpr_cluster_id.csv)
- Naturalization (TSA/nat_cluster_id.csv)
- Nonimmigrant (TSA/non_cluster_id.csv)

## R-Shiny Application
Files, scripts, and images are under the U.S. Border Activity folder and needed to run the same R-Shiny app on R-Studio. Use the app.R file to run the R-Shiny app. It should appear exactly like the Shiny App website (https://ekayfabio.shinyapps.io/US_TSA/).
