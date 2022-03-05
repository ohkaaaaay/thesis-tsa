# Time Series Analysis on U.S. Immigration Data
*Note: Refer to the thesis report for more information.*

## Introduction
### Goal
The goal is to detect trends and outliers from immigration data to monitor the effects of newly implemented immigration policies and of economic events. From monitoring these trends and outliers, immigration flow can be better managed in the U.S.

### Research Questions
This study accomplishes the goal described above by answering the following research questions:
1.	What political and economic factors can make an impact on U.S. immigration?
2.	How the implementation of U.S. immigration policies and the occurrence of economic events changed immigration in the short term and in the long term?
3.	What are significant emigration trends from sending countries?

## Data
Intervention Event Analysis:
- Monthly Apprehension (monthly_app_cbp.csv)
- Annual Apprehension (app.csv)
- Annual Lawful Permanent Resident (LPR) Status (lawful_res.csv)

Time Series Clustering:
- Annual Apprehension by Region and Country of Birth (app_region_country_2011_2020.csv)
- Annual LPR by Region and Country of Birth (lpr_region_country_2011_2020.csv)
- Annual Naturalization by Region and Country of Birth (nat_region_country_2011_2020.csv)
- Annual Nonimmigrant Admission by Region and Country of Birth (nonimmigrant_region_country_2011_2020.csv)


## Methods
Preprocessing, modeling, and visualizations is performed on R (https://www.r-project.org/) along with intervention event analysis and time series clustering.

### Intervention Events Analysis
The R scripts are listed below:
- Monthly Apprehension (monthly_apprehension_ts.R)
- Annual Apprehension (app_ts.R)
- Annual Lawful Permanent Resident (LPR) Status (lawful_res_ts.R)

#### Events
![image](https://user-images.githubusercontent.com/70343375/156901649-668819e0-14fd-4014-b3fa-259b1792f6a7.png)

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
- Apprehension (app_cluster_id.csv)
- LPR (lpr_cluster_id.csv)
- Naturalization (nat_cluster_id.csv)
- Nonimmigrant (non_cluster_id.csv)
