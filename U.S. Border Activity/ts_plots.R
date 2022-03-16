### Load Packages #####
library(ggplot2)
library(plotly)
library(zoo)

##### Read CSV File #####
# Monthly Apprehension
app.monthly <- read.csv("monthly_app_cbp.csv")
mon <- as.Date(app.monthly$Month, "%Y-%m-%d") # Format "Month"
app.monthly$Month <- as.yearmon(mon) # Add back to table
# Annual Apprehension
app.yearly <- read.csv("app.csv")
# Annual LPR
res.yearly <- read.csv("lawful_res.csv")
# Cluster Assignment
app_clust <- read.csv("app_cluster_id.csv")
lpr_clust <- read.csv("lpr_cluster_id.csv")
nat_clust <- read.csv("nat_cluster_id.csv")
non_clust <- read.csv("non_cluster_id.csv")
case_study_clust <- read.csv("case_study_clust.csv")

##### Plotting Time Series #####
# Monthly Apprehension
mon.app <- ggplot(data=app.monthly, aes(x=Month, y=Number)) + geom_line() +
 labs(title="U.S. Border Patrol Monthly Apprehensions (FY 2000 - FY 2020)",
      x="Year", y="Number")
# Annual Apprehension
ann.app <- ggplot(data=app.yearly, aes(x=Year, y=Number)) + geom_line() +
  labs(title="Annual Apprehensions (FY 1925 - FY 2020)", x="Year", y="Number")
# Annual LPR
ann.lpr <- ggplot(data=res.yearly, aes(x=Year, y=Number)) + geom_line() +
  labs(title="Annual Lawful Permanent Residents (FY 1820 - 2020)",
       x="Year", y="Number")

##### Intervention Event Section Prep #####
x <- c(1:10)
y.step <- c(0,0,0,0,0,1,1,1,1,1)
y.pulse <- c(0,0,0,0,0,1,0,0,0,0)
par(mfrow=c(1,2))
# Step indicator
plot(x, y.step, type='o', main="Step Indicator", xlab="Time Point", ylab="Input")
# Pulse indicator
plot(x, y.pulse, type='o', main="Pulse Indicator", xlab="Time Point", ylab="Input")

##### Cluster Assignment #####
clust_names <- c("Country", "SBD_Average", "SBD_DIANA", "DTW_Average", "DTW_DIANA")
# Selecting columns
app_clust <- app_clust[2:6]
lpr_clust <- lpr_clust[2:6]
nat_clust <- nat_clust[2:6]
non_clust <- non_clust[2:6]
# Rename columns
names(app_clust) <- clust_names
names(lpr_clust) <- clust_names
names(nat_clust) <- clust_names
names(non_clust) <- clust_names

