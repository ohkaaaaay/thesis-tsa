# Load packages ----
library(shiny)
library(shinythemes)
library(rsconnect)
library(DT)

# Source functions -----
#source("yearly_app_cit.R")
#source("yearly_rfa_region.R")
source("ts_plots.R")

# Define UI ----
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage("Time Series Analysis on U.S. Immigration Variables",

##### TAB: Summary #####
tabPanel("Summary",
         ##### Research Questions #####
         h2("Research Questions"),
         tags$hr(
           style="border-color: grey;"
         ),
         tags$ol(
           tags$li("What political and economic factors significant impact U.S. immigration?"),
           tags$li("How does the implementation of U.S. immigration policies and the occurrence of economic events change immigration in the short term and in the long term?"),
           tags$li("What are significant emigration trends from sending countries?")
         ),
         ##### Goal #####
         h2("Goal"),
         tags$hr(
           style="border-color: grey;"
         ),
         p("With the understanding of domestic politics and globalization, the goal is to detect trends and outliers from immigration data to monitor the effects of newly implemented immigration policies and of economic events. From monitoring these trends and outliers, immigration flow can be better managed in the U.S."),
         p(tags$b("GitHub:"), a("https://github.com/ohkaaaaay/thesis-tsa",
                                href = "https://github.com/ohkaaaaay/thesis-tsa")),
         img(src="flag.jpg", width="100%", height="100%"),
         p()
), # navbarMenu

##### TAB: Data #####
tabPanel("Data",
         ##### Intervention Analysis #####
         h2("Intervention Analysis"),
         tags$hr(
           style="border-color: grey;"
         ),
         h3("Monthly Apprehensions"),
         p(tags$b("Description:"), "The monthly total apprehensions from the", tags$em("U.S. Border Patrol Monthly Encounters Report"), ". Apprehension is defined by the Department of Homeland Security as \"the arrest of a removable alien\"."),
         p(tags$b("Range:"), "October 1999 - September 2020"),
         p(tags$b("Source:"), a("U.S. Border Patrol",
                                href = "https://www.cbp.gov/document/stats/us-border-patrol-monthly-encounters-fy-2000-fy-2020?_ga=2.39847586.1406906253.1629969446-988257266.1629352316")),
         h3("Annual Apprehension"),
         p(tags$b("Description:"), "The annual apprehension count from the", tags$em("2020 Yearbook of Immigration Statistics"), ". To note, a person is counted again every time they are apprehended within the same fiscal year."),
         p(tags$b("Range:"), "FY 1925 - FY 2020"),
         p(tags$b("Source:"), a("U.S. Department of Homeland Security",
                                href = "https://www.dhs.gov/immigration-statistics/yearbook/2020")),
         h3("Annual Lawful Permanent Resident (LPR) Status"),
         p(tags$b("Description:"), "The number of people that obtained LPR status for each fiscal year from the", tags$em("2020 Yearbook of Immigration Statistics"), ". LPR, also known as \"green card holders\", are non-citizens authorized to live in the U.S."),
         p(tags$b("Range:"), "FY 1820 - FY 2020"),
         p(tags$b("Source:"), a("U.S. Department of Homeland Security",
                                href = "https://www.dhs.gov/immigration-statistics/yearbook/2020")),
         ##### Time Series Clustering #####
         h2("Time Series Clustering"),
         p(tags$b("Note:"), "These datasets contains former countries (e.g. Czechoslovakia) labeled as “former” at the end of the name (e.g. Czechoslovakia_former)."),
         tags$hr(
           style="border-color: grey;"
         ),
         h3("Annual Apprehension by Region and Country of Birth"),
         p(tags$b("Description:"), "The annual apprehension count from the", tags$em("2020 Yearbook of Immigration Statistics"), "filtered by region and country."),
         p(tags$b("Range:"), "FY 2011 - FY 2020"),
         p(tags$b("Source:"), a("U.S. Department of Homeland Security",
                                href = "https://www.dhs.gov/immigration-statistics/yearbook/2020")),
         h3("Annual LPR Status by Region and Country of Birth"),
         p(tags$b("Description:"), "The annual LPR status from the", tags$em("2020 Yearbook of Immigration Statistics"), "filtered by region and country."),
         p(tags$b("Range:"), "FY 2011 - FY 2020"),
         p(tags$b("Source:"), a("U.S. Department of Homeland Security",
                                href = "https://www.dhs.gov/immigration-statistics/yearbook/2020")),
         h3("Annual Naturalization Status by Region and Country of Birth"),
         p(tags$b("Description:"), "The annual naturalization from the", tags$em("2020 Yearbook of Immigration Statistics"), "filtered by region and country."),
         p(tags$b("Range:"), "FY 2011 - FY 2020"),
         p(tags$b("Source:"), a("U.S. Department of Homeland Security",
                                href = "https://www.dhs.gov/immigration-statistics/yearbook/2020")),
         h3("Annual Nonimmigrant Status by Region and Country of Birth"),
         p(tags$b("Description:"), "The annual nonimmigrant status from the", tags$em("2020 Yearbook of Immigration Statistics"), "filtered by region and country."),
         p(tags$b("Range:"), "FY 2011 - FY 2020"),
         p(tags$b("Source:"), a("U.S. Department of Homeland Security",
                                href = "https://www.dhs.gov/immigration-statistics/yearbook/2020")),
         p()
),

##### MENU: Methods #####
navbarMenu("Methods",
           ##### TAB: Intervention Analysis #####
           tabPanel("Intervention Analysis",
                    ##### Intervention Analysis #####
                    h2("Intervention Analysis"),
                    tags$hr(
                      style="border-color: grey;"
                    ),
                    p("Before intervention analysis is performed, an ARIMA model needs to be determined based on the following steps below:"),
                    tags$ol(
                      tags$li("Plot the autocorrelation function (ACF) and the partial autocorrelation function (PACF) to determine the autoregressive (AR) order \"p\" and the moving average (MA) order \"q\"."),
                      tags$li("Create possible ARIMA models with a few selected preliminary values of p and q. Then use Akaike information criterion (AIC) as a metric to determine the best fit. The best model is one with the smallest AIC value."),
                      tags$li("Perform model diagnostics to guarantee the model is the best fit."),
                    ),
                    p("Next, intervention analysis takes a look at significant outliers in a time series and measures the effect after the event occurs. Both an indicator variable and ARIMA model can determine the type of intervention event."),
                    p("The indicator variable can be represented as a list of zero's and one's. The zero's indicate no input and the one's indicate an input applied."),
                      img(src="step_pulse.jpeg", width="70%", height="70%"),
                    p("From here, the following responses can be estimated using maximum likelihood:"),
                    tags$ul(
                      tags$li("Permanent"),
                      tags$li("Temporary"), 
                      tags$li("Permanent & Temporary"),
                      tags$li("None")
                    ),
                    p("The best response model would be one with the smallest AIC value."),
                    h3("Immigration History"),
                    tags$ul(
                      tags$li(tags$b("World War I & World War II:"), "The outbreak of these wars lead to the introduction of systematic immigration controls and anti-immigrant sentiment."),
                      tags$li(tags$b("Great Depression & Great Recession:"), "Economic recessions that have indirectly set forth immigration policies."),
                      tags$li(tags$b("Operation Wetback:"),
                              "A militarization effort that removed over two million Mexican immigrants from the U.S.",
                              "It all began with the Bracero Program in 1942.",
                              "The Bracero Program was an agreement between U.S. and Mexico that issued temporary work to Mexican migrants."),
                      tags$li(tags$b("Immigration Reform and Control Act of 1986 (IRCA):"),
                              "The goal was to control undocumented immigration in three ways.",
                              "First, it eliminated the attraction of U.S. jobs by imposing sanctions on employers who knowingly hire undocumented migrants.",
                              "Second, it discouraged people from entering the U.S. illegally by increasing funding to the U.S. Border Patrol which would thus increase border enforcement.",
                              "Lastly, it authorized amnesty for undocumented migrants who have already resided in the U.S. since 1982"),
                      tags$li(tags$b("Illegal Immigration Reform and Immigrant Responsibility Act of 1996 (IIRIRA):"), "It reclassified undocumented immigrants as deportable and/or admissible and curtailed immigrants’ rights for due process."),
                      tags$li(tags$b("Mexico Temporary Humanitarian Visa:"),
                              "Caravans of migrants from Central America that transmigrated through Mexico with the intention of crossing the U.S.-Mexico border.",
                              "The issuance of Mexico temporary humanitarian visas made this migration possible for many Central Americans."),
                    ),
                    img(src="immigration_history.png", width="75%", height="75%"),
                    h3("Events Table"),
                    p("The intervention events that will be analyzed is listed in the table below:"),
                    img(src="events_table.jpeg", width="50%", height="50%"),
                    p("1: Time point analyzed on the annual apprehension dataset.", tags$br(),
                      "2: Time point analyzed on the annual LPR dataset."),
                    ), # tabPanel
           ##### TAB: Time Series Clustering #####
           tabPanel("Time Series Clustering",
                    h2("Time Series Clustering"),
                    tags$hr(
                      style="border-color: grey;"
                    ),
                    h3("Clustering Parameters"),
                    p("Time series clustering involves deciding on the following parameters:"),
                    tags$ul(
                      tags$li(tags$b("Type:"), "Determine if to perform hierarchical or partitional clustering."),
                      tags$ul(
                        tags$li(tags$b("Hierarchical:"), "This clustering method was chosen because it observed and measured the linkages between inter-groups visualized on a dendrogram."),
                        tags$li(tags$b("Partitional:"), "This clustering method uses a k-means algorithm by defining a k number of clusters."),
                      ),
                      tags$li(tags$b("Splitting Method:"), "Specifically for hierarchical clustering there are two different methods based on how they split."),
                      tags$ul(
                        tags$li(tags$b("Agglomerative:"), "A \"bottom-up\" approach that starts with all the observations as their own cluster. Different observations begin to group with each other based on similarity. In this study, we use average linkage as a measure."),
                        tags$li(tags$b("Divisive:"), "A \"top-down\" approach that has all the observations starting under one cluster and then splits into smaller clusters."),
                      ),
                      tags$li(tags$b("Distance Measure:"), "This is the most important decision when performing time series clustering. We will be using distance measures that outperforms all the others:"),
                      tags$ul(
                        tags$li(tags$b("Shape-Based Distance (SBD):"), "Manages distortions in amplitude and phase and uses cross-correlation to measure the similarity between two time series."),
                        tags$li(tags$b("Dynamic Time Warping (DTW):"), "An extension of Euclidean distance that creates a non-linear, local alignment called the warping path on a matrix."),
                      ),
                      tags$li(tags$b("Centroid:"), "Also known as prototypes, it summarizes the most important characteristics of time series from the same cluster. The prototype function is selected based on the distance measure used."),
                      tags$ul(
                        tags$li(tags$b("Shape Extraction:"), "Applies the k-Shape algorithm to compute an average sequence by minimizing the sum of squared distances to all the other time series sequences."),
                        tags$li(tags$b("DTW Barycenter Averaging (DBA):"), "Similar to shape extraction except that the DTW alignment between each time series in a cluster and centroid is measured instead."),
                      ),
                      tags$li(tags$b("Pre-Processing:"), "Based on the prototype measure, shape extraction requires pre-processing. The time series is normalized by applying z-score. For standardization, pre-processing was also performed using dynamic time warping (DTW) barycenter averaging (DBA)."),
                    ),
                    p("In using the dtwclust package in R, these parameters can easily be adjusted to compare differnt clustering methods and conditions."),
                    h3("Parameters Table"),
                    p("The conditions below show four different clustering methods used:"),
                    img(src="clust_parameters.jpeg", width="65%", height="65%"),
           ) # tabPanel
), # navbarMenu

##### MENU: Analysis #####
navbarMenu("Analysis",
           ##### TAB: Time Series #####
           tabPanel("Time Series",
                    ##### Time Series Plots #####
                    h2("Time Series Plots"),
                    tags$hr(
                      style="border-color: grey;"
                    ),
                    p("Time series plots are created to observe any trends and abnormalities from the intervention analysis datasets."),
                    h3("Monthly Apprehension"),
                    plotlyOutput("mon"),
                    h3("Annual Apprehension"),
                    plotlyOutput("app"),
                    h3("Annual Lawful Permanent Resident Status"),
                    plotlyOutput("lpr")
            ),
            ##### TAB: Intervention Analysis #####
            tabPanel("Intervention Analysis",
                     h2("Intervention Analysis"),
                     tags$hr(
                       style="border-color: grey;"
                     ),
                     h3("Summary of Impact"),
                     p("The total intervention event analysis summarized in the table below shows that the majority of the events have a temporary negative impact. Depending on the range of the datasets, not all events were analyzed."),
                     img(src="impact_analysis.jpeg", width="50%", height="50%"),
                     p(tags$b("Note:"), "The dashed line (-) indicates that the event was not analyzed for that dataset."),
                     h3("Monthly Apprehension Dataset"),
                     img(src="app_monthly_ie_impact_plot.jpeg", width="75%", height="75%"),
                     h3("Annual Apprehension Dataset"),
                     img(src="app_ie_impact_plot.jpeg", width="75%", height="75%"),
                     h3("LPR Dataset"),
                     img(src="lpr_ie_impact_plot.jpeg", width="75%", height="75%"),
            ),
            ##### TAB: Time Series Clustering #####
            tabPanel("Dendrogram",
                     ##### Dendrogram #####
                     h2("Dendrogram"),
                     tags$hr(
                       style="border-color: grey;"
                     ),
                     p("The figure below shows dendrograms for each dataset under different clustering methods.",
                       "The different color branches (green and red) indicate the cluster assignment for each sending country.",
                       "Overall, the dendrograms that appeared to show the best method with an even cluster assignment distribution is a divisive hierarchical cluster using DTW as a distance measure."),
                     h3("Apprehension Dataset"),
                     img(src="app_dendrograms.jpeg", width="75%", height="75%"),
                     h3("LPR Dataset"),
                     img(src="lpr_dendrograms.jpeg", width="75%", height="75%"),
                     h3("Naturalization Dataset"),
                     img(src="nat_dendrograms.jpeg", width="75%", height="75%"),
                     h3("Nonimmigrant Dataset"),
                     img(src="non_dendrograms.jpeg", width="75%", height="75%"),
            ),
            ##### TAB: Prototypes #####
            tabPanel("Prototypes",
                     ##### Prototypes #####
                     h2("Prototypes"),
                     tags$hr(
                       style="border-color: grey;"
                     ),
                     p("Prototyping captures important trends of a group of sending countries with similar shape. Mostly the clustering algorithms and centroid methods resulted in similar prototypes as seen in the figures below. However, there was a bigger distinction between SBD and DTW prototypes compared to average and divisive prototypes."),
                     h3("Apprehension Dataset"),
                     img(src="app_prototypes.jpeg", width="75%", height="75%"),
                     h3("LPR Dataset"),
                     img(src="lpr_prototypes.jpeg", width="75%", height="75%"),
                     h3("Naturalization Dataset"),
                     img(src="nat_prototypes.jpeg", width="75%", height="75%"),
                     h3("Nonimmigrant Dataset"),
                     img(src="non_prototypes.jpeg", width="75%", height="75%"),
            ),
            ##### TAB: Cluster Assignment #####
            tabPanel("Clustering Assignment",
                     ##### Clustering Assignment #####
                     h2("Clustering Assignment"),
                     tags$hr(
                       style="border-color: grey;"
                     ),
                     p("The cluster assignment for each dataset and method can be seen in the tables below. Since three or more clusters resulted in similar prototypes with complex interpretations, two clusters were only built for analyzing sending countries."),
                     p(tags$b("Note:"), "The maps below only show the selected clustering method, which is divisive hierachical clusters using DTW as a distane measurement. Also, some countries are not labeled on the maps but can be located in the tables."),
                     h3("Apprehension Dataset"),
                     DT::dataTableOutput("app_clust"),
                     img(src="Map_App_DTW_DIANA.png", width="100%", height="100%"),
                     h3("LPR Dataset"),
                     DT::dataTableOutput("lpr_clust"),
                     img(src="Map_LPR_DTW_DIANA.png", width="100", height="100%"),
                     h3("Naturalization Dataset"),
                     DT::dataTableOutput("nat_clust"),
                     img(src="Map_Nat_DTW_DIANA.png", width="100%", height="100%"),
                     h3("Nonimmigrant Dataset"),
                     DT::dataTableOutput("non_clust"),
                     img(src="Map_Non_DTW_DIANA.png", width="100%", height="100%"),
                     p()
            ),
           ##### TAB: Case Study #####
           tabPanel("Case Study",
                    ##### Case Study #####
                    h2("Case Study: Mexico & Northern Triangle"),
                    tags$hr(
                      style="border-color: grey;"
                    ),
                    p("In this case study, time series clustering results from Mexico and Norther Triangle countries (Guatemala, Honduras, and El Salvador) were analyzed further."),
                    p("The gap between Mexico and the Northern Triangle countries can be due to a variety of factors such as the accessibility to the U.S. Mexico brings in more traffic since it borders the country.",
                      "However, the large inflow of Mexicans crossing the border seeking employment is slowly being replaced by the Northern Triangle countries seeking protection."),
                    h3("Time Series Plots"),
                    p("Based on the time series plots for each dataset, Mexico appears to be very different from the Northern Triangle countries.",
                      "Mexico has more apprehensions, LPR, naturalizations, and nonimmigrant visa issuances.",
                      "This difference in height can be deceiving in assuming that Mexico is in a different cluster from the Northern Triangle countries.",
                      "However, the shape of the time series must be the determining factor for clustering."),
                    img(src="case_study_tsplot.jpeg", width="75%", height="75%"),
                    h3("Cluster Assignment"),
                    p("Regarding cluster assignment of Mexico and the Northern Triangle countries, a divisive hierarchical clustering method with DTW as the distance measure is selected for analysis."),
                    DT::dataTableOutput("case_study_clust"),
                    h4("Summary"),
                    tags$ul(
                      tags$li(tags$b("Apprehension:"), "The results show Mexico as a different cluster from the Northern Triangle countries. Since Mexico borders the U.S., there are more opportunities for Mexicans to cross the border."),
                      tags$li(tags$b("LPR & Naturalization:"), "Both results show both Mexico and the Northern Triangle countries all under the same cluster.",
                              "The reason for this may be due to an annual quota of nonimmigrant visa issuances per country which then limit the number of LPRs and naturalizations."), 
                      tags$li(tags$b("Nonimmigrant Status:"), "Mexico, Honduras, and Guatemala are assigned to the same cluster while El Salvador is assigned to another. Since cluster one and two have similar prototype shapes, it is hard to determine why there is a mismatch between El Salvador and the rest of the countries without further analysis."),
                    ),
           ),
), # navbarMenu

##### TAB: Conclusion #####
tabPanel("Conclusion",
         h2("Conclusion"),
         tags$hr(
           style="border-color: grey;"
         ),
         p("This study determined the political and economic factors and the length of their impact on U.S. immigration through intervention analysis."),
         p("The significant emigration trends from sending countries were also explored using time series clustering. A case study from the results focused on Mexico and the Northern Triangle countries to analyze the effectiveness of time series clustering and open the door for further analysis on other sending countries."),
         img(src="immigration_banner.png", width="100%"),
),
           
##### TAB: References #####
tabPanel("References",
         h2("Related Projects"),
         tags$hr(
           style="border-color: grey;"
         ),
         h3("Immigration Nation"),
         p(tags$b("GitHub:"), a("https://github.com/ohkaaaaay/Immigration_Nation")),
         h4("Description"),
         p("Performing a multivariate time series analysis on the number of undocumented immigrants apprehended and deported each year. This was a group project completed in the MIS-748 (Seminar in Applied Multivariate Analytics) course."),
         p("The following time series procedures were done:"),
         tags$ul(
           tags$li("Time series plot"),
           tags$li("Intervention events"), 
           tags$li("Forecasting"),
           tags$li("Impulse response function (IRF)")
         ),
         h4("Datasets"),
         h5("Undocumented Immigrants Apprehended in the U.S."),
         p(tags$b("Kaggle:"), a("https://www.kaggle.com/ekayfabio/immigration-apprehended")),
         img(src="app_banner.jpeg", width="100%", height="100%"),
         h5("Undocumented Immigrants Deported in the U.S."),
         p(tags$b("Kaggle:"), a("https://www.kaggle.com/ekayfabio/immigration-deported")),
         img(src="dep_banner.jpeg", width="100%", height="100%"),
         p()
)

) # navbarPage
) # fluidPage

# Define server logic ----
server <- function(input, output) {
#####
  # Plot TS outputs
  output$mon <- renderPlotly({
     ggplotly(mon.app) # Monthly apprehension
  })
  output$app <- renderPlotly({
    ggplotly(ann.app) # Annual apprehension
  })
  output$lpr <- renderPlotly({
    ggplotly(ann.lpr) # Annual LPR
  })
  
  # Output cluster assignment
  output$app_clust = DT::renderDataTable({
    app_clust
  })
  output$lpr_clust = DT::renderDataTable({
    lpr_clust
  })
  output$nat_clust = DT::renderDataTable({
    nat_clust
  })
  output$non_clust = DT::renderDataTable({
    non_clust
  })
  output$case_study_clust = DT::renderDataTable({
    case_study_clust
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

##### REFERENCE #####
# Winners of 2nd Annual R-Shiny Competition
# Link: https://blog.rstudio.com/2020/07/13/winners-of-the-2nd-shiny-contest/
# Favorite App: https://kneijenhuijs.shinyapps.io/Datify/?_ga=2.165056255.1126449759.1627279149-1278464522.1626404852
# Shiny Themes: https://rstudio.github.io/shinythemes/
# Bootstrap: https://bootswatch.com/
