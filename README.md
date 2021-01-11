# [Group 9] Clustering Countries By Their Covid-19 Confirmed/Death/Recovered Time Series

### Groups
* Choi Pui Shan, 109753158
* Armando Serrato, 107753048

### Goal
* Clustering Countries By Their Covid-19 Confirmed/Death/Recovered Time Series
* Plotting Covid-19 Data On Shiny Interactive App
* To Understand Which Countries Have Similar Time Series Of Covid-19 Cases & Their Trends => Make A Reference When Doing Business

### Demo 
You should provide an example commend to reproduce your result
* As we are using data source from github that will be updated daily, so there is no input .csv file needed.
* When running the code, it will already generate the cluster lists into several .csv file.
```R
Rscript code/TimeSeriesCluster.R
```
* any on-line visualization
  * No

## Folder organization and its related information

### docs
* Your presentation, 1091_datascience_FP_<yourID|groupName>.ppt/pptx/pdf, by **Jan. 12**
* Any related document for the final project
  * papers
  * software user guide

### data

* Source

https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series

* Input format
  * Province.State
  * Country.Region
  * Latitude
  * Longitude
  * Time Series of Cases

* Any preprocessing?
  * Change the data from accumulated cases into daily change cases
  * Take Lat and Long for Canada in the Recovered Data to replace the missing value in ConfirmedData and Death Data
  * Insert the 2nd column into 1st column as the Province.State of that row
  * Remove 42th row of ConfirmedData and Death Data
  * Standandized the data as zscore when doing hierarchical clustering

### code

* Which method do you use?
  * Hierarchical clustering of Time Series
  * Projection of clustering results on PCA
  * Plot on World Map
* What is a null model for comparison?
  * No
* How do your perform evaluation? ie. Cross-validation, or extra separated data
  * DTW

### results

* Which metric do you use 
  * DTW
* Is your improvement significant?
  * No Comparisions
* What is the challenge part of your project?
  * Choosing suitable k

## References
* Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
* Packages you use
  * dtwclust
  * map
  
* Related publications
  * https://www.r-bloggers.com/2013/04/r-beginners-plotting-locations-on-to-a-world-map/

