# [Group 9] Clustering Countries By Their Covid-19 Confirmed/Death/Recovered Time Series

### Groups
* ���ج�, 109753158
* �����M, 107753048

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
Province.State,Country.Region,Latitude,Longitude,...(Time Series of Cases)

* Any preprocessing?
  * Change the data from accumulated cases into daily change cases
  * Handle missing data
    *Found that 43th,53th row of ConfirmedData and Death Data missed the value for Lat and Long, and the number of cases of them are really small(1,1,13),but in the Recovered Data, there are Lat and Long for Canada
     *take them to replace the missing value in ConfirmedData and Death Data
    *As some of the 1st column are empty
     *insert the 2nd column into 1st column as the Province.State of that row
  *Found that 42th row of ConfirmedData and Death Data are strange, and required case is only 1
   *Remove 42th row of ConfirmedData and Death Data
  * Scale value 
   *Standandized the data as zscore when doing hierarchical clustering

### code

* Which method do you use?
  *Hierarchical clustering of Time Series
  *Projection of clustering results on PCA
  *Plot on World Map
* What is a null model for comparison?
  *No
* How do your perform evaluation? ie. Cross-validation, or extra separated data
  *DTW

### results

* Which metric do you use 
  *DTW
* Is your improvement significant?
  *No Comparisions
* What is the challenge part of your project?
  *Choosing suitable k

## References
* Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
* Packages you use
  *dtwclust
  *map
  
* Related publications
  *https://www.r-bloggers.com/2013/04/r-beginners-plotting-locations-on-to-a-world-map/

