# data4health <a href='https://www.harmonize-tools.org/'><img src='https://harmonize-tools.github.io/harmonize-logo.png' align="right" height="139" /></a>

<!-- badges: start -->
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!---
[![GitHub
contributors](https://img.shields.io/github/contributors/harmonize-tools/data4health)](https://github.com/harmonize-tools/data4health/graphs/contributors)
![commits](https://badgen.net/github/commits/harmonize-tools/data4health/main)
-->
<!-- badges: end -->

## Overview
<p style="font-family: Arial, sans-serif; font-size: 14px;">

  data4health is a tool developed as part of the HARMONIZE project to facilitate the access, preprocessing, and aggregation of health data at customized spatiotemporal resolutions. Originally designed for data from Colombia, Brazil, Peru, and the Dominican Republic, the tool is intended to be adaptable for any linelist health data.

The R package and offers two modes of operation based on the user's coding experience:
<ul>
  <li> For users with <b>coding experience</b>: A wide range of functions can be directly used within R. </li>
  <li> For <b>non-coding users</b>: A graphical user interface (GUI) guides users through the data processing pipeline in an intuitive, user-friendly way. </li>
</ul>

Key Features of the R Package:

<ul>
  <li> Instructions on how to access health data </li>
  <li> Functions for cleaning and preprocessing health data </li>
  <li> Spatial harmonization, allowing aggregation to any coarser administrative unit </li>
  <li> Temporal harmonization, enabling aggregation to epidemiological weeks or months </li>
  <li> Data visualization capabilities</li>
  <li> Output as a .csv file, formatted to meet user-specified requirements</li>
</ul>

</p>


## Dependencies

<table>
  <tr>
    <td align="center">
      <a href="https://cran.r-project.org/web/packages/dplyr/index.html" target="_blank">
        <img src="https://tidyverse.tidyverse.org/logo.png" height="50" alt="dplyr logo">
      </a>
    </td>
    <td align="left">
      <strong>dplyr</strong><br>
      Provides a set of tools for efficiently manipulating datasets in R.<br>
    </td>
  </tr>
  <tr>
    <td align="center">
      <a href="https://cran.r-project.org/web/packages/ggplot2/index.html" target="_blank">
        <img src="https://imgs.search.brave.com/7xErK1yv_WwEZ-syGmCUbH4n1THQcF7ukwTLS42zAyM/rs:fit:860:0:0/g:ce/aHR0cHM6Ly9yLWdy/YXBoLWdhbGxlcnku/Y29tL2ltZy9vdGhl/ci9nZ3Bsb3QySGV4/LmpwZw" height="50" alt="ggplot2 logo">
      </a>
    </td>
    <td align="left">
      <strong>ggplot2</strong><br>
      Used for creating complex plots from data in a data frame.<br>
    </td>
  </tr>
  <tr>
    <td align="center">
      <a href="https://cran.r-project.org/web/packages/shiny/index.html" target="_blank">
        <img src="" height="50" alt="shiny logo">
      </a>
    </td>
    <td align="left">
      <strong>shiny</strong><br>
      Facilitates building interactive web apps straight from R.<br>
    </td>
  </tr>
  
  <tr>
    <td align="center">
      <a href="https://rstudio.github.io/leaflet/" target="_blank">
        <img src="https://filippomariaraeli.com/post/leaflet/summary/featured.png" height="50" alt="leaflet logo">
      </a>
    </td>
    <td align="left">
      <strong>leaflet</strong><br>
      Leaflet makes it easy to create mobile-friendly interactive maps from R. <br>
    </td>
  </tr>
  
  <tr>
    <td align="center">
      <a href="https://lubridate.tidyverse.org/" target="_blank">
        <img src="https://lubridate.tidyverse.org/logo.png" height="50" alt="shiny logo">
      </a>
    </td>
    <td align="left">
      <strong>lubridate</strong><br>
      Lubridate makes it easier to do the things R does with date-times and possible to do the things R does not.<br>
    </td>
  </tr>
</table>

<!-- If not yet installed, users can install dependencies with the following lines.
```r
packages <- c("dplyr", "DT", "foreign",  "ggplot2", "lubridate", "leaflet",
               "shinyWidgets", "shiny", "shinycssloaders",  "sf", "zoo", "read.dbc")
devtools::install_github("danicat/read.dbc")
install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")
``` -->


## Installation

You can install the latest version of the package from GitHub using the `remotes` package:

```R
# Install remotes if you haven't already
install.packages("devtools")

# Install the package from GitHub
devtools::install_gitlab("your_gitlab_username/your_package_name")
```
Keep in mind that the functions that are still being developed can be found in a branch named "dev_" + functionname.

## How to Use it / Vignette

There are two main functionalities of data4health. For code-experienced users, a series a functions to support health data analysis are provided that useres can implement to simplify their existing data pipeline. Users with less code experience can employ the graphic user interface to clean and aggregate their data in a user friendly way.

<details>
<summary>
 Functions
</summary>
  
#### Explore and clean

Since the cleaning process is different for each country, there are prepared scripts for each disease and each of the project countries. You can easily load these scripts, and then run it line by line, adjusting it whenever needed, with the function "data4health_loadscript". Just define what country and disease you would like to work on. If the function is run empty, it will give you the available countries and diseases.

```r
data4health_loadscript()
```

The above line will give you the follwoing error message:

```r
Error in data4health_loadscript() : Disease and country are missing. 
                Country must be one of the following: "Brazil","Colombia","Peru", "Domrep".
 
Disease must be one of the following: "malaria", "dengue".
```
Executing the line with the correct country name and disease will make your editor open a new file.
```r
data4health_loadscript(country = "brazil", disease = "malaria")
```
Save this file, And execute it line by line, learning and adjusting it if needed. Use the last line of the code to save the cleaned health data to a secure folder.

#### Clean

In most cases, data of different years is similar enough so that there is no need to ecplore it every single year and you can automate it using the data4health_clean function:

```r
data4health_clean(country = "peru", disease = "dengue",
                       input_file = "data/peru/datos_abiertos_vigilancia_dengue.csv",
                       output_file = "/shared/earth/GHR/daniela/harmonize_datapaper/clean_data_output/peru_dengue.csv",
                       cols_to_remove = c("diresa", "ubigeo", "localcod"),
                       missing_threshold = 20,
                       age_threshold = 120)
```

#### Filter


```r
 data <- data.frame(
   ID = c(1, 2, 3, 4, 5),
   Name = c("Alice", "Bob", "Charlie", "David", "Emily"),
   Age = c(25, 30, 22, 28, 35),
   Date = as.Date(c("2023-01-15", "2023-02-20", "2023-03-05", "2023-04-10", "2023-05-18")),
   City = c("New York", "London", "Paris", "Tokyo", "Sydney")
 )
 filtered_data <- data4health_filter(
   data,
   Age = list(over = 25),
   Date = list(between = c("2023-02-10", "2023-04-15")),
   City = list(exclude = c("London", "Tokyo"))
 ) 
```
#### Aggregating

To aggregate from linelist data to a an aggregated datframe, you can use the following function

```r
 data <- data.frame(
   space = c("A", "B", "B", "B"),
   day = c("2023-01-15", "2023-05-20", "2023-01-27", "2023-02-02"),
   month = c("2023-01-01", "2023-05-01", "2023-01-01", "2023-02-01"),
   month.name = c("jan", "may", "jan", "feb"),
   value1 = c(10, 15, 20, 25),
   value2 = c(5, 8, 12, 17),
   gender = c("Male", "Female", "Female", "Female")
 )

data4health_aggregate(df=data, time_col="day",space_col = "space", "gender")
data4health_aggregate(data, time_col = "time", space_col = "space", "gender")
data4health_aggregate(df=data, time_col="month.name",space_col="space", "gender")

```

#### Visualise
```r
plot_map()
```
</details>
<details>
<summary>
Graphic user interface
</summary>

#### Load GUI
Once data4health is loaded, the user interface can be loaded with the following command:

```r
library(data4health)
run_gui()
```
A browser window will automatically open, and the landing page will look like this:

<img src="img/landing_page.png" width="80%" alt="cleaning UI">

#### Clean

<img src="img/preprocess.png" width="80%" alt="cleaning UI">

#### Aggregate

<img src="img/agregate.png" width="80%" alt="cleaning UI">

#### Visualise

<img src="img/plot.png" width="80%" alt="cleaning UI">

</details>

## Resources

<details>
<summary>
Other HARMONIZE tools
</summary>

HARMONIZE collates existing multi-source climate, environmental, socio-economic and health data, as well as collects new longitudinal ground-truth data using drone technology and low-cost weather sensors. Each data source has its own digital toolkit to allow local researchers and users, to prepare, interrogate and eventually merge the data spatio-temporally, to understand the links between environmental change and infectious disease risk in their local context, and to build robust early warning and response systems in low-resource settings. the other toolkits are:
<ul>
  <li> [clim4health]("https://github.com/harmonize-tools/clim4health") </li>
  <li> [land4health](https://github.com/harmonize-tools/land4health) </li>
  <li> [drone4health](https://github.com/harmonize-tools/drone4health) </li>
  <li> [socio4health](https://github.com/harmonize-tools/socio4health) </li>
</ul>

</details>

<!---
<details>
<summary>
Package Website
</summary>

The [`example` website](https://cran.r-project.org/) package website includes a function reference, a model outline, and case studies using the package. The site mainly concerns the release version, but you can also find documentation for the latest development version.

</details>
-->
<details>
<summary>
Organisation Website
</summary>

[Harmonize](https://www.harmonize-tools.org/) is an international develop cost-effective and reproducible digital tools for stakeholders in hotspots affected by a changing climate in Latin America & the Caribbean (LAC), including cities, small islands, highlands, and the Amazon rainforest.

The HARMONIZE digital toolkits will allow local researchers and users, including national disease control programs, to link, interrogate and use multi-scale spatiotemporal data, to understand the links between environmental change and infectious disease risk in their local context, and to build robust early warning and response systems in low-resource settings.

The project consists of resources and [tools](https://harmonize-tools.github.io/) developed in conjunction with different teams from Brazil, Colombia, Dominican Republic, Peru and Spain.

</details>
  
## Organizations

<table>
  <tr>
    <td align="center">
      <a href="https://www.bsc.es/" target="_blank">
        <img src="https://imgs.search.brave.com/t_FUOTCQZmDh3ddbVSX1LgHYq4mzCxvVA8U_YHywMTc/rs:fit:500:0:0/g:ce/aHR0cHM6Ly9zb21t/YS5lcy93cC1jb250/ZW50L3VwbG9hZHMv/MjAyMi8wNC9CU0Mt/Ymx1ZS1zbWFsbC5q/cGc" height="64" alt="dplyr logo">
      </a>
    </td>
    <td align="left">
      <strong>GHR</strong><br>
      Global Health Resilience
    </td>
  </tr>
</table>


## Authors / Contact information

List the authors/contributors of the package and provide contact information if users have questions or feedback.
</br>
</br>
<a href="https://github.com/Daniela-L">
  <img src="https://avatars.githubusercontent.com/u/76750744?v=4" style="width: 50px; height: auto;" />
</a>
<span style="display: flex; align-items: center; margin-left: 10px;">
  <strong>Daniela Lührsen</strong> (developer)
  <a href="https://orcid.org/0009-0002-6340-5964" style="margin-left: 10px;">
    <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" />
  </a>
</span>

<a href="https://github.com/drrachellowe">
  <img src="https://imgs.search.brave.com/5LHcD0fArBHiqOOzb1AlCj7YGRHVMHCZcK_kYao0aos/rs:fit:500:0:0:0/g:ce/aHR0cHM6Ly9jZG4t/aWNvbnMtcG5nLmZy/ZWVwaWsuY29tLzI1/Ni80NjYxLzQ2NjEz/MTgucG5nP3NlbXQ9/YWlzX2h5YnJpZA" style="width: 50px; height: auto;" />
</a>
<span style="display: flex; align-items: center; margin-left: 10px;">
  <strong>Raquel Lana</strong> (developer)
</span>

## Citation

- **APA Format:**
  - Lowe, R. (2020). *How to use the NetCDF files*. Package documentation. Retrieved from https://cran.r-project.org/).