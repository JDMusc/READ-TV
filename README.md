read-tv, Research & Exploratory Analysis Driven Time-data Visualization.
Version 0.1
================

## Description

read-tv is a shiny application and associated console interface designed
for visualizing changepoints in **irregularly and regularly spaced**
longitudinal data. Technical users can rapidly view data through the R
console interface, and non-technical users can use it as a standard web
application. Intermediate users, as well as users who want to replicate
plots, will benefit from the source code generation associated with any
plot.

This application is built on top of a wide array of packages, and
especially wraps functionality from the
[changepoint](https://cran.r-project.org/web/packages/changepoint/index.html)
package and [tidyverse](https://github.com/tidyverse) set of packages.

Below is a plot of new COVID-19 cases in the United States with detected
changepoints. <img src="tools/readme/covid_global_us.png">

## Installation

This package is not on CRAN. Please install from GitHub with the
[install\_github function from the remotes
package](https://www.rdocumentation.org/packages/remotes/versions/2.2.0/topics/install_github).

## Usage

### Data requirements

A data frame or CSV file where each row is an observation. If the data
does not have columns for Case, Event.Type, or Time, then read-tv will
open a pop-up that enables the user to map Case and Event.Type to
another column (or mock value), and map Time to a column.

### The main function is **launchReadtv**:

``` r
# launchReadtv() #uncomment to launch the web application
```

Or it can be launched as

``` r
app = readtv::launchReadtv()
#shiny::runApp(app) #uncomment to launch the web application
```

Both files and in-memory objects can be passed to **launchReadtv**

``` r
library(magrittr)
library(readtv)

app = readtv::japan_eq_3_11 %>% 
  launchReadtv(plotOpts = tvOpts(y = mag, x = Time, color = place))

#shiny::runApp(app)
```

The above code generates the following if `shiny::runApp` is executed.
The `plotOpts` argument and the `tvOpts` function tells the plot to load
with the specified axes, colors, etc.
<img src="tools/readme/japan_eq_3_11.png">

read-tv exports 5 functions and 3 data sets. The 5 functions are
`launchReadtv`, `tvOpts`, `preprocessForCpa`, `slidingWindow`, and
`generatePlotDefaults`.

Each of these functions has associated help documentation. The 3 data
sets are: \* [Global COVID data, from Our World in
Data](https://ourworldindata.org/coronavirus-source-data) \* [US
state-based COVID data, from NY
Times](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv)
\* [USGS seismic records from Japan on
March 11, 2011](https://earthquake.usgs.gov/earthquakes/map).

### CPA tab

The CPA tab has features to regularly space the data (preprocess), and
then execute methods from the changepoints package. It spaces the data
with the `preprocessForCpa` function and `slidingWindow` function.

To avoid the column mapping pop up, we can use dplyr to specify the
columns and mock values (Case) before it is passed into `launchReadtv`.

``` r
library(dplyr, warn.conflicts = FALSE)
library(magrittr)
library(readtv)

app = readtv::japan_eq_3_11 %>% 
  mutate(Time = time, Event.Type = place, Case = 1) %>% 
  launchReadtv(plotOpts = tvOpts(y = mag, x = Time, color = place))

#shiny::runApp(app) # and move to CPA tab after viewing in Basic display tab
```

Below is a screen shot from read-tv. Note that the time points are
regularized with values interpolated through a sliding window. The
timing intervals were not originally evenly spaced (irregular) since it
is earthquake event data. The CPA tab has functionality to create
regular spacing.

<img src="tools/readme/japan_eq_3_11_cpa.png">

### Faceting

Sometimes it helps to facet and paginate the data.

``` r
app = readtv::covid_usa %>% 
  mutate(Time = date, Case = state, Event.Type = state) %>% 
  launchReadtv(plotOpts = 
                 tvOpts(y = cases, facetOn = state, isFacetPaginated = TRUE,
                        facetRowsPerPage = 3))

#shiny::runApp(app)
```

<img src="tools/readme/covid_states_facet.png">

And the source code can also be viewed from the app. *Please note that
it assumes `tidyverse`, `changepoint`, and `readtv` packages are
attached*.

<img src="tools/readme/covid_states_facet_src.png">
