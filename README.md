# BIS data dashboard

This repository contains code to create a dashboard of BIS statistics using [R markdown](https://rmarkdown.rstudio.com/index.html) and [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/).

The dashboard will give you a set of charts for a user-specified economy as well as the global economy. The presentation is fairly general and self-explanatory, but sticks to the BIS's abbreviation format for concepts and identifiers in order to work well on small screens. By default, exchange rate data are based on [nominal effective exchange rates](https://stats.bis.org/statx/toc/XR.html) (which cover fewer countries but longer history), [bank liabilities](https://stats.bis.org/statx/toc/LBS.html) are shown with a negative sign, and [domestic debt securities](https://www.bis.org/statistics/about_securities_stats.htm) are calculated as the difference between [total and international debt securities](https://www.bis.org/statistics/about_securities_stats.htm) (as some economies do not report them separately).

## How to use the code

1. Save a copy of `bis_dashboard.Rmd`, `bis_helper_functions.R`, `refresh_bis_data.R` in a folder of your choice. Open R and set that folder as your working directory.

    ``` r
    setwd("/home/user/path/to/bisdashboard")
    ```

2. Source the file `refresh_bis_data.R` to download raw data from the BIS and save it in a local `.RData` file. This file will function as a local cache to avoid unnecessary calls to the BIS's servers. You will need to run this command again every time you wish to update for the latest data.

    ``` r
    source("refresh_bis_data.R")
    ```

3. Once you have the data, call rmarkdown's `render()` function to create a dashboard for an economy of your choice, identified by ISO 2-character code, e.g. "JP" for Japan.

    ``` r
    rmarkdown::render("bis_dashboard.Rmd", params = list(ctr = "JP"), output_file = "bis_dashboard_JP.html")
    ```

4. When this is complete, open the HTML file produced by rmarkdown (`bis_dashboard_JP.html` in the above example). Note that the file may take a moment to load, depending on the amount of data to display.

## Examples

* [United States](https://media.portblue.net/resources/190211_bis-data-dashboard/bis_dashboard_US.html)
* [Japan](https://media.portblue.net/resources/190211_bis-data-dashboard/bis_dashboard_JP.html)
* [United Kingdom](https://media.portblue.net/resources/190211_bis-data-dashboard/bis_dashboard_GB.html)
* [Euro Area](https://media.portblue.net/resources/190211_bis-data-dashboard/bis_dashboard_XM.html)

## Screenshots

![Screenshot 01](https://media.portblue.net/resources/190211_bis-data-dashboard/bisdashboard01.png)
![Screenshot 02](https://media.portblue.net/resources/190211_bis-data-dashboard/bisdashboard02.png)
![Screenshot 03](https://media.portblue.net/resources/190211_bis-data-dashboard/bisdashboard03.png)

## Disclaimer

This dashboard is not officially related to, or endorsed by, the BIS. The code comes with no guarantee of correctness.
