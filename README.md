# World Data - Shiny dashboard

This project includes data extraction from several sources, plot construction and Shiny web app deployment.

To run this app locally, clone this repository and simply run the `app.R` file. It will source in `s03_viz.R` which in turn sources `s00_init.R`, the remaining scripts are there to extract and prepare the data.

Note that one of the packages used by this app is `chorddiag` which isn't available on CRAN and has to be downloaded from GitHub:

```r
devtools::install_github("mattflor/chorddiag")
```

Also, make sure that your packages are up to date.

This packages is deployed on [ShinyApps.io](https://ivan-rivera.shinyapps.io/world_data/)

For references, here is the list of data sources used:

* [Word Development Indicators Data](http://data.worldbank.org/products/wdi)
* [WDI package - WDI API wrapper in R](https://cran.r-project.org/web/packages/WDI/WDI.pdf)
* [Energy data for Sankey Plot - International Energy Agency](http://www.iea.org/etp/explore/)
* [Bilateral Trade Chord Diagram data - Economics Web Institute](http://www.economicswebinstitute.org/ecdata.htm)
* [Country Attributes - Languages and Currencies](https://github.com/mledoze/countries)

