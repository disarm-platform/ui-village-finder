# ui-village-finder

This folder includes everything necessary to run the shiny app to detect villages locations from populated locations. 

The app leverages [this](https://github.com/disarm-platform/fn-village-finder) function. 

Note that the app is currently set up to work in a limited number of countries. If you would like this to work on a different population raster dataset, the simplest is to follow these instructions.

* Create an RData file of the population raster of interest (WGS84)
* Host the RData file somewhere accessible over the web via a URL (Dropbox, Google drive etc.)
* Create a csv file also accessible over the web with 2 columns i) the ISO3 code ii) the URL to the RData file. See [here](https://www.dropbox.com/s/amp4qeh2z8cs2hs/worldpop_country_urls.csv?dl=1) for the file currently used. 
* Edit the server.R code of this app modifying line 14 to point to the csv file you just created. 

```
data_urls <- read.csv("https://www.dropbox.com/s/amp4qeh2z8cs2hs/worldpop_country_urls.csv?dl=1",
                      header= T)
```

You will also need to change line 30 to match the filename you give your RData file.

```
      filename_raster_pop <- list(filename = paste0(ISO3, "_worldpop_2015_ppp.RData"),
                                  url = as.character(data_urls$URL[data_urls$ISO3==ISO3]))
```

Written by francois.rerolle@ucsf.edu

