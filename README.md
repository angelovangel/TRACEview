# TRACEview
A Shiny app for visualizing HPLC (or other chromatographic) traces. The app reads in chromatogram files from any chromatography system (exported as csv or similar) and plots the traces, allowing also shifting of the traces in the y-axis.
Tested with chromatograms from Chromeleon (some example files can also be found here as a zip).

### Usage  
the easiest way is to run this in RStudio  
`shiny::runGitHub("TRACEview", "angelovangel")`  
or alternatively,  
download the app.R file, open it in RStudio and press the "Run App" button.  

### Prerequisites
The libraries required (have to be installed in `R` first) are:  
`ggplot2` `dplyr` `purrr` `readr` `stringr` `rbokeh` `shiny` and `shinydashboard`

Have fun!  
Angel


