# FairSubset
R Shiny App for fair subsetting of data

To run FairSubset, R and RStudio must be installed.  
https://www.r-project.org/
https://www.rstudio.com/

Once the R software is installed, packages must be installed within RStudio using the following command:
> install.packages(c("shiny", "data.table", "ggplot2", "matrixStats") 

Once the packages are installed, open either the ui.R file or the server.R file in RStudio.

Click the "Run App" button to get started.

This App is intended to assist in the automatic subsetting of data.  Input data will be tested for a user specified average type (mean or median) and for standard deviation.  Random subsetting will be performed on the data and the subset which best matches the original data with a smaller sample size will be output as the "Fair Subset".
