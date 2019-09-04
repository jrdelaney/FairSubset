# FairSubset
R Shiny App for fair subsetting of data

Detailed description of the App and the underlying calculations can be found in the publication at:
http://www.jbmethods.org/jbm/article/view/299/265

To run FairSubset as a Shiny App, R and RStudio must be installed.  
https://www.r-project.org/
https://www.rstudio.com/

Once the R software is installed, packages must be installed within RStudio using the following command:
> install.packages(c("shiny", "data.table", "ggplot2", "matrixStats", "shinyjs", "shinyBS"))

Once the packages are installed, open either the ui.R file or the server.R file in RStudio.

Click the "Run App" button to get started.  

This App is intended to assist in the automatic subsetting of data.  Input data will be tested for a user specified average type (mean or median) and for standard deviation.  For more complex data without normal distribution, the "Kolmogorov-Smirnov" setting will use the Kolmogorov Smirnov test to choose the best subset.  Random subsetting will be performed on the data and the subset which best matches the original data with a smaller sample size will be output as the "Fair Subset".

For users who which to use the fairsubset function within R, the package "fairsubset" may be downloaded from https://github.com/jrdelaney/FairSubset/
