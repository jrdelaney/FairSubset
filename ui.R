library(shiny)
library(ggplot2)
library(shinyjs)
library(shinyBS)

fluidPage(
  
  titlePanel("FairSubset: Subset your data into equal sample sizes which are best representative of each complete set of data"),
  
  sidebarPanel(
    
    tags$head(
      tags$style(HTML('#run_simulations{background-color:#9999FF; text-color:black; border-color:black; font-weight:bold}'))
    ),

    textAreaInput('pasted_data_1',"Paste your data here, with each sample in a column", value = "" ,width = "67%", height = "67%", resize = "both"),
    fileInput('uploaded_data_1', 'Or upload your data', multiple = FALSE),
    downloadLink("example_data_download", "Download example input data"),
    br(),
    br(),
    
    actionButton('run_simulations', 'Find best representative subset'),
    actionButton("refresh", "Refresh All Inputs"),
    
    conditionalPanel(
      condition = "input.average_setting != 'Kolmogorov-Smirnov' ",
    helpText("Runs 1000 subsets of data to test for averages and standard deviations which are most similar to your original data.  By default, N is your smallest sample size.")
    ),
    conditionalPanel(
      condition = "input.average_setting == 'Kolmogorov-Smirnov' ",
      helpText("Runs 1000 subsets of data to test for data distributions which are most similar to your original data.  By default, N is your smallest sample size.")
    ),
    textInput('manual_N',"Manually set desired number of data points (N) per group"),
    selectInput('average_setting', 'Subsetting criterion', c("Mean","Median", "Kolmogorov-Smirnov"), selected = "Mean"),
    bsTooltip(id = "average_setting", 
              title =  "Mean or median will choose the subset averages and SDs which best match the original data.  The Kolmogorov-Smirnov option will perform KS tests and select the subset with the highest p-value (most similar to) the original data, and can be used for skewed data.",
              placement = "top", options = NULL
    ),
    sliderInput('plot_width', "Plot width", min = 0, max = 1000, value = 400),
    sliderInput('plot_height', "Plot height", min = 0, max = 2000, value = 210)

  ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.executed != 'done' ",
        helpText('Please input your data.  For an example of how the data should be formatted, please click "Download example input data"'),
        br(),
        img(src='FairSubset_Landing.PNG', align = "left", width = 637, height = 302)
      ),
      
      conditionalPanel(
        condition = "output.executed == 'done' ",
      span(textOutput('warning'), style="color:red"),
      br(),
      h3("Subsetted data averages +/- standard deviation"),
      img(src = "average_sd_label.JPG", height = 133, width = 230),
      br(),
      uiOutput('ui_plot'),
      br(),
      h3("Demo plotted data"),
      uiOutput('ui_plot2'),
      br(),
      downloadButton("download_best_subset", "Download Fair Subset of Data"),
      helpText("These data are the most representative of your entire set of data, subsetted to the lowest N within your columns (or the manually set N).  Use data for plotting in Excel and other graphing software.  Statistics should be performed on original data; subsetting is intended to reduce visual bias."),
      downloadButton("download_worst_subset", "Download Worst Subset of Data"),
      helpText("Because... live a little!")
      )
    )
  
    
)
