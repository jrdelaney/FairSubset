library(shiny)
library(data.table)
library(ggplot2)
library(matrixStats)

function(input, output) {
  
  example_data_download <- as.data.frame(fread(paste0("Example_Data.csv"), header= TRUE, stringsAsFactors = FALSE))
  output$example_data_download <- downloadHandler(
    filename = function() {
      paste("Data_example.csv", sep="")
    },
    content = function(file) {
      write.csv(example_data_download, file,row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  
  statistical_shiny <- eventReactive(input$run_simulations,{
    
    if(!is.null(input$uploaded_data_1)){
      pasted_data_1 <- as.data.frame(fread(input$uploaded_data_1$datapath, stringsAsFactors = FALSE))
    } else {
    pasted_data_1 <- as.data.frame(fread(input$pasted_data_1, stringsAsFactors = FALSE))
    }
    #testing: pasted_data_1 <- as.data.frame(fread("Example_Data_char.csv", header = TRUE, stringsAsFactors = FALSE))
    warning <- ""
    average_setting <- input$average_setting
    
    pasted_data_header <- colnames(pasted_data_1)
    pasted_data_1 <- lapply(1:ncol(pasted_data_1), function(column){unlist(pasted_data_1[,column])[!is.na(unlist(pasted_data_1[,column]))]})
    
    if(length(pasted_data_1[sapply(unlist(pasted_data_1), is.numeric)]) != length(unlist(pasted_data_1))){
      suppressWarnings(
      pasted_data_1 <- lapply(1:length(pasted_data_1), function(list_item){
        as.numeric(unlist(pasted_data_1[[list_item]]))[!is.na(as.numeric(unlist(pasted_data_1[[list_item]])))]
                                                   })
      )
    warning <- "Your data may contain non-numeric values.  These have been removed prior to calculations."
    }
    
    shortest_data <- min(sapply(1:length(pasted_data_1), function(list_item){length(pasted_data_1[[list_item]])}))
    if(input$manual_N != ""){shortest_data <- as.integer(input$manual_N)}
    sampled_data_report <- as.data.frame(matrix(0, nrow=2, ncol = length(pasted_data_1)))
    colnames(sampled_data_report) <- pasted_data_header
    row.names(sampled_data_report) <- c("average","standard deviation")
    
    sampled_data_best_indices <- as.data.frame(matrix(0, nrow=shortest_data, ncol = length(pasted_data_1)))
    colnames(sampled_data_best_indices) <- pasted_data_header
    
    sampled_data <- as.data.frame(matrix(0, nrow=shortest_data, ncol = length(pasted_data_1)))
    colnames(sampled_data) <- pasted_data_header
    
    data_vector <- rep(0,shortest_data)
    average_value <- 0
    standard_deviation_value <- 0
    
    if(average_setting == "Mean"){
    average_values_all <- sapply(1:length(pasted_data_1), function(list_item){mean(pasted_data_1[[list_item]])})
    } else { #Median
    average_values_all <- sapply(1:length(pasted_data_1), function(list_item){median(pasted_data_1[[list_item]])})  
    }
    
    standard_deviation_values_all <- sapply(1:length(pasted_data_1), function(list_item){sd(pasted_data_1[[list_item]])})
    
    all_sampled_data <- lapply(1:1000, function(iteration){
      for(column in 1:length(pasted_data_1)){
        sampled_data[,column] <- sample(pasted_data_1[[column]],size = shortest_data, replace = FALSE)
      }
      return(sampled_data)
    })
    
    
    if(average_setting == "Mean"){
    average_values_randomized <- lapply(1:1000, function(iteration){
      abs(colMeans(all_sampled_data[[iteration]]) - average_values_all) #subtract to get distance vector from original
    })
    } else { #Median
    average_values_randomized <- lapply(1:1000, function(iteration){
      abs(colMedians(as.matrix(all_sampled_data[[iteration]])) - average_values_all)
    })
    }
    
    standard_deviation_values_randomized <- lapply(1:1000, function(iteration){
      abs(sapply(1:ncol(sampled_data), function(column){sd(all_sampled_data[[iteration]][,column])}) - standard_deviation_values_all) #subtract to get distance vector from original
    })
    
    average_vector <- rep(0,1000)
    standard_deviation_vector <- rep(0,1000)
    sum_vector <- rep(0,1000)
    
    best_simulations <- unlist(sapply(1:length(pasted_data_1), function(list_item){
      
      if(sum(sapply(1:1000, function(iteration){
        average_values_randomized[[iteration]][list_item]
      })) == 0
      ){return(1)} else {
      
      average_vector <-
      sapply(1:1000, function(iteration){
        average_values_randomized[[iteration]][list_item]
      }) /
        sum(sapply(1:1000, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
          average_values_randomized[[iteration]][list_item]
        })) *1000
      
      standard_deviation_vector <- 
      sapply(1:1000, function(iteration){
        standard_deviation_values_randomized[[iteration]][list_item]
      }) / 
        sum(sapply(1:1000, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
          standard_deviation_values_randomized[[iteration]][list_item]
        })) *1000

      sum_vector <- average_vector + standard_deviation_vector #determine best simulation for given column
      
      return(as.integer(which(sum_vector == min(sum_vector))))
      }
      
    }))
    
    worst_simulations <- unlist(sapply(1:length(pasted_data_1), function(list_item){
      
      if(sum(sapply(1:1000, function(iteration){
        average_values_randomized[[iteration]][list_item]
      })) == 0
      ){return(1)} else {
        
        average_vector <-
          sapply(1:1000, function(iteration){
            average_values_randomized[[iteration]][list_item]
          }) /
          sum(sapply(1:1000, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
            average_values_randomized[[iteration]][list_item]
          })) *1000
        
        standard_deviation_vector <- 
          sapply(1:1000, function(iteration){
            standard_deviation_values_randomized[[iteration]][list_item]
          }) / 
          sum(sapply(1:1000, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
            standard_deviation_values_randomized[[iteration]][list_item]
          })) *1000
        
        sum_vector <- average_vector + standard_deviation_vector #determine best simulation for given column
        
        return(as.integer(which(sum_vector == max(sum_vector))))
      }
      
    }))
    
    for(column in 1:ncol(sampled_data)){
      sampled_data[,column] <- all_sampled_data[[best_simulations[column]]][,column]
    }
    
    worst_sampled_data <- sampled_data
    for(column in 1:ncol(worst_sampled_data)){
      worst_sampled_data[,column] <- all_sampled_data[[worst_simulations[column]]][,column]
    }
    
    report <- as.data.frame(matrix(0,nrow = 6, ncol=length(pasted_data_1)))
    colnames(report) <- pasted_data_header
    if(average_setting == "Mean"){
    row.names(report) <- c("Mean of original data", "Mean of best subset of data", "Mean of worst subset of data",
                           "Standard deviation of original data", "Standard deviation of best subset of data", "Standard deviation of worst subset of data")
    report["Mean of original data",] <- average_values_all
    report["Mean of best subset of data",] <- colMeans(sampled_data)
    report["Mean of worst subset of data",] <- colMeans(worst_sampled_data)
    report["Standard deviation of original data",] <- standard_deviation_values_all
    report["Standard deviation of best subset of data",] <- sapply(1:length(pasted_data_1), function(column){sd(sampled_data[,column])})
    report["Standard deviation of worst subset of data",] <- sapply(1:length(pasted_data_1), function(column){sd(worst_sampled_data[,column])})
    } else { #Median
      row.names(report) <- c("Median of original data", "Median of best subset of data", "Median of worst subset of data",
                             "Standard deviation of original data", "Standard deviation of best subset of data", "Standard deviation of worst subset of data")
      report["Median of original data",] <- average_values_all
      report["Median of best subset of data",] <- colMedians(as.matrix(sampled_data))
      report["Median of worst subset of data",] <- colMedians(as.matrix(worst_sampled_data))
      report["Standard deviation of original data",] <- standard_deviation_values_all
      report["Standard deviation of best subset of data",] <- sapply(1:length(pasted_data_1), function(column){sd(sampled_data[,column])})
      report["Standard deviation of worst subset of data",] <- sapply(1:length(pasted_data_1), function(column){sd(worst_sampled_data[,column])})
    }
    
    
    
    nudge_x <- -0.17
    spread_x <- 0.17
    plot_data <- as.data.frame(matrix(0,nrow=3*length(pasted_data_1), ncol=5))
    colnames(plot_data) <- c("Data_type", "Graph_color", "x_val","y_val","error_bars")
    plot_data$`Data_type` <- c(rep("Original",length(pasted_data_1)),rep("Best",length(pasted_data_1)),rep("Worst",length(pasted_data_1)))
    plot_data$`Graph_color` <- c(rep("Black",length(pasted_data_1)),rep("Blue",length(pasted_data_1)),rep("Red",length(pasted_data_1)))
    plot_data$x_val <- c(1:length(pasted_data_1)+nudge_x,1:length(pasted_data_1)+nudge_x+spread_x,1:length(pasted_data_1)+nudge_x+spread_x*2)
    if(average_setting == "Mean"){
    plot_data$y_val <- as.numeric(c(report["Mean of original data",], report["Mean of best subset of data",], report["Mean of worst subset of data",]))
    } else { #Median
      plot_data$y_val <- as.numeric(c(report["Median of original data",], report["Median of best subset of data",], report["Median of worst subset of data",]))
      }
    plot_data$`error_bars` <- as.numeric(c(report["Standard deviation of original data",],report["Standard deviation of best subset of data",],report["Standard deviation of worst subset of data",]))
    
    plot <- ggplot(plot_data) + 
      geom_errorbar(aes(x=as.numeric(plot_data$x_val), ymin=(as.numeric(plot_data$y_val)-as.numeric(plot_data$error_bars)), ymax=(as.numeric(plot_data$y_val)+as.numeric(plot_data$error_bars))), color=plot_data$`Graph_color`, width=.1) + 
      geom_point(aes(x=as.numeric(plot_data$x_val), y=as.numeric(plot_data$y_val)),size=3, shape=21, fill=plot_data$`Graph_color`) +
      theme(plot.background = element_blank()
            ,panel.grid.major = element_blank()
            ,panel.grid.minor = element_blank()
            ,panel.border = element_blank()
            ,panel.background = element_blank()
            #,legend.position="right"
            ,axis.line.x = element_blank()
            ,axis.title.x= element_text(size=rel(1.7), color = "#000000")
            ,axis.text.y= element_text(size=rel(1.7))
            ,axis.title.y= element_text(size=rel(1.7))
            ,axis.line.y = element_blank()
            ,axis.text.x= element_text(size=rel(1.7), color ="#000000")
            ,axis.ticks.x= element_blank()
            ,axis.ticks.y= element_line()
            ,plot.title = element_text(hjust=0.5, size=rel(2))
      ) +
      scale_x_discrete(limits=1:ncol(sampled_data)) +
      labs(x = "Sample", y = "Data value", title = "") +
      expand_limits(x = 0.33, y = 0) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0.33)
    
    
    
    plot_data_jitter <- as.data.frame(matrix(0,nrow=length( c(unlist(pasted_data_1), unlist(sampled_data), unlist(worst_sampled_data)) ), ncol=4))
    colnames(plot_data_jitter) <- c("Data_type", "Graph_color", "x_val","y_val")
    plot_data_jitter$`Data_type` <- c(rep("Original",length(unlist(pasted_data_1))),rep("Best",length(unlist(sampled_data))),rep("Worst",length(unlist(worst_sampled_data))))
    plot_data_jitter$`Data_type` <- factor(plot_data_jitter$`Data_type`, levels= c("Original","Best","Worst"))
    plot_data_jitter$`Graph_color` <- c(rep("Black",length(unlist(pasted_data_1))),rep("Blue",length(unlist(sampled_data))),rep("Red",length(unlist(worst_sampled_data))))
    plot_data_jitter$y_val <- c(unlist(pasted_data_1), unlist(sampled_data), unlist(worst_sampled_data))
    plot_data_jitter$x_val <- c( 
      unlist(sapply(1:length(pasted_data_1), function(sample_num){
        sample_num + nudge_x + sample(1:100,length(pasted_data_1[[sample_num]]),replace = TRUE)/800 }))
      ,unlist(sapply(1:ncol(sampled_data), function(sample_num){
        sample_num + nudge_x + spread_x + sample(1:100,length(sampled_data[,sample_num]),replace = TRUE)/800 }))
      ,unlist(sapply(1:ncol(worst_sampled_data), function(sample_num){
        sample_num + nudge_x + spread_x*2 + sample(1:100,length(worst_sampled_data[,sample_num]),replace = TRUE)/800 }))
    )
    
    plot_jitter <- ggplot(plot_data_jitter, aes(x= x_val, y=y_val)) + 
      geom_point(data = plot_data_jitter,  color = plot_data_jitter$Graph_color, size = rel(1), alpha = 0.5)+ 
      theme(plot.background = element_blank()
            ,panel.grid.major = element_blank()
            ,panel.grid.minor = element_blank()
            ,panel.border = element_blank()
            ,panel.background = element_blank()
            #,legend.position="right"
            ,axis.line.x = element_blank()
            ,axis.title.x= element_text(size=rel(1.7), color = "#000000")
            ,axis.text.y= element_text(size=rel(1.7))
            ,axis.title.y= element_text(size=rel(1.7))
            ,axis.line.y = element_blank()
            ,axis.text.x= element_text(size=rel(1.7), color ="#000000")
            ,axis.ticks.x= element_blank()
            ,axis.ticks.y= element_line()
            ,plot.title = element_text(hjust=0.5, size=rel(2))
      ) +
      scale_x_discrete(limits=1:ncol(sampled_data)) +
      labs(x = "Sample", y = "Data value", title = "")+
      expand_limits(x = 0.33, y = 0) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0.33) +
      facet_grid(. ~Data_type)
    
    return(list(
      subset_table = sampled_data
      ,worst_subset_table = worst_sampled_data
      ,plot = plot
      ,plot_jitter = plot_jitter
      ,warning = as.character(warning)
      ,best_subset = sampled_data
      ,worst_subset = worst_sampled_data
      ,done = 'done'
      ))
      
  })
 
  output$plot <- renderPlot({statistical_shiny()$plot})
  output$ui_plot <- renderUI({plotOutput("plot", width = input$plot_width, height = input$plot_height)})
  output$plot_jitter <- renderPlot({statistical_shiny()$plot_jitter})
  output$ui_plot2 <- renderUI({plotOutput("plot_jitter", width = input$plot_width, height = input$plot_height)})
  output$warning <- renderText({statistical_shiny()$warning})
  output$download_best_subset <- downloadHandler(
    filename = "Best_subsets.csv",
    content = function(file) {
      write.table(statistical_shiny()$best_subset, file, row.names = FALSE, col.names = TRUE, sep = ",")
    },
    contentType = "text/csv"
  )
  output$download_worst_subset <- downloadHandler(
    filename = "Worst_subsets.csv",
    content = function(file) {
      write.table(statistical_shiny()$worst_subset, file, row.names = FALSE, col.names = TRUE, sep = ",")
    },
    contentType = "text/csv"
  )
  output$executed = renderText({statistical_shiny()$done})
  outputOptions(output, "executed", suspendWhenHidden=FALSE)
  
}