library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)
library(viridis)
library(ggiraph)
library(Biostrings)
library(ggridges)



### function for radiobuttons
radioButtons_withHTML <- function (inputId, label, choices, selected = NULL, inline = FALSE, 
                                   width = NULL) 
{
  choices <- shiny:::choicesWithNames(choices)
  selected <- if (is.null(selected)) 
    choices[[1]]
  else {
    shiny:::validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  options <- generateOptions_withHTML(inputId, choices, selected, inline, 
                                      type = "radio")
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  tags$div(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
    shiny:::controlLabel(inputId, label), options)
}

generateOptions_withHTML <- function (inputId, choices, selected, inline, type = "checkbox") 
{
  options <- mapply(choices, names(choices), FUN = function(value, 
                                                            name) {
    inputTag <- tags$input(type = type, name = inputId, value = value)
    if (value %in% selected) 
      inputTag$attribs$checked <- "checked"
    if (inline) {
      tags$label(class = paste0(type, "-inline"), inputTag, 
                 tags$span(HTML(name)))
    }
    else {
      tags$div(class = type, tags$label(inputTag, tags$span(HTML(name))))
    }
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(class = "shiny-options-group", options)
}
###
# define input for radiobutton
choices = c('<img src="https://raw.githubusercontent.com/felixgrunberger/shiny-nano/master/colors/viridis_pal.png">' = '1',
            '<img src="https://raw.githubusercontent.com/felixgrunberger/shiny-nano/master/colors/magma_pal.png">'= '2',
            '<img src="https://raw.githubusercontent.com/felixgrunberger/shiny-nano/master/colors/plasma_pal.png">' = '3',
            '<img src="https://raw.githubusercontent.com/felixgrunberger/shiny-nano/master/colors/inferno_pal.png">' = '4',
            '<img src="https://raw.githubusercontent.com/felixgrunberger/shiny-nano/master/colors/cividis_pal.png">'= '5')


###


####################################
## ui.R ##
ui <- dashboardPage(
  skin = "black",
  # Application title
  dashboardHeader(title = "shinyNano"),
  
  
  dashboardSidebar(
    tags$div(tags$style(HTML("
                             @keyframes bounce {
                             0%, 20%, 60%, 100% {
                             -webkit-transform: translateY(0);
                             transform: translateY(0);
                             }
                             
                             40% {
                             -webkit-transform: translateY(-20px);
                             transform: translateY(-20px);
                             }
                             
                             80% {
                             -webkit-transform: translateY(-10px);
                             transform: translateY(-10px);
                             }
                             }
                             
                             
                             #button:hover {
                             text-shadow: 0.5px 0.5px 0.5px;
                             color: #22A884;
                             animation: bounce 1s;
                             }
                             
                             
                             .btn-primary {
                             background-color: #252C34;
                             color: #fff;
                             width: 92%;
                             font-size:100%;
                             font-family: Avenir;
                             border-color: white;
                             font-color: white;
                             }
                             
                             .btn-primary:hover {
                             width: 92%; 
                             font-color: #22A884;
                             color: #22A884;
                             font-size: 100%; 
                             font-family: Avenir; 
                             background-color:  #152227;
                             border-color: #22A884;
                             
                             
                             }
                             
                             
                             "))),
    tags$style(".left-side, .main-sidebar {padding-top: 65px; font-family: Avenir; font-size: 120%; max-height: 1000px; font-weight: bold;}"),
    
    sidebarMenu(
      menuItem("Analysis", tabName = "albacoreanalysis", icon = icon("signal",lib = "font-awesome")),
      menuItem("Example Data", tabName = "exampledata", icon = icon("download")),
      br(),
      ## color style of plots
      radioButtons_withHTML("select_color", h5('Choose color style'),choices = choices, inline = F),
      
      
      
      
      
      # ----------------------------------------------------------------------------------------------------------------------------- #
      
      
      # ----------------------------------------------------------------------------------------------------------------------------- #
      # ----------------------------------------------------------------------------------------------------------------------------- #
      # personal information
      
      a(icon("twitter"), target = "blank", href="https://twitter.com/messages/compose?recipient_id=713459142653059072",
        id = "button",
        title = "write me on Twitter",
        style = "
        overflow: hidden;
        position: fixed;
        left:20px;
        bottom: 20px;"),
      
      a(icon("envelope-o"), target = "blank", href="mailto:felix.gruenberger@ur.de?subject=GenomeBrowser",
        id = "button",
        title = "write me an Email",
        style = "
        overflow: hidden;
        position: fixed;
        left: 60px;
        bottom: 20px;")
      #, 
      #
      #a(icon("institution"), target = "blank", href="http://www.uni-regensburg.de/biologie-vorklinische-medizin/mikrobiologie/",
      #  id = "button",
      #  title = "check out our institutes homepage",
      #  style = "
      #  overflow: hidden;
      #  position: fixed;
      #  left:100px;
      #  bottom: 20px;")
      )
      ),
  ## ----------------------------------------------------------------------------------------------------------------------------- #
  ## ----------------------------------------------------------------------------------------------------------------------------- #
  ## DASHBOARDBODY 
  dashboardBody(
    
    includeCSS("/Users/felixgrunberger/Documents/R/shiny_nano/style/style.css"),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$head(tags$style(HTML(".small-box {height: 101px}"))),
    tags$style(HTML("
                    .tabbable > .nav > li > a {background-color: white;  color: #152227; font-family: Avenir; font-size: 100%; font-weight: bold;}
                    .tabbable > .nav > li[class=active]    > a {background-color: #152227; color: white; }
                    .small-box {color:red; background:red;}      "
    )),
    tabItems(
      
      ##### albacoreanalysis CONTENT
      tabItem(tabName = "albacoreanalysis",
              fluidRow(
                
                
                ## INPUT DATA
                column(width = 12,
                       box(
                         title = "Input Data", status = "primary", solidHeader = T,
                         collapsible = F,width = 8,height = 470,
                         fileInput("file1", "Upload albacore summary file",
                                   accept = c(
                                     "txt")
                         ),
                         fileInput("fasta_file", "Upload fasta file (for sequencing depth calculation)",
                                   accept = c(".fa", ".fasta")
                         ),
                         fileInput("bedgraph_file", "Upload bedtools coverage file (-d option, for coverage plot)",
                                   accept = c(
                                     ".bed", "txt")
                         ),
                         hr(),
                         br()
                         #actionButton("example_action",width = "100%","Show me an example!"),
                         #textOutput(outputId = "content")
                       ),
                       
                       ### STATS BOXES (DYNAMIC)
                       valueBoxOutput("yieldBox"),
                       
                       valueBoxOutput("totalyieldBox"),
                       
                       valueBoxOutput("totalqualityBox"),
                       valueBoxOutput("totalseqdepth")
                ),
                
                ################# 
                column(width = 12,
                       ## PLOTLY HEATMAP
                       box(width = 6,
                           title = HTML("<span class='fa fa-braille'></span>  Channel throughput"), status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           ggiraphOutput("channel")
                       ),
                       ## YIELD PER HOUR
                       box(width = 6,
                           title = HTML("<span class='fa fa-align-left'></span>  Yield per hour"), status = "primary", solidHeader = T,
                           collapsible = T,
                           ggiraphOutput("yield_per_hour")
                       )
                ),
                
                
                ## SEQUENCE LENGTH & SEQUENCE LENGTH OVER TIME
                column(width = 12,
                       
                       box(
                         title = HTML("<span class='fa fa-align-left'></span>  Sequence length"), status = "primary", solidHeader = T,
                         collapsible = T,
                         radioButtons("radio", h5("transformation"),
                                      choices = list("no" = 1, "log10" = 2),selected = 2),
                         ggiraphOutput("sequence_length")
                       ),
                       box(
                         title = HTML("<span class='fa fa-align-left'></span>  Sequence length over time"), status = "primary", solidHeader = T,
                         collapsible = T,
                         radioButtons("radio2", h5("transformation"),
                                      choices = list("no" = 1, "log10" = 2),selected = 2),
                         plotOutput("sequence_length_over_time")
                       )
                ),
                ## QUALITY SCORE & QUALITY SCORE OVER TIME
                column(width = 12,
                       box(
                         title = HTML("<span class='fa fa-align-left'></span>  Quality score"), status = "primary", solidHeader = T,
                         collapsible = T,
                         ggiraphOutput("quality_score")
                       ),
                       box(
                         title = HTML("<span class='fa fa-align-left'></span>  Quality score over time"), status = "primary", solidHeader = T,
                         collapsible = T,
                         plotOutput("quality_score_over_time")
                       )
                ),
                ## FULL WIDTH GENOME COVERAGE
                column(width = 12,
                       box(width = 6,
                           title = HTML("<span class='fa fa-align-left'></span>  Genome coverage"), status = "primary", solidHeader = T,
                           collapsible = T,
                           textOutput(outputId = "coverage_text"),
                           ggiraphOutput("genome_coverage")
                       ),
                       box(
                         title = HTML("<span class='fa fa-align-left'></span>  Stats"), status = "primary", solidHeader = T,
                         collapsible = T,
                         dataTableOutput("statistics_table")
                       )
                       
                )
                
              )
      ),
      
      
      ##### exampledata CONTENT
      tabItem(tabName = "exampledata",
              column(width = 12,
                     box(width = 12,
                         title = HTML("<span class='fa fa-align-left'></span>  Example data"), status = "primary", solidHeader = T,
                         collapsible = F,
                         HTML("Example data sets can be found in"),a(target="_blank", href="https://github.com/felixgrunberger/shiny-nano/tree/master/example-data", "my github repository."),
                         "I´ll soon upload a gif to show you the different features."
                     
                         
                     )
              )
      )
    )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
####### --------- #######
# example data input
contentInput <- reactive({ 
  
  if(input$example_action == 0) return()
  isolate({
    writeLines(paste("https://raw.githubusercontent.com/felixgrunberger/shiny-nano/master/example-data/sequencing_summary_subset.txt", collapse = "\n"))
  })
})



###### ---------- #######
  
  ## text message to upload coverage file
  output$coverage_text <- renderText(
    if(is.null(input$bedgraph_file) && is.null(input$fasta_file))
      ("please upload coverage & fasta file")
  )


  ## limit for upload
  options(shiny.maxRequestSize=800000*1024^2) 
  #
  #
  #
  #
  colorInput <- reactive({
    switch(input$select_color,
           "1" = "viridis",
           "2" = "magma",
           "3" = "plasma",
           "4" = "inferno",
           "5" = "cividis")
  })
  #
  albacore_data <- reactive({
      inFile <- input$file1
      fread(input = inFile$datapath) %>%
        as.data.table()
  })
  
  


  #
  #
  #
  ######
  progress <- shiny::Progress$new()
  progress$set(message = "uplode files")
  ######
  #
  #
  #
  ################## 
  ##  ALBACORE HEATMAP
  output$channel <- renderggiraph({
   
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    #####
    progress$set(message = "Making plot", value = 0)
    #####
    platelayout <- data.frame (rown = rep (1:16, 32), coln = rep (1:32, each = 16), channel = 1:512)
    channels <- albacore_data() %>%
      group_by(channel) %>%
      summarise(n = n()) %>%
      left_join(platelayout, by = "channel") %>%
      ggplot(aes(y = factor(rown),x = factor(coln))) +
      geom_tile_interactive(color  = "white", aes(fill = log10(n), tooltip = sprintf("count: %s\nchannel: %s", n,coln*rown)),size = 0.25)  +
      scale_fill_viridis(begin = 0.3, end = 0.6,option = colorInput(), guide = guide_colorbar())+
      ggtitle("") +
      theme(legend.position="bottom") +
      theme(legend.title=element_blank()) +
      labs(x=NULL, y = NULL) +
      guides(fill = F) +
      theme_void()
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 1 of 10"))
    #
    tooltip_css <- "background-color:white;font-family:Avenir;padding:10px;border-radius:10px 20px 10px 20px;"
    ggiraph(code = print(channels),zoom_max = 5,tooltip_extra_css = tooltip_css,tooltip_opacity = .75)
    
    
  })
  
  
  #### SUMMED READS
  output$yieldBox <- renderValueBox({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(valueBox(
        h5(HTML("<i>please upload summary file</i>")), "Reads analysed", icon = icon("stats", lib = "glyphicon"),
        color = "red"
      ))
    length <- albacore_data()
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 2 of 10"))
    #
    valueBox(
      length(length$filename), "Reads analysed", icon = icon("stats", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  
  
  #### TOTAL YIELD
  output$totalyieldBox <- renderValueBox({
    inFile <- input$file1
    if (is.null(inFile))
      return(valueBox(
        h5(HTML("<i>please upload summary file</i>")), "Total yield", icon = icon("saved", lib = "glyphicon"),
        color = "red"
      ))
    yield<- albacore_data()
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 3 of 10"))
    #
    valueBox(
      paste(round(sum(yield$sequence_length_template)/1000000000, digits = 2)," Gb", sep = ""), "Total yield", icon = icon("saved", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  
  #### Average Quality
  output$totalqualityBox <- renderValueBox({
    inFile <- input$file1
    if (is.null(inFile))
      return(valueBox(
        h5(HTML("<i>please upload summary file</i>")), "Average Quality", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "red"
      ))
    quality <- albacore_data()
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 4 of 10"))
    #
    valueBox(
      round(mean(quality$mean_qscore_template), digits = 2), "Average Quality", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  
  
  #### TOTAL SEQUENCING DEPTH
  output$totalseqdepth <- renderValueBox({
    inFile <- input$file1
    inFile_fasta <- input$fasta_file
    if (is.null(inFile))
      return(valueBox(
        h5(HTML("<i>please upload summary & fasta file</i>")), "Sequencing depth", icon = icon("align-justify", lib = "glyphicon"),
        color = "red"
      ))
    
    if (is.null(inFile_fasta))
      return(valueBox(
        h5(HTML("<i>please upload fasta file</i>")), "Sequencing depth", icon = icon("align-justify", lib = "glyphicon"),
        color = "red"
      ))
    
    if (!is.null(inFile_fasta$datapath))
      fasta = Biostrings::readDNAStringSet(filepath = inFile_fasta$datapath)
    names(fasta) <- "genome"
    yield<- albacore_data()
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 5 of 10"))
    #
    valueBox(
      round(sum(yield$sequence_length_template)/length(fasta$genome), digits = 2), "Sequencing depth", icon = icon("align-justify", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  #
  #
  ##### YIELD PER HOUR
  output$yield_per_hour <- renderggiraph({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    yield_h <- albacore_data() %>%
      mutate(timeindex = round((start_time + 3600)/3600)) %>%
      group_by(timeindex) %>%
      summarise(yield = sum(sequence_length_template)/1000000) %>%
      ggplot(aes(x=timeindex, y = yield, fill = yield)) +
      geom_bar_interactive(stat="identity", width = .6, 
                           aes(tooltip = sprintf("%s hours since start\nYield: %s M bases", timeindex, round(yield, digits = 2)))) +
      scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
      theme_light() +
      ggtitle("") +
      xlab("Hours since start") +
      ylab("Yield [Mb]") +
      guides(fill = F)
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 6 of 10"))
    #
    tooltip_css <- "background-color:white;font-family:Avenir;padding:10px;border-radius:10px 20px 10px 20px;"
    #
    ggiraph(code = print(yield_h),tooltip_extra_css = tooltip_css,tooltip_opacity = .75, zoom_max = 5)
    
  })
  #
  ##### QUALITY SCORE
  output$quality_score <- renderggiraph({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    quality_h <- albacore_data() %>%
      ggplot(aes(x = mean_qscore_template)) +
      geom_histogram_interactive(bins = 50,col = "white", 
                                 aes(fill = ..count..,tooltip = sprintf("count: %s\nQuality score: %s",..count.., round(..x.., digits = 2)))) +
      scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
      theme_light() +
      ggtitle("") +
      xlab("Quality score") +
      ylab("Read count") +
      guides(fill = F)
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 7 of 10"))
    #
    #
    tooltip_css <- "background-color:white;font-family:Avenir;padding:10px;border-radius:10px 20px 10px 20px;"
    #
    ggiraph(code = print(quality_h), tooltip_extra_css = tooltip_css,tooltip_opacity = .75, zoom_max = 5)
    
  })
  #
  ##### SEQUENCE LENGTH
  output$sequence_length <- renderggiraph({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    choice <- input$radio
    if (choice == "1"){
      length_h <- albacore_data() %>%
        ggplot(aes(x = sequence_length_template)) +
        geom_histogram_interactive(bins = 50,col = "white", 
                                   aes(fill = ..count..,tooltip = sprintf("count: %s\nSequence Length: %s",..count.., round(..x.., digits = 2)))) +
        scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
        theme_light() +
        ggtitle("") +
        xlab("Sequence Length") +
        ylab("Read count") +
        guides(fill = F) +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }
    if (choice == "2"){
      length_h <- albacore_data() %>%
        ggplot(aes(x = sequence_length_template)) +
        geom_histogram_interactive(bins = 50,col = "white", 
                                   aes(fill = ..count..,tooltip = sprintf("count: %s\nSequence Length: %s",..count.., round(exp(..x..), digits = 2)))) +
        scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
        theme_light() +
        ggtitle("") +
        scale_x_log10() +
        xlab("Sequence Length") +
        ylab("Read count") +
        guides(fill = F) +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 8 of 10"))
    #
    tooltip_css <- "background-color:white;font-family:Avenir;padding:10px;border-radius:10px 20px 10px 20px;"
    #
    ggiraph(code = print(length_h), tooltip_extra_css = tooltip_css,tooltip_opacity = .75, zoom_max = 5)
    
    
  })
  #
  ##### SEQUENCE LENGTH OVER TIME
  output$sequence_length_over_time <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    choice2 <- input$radio2
    if (choice2 == "1"){
      length_over <- albacore_data() %>%
        mutate(timeindex = round((start_time + 3600)/3600)) %>%
        group_by(timeindex) %>%
        ggplot(aes(x = sequence_length_template, y = factor(timeindex), fill = ..x..)) +
        geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, alpha = 0.5, col = "white") +
        scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
        theme_light() +
        ylab("Hours since start") +
        xlab("Read length") +
        guides(fill = F)
    }
    
    if (choice2 == "2"){
      length_over <- albacore_data() %>%
        mutate(timeindex = round((start_time + 3600)/3600)) %>%
        group_by(timeindex) %>%
        ggplot(aes(x = sequence_length_template, y = factor(timeindex), fill = ..x..)) +
        geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, alpha = 0.5, col = "white") +
        scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
        theme_light() +
        ylab("Hours since start") +
        xlab("Read length") +
        guides(fill = F) +
        scale_x_log10()
    }
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 9 of 10"))
    #
    print(length_over)
    
  })
  #
  #
  ##### QUALITY SCORE OVER TIME
  output$quality_score_over_time <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    quality_over <- albacore_data() %>%
      mutate(timeindex = round((start_time + 3600)/3600)) %>%
      group_by(timeindex) %>%
      ggplot(aes(x = mean_qscore_template, y = factor(timeindex), fill = ..x..)) +
      geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, alpha = 0.5, col = "white") +
      scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
      theme_light() +
      ylab("Hours since start") +
      xlab("Read quality") +
      guides(fill = F)
    # Increment the progress bar, and update the detail text.
    progress$inc(1/10, detail = paste("Making plot 10 of 10"))
    #
    print(quality_over)
    
    on.exit(progress$close())
  })
  #
  #
  #
  #
  ##### QUALITY VS LENGTH
  output$quality_vs_length <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    quality_h <- albacore_data() %>%
      ggplot(aes(x = mean_qscore_template, text = sprintf("count: %s\nQuality score: %s",..count.., round(..x.., digits = 2)))) +
      geom_histogram(bins = 50,col = "white", aes(fill = ..count..)) +
      scale_fill_viridis(begin = 0.2, end = 0.6, option = colorInput()) +
      theme_light() +
      ggtitle("") +
      xlab("Quality score") +
      ylab("Read count") +
      guides(fill = F)
    quality_h
  })
  #
  ##### GENOME COVERAGE
  output$genome_coverage <- renderggiraph({
    inFile_fasta <- input$fasta_file
    coverageFile <- input$bedgraph_file
    if (is.null(coverageFile))
      return(NULL)
    if (is.null(inFile_fasta))
      return(NULL)
    if (!is.null(inFile_fasta))
      fasta = Biostrings::readDNAStringSet(filepath = inFile_fasta$datapath)
    names(fasta) <- "genome"
    #####
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating genome coverage", value = 0)
    #####
    bed <- fread(coverageFile$datapath) %>%
      mutate(chr = V1, 
             position = V2,
             value = V3)
    
    values <- colMeans(matrix(bed$value, nrow=100))
    bed <- NULL
    positions <- 1:length(values)
    
    help_matrix <- matrix(ncol = 2, nrow = length(positions)) %>%
      as.data.table() %>%
      mutate(position = positions,
             value = ifelse(values != 0, values, NA)) %>%
      dplyr::select(position, value)
    values <- NULL
    positions <- NULL
    
    tooltip_css <- "background-color:white;font-family:Avenir;padding:10px;border-radius:10px 20px 10px 20px;"
    progress$inc(10/10, detail = paste("Finishing"))
    #
    on.exit(progress$close())
    ggiraph(code = print(ggplot(data = help_matrix, aes(x = position, y= value + 1)) +
                           geom_point_interactive(alpha = 0.1, size = 5, shape = 21,
                                                  aes(fill = log10(value + 1), col = log10(value+1),
                                                      tooltip = sprintf("position: %s\ncoverage: %s", position*100, value))) +
                           geom_ribbon(aes(ymin = 0, ymax = value + 1),fill = viridis_pal()(10)[1], alpha = 0.1) +
                           scale_x_continuous(labels = function(x)x*100, limits = c(0,length(fasta$genome)/100)) +
                           scale_fill_viridis(begin = 0.2, end = 0.6,option = colorInput()) +
                           
                           scale_color_viridis(begin = 0.2, end = 0.6,option = colorInput()) +
                           xlab("Reference sequence position") +
                           ylab("Estimated coverage") +
                           guides(fill = F, col = F) +
                           theme(text = element_text(family="Serif")) +
                           theme_light() +
                           ggtitle("") +
                           stat_smooth(col = viridis_pal(option = colorInput())(10)[10],lwd = 1, span = 0.4)), zoom_max = 5,tooltip_extra_css = tooltip_css,tooltip_opacity = .75)
    
    
  })
  #
  #
  ## STATISTICS TABLE
  output$statistics_table <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    #
    table <- albacore_data() %>%
      dplyr::arrange(desc(sequence_length_template)) %>%
      mutate(readlength = sequence_length_template,
             basecall_quality = mean_qscore_template) %>%
      dplyr::select(basecall_quality,readlength)
    datatable(table,options = list(pageLength = 5))
    
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

