######################################
########  BIOSTATS  LEVEL 1    #######
######################################

# This version of the biostats website allows:
# data upload
# inspection of summary statistics
# inspection of a histogram for a continuous variable across one or two categorical variables
# making scatter and box plots
# running parametric and non-parametric two-sample group tests 
# running parametric and non-parametric correlation tests

# This website version also includes instructions for students about how to use R on their own computers

# Before you start using this app I suggest you watch the Shiny tutorial https://shiny.rstudio.com/tutorial/ and read the first couple of chapters of Mastering Shiny https://mastering-shiny.org/index.html  

# The annotations below will not cover basic Shiny functions and commands. See notes for references to other websites for additional information



#################################
############ LAYOUT #############
#################################

library(shiny) 


ui <-fluidPage(
  
  # Website title
  titlePanel("Inferential Statistics for Introductory Biology Laboratory, Level 1"),
  
  # Greeting
  fluidRow(mainPanel(p("Welcome to our statistics website! This website allows you to upload any .csv file to calculate summary statistics and test the statistical significance of correlations and group differences."),
                     br(),
                     p("This web page uses R statistical programming language, which is the main statistical software used by scientists from natural sciences around the world. It is an free open source software that you can download on your computer. However, it does take a little time to get used to it, so instead of asking you to use R from the get go, we will SHOW you what R does for each of the statistical steps that you will be taking"),
                     br(),
                     code("R code will be displayed as red letters on a grey background. You can copy and paste this into your own R script if you want to run this on R yourself!"),
                     br(),
                     br(),
                     br())),
  
  #######  CSV FILE UPLOAD LAYOUT AND INPUTS ######## 
  
  # The file upload code is taken with minor modifications from the https://shiny.rstudio.com/gallery/file-upload.html widget. See the website for explanations
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      tags$hr(),
      
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      p("If you want to use the R command-line interface to work on your data, you can import a .csv file in R using this command, where d is a new short \"name\" we give your dataset, and \"filename\"is the name of the file you are trying to import"),
      code("d <- read.csv(\"filename.csv\",header=TRUE)")
      
      
    ),
    
    mainPanel(
      
      tableOutput("contents")
      
    )
    
  ),
  
  
  ####### STATISTICAL TOOLS ########
  
  fluidRow(
    
    # These conditional panels contain additional information about the statistical tools. Feel free to insert your own explanation about how these tools work. In more advanced version of this webpage, this is where I show students the R code they can use to carry out these tests on their own computers.
    sidebarPanel(
     
       # Summary statistics information and optional R-code
       conditionalPanel(condition = "input.tabs=='Data summary'",
                       h4("Summary statistics"),
                       p("In this tab you can calculate summary statistics for any continuous variable in your dataset (average, sample size, minimum and maximum values, and variance), inspect a histogram of a continuous variable, and inspect the distribution of a continuous variable across groups of a categorical variable."),
                       br(),
                       strong("If you want to do this in R on your own computer using command-line interface, use the instructions below:"),
                       br(),
                       p("To calculate the average of any continuous variable, use the following code, where \"d\" is the short name you gave to your imported dataset, and \"contvar\" is the name of a continuous variable from your dataset."),
                       code("mean(d$contvar, na.rm=TRUE)"),
                       br(),
                       br(),
                       p("To calculate the sample size of any continuous variable, use the following code, where \"d\" is the name of your dataset, and \"contvar\" is the name of your continuous variable from your dataset."),
                       code("length(d$contvar[!is.na(d$contvar)])"),
                       br(),
                       br(),
                       p("To calculate the minimum and maximum values of any continuous variable, use the following code, where \"d\" is the name of your dataset, and \"contvar\" is the name of your continuous variable from your dataset."),
                       code("range(d$contvar, na.rm=TRUE)"),
                       br(),
                       br(),
                       p("To calculate the variance of any continuous variable, use the following code, where \"d\" is the name of your dataset, and \"contvar\" is the name of your continuous variable from your dataset."),
                       code("var(d$contvar, na.rm=TRUE)"),
                       br(),
                       br(),
                       p("You can calculate many of these summary statistics for the whole dataset at once using this code, where \"d\" is the name of your dataset."),
                       code("summary(d)"),
                       br(),
                       br(),
                       p("To plot a histogram of a continuous variable, use the following code, where \"d\" is the name of your dataset, and \"contvar\" is the name of your continuous variable from your dataset."),
                       code("hist(d$contvar)"),
                       br(),
                       br(),
                       p("To plot a histogram of a observations of continuous variable from a particular group, use the following code, where \"d\" is the name of your dataset, and \"contvar\" is the name of your continuous variable from your dataset, \"groupvar\" is the name of your categorical variable and \"groupvalue\" is the name of the group."),
                       code("hist(d$contvar[d$groupvar==\"groupvalue\"])"),
                       br(),
                       br()
                      ),
      
      # Information about plots and optional R-code
      conditionalPanel(condition = "input.tabs=='Plots'",
                       h4("Plotting data"),
                       p("Use this tab to plot a correlation plot (scatter plot) between two continuous variables and a boxplot that shows the values of a continuous variable across two (or more) groups."),
                       br(),
                       strong("If you want to do this in R on your own computer using command-line interface, use the instructions below:"),
                       p("To plot the relationship between two continuous variables, use the following code, where \"d\" is the name of your dataset, \"contvar1\" is the name of one of your continuous variables, and \"contvar2\" is the name of the other continuous variable."),
                       code("plot(contvar1~contvar2, data=d)"),
                       br(),
                       br(),
                       p("To make a boxplot that shows how a continuous variable is distributed across two or more groups, use the following code, where \"d\" is the name of your dataset, \"contvar\" is the name of your continuous variable, and \"catvar\" is the name of the categorical variable."),
                       code("boxplot(contvar~catvar, data=d)"),
                       br(),
                       br()
                      ),
      
      # Information about group tests and optional R-code
      conditionalPanel(condition = "input.tabs=='Group tests'",
                       h4("Testing differences between groups"),
                       p("Use this tab to calculate the statistical significance of the difference in the average of a continuous variable across two groups. Depending on whether your data are normally distributed, you can use either a parametric test (T-test) or a non-parametric test (Mann-Whitney U test)."),
                       br(),
                       strong("If you want to do this in R on your own computer using command-line interface, use the instructions below:"),
                       p("To use a T-test to calculate the statistical significance of group differences, use the following code. In this code \"t.test\" is the R command for running the T-test, \"d\" is the name of your dataset, \"contvar\" is the name of your continuous variable, and \"catvar\" is the name of the categorical variable. This command will not work if you have more than two groups in your categorical variable."),
                       code("t.test(contvar~catvar, data=d)"),
                       br(),
                       br(),
                       p("To use a Mann-Whitney U test to calculate the statistical significance of group differences, use the following code. In this code \"wilcox.test\" is the R command for running the Mann-Whitney U test, \"d\" is the name of your dataset, \"contvar\" is the name of your continuous variable, and \"catvar\" is the name of the categorical variable."),
                       code("wilcox.test(contvar~catvar, data=d)"),
                       br(),
                       br()
                       ),
      
      # Information about correlation tests and optional R-code
      conditionalPanel(condition = "input.tabs=='Correlation tests'",
                       h4("Testing relationship between continuous variables"),
                       p("Use this tab to calculate the statistical significance of a relationship between two continuous variables. Depending on whether your variables are normally distributed, you can use either a parametric test (Pearson Correlation Coefficient) or a non-parametric test (Spearman Rank Correlation Coefficient)."),
                       strong("If you want to do this in R on your own computer using command-line interface, use the instructions below:"),
                       p("To use Pearson Correlation Coefficient to calculate the statistical significance of a correlation between two continuous variables, use the following code. In this code \"cor.test\" is the R command for running the correlation test, \"d\" is the name of your dataset, \"contvar1\" is the name of one of your continuous variables, \"contvar2\" is the name of the other continuous variable, and \"method=\"pearson\"\" means you want to use the Pearson Correlation Coefficient. This method will give you the unsquared correlation coefficient (r). You'll have to square it (just type the number in R as number^2) to get r-squared."),
                       code("cor.test(d$contvar1,d$contvar2, method=\"pearson\")"),
                       br(),
                       br(),
                       p("To use Spearman Rank Correlation Coefficient to calculate the statistical significance of a correlation between two continuous variables, use the following code. In this code \"cor.test\" is the R command for running the correlation test, \"d\" is the name of your dataset, \"contvar1\" is the name of one of your continuous variables, \"contvar2\" is the name of the other continuous variable, and \"method=\"spearman\"\" means you want to use the Spearman Rank Correlation Coefficient. This method will give you the unsquared correlation coefficient (rho). You'll have to square it (just type the number in R as number^2) to get rho-squared."),
                       code("cor.test(d$contvar1,d$contvar2, method=\"spearman\")"),
                       br(),
                       br()
                      ),
               ),
   
    
    # These are tabs that students can use to run statistical tests. If they click on different tabs, they will see different conditional tabs (see above)
     mainPanel(
      tabsetPanel(
        id = "tabs", 
        
        # Summary statistics tab 
        tabPanel('Data summary',
                 br(),
                 h4("Inspecting summary statistics"),
                 selectInput('choosevarSUM', 'Choose a continuous variable', c()),
                 fluidRow(column(12, offset=1, p(strong("Average")),
                                 textOutput('valueAVG'),
                                 br(),
                                 p(strong("Sample size \"n\"")),
                                 textOutput('valueN'),
                                 br(),
                                 p(strong("Minimum and maximum values")),
                                 textOutput('valueRANGE'),
                                 br(),
                                 p(strong("Variance")),
                                 textOutput('valueVAR'),
                                 br(),
                                 br())),
                 h4("Inspecting a histogram of a continuous variable"),
                 selectInput('choosevarHIST1', 'Choose a continuous variable', c()),
                 plotOutput(outputId = "hist_plot1", height = "300px", width="600px"),
                 br(),
                 br(),
                 br(),
                 h4("Inspecting histograms of a continuous variable across two groups"),
                 selectInput('choosevarHIST2cont', 'Choose a continuous variable', c()),
                 selectInput('choosevarHIST2cat', 'Choose a group variable', c()),
                 splitLayout(
                   plotOutput(outputId = "hist_plot2", height = "300px"),
                   plotOutput(outputId = "hist_plot3", height = "300px")),
                 ),
        
        # Plots tab where students can make a simple scatter plot or box plot 
        tabPanel('Plots',
                 
                 h4("Making a correlation plot (scatter plot)"),
                 fluidRow(
                   column(4,
                          selectInput('choosevarSP2', 'Choose a continuous variable (X axis)', c()),
                          selectInput('choosevarSP1', 'Choose another continuous variable (Y axis)', c())),
                   column (4,style = "background-color:antiquewhite",
                           textInput("cortitleTXT", "Enter plot title:", ""),
                           textInput("corXaxisTXT", "Enter custom X axis label:", ""),
                           textInput("corYaxisTXT", "Enter custom Y axis label:", "")
                          )),
                 plotOutput("scatterplot1", width="600px"),
                 
                 
                 h4("Making a boxplot"),
                 fluidRow(
                   column(4,
                          selectInput('choosevarBP2', 'Choose a group variable (X axis)', c()),
                          selectInput('choosevarBP1', 'Choose a continuous variable (Y axis)', c())),
                   column (4, style = "background-color:antiquewhite",
                           textInput("boxtitleTXT", "Enter plot title:", ""),
                           textInput("boxXaxisTXT", "Enter custom X axis label:", ""),
                           textInput("boxYaxisTXT", "Enter custom Y axis label:", ""),
                           )),
                 plotOutput("boxplot1", width="500px"),
                ),
        
        # Two-sample tests for group differences, includes parametric and non-parametric tests 
        tabPanel('Group tests',
                 
                 # t-test
                 h4("Two-sample t-test"),
                 p("Use this test if your continuous variable is normally distributed within groups"),
                 selectInput('choosevarTT1', 'Choose a continuous variable', c()),
                 selectInput('choosevarTT2', 'Choose a group variable', c()),
                 fluidRow(column (8, offset=1,
                                  p(strong("The observed test statistic t:")),
                                  textOutput('statisticTT'),
                                  p(strong("The observed p-value:")),
                                  textOutput('pvalueTT'),
                                  br(),
                                  br())),
                 
                 # Mann-Whitney test
                 h4("Mann-Whitney U Test"),
                 p("Use this test if your continuous variable is NOT normally distributed within groups"),
                 selectInput('choosevarMWU1', 'Choose a continuous variable', c()),
                 selectInput('choosevarMWU2', 'Choose a group variable', c()),
                 fluidRow(column (8, offset=1,
                                  p(strong("The observed test statistic U:")),
                                  textOutput('statisticMWU'),
                                  p(strong("The observed p-value:")),
                                  textOutput('pvalueMWU'))),
                 br(),
                 br(), 
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
                ),
        
        # Correlation test tab, includes parametric and non-parametric tests 
        tabPanel('Correlation tests',
                 
                 # Pearson
                 h4("Pearson correlation coefficient"),
                 p("Use this test if your continuous variables are normally distributed"),
                 selectInput('choosevarPC1', 'Choose one continuous variable', c()),
                 selectInput('choosevarPC2', 'Choose another continuous variable', c()),
                 fluidRow(column (8, offset=1,
                                  p(strong("The observed correlation coefficient r^2:")),
                                  textOutput('statisticPC'),
                                  p(strong("The observed p-value:")),
                                  textOutput('pvaluePC'),
                                  br(),
                                  br(),
                                  br(),
                                  br())),
                 
                 # Spearman
                 h4("Spearman correlation coefficient"),
                 p("Use this test if your continuous variables are NOT normally distributed"),
                 selectInput('choosevarSPR1', 'Choose one continuous variable', c()),
                 selectInput('choosevarSPR2', 'Choose another continuous variable', c()),
                 fluidRow(column (8, offset=1,
                                  p(strong("The observed Spearman correlation coefficient rho^2:")),
                                  textOutput('statisticSPR'),
                                  p(strong("The observed p-value:")),
                                  textOutput('pvalueSPR'),
                                  br(),
                                  br(),
                                  br(),
                                  br())),
                 
              )
           ),
        )
    ),
)

###################################
###########   SERVER    ###########
###################################


server <- function(input, output,session) {
  
  #######  CSV FILE UPLOAD COMMANDS #######
  
  # The file upload code is taken without modifications from the https://shiny.rstudio.com/gallery/file-upload.html widget. See the website for explanations
  output$contents <- renderTable({
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  # This code defines the uploaded spreadsheet as your data
  data <- reactive({
    validate(need(input$file1 !="",""))
    inFile <- input$file1 
    if (is.null(inFile)){return(NULL)} 
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  # These lines ask Shiny to "observe" which variables you choose to analyze. Each drop-menu in the webpage has been given a unique name. The default choice is empty
  observe({updateSelectInput(session,"choosevarSUM",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarHIST1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarHIST2cont",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarHIST2cat",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarSP1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarSP2",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarBP1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarBP2",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarTT1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarTT2",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarMWU1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarMWU2",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarPC1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarPC2",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarSPR1",choices=c("\n",names(data())))})  
  observe({updateSelectInput(session,"choosevarSPR2",choices=c("\n",names(data())))})  
  
  
  
  
  
  ##################### COMMANDS FOR STATISTICAL TESTS AND PLOTS ######################
  
  #################### Summary statistics ####################
  
  # Tell Shiny that you want to use the variable selected in a specific drop down menu on the website. These are the inputs that Shiny continually "observes".
  # This code also overrides the error messages that pop up when nothing has been selected
  selectedDataSUM <- reactive({
    validate(need(input$choosevarSUM !="\n",""))
    data()[, c(input$choosevarSUM)]
  })
  
  # Commands for calculating average, sample size, variance, and range.
  AVGtestout<-reactive({
    varAVG=selectedDataSUM()
    mean(varAVG, na.rm=TRUE)
  })
  
  Ntestout<-reactive({
    varN=selectedDataSUM()
    length(varN[!is.na(varN)])
  })
  
  VARtestout<-reactive({
    varVAR=selectedDataSUM()
    var(varVAR, na.rm=TRUE)
  }) 
  
  RANGEtestout<-reactive({
    varRANGE=selectedDataSUM()
    range(varRANGE, na.rm=TRUE)
  }) 
  
  # Commands for passing the above calculations as a text to the website
  output$valueAVG <- renderText({
    AVGtestout()
  })
  
  output$valueN <- renderText({
    Ntestout()
  })
  
  output$valueVAR <- renderText({
    VARtestout()
  })
  
  output$valueRANGE <- renderText({ 
    RANGEtestout()
  })
  
  ##################### Histograms ######################
  
  #Single histogram
  # Tell Shiny that you want to use the variable selected in a specific drop down menu on the website.
  selectedDataHIST1 <- reactive({
    validate(need(input$choosevarHIST1 !="\n",""))
    data()[, c(input$choosevarHIST1)]
  })
  
  # Commands for making a simple histogram. Feel free to change its title under "main". Feel free to choose a different color. 
  output$hist_plot1<- renderPlot({
    hist(selectedDataHIST1(), main="", col="lightblue")
  })
  
  # Histograms of continuous variable across two groups
  # Tell Shiny that you want to use the variable selected in a specific drop down menu on the website.
  selectedDataHIST2 <- reactive({
    validate(need(input$choosevarHIST2cont !="\n",""))
    validate(need(input$choosevarHIST2cat !="\n",""))
    data()[, c(input$choosevarHIST2cont, input$choosevarHIST2cat)]
  })
  
  #Command for histogram for the first group
  output$hist_plot2<- renderPlot({
    splitdata=split(selectedDataHIST2()[,input$choosevarHIST2cont],selectedDataHIST2()[,input$choosevarHIST2cat])
    splitgroup1=data.frame(splitdata[1])
    hist(splitgroup1[,], main=names(splitgroup1[]), col="lightgray")
  })
 
   #Command for histogram for the second group
  output$hist_plot3<- renderPlot({
    splitdata=split(selectedDataHIST2()[,input$choosevarHIST2cont],selectedDataHIST2()[,input$choosevarHIST2cat])
    splitgroup2=data.frame(splitdata[2])
    hist(splitgroup2[,],main=names(splitgroup2[]), col="pink")
  }) 
  
  ##################### Plots ######################
  
  # Scatter plot
  # Tell Shiny that you want to use the variables selected in the specific drop down menus on the website.
  selectedDataSP <- reactive({
    validate(need(input$choosevarSP1 !="\n",""))
    data()[, c(input$choosevarSP1, input$choosevarSP2)]
  })
  
  # Commands to make a scatter plot
  # The if-else statement makes it possible for students to make a box plot and then change its title and axes labels.
  output$scatterplot1 <- renderPlot({
    labsSP=c(names(selectedDataSP()))
    cortitle=input$cortitleTXT
    if(input$corXaxisTXT=="" & input$corYaxisTXT==""){plot(selectedDataSP()[,input$choosevarSP1]~selectedDataSP()[,input$choosevarSP2], pch=19, lwd=5, ylab=labsSP[1], xlab=labsSP[2], main=cortitle)}
    else{plot(selectedDataSP()[,input$choosevarSP1]~selectedDataSP()[,input$choosevarSP2], pch=19, lwd=5, ylab=input$corYaxisTXT, xlab=input$corXaxisTXT, main=cortitle)}
  })
  
  # Box plot
  # Tell Shiny that you want to use the variables selected in the specific drop down menus on the website.
  selectedDataBP <- reactive({
    validate(need(input$choosevarBP1 !="\n",""))
    data()[, c(input$choosevarBP1, input$choosevarBP2)]
  })
  
  # Commands to make a box plot
  # The if-else statement makes it possible for students to make a box plot and then change its title and axes labels.
  output$boxplot1 <- renderPlot({
    labsBP=c(names(selectedDataBP()))
    boxtitle=input$boxtitleTXT
    if(input$boxXaxisTXT=="" & input$boxYaxisTXT==""){boxplot(selectedDataBP()[,input$choosevarBP1]~selectedDataBP()[,input$choosevarBP2], xlab=labsBP[2], ylab=labsBP[1], col="lightblue",main=boxtitle)}
    else{boxplot(selectedDataBP()[,input$choosevarBP1]~selectedDataBP()[,input$choosevarBP2], ylab=input$boxYaxisTXT, xlab=input$boxXaxisTXT, main=boxtitle, col="lightblue")}
  })
  
  
  ##################  Group tests ####################
  
  #t-test
  # Tell Shiny that you want to use the variables selected in the specific drop down menus on the website.
  selectedDataTT <- reactive({
    validate(need(input$choosevarTT1 !="\n",""))
    validate(need(input$choosevarTT2 !="\n",""))
    data()[, c(input$choosevarTT1, input$choosevarTT2)]
  })
  
  # Commands for running a basic two-tailed t-test
  TTestout<-reactive({
    varTT1=selectedDataTT()[,input$choosevarTT1]
    varTT2=selectedDataTT()[,input$choosevarTT2]
    t.test(varTT1~varTT2)
  })
  
  # This code extracts the t-statistic
  output$statisticTT <- renderText({
    vals <- TTestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
  })
  
  # This code extracts the p-value
  output$pvalueTT <- renderText({
    vals <- TTestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value
  })
 
  
  #Mann-Whitney U test
  # Tell Shiny that you want to use the variables selected in the specific drop down menus on the website.
  selectedDataMWU <- reactive({
    validate(need(input$choosevarMWU1 !="\n",""))
    validate(need(input$choosevarMWU2 !="\n",""))
    data()[, c(input$choosevarMWU1, input$choosevarMWU2)]
  })
  
  # Commands for running a basic two-tailed MWU test
  MWUtestout<-reactive({
    varMWU1=selectedDataMWU()[,input$choosevarMWU1]
    varMWU2=selectedDataMWU()[,input$choosevarMWU2]
    wilcox.test(varMWU1~varMWU2)
  })
  
  # This code extracts the MWU p-value
  output$pvalueMWU <- renderText({
    vals <- MWUtestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value
  })
  
  # This code extracts the MWU U statistic
  output$statisticMWU <- renderText({
    vals <- MWUtestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
  })
  
  ################## Correlation tests ######################
 
  # Pearson correlation
  # Tell Shiny that you want to use the variables selected in the specific drop down menus on the website.
  selectedDataPC <- reactive({
    validate(need(input$choosevarPC1 !="\n",""))
    validate(need(input$choosevarPC2 !="\n",""))
    data()[, c(input$choosevarPC1, input$choosevarPC2)]
  })
  
  # Command for Pearson test
  PCtestout<-reactive({
    varPC1=selectedDataPC()[,input$choosevarPC1]
    varPC2=selectedDataPC()[,input$choosevarPC2]
    cor.test(varPC1,varPC2, method="pearson")
  })
  
  # While the coefficient of determination is not technically a statistic that belongs to the Pearson correlation, it is equivalent to a squared Pearson's r. r-squared is more intuitive for undergrads than r. 
  CORtestout<-reactive({
    varCOR1=selectedDataPC()[,input$choosevarPC1]
    varCOR2=selectedDataPC()[,input$choosevarPC2]
    cor(varCOR1,varCOR2,use="complete.obs")^2
  })
  
  # These lines pass the p-value and r^2 as text to the webpage. 
  output$statisticPC <- renderText({
    CORtestout()
  })
  
  output$pvaluePC <- renderText({
    vals <- PCtestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value
  })
  
  # Spearman rank correlation
  # Tell Shiny that you want to use the variables selected in the specific drop down menus on the website.
  selectedDataSPR <- reactive({
    validate(need(input$choosevarSPR1 !="\n",""))
    validate(need(input$choosevarSPR2 !="\n",""))
    data()[, c(input$choosevarSPR1, input$choosevarSPR2)]
  })
  
  # Command for Spearman Rank test
  SPRtestout<-reactive({
    varSPR1=selectedDataSPR()[,input$choosevarSPR1]
    varSPR2=selectedDataSPR()[,input$choosevarSPR2]
    cor.test(varSPR1,varSPR2, method="spearman")
  })
  
  # These lines pass the r^2 as text to the webpage. While Spearman's r^2 is not normally provided with the test, it is the non-parametric equivalent of the coefficient of determination, and as such is equlivalent to r^2 from linear regreessions
  output$statisticSPR <- renderText({
    vals <- SPRtestout()
    if (is.null(vals)){return(NULL)}
    vals$estimate^2
  })
  
  # These lines pass the p value as text to the webpage
  output$pvalueSPR <- renderText({
    vals <- SPRtestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value
  })
  
}

shinyApp(ui = ui, server = server)
