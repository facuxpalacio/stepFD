library(shiny) #1.6.0
library(shinydashboard) #0.7.1
library(shinyjs) #2.0.0
library(shinyBS) #0.61.1

ui <-dashboardPage(
  dashboardHeader(title = "A protocol for functional diversity analyses",
                  titleWidth = 450),
  ## Sidebar content
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("home")),
      menuItem("Step 1. Research question", tabName = "step1"),
      menuItem("Step 2. Study design", tabName = "step2"),
      menuItem("Step 3. Community data", tabName = "step3"),
      menuItem("Step 4. Trait data", tabName = "step4"),
      menuItem("Step 5. Explore your data!", tabName = "step5"),
      menuItem("Step 6. Functional diversity", tabName = "step6"),
      menuItem("Step 7. Modelling", tabName = "step7"),
      menuItem("Step 8. Reproducibility", tabName = "step8")
    )
  ),
  
  ## Body content
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      
      # Tab contents
      tabItem(tabName = "dashboard",
              fluidRow(
              box(Title = "", width = 8,
                  "This application is intended to provide students and researchers with a checklist to maximize methods' reproducibility, 
                  comparability, and transparency across trait-based studies. For further details, see:",
                  tags$a("Palacio", em("et al."), " (2021). A protocol for reproducible functional diversity 
                           analyses. EcoEvoRxiv. doi: 10.32942/osf.io/yt9sb", 
                           href = "https://doi.org/10.32942/osf.io/yt9sb"), "and the ",
                  tags$a("user's guide.", href = "https://github.com/facuxpalacio/stepFD"),
                  br(),
                  br(),
                  em("This app is maintained by ",
                  tags$a("Facundo X. Palacio, ", href = "https://github.com/facuxpalacio"),
                  tags$a("Emma J. Hudgins ", href = "https://github.com/emmajhudgins"), "and ",
                  tags$a("Caio Graco-Roza.", href = "https://github.com/graco-roza"),
                  "Please feel free to contact us with any suggestions!")
                  ),
              
              img(src = "sticker_app.png", height = 150))
      ),
      
      tabItem(tabName = "step1",
              helpText("Start with the conceptualization of an ecological question 
                  embedded in a theoretical framework with a set of hypotheses and 
                  predictions.", style = "background-color:lightblue; border-radius:5px"),
      
              radioButtons("step1", "Identify whether your work is open-ended or answers a specific research question",
                   choices = c("My work focuses on a particular question, e.g. Does seed size diversity decrease at higher latitudes?",
                               "My work is open-ended, e.g. How do abiotic variables shape leaf morphology?")),     
                      fluidRow(
                        column(6, conditionalPanel('input.step1 == ["My work focuses on a particular question, e.g. Does seed size diversity decrease at higher latitudes? or
                                                   Are spider assemblages functionally richer than others?"]',
                                                   textInput("hyp", "Hypotheses and predictions", value = "", placeholder = "My ecological question ...")))),
                      fluidRow(column(6, conditionalPanel('input.step1 == ["My work is open-ended, e.g. How do abiotic variables shape leaf morphology?"]',
                                                          textInput("nohyp", "Main patterns/variables examined", value = "", placeholder = "Variables under study..."))))
      ),
             
      tabItem(tabName = "step2",
              helpText("Choose an appropriate sampling or experimental design, along with the 
                  scale of analysis and the study organisms and units (populations, 
                  species, communities) selected to answer the research question.",
                  style = "background-color:lightblue; border-radius:5px"),
                  
                  div(id="step2", "Identify an appropriate experimental or sampling design"),
                  textInput("scale1","What is(are) your spatial scale(s) of analysis?"),
                  bsTooltip("scale1", title = "Geographic scale, composed by grain and extent",
                        placement = "right"),
              
                  textInput("scale2","What is(are) your temporal scale(s) of analysis?"),
                  bsTooltip("scale2", title = "E.g., monthly, seasonal, a full year",
                        placement = "right"),
              
                  textInput("unit","What is your target ecological unit?"),
                  bsTooltip("unit", title = "Ecological entity used as sampling unit, typically species, but it can be any, such as DNA, individuals, genera or communities",
                        placement = "right"),
              
                  radioButtons("pow1", "Did you perform a power analysis?", choices=c("Yes", "No")),
                  textInput("pow2", "Results of power analysis or rationale for lack of need", value = "", 
                            placeholder = "I can detect an effect size of..."),
                  radioButtons("prer1", "Did you preregister?", choices=c("Yes", "No")),
                  textInput("prer2", "Link to preregistration or rationale for lack of need", value = "", 
                  placeholder = "My preregistration is hosted at osf.io/...")
      ),
            
      tabItem(tabName = "step3",
                      withMathJax(),
                          helpText("Collect occurrence data and build a matrix of", em("S"),
                              "sampling units \\(\\times\\)", em("N"), "taxa.",
                              style = "background-color:lightblue; border-radius:5px"),
                              br(),
                              textInput("foc","Indicate the focal taxon/taxa"),
                              textInput("reso", "What is your taxonomic resolution?"),
                              bsTooltip("reso", title = "It is common for traits to be measured at the individual level and then averaged at the species level, so taxonomic resolution refers to the final taxonomic level used for functional diversity computation",
                              placement = "right"),
              
                              textInput("ntax", "Indicate the number of taxa"),
                              textInput("s_units", "Indicate the number of sampling units"),
                              bsTooltip("s_units", title = "Number of 'independent' samples",
                              placement = "right"),
              
                              textInput("s_eff", "Report sampling effort"),
                              bsTooltip("s_eff", title = "Includes how many sampling sites were sampled within a particular time-frame and sample duration, frequency and intensity",
                              placement = "right"),
                              
                              selectInput("dtyp","Indicate the occurrence data type", 
                                          choices = c("Presence-absence", "Presence-only", 
                                                      "Occupancy probability", "Abundance", 
                                                      "Biomass", "Percent cover"))
                          ),
             
      tabItem(tabName = "step4",
                          helpText("Collect functional trait data and build a table of", em("N"), 
                              "taxa \\(\\times\\)", em("p"), "traits.",
                              style = "background-color:lightblue; border-radius:5px"),
                              br(),
                          textInput("ntraits","Indicate the number of traits"),
                          selectInput("tdtype", "Indicate the trait data types",
                                             choices = c("Numerical", "Categorical")),
              conditionalPanel(
                condition = "input.tdtype == 'Categorical'",
                selectInput(
                  "breaks", "Breaks",
                  c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
                )),
              
                          textInput("samps", "Report sample sizes per species and trait"),
                          textInput("mean", "What is the ecological significance or hypothetized function of the selected traits?"),
                          radioButtons("intra",  "Did you account for intraspecific trait variation?", choices=c("Yes", "No")),
                          checkboxGroupInput("dsource","Indicate the data source(s)",
                                 choices = c("Online database", "Museum/herbarium collection", "Field measurements", "Literature review")),
              ),
             
      tabItem(tabName = "step5",
             helpText("Visually inspect the community and trait matrices to familiarize with your
                      data and deal with any issue therein.", 
                      style = "background-color:lightblue; border-radius:5px"),
             radioButtons("plot", "Have you plotted your data?", choices=c("Yes", "No")),
             textInput("coll", "Indicate which traits possess collinearity, if any"),
             textInput("trans","Indicate any data transformations performed"),
             textInput("miss", "Do you have missing data? How did you handle these?"),
            radioButtons("det", "Did you account for imperfect detection?", choices=c("Yes", "No"))
            ),
      
      tabItem(tabName = "step6",
             helpText("Now you can compute functional diversity metrics!",
                      style = "background-color:lightblue; border-radius:5px"),
                  
                selectInput("level", "Identify the level of analysis", choices=c('alpha diversity', 'beta diversity', 'gamma diversity')),
                            textInput('group', "Indicate if any grouping of traits reflecting a similar function wasperformed"),
                            selectInput("methods", "Select the appropriate method based on the research question", choices=c("Richness", "Regularity", "Divergence", "Similarity", "Rarity/Originality")),
            textInput('metric',"Provide more specifics on implementation here (e.g. use of Jaccard dissimilarity metric)")
            ),
             ### Emma removed these because they seemed redundant?
                            # selectInput(metric,"Select the appropriate functional diversity metric", choices=c("")),
                            # selectInput("level", "Identify the level of functional diversity metric measurement", choices=c())),
            
    tabItem(tabName = "step7",
            helpText("Fit, interpret, report and validate your statistical model.",
                     style = "background-color:lightblue; border-radius:5px"), # Input: Load your community data
                textInput("model", "Indicate the statistical model or test chosen that is appropriate to answer your research question"),
            textInput('effs',   "Report effect sizes, model support and uncertainty"),
            radioButtons("graph", "Do you require graphical output? If so, save your plots!", choices=c("Yes", "No")),
            textInput("valid",  "Did you validate your model? If so, how?")
            ),

    tabItem(tabName = "step8",
            helpText("Provide enough data and code detail to allow full reproducibility
                of your results.", style = "background-color:lightblue; border-radius:5px"),
                                   checkboxGroupInput('dms',"Data management and storage",
                                                      choices = c('I have a Research Data Management Plan', 'My data and code are stored in a location with version control','I have backup copies of my data and code','My data and code are in an organized file system','my raw data is unaltered')),
                                   checkboxGroupInput('ip',"Intellectual property",
                                                      choices = c("My metadata and/or RDMP make it clear whose intellectual property this work represents", 'I have appointed a data steward','My project has a license that describes conditions of reuse')),
                                   checkboxGroupInput('metadata',"Metadata",
                                                      choices = c("I have a README file", 'My README explains how all my files interact','My README contains the title, authors, date and License', 'If applicable, my README contains download dates for external data and any filters used', 'I will update my README continuously as my project progresses')),
                                   checkboxGroupInput('code',"Code",
                                                      choices = c("I've included software and package version numbers", 'My code has informative comments','The code I provided can reproduce all results,figures and tables,', 'If applicable, my README contains download dates for external data and any filters used', 'I will update my README continuously as my project progresses')),
                                   checkboxGroupInput('host',"Hosting",
                                                      choices = c('My project files can be linked to a DOI (such as Zenodo in combination with GitHub')),
                                   checkboxGroupInput('naming',"Naming",
                                                      choices = c('I have named data files, variables, and scripts in an informative way')),
                      div(
                        id = "form",
                        actionButton("submit", "Save filled checklist", class = "btn-primary"),
                        
                        shinyjs::hidden(
                          div(
                            id = "thankyou_msg",
                            h3("Thanks for creating your protocol! See the output folder for your filled form")
                          )
                        )
                )
              )
      )
    )
)
######################################################################################


server <- function(input, output, session) {

bsTooltip("scale", "The scale of analysis...", placement = "bottom", trigger = "hover",
          options = NULL)
  
 formData <- reactive({
   data <- sapply(fieldsAll, function(x) input[[x]])
   data <- c(data, date = humanTime())#add escape characters to commas to avoid breaking up into more than 1 cell
  data<-gsub(",", "\\,", data)
  data<-gsub("c\\(", "", data)
  data<-gsub("\\)", "", data)
  data<-cbind(fieldnames,data)
  colnames(data)<-c("Field of checklist", "Response")
   data
 })
 saveData <- function(data) {
   fileName <- sprintf("FDprotocol_%s.csv",
                       humanTime())
   write.csv(x = data, file = file.path(responsesDir, fileName),
             row.names = FALSE)
 }
  # action to take when submit button is pressed
 observeEvent(input$submit, {
   shinyjs::show("thankyou_msg")
   saveData(formData())
 })
}

fieldsAll <- c("step1","hyp","nohyp", "scale", "unit","pow1", "pow2", "prer1", "prer2", "foc","reso", "ntax", "s_eff","ntraits","tdtype","samps", "respeff","sofhar","mean","intra","dsource", "plot","coll","trans","miss", "det", "level",'group', "methods", 'metric', "model", "graph", "valid", "dms",'ip','metadata','code','host','naming')

fieldnames <- c("Focus","Hypothesis","Patterns examined", "Scale", "Unit","Power analysis", "Power analysis results/rationale", "Preregistration", "Justification/location", "Focal taxa","Resolution", "Number of taxa","Sampling effort","Number of traits","ttrait data type","Number of samples", "Response or effect","Soft or hard","Ecological Significance","Intraspecific variation","Data source", "Plots made?","Collinearity","Transformation","Missing data", "Imperfect detection control", "Level of analysis",'Grouping/subsetting', "FD method", 'Method detail', "Model", "Graph needed?", "Validation method", "Data management system",'Intellectual property','Metadata','Code','Hosting','Naming', "Date")
responsesDir <- file.path("../output")

humanTime <- function() format(Sys.time(), "%Y%m%d")


shinyApp(ui = ui, server = server) 

