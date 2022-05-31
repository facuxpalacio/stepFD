library(shiny) #1.6.0
library(shinydashboard) #0.7.1
library(shinyjs) #2.0.0
library(shinyBS) #0.61.1
library(rmarkdown) #2.13
library(knitr) #1.34

ui <-dashboardPage(
  dashboardHeader(title = "A protocol for functional diversity analyses",
                  titleWidth = 450),
  
  # Sidebar content
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("home")),
      menuItem("Step 1. Research question", tabName = "step1"),
      menuItem("Step 2. Study design", tabName = "step2"),
      menuItem("Step 3. Community data", tabName = "step3"),
      menuItem("Step 4. Trait data", tabName = "step4"),
      menuItem("Step 5. Data exploration", tabName = "step5"),
      menuItem("Step 6. Functional diversity", tabName = "step6"),
      menuItem("Step 7. Modelling", tabName = "step7"),
      menuItem("Step 8. Reproducibility", tabName = "step8")
    )
  ),
  
  # Body content
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      
      # Tab contents
      tabItem(tabName = "dashboard",
              fluidRow(
              box(title = "", width = 10,
                  "This application is intended to provide students and researchers with a checklist to maximize methods' reproducibility, 
                  comparability, and transparency across trait-based studies. It allows the user to create a reproducibility document that 
                  reports all the steps of a trait-based analysis. This can be uploaded alongside a scientific publication to ensure 
                  transparency and reproducibility. For further details, see:",
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
              
              img(src = "sticker_app.png", height = 150),
              
              box(title = "Protocol to be filled out offline", width = 5,
              downloadButton("download_csv", "Download csv file"),
              downloadButton("download_doc", "Download doc file"),
              downloadButton("download_rtf", "Download rtf file"))
              )
      ),
      
      tabItem(tabName = "step1",
              helpText("Start with the conceptualization of an ecological question 
                  embedded in a theoretical framework with a set of hypotheses and 
                  predictions.", style = "background-color:lightblue; border-radius:5px"),
              
              textInput("title", "Title of the study"),
              textInput("authors", "Authors"),
              textInput("study_link", "Link to preprint/DOI (if available)"),
              
              radioButtons("step1", "Is your work open-ended (e.g., descriptive) or does it answer a specific research question?",
                           choices = c("My work focuses on a particular question",
                                       "My work is open-ended")),     
              fluidRow(
                column(6, conditionalPanel('input.step1 == ["My work focuses on a particular question"]',
                                           textInput("hyp", "Hypotheses and predictions", 
                                                     value = "", placeholder = "My ecological question ...")))),
              fluidRow(column(6, 
                              conditionalPanel('input.step1 == ["My work is open-ended"]',
                                                  textInput("nohyp", "Main patterns/variables examined", 
                                                            value = "", placeholder = "Variables under study...")))),
              
              bsTooltip("Main patterns/variables examined", title = "Does seed size diversity decrease at higher latitudes? or are spider assemblages functionally richer than others?",
                        placement = "right"),
      ),
             
      tabItem(tabName = "step2",
              helpText("Choose an appropriate sampling or experimental design, along with the 
                  scale of analysis and the study organisms and units (populations, 
                  species, communities) selected to answer the research question.",
                  style = "background-color:lightblue; border-radius:5px"),
                  
                  div(id="step2", "Experimental or sampling design"),
                  textInput("scale1","Spatial scale(s) of analysis"),
                  bsTooltip("scale1", title = "Geographic scale, composed by grain and extent",
                        placement = "right"),
              
                  textInput("scale2","Temporal scale(s) of analysis"),
                  bsTooltip("scale2", title = "E.g., monthly, seasonal, a full year",
                        placement = "right"),
              
                  textInput("unit1","Target ecological unit"),
                  bsTooltip("unit1", title = "Ecological entity used as sampling unit, typically species, but it can be any, such as DNA, individuals, genera or communities",
                        placement = "right"),
              
                  radioButtons("pow1", "Did you perform a power analysis?", choices=c("No", "Yes")),   
                  bsTooltip("pow1", title = "Power analysis can help understand the effect size required to conclude significance",placement = "right"),
                  
                  textInput("pow2", "Results of power analysis or rationale for lack of need", value = "", 
                            placeholder = "I can detect an effect size of...")
      ),
            
      tabItem(tabName = "step3",
                      withMathJax(),
                          helpText("Collect occurrence data and build a matrix of", em("S"),
                              "sampling units \\(\\times\\)", em("N"), "taxa.",
                              style = "background-color:lightblue; border-radius:5px"),
                              br(),
                              textInput("foc","Focal taxon/taxa"),
                              textInput("reso", "Taxonomic resolution"),
                              bsTooltip("reso", title = "It is common for traits to be measured at the individual level and then averaged at the species level, so taxonomic resolution refers to the final taxonomic level used for functional diversity computation",
                              placement = "right"),
              
                              textInput("ntax", "Number of taxa"),
                              
                              textInput("unit2", "Sampling unit"),
                              bsTooltip("unit2", title = "Ecological unit that describes a single community and represents one row in the community matrix (e.g., city, field, quadrat, transect, lake)",
                              placement = "right"),
                              
                              textInput("s_units", "Number of sampling units"),
                              bsTooltip("s_units", title = "Number of independent samples (e.g., if having repeated measures, the number of sampling units is the number of unique sampling sites)",
                              placement = "right"),
              
                              textInput("s_eff", "Sampling effort"),
                              bsTooltip("s_eff", title = "Includes how many sampling sites were sampled within a particular time-frame and sample duration, frequency and intensity",
                              placement = "right"),
                              
                              checkboxGroupInput("dtyp","Occurrence data type(s)", 
                                          choices = c("Presence-absence", "Presence-only", 
                                                      "Occupancy probability", "Abundance",
                                                      "Detection-corrected abundance",
                                                      "Biomass", "Percent cover", "Other")),
              conditionalPanel('input.dtyp.indexOf("Other") > -1', textInput("dtyp_info", "Please specify")),
                          ),
             
      tabItem(tabName = "step4",
                          helpText("Collect functional trait data and build a table of", em("N"), 
                              "taxa \\(\\times\\)", em("p"), "traits.",
                              style = "background-color:lightblue; border-radius:5px"),
                              br(),
                          
              textInput("ntraits","Number of traits"),
            
              textInput("cont", "Number of continuous traits"),
              bsTooltip("cont", title = "Typically morphological measurements",
                        placement = "right"),
              textInput("disc", "Number of discrete traits"),
              bsTooltip("disc", title = "Integers. E.g., offspring number, number of pollinator species",
                        placement = "right"),
              textInput("bin", "Number of binary traits"),
              bsTooltip("bin", title = "Traits with only two mutually-exclusive categories, e.g., diurnal/nocturnal, migratory/non-migratory, presence/absence of a dietary item. If a trait has more than two mutually-exclusive categories, then for analysis purposes, these are usually split into as many traits as categories there are",
                        placement = "right"),
              textInput("fuzzy", "Number of fuzzy-coded traits"),
              bsTooltip("fuzzy", 
                        title = "Indicates to which extent a taxon exhibits each trait category. E.g., 0 = taxon has no affinity for a certain trait category, 1 = taxon has low affinity for a certain trait category, 2 = taxon has a high affinity for a certain trait category, but other categories can occur with equal (2) or lower (1) affinity, 3 = taxon has exclusive affinity for a certain trait category",
                        placement = "right"),

             textInput("t_resol", "Trait resolution"),
             bsTooltip("t_resol", title = "Coarseness of traits, ranging from highest-resolution continuous measurements to lowest-resolution binary categories. For those categorical traits that are not categorical in nature (e.g., body mass split into small vs large species), report the number of categories (binary traits) used to describe the trait",
             placement = "right"),
              
             textInput("samps", "Mean sample sizes per species and trait"),
             
             textInput("mean", "Ecological significance of the selected traits"),
             bsTooltip("mean", title = "Hypothesized function of traits. E.g., tree height influences competitive ability, fruit and seed consumption relates to seed dispersal and seedling establishment",
                       placement = "right"),
             
             radioButtons("intra",  "Did you account for intraspecific trait variation?", choices=c("No", "Yes")),
             conditionalPanel('input.intra == ["Yes"]', textInput("intrasp_info", "Which approach did you use to account for intraspecific variability?")),
             
             checkboxGroupInput("dsource","Data source(s)",
                                choices = c("Online database", "Museum/herbarium collection", "Own's field measurements", "Literature review", "Other")),
             conditionalPanel('input.dsource.indexOf("Other") > -1', textInput("other_source", "Please specify"))
             ),
             
      tabItem(tabName = "step5",
             helpText("Visually inspect the community and trait matrices to familiarize with your
                      data and deal with any issue therein.", 
                      style = "background-color:lightblue; border-radius:5px"),
             
             checkboxGroupInput("dataexp", "Data exploration analysis", 
                                choices=c("Data visualization", "Collinearity assessment", "Missing data assessment", "Spatiotemporal dependence assessment", "Species sampling coverage")),
             
             textInput("coll", "Traits possessing collinearity, if any"),
             bsTooltip("coll", title = "Collinearity: high correlation (e.g., >|0.7|) between traits",
                       placement = "right"),
             
             textInput("trans","Data transformations performed, if any"),
             bsTooltip("trans", title = "This may be required to conform to future model assumptions",
                       placement = "right"),
             
             radioButtons("miss", "Do you have missing data?", choices=c("No", "Yes")),
             conditionalPanel('input.miss == ["Yes"]', 
                              textInput("miss_info", "How did you handle these?")),
            
             radioButtons("det", "Did you account for imperfect detection?", choices=c("No", "Yes")),
             conditionalPanel('input.det == ["Yes"]', textInput("detection_info", "Which approach did you use to account for imperfect detection?"))
    ),
    
      tabItem(tabName = "step6",
             helpText("Now you can compute functional diversity metrics!",
                      style = "background-color:lightblue; border-radius:5px"),
            
             checkboxGroupInput("level", "Level of analysis", 
                                choices=c("Sub-alpha diversity (observation/individual level)", "Alpha diversity (within group)", "Beta diversity (between group)", "Gamma diversity (entire trait pool)")),
             
             checkboxGroupInput("space", "Method used to build the functional trait space(s)",
                                choices=c("Functional dendrogram", "Ordination methods (e.g., PCA, PCoA)", "Convex hull", "Probabilistic hypervolume", "Other")),
             conditionalPanel('input.space.indexOf("Other") > -1',
                              textInput("space_info", "Please specify")),
             
             checkboxGroupInput("diss", "Dissimilarity metric used",
                                choices=c("Gower", "Bray-Curtis", "Jaccard", "Sørensen", "Other")),
             conditionalPanel('input.diss.indexOf("Other") > -1', 
                              textInput("diss_info", "Please specify")),
             
             checkboxGroupInput("methods", "Trait space property measured (i.e., functional diversity facet)", 
                            choices = c("Richness", "Regularity", "Divergence", "Redundancy", "Composition", "Originality/rarity", "Other")),
             conditionalPanel('input.methods.indexOf("Other") > -1', textInput("methods_info", "Please specify")),
             
             textInput("metric","Additional information on implementation (e.g. use of Jaccard dissimilarity metric)"),
            
             radioButtons("null", "Did you use null models to account for species richness?", choices=c("No", "Yes")) 
             ),
            
    tabItem(tabName = "step7",
            helpText("Fit, interpret, report and validate your statistical model.",
                     style = "background-color:lightblue; border-radius:5px"),
              
            textInput("model", "Statistical model or test chosen that is appropriate to answer your research question"),
            
            textInput("effs", "Effect sizes"),
            bsTooltip("effs", title = "Useful for interpreting the results and for future meta-analyses",
                      placement = "right"),
            
            textInput("supp", "Model support"),
            bsTooltip("supp", title = "Information criteria (e.g., AIC, BIC), R², AUC",
                      placement = "right"),
            
            textInput("uncert", "Model uncertainty"),
            bsTooltip("uncert", title = "Standard errors, confidence or credible intervals",
                      placement = "right"),
            
            textInput("valid", "Model validation"),
            bsTooltip("valid", title = "Validation involves testing your model against data that were not used to fit it.",
            placement = "right")),
    
    tabItem(tabName = "step8",
            helpText("Provide enough data and code detail to allow full reproducibility
                of your results.", style = "background-color:lightblue; border-radius:5px"),
      
            
            textInput("prer", "Link to preregistration (if any)", value = "", 
                      placeholder = "My preregistration is hosted at osf.io/..."),
            bsTooltip("prer", title = "Posting a plan for analysis before performing it, both for transparency and to demonstrate hypotheses were made before the results were known e.g. on OSF, bioRxiv or EcoEvoRxiv",
                      placement = "right"),
            
            selectizeInput('dms','I have a Research Data Management Plan',choices=c("yes", "no")),
            bsTooltip("dms", title = "Research data management plans ensure the long-terms stability of data and code produced as part of a project, appoint data stewards, and provide intellectual property details.",
                      placement = "right"),       
            textInput("link_code", "Link to repository of code", value = ""),
            textInput("link_comm_data", "Link to repository of Community data (if it's the same the as code repository, just repeat)", value = ""),
            textInput("link_trait_data", "Link to repository of Trait data (if it's the same the as code repository, just repeat)", value = ""),
            textInput("link_env_data", "Link to repository of Environmental data (if it's the same the as code repository, just repeat)", value = ""),
            checkboxGroupInput('data_code_desc',"Data and code description",
                               choices = c('My project has a license that describes conditions of reuse',"I have a README file","I've included software and package version numbers", 'My code has informative comments','The code I provided can reproduce all results, figures and tables', 'If applicable, my README contains download dates for external data and any filters used', 'I have backup copies of my data and code','I have named data files, variables, and scripts in an informative way')),
            bsTooltip("data_code_desc", title = "These components ensure a minimum level of reproducibility using stored data and code",
                      placement = "right"),
            textInput('repro_notes', "Other notes on reproducibility"),
            bsTooltip("repro_notes", title = "e.g. have you appointed a data steward to ensure long-term stability? Does the code use a package management system like renv? Will the project be linked to a DOI with Zenodo or similar?",
                      placement = "right"),
    
                      div("Save filled checklist"),
                      div(downloadButton("download_filled_csv", "Download csv file", class = "btn-primary")),
                          div(downloadButton("download_filled_doc", "Download doc file", class = "btn-primary")),
                            div(downloadButton("download_filled_rtf", "Download rtf file", class = "btn-primary")),
                  
                        shinyjs::hidden(
                          div(
                            id = "thankyou_msg",
                            h3("Thanks for creating your protocol!")
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
  
  # Downloadable csv or docx template to fill out offline
  fieldnames <- c("Study title", "Authors", "Link to preprint/DOI (if available)", "Focus","Hypothesis","Open-ended patterns examined (if applicable)", "Scale", "Ecological unit","Power analysis", "Power analysis results/rationale", "Focal taxa","Resolution", "Number of taxa", "Sampling unit", "Number of sampling units","Sampling effort","Occurrence data type","Other occurrence data type (if applicable)", "Number of traits","Continuous traits used","Discrete traits used", "Binary traits used", "Fuzzy-coded traits used", "Trait resolution", "Sample site per species and trait", "Hypothesized function of each trait","Intraspecific variation accounted for?", "How was intraspecific variation accounted for (if applicable)?","Data source", "Other data sources (if applicable)", "Data exploration","Collinearity assessed?","Transformations done?","Missing data accounted for?", "Imperfect detection control","Functional trait space method", "Other functional trait space method (if applicable)", "Dissimilarity metric used for trait space (if applicable)","Other dissimilarity metric (if applicable)", "Level of analysis", "FD method", "Other FD method (if applicable)", "Method detail", "Model", "Effect sizes", "Model support", "Model uncertainty", "Validation method","Preregistration", "Code link","Community data link","Trait data link","Environmental data link","Data and Code description",'Reproducibility Notes', "Date")
  output$download_csv <- downloadHandler(
    filename = "FDprotocol.csv",
    content = function(file) {
      write.csv(data.frame(Field = fieldnames, Response = ""), file, row.names = FALSE)
    }
  )
  
  output$download_doc <- downloadHandler(
    filename = "FDprotocol.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "FDprotocol.Rmd")
      file.copy("FDprotocol.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = gsub('\\"','',data.frame(Field = fieldnames, Response = NA)))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      }
  )
  
  output$download_rtf <- downloadHandler(
    filename = "FDprotocol.rtf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "FDprotocol.Rmd")
      file.copy("FDprotocol.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = data.frame(Field = fieldnames, Response = NA))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_format='rtf_document', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
 output$download_filled_rtf <- downloadHandler(
     filename = "FDprotocol_filled.rtf",
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "FDprotocol.Rmd")
       file.copy("FDprotocol.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
     #  params <- list(n = data.frame(Field = fieldnames, Response = NA))
      params <- list(n = data.frame(formData(), row.names = NULL))
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_format="rtf_document",output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
  
  output$download_filled_csv <- downloadHandler(
    filename = "FDprotocol_filled.csv",
    content = function(file) {
      write.csv(formData2(), file, row.names = FALSE)
    }
  )
  output$download_filled_doc <- downloadHandler(
    filename = "FDprotocol_filled.doc", 
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "FDprotocol.Rmd")
      file.copy("FDprotocol.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = data.frame(formData(), row.names=NULL))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
 formData <- reactive({
   data <- sapply(fieldsAll, function(x) input[[x]])
   long<-which(sapply(data, length)>1)
   data[long]<-gsub("c\\(", "", data[long])
   for (i in long)
   {
     data[i][length(data[i])]<-gsub("\\)$", "", data[i][length(data[i])])
   }
   data <- c(data, date = humanTime())
   data<-cbind(fieldnames,data)
  colnames(data)<-c("Field", "Response")
   data
 })
 
 #Here is my addition, I created a second FormaData where we quote the fields so excel reads them a string
 #Quoting is performed only in fields where we have commas. 
 #We could have had only one formData, but then quoting would have been seen in DOC or RTF. Therefore I decided to make a separate one.
 formData2 <- reactive({
   data <- sapply(fieldsAll, function(x) input[[x]])
   long<-which(sapply(data, length)>1)
   data[long]<-gsub("c\\(", "", data[long])
  for (i in long)
  {
    data[i][length(data[i])]<-gsub("\\)$", "", data[i][length(data[i])])
  }
   data <- dQuote(data,q=FALSE)
   data <- c(data, date = humanTime())#add escape characters to commas to avoid breaking up into more than 1 cell
   data<-cbind(fieldnames,data)
   colnames(data)<-c("Field", "Response")
   data
 })
 
  # action to take when checklist downloadded
 observeEvent(input$download_filled_csv, {
   shinyjs::show("thankyou_msg")})
   observeEvent(input$download_filled_doc, {
     shinyjs::show("thankyou_msg")})
     observeEvent(input$download_filled_doc, {
       shinyjs::show("thankyou_msg")})
}



fieldsAll <- c("title","authors", "study_link", "step1","hyp","nohyp", "scale", "unit1","pow1", "pow2", "foc", "reso", "ntax", "unit2", "s_units", "s_eff", "dtyp", "dtyp_info", "ntraits", "cont", "disc", "bin", "fuzzy", "t_resol", "samps", "mean", "intra", "intra_info", "dsource", "other_source", "dataexp", "coll", "trans","miss", "det", "space", "space_info", "diss", "diss_info", "level", "methods", "methods_info", "metric", "model", "effs", "supp", "uncert", "valid", "prer","link_code","link_comm_data","link_trait_data","link_env_data","data_code_desc",'repro_notes')

responsesDir <- file.path("../output")

humanTime <- function() format(Sys.time(), "%Y%m%d")


shinyApp(ui = ui, server = server) 
