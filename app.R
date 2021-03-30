library(tidyverse)
library(shiny)
library(rsconnect)
# Define UI for dataset viewer app ----
# Define UI for dataset viewer app ----
#data
good_pv <- read.csv("Pv_STR_Top_Table.csv")
good_pf <- read.csv("Pf_STR_Top_Table.csv")
#Population or Country pairs
compare_Pv_Country <- colnames(good_pv)[11:ncol(good_pv)]
compare_Pf_Country <- colnames(good_pf)[39:ncol(good_pf)]
compare_Pf_Population <- colnames(good_pf)[11:38]
#Plot
#Pf Population
#Pf population loci plot
geno_pf <- read.csv("Pf_Genotype.csv")
geno_origin_Pf <- geno_pf
Pf_pop_loci <- read.csv("Pf_STR_Top_ShinyPlot.csv")
Pf_pop_loci <- good_pf %>% 
  filter(STRid %in% as.character(Pf_pop_loci$STRid))
Pf_pop_loci <- Pf_pop_loci %>% 
  pivot_longer(cols = CAF.EAF:WSEA.SAM) %>% 
  filter(is.na(value)==FALSE) %>% 
  arrange(value)
vars_pf_population <- setdiff(unique(Pf_pop_loci$value), "STRLoci")
geno_pf_population <- geno_origin_Pf
geno_pf_population <- geno_pf_population %>% 
  filter(STRid %in% as.character(Pf_pop_loci$STRid))

STR_Position <- Pf_pop_loci %>% 
  dplyr::select(STRid,value)

geno_pf_population <- STR_Position %>% 
  left_join(geno_pf_population,by="STRid")
geno_pf_population <- geno_pf_population[,-1]
geno_pf_population$value <- as.character(geno_pf_population$value)
geno_pf_population <- as.data.frame(geno_pf_population)
rownames(geno_pf_population) <- geno_pf_population$value
geno_pf_population <- geno_pf_population[,-1]
geno_pf_population <- t(geno_pf_population)
geno_pf_population <- data.frame(Sample=rownames(geno_pf_population),geno_pf_population)
pop_pf_population <- read.csv("Pf_Sample.csv")
colnames(pop_pf_population)[1] <- "Sample"
pop_pf_population$Country <- as.character(pop_pf_population$Country)
pop_pf_population$Country[pop_pf_population$Country=="Papua New Guinea"] <- "PapuaNewGuinea"
pop_pf_population$Country[pop_pf_population$Country=="Viet Nam"] <- "VietNam"
pop_pf_population$Country[pop_pf_population$Country=="Ivory Coast"] <- "IvoryCoast"
pop_pf_population$Country[pop_pf_population$Country=="Congo DR"] <- "CongoDR"

geno_pf_population <- geno_pf_population %>% 
  left_join(pop_pf_population,by="Sample")

vars_pf_population <- gsub(vars_pf_population,pattern="-",replacement=".")

#Pf Country
Pf_country_loci <- read.csv("Pf_STR_Top_ShinyPlot.csv")
Pf_country_loci <- good_pf %>% 
  filter(STRid %in% as.character(Pf_country_loci$STRid))
Pf_country_loci <- Pf_country_loci %>% 
  pivot_longer(cols = Bangladesh.Colombia:Viet.Nam.Thailand) %>% 
  filter(is.na(value)==FALSE) %>% 
  arrange(value)
vars_pf_country <- setdiff(unique(Pf_country_loci$value), "STRLoci")
geno_pf_country <- geno_origin_Pf
geno_pf_country <- geno_pf_country %>% 
  filter(STRid %in% as.character(Pf_country_loci$STRid))

STR_Position_pf_country <- Pf_country_loci %>% 
  dplyr::select(STRid,value)

geno_pf_country <- STR_Position_pf_country %>% 
  left_join(geno_pf_country,by="STRid")
geno_pf_country <- geno_pf_country[,-1]
geno_pf_country$value <- as.character(geno_pf_country$value)
geno_pf_country <- as.data.frame(geno_pf_country)
rownames(geno_pf_country) <- geno_pf_country$value
geno_pf_country <- geno_pf_country[,-1]
geno_pf_country <- t(geno_pf_country)
geno_pf_country <- data.frame(Sample=rownames(geno_pf_country),geno_pf_country)
pop_pf_country <- read.csv("Pf_Sample.csv")
colnames(pop_pf_country)[1] <- "Sample"
pop_pf_country$Country <- as.character(pop_pf_country$Country)
pop_pf_country$Country[pop_pf_country$Country=="Papua New Guinea"] <- "PapuaNewGuinea"
pop_pf_country$Country[pop_pf_country$Country=="Viet Nam"] <- "VietNam"
pop_pf_country$Country[pop_pf_country$Country=="Ivory Coast"] <- "IvoryCoast"
pop_pf_country$Country[pop_pf_country$Country=="Congo DR"] <- "CongoDR"

geno_pf_country <- geno_pf_country %>% 
  left_join(pop_pf_country,by="Sample")
vars_pf_country <- gsub(vars_pf_country,pattern="-",replacement=".")

#Pv country
geno_pv <- read.csv("Pv_Genotype.csv")
geno_origin_Pv <- geno_pv
Pv_country_loci <- read.csv("Pv_STR_Top_ShinyPlot.csv")
Pv_country_loci <- good_pv %>% 
  filter(STRid %in% as.character(Pv_country_loci$STRid))
Pv_country_loci <- Pv_country_loci %>% 
  pivot_longer(cols = Cambodia.Colombia:Thailand.Peru) %>% 
  filter(is.na(value)==FALSE) %>% 
  arrange(value)
vars_pv_country <- setdiff(unique(Pv_country_loci$value), "STRLoci")
geno_pv_country <- geno_origin_Pv
geno_pv_country <- geno_pv_country %>% 
  filter(STRid %in% as.character(Pv_country_loci$STRid))

STR_Position_pv_country <- Pv_country_loci %>% 
  dplyr::select(STRid,value)

geno_pv_country <- STR_Position_pv_country %>% 
  left_join(geno_pv_country,by="STRid")
geno_pv_country <- geno_pv_country[,-1]
geno_pv_country$value <- as.character(geno_pv_country$value)
geno_pv_country <- as.data.frame(geno_pv_country)
rownames(geno_pv_country) <- geno_pv_country$value
geno_pv_country <- geno_pv_country[,-1]
geno_pv_country <- t(geno_pv_country)
geno_pv_country <- data.frame(Sample=rownames(geno_pv_country),geno_pv_country)
pop_pv_country <- read.csv("Pv_Sample.csv")
colnames(pop_pv_country)[1] <- "Sample"
pop_pv_country <- pop_pv_country[,c(1,3)]
pop_pv_country$Country <- as.character(pop_pv_country$Country)

geno_pv_country <- geno_pv_country %>% 
  left_join(pop_pv_country,by="Sample")
vars_pv_country <- gsub(vars_pv_country,pattern="-",replacement=".")



#navil panel
ui <- fluidPage(
  # App title ----
  titlePanel("Population-level genome-wide high-quality STRs in Plasmodium falciparum and Plasmodium vivax"),
  
  # Sidebar layout with input and output definitions ----
  
  navlistPanel(
    tabPanel("Directions",
             h4("Introduction"),
             p("We genotype STRs using HipSTR in more than 3,000 P. falciparum and 174 P. vivax whole- genome sequence worldwide samples. We develop a multivariable logistic regression model for the measurement and prediction of the quality of STRs. A set of high-quality STR loci (6,768 from P. falciparum and 3,496 from P. vivax) were selected."),
             br(),
             HTML("<ol><li>Genome-wide high-quality STRs (6,768 from P. falciparum and 3,496 from P. vivax)</li><li>The top ten most highly differentiated STRs from each pairwise population or country</li><li>Samples corresponding genotypes distribution of the top ten most highly differentiated STRs over different population or country pair</li></ol>"),
             br(),
             p(strong("Genome-wide high-quality STRs.",style = "color:#337ab7"), "Choose the dataset (Pf or Pv) and the STR genomic location (All, Coding, Promoter, Intergenic, Intron, or Other) to access the high-quality STR loci information."),
             p(strong("The top ten most highly differentiated STRs.",style = "color:#337ab7"), "Choose the P. falciparum population pair, P. falciparum country pair, or P. vivax country pair to access the top ten most highly differentiated STRs between this population or country pairs. To explore the samples corresponding genotypes plots of these highly differentiated STRs, please use the end columns values to select each locus."),
             p(strong("Samples corresponding genotypes plots.",style = "color:#337ab7"), "Plot samples corresponding genotypes distribution of each highly differentiated STR locus over different population or country pair.")),
    tabPanel("1. Genome-wide high-quality STRs",
             fluidRow(column(12,
                             wellPanel(
                               helpText("Genome-wide high-quality STRs (6,768 from P. falciparum and 3,496 from P. vivax)"),
                               hr(),
                               selectInput(inputId = "dataset",
                                           label = "Please choose a dataset:",
                                           choices = c("","Pf", "Pv")),
                               
                               selectInput(inputId = "STRcategory",
                                           label = "Please choose a genomic location:",
                                           choices = c("","All","Coding", "Promoter","Intergenic","Intron", "Other")),
                               numericInput(inputId = "obs",
                                            label = "Number of observations to view:",
                                            value = 10),
                               actionButton("View","View Table")))),
             fluidRow(column(12,
                             DT::dataTableOutput("view")))), 
    
    tabPanel("2. The top ten most highly differentiated STRs",
             fluidRow(column(12,
                             wellPanel(#Pf population
                               selectInput(inputId = "PfPopulationPair",
                                           label = "Please choose a P. falciparum population pair:",
                                           choices = c("",compare_Pf_Population)),
                               #Pf country
                               selectInput(inputId = "PfCountrypair",
                                          label = "Please choose a P. falciparum country pair:",
                                           choices = c("",compare_Pf_Country)),
                               
                               #selectizeInput(inputId = "PfCountrypair",
                               #               label = "Please choose a P. falciparum country pair:",
                               #               choices = c("",compare_Pf_Country),
                              #                options = list(maxOptions=3500)),
                               
                               #Pv country
                               selectInput(inputId = "PvCountrypair",
                                           label = "Please choose a P. vivax country pair:",
                                           choices = c("",compare_Pv_Country)),
                               actionButton("View2","View Table"))),
                      fluidRow(column(12,
                                      DT::dataTableOutput("view1"),
                                      DT::dataTableOutput("view2"),
                                      DT::dataTableOutput("view3"))))), 
  
      tabPanel("3. Samples corresponding genotypes plots",
                      fluidRow(column(12,
                                      wellPanel(
                                        selectInput(inputId = 'STRLociPfPopulation', 
                                                    label ='Top ten most highly differentiated STRs from each pairwise population(P. falciparum)', 
                                                    choices = c("",vars_pf_population)),
                                        
                                        selectInput(inputId = 'xcol', 
                                                    label ='Please choose the plot color for P. falciparum (Population or Country)', 
                                                    choices = c("Population","Country")),
                                        
                                        actionButton("Plot1","Plot P. falciparum population"),
                                        
                                        selectInput(inputId = 'STRLociPfCountry', 
                                                    label ='Top ten most highly differentiated STRs from each pairwise country(P. falciparum)', 
                                                    choices = c("",vars_pf_country)),
                                        
                                        actionButton("Plot2","Plot P. falciparum country"),
                                      
                                        
                                        selectInput(inputId = 'STRLociPvCountry', 
                                                    label ='Top ten most highly differentiated STRs from each pairwise country(P. vivax)', 
                                                    choices = c("",vars_pv_country)),
                                        
                                        actionButton("Plot3","Plot P. vivax country")))),
                      fluidRow(column(12,
                                      plotOutput('plot1'),
                                      plotOutput('plot2'),
                                      plotOutput('plot3'))))
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  Pf = good_pf %>% 
    dplyr::select(Gene,Genomic.Location,Product.Description,Category)
  Pv = good_pv %>% 
    dplyr::select(Gene,Genomic.Location,Product.Description,Category)
  datasetInput <- eventReactive(input$View, {
    if (input$dataset == "Pf" & input$STRcategory == "All") {
      Pf
    } else if (input$dataset == "Pf" & input$STRcategory == "Coding") {
      Pf %>% 
        filter(Category=="coding")
    } else if (input$dataset == "Pf" & input$STRcategory == "Promoter") {
      Pf %>% 
        filter(Category=="promoter")
    } else if (input$dataset == "Pf" & input$STRcategory == "Intergenic") {
      Pf %>% 
        filter(Category=="intergenic")
    } else if (input$dataset == "Pf" & input$STRcategory == "Intron") {
      Pf %>% 
        filter(Category=="intron")
    } else if (input$dataset == "Pf" & input$STRcategory == "Other") {
      Pf %>% 
        filter(!(Category %in% c("coding", "promoter","intergenic","intron")))
    } else if (input$dataset == "Pv" & input$STRcategory == "All") {
      Pv
    } else if (input$dataset == "Pv" & input$STRcategory == "Coding") {
      Pv %>% 
        filter(Category=="coding")
    } else if (input$dataset == "Pv" & input$STRcategory == "Promoter") {
      Pv %>% 
        filter(Category=="promoter")
    } else if (input$dataset == "Pv" & input$STRcategory == "Intergenic") {
      Pv %>% 
        filter(Category=="intergenic")
    } else if (input$dataset == "Pv" & input$STRcategory == "Intron") {
      Pv %>% 
        filter(Category=="intron")
    } else if (input$dataset == "Pv" & input$STRcategory == "Other") {
      Pv %>% 
        filter(!(Category %in% c("coding", "promoter","intergenic","intron")))
    } 
  })
  
  #Pf population pair
  datasetInput2 <- eventReactive(input$View2,{
    if (input$PfPopulationPair != "") {
      good_pf %>% 
        dplyr::filter(is.na(good_pf[,which(colnames(good_pf)==input$PfPopulationPair)])==FALSE) %>% 
        dplyr::select(Gene,Genomic.Location,Product.Description,Category,input$PfPopulationPair)
    } else {
      stop()
    }
    
  })
  
  #Pf country pair
  datasetInput3 <- eventReactive(input$View2,{
    if (input$PfCountrypair != "") {
      good_pf %>% 
        dplyr::filter(is.na(good_pf[,which(colnames(good_pf)==input$PfCountrypair)])==FALSE) %>% 
        dplyr::select(Gene,Genomic.Location,Product.Description,Category,input$PfCountrypair)
    } else {
      stop()
    }
    
  })
  
  #Pv country pair
  datasetInput4 <- eventReactive(input$View2,{
    if (input$PvCountrypair != "") {
      good_pv %>% 
        dplyr::filter(is.na(good_pv[,which(colnames(good_pv)==input$PvCountrypair)])==FALSE) %>% 
        dplyr::select(Gene,Genomic.Location,Product.Description,Category,input$PvCountrypair)
    } else {
      stop()
    }
    
    
    
    
  })
  
  
  plot1 <- eventReactive(input$Plot1,{
    geno_pf_population %>% 
      filter(is.na(geno_pf_population[,which(colnames(geno_pf_population)==input$STRLociPfPopulation)])==FALSE)  %>% 
      filter(Population %in% unlist(strsplit(gsub('[[:digit:]]+', '', input$STRLociPfPopulation),"\\."))) %>% 
      ggplot(aes_string(x=input$xcol,y=input$STRLociPfPopulation,col=input$xcol))+
      geom_jitter(alpha=0.5,height = 0.1)+
      theme_bw()+
      labs(title = "Genotype of pairwise population samples in highly differentiated STR (P. falciparum)",
           x="Pairwise population",
           y="Genotype")
  })
  
  output$plot1 <- renderPlot({
    plot1()
  })
  
  plot2 <- eventReactive(input$Plot2, {
    
    geno_pf_country %>% 
      filter(is.na(geno_pf_country[,which(colnames(geno_pf_country)==input$STRLociPfCountry)])==FALSE) %>% 
      filter(Country %in% unlist(strsplit(gsub('[[:digit:]]+', '', input$STRLociPfCountry),"\\."))) %>% 
      ggplot(aes_string(x=input$xcol,y=input$STRLociPfCountry,col=input$xcol))+
      geom_jitter(alpha=0.5,height = 0.1)+
      theme_bw()+
      labs(title = "Genotype of pairwise country samples in highly differentiated STR (P. falciparum)",
           x="Pairwise country",
           y="Genotype")
  })
  
  output$plot2 <- renderPlot({
    plot2()
  })
  
  
  plot3 <- eventReactive(input$Plot3, {
    geno_pv_country %>% 
      filter(is.na(geno_pv_country[,which(colnames(geno_pv_country)==input$STRLociPvCountry)])==FALSE) %>% 
      filter(Country %in% unlist(strsplit(gsub('[[:digit:]]+', '', input$STRLociPvCountry),"\\."))) %>% 
      filter(is.na(input$STRLociPvCountry)==FALSE) %>% 
      ggplot(aes_string(x="Country",y=input$STRLociPvCountry,col="Country"))+
      geom_jitter(alpha=0.5,height = 0.1)+
      theme_bw()+
      labs(title = "Genotype of pairwise country samples in highly differentiated STR (P. vivax)",
           x="Pairwise country",
           y="Genotype")
  })
  
  output$plot3 <- renderPlot({
    plot3()
  })
  
  
  # Show the first "n" observations ----
  output$view <- DT::renderDataTable(
    DT::datatable(datasetInput(),caption = 'Table: This is genome-wide high-quality STR loci table.')
  )
  
  output$view1 <- DT::renderDataTable(
    DT::datatable(datasetInput2(),caption = 'Table: This is top 10 most highly differentitated STR loci table (P. falciparum population pair).')
  )
  
  output$view2 <- DT::renderDataTable(
    DT::datatable(datasetInput3(),caption = 'Table: This is top 10 most highly differentitated STR loci table (P. falciparum country pair).')
  )
  
  output$view3 <- DT::renderDataTable(
    DT::datatable(datasetInput4(),caption = 'Table: This is top 10 most highly differentitated STR loci table (P. vivax country pair).')
  )
}

# Create Shiny app ----
shinyApp(ui, server)



