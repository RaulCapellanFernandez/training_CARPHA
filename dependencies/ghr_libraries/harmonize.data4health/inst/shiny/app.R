# ---------------------------
#
# Script name: Health Data Training App
#
# Purpose of script: creates a shiny app, where health data can be loaded, 
# cleaned, agregated and visualized
# Author: Daniela Lührsen
# Date Created: 2023-03
# Email: daniela.luhrsen@bsc.es
#
# ---------------------------

# Load the packages

packages <- c("dplyr", "DT", "foreign",  "ggplot2","lubridate", "leaflet",# "read.dbc",
               "shinyWidgets","shiny", "sf", "zoo","read.dbc",
              "plotly")#"shinycssloaders", 
# devtools::install_github("danicat/read.dbc")
# install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")
lapply(packages, require, character.only = TRUE)

source("app_support_functions.R")
options(shiny.maxRequestSize=200*1024^2) 
options(repos = c(CRAN = "https://cloud.r-project.org"))


# ---------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
  style="padding-right: 10%; padding-left: 10%;",
  tags$style(HTML("
  body {
	  font-family: 'Roboto', Helvetica, Arial, sans-serif;
	  color: #525252;
  }
  h1, h2, h3, h4, h5, h6 {
    font-family: 'Roboto', Helvetica, Arial, sans-serif;
    font-weight: 500;
    line-height: 1.1;
    color: #333333;
  } 
  p {
	  text-align: justify;
	  font-family: 'Roboto', Helvetica, Arial, sans-serif;
	  color: #525252;
  }
  .navbar-brand{
    display: flex;
    align-items: center;
  }
  .dropdown-content {
    display: none;
    position: absolute;
    background-color: #f9f9f9;
    min-width: 160px;
    box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
    z-index: 1;
  }
  .dropdown:hover .dropdown-content {
    display: block;
  }
  #editor {
  background-color: #f0f0f0; /* Change background color */
    color: #333; /* Change font color */
    font-family: Arial, sans-serif; /* Change font family */
    font-size: 14px; /* Change font size */
    }
")),
  headerPanel(
    img(src = "logo.png", height = "75px"),
   "Health Data Tool"
  ),
  # About ###########################
  tabsetPanel(
    id="tabs", 
    type="tabs",
    # About --------------------------------------------------------------------
    navbarMenu(
      "About",
      tabPanel(
      # About the tool -------------------------------------------------------
        "About this tool",
        tags$br(),
        p("This tool was developed within the HARMONIZE project. It aims to streamline health data analysis for researchers and decision makers wihthout the need of coding. It contains the following steps:"),
        # fluidRow(
        #   style="padding-left: 4%;",
        #   div(tags$b("1. Access data:"),"	In the case of publicly available data,..... And in the case of restricted data, the information on who to contact will be given."),
        #   div(tags$b("2. Preprocess:"),"	Cleaning the data of inconsistencies and doing quality analysis"),
        #   div(tags$b("3. Load:"),"  In case you already have the data prepared, load it directly in."),
        #   div(tags$b("4. Aggregate:"),"			Aggregate it."),
        #   div(tags$b("5. Visualise:")," 			Visualise a a timeseries or as a table")
        # ),
        tags$div(
          style = "text-align: center;",
          tags$img(src="flowchart.png", style = "max-width: 80%; height: auto;")
        ),
        tags$br(),
        p("You can input data at any step and also download the result of any step."),
        p("The 'access data' is currently available for Brazil and will be available for Peru, Colombia and Dominican Republic in the near future. All other steps can be performed for any data as long as it has the correct data format.")
      ),
      # About the project ------------------------------------------------------
      tabPanel(
        "About the project",
        tags$br(),
        tags$div(
          style = "border: 2px solid #E296B5; border-radius: 10px; padding: 10px; background-color: #E296B5;",
          p("HARMONIZE will develop cost-effective and reproducible digital tools for stakeholders in hotspots affected by a changing climate in Latin America & the Caribbean (LAC), including cities, small islands, highlands, and the Amazon rainforest.", style = "color: white; font-weight: bold; font-size: 16px;")
        ),
        tags$br(),
        p("Extreme climatic events, environmental degradation and socio-economic inequalities exacerbate the risk of infectious disease epidemics. However, there are significant challenges in obtaining science-based evidence to understand and predict the impacts of extreme events and landscape changes on disease risk, leaving communities in climate change hotspots, vulnerable to increasing health threats. This is partly due to a lack of longitudinal and localised data, describing fine-scale environmental changes in remote and under-resourced areas, a lack of investment and capacity building in data science and software tools, and a lack of trained research software engineers and data scientists. The HARMONIZE project convenes a transdisciplinary community of practitioners, community-based organisations, local governments, software  engineers and data scientists to address these challenges. "),
        p("The main goal of the project is to develop cost-effective and reproducible digital tools for decision-makers in climate change hotspots in Latin America & the Caribbean, including cities, small islands, highlands, and the Amazon rainforest. HARMONIZE will collate existing multi-source climate, environmental, socio-economic and health data, and collect new longitudinal ground-truth data using drone technology and low-cost weather sensors, to calibrate and downscale coarser-resolution Earth Observation, climate reanalysis and forecast datasets in areas most relevant for disease transmission. The HARMONIZE digital toolkits will allow local researchers and users, including national disease control programs, to link, interrogate and use multi-scale spatiotemporal data, to understand the links between environmental change and infectious disease risk in their local context, and to build robust early warning and response systems in low-resource settings."),
        tags$div(
          style = "text-align: center;",
          tags$img(src="harmonize_banner.png", style = "max-width: 80%; height: auto;")
        ),
        tags$br(),
        p("At the core of HARMONIZE is the digital infrastructure, which collects and post-processes different data streams to create harmonised datasets across different spatiotemporal scales and communities of practice. Existing datasets, including global Earth observations from satellite images and gridded climate reanalysis, and forecast data, will be combined with new data collected from drones and weather sensors during the project, to improve classification, bias-correction and downscaling of existing products. This will be merged with socio-economic, demographic and health system data (i.e., census, micro survey, and health service data) and disease surveillance data (e.g., case reports for dengue, chikungunya, Zika, malaria, leptospirosis, Chagas, etc.) collected via online platforms and provided by public health practitioners. From this core digital infrastructure, digital toolkits will be designed for key decision-makers operating across a range of hotspot, including cities, the Amazon rainforest, highland areas, and small islands. This new knowledge and technology will have lasting benefits for the wider community of practice and generate new evidence to design strategies to protect local communities from the adverse health impacts of a changing climate."),
        p(HTML(paste0("For more information about the project please visit the ", tags$a(" HARMONIZE webpage", href="https://www.harmonize-tools.org/", target="_blank"))))
      ),
      # Documentation #############
      tabPanel(
        "Documentation",
        conditionalPanel(
          condition = "output.document_page == 'start'",
          h4("Documentation"),
          h5("Examples for cleaning"),
          p("Here you can find examples on how to interpret and clean health data for different countries and diseases."),
          tags$b("Brazil:"),
          tags$ul(
            tags$li(actionLink("go_dengue","Dengue")),
            tags$li(actionLink("go_malaria","Malaria")),
            tags$li("Leptospirosis")

          ),
          tags$br(),
          h5("Dictionary"),
          p("The dictionaries are the key to understanding the health data sets. They contain an explanation on what variable each column contains and, for categorical variables, also explains what each category means."),
          tags$b("Brazil"),
          tags$ul(
            tags$li(actionLink("download_br_dengue_dictionary","Dengue")),
            tags$li(actionLink("download_br_dengue_dictionary", "Malaria"))
          ),
          tags$b("Colombia"),
          tags$ul(
            tags$li("Dengue"),
            tags$li("Malaria")
          ),
          tags$b("Dominican Republic"),
          tags$ul(
            tags$li("Dengue"),
            tags$li("Malaria")
          ),
          tags$b("Peru"),
          tags$ul(
            tags$li("Dengue"),
            tags$li("Malaria")
          )
        ),
        conditionalPanel(
          condition = "output.document_page == 'br_dengue'",
          actionLink("go_brdengue_start","Go back to overview"),
          "Coming soon!"
        ),
        conditionalPanel(
          condition = "output.document_page == 'br_malaria'",
          tags$br(),
          actionLink("go_brmalaria_start","Go back to overview"),
          tags$br(),

          p("This document aims at introducing dengue data to someone with no previous experience. Before starting it is necessary to have the health data, instructions on how to obtain these are in the ReadMe of this project."),
          p("The steps include:"),
          tags$ol(
            tags$li("Downloading the data"),
            tags$li("Getting to know the data: variables and completeness"),
            tags$li("Information on the patients: Age, Sex, Pregnancy and Race"),
            tags$li("Classifying the malaria cases: detection and symptoms"),
            tags$li("Timeline of malaria cases: distribution throughout the year, and course of the cases"),
            tags$li("Mapping spatially: municipality types, malaria types and incidences")
          ),
#           shinyAce::aceEditor(
#             "brasil_malaria_1", mode="r", theme="tomorrow", readOnly=TRUE, height="160",
#             value= "# load packages
# packages  <- c('microdatasus', 'dplyr', 'purrr', 'ggplot2','tidyr',
#                'lubridate','geobr', 'stringr')
# invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
# invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))

# # other options
# options(scipen = 999)
# options(timeout = 900)

# # load functions
# files.sources <-  list.files('R/')
# invisible(sapply(paste0('R/', files.sources), source))"
#           ),
          h3("Downloading the data"),
          tags$hr(),
          p("As described in the 0_DownloadData file, the health data from Brazil can be downloaded in many different ways. Here, it is downloaded with the help of the package microdatasus. The following code block will download the data for the year 2014 (which was chosen randomly)."),
        #   shinyAce::aceEditor(
        #     "brasil_malaria_2", mode="r", theme="tomorrow", readOnly=TRUE, height="13",
        #     value= "malariaData <- fetch_datasus(year_start = 2014, year_end = 2014, information_system = 'SINAN-MALARIA')"
        #   ),
          h3("Getting to know the Data"),
          h5("Variables"),
          p("There are a total of 50 variables. The variables names can be seen below:"),
#           shinyAce::aceEditor(
#             "brasil_malaria_3", mode="r", theme="tomorrow", readOnly=TRUE, height="13",
#             value= "colnames(malariaData)"
#           ),
#           shinyAce::aceEditor(
#             "brasil_malaria_4", mode="r", theme="tomorrow", readOnly=TRUE, height="130",
#             value= "##  [1] 'TP_NOT'     'ID_AGRAVO'  'DT_NOTIFIC' 'SEM_NOT'    'NU_ANO'
# ##  [6] 'SG_UF_NOT'  'ID_MUNICIP' 'ID_REGIONA' 'ID_UNIDADE' 'DT_SIN_PRI'
# ## [11] 'SEM_PRI'    'ANO_NASC'   'NU_IDADE_N' 'CS_SEXO'    'CS_GESTANT'
# ## [16] 'CS_RACA'    'CS_ESCOL_N' 'SG_UF'      'ID_MN_RESI' 'ID_RG_RESI'
# ## [21] 'ID_PAIS'    'DT_INVEST'  'ID_OCUPA_N' 'CLASSI_FIN' 'AT_ATIVIDA'
# ## [26] 'AT_LAMINA'  'AT_SINTOMA' 'TPAUTOCTO'  'COUFINF'    'COPAISINF'
# ## [31] 'COMUNINF'   'LOC_INF'    'DEXAME'     'RESULT'     'PMM'
# ## [36] 'PCRUZ'      'TRA_ESQUEM' 'DSTRAESQUE' 'DTRATA'     'NU_LOTE_I'
# ## [41] 'DT_ENCERRA' 'DT_DIGITA'  'DT_TRANSUS' 'DT_TRANSDM' 'DT_TRANSSM'
# ## [46] 'DT_TRANSRM' 'DT_TRANSRS' 'DT_TRANSSE' 'NU_LOTE_V'  'NU_LOTE_H'"
#           ),
          p(HTML(paste0("The definition of each variable can be found in the ",
                        tags$a("official documentation", href="Workspace/Dictionary_Malaria_PT.pdf", target="_blank")," or ", tags$a("online", href="http://portalsinan.saude.gov.br/images/documentos/Agravos/Malaria/DIC_DADOS_Malaria_v5.pdf",target="_blank"), " in Portuguese, or in this ", tags$a("translated csv file", href="Workspace/Dictionary_Malaria_EN.csv", target="_blank"),". This file also contains the decodification for all numerical categories. A small overview of this file can be seen below:"))),
#           shinyAce::aceEditor(
#             "brasil_malaria_5", mode="r", theme="tomorrow", readOnly=TRUE, height="36",
#             value= "malariaVarDefPath <- file.path(getwd(), 'Workspace', 'Dictionary_Malaria_EN.csv')
# malariaVarDef <- read.csv(malariaVarDefPath)
# knitr::kable(malariaVarDef[14:20,1:5])"
#           ),
          ### table
          tags$img(src="filter.svg",width="20"),
          p("To avoid working with unnecessary data, it is useful to filter out the variables that will not be used. Below is a line of code on how to select only the needed variables."),
#           shinyAce::aceEditor(
#             "brasil_malaria_6", mode="r", theme="tomorrow", readOnly=TRUE, height="24",
#             value= "sel_var <- c('NU_IDADE_N','CS_SEXO', 'CS_GESTANT', 'CS_RACA', 'AT_LAMINA', 'AT_SINTOMA', 'DT_NOTIFIC', 'DT_SIN_PRI', 'DT_DIGITA', 'DEXAME', 'DTRATA', 'ID_MUNICIP',  'ID_MN_RESI',  'COMUNINF','RESULT')
# malariaData <- malariaData %>% select(all_of(sel_var))"
#           ),
          h3("Completeness of the data"),
          p("Some of these variables are mandatory to notify, others are not, which leads to incomplete datasets. The chart ", tags$b("NACountChart"), "  shows the the variables which have NA-values and the percentage of NAs they contain."),
#           shinyAce::aceEditor(
#             "brasil_malaria_7", mode="r", theme="tomorrow", readOnly=TRUE, height="125",
#             value= "# get count of NA for each variable
# NACount <- malariaData %>% map(~mean(is.na(.)))
# NACount <- data.frame(count=unlist(NACount),
#                       var=names(NACount))

# ggplot(NACount[NACount$count != 0, ], aes(x = reorder(var, count), y = count)) +
#   geom_bar(stat = 'identity', width = 0.9, position = position_dodge(width = 5)) +
#   ylab('Percentage of NAs in each column') +
#   xlab('Variables') +
#   coord_flip()"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_NAs-1.png"),
          p("On the contrary, it is also good to know what variables are complete."),
        #   shinyAce::aceEditor(
        #     "brasil_malaria_8", mode="r", theme="tomorrow", readOnly=TRUE, height="12",
        #     value= "NACount$var[NACount$count == 0]"
        #   ),
          ### result
          tags$img(src="filter.svg",width="20"),
          p("To filter out variables which are (almost) completely empty, you can run the following lines."),
#           shinyAce::aceEditor(
#             "brasil_malaria_9", mode="r", theme="tomorrow", readOnly=TRUE, height="24",
#             value= "drop_variables <- NACount$var[NACount$count == 1]
# malariaData <- malariaData[, !(names(malariaData) %in% drop_variables)]"
#           ),
          h3("Exam Results"),
          p("The database distinguishes 10 different types/combinations of malaria:"),
          tags$ul(
            tags$li("Negative"),
            tags$li(tags$i("Falciparum")),
            tags$li(tags$i("F+Fg")),
            tags$li(tags$i("Vivax")),
            tags$li(tags$i("F+V")),
            tags$li(tags$i("V+Fg")),
            tags$li(tags$i("Fg")),
            tags$li(tags$i("Malariae")),
            tags$li(tags$i("F+M")),
            tags$li(tags$i("Ovale"))
          ),
          p("The ", tags$b("negative"), "  notification needs to be filtered (excluded) to obtain the total malaria cases because they are not cases as the exam result was negative. Also, we should exclude NAs."),
#           shinyAce::aceEditor(
#             "brasil_malaria_10", mode="r", theme="tomorrow", readOnly=TRUE, height="255",
#             value= "malariaData <- malariaData %>%
#   mutate(RESULT = as.factor(case_when(RESULT == '1' ~ 'Negative',
#                                       RESULT == '2' ~ 'Falciparum',
#                                       RESULT == '3' ~ 'F+Fg',
#                                       RESULT == '4' ~ 'Vivax',
#                                       RESULT == '5' ~ 'F+V',
#                                       RESULT == '6' ~ 'V+Fg',
#                                       RESULT == '7' ~ 'Fg',
#                                       RESULT == '8' ~ 'Malariae',
#                                       RESULT == '9' ~ 'F+M',
#                                       RESULT == '10' ~ 'Ovale')))

# ggplot(malariaData, aes(x = RESULT, fill=RESULT)) +
#   geom_bar() +
#   annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4,
#            label = paste0(sum(is.na(malariaData$RESULT)), ' NA values'))+
#   labs(title='Exam results during malaria cases in 2014',
#        y='Count', x='Exam Result', fill='Exam Results') +
#   geom_text(stat = 'count', position = position_stack(vjust = 0.5),
#             aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), '%'),
#                 x = RESULT))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Type-1.png"),
          p("However, generally, people only distinguish in 3 categories: ", tags$i("P. vivax"), "  and ", tags$i("P. falciparum"), "  and the mixed version ", tags$i("P+V"), " ."),
#           shinyAce::aceEditor(
#             "brasil_malaria_11", mode="r", theme="tomorrow", readOnly=TRUE, height="205",
#             value= "malariaData <- malariaData %>%
#   mutate(RES_EXAM = as.factor(case_when(RESULT == 'Falciparum' | RESULT == 'F+Fg' | RESULT == 'Fg' | RESULT == 'F+M' ~ 'F',
#                                         RESULT == 'Vivax' | RESULT == 'V+Fg' ~ 'V',
#                                         RESULT == 'F+V'  ~  'V+F',
#                                         RESULT == 'Malariae' | RESULT == 'Ovale' ~ 'other',
#                                         RESULT == 'Negative' ~ 'Negative')))

# ggplot(malariaData, aes(x = RES_EXAM, fill=RES_EXAM)) +
#   geom_bar() +
#   labs(title='Exam results during malaria cases in 2014',
#        y='Count', x='Exam Result', fill='Exam Results') +
#   annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4,
#            label = paste0(sum(is.na(malariaData$RES_EXAM)), ' NA values'))+
#   geom_text(stat = 'count', position = position_stack(vjust = 0.5),
#             aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), '%'),
#                 x = RES_EXAM))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/unnamed-chunk-2-1.png"),
          p(HTML(paste0("The majority of case are ", tags$i("Vivax")," (2.9%), followed by ", tags$i("Falciparum")," (2.9%). All other categories together make 100%. Fg is the gametocyte phase of the ", tags$i("P. falciparum")," life cycle."))),
          p(HTML(paste0("Below, the distribution of the most prevalent malaria can bee seen throughout the year on an epiweek time resolution. Note the ", tags$i("vivax")," and ", tags$i("falciparum")," cases are the combination between more than one ", tags$b("RESULT")," category."))),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_TypeTimeseries-1.png"),
          tags$img(src="filter.svg",width="20"),
          p("To exclude cases with a negative test result, you can run the following lines"),
        #   shinyAce::aceEditor(
        #     "brasil_malaria_12", mode="r", theme="tomorrow", readOnly=TRUE, height="12",
        #     value= "malariaData <- malariaData[malariaData$RESULT != 'Negative',]"
        #   ),
          h3("Information on the Patients"),
          h5("Age of patients"),
          p(HTML(paste0("For malaria, the age of the patients can be derived from ", tags$b("NU_IDADE_N"),". ", tags$b("NU_IDADE_N"), " is codified:"))),
          tags$ol(
            tags$li("the first number indicates what “dimension” it is using. 1= Hour, 2= day, 3= month, 4= year."),
            tags$li("the last numbers indicate the age.")
          ),
          p("The age can be “decodified” with the command line below. This line of code also automatically removes all inconsistent ages, i.e. people over 120 years and people whose age was given in a unusual way (e.g. 84 months)."),
#           shinyAce::aceEditor(
#             "brasil_malaria_13", mode="r", theme="tomorrow", readOnly=TRUE, height="200",
#             value= "malariaData  <- malariaData %>%
#   mutate(AGE = case_when(NU_IDADE_N < 120 ~ as.numeric(NU_IDADE_N), # assumes that non-codified data means that the age in years was given
#                            NU_IDADE_N >= 120 & NU_IDADE_N < 1000 ~ NA_real_,
#                            NU_IDADE_N >= 1000 & NU_IDADE_N < 2366 ~ 0,
#                            NU_IDADE_N >= 2366 & NU_IDADE_N < 3000 ~ NA_real_,
#                            NU_IDADE_N >= 3000 & NU_IDADE_N < 3013 ~ 0,
#                            NU_IDADE_N >= 3013 & NU_IDADE_N < 4000 ~ NA_real_,
#                            NU_IDADE_N >= 4000 & NU_IDADE_N < 4120 ~ as.numeric(NU_IDADE_N - 4000),
#                            NU_IDADE_N >= 4120 ~ NA_real_,
#                            TRUE ~ NA_real_))

# ggplot(malariaData, aes(x = AGE))+
#   geom_histogram(binwidth = 1, fill = 'lightblue',color='black', alpha = 0.7)+
#   annotate('text', x = 100, y = Inf, label = paste0(sum(is.na(malariaData$AGE)), ' NA values'), hjust = 1, vjust = 1, size = 4)+
#   labs(x='Age [years]', y='Count', title='Age distribution')+
#   theme_light()"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Age-1.png"),
          tags$img(src="filter.svg",width="20"),
          p("Other datasets may include a date of birth, if provided, it can be used to corroborate the real age."),
          h3("Sex of the patients"),
          p(HTML(paste0("The sex of the patients is given by ", tags$b("CS_SEXO"),". A table and a barplot of its distribution can be seen below:"))),
#           shinyAce::aceEditor(
#             "brasil_malaria_14", mode="r", theme="tomorrow", readOnly=TRUE, height="180",
#             value= "malariaData <- malariaData %>%
#   mutate(CS_SEXO = as.factor(case_when(CS_SEXO == 'F' ~ 'Female',
#                                        CS_SEXO == 'M' ~ 'Male',
#                                        CS_SEXO == 'I' ~ 'Ignored',
#                                        TRUE ~ 'NA')))

#  ggplot(malariaData, aes(x = CS_SEXO, fill=CS_SEXO)) +
#   geom_bar() +
#   labs(title='Sex distribution of malaria cases in 2015',
#        x='Sex of the patient', y='Count', fill='Sex')+
#   geom_text(stat = 'count', position = position_stack(vjust = 0.5),
#             aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), '%'),
#                 x = CS_SEXO)) +
#   theme_light()"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Sex%20plot-1.png"),
          h5("Pregnancy"),
          p(HTML(paste0("Another related variable is ", tags$b("CS_GESTANT"),", which indicates whether the patient is pregnant. Since only women below a certain age can get pregnant this is another way to check for inconsistencies."))),
#           shinyAce::aceEditor(
#             "brasil_malaria_15", mode="r", theme="tomorrow", readOnly=TRUE, height="140",
#             value= "pregnant <- malariaData %>%
#   filter((CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>%
#   count()

# mpreg <- malariaData %>%
#   filter(CS_SEXO == 'M' & (CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>%
#   count()

# pover50 <- malariaData %>%
#   filter(AGE > 50 & (CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>%
#   count()"
#           ),yAce::aceEditor(
#             "brasil_malaria_15", mode="r", theme="tomorrow", readOnly=TRUE, height="140",
#             value= "pregnant <- malariaData %>%
#   filter((CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>%
#   count()

# mpreg <- malariaData %>%
#   filter(CS_SEXO == 'M' & (CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>%
#   count()

# pover50 <- malariaData %>%
#   filter(AGE > 50 & (CS_GESTANT == 1 | CS_GESTANT == 2 | CS_GESTANT ==3 | CS_GESTANT == 4)) %>%
#   count()"
#           ),
          tags$img(src="filter.svg",width="20"),
          p("Out of 5 pregnant patients, there are 0 pregnant males and 0 pregnant women over the age of 50. Again, depending on the aim of the study, these patients could be excluded using the following line."),
#           shinyAce::aceEditor(
#             "brasil_malaria_16", mode="r", theme="tomorrow", readOnly=TRUE, height="50",
#             value= "malariaData <- malariaData %>%
#   filter(!(CS_GESTANT %in% c('1', '2', '3', '4') & CS_SEXO == 'M') &
#            !(CS_GESTANT %in% c('1', '2', '3', '4') & AGE > 50))"
#           ),
          h5("Race of the patients"),
          p(HTML(paste0("The race of the patient is given by ", tags$b("CS_RACA"),". A barplot of its distribution can be seen below:"))),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Race-1.png"),
          h3("Classifying the malaria cases"),
          p(HTML(paste0("Malaria can be detected either actively (when someone goes to doctor because of symptoms), passively (as protocol when one case is detected, people living in the same place are screened) or through LVC (Lamina de Verificaçao de Cura = cure thick blood smear verification). The variable ", tags$b("AT_LAMINA"),", shows exactly this."))),
#           shinyAce::aceEditor(
#             "brasil_malaria_17", mode="r", theme="tomorrow", readOnly=TRUE, height="180",
#             value= "malariaData <- malariaData %>%
#     mutate(AT_LAMINA = as.factor(case_when(AT_LAMINA == 1 ~ 'Active',
#                                        AT_LAMINA == 2 ~ 'Passive',
#                                        AT_LAMINA == 3 ~ 'LVC',
#                                        TRUE ~ 'NA')))
# ggplot(malariaData, aes(x = AT_LAMINA, fill=AT_LAMINA)) +
#   geom_bar() +
#   labs(title='Type of detections of malaria cases in 2014',
#        y='Count', x='Detection type', fill='Detection type')+
#   geom_text(stat = 'count',
#             aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), '%'),
#                 x = AT_LAMINA),
#             position = position_stack(vjust = 0.5)) +
#   annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4,
#            label = paste0(sum(is.na(malariaData$AT_LAMINA)), ' NA values'))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Detection-1.png"),
          p("As can be seen, most cases (65.9%) are found actively and only 7.3% are passively."),
          h5("Symptoms"),
          p(HTML(paste0("For malaria, the variable ", tags$b("AT_SINTOMA")," makes the distinction whether there are symptoms present or not."))),
#           shinyAce::aceEditor(
#             "brasil_malaria_18", mode="r", theme="tomorrow", readOnly=TRUE, height="160",
#             value= "malariaData %>%
#   mutate(AT_SINTOMA = as.factor(case_when(AT_SINTOMA== '1' ~ 'Yes',
#                                           AT_SINTOMA == '2' ~ 'No',
#                                           TRUE ~ 'NA'))) %>%
#   ggplot(aes(x = AT_SINTOMA, fill=AT_SINTOMA)) +
#   geom_bar() +
#   labs(title='Presence of symptoms during malaria cases in 2014',
#        y='Count', x='Presence of symptoms', fill='Presence of symptoms') +
#   annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4,
#            label = paste0(sum(is.na(malariaData$AT_SINTOMA)), ' NA values'))+
#   geom_text(stat = 'count', position = position_stack(vjust = 0.5),
#             aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), '%'),
#                 x = AT_SINTOMA))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Symptoms-1.png"),
          p("A clear majority of NA% malaria cases had symptoms."),
          h1("Timeline of malaria cases"),
          h3("Notification distribution throughout the year"),
          p(HTML(paste0("In the dataset, you can find many dates, one of the most important might be the notification date ", tags$b("DT_NOTIFIC"),". This is the first time that a potential malaria patient contacts the medical authorities. Below the annual distribution of notifications during 2014 can be seen. There is no clear seasonal variation but tended to concentrate cases during the summer season in Brazil."))),
#           shinyAce::aceEditor(
#             "brasil_malaria_19", mode="r", theme="tomorrow", readOnly=TRUE, height="105",
#             value= "malariaData %>%
#   mutate(EPIWEEK = floor_date(DT_NOTIFIC, 'weeks')) %>%
#   group_by(EPIWEEK) %>%
#   summarise(Count = n()) %>%
#   ggplot() +
#   geom_line(aes(x = EPIWEEK, y = Count)) +
#   scale_x_date(date_labels = '%m-%d', date_breaks = '1 week') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/unnamed-chunk-3-1.png"),
          h3("Timeline of all other events"),
          p(HTML(paste0("First, symptoms occur (", tags$b("DT_SIN_TO_PRI"), " ). Once the doctor notifies a suspected malaria case (", tags$b("DT_NOTIFIC"), " ), it will be digitized (", tags$b("DT_DIGITA"), " ). The malaria case needs to be confirmed through an exam (", tags$b("DEXAME"), "), and if confirmed, treated (", tags$b("DTRATA"), " ). Below are boxplots for each of the events date."))),
#           shinyAce::aceEditor(
#             "brasil_malaria_20", mode="r", theme="tomorrow", readOnly=TRUE, height="180",
#             value= "malariaData <- malariaData %>%
#   mutate(Symptoms = as.numeric(DT_SIN_PRI - DT_NOTIFIC),
#          Digitalization = as.numeric(DT_DIGITA - DT_NOTIFIC),
#          Exam = as.numeric(DEXAME - DT_NOTIFIC),
#          Treatment = as.numeric(DTRATA - DT_NOTIFIC))

# malariaData %>%
#   select(Symptoms, Digitalization, Exam, Treatment) %>%
#   gather(TYPE, DAYS) %>%
#   ggplot(aes(x = TYPE, y = DAYS)) +
#   geom_boxplot() +
#   coord_cartesian(ylim = c(-100, 100)) +
#   xlab('Event') +
#   ylab('Days from the notification day')"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Timeline-1.png"),
          p("The boxplot is able to visualize possible outliers:"),
          tags$ul(
            tags$li("The ", tags$b("first symptoms"), "  occur before the notification date, the vast majority of them in the 10 days prior to it. Symptoms that occur after the notification date or a long time before hte notification date may have to be removed."),
            tags$li("The ", tags$b("treatment"), " , ", tags$b("digitalization"), "  and ", tags$b("exam"), "  dates, must fall on or within the 60 days after the notification date. If they fall outside this range, they can be coonsidered outliers and may be removed.")
          ),
          h1("Mapping spatially"),
          h3("Notification, Residence & Probable Infection"),
          p(HTML(paste0("There is of course also spatial data in the data sets. Before diving in deeper it is necessary to download the shapefiles of Brazil municipalities. This can be done via the ", tags$i("geobr"), "  package. However, the municipality code given by geobr is one digit too long and has to be shortened to 6 digits."))),
          p("In total, the dataset refers to 3 different locations:"),
          tags$ul(
            tags$li("", tags$b("ID_MUNICIP"), "  is the municipality where the case is notified."),
            tags$li("", tags$b("ID_MN_RESI"), "  is the municipality of residence of the patient."),
            tags$li("", tags$b("COMUNINF"), "  is the municipality the patient probably got the infection.")
          ),
#           shinyAce::aceEditor(
#             "brasil_malaria_21", mode="r", theme="tomorrow", readOnly=TRUE, height="70",
#             value= "muni <- read_municipality(code_muni = 'all', year = 2014, showProgress = FALSE) %>%
#   mutate(code_muni = str_sub(code_muni, end = 6)) %>%
#   left_join(as.data.frame(table(malariaData$ID_MUNICIP, dnn = list('code_muni')), responseName = 'noti_muni')) %>%
#   left_join(as.data.frame(table(malariaData$COMUNINF, dnn = list('code_muni')), responseName = 'infec_muni')) %>%
#   left_join(as.data.frame(table(malariaData$ID_MN_RESI, dnn = list('code_muni')), responseName = 'resi_muni'))"
#           ),
          p("Depending on what the aim of the study is, each of these variables can be important."),
#           shinyAce::aceEditor(
#             "brasil_malaria_22", mode="r", theme="tomorrow", readOnly=TRUE, height="115",
#             value= "cols <- c('green', 'yellow','orange','red', 'black') #change these as needed
# breaks <- c(0, 10, 100, 1000, 10000, 100000) #change these as needed

# ggplot(muni) +
#   ggtitle('Extra-Amazon malaria per municipality of notification') +
#   geom_sf( aes(fill = noti_muni), size = .15, show.legend = TRUE) +
#   scale_fill_stepsn(breaks = breaks,
#                     colours = cols,
#                     values = scales::rescale(breaks))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_plot-1.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_23", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "ggplot(muni) +
#   ggtitle('Extra-Amazon malaria per municipality of probable infection') +
#   geom_sf(aes(fill = infec_muni), size = .15, show.legend = TRUE)  +
#   scale_fill_stepsn(breaks = breaks,
#                     colours = cols,
#                     values = scales::rescale(breaks))"
#           ),
#           tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_plot-2.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_24", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "ggplot(muni) +
#   ggtitle('Extra-Amazon malaria per municipality of residence')+
#   geom_sf( aes(fill = resi_muni), size = .15, show.legend = TRUE)  +
#   scale_fill_stepsn(breaks = breaks,
#                     colours = cols,
#                     values = scales::rescale(breaks))"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_plot-3.png"),
          p(HTML(paste0("", tags$b("What did you find after see the maps?"), "  Take a look of the difference between the Extra-Amazon malaria by municipality of notification and probable infection. Almost all malaria notified in the Extra-Amazon region was acquired in the Amazon region. Some people live in the Amazon region were notified in the Extra-Amazon region."))),
          h3("Mapping types of Malaria"),
          p("If one wants to map them distinguishing by type, the following code can be used:"),
#           shinyAce::aceEditor(
#             "brasil_malaria_25", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "muni <- muni %>%
#   left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == 'V'], dnn = list('code_muni')), responseName = 'vivax_muni')) %>%
#   left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == 'F'], dnn = list('code_muni')), responseName = 'falci_muni')) %>%
#   left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == 'V+F'], dnn = list('code_muni')), responseName = 'mixed_muni')) %>%
#   left_join(as.data.frame(table(malariaData$ID_MUNICIP[malariaData$RES_EXAM == 'other'], dnn = list('code_muni')), responseName = 'other_muni'))

# vivax_plot <- ggplot(muni) +
#   geom_sf( aes(fill = vivax_muni), size = .15, show.legend = TRUE) +
#   scale_fill_viridis_c() +
#   labs(subtitle = paste0('Number of P. vivax cases in Brazil 2015'), size = 8) +
#   theme_minimal()
# vivax_plot"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_plotType-1.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_26", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "falci_plot <-  ggplot(muni) +
#   geom_sf( aes(fill = falci_muni), size = .15, show.legend = TRUE) +
#   scale_fill_viridis_c() +
#   labs(subtitle=paste0('Number of P. falciparum cases in Brazil 2015'), size = 8) +
#   theme_minimal()
# falci_plot"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_plotType-2.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_27", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "mixed_plot <-  ggplot(muni) +
#   geom_sf( aes(fill = mixed_muni), size = .15, show.legend = TRUE) +
#   scale_fill_viridis_c() +
#   labs(subtitle=paste0('Number of mixed cases in Brazil 2015'), size = 8) +
#   theme_minimal()
# mixed_plot"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_plotType-3.png"),
          h3("Calculating the incidence"),
          p(HTML(paste0("Sometimes, having the absolute values is helpful. But other times, the number of cases can be highly dependent on the number of inhabitants and then the incidence or other similar indicator is used. Another reason to use incidence is to make comparable the number of cases between areas and different times. Specifically for malaria, the analogous indicator, the Annual Parasite Index (API) is most common. This indicator is calculated by the number of malaria positive patients occurred in a specific time and place divided by the population per 1.000 inhabitants. This positive patients can be a new case or a relapse. Relapses are most common in malaria ", tags$i("vivax"), "  infection."))),
          p(HTML(paste0("For this, the population data needs to be loaded from [IBGE](https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?edicao=31451&t=resultados) either manually or using the local function “get_br_pop_data” (instructions can be viewed in 0_DownloadData.md)."))),
#           shinyAce::aceEditor(
#             "brasil_malaria_28", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "get_br_pop_data(2015, save=F)

# muni <-  left_join(muni, pop_2015[c('code_muni','pop')], by='code_muni')
# muni$pop <- as.numeric(muni$pop)"
#           ),
          p("The incidence is then calculated by dividing the malaria cases by the population of each municipality and multiplication by 1000."),
#           shinyAce::aceEditor(
#             "brasil_malaria_29", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "muni$noti_inc <- muni$noti_muni / muni$pop*1000
# muni$infec_inc <- muni$infec_muni / muni$pop*1000
# muni$resi_inc <- muni$resi_muni / muni$pop*1000

# breaks2 <- c(0, 5, 25, 50, 100, 250)
# noti_inci_plot <- ggplot(muni) +
#   ggtitle('Incidence of malaria notifications in 2015')+
#   geom_sf( aes(fill = noti_inc), size = .15, show.legend = TRUE)  +
#   scale_fill_stepsn(breaks = breaks2,
#                     colours = cols,
#                     values = scales::rescale(breaks2))
# noti_inci_plot"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Incidence-1.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_30", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "infe_inci_plot <- ggplot(muni) +
#   ggtitle('Incidence of probable malaria infections in 2015')+
#   geom_sf(aes(fill = infec_inc), size = .15, show.legend = TRUE)  +
#   scale_fill_stepsn(breaks = breaks2,
#                     colours = cols,
#                     values = scales::rescale(breaks2))
# infe_inci_plot"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Incidence-2.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_31", mode="r", theme="tomorrow", readOnly=TRUE, height="80",
#             value= "resi_inci_plot <- ggplot(muni) +
#   ggtitle('Incidence of residents with malaria in 2015')+
#   geom_sf(aes(fill = resi_inc), size = .15, show.legend = TRUE)  +
#   scale_fill_stepsn(breaks = breaks2,
#                     colours = cols,
#                     values = scales::rescale(breaks2))
# resi_inci_plot"
#           ),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_Incidence-3.png"),
          p("If one wants to know the incidence depending on the malaria type:"),
#           shinyAce::aceEditor(
#             "brasil_malaria_32", mode="r", theme="tomorrow", readOnly=TRUE, height="130",
#             value= "muni$vivax_inc <- muni$vivax_muni / muni$pop*1000
# muni$falci_inc <- muni$falci_muni / muni$pop*1000
# muni$mixed_inc <- muni$mixed_muni / muni$pop*1000

# vivax_inci_plot <- ggplot(muni) +
#   geom_sf(aes(fill = vivax_inc), size = .15, show.legend = TRUE) +
#   scale_fill_viridis_c() +
#   labs(subtitle = paste0('Vivax incidence in Brazil 2015'), size = 8) +
#   theme_minimal()
# vivax_inci_plot"
#           ),
#           tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_TypeIncidence-1.png"),
#           shinyAce::aceEditor(
#             "brasil_malaria_33", mode="r", theme="tomorrow", readOnly=TRUE, height="130",
#             value= "falci_inci_plot<- ggplot(muni) +
#   geom_sf( aes(fill = falci_inc), size = .15, show.legend = TRUE) +
#   scale_fill_viridis_c()+
#   labs(subtitle = paste0('Falciparum incidence in Brazil in 2015'), size = 8) +
#   theme_minimal()
# falci_inci_plot"
#           ),
#           tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_TypeIncidence-2.png"),
#           shinyAce::aceEditor( "brasil_malaria_34", mode="r", theme="tomorrow", 
#                                readOnly=TRUE, height="70",
#             value= "mixed_inci_plot<- ggplot(muni) +
#   geom_sf( aes(fill = mixed_inc), size = .15, show.legend = TRUE) +
#   scale_fill_viridis_c()+
#   labs(subtitle = paste0('Mixed incidence in Brazil in 2015'), size = 8) +
#   theme_minimal()
# mixed_inci_plot"),
          tags$img(src="1a_MalariaData_files/figure-gfm/Malaria_TypeIncidence-3.png")
        )
      )
    ),
    navbarMenu(
      "Access",
      tabPanel(
        "Brazil",
        tags$br(),
        p(HTML(paste0("Here you can find four different ways to download the anonymised raw health Brazilian data. If you do not need the raw data, you can access the ", tags$a("DataSUS tabnet", href="https://datasus.saude.gov.br/informacoes-de-saude-tabnet/", target="_blank"), " webpage and customise tables as you need it."))),
        p(HTML(paste0("Depending on the disease, the available date changes because of the year the disease was introduced and / or started the notification and / or became a mandatory disease notification in Brazil. The minimum year available on SINAN FTP link is 2000. The minimum year available on SIVEP is 2003. If you need years before it and / or nominal data and data from SIVEP, you have to request to the Ministry of Health on the e-SIC portal. Nominal data require ethical committee approval in most of the cases."))),
        tags$hr(),
        h3("Using microdatasus"),
        tags$hr(),
        p(HTML(paste0(tags$a("Microdatasus", href="https://www.scielo.br/j/csp/a/gdJXqcrW5PPDHX8rwPDYL7F/abstract/?lang=en", target="_blank"), " is an ", tags$a("R package", href="https://github.com/rfsaldanha/microdatasus", target="_blank"), " developed to download and preprocess data from the department of informatic of the Unified Health System (DATASUS). It can be installed and loaded with the following commands in R:"))),
#         shinyAce::aceEditor("code_install_microdatasus",
#                             height = "50px", mode = "r", theme="tomorrow",
#                             "if(!'devtools' %in% installed.packages()) { install.packages('devtools') }
# library(devtools)
# if(!'microdatasus' %in% installed.packages()){ devtools::install_github('rfsaldanha/microdatasus') }
# library(microdatasus)
# "),
        p("The microdatasus package has two main functions: downloading data and pre-processing it. The function to download data is “fetch_datasus”, and one needs to specify:"),
        tags$ol(
          tags$li("The temporal extent of the data: via year_start, month_start, year_end and month_end."),
          tags$li("The spatial extent: by specifying the federative units."),
          tags$li("The disease: by selecting the health information system."),
          tags$li("The variables: default is downloading every variable.")
        ),
        p("Here a few examples: Observation: SINAN malaria has data for Extra-Amazon region."),
#         shinyAce::aceEditor("code_fetch_microdatasus",
#                             height="100px", mode = "r", theme="tomorrow",
#                             "chik     <-    fetch_datasus(year_start = 2017, year_end = 2019, information_system = 'SINAN-CHIKUNGUNYA')
# mal      <-    fetch_datasus(year_start = 2004, year_end = 2006, information_system = 'SINAN-MALARIA')
# den      <-    fetch_datasus(year_start = 2011, year_end = 2013, information_system = 'SINAN-DENGUE')
# zika     <-    fetch_datasus(year_start = 2016, year_end = 2021, information_system = 'SINAN-ZIKA')
# "),
        p("The second functionality of this package is to preprocess the datasets. This includes the following steps:"),
        tags$ul(
          tags$li("Assigning labels to categorical fields, eg CS_RACA goes from (1,2,3,…) to (Branca (White), Preta (Black), Amarela (Yellow),…)"),
          tags$li("Converts each variable to its correct format, eg. from factor to character."),
          tags$li("Simplifying the age variable by changing the NU_IDADE_N field into IDADEminutos, IDADEhoras, IDADEdias, IDADEmeses and IDADEanos.")
        ),
        p("Since the datasets of all loaded diseases are different, the package has distinct function for all of them."),
#         shinyAce::aceEditor("code_fetch_microdatasus",
#                             height="100px", mode = "r", theme="tomorrow",
#                             "chik2 <- process_sinan_chikungunya(chik)
# mal2  <- process_sinan_malaria(mal)
# den2  <- process_sinan_dengue(den)
# zika2 <- process_sinan_zika(zika)
# "),
        tags$hr(),
        h3("Using Filezilla"),
        tags$hr(),
        p("SINAN has File Transfer Protocol (FTP) link to download data directly. For this, it is necessary to use a FTP application like FileZilla."),
        p(HTML(paste0("The host link is ftp.datasus.gov.br and the remote site is /dissemin/publicos/SINAN/. Once the connection is established, the correct folder will appear on the screen. Inside of the “DADOS” folder, there are two folders: “FINAIS” and “PRELIM”. The “FINAIS” folder contains all the revised historical data, while “PRELIM” contains the non-revised raw data of the last year(s). Both are organized by disease and year. If a database is available on “FINAIS”, it means this is not available on “PRELIM”."))),
        tags$div(
          style = "text-align: center;",
          tags$img(src = "Filezilla_SINAN.png", style = "max-width: 80%; height: auto;")
        ),
        tags$hr(),
        h3("Using Datasus website + commandline"),
        tags$hr(),
        p(HTML(paste0("The Ministry of Health provides a ", tags$a("website", href="https://datasus.saude.gov.br/transferencia-de-arquivos/", target="_blank"), " to search for and download files. Enter the page and fill in the following:"))),
        tags$ul(
          tags$li("Source (Fonte): SINAN - Sistema de Informaçoes de Agravos de Notificaçao"),
          tags$li("Modalidade: Dados"),
          tags$li("Tipo de Arquivo: Select the disease of your interest"),
          tags$li("Ano: Select your desired years"),
          tags$li("UF: BR")
        ),
        p("And then click on “enviar”. A list with all selected data files will appear. Clicking on the files, a pop up will appear asking you to choose which program should be used to open the link. If you have a FTP program which accepts FTP links with the filename in the url you can select it (FileZilla does not support this). Otherwise, you can right click on the file, copy the link address and go to the command line to write the following:"),
        # shinyAce::aceEditor("code_wget",
        #                     height="100px", mode = "r", theme="tomorrow",
        #                     "wget ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/DENGBR21.dbc -P Path/where/you/want/to/save/"),
        
        
        tags$hr(),
        h3("Using PySUS package"),
        tags$hr(),
        p("The PySUS package was developed to directly download data from Python. The package can be downloaded using the following commands in the terminal:"),
#         shinyAce::aceEditor("code_pysus_install", height="100px", mode = "r", theme="tomorrow",
#                             "sudo apt install libffi-dev
# sudo pip install PySUS
# pip install geocoder
# "),
#         p("Then, a file path for the loaded data has to be established. The following can either be typed into the command line each time before or written into the ~/.bashrc file for permanent setting."),
#         shinyAce::aceEditor("code_pysus_path",
#                             height="100px", mode = "r", theme="tomorrow",
#                             "export PYSUS_CACHEPATH='/home/me/desired/path/.pysus'"),
#         p("Then, within python, the data can be loaded and explored with the following lines of code."),
#         shinyAce::aceEditor("code_pysus_load", height="100px", mode = "r", theme="tomorrow",
#                             "from pysus.preprocessing.sinan import SINAN
# SINAN.list_diseases()
# SINAN.get_available_years('zika')
# df = SINAN.download(2019,'Zika')
# "),
        p(HTML(paste0("Downloaded files can be found as parquet files in the folder that was defined above. Further functions can be seen in the ", tags$a("guide", href="https://pysus.readthedocs.io/en/latest/", target="_blank"),".")))
        
      ),
      tabPanel(
        "Dominican Republic",
        "Coming soon!"
      ),
      tabPanel(
        "Peru",
        "Coming soon!"
      ),
      tabPanel(
        "Colombia",
        "Coming soon!"
      )
    ),
    # Preproces ####
    tabPanel(
      "Preprocess",
      tags$br(),
      
      # load data --------------------------------------------------------------
      h3("Upload your data here"),
      p("Accepted file formats are .csv, .dbf, and .dbc."),
      p("File should be in linelist format, each row represents a disease case and each column is a variable describing the case."),
      fluidRow(
        column(
          width=6,
          fileInput("input_pre", "", accept=c('.csv','.dbf','.dbc'), multiple=TRUE)
        ),
        column(
          width=6,
          tags$br(),
          actionButton("default_data","Load Test data")
        )
      ),
      
      tags$hr(),
      
      # column name meaning -----------------------------------------------------------
      h3("Get to know the data"),
      tags$hr(),
    
      h4("Content of your data"),
      tags$img(src="eye.png",width="20"),
      "In your current dataset you have the following columns:",
      shiny::uiOutput("pre_colnames"),
    #   shinycssloaders::withSpinner(shiny::uiOutput("pre_colnames")),
      p(HTML(paste0("If you are using official data from one of the project countries, you can find the definition of the column names in one of the dictionaries which can be found in the ", actionLink("go_documentation", "Documentation")," tab."))),
      ### add files here
      tags$br(),
      
      h4("Only keep neccessary columns"),
      tags$img(src="filter.svg",width="20"),
      uiOutput("pre_colnames_select"),
      tags$br(),
      uiOutput("pre_colnames_delete"),
      actionButton("run_code_colnames_delete", "Filter columns"),
      tags$br(),
      # rename columns ---------------------------------------------------------
      h4("Rename columns"),
      shiny::uiOutput("pre_colnames_rename1"),
      shiny::uiOutput("pre_colnames_rename2"),
      
      tags$hr(),
      # completeness of data ---------------------------------------------------
      h3("Completeness of the data"),
      tags$hr(),
      
      tags$img(src="idea.png",width="20"),
      p("Some of these variables are mandatory to notify, others are not, which leads to incomplete datasets. Below you can see which columns are complete and a chart showing the variables which have NA-values and the percentage of NAs they contain."),
      tags$br(),  
      
      tags$img(src="eye.png",width="20"),
      uiOutput("code_pre_NAcount_ui"),
      uiOutput("pre_NAcount"),
      uiOutput("code_pre_NAplot_ui"),
      # plotOutput("NAplot"),
      plotlyOutput("NAplot"),
      tags$br(),     
      
      tags$img(src="filter.svg",width="20"),
      p("To filter out variables which are completely empty, you can run the following lines."),
      uiOutput("pre_NAdelete_ui"),
#       shinyAce::aceEditor(
#         "code_pre_NAdelete", 
#         mode = "r", theme="tomorrow", height = "50px", 
#         value = "drop_variables <- NACount$var[NACount$count == 1]
# data <- data[, !(names(data) %in% drop_variables)]"
#       ),
      actionButton("run_code_NAdelete", "Delete columns"),
    
      tags$hr(),
      
      # clean columns-----------------------------------------------------------
      h3("Clean columns"),
      tags$hr(),
      
      p("The three most important ones are categorical, numerical and dates."),
      uiOutput("clean_all_columns"),
      # uiOutput("select_column_buttons"),
      # uiOutput("column_sections"),
      # uiOutput("pre_column_format"),
      # selectizeInput("col_format","",
      #              choices=c(
      #                  "Categorical"="cat",
      #                          "Numerical"="num",
      #                          "Dates"="dat"),
      #              multiple=TRUE,
      #              options=list(maxItems =1)),

      # uiOutput("pre_column_values1"),
      # uiOutput("pre_column_values2"),
      # uiOutput("pre_column_values3"),
      # uiOutput("pre_column_plot"),
      # shinycssloaders::withSpinner(uiOutput("pre_columns_visualise_ui")),
      
      tags$br(),
      tags$br(),
      fluidRow(
        column(
          width=6,
          downloadButton("download_clean", label="Download Cleaned Data")
        ),
        column(
          width=6,
          actionButton("go_preprocess_aggregation","Continue to aggregation")
          # downloadButton("download_clean_report", label="Download Report"),
          # "Creating reports is not functional yet"
        )
      ),
      tags$br(),
      tags$br()
    ),
    # Aggregate ----------------------------------------------------------------
    tabPanel(
      "Aggregate",
      tags$br(),
      h3("Define Columns"),
      uiOutput("agg_load"),

      h4("Necessary columns"),
      fluidRow(
        column(
          width =6,
          selectInput("NotificationDate",   label = "Notification Date",   choices = c(""), selected = "")
        ),
        column(
          width =6,
          selectInput("NotificationMunicipality", label = "Notification Municipality", choices = c(""), selected = "")
        )
      ),
      
      h4("Optional columns"),
      fluidRow(
        column(
          width =6,
          selectInput("Sex", label = "Sex", choices = c(""), selected = "")
        ),
        column(
          width =6,
          selectInput("Cases", label = "Disease status", choices = c(""), selected = "")
        )
      ),
         
      actionButton("cleanbutton", label = "Continue"),

      tags$br(),
      tags$hr(),
      h3("Aggregate"),
      helpText("Select the parameters by which the data should be aggregated."),
     
      selectInput("Itime", label = "Initial Year", choices = c(""), selected = ""),
      selectInput("Ftime", label = "Final Year", choices = c(""), selected = ""),           
      selectInput("Tunit", label = "Temporal Resolution", choices = c(""), selected=character(0)),
      selectInput("Sunit", label = "Spatial Resolution", choices = c(""), selected = ""),
      selectInput("Sloc", label = "Research Area", choices = c(""), selected = "", multiple=TRUE),
      
      conditionalPanel(
       condition= "output.gendercond2 == true",
       selectInput("Sex2",   label = "Sex",   choices = c(""), selected = "")
      ),
      conditionalPanel(
       condition= "output.critcond == true",
       selectInput("Crit", label = "Confirmation criteria", choices = c("Confirmed","Suspected", "Negative"), selected = "", multiple= TRUE)
      ),
      conditionalPanel(
       condition= "output.conf1cond == true",
       selectInput("ConfirmedCase", label = "Confirmed", choices = c(""), selected = "", multiple= TRUE)
      ),
      conditionalPanel(
       condition= "output.conf2cond == true",
       selectInput("SuspectedCase", label = "Suspected", choices = c(""), selected = "", multiple= TRUE)
      ),
      conditionalPanel(
       condition= "output.conf3cond == true",
       selectInput("NegativeCase", label = "Negative", choices = c(""), selected = "", multiple= TRUE)
      ),
      
      actionButton("aggregatebutton", label = "Aggregate Data"),
      downloadButton("downloadaggdata", label="Download Aggregated Data")


    ),
    # visualise ----------------------------------------------------------------
    tabPanel(
      "Plot",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons("plottype","Type of plot", c("Timeseries" = 'timeseries', "Table"="table"), inline= TRUE, selected=character(0)),
          # radioButtons("plottype","Type of plot", c("Timeseries" = 'timeseries', "Map" = "map", "Table"="table"), inline= TRUE, selected=character(0)),
          
          conditionalPanel(
           condition= "input.plottype == 'timeseries'",
           selectInput("comparison", label = "Comparison", choices = c(""), selected = ""),
           conditionalPanel(
             condition= "output.comparisonS == true",
             selectInput("Sex3", label = "Sex", choices = c(""), selected = "", multiple= TRUE)
          ),
          conditionalPanel(
            condition= "output.comparisonC == true",
            selectInput("Status3", label = "Disease status", choices = c(""), selected = "", multiple= TRUE)
          ),
          conditionalPanel(
            condition= "output.comparisonL == true",
            selectInput("Sloc3", label = "Location", choices = c(""), selected = "", multiple= TRUE)
          ),
          sliderInput(
            "Timestep", 
            label = "Timeperiod",
            min=as.Date("2016-12-31","%Y-%m-%d"),
            max=as.Date("2020-12-31","%Y-%m-%d"),
            value=c(as.Date("2020-05-05"), as.Date("2020-05-06")), 
            timeFormat = "%Y-%m-%d"
          ),
          actionButton("tsbutton", label = "Plot Timeseries")
          ),
          # {
          #  # conditionalPanel(
          #  #   condition= "input.plottype == 'map'",
          #  #
          #  #   selectInput("country", "Country", choices=c(""),selected=""),
          #  #   radioButtons("abs_inc", "Absolute values or incidence?", c("Absolute"="abs", "Incidence"="inc"), inline= TRUE, selected=character(0)),
          #  #   conditionalPanel(
          #  #     condition="input.abs_inc == 'inc'",
          #  #     numericInput("incidence_val",label="Incidence value",value=1000,min=0,max=1000000),
          #  #     helpText("Population file should include a first column with the area name, and a second column with the population count."),
          #  #     fileInput("filepop", "", accept=c('.csv','.dbf','.dbc'), multiple= TRUE)
          #  #   ),
          #  #   conditionalPanel(
          #  #     condition= "output.gendercond4 == true",
          #  #     selectInput("Sex4",   label = "Sex",   choices = c(""), selected = "", multiple= TRUE)
          #  #   ),
          #  #   radioButtons("tstep", "Specific moment or period?", c("One Time"="onetime","Timeperiod"="tperiod"), inline= TRUE, selected=character(0)),
          #  #   conditionalPanel(
          #  #     condition="input.tstep == 'onetime'",
          #  #     sliderInput("Timestep2",
          #  #                 label = "Timeperiod",
          #  #                 min=as.Date("2016-12-31","%Y-%m-%d"),
          #  #                 max=as.Date("2020-12-31","%Y-%m-%d"),
          #  #                 value=c(as.Date("2020-05-05")), timeFormat = "%Y-%m-%d"),
          #  #   ),
          #  #   conditionalPanel(
          #  #     condition="input.tstep == 'tperiod'",
          #  #     sliderInput("Timestep3",
          #  #                 label = "Timeperiod",
          #  #                 min=as.Date("2016-12-31","%Y-%m-%d"),
          #  #                 max=as.Date("2020-12-31","%Y-%m-%d"),
          #  #                 value=c(as.Date("2020-05-05"), as.Date("2020-05-06")), timeFormat = "%Y-%m-%d"),
          #  #   ),
          #  #   actionButton("mapbutton", label = "Plot Map")
          #  # ),
          # }
          conditionalPanel(
            condition="input.plottype == 'table'",
            selectInput("Sloc5", "Research area", choices=c(""),selected="", multiple= TRUE),
            sliderInput(
              "Timestep4",
              label = "Timeperiod",
              min=as.Date("2016-12-31","%Y-%m-%d"),
              max=as.Date("2020-12-31","%Y-%m-%d"),
              value=c(as.Date("2020-05-05"), as.Date("2020-05-06")), 
              timeFormat = "%Y-%m-%d"
            )
          ),
          conditionalPanel(
            condition= "output.gendercond5 == true",
            selectInput("Sex5",   label = "Sex",   choices = c(""), selected = "", multiple= TRUE)
          ),
          conditionalPanel(
            condition= "output.statuscond5 == true",
            selectInput("Status5",   label = "Disease status",   choices = c(""), selected = "", multiple= TRUE)
          ),
          actionButton("tablebutton", label= "Show Table")
        ), 
        mainPanel(
          conditionalPanel(
            condition="input.plottype == 'timeseries'",
            plotOutput("ts_plot")
          ),
          # conditionalPanel(
          #   condition="input.plottype == 'map'",
          #   plotOutput("map_plot"),
          # ),
          conditionalPanel(
            condition="input.plottype == 'table'",
            dataTableOutput("table_plot")
          )
        )
      )
    )
    # end ----------------------------------------------------------------------
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ##############################################################################
  ######################### Epi Data Input #####################################
  ##############################################################################
  
  #### Links -----------------------------------------
  
  # Documentation 
  
  # download dictionaries
  observeEvent(input$download_br_dengue_dictionary,{
    file <- read.csv("Dictionary_Dengue.csv")
    downloadHandler(
      filename = "BR_dengue_dictionary.csv",
      content = write.csv(file)
    )
  })
  
  # switch to documents
  document_page <- reactiveVal("start")
  observeEvent(input$go_dengue, {
    document_page("br_malaria")
  })
  observeEvent(input$go_malaria, {
    document_page("br_malaria")
  })
  observeEvent(input$go_brdengue_start, {
    document_page("start")
  })
  observeEvent(input$go_brmalaria_start, {
    document_page("start")
  })
  output$document_page <- renderText({
    document_page()
  })
  outputOptions(output, "document_page", suspendWhenHidden=FALSE)

  #### Preprocess data -------------------------------
  
  # load dictionary
  # dictionary <- read.csv("data/Dictionary_all.csv")
  dictionary <- system.file("dictionary.rda", package = "data4health")
  
  # load dataset for pre processing
  data <- reactiveVal(NULL)
  data_change <- reactiveVal(0)
  observeEvent(input$input_pre,{
    inFile <- input$input_pre
    if (is.null(inFile)){
      data(return(NULL))
    } else if (sub(".*\\.","", inFile$datapath) == "dbc"){
      data(read.dbc(inFile$datapath))
    } else if (sub(".*\\.","", inFile$datapath) == "dbf") {
      data(read.dbf(inFile$datapath, as.is = TRUE))
    } else if (sub(".*\\.","", inFile$datapath) == "csv") {
      data(read.csv(inFile$datapath))
    }
    data_change(data_change() +1)
  })
  
  # load default dataset
  observeEvent(input$default_data,{
    # data(read.dbc("DENGBR14.dbc"))
    data(read.csv("small_test_dataset.csv"))
    # data(read.csv("Dictionary_Dengue.csv"))
    data_change(data_change() +1)
  })
  
  # Filter selected column -----------------------------------------------------
  
  # UI - list of colnames with explanation
  output$pre_colnames <- renderUI({
    req(data())
    tagList(
      tags$table(
        width="100%",
        tags$tr(
          width="100%",
          if (columns_renamed()) { tags$th("Original name")},
          tags$th("Column name"),
          tags$th("Meaning")
        ),
        lapply(colnames(data()), function(i) {
          tags$tr(
            width="100%",
            if (columns_renamed()) {
              tags$td(ifelse(i %in% names(col_changes), col_changes[[i]], i))
            },
            if (columns_renamed()) {
              tags$td(i)
            } else {
              tags$td(i)
            },
            if (columns_renamed()) {
              tags$td(dictionary[dictionary$Var == ifelse(i %in% names(col_changes),
                                                          col_changes[[i]],i), "Description"])
            } else {
              tags$td(dictionary[dictionary$Var == i, "Description"])
            }
          )
        })
      )
    )
  })
  
  # UI - show buttons to select colnames which should be deleted
  output$pre_colnames_select <- renderUI({
    req(data())
    col_names <- colnames(data())
    buttons <- lapply(col_names, function(col) {
      actionButton(inputId = paste0("button_", col), label = col, onclick = paste0("Shiny.setInputValue('btn_', this.id);"))
    })
    do.call(tagList, buttons)
  })
  observeEvent(input$btn_, {
    btn_id <- input$btn_
    btn_name <- gsub("button_", "", btn_id)
    if (btn_name %in% rv$clicked) {
      rv$clicked <- rv$clicked[!rv$clicked %in% btn_name]
      updateActionButton(session, inputId = btn_id, label = btn_name, icon = NULL)
    } else {
      rv$clicked <- c(rv$clicked, btn_name)
      updateActionButton(session, inputId = btn_id, label = btn_name, icon = icon("check"))
    }
  })
  
  # UI - code to only keep selected variables
  rv <- reactiveValues(clicked = list())
  output$pre_colnames_delete <- renderUI({
    if (hide3_count() %% 2 == 0){
      tagList(
        actionLink("hide3", 
                   "Click here to show the code",
                   icon("angle-down"),
                   style="color: #bfbfbf; text-decoration: none"),
        tags$div(style = "display: none;",
                #  shinyAce::aceEditor(
                #    "pre_colnames_delete_code", 
                #    mode = "r", theme="tomorrow", 
                #    height = "40px", 
                #    value = paste0("sel_var <- c(",paste(sprintf('\"%s\"', rv$clicked), collapse = ", "),")\n",
                #                   "data <- data %>% select(all_of(sel_var))")
                #  )
        )
      )
    } else {
      tagList(
        actionLink("hide3",
                   "Click here to hide the code",
                   icon("angle-up"),
                   style="color: #bfbfbf; text-decoration: none"),
        tags$br(),
#         shinyAce::aceEditor(
#           "pre_colnames_delete_code", 
#           mode = "r", theme="tomorrow", 
#           height = "40px", 
#           value = paste0("sel_var <- c(",paste(sprintf('"%s"', rv$clicked), collapse = ", "),")
# data <- data %>% select(all_of(sel_var))")
#         )
      )
    }
  })

  hide3_count <- reactiveVal(0)
  observeEvent(input[["hide3"]],{
    hide3_count(hide3_count() + 1)
  })
  

  # execute - keep only selected variables
  observeEvent(input$run_code_colnames_delete, {
    req(data())
    code <- append(input$pre_colnames_delete_code, "data(data)")
    tryCatch({
      env <- new.env()
      env$input <- input
      env$data <- data()
      eval(parse(text = code), envir = env)
    }, error = function(e) {
      showNotification("Error in code execution", type = "error")
    })
    data_change(data_change() +1)
  })
  
  # rename columns --------------------------------------------------------------
    
  # UI - rename columns (table)
  columns_renamed <- reactiveVal(FALSE)
  output$pre_colnames_rename1 <- renderUI({
    req(data())
    colnames <- colnames(data())
    table <- tags$table(
      width="100%",
      tags$tr(
        width="100%",
        tags$th("Current Value", width = "30%"),
        tags$th("New value", width = "70%")
      ),
      lapply(seq_along(colnames), function(i) {
        val <- colnames[i]
        tags$tr(
          width="100%",
          tags$td(width ="30%", val),
          tags$td(width ="70%",
                  textInput(inputId = paste0("input_col_", val), 
                            label = NULL, 
                            value = dictionary$Translation[dictionary$Var == val]))
        )
      })
    )  
    table
  })
  
  # UI - rename columns (code block and button)
  col_changes <- reactiveValues()
  output$pre_colnames_rename2 <- renderUI({
    req(data())
    colnames <- colnames(data())
    result_string <- NULL
    all_empty <- TRUE
    for (val in colnames) {
      new_name <- input[[paste0("input_col_", val)]]
      if (!is.null(new_name) && length(new_name) != 0 && new_name != "") {
        # get new value
        result_string <- append(result_string, new_name)
        col_changes[[new_name]] <- val
        all_empty <- FALSE
      } else {
        result_string <- append(result_string, val)
      }
    }
    code_string <- ifelse(all_empty, "",
                          paste0("colnames(data) <- c(\"",
                                 paste(result_string, collapse=("\", \"")),"\")"))

    output <- tagList()
    
    if (hide4_count() %% 2 == 0){
      tagList(
        actionLink("hide4", 
                   "Click here to show the code",
                   icon("angle-down"),
                   style="color: #bfbfbf; text-decoration: none"),
        tags$br(),
        actionButton("run_code_colnames_rename",
                                    label = "Rename columns"),
        tags$div(style = "display: none;",
                #  shinyAce::aceEditor("code_colnames_rename",
                #                      mode = "r", theme="tomorrow",
                #                      height = "100px",
                #                      value = code_string)
        )        
      )

    } else {
      tagList(
        actionLink("hide4",
                  "Click here to hide the code",
                  icon("angle-up"),
                  style="color: #bfbfbf; text-decoration: none"),
        tags$br(),
        # shinyAce::aceEditor("code_colnames_rename",
        #                   mode = "r", theme="tomorrow",
        #                   height = "100px",
        #                   value = code_string),
        actionButton("run_code_colnames_rename",
                                    label = "Rename columns")        
      )
    }
  })
  
  hide4_count <- reactiveVal(0)
  observeEvent(input[["hide4"]],{
    hide4_count(hide4_count() + 1)
  })

  # execute_ rename columns
  observeEvent(input$run_code_colnames_rename, {
    columns_renamed(TRUE)
    req(data())
    code <- append(input$code_colnames_rename, "data(data)")
    tryCatch({
      env <- new.env()
      env$input <- input
      env$data <- data()
      eval(parse(text = code), envir = env)
    }, error = function(e) {
      showNotification("Error in code execution", type = "error")
    })
    data_change(data_change() +1)
  })
  
  # show completeness of columns  ----------------------------------------------
    
  # UI - get NA threshold value to eliminatecolumns 
  threshold_value <- reactiveVal(0)
  
  # UI - show code to show complete columns
  output$code_pre_NAcount_ui <- renderUI({
    if (hide1_count() %% 2 == 0){
      tagList(
        actionLink("hide1", "Click here to show the code",icon("angle-down"),
                   style="color: #bfbfbf; text-decoration: none"),
        tags$div(style = "display: none;",
#                  shinyAce::aceEditor(outputId = "code_pre_NAcount",
#                                      mode = "r", 
#                                      theme="tomorrow", 
#                                      height = "100px", 
#                                      value = paste0("# calculate complete columns
# NACount <- data %>% map(~mean(is.na(.))) 
# NACount <- data.frame(count=unlist(NACount),
#                       var=names(NACount))
# complete <- NACount$var[NACount$count == ",threshold_value(),"]"))
        )        
      )
    } else {
      output <- tagList(
        actionLink("hide1", "Click here to hide the code",icon("angle-up"), 
                                  style="color: #bfbfbf; text-decoration: none"),
        tags$br(),
        tags$b("Calculate the complete columns"),
#         shinyAce::aceEditor(outputId = "code_pre_NAcount",
#                                            mode = "r", 
#                                            theme="tomorrow", 
#                                            height = "100px", 
#                                            value = paste0("# calculate complete columns
#   NACount <- data %>% map(~mean(is.na(.))) 
#   NACount <- data.frame(count=unlist(NACount),
#                         var=names(NACount))
#   complete <- NACount$var[NACount$count == ",threshold_value(),"]"))        
      )
    }
  })
  hide1_count <- reactiveVal(0) 
  observeEvent(input$hide1,{
    hide1_count(hide1_count() + 1) 
  })
  
  # execute - show complete columns
  complete <- reactiveVal(NULL)
  NAcount <- reactiveVal(NULL)
  observeEvent(data_change(), {
    req(data())
    req(input$code_pre_NAcount)
    
    code <- append(input$code_pre_NAcount, "complete(complete)")
    code <- append(code, "NAcount(NAcount)")

    tryCatch({
      env <- new.env()
      env$input <- input
      env$data <- isolate(data())
      with(env, {
        library(dplyr)
        library(tidyr)
        library(purrr)
        library(ggplot2)
      })
      eval(parse(text = code), envir = env)

    }, error = function(e) {
      showNotification("Error in code execution", type = "error")
    })
  })
  
  # UI - show complete columns
  output$pre_NAcount <- renderUI({
    req(complete())
    paste0("The following columns are complete: ", paste(complete(), collapse=", "))
  })
  
  # UI - code to plot incomplete columns
  output$code_pre_NAplot_ui <- renderUI({
    if (hide6_count() %% 2 == 0){
      tagList(
        actionLink("hide6", "Click here to show the code",icon("angle-down"),
                   style="color: #bfbfbf; text-decoration: none"),
        tags$div(style = "display: none;",
#                  shinyAce::aceEditor(outputId = "code_pre_NAplot",
#                                      mode = "r", 
#                                      theme="tomorrow", 
#                                      height = "100px", 
#                                      value = paste0("
# NA_plot <- NACount[NACount$count != 0, ]
# NA_plot$var <- factor(NA_plot$var, levels = unique(NA_plot$var)[order(NA_plot$count, decreasing = FALSE)])
# plot_ly(
#   data = NA_plot,
#   x = NA_plot$count,
#   y = NA_plot$var,
#   type = 'bar',
#   orientation = 'h'
#   ) %>%
#   layout(
#      xaxis = list(title = 'Percentage of NAs in each column'),
#      yaxis = list(title = 'Variables'),
#      margin = list(l = 150) # Adjust left margin to fit labels
#   ) "))
        )        
      )
    } else {
      output <- tagList(
        actionLink("hide6", "Click here to hide the code",icon("angle-up"), 
                                style="color: #bfbfbf; text-decoration: none"),
        tags$br(),
        tags$b("Plot the incomplete columns"),
#         shinyAce::aceEditor(outputId = "code_pre_NAplot",
#                                          mode = "r", 
#                                          theme="tomorrow", 
#                                          height = "100px", 
#                                          value = paste0("# plot the graph                                        
# NA_plot <- NACount[NACount$count != 0, ]
# NA_plot$var <- factor(NA_plot$var, levels = unique(NA_plot$var)[order(NA_plot$count, decreasing = FALSE)])
# plot_ly(
#   data = NA_plot,
#   x = NA_plot$count,
#   y = NA_plot$var,
#   type = 'bar',
#   orientation = 'h'
#   ) %>%
#   layout(
#      xaxis = list(title = 'Percentage of NAs in each column'),
#      yaxis = list(title = 'Variables'),
#      margin = list(l = 150) # Adjust left margin to fit labels
#   )                          
#                                                         ")),
        actionButton("run_code_NAplot", "Plot again")
      )
    }
  })
  hide6_count <- reactiveVal(0) 
  observeEvent(input$hide6,{
    hide6_count(hide6_count() + 1) 
  })
  
  # execute - plot the NA count
  observeEvent(c(data_change(), input$run_code_NAplot), {
    req(data())
    code <- append(input$code_pre_NAcount, input$code_pre_NAplot)
    tryCatch({
      env <- new.env()
      env$input <- input
      env$data <- data()
      with(env, {
        library(dplyr)
        library(tidyr)
        library(purrr)
        library(ggplot2)
      })
      NAplot <- eval(parse(text = code), envir = env)
      output$NAplot <- renderPlotly({
        NAplot
      })
    }, error = function(e) {
      showNotification("Error in code execution", type = "error")
    })
  })

  # UI - code to delete empty columns
  output$pre_NAdelete_ui <- renderUI({
    if (hide5_count() %% 2 == 0){
      tagList(
        actionLink("hide5", "Click here to show the code",icon("angle-down"), 
                 style="color: #bfbfbf; text-decoration: none"),
        tags$div(style = "display: none;",
#                  shinyAce::aceEditor(
#                    "code_pre_NAdelete", 
#                    mode = "r", theme="tomorrow", height = "50px", 
#                    value = "drop_variables <- NACount$var[NACount$count == 1]
# data <- data[, !(names(data) %in% drop_variables)]")
        )
      )
    } else {
      tagList(
        actionLink("hide5", "Click here to hide the code",icon("angle-up"), 
                              style="color: #bfbfbf; text-decoration: none"),
        tags$br(),
#         shinyAce::aceEditor(
#                       "code_pre_NAdelete", 
#                       mode = "r", theme="tomorrow", height = "50px", 
#                       value = "drop_variables <- NACount$var[NACount$count == 1]
# data <- data[, !(names(data) %in% drop_variables)]")
      )
    }
  })
  hide5_count <- reactiveVal(0) 
  observeEvent(input$hide5,{
    hide5_count(hide5_count() + 1) 
  })

  
  # run code to delete empty columns
  observeEvent(input$run_code_NAdelete, {
    req(data())
    code <- append(input$code_pre_NAcount, input$code_pre_NAdelete)
    code <- append(code, "data(data)")
    tryCatch({
      env <- new.env()
      env$input <- input
      env$data <- data()
      eval(parse(text = code), envir = env)
    }, error = function(e) {
      showNotification("Error in code execution", type = "error")
    })
    data_change(data_change() +1)
  })
  
  # clean columns --------------------------------------------------------------
  
  # print all columns
  output$clean_all_columns <- renderUI({
    req(data())

    columns <- colnames(data())

    if (length(columns) > 0) {
      # for (column in columns) {
      #   uiOutput(paste0("clean_", column))
      # }
      ui_list <- lapply(columns, function(column) {
        uiOutput(paste0("clean_1_", column))
      })
      # Return the list of UI elements
      do.call(tagList, ui_list)
    } else {
      h5("There was a problem.")
    }
  })

  # print each "Clean column"
  observe({
    req(data())
    columns <- colnames(data())

    lapply(columns, function(column) {
      
      # render selecting column format
      output[[paste0("clean_1_", column)]] <- renderUI({
        
        print(column)
        print(columns_renamed)
        if (columns_renamed()) {
          format1 <- dictionary$format[dictionary$Var == col_changes[[column]]]
        } else {
          format1 <- dictionary$format[dictionary$Var == column]
        }
        print(format1)
        if (!is.null(format1)){
          print("bla")
          col_format <- format1
        } else {
          print("ble")
          col_format <- "cat"
        }
        print(col_format)
        tagList(
          tags$hr(),
          h3(paste0("Clean ", column)),
          tags$hr(),
          h5("1. Select Column format"),
          "data4health estimates the format, if you dont agree you can change the format here data4health gusses the format, however this is not completly accurate. If you think your column has a different format please change below::",

          # Use a unique ID for each selectizeInput
          selectizeInput(paste0("col_format_", column), "",
                         choices = c(
                           "Categorical" = "cat",
                           "Numerical" = "num",
                           "Dates" = "dat"
                         ),
                         multiple = TRUE,
                         options = list(maxItems = 1),
                         selected = col_format),

          # This will hold the additional UI based on the selected format
          uiOutput(paste0("clean_2_", column))
        )
      })

      # once column format is selected
      observeEvent(input[[paste0("col_format_", column)]], {
        col_format <- input[[paste0("col_format_", column)]]

        # render first modification
        output[[paste0("clean_2_", column)]] <- renderUI({
          if (col_format == "cat") {
            # Render UI specific to "Categorical"
            unique_values <- unique(data()[, column])
            table <- tags$table(
              width="100%",
              tags$tr(
                width="100%",
                tags$th("Current Value", width = "30%"),
                tags$th("New value", width = "70%")
              ),
              lapply(seq_along(unique_values), function(i) {
                val <- unique_values[i]
                tags$tr(
                width="100%",
                    tags$td(width ="30%", val),
                    tags$td(width ="70%",textInput(inputId = paste0("input_", val, "_", column), label = NULL, value = ""))
                )
              })
            )
            tagList(
              h5("2. Rename values"),
              "To clean categorical columns, it is important to first at all the unique values present:",
              tags$br(),
              table,
              uiOutput(paste0("clean_3_", column))
            )
          } else if (col_format == "num") {
            print("in num")
            # Render UI specific to "Numerical"
            h5("Numerical selected")
          } else if (col_format == "dat") {
            bla <- tagList(
              h5("2. Select temporal resolution"),
              "Temporal resolution can be daily, epiweekly or monthly:",
              tags$br(),
              radioButtons("temporal_resolution",label="",
                                        choices=c("Day"="d","Month"="m")),#,"Epiweek"="w"))
              uiOutput(paste0("clean_3_", column))
            )
          } else {
            NULL
          }

          # This will hold the additional UI based on the selected format
          # bla[[length(bla)+1]] <- uiOutput(paste0("clean_3_", column))
        })
      })
      
      # once column format is selected
      observeEvent(c(input[[paste0("col_format_", column)]],data_change()), {
        
        col_format <- input[[paste0("col_format_", column)]]

        # render code and actionbutton
        output[[paste0("clean_3_", column)]] <- renderUI({

        print_code <- TRUE

        # print(1)
        if (col_format == "cat"){

          # print(2)
          unique_values <- unique(data()[, column])

          # print(unique_values)
          result_string <- ""
          all_empty <- TRUE
          for (val in unique_values) {
            # print(input[[paste0("input_", val, "_", column)]])
            if (input[[paste0("input_", val, "_", column)]] != ""){
              # Construct the part of the string for each value
              part <- paste0(column, " == \"", val, "\" ~ \"", input[[paste0("input_", val, "_", column)]], "\",\n")
              # Append the part to the result string
              result_string <- paste0(result_string, part)
              all_empty <- FALSE
            }
          }

          code_string <- ifelse(all_empty, "",paste0("data <- data %>%
              mutate(",column," = as.factor(case_when(",result_string,")))"))

        } else if (col_format == "num"){
            print_code <- FALSE

        } else if (col_format == "dat"){
          if (input$temporal_resolution == "d"){
            code_string <- paste0("data$",column," <- as.Date(data$",column,")\ndata$",column,"_",input$temporal_resolution," <- data$",column)
          # } else if (input$temporal_resolution == "w"){
          #   code_string <- paste0("data$",column,"_",input$temporal_resolution," <- data[[column]] - weekdays(data$",column,") + 1")
          } else if (input$temporal_resolution == "m"){
            code_string <- paste0("data$",column," <- as.Date(data$",column,")\ndata$",column,"_",input$temporal_resolution," <- floor_date(data$",column,", unit = 'month')")
          }
        }
        if(print_code){
          if (hide2_count() %% 2 == 0){
            actionLink(paste0("hide_",column),
                       "Click here to show the code",
                       icon("angle-down"),
                       style="color: #bfbfbf; text-decoration: none")
          } else {
            tagList(
              actionLink(paste0("hide_",column),
                         "Click here to hide the code",
                         icon("angle-up"),
                         style="color: #bfbfbf; text-decoration: none"),
              tags$br(),
            #   shinyAce::aceEditor(paste0("code_clean_",column),
            #                       mode = "r",
            #                       theme="tomorrow",
            #                       height = "260px",
            #                       value = code_string),
              actionButton(paste0("run_clean_",column), "Run Code")
            )
          }
        }
      })

        hide2_count <- reactiveVal(0)
        observeEvent(input[[paste0("hide_", column)]],{
          hide2_count(hide2_count() + 1)
        })
      })
      
      # execute code block
        observeEvent(input[[paste0("run_clean_",column)]],{
          req(data())
          code <- append(input[[paste0("run_clean_",column)]], "data(data)")
          tryCatch({
            env <- new.env()
            env$input <- input
            env$data <- data()
            with(env, {
              library(dplyr)
              library(tidyr)
              library(purrr)
              library(ggplot2)
            })
            eval(parse(text = code), envir = env)
          }, error = function(e) {
            showNotification("Error in code execution", type = "error")
          })
        })
      
      outputOptions(output, paste0("clean_1_", column), suspendWhenHidden = FALSE)
    })
  })
  
  {
# 
#   # UI - select columnbutton
#   output$select_column_buttons <- renderUI({
#     req(data())
#     buttons <- lapply(colnames(data()), function(col_name) {
#       #actionButton(inputId = paste0("button_", col), label = col, onclick = paste0("Shiny.setInputValue('btn_', this.id);"))
#       
#       actionButton(inputId = paste0("button2_", col_name), col_name, onclick = paste0("Shiny.setInputValue('btn2_', this.id);") )#class = "dyn_button")
#     })
#     do.call(tagList, buttons)
#   })
# 
#   # which column should be worked on?
#   column_num <- reactiveVal(1)
#   # Event handler to update column_num when a button is clicked
#   observeEvent(input$btn2_, {
#     btn_id <- input$btn2_
#     btn_name <- gsub("button2_", "", btn_id)
#     print(btn_name)
#     
#     column_num(which(colnames(data()) == btn_name))
#    
#     print(column_num())
#   })
#   
#   # select the format of column
#   output$pre_column_format <- renderUI({
#     req(data())
#     colname <- colnames(data()[column_num()])
#     output <- tagList()
#     output[[1]] <- h3(colname)
#     output[[2]] <- h5("1. Select Column format")
#     output[[3]] <- "Select the format of this column:"
#     # output[[4]] <- isolate(selectizeInput("col_format","",
#     #                            choices=c(
#     #                                "Categorical"="cat",
#     #                                        "Numerical"="num",
#     #                                        "Dates"="dat"),
#     #                            multiple=TRUE,
#     #                            options=list(maxItems =1)))
#     output
#   })
# 
#   col_format <- reactiveVal(NULL)
#   observeEvent(input$pre_colnames_values, {
#     col_format(input$col_format)
#   })
#   observeEvent(input$col_format, {
#     col_format(input$col_format)
#   })
# 
# 
#   # UI - rename the unique values
#   output$pre_column_values1 <- renderUI({
#     req(col_format())
#     output <- tagList()
#     if (col_format() == "cat"){
#       unique_values <- unique(data()[, column_num()])
#       table <- tags$table(
#         width="100%",
#         tags$tr(
#           width="100%",
#           tags$th("Current Value", width = "30%"),
#           tags$th("New value", width = "70%")
#         ),
#         lapply(seq_along(unique_values), function(i) {
#           val <- unique_values[i]
#           tags$tr(
#           width="100%",
#               tags$td(width ="30%", val),
#               tags$td(width ="70%",textInput(inputId = paste0("input_", val), label = NULL, value = ""))
#           )
#         })
#       )
# 
#       output[[1]] <- h5("2. Rename values")
#       output[[2]] <- "To clean categorical columns, it is important to first at all the unique values present:"
#       output[[3]] <- tags$br()
#       output[[4]] <- table
#     } else if (col_format() == "num"){
# 
#     } else if (col_format() == "dat"){
#       output[[1]] <- h5("2. Select temporal resolution")
#       output[[2]] <- "Temporal resolution can be daily, epiweekly or monthly:"
#       output[[3]] <- tags$br()
#       output[[4]] <- radioButtons("temporal_resolution",label="",
#                                   choices=c("Day"="d","Month"="m"))#,"Epiweek"="w"))
#     } else {
#       output[[1]] <- "format is not cat"
#     }
#     output
#   })
  
# 
#   # UI - rename the unique values
#   output$pre_column_values2 <- renderUI({
#     req(data())
#     req(col_format())
#     print_code <- TRUE
#     colname <- colnames(data()[column_num()])
#     if (col_format() == "cat"){
# 
#       unique_values <- unique(data()[, column_num()])
# 
#       result_string <- ""
#       all_empty <- TRUE
#       for (val in unique_values) {
#         if (input[[paste0("input_", val)]] != ""){
#           # Construct the part of the string for each value
#           part <- paste0(colname, " == \"", val, "\" ~ \"", input[[paste0("input_", val)]], "\",\n")
#           # Append the part to the result string
#           result_string <- paste0(result_string, part)
#           all_empty <- FALSE
#         }
#       }
# 
#       code_string <- ifelse(all_empty, "",paste0("data <- data %>%
#           mutate(",colname," = as.factor(case_when(",result_string,")))"))
# 
#     } else if (col_format() == "num"){
#         print_code <- FALSE
# 
#     } else if (col_format() == "dat"){
#       if (input$temporal_resolution == "d"){
#         code_string <- paste0("data$",colname," <- as.Date(data$",colname,")\ndata$",colname,"_",input$temporal_resolution," <- data$",colname)
#       # } else if (input$temporal_resolution == "w"){
#       #   code_string <- paste0("data$",colname,"_",input$temporal_resolution," <- data[[colname]] - weekdays(data$",colname,") + 1")
#       } else if (input$temporal_resolution == "m"){
#         code_string <- paste0("data$",colname," <- as.Date(data$",colname,")\ndata$",colname,"_",input$temporal_resolution," <- floor_date(data$",colname,", unit = 'month')")
#       }
#     }
# 
#     if(print_code){
#       shinyAce::aceEditor(
#         "pre_column_values_code",
#         mode = "r", theme="tomorrow",
#         height = "100px",
#         value = code_string
#       )
#     }
#   })
  }
  {
# 
#   # UI - rename the unique values
#   output$pre_column_values3 <- renderUI({
#     req(col_format())
#     if (col_format() == "cat"){
#       actionButton("pre_colnames_values", label = "Rename values")
#     } else if (col_format() == "dat"){
#       actionButton("pre_colnames_values",  label = "Select resolution")
#     }
#   })
# 
#   # execute - rename the unique values
#   observeEvent(input$pre_colnames_values,{
#     req(data())
#     code <- append(input$pre_column_values_code, "data(data)")
#     tryCatch({
#       env <- new.env()
#       env$input <- input
#       env$data <- data()
#       with(env, {
#         library(dplyr)
#         library(tidyr)
#         library(purrr)
#         library(ggplot2)
#       })
#       eval(parse(text = code), envir = env)
#     }, error = function(e) {
#       showNotification("Error in code execution", type = "error")
#     })
#   })
# 
#   # ---------------------------------------------------------------------
# 
#   # UI - plot the column
#   output$pre_column_plot <- renderUI({
#     req(input$col_format)
#     req(input$temporal_resolution)
#     colname <- colnames(data()[column_num()])
#     output <- tagList()
#     output[[1]] <- h5("3. Visualise")
#     output[[2]] <- "Seeing the data visually makes it easier to understand the data. And also to find errors/outliers:"
#     output[[3]] <- tags$br()
# 
#     if (input$col_format == "cat"){
#       code_visualise <- paste0("ggplot(data, aes(x = ",colname,", fill=",colname,")) +
#   geom_bar() +
#   labs(title='Title', y='Count', x='",colname,"', fill='",colname,"')+
#   geom_text(stat = 'count', aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), '%'),
#                                        x = ",colname, "),
#                                    position = position_stack(vjust = 0.5)) +
#   annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4,
#             label = paste0(sum(is.na(data['",colname,"'])), ' NA values'))")
#     } else if (input$col_format == "dat"){
#       code_visualise <- paste0("ggplot(data, aes(x = ", colname, "_",input$temporal_resolution,")) +
#           geom_bar(stat = 'count', fill = 'skyblue') +
#           labs(x = 'Date', y = 'Count') +
#           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#           ggtitle('')")
#     } else if (input$col_format == "num"){
#       code_visualise <- paste0("ggplot(data, aes(x=",colname,"))+
#         geom_histogram(binwidth=1,fill = 'lightblue',color='black', alpha = 0.7)+
#         annotate('text',  x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4,
#                  label = paste0(sum(is.na(data$",colname,")), ' NA values')) +
#         labs(title='',x='', y='')+
#         theme_light()")
#     } else {
#       code_visualise <- "format is not correct"
#     }
# 
#     output[[4]] <- shinyAce::aceEditor(
#                         "code_pre_columns_visualise",
#                         mode = "r", theme="tomorrow", height = "140px",
#                         value = code_visualise
#                       )
#     output[[5]] <- actionButton("pre_colnames_plot", label = "Visualise data")
#     output
#   })
# 
#   # execute - plot the column
#   observeEvent(input$pre_colnames_plot, {
#     req(data())
#     code <- input$code_pre_columns_visualise
#     tryCatch({
#       env <- new.env()
#       env$input <- input
#       env$data <- data()
#       with(env, {
#         library(dplyr)
#         library(tidyr)
#         library(purrr)
#         library(ggplot2)
#       })
#       plot <- eval(parse(text = code), envir = env)
#       output$plot2 <- renderPlot({
#         plot
#       })
#       output$pre_columns_visualise_ui <- renderUI({
#         plotOutput("plot2")
#       })
#     }, error = function(e) {
#       showNotification("Error in code execution", type = "error")
#     })
#   })
#   
  # ---------------------------------------------------------------------
} 
  # download cleaned data
  output$download_clean <- downloadHandler(
    filename = function() {
      paste("CleanedData-", format(Sys.time(),"%Y%m%d_%H%M"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # download cleaned data report
  output$download_clean_report <- downloadHandler(
    filename = paste("CleanedData-", format(Sys.time(),"%Y%m%d_%H%M"), "report.pdf", sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
  
      params <- list(
        text_content = output$text()
      )
  
      render(tempReport, output_file = file,
             params = params,
             envir = new.env(parent = globalenv())
      )
    }
  )
  
  # if proceed to Aggregation button is clicked, then load data there
  
  go_aggregation <- reactiveVal(FALSE)
  observeEvent(input$go_preprocess_aggregation, {
    go_aggregation(TRUE)
    req(data())
    updateTabsetPanel(session, "tabs", "Aggregate")
  })
  
  #### Load data -------------------------------------
  
  # load data if needed
  output$agg_load <- renderUI({
    if (go_aggregation()) {
      helpText("The pre-processed data from the previous tab is automatically loaded.")
    } else {
      helpText("Each row should be a case, and the columns (with headers) are information about it.")
      fileInput("filemap", "", accept=c('.csv','.dbf','.dbc'), multiple= TRUE)
    }
  })
  
  
  #load epidata
  load_epidata <- reactive({
    inFile <- input$filemap
    if (is.null(inFile)){
      if (go_aggregation()){
        data()
      } else{
        return(NULL)
      }
    } else if (sub(".*\\.","", inFile$datapath) == "dbc"){
      read.dbc(inFile$datapath)
    } else if (sub(".*\\.","", inFile$datapath) == "dbf") {
      read.dbf(inFile$datapath, as.is = TRUE)
    } else if (sub(".*\\.","", inFile$datapath) == "csv") {
      read.csv(inFile$datapath)
    }
  })
  
  #give correct columnname choices in clean data section
  observe({
    x <- names(load_epidata())
    updateSelectInput(session, "NotificationDate",        choices = x,  selected = "-")
    updateSelectInput(session, "NotificationMunicipality",      choices = x,  selected = "-")
    updateSelectInput(session, "Sex",        choices = x,  selected = "-")
    updateSelectInput(session, "Cases",        choices = x,  selected = "-")
  })
  
  #clean data when button is clicked
  clean_data <- eventReactive(input$cleanbutton,{
    updateTabsetPanel(session, "tabs ",selected = "Aggregate")
  
    colnames <- c(if(input$NotificationDate != "")  input$NotificationDate,
                  if(input$NotificationMunicipality != "")  input$NotificationMunicipality,
                  if(input$Sex != "")  input$Sex,
                  if(input$Cases != "")  input$Cases)
  
    newnames <- c(if(input$NotificationDate != "")  "DT_NOTI",
                  if(input$NotificationMunicipality != "")  "MUNI_NOTI",
                  if(input$Sex != "")  "SEX",
                  if(input$Cases != "")  "CRIT")
  
    clean_epidata <- load_epidata()
    clean_epidata <- clean_epidata[,colnames]
    names(clean_epidata) <- newnames
    clean_epidata$DT_NOTI <- as.Date(clean_epidata$DT_NOTI)
    return(clean_epidata)
  })
  
  ##initialize reactive values
  rv <- reactiveValues("NotificationDate"=NULL,
                       "NotificationMunicipality"=NULL,
                       "Sex"=NULL,
                       "Cases"=NULL)
  
  #only show available agregation parameters
  output$gendercond2 <- reactive("SEX" %in% colnames(clean_data()))
  outputOptions(output, "gendercond2", suspendWhenHidden = FALSE)
  output$agecond <- reactive("AGE" %in% colnames(clean_data()))
  outputOptions(output, "agecond", suspendWhenHidden = FALSE)
  output$critcond <- reactive("CRIT" %in% colnames(clean_data()))
  outputOptions(output, "critcond", suspendWhenHidden = FALSE)
  output$conf1cond <- reactive("Confirmed" %in% input$Crit)
  outputOptions(output, "conf1cond", suspendWhenHidden = FALSE)
  output$conf2cond <- reactive("Suspected" %in% input$Crit)
  outputOptions(output, "conf2cond", suspendWhenHidden = FALSE)
  output$conf3cond <- reactive("Negative" %in% input$Crit)
  outputOptions(output, "conf3cond", suspendWhenHidden = FALSE)
  
  
  #show correct options available in agregation pull down menus
  observe({
    updateSelectInput(session, "Itime", choices = unique(lubridate::year(clean_data()$DT_NOTI)) ,  selected = "-")
    updateSelectInput(session, "Ftime", choices = unique(lubridate::year(clean_data()$DT_NOTI)),  selected = "-")
    updateSelectInput(session, "Tunit",     choices =  c("month"),  selected = "-") #add epiweek and year
    updateSelectInput(session, "Sunit",      choices = c(if ("MUNI_NOTI" %in% colnames(clean_data())) "Municipality",
                                                         if ("UF_NOTI" %in% colnames(clean_data())) "Region"),
                      selected = "-")
    updateSelectInput(session, "Sex2",        choices = c("all",unique(clean_data()$SEX)),  selected = "-")
    updateSelectInput(session, "ConfirmedCase",        choices = unique(clean_data()$CRIT),  selected = "-")
    updateSelectInput(session, "SuspectedCase",        choices = unique(clean_data()$CRIT),  selected = "-")
    updateSelectInput(session, "NegativeCase",        choices = unique(clean_data()$CRIT),  selected = "-")
  })
  observe({
    updateSelectInput(session, "Sloc",
                      choices = c("all",
                                  if (input$Sunit == "Municipality") unique(clean_data()$MUNI_NOTI),
                                  if (input$Sunit == "Region") unique(clean_data()$UF_NOTI)),
                      selected = "all")
  })
  
  #aggregarte data when button is clicked
  agg_data <- eventReactive(input$aggregatebutton,{
    updateTabsetPanel(session, "tabs ",selected = "Plot")
  
    epidata <- clean_data()
    if ("Confirmed" %in% input$Crit) { epidata$CRIT2[epidata$CRIT %in% input$ConfirmedCase] <- "Confirmed"}
    if ("Suspected" %in% input$Crit) { epidata$CRIT2[epidata$CRIT %in% input$SuspectedCase] <- "Suspected"}
    if ("Negative" %in% input$Crit) { epidata$CRIT2[epidata$CRIT %in% input$NegativeCase] <- "Negative"}
  
    agg_epidata <- aggregate_data(infile   = epidata,
                                  gender   = if ("SEX" %in% colnames(clean_data())) input$Sex2 else NULL,
                                  itime    = as.numeric(input$Itime),
                                  ftime    = as.numeric(input$Ftime),
                                  timetype = "noti",
                                  tunit    = input$Tunit,
                                  sunit    = if (input$Sunit == "Municipality") "MUNI" else "UF",
                                  sloc     = "all",#unique(clean_data()$MUNI_NOTI),
                                  spacetype= "N",
                                  crit     = if ("CRIT" %in% colnames(clean_data())) input$Crit else NULL)
  
    return(agg_epidata)
  })
  
  #only show available plotting parameters
  #  output$gendercond3 <- reactive("gender" %in% colnames(agg_data()))
  #  outputOptions(output, "gendercond3", suspendWhenHidden = FALSE)
  output$gendercond4 <- reactive("gender" %in% colnames(agg_data()))
  outputOptions(output, "gendercond4", suspendWhenHidden = FALSE)
  output$gendercond5 <- reactive("gender" %in% colnames(agg_data()))
  outputOptions(output, "gendercond5", suspendWhenHidden = FALSE)
  output$comparisonS <- reactive(input$comparison == "Sex")
  outputOptions(output, "comparisonS", suspendWhenHidden = FALSE)
  output$comparisonC <- reactive(input$comparison == "Cases")
  outputOptions(output, "comparisonC", suspendWhenHidden = FALSE)
  output$comparisonL <- reactive(input$comparison == "Location")
  outputOptions(output, "comparisonL", suspendWhenHidden = FALSE)
  output$statuscond5 <- reactive("crit" %in% colnames(agg_data()))
  outputOptions(output, "statuscond5", suspendWhenHidden = FALSE)
  
  # download the aggregated data
  output$downloadaggdata <- downloadHandler(
    filename = function() {
      paste("AggregatedData-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(agg_data(), file)
    }
  )
  
  #show correct options available in plotting pull down menus
  observe({
    temp1 <- agg_data()
    temp1$t_step <- format(as.Date(paste0(as.character(temp1$t_step), "-01"),  format = "%m-%Y-%d"), "%m-%Y")
    updateSliderInput(session, "Timestep",
                      min=as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      max=as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      value=c(as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                              as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d")),
                      timeFormat = "%m-%Y")
    updateSliderInput(session, "Timestep2",
                      min=as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      max=as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      value=c(as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d")),
                      timeFormat = "%m-%Y")
    updateSliderInput(session, "Timestep3",
                      min=as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      max=as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      value=c(as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                              as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d")),
                      timeFormat = "%m-%Y")
    updateSliderInput(session, "Timestep4",
                      min=as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      max=as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                      value=c(as.Date(paste0(min(temp1$t_step), "-01"),  format = "%m-%Y-%d"),
                              as.Date(paste0(max(temp1$t_step), "-01"),  format = "%m-%Y-%d")),
                      timeFormat = "%m-%Y")
  
    updateSelectInput(session, "Sex3", choices = as.character(unique(agg_data()$gender)) ,  selected = "-")
    updateSelectInput(session, "Sex4", choices = as.character(unique(agg_data()$gender)) ,  selected = "-")
    updateSelectInput(session, "Sex5", choices = as.character(unique(agg_data()$gender)) ,  selected = "-")
    # updateSelectInput(session, "country", choices=c(geodata::country_codes()$NAME), select="-" )
    updateSelectInput(session, "comparison", choices= c(if ("area_code" %in% colnames(agg_data())) "Location",
                                                        if ("age_grp" %in% colnames(agg_data())) "Age",
                                                        if ("gender" %in% colnames(agg_data())) "Sex",
                                                        if ("crit" %in% colnames(agg_data())) "Cases",
                                                        "None"),
                      selected = "None")
    updateSelectInput(session, "Sloc3",
                      choices = c(if (input$Sunit == "Municipality") unique(clean_data()$MUNI_NOTI),
                                  if (input$Sunit == "Region") unique(clean_data()$UF_NOTI)))
    updateSelectInput(session, "Sloc5",
                      choices = c(if (input$Sunit == "Municipality") unique(clean_data()$MUNI_NOTI),
                                  if (input$Sunit == "Region") unique(clean_data()$UF_NOTI)))
  
    updateSelectInput(session, "Status3", choices = unique(agg_data()$crit) ,  selected = "-")
    updateSelectInput(session, "Status5", choices = unique(agg_data()$crit) ,  selected = "-")
  })
  
  #calculates the timeseries when button is pressed
  ts <- eventReactive(input$tsbutton,{
    ts_data <- agg_data()
    ts_data$t_step <- as.Date(paste0(as.character(ts_data[,"t_step"]), "-01"),  format = "%m-%Y-%d")
    bla <- plot_ts(ts_data,
                   comparison=substr(input$comparison,1,1),
                   area = input$Sloc3,
                   timestep = seq(floor_date(input$Timestep[1],"month"), floor_date(input$Timestep[2],"month"), input$Tunit),
                   age = NULL,
                   gender= input$Sex3,
                   title="",
                   case= input$Cases3)
    return(bla)
  })
  
  #outputs the plot
  output$ts_plot <- renderPlot({
    ts()
  })
  
  # load population data
  load_popdata <- reactive({
    inFile <- input$filepop
    if (is.null(inFile)){
      return(NULL)
    } else if (sub(".*\\.","", inFile$datapath) == "dbc"){
      inFile <- read.dbc(inFile$datapath)
    } else if (sub(".*\\.","", inFile$datapath) == "dbf") {
      inFile <- read.dbf(inFile$datapath, as.is = TRUE)
    } else if (sub(".*\\.","", inFile$datapath) == "csv") {
      inFile <- read.csv(inFile$datapath)
    }
  
    colnames(inFile) <- c("area_code","pop")
    return(inFile)
  })
  
  #draws the map when button is pressed
  # map <- eventReactive(input$mapbutton,{
  #   map_data <- agg_data()
  #   map_data$t_step <- as.Date(paste0(as.character(map_data[,"t_step"]), "-01"),  format = "%m-%Y-%d")
  #
  #   plotted_map <- plot_map(infile = map_data,
  #                   popdata = load_popdata(),
  #                   UF_shp = geodata::gadm(geodata::country_codes()$ISO3[geodata::country_codes()$NAME == input$country], level=1, path="."),
  #                   muni_shp = geodata::gadm(geodata::country_codes()$ISO3[geodata::country_codes()$NAME == input$country], level=2, path="."),
  #                   type = input$abs_inc,
  #                   incidence= input$incidence_val,
  #                   #age=NULL,
  #                   gender= if ("gender" %in% colnames(map_data)) input$Sex4 else NULL,
  #                   timestep= if (input$tstep == "tperiod") seq(floor_date(input$Timestep3[1],"month"), floor_date(input$Timestep3[2],"month"), input$Tunit) else  floor_date(input$Timestep2, "month"))
  #   return(plotted_map)
  # })
  
  #outputs the plot
  # output$map_plot <- renderPlot({
  #   map()
  # })
  
  
  #fitlers the table correctly
  table <-  eventReactive( input$tablebutton,{
    table_data <- agg_data()
    if (!is.null(input$Sex5)){
      table_data <- table_data[table_data[["gender"]] %in% input$Sex5,]
    }
    if (!is.null(input$Sloc5)){
      table_data <- table_data[table_data[["area_code"]] %in% input$Sloc5,]
    }
    if (!is.null(input$Timestep4)){
      table_data$extra <- as.Date(paste0(as.character(table_data[,"t_step"]), "-01"),  format = "%m-%Y-%d")
      table_data <- table_data[table_data$extra >= input$Timestep4[1] & table_data$extra < input$Timestep4[2],]
      table_data <- subset(table_data, select=-c(extra))
    }
    return(table_data)
  })
  
  #outputs th table
  output$table_plot <- renderDataTable(datatable({
    table()
  }))

}

# Run the application 
shinyApp(ui = ui, server = server)



