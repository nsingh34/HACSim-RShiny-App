# This file defines the UI of the application
# client-side functionality

# UI is styled with CSS

# importing shiny library
library(shiny)

# importing shiny widgets
library(shinyWidgets)

# importing shiny css loaders
library(shinycssloaders)

# importing HACSim library
library(HACSim)

# importing ggplot2
library(ggplot2)

# defining the logo for the home page
logo <- a(href = "", img(src = "HACSim.png", alt = "HACSim: Haplotype Accumulation Curve Simulator", height = "100px",width="300px"))

tags$body(
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
  ),
  # navigation bar to navigate through page
  navbarPage(
    title = logo,
    selected = "Home",
    windowTitle = "HACSim: Haplotype Accumulation Curve Simulator",
    # App UI contents are chilling here!
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      )
    ),
    tabPanel(
      "Home",
      selected = TRUE,
      div(
        style = "position: fixed; left: 0; top: 0; z-index: -1;",
        img(src = "home_page.jpeg", style = "min-width: 100vw; min-height: 100vh;")
      ),
      tags$blockquote(h1("HACSim helps individuals to find required specimen sample sizes necessary for genetic diversity assessment
      !",style="font-size:250%; color:white;position:fixed;left:5%;top:40%;
                  right:35%"))
    ),
    tabPanel("About",
             div(tabsetPanel(tabPanel("HACSim?",
                                      tags$blockquote(h3("What is HACSim and how does it work?"),
                                                      p("HACSim is a novel nonparametric stochastic (Monte Carlo) local search optimization algorithm written in R 
                        for the simulation of haplotype accumulation curves. It can be employed to determine likely required 
                        sample sizes for DNA barcoding, specifically pertaining to recovery of total haplotype variation that may 
                        exist for a given species."),
                                                      p("Most DNA barcoding studies conducted to date suggest sampling between 5-10 individuals per 
                        species due to research costs. However, it has been shown that low sample sizes can greatly 
                        underestimate haplotype diversity for geograpically-widespread taxa. The present algorithm 
                        is in place to more accurately determine sample sizes that are needed to uncover all putative 
                        haplotypes that may exist for a given species. Implications of such an approach include 
                        accelerating the construction and growth of DNA barcode reference libraries for species of 
                        interest within the Barcode of Life Data Systems", (tags$a(href = "http://www.boldsystems.org", "(BOLD)")), 
                                                        "or similar database such as", (tags$a(href = "https://www.ncbi.nlm.nih.gov/genbank/", "GenBank."))),
                                                      p("Within the simulation algorithm, species haplotypes are treated as distinct character labels 
                        (1, 2, ...), where 1 denotes the most frequent haplotype, 2 denotes the second-most frequent 
                        haplotype, and so forth. The algorithm then randomly samples species haplotype labels in an 
                        iterative fashion, until all unique haplotypes have been observed. The idea is that levels of 
                        species haplotypic variation that are currently catalogued in BOLD can serve as proxies for 
                        total haplotype diversity that may exist for a given species."),
                                                      p("Molecular loci besides DNA barcode genes (5'-COI, rbcL/matK, ITS regions) can be used with HACSim 
                        (",em("e.g.,"),"cytb)."),style="font-size:120%; color:black;
                  ")),
                             tabPanel("More info",tags$blockquote(h3("More Information"), 
                                                                  p("Are you interested in doing even more with HACSim? Consider downloading the R package! See the HACSim", 
                                                                    tags$a(href = "https://cran.r-project.org/web/packages/HACSim/index.html", "CRAN"),
                                                                    "page for more details. You can also check out the HACSim R package repository on", 
                                                                    tags$a(href = "https://github.com/jphill01/HACSim.R", "GitHub.")),style="font-size:120%; color:black;
                  ")),
                             tabPanel(
                               "Author",
                               tags$blockquote(h3("Jarrett D. Phillips"),
                                               p("This is a placeholer and needs to be placed by something.....")),
                               tags$blockquote(h3("Navdeep Singh"),
                                               p("This is a placeholer and needs to be placed by something....."))
                             ),
                             tabPanel("Citation",
                                      tags$blockquote(h3(tags$a("Citation",href="Phillips et al. (2020).pdf")),
                                                      p(strong("Phillips, J.D.",),",", "French, S.H., Hanner, R.H. and  Gillis, D.J. (2020). HACSim: An 
                    R package to estimate intraspecific sample sizes for genetic diversity assessment 
                    using haplotype accumulation curves.",em("PeerJ Computer Science,"), strong("6"),"(192): 1-37.")
                                                      ,style="font-size:120%; color:black;"))),style="position:fixed;top:15%")
             
    ),
    tabPanel(
      div(
        actionBttn(
          inputId = "run_simulation",
          size = "sm",
          label ="Run Simulation",
          color = "warning",
          style = "material-flat",
          icon = icon("rocket"),
          block = TRUE
        )
      ),
      titlePanel("Genetic Diversity Assessment"),
      div(
        style = "position: fixed; left: 0; top: 0; z-index: -1;",
        img(src = "run_sim.jpg", style = "min-width: 100vw; min-height: 100vh;")
      ),
      sidebarLayout(
        tabsetPanel(
          type = "pills",
          tabPanel("Main interface",
                   selected = TRUE,
                     sidebarPanel(
                       width = 12,
                       numericInput(inputId = "perms",
                                    label = "Number of permutations (perms)",
                                    value = 10000,
                                    min = 2,
                                    step = 1),
                       
                       numericInput(inputId = "p",
                                    label = "Proportion of haplotypes to recover (p)",
                                    value = 0.95,
                                    min = 0,
                                    max = 1,
                                    step = 0.01), 
                       
                       numericInput(inputId = "conf.level",
                                    label = "Confidence level for calculations and plotting (conf.level)",
                                    value = 0.95,
                                    min = 0.01,
                                    max = 0.99,
                                    step = 0.01),
                       
                       actionBttn(
                         inputId = "run",
                         label ="Run",
                         color = "success",
                         style = "jelly",
                         size ="sm",
                         block = FALSE
                       )
                       
                     ) # end side bar Panel
                   
                   ),
          tabPanel("Sub interface",
                   sidebarPanel(
                     width = 12,
                     h3("Simulation type"),
                     switchInput(
                       inputId = "switch",
                       onLabel = "Real",
                       offLabel = "Hypothetical",
                       value = TRUE,
                       onStatus = "success", 
                       offStatus = "danger"
                     ),
                     conditionalPanel(condition = "input.switch == 1",
                                      fileInput(inputId = "file", 
                                                label = "Choose FASTA file",
                                                multiple = FALSE,
                                                accept = ".fas",
                                                buttonLabel = "Browse...",
                                                width = '20%',
                                                placeholder = "No file selected"
                                      ),
                                      
                                      checkboxInput(inputId = "subsampleseqs", 
                                                    label = "Subsample DNA sequences",
                                                    value = FALSE),
                                      
                                      conditionalPanel(condition = "input.subsampleseqs == 1",
                                                       numericInput(inputId = "prop",
                                                                    label = "Proportion of DNA sequences to subsample (prop.seqs)",
                                                                    value = NULL,
                                                                    min = 0,
                                                                    max = 1,
                                                                    step = 0.01),
                                                       
                                      ),
                                      helpText("Note: Inputted DNA sequences should not contain missing and/or ambiguous 
	                                       nucleotides, which may lead to overestimation of the number of 
	                                       observed unique haplotypes. Consider excluding sequences or alignment 
	                                       sites containing these data. If missing and/or ambiguous bases occur 
	                                       at the ends of sequences, further alignment trimming is an option.")
                     ),
                     conditionalPanel(condition = "input.switch != 1",
                                      numericInput(inputId = "N",
                                                   label = "Number of observed specimens (N)",
                                                   value = 100,
                                                   min = 2),
                                      
                                      numericInput(inputId = "Hstar",
                                                   label = "Number of observed haplotypes (H*)",
                                                   value = 5,
                                                   min = 1),
                                      
                                      textInput(inputId = "probs",
                                                label = "Haplotype frequency distribution (probs)",
                                                value = "0.20, 0.20, 0.20, 0.20, 0.20")
                                      ,
                                      
                                      checkboxInput(inputId = "subsampleseqs_2", 
                                                    label = "Subsample DNA sequences",
                                                    value = FALSE),
                                      
                                      conditionalPanel(condition = "input.subsampleseqs_2 == 1",
                                                       numericInput(inputId = "prop",
                                                                    label = "Proportion of DNA sequences to subsample (prop.seqs)",
                                                                    value = NULL,
                                                                    min = 0,
                                                                    max = 1,
                                                                    step = 0.01),
                                                       
                                      )
                     ))
                   ),
          sidebarPanel(
            width = 12,
            wellPanel(
              h3("Result Panel"),
              withSpinner(
                image = "loading.gif", image.width = 100, image.height = 100,
                plotOutput("plot")
              )
            )
          )
        ), # end condition panel
        
        mainPanel(
          textOutput("selected_var")
        )
      )
    )
  )
)
