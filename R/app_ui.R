#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(

      ### Header ###

      dashboardHeader(title="Introduction to Epidemiological Modelling", titleWidth = 450,
                      dropdownMenuOutput("Progress")

      ),

      ### Sidebar ###

      dashboardSidebar(

        sidebarMenu(
          # sidebarSearchForm(textId='searchText',buttonId='searchButton',label='Search...'),
          # menuItem("Introduction",tabName='intro'),
          menuItem("Compartmental models",tabName="compmods",icon=icon("project-diagram")),
          menuItem("Numerical approximation",tabName='numapprox',icon=icon("chart-line")),
          menuItem("Spatial models",tabName="spatmods",icon=icon("globe-americas")),
          menuItem("Stochastic models",tabName="stocmods",icon=icon("dice")),
          # menuItem("Model fitting",tabName="modfit",icon=icon("chart-line")),
          br(),
          br(),
          bookmarkButton()
        )
      ),

      ### Body ###

      dashboardBody(

        tabItems(
          tabItem(tabName="compmods",
                  tabBox(title='Compartmental Models',width=12,height="900px",
                         tabPanel(
                           title='Overview',
                           column(width=12,
                                  box(width=12,"When modelling the spread of a disease, a common method is to use compartmental models. In a compartmental model, we split the population into different 'compartments' or 'states', each defined by a different property. For example, we may split the population into those who are currently susceptible to the disease and those who are currently infected with the disease. Note it is important that the compartments are mutually exclusive, that is, an individual can only belong to one compartment at a time.",br(),
                                      br(),
                                      "Once we have split the population into a set of mutually exclusive compartments, we can use differential equations to predict how the amount of individuals in each compartment will change. For example, individuals move out of the susceptible compartment when they become infected, or out of the infected compartment when they recover. The exact movements depend on the context and model we use.",br(),
                                      br(),
                                      "In this section we introduce 4 different compartmental models, increasing in complexity. For simplicity, all the models assume a constant population size and no demography (i.e. no births, deaths or migration)."
                                  ))
                         ),
                         tabPanel(
                           title='SIS model',
                           column(width=6,
                                  box(width=12, title="Summary",
                                      "The first compartmental model we introduce is an SIS model. In this model we split the population into two compartments:",br(),
                                      br(),
                                      "S: the proportion of the population that are currently susceptible to the disease, and", br(),
                                      "I: the proportion of the population that are currently infected with the disease.", br(),
                                      br(),
                                      "We define two parameters for this model:",br(),
                                      br(),
                                      div(HTML("&beta;: the ‘transmission rate’ of the disease, calculated by multiplying the average number of contacts per individual and the probability of transmission given a susceptible and infected individual come into contact. The rate at which inidividuals leave the susceptible compartment and join the infected compartment depends on this rate and the proportion of susceptibles and infecteds in the population (since more susceptibles/infecteds means it is more likely for the two to come in contact, and vice versa).")),br(),

                                      div(HTML("&gamma;: the ‘recovery rate’ from the disease, calculated as the reciprocal of the length of the infection (i.e. if the infection lasts 5 days, &gamma; = 1/5). For an SIS model, after an individual recovers they move back into the susceptible compartment, hence we assume they do not gain immunity to the disease (e.g. the common cold, chlamydia).")),br(),
                                      div(HTML("We represent this model mathematically using a system of Ordinary Differential Equations (ODEs), where each equation calculates the rate of change of one compartment. Since we are assuming a constant population size with no demography, the equations must sum to 0. Also, because each compartment represents a proportion, they must sum to 1.")), br(),
                                  )),
                           column(width=6,
                                  box(width=12, title='Model',
                                      div(img(src='SIS.png',width=250),style="text-align: center;"),
                                      br()),
                                  br(),
                                  box(width=12,title='ODEs',
                                      div(img(src='SISodes.png',width=250),style="text-align: center;"),
                                      br()),
                                  box(width=12,title='Conditions',
                                      div(img(src="SISconds.png",width=200),style="text-align: center;"),
                                      br()
                                  )
                           )),
                         tabPanel(
                           title='SIR model',
                           column(width=6,
                                  box(width=12,title='Summary',
                                      "To go from an SIS model to an SIR model, we add one more compartment, R, representing those that are 'recovered' or 'removed'. Thus we have three compartments:",br(),
                                      br(),
                                      "S: the proportion of the population that are currently susceptible to the disease,", br(),
                                      "I: the proportion of the population that are currently infected with the disease, and", br(),
                                      "R: the proportion of the population that have been infected but have now recovered, or died from the infection (removed).",br(),
                                      br(),
                                      "Individuals in the R compartment cannot be infected with the disease again, thus we are assuming that the immunity gained after recovering from the disease is indefinite.",br(),
                                      br(),
                                      div(HTML("This model depends on the same two parameters as the SIS model: the transmission rate (&beta;) and recovery rate (&gamma;). The only difference is that in an SIR model, when an infected individual recovers it moves to the R compartment, rather than back to the S compartment.")),br(),
                                      "The ODEs for the SIR model are subject to the same conditions as the SIS model, that is: the derivatives must sum to 0 (since we have a closed population) and the compartments sum to 1 (since they represent proportions).",br()
                                  )
                           ),
                           column(width=6,
                                  box(width=12,title="Model",
                                      div(img(src='SIR.png',width=400),style="text-align: center;"),
                                      br()
                                  ),
                                  br(),
                                  box(width=12,title='ODEs',
                                      div(img(src='SIRodes.png',width=280),style="text-align: center;"),
                                      br()
                                  ),
                                  br(),
                                  box(width=12,title='Conditions',
                                      div(img(src="SIRconds.png",width=280),style="text-align: center;"),
                                      br()
                                  )
                           )
                         ),
                         tabPanel(
                           title='SEIR model',
                           column(width=6,
                                  box(width=12,title='Summary',
                                      "There are many other compartments we could include in our model. One common one is an 'Exposed' compartment, E, used for diseases that have an incubation period during which an individual is infected, but not yet infectious (i.e. cannot transmit the disease to others). For such a model we have four compartments:",br(),
                                      br(),
                                      "S: the proportion of the population that are currently susceptible to the disease,", br(),
                                      "E: the proportion of the population that are currently infected with the disease, but not yet infectious,",br(),
                                      "I: the proportion of the population that are currently infectious, and", br(),
                                      "R: the proportion of the population that have been infected but have now recovered, or died from the infection (removed).",br(),
                                      br(),
                                      "In an SEIR model, to become infectious, susceptibles must first move through the exposed class. To model this we need to define another parameter:",br(),
                                      br(),
                                      div(HTML("&sigma;: the 'incubation rate', calculated as the reciprocal of the length of the incubation period.")),br(),
                                      "Note that, similar to an SIR model, we assume that once an individual has recovered it cannot be infected again (that is, individuals move from I to R). However, we could change this assumption and have individuals moving from I back to S; this would be called an SEIS model.",br(),
                                      br(),
                                      "Once again, the ODEs for the SEIR model are subject to the same conditions as the SIS and SIR model, that is: the derivatives must sum to 0 (since we have a closed population) and the compartments sum to 1 (since they represent proportions).",
                                      br()
                                  )
                           ),
                           column(width=6,
                                  box(width=12,title='Model',
                                      div(img(src='SEIR.png',width=400),style="text-align: center;"),
                                      br()
                                  ),
                                  br(),
                                  box(width=6,title='ODEs',
                                      div(img(src='SEIRodes.png',width=250),style="text-align: center;"),
                                      br(),
                                  ),
                                  box(width=6,title='Conditions',
                                      div(img(src="SEIRconds.png",width=250),style="text-align: center;"),
                                      br()
                                  )
                           )
                         ),
                         tabPanel(
                           title='SIRV model',
                           column(width=6,
                                  box(width=12,title='Summary',
                                      "We may also wish to model the effect of interventions, for example vaccination. To do so, we can introduce a 'Vaccinated' compartment, V, representing those individuals that have been vaccinated. If we assume the disease follows SIR dynamics, we end up with four compartments:",br(),
                                      br(),
                                      "S: the proportion of the population that are currently susceptible to the disease,", br(),
                                      "I: the proportion of the population that are currently infectious,", br(),
                                      "R: the proportion of the population that have been infected and recovered (or died), or gained immunity from vaccination, and",br(),
                                      "V: the proportion of the population that have been vaccinated but not yet gained immunity.",br(),
                                      br(),
                                      "Note that we are assuming some delay between vaccanation and immunity and the vaccine may also not be 100% effective, hence a vaccinated individual my never gain immunity. To model such a scenario we introduce three new parameters:",br(),
                                      br(),
                                      div(HTML("&nu;: the vaccination rate, as a proportion of the susceptible population per time step.")),br(),
                                      div(HTML("&delta;: the rate at which immunity is gained, calculated as the reciprocal of the length of the delay between vaccination and immunity.")),br(),
                                      div(HTML("&epsilon;: the vaccine efficacy, as a proportion of vaccines administered.")),
                                      br(),
                                      "In this set-up, a proportion of the vaccines administered (the vaccine efficacy) are deemed successful and move into the vaccinated class, whilst unsuccessful vaccinations remain susceptible. This is known as 'all-or-nothing' vaccination. However, there are other methods of incorporating vaccine efficacy, including 'leaky vaccination' for example."
                                  )
                           ),
                           column(width=6,
                                  box(width=12, title='Model',
                                      div(img(src='SIRV2.png',width=400),style="text-align: center;"),
                                      br()
                                  ),
                                  box(width=6,title='ODEs',
                                      div(img(src='SIRVodes.png',width=320),style="text-align: center;"),
                                      br()
                                  ),
                                  box(width=6, title='Conditions',
                                      div(img(src="SIRVconds.png",width=280),style="text-align: center;"),
                                      br()
                                  ),
                           )
                         ),
                         tabPanel(title='Simulations',
                                  column(width=8,
                                         box(width=12,title='Output',height="700px",
                                             plotOutput("plot1")
                                         ),
                                         box(width=12,title='Assumptions',
                                             "Initial infection in 0.5% of the population (I(0) = 0.005). The remainder of the population is assumed to be susceptible.")
                                  ),
                                  box(width=4,title='Options',
                                      radioButtons("modelselect","Select model:",choiceNames=c("SIS","SIR","SEIR","SIRV"),choiceValues=c(1,2,3,4),inline=TRUE),
                                      sliderInput("Lslider","Simulation length (days):",min=10,max=200,step=1,value=60),
                                      sliderInput("Bslider",div(HTML("Transmission rate (&beta;):")),min=0,max=5,step=0.1,value=1),
                                      sliderInput("Gslider",div(HTML("Recovery rate (&gamma;):")),min=0,max=1,step=0.05,value=0.2),
                                      sliderInput("Sslider",div(HTML("Incubation rate (&sigma;):")),min=0,max=1,step=0.05,value=0.2),
                                      sliderInput("Vslider",div(HTML("Vaccination rate (&nu;):")),min=0,max=1,step=0.05,value=0.1),
                                      sliderInput("Dslider",div(HTML("Immunity rate (&delta;):")),min=0,max=1,step=0.05,value=0.5),
                                      sliderInput("Eslider",div(HTML("Vaccine efficacy (&epsilon;):")),min=0,max=1,step=0.05,value=0.5)
                                  )
                         )
                  )
          ),

          tabItem(tabName="numapprox",
                  fluidRow(
                    column(width=6,
                           box(height="900px",
                               title='Solutions to ODEs', status='primary',
                               id='NAintrobox', width=NULL, collapsible=TRUE, collapsed=FALSE,
                               "In the previous section we introduced the idea of compartmental models and showed how we are able to represent them with a system of ordinary differential equations (ODEs). In the 'Simulations' tab we display graphs showing the value of each compartment over time. This is known as the 'solution' to the system. But how did we calculate it?",br(),
                               br(),
                               "In order to solve a system of differential equations, we need to find a function of each variable that satisfies the given equations. For example, for the SIS model, we must find functions S(t) and I(t) that satisfy the equations:",br(),
                               br(),
                               div(img(src='SISodes.png',width=150),style="text-align: center;"),br(),
                               "Or, in other words, find the value of S and I over time.",br(),
                               br(),
                               "For simple systems, we may be able to find an analytical solution. For example, consider the 1-dimensional system:",br(),
                               br(),
                               div(img(src='simpleode.png',width=200),style="text-align: center;"),br(),
                               "This can be solved by integrating both sides and substituting in the initial condition, giving:",br(),
                               br(),
                               div(img(src='simpleode_sol.png',width=100),style="text-align: center;"),br(),
                               "Or graphically:",br(),
                               div(img(src='simpleode_plot.png',width=300),style="text-align: center;"),
                               br(),
                               "However, this is often not possible with complex, multi-dimensional compartmental models. Hence, we resort to numerical approximations."
                           )
                    ),
                    column(width=6,
                           tabBox(height="900px",
                                  tabPanel(title="Euler's method",
                                           "One of the simplest numerical approximation methods is Euler's method, named after Leonhard Euler. It is described by the following algorithm:",br(),
                                           br(),
                                           "Next estimate = current estimate + slope from current estimate x time step,",br(),
                                           br(),
                                           "or, mathematically:",br(),
                                           div(img(src='eulersmethod.png',width=350),style="text-align: center;"),br(),
                                           "We code this method in R and compare it to the analytical solution for the simple, 1-D system introduced in the left-hand panel.",br(),
                                           br(),
                                           column(width=12,
                                                  column(width=6,
                                                         box(br(),plotOutput("plot2"),
                                                             width=NULL,height=250),
                                                         sliderInput("Tstepslider","Time step:",min=0,max=1,step=0.005,value=1)
                                                  ),
                                                  column(width=6,
                                                         img(src='eulersmethod_Rcode.png',width=300)
                                                  )),
                                           br(),
                                           br(),
                                           "Notice that, as we reduce the time step ('deltat'), the approximation becomes more accurate. However, this also increases the number of time steps and therefore the number of calculations required. For complex models, the size of time step required to obtain an accurate estimate may be so small that the computation time and space required to perform Euler's method is too great for the resources we have available. Therefore, we need more efficient methods..."
                                  ),
                                  tabPanel(title="R packages",
                                           "We saw that Euler's method may not provide an accurate approximation, especially for larger time steps. This can always be improved by making the time step smaller and smaller, however it is not long before we encounter problems regarding computational efficiency. Each time we halve the time step, we double the amount of time and memory required to compute the approximation. Much research has been done on developing efficient, accurate numerical approximation methods. One major advantage of using computing programmes such as R is that we are able to make use of these complex methods with relative ease.",br(),
                                           br(),
                                           "The 'deSolve' package in R (https://cran.r-project.org/web/packages/deSolve/index.html) contains many different numerical methods for solving a range of differential equations. Here, and from now on, we will use the 'lsoda()' function from this package to solve our systems of ODEs. The lsoda() function takes 4 inputs:",br(),
                                           br(),
                                           "1) func (function): a function containing the system of equations, taking 't', 'state' and 'parameters' as inputs and returning a list of derivatives,",br(),
                                           "2) y (vector): initial conditions/state, the initial value of our compartments,", br(),
                                           "3) parms (list): parameter values, and", br(),
                                           "4) times (vector): time points at which we want the solution.",br(),
                                           br(),
                                           "It returns an object containing the state values at each time point requested (i.e. the values of our compartments at each point in time).",br(),
                                           br(),
                                           "Below is an example of how we would implement this function in R to solve the same system as before.",br(),
                                           br(),
                                           div(fluidRow(
                                             img(src='lsodamethod.png',width=500),
                                             img(src='lsoda_out.png',width=150),style="text-align: center;")
                                           )
                                  ),
                                  title='Numerical approximations',id='NAbox',width=NULL
                           )
                    )
                  )
          ),

          tabItem(tabName="spatmods",
                  tabBox(title='Metapopulations models', width=12, height="900px",
                         tabPanel(title='Overview',
                                  box(width=12,
                                      "In all the models we have introduced so far (see Compartmental Models section), we have assumed 'homogeneous mixing'; that is, all individuals mix freely and there is an equal probability that any susceptible individual will be infected by any infectious individual, regardless of their location and the distance between them. This may be an acceptable assumption in a small, well mixed population, however, if we were modelling the spread of a disease across a whole country, this would certainly not be the case.",br(),
                                      br(),
                                      "There are many ways in which we can incorporate spatial location into epidemiological models. Here we introduce one such method: 'metapopulation' models.",br(),
                                      br(),
                                      "In a metapopulation model, the population is divided into groups, or 'patches', representing spatially distinct groups within the population. For example, patches may define cities within a country, households within a village, shoals of fish or herds of cattle. Within each patch, we assume that all individuals mix freely (as before) and model the dynamics of the disease using a compartmental model (e.g. SIR). However, there is also interaction between patches that can spread the disease from one patch to another. How we model this interaction depends on the context; we introduce 2 examples of interaction here: coupling and migration.",br(),
                                      br(),
                                      div(img(src='metapopcities.png',width=300),style="text-align: center;"),br(),
                                      "In the following models we change the definition of our compartments slightly. Where before 'S' represented the PROPORTION of the population that are susceptible, we now use it represent the NUMBER of individuals that are susceptible. We make this change since the number of individuals in each patch may be different and this can have significant effects on the spread of the disease. As a result of this change, the ODEs for an SIR model, for example, will look slightly different:",br(),
                                      br(),
                                      column(width=3),
                                      column(width=6,
                                             box(width=4,title='ODEs using proportions',
                                                 div(img(src='SIRodes.png',width=210),style="text-align: center;")
                                             ),
                                             column(width=4,
                                                    br(),
                                                    br(),
                                                    div(img(src='PtoNsub.png',width=230),style="text-align: center;")
                                             ),
                                             box(width=4,title='ODEs using numbers',
                                                 div(img(src='SIRodesN.png',width=200),style="text-align: center;")
                                             )
                                      ),
                                      column(width=12,
                                             "N represents the total population size, hence now S + I + R = N (where before they summed to 1)."
                                      )
                                  )),
                         tabPanel(title='Coupling',
                                  column(width=6,
                                         box(width=12,title='Summary',
                                             "The first, and simplest, description of interaction between patches is via 'coupling'. For this model we define a coupling parameter:",br(),
                                             br(),
                                             div(HTML("&rho;<sub>ij</sub>: the strength of interaction from patch j to patch i, relative to the within patch force of infection.")),br(),
                                             div(HTML("For example, if &rho;<sub>12</sub> = 0.1, then an infected individual in patch 2 will transmit the infection to susceptible individuals in patch 1 at a rate 10x less than to individuals in its own patch.")),br(),
                                             div(img(src='coupling.png',width=500),style="text-align: center;"),br(),
                                             "This type of representation is appropriate for modelling vector-borne pathogens, where the disease is spread between patches via a vector (including water and wind) rather than via the movement of the individuals themselves (which is usually modelled explicitly; we introduce this in the next section). However, it can also be used to approximate the latter in some scenarios (e.g. very short term commuting between comparably sized populations), which can help to reduce complexity and improve efficiency.",br(),
                                             br(),
                                             "The image above and model schematic on the right show an example of coupling interaction between two patches, however this can easily be extended to incorporate any number of patches.",br(),
                                             br(),
                                             "Throughout this section, we assume that the within-patch dynamics follow SIR dynamics.",br(),
                                             br(),
                                             div(HTML("Note in the ODEs we have an N<sub>i</sub> in the denominator of the transmission term for infection in patch i. This means we are assuming that, although the infection may be coming from another patch, the infection event itself occurs within patch i. For example, an infected person moves from its own patch to patch i and infects someone, before moving back. Or, spores from an infected plant are blown into another patch by the wind, where they infect other plants."))
                                         )),
                                  column(width=6,
                                         box(width=12,title='Model (2 patches)',
                                             div(img(src='coupling_odespic.png',width=360),style="text-align: center;"),br()
                                         ),
                                         box(width=12,title='ODEs (2 patches)',
                                             div(img(src='coupling_odes.png',width=500),style="text-align: center;")
                                         ),
                                         box(width=12,title='ODEs (n patches)',
                                             column(width=6,div(img(src='coupling_odes_npatch.png',width=200),style="text-align: center;")),
                                             column(width=6,br(),br(),div(img(src='coupling_odes_npatchconds.png',width=280),style="text-align: center;"))
                                         )
                                  )
                         ),
                         tabPanel(title='Migration',
                                  column(width=6,
                                         box(width=12, title='Summary',
                                             "When defining the interaction between patches via coupling, we use the 'relative strength of interaction' between patches, compared to within. However, this doesn't model the mechanics underlying the interaction, for example the movements of individuals between patches. We now introduce a 'mechanistic' metapopulation model, that can be used in contexts where individuals make long-term movements between patches, for example migrating animals or livestock herds.",br(),
                                             br(),
                                             "To model this movement, we introduce a 'migration' parameter:",br(),
                                             br(),
                                             div(HTML("m<sub>ij</sub>: the migration rate from patch j to patch i, as a proportion of the population in patch j.")),br(),
                                             div(img(src='migration.png',width=500),style="text-align: center;"),br(),
                                             "The image above and model schematic on the right show an example of migration interaction between two patches, however this can easily be extended to incorporate any number of patches. We continue with the assumption that within each patch the disease follows SIR dynamics."
                                         )
                                  ),
                                  column(width=6,
                                         box(width=12,title='Model (2 patches)',
                                             div(img(src='migration_odespic.png',width=360),style="text-align: center;"),br(),
                                         ),
                                         box(width=12,title='ODEs (2 patches)',
                                             div(img(src='migration_odes.png',width=570),style="text-align: center;")
                                         ),
                                         box(width=12,title='ODEs (n patches)',
                                             column(width=6,div(img(src='migration_odes_npatch.png',width=330),style="text-align: center;")),
                                             column(width=6,div(img(src='migration_odes_npatchconds.png',width=200),style="text-align: center;"))
                                         )
                                  )
                         ),
                         tabPanel(title='Simulations',
                                  fluidRow(
                                    box(title='Coupling model',width=6,height=370,plotOutput("plot3")),
                                    box(title='Migration model',width=6,height=370,plotOutput("plot4"))
                                  ),
                                  box(width=8,
                                      column(width=5,
                                             radioButtons("plotPop","Select compartment:",choiceNames=c("S","I","R","N"),choiceValues=c(1,2,3,4),inline=TRUE),
                                             column(width=6,radioButtons("npops",div(HTML("Number of patches:")),choiceNames=c("1","2","3","4"),choiceValues=c(1,2,3,4),inline=TRUE)),
                                             column(width=6,radioButtons("infectionlocation",div(HTML("First infected patch:")),choiceNames=c("1","2","3","4"),choiceValues=c(1,2,3,4),inline=TRUE)),
                                             sliderInput("interaction",div(HTML("Coupling parameter:")),min=0,max=0.2,step=0.001,value=0.1),
                                             sliderInput("interaction2",div(HTML("Migration rate:")),min=0,max=0.2,step=0.001,value=0.1),
                                             radioButtons("popdistances","Interaction strength:",choiceNames=c("Fixed","By distance"),choiceValues=c(1,2),inline=TRUE)
                                      ),
                                      box(width=7,title='Patch sizes:',
                                          column(width=3,sliderInput("patchsize1",div(HTML("1")),min=50,max=1000,step=50,value=500)),
                                          column(width=3,sliderInput("patchsize2",div(HTML("2")),min=50,max=1000,step=50,value=500)),
                                          column(width=3,sliderInput("patchsize3",div(HTML("3")),min=50,max=1000,step=50,value=500)),
                                          column(width=3,sliderInput("patchsize4",div(HTML("4")),min=50,max=1000,step=50,value=500))

                                      ),
                                      box(width=7,title='Patch locations:',
                                          column(width=3,sliderInput("patchloc1",div(HTML("1")),min=1,max=10,step=1,value=1)),
                                          column(width=3,sliderInput("patchloc2",div(HTML("2")),min=1,max=10,step=1,value=2)),
                                          column(width=3,sliderInput("patchloc3",div(HTML("3")),min=1,max=10,step=1,value=3)),
                                          column(width=3,sliderInput("patchloc4",div(HTML("4")),min=1,max=10,step=1,value=4))

                                      )
                                  ),
                                  box(width=4,title='Note on interaction strength',
                                      "When calculating interaction strength 'by distance', we scale the interaction parameter by the reciprocal of the distance between patches. The distance between two patches is the absolute difference in their location numbers. For example, if two patches are at locations 1 and 5, the distance between them is 4 and the strength of interaction between them will be the interaction parameter multiplied by 1/4. If two patches are at the same location, we assume an interaction parameter of 1."
                                  ),
                                  box(width=4,title='Assumptions',
                                      "Initial infection in 10 individuals (I(0) = 10), in the selected patch. Other patches are completely susceptible.",br(),
                                      br(),
                                      "Within-patch SIR parameters fixed at:",br(),
                                      div(HTML("&beta; = 0.4,<br> &gamma; = 1/5"),style="text-align: center;")
                                  )
                         )
                  )

          ),

          tabItem(tabName="stocmods",
                  tabBox(title='Stochastic Models',width=12,height="900px",
                         tabPanel(title='Overview',
                                  box(width=12,
                                      "The models considered so far have all been DETERMINISTIC. This means that, given the same
                        parameters and starting conditions, we will obtain the same, single solution every time.
                        This is not the case in real life. Even if we were to observe two epidemics with the exact same
                        transmission and recovery rates, we would not expect to observe exactly the same people becoming
                        infected at exactly the same times, since there is always some level of chance involved, whether
                        that be from external, environmental factors or differences in individual immunity. This is especially
                        important when there is only a small number of infectious individuals, for example in small populations, during the early stages of
                        an outbreak or when the disease is near eradication. Stochastic models attempt to incorporate this randomness
                        in order to better represent the real-world behaviour of disease spread.",br(),
                                      br(),
                                      "There are various ways in which we can incorporate stochasticity into our models, that fall into three main groups:",br(),
                                      "1) adding noise directly to the number of individuals observed in each compartment (observational noise),",br(),
                                      "2) adding noise to the parameters within our model (process noise), or",br(),
                                      "3) explicitly modelling the random events (event-driven approaches).",br(),
                                      br(),
                                      "We give an overview of each of these methods in this section."
                                  )),
                         tabPanel(title='Observational noise',
                                  box(width=6,title='Summary',
                                      "The first and simplest method of incorporating stochasticity is by adding observational noise. In this method, the
                       underlying dynamics of the disease itself remain deterministic (e.g. using the deterministic SIR model from the Compartmental Models section), however we assume there is some error introduced when observing the presence of the disease in the population.
                        This would most commonly arise from incomplete reporting of cases (not all infected individuals are
                       reported) or mis-diagnosis of cases (when a healthy individual is reported as infected).",br(),
                                      br(),
                                      "To model these errors we introduce two new parameters:",br(),
                                      br(),
                                      div(HTML("p<sub>r</sub>: the probability that an infected individual is reported as such, and")),br(),
                                      div(HTML("p<sub>m</sub>: the probability that a healthy individual is mis-diagnosed as infected.")),br(),
                                      "Using these two parameters, we add noise directly to the number of infected individuals using a
                       sum of binomial distributions:",br(),
                                      br(),
                                      div(img(src='obsnoise.png',width=450),style="text-align: center;"),br(),
                                      br(),
                                      "This calculation is performed for a specified observation interval (i.e. we observe a reported value of I daily/weekly/fortnightly etc).",br(),
                                      br(),
                                      div(HTML("If p<sub>r</sub> = 1 and p<sub>m</sub> = 0, then the reported number of infected individuals will be the same as the true number. Otherwise, as p<sub>r</sub> decreases, the reported number of infecteds decreases, and as p<sub>m</sub> increases, the reported number of infecteds increases.")),br(),
                                      div(HTML("On the right-hand side, we give an example of how this procedure may be implemented in an SIR setting, with p<sub>r</sub> = 0.9, p<sub>m</sub> = 0.1 and daily observations."))
                                  ),
                                  box(width=6,title='Example for SIR model',
                                      div(img(src='obsnoiseexample.png',width=450),style="text-align: center;")
                                  )),
                         tabPanel(title='Process noise',
                                  box(width=6,title='Summary',
                                      "A more realistic method of incorporating stochasticity is to include it in the differential equations, thereby altering the
                       spread of the disease itself, rather than just how we observe it. In this case, the dynamics of spread at each time point are subject to random variation,
                       hence parameters such as transmission or recovery will fluctuate randomly over time. In reality, this may be caused by
                       environmental conditions (e.g. fluctuations in temperature or humidity) or individual variation caused by different
                       patterns of contacts or immunological response.",br(),
                                      br(),
                                      "When adding process noise, we must change the differential equations themselves. For an SIR model, we add stochasticity
                       to both the transmission and recovery rates. To do so, we introduce two new parameters:",br(),
                                      br(),
                                      div(HTML("&sigma;<sub>&epsilon;</sub>: the amount of noise present in transmission, and")),br(),
                                      div(HTML("&sigma;<sub>&xi;</sub>: the amount of noise present in recovery.")),br(),
                                      "These parameters are used as variance parameters in normally distributed scaling terms applied to their respective disease parameters. That is, at each time step, we use them to generate normal random numbers centred around 0:",br(),
                                      br(),
                                      div(img(src='processnoise2.png',width=150),style="text-align: center;"),br(),
                                      "which are then used to scale the transmission and recovery parameters for that time step:",br(),
                                      br(),
                                      div(img(src='processnoise3.png',width=150),style="text-align: center;"),br(),
                                      br(),
                                      "This is repeated for each time step, so that the overall transmission and recovery terms fluctuate throughout the epidemic. Note that here we are assuming constant variance throughout (constant 'amount' of noise), however this may not be the case in real life."
                                  ),
                                  box(width=6,title="Model",
                                      div(img(src='SIRprocess.png',width=450),style="text-align: center;"),br()
                                  ),
                                  box(width=6,title='ODEs',
                                      div(img(src='processnoise.png',width=400),style="text-align: center;"),br()
                                  ),
                                  box(width=6,title='Stochastic parameters',
                                      div(img(src='processnoise2.png',width=200),style="text-align: center;"),br()
                                  )
                         ),
                         tabPanel(title='Event driven',
                                  column(width=6,box(width=12,title='Summary',
                                                     "Event-driven approaches have become the most popular method of representing stochasticity in epidemiological models. Such methods
                       require significantly changing the method of simulation, which is no longer a simple numerical approximation of a system
                       of ODEs, in order to explicitly model the individual 'events' that can occur over time. For an SIR model without demography,
                       (no births or deaths), there are two events that can occur at any point in time:",br(),
                                                     br(),
                                                     div(HTML("1) transmission - occurs at a rate &beta;SI/N - results in S&rarr;S-1, I&rarr;I+1,")),br(),
                                                     div(HTML("2) recovery - occurs at a rate &gamma;I - results in I&rarr;I-1, R&rarr;R+1.")),br(),
                                                     div(HTML("We know that, at any point in time, transmission occurs at a rate  &beta;SI/N (from the ODEs of an SIR model). We also know that, if transmission occurs, one individual must move from the susceptible compartment to the infected compartment. Hence  S&rarr;S-1 and I&rarr;I+1.")),br(),
                                                     div(HTML("Similarly, we know that recovery occurs at a rate &gamma;I, and if recovery occurs one individual must move from the infected compartment to the recovered compartment, hence I&rarr;I-1 and R&rarr;R+1.")),br(),
                                                     "By simulating the repeated occurence of these two events over time, we can obtain a stochastic solution to the SIR model.",br(),
                                                     br(),
                                                     "There are many different ways to model these events over time. Here, we introduce a popular method known as
                       the Gillespie Algorithm. The full algorithm for a general system and for an SIR epidemiological system is provided on the right. Note that RAND stands for a uniform random number between 0 and 1.",br(),
                                                     br(),
                                                     "Note that, in contrast to the simulation methods we have used so far, this method only allows integer values for the compartments. That is, we are now dealing explicitly with the movements of individuals between compartments. "
                                  )),
                                  column(width=6,box(width=12,title='Gillespie Algorithm (general)',
                                                     div(img(src='gillespie.png',width=500),style="text-align: center;")
                                  ),
                                  box(width=12,title='Gillespie Algorithm (SIR model)',
                                      div(img(src='SIRgillespie.png',width=450),style="text-align: center;")
                                  )
                                  )),
                         tabPanel(title='Simulations',
                                  box(title='Observational noise',width=4,height=320,plotOutput("plot5"),
                                      column(width=6,
                                             sliderInput("popsizeStoch","Population size:",min=50,max=5000,step=10,value=500),
                                             sliderInput("Nsims3","Number of simulations:",min=1,max=100,step=1,value=1),
                                             sliderInput("ObsFreq","Frequency of observations (days):",min=1,max=20,step=1,value=1)
                                      ),
                                      column(width=6,
                                             sliderInput("Pr",div(HTML("p<sub>r</sub>:")),min=0,max=1,step=0.001,value=1),
                                             sliderInput("Pm",div(HTML("p<sub>m</sub>:")),min=0,max=0.1,step=0.0001,value=0)                                  )
                                  ),
                                  box(title='Process noise',width=4,height=320,plotOutput("plot6"),
                                      column(width=6,
                                             sliderInput("popsizeStoch2","Population size:",min=50,max=5000,step=10,value=500),
                                             sliderInput("Nsims","Number of simulations:",min=1,max=100,step=1,value=1)
                                      ),
                                      column(width=6,
                                             sliderInput("sigma_b",div(HTML("&sigma;<sub>&epsilon;</sub>:")),min=0,max=3,step=0.01,value=1),
                                             sliderInput("sigma_r",div(HTML("&sigma;<sub>&xi;</sub>:")),min=0,max=3,step=0.01,value=1)                                  )
                                  ),
                                  box(title='Event-driven',width=4,height=320,plotOutput("plot7"),
                                      column(width=6,
                                             sliderInput("popsizeStoch3","Population size:",min=50,max=5000,step=10,value=500)
                                      ),
                                      column(width=6,
                                             sliderInput("Nsims2","Number of simulations:",min=1,max=100,step=1,value=1)                                 )
                                  ),
                         )
                  )
          )


          # tabItem(tabName="modfit",
          #         fluidRow(
          #           column(width=6,
          #                  box(
          #                    title='Model fitting', solidHeader = TRUE, status='danger',
          #                    id='MFintrobox', width=NULL, collapsible=TRUE, collapsed=FALSE,
          #                    "When modelling the spread of a disease, a common method is to use compartmental models. In a compartmental model, we split the population into different 'compartments' or 'states', each defined by a different property. For example, we may split the population into those who are currently susceptible to the disease and those who are currently infected with the disease. Note it is important that the compartments are mutually exclusive, that is, an individual can only belong to one compartment at a time.",br(),
          #                    "Once we have split the population into a set of mutually exclusive compartments, we can use differential equations to predict how the amount of individuals in each compartment will change. For example, individuals move out of the susceptible compartment when they become infected, or out of the infected compartment when they recover. The exact movements depend on the context and model we use.",br(),
          #                    "Below we introduce 4 different compartmental models, increasing in complexity. For simplicity, all these models assume a constant population size and no demography (i.e. no births, deaths or migration)."
          #                  ),
          #                  box(
          #                    title='Least squares', solidHeader = TRUE, status='danger',
          #                    id='processbox', width=NULL, collapsible=TRUE, collapsed=TRUE,
          #                    "Least squares stuff"
          #                  ),
          #                  box(
          #                    title='ABC', solidHeader = TRUE, status='danger',
          #                    id='eventbox', width=NULL, collapsible=TRUE, collapsed=TRUE,
          #                    "ABC stuff"
          #                  ),
          #                  box(
          #                    title='MCMC', solidHeader = TRUE, status='danger',
          #                    id='eventbox', width=NULL, collapsible=TRUE, collapsed=TRUE,
          #                    "MCMC stuff"
          #                  )
          #           ),
          #           column(width=6,
          #                  box(plotOutput("plot6"), title='Simulations',
          #                      width=NULL, solidHeader=TRUE, status='danger',height=360
          #                  ),
          #                  box(status='danger',width=4,
          #                      radioButtons("modelselect","Select model:",choiceNames=c("Coupling","Migration","Commuting"),choiceValues=c(1,2,3),inline=TRUE),
          #                      sliderInput("Lslider","Simulation length (days):",min=10,max=200,step=1,value=60)
          #                  ),
          #                  box(status='danger',width=4,
          #                      sliderInput("Bslider",div(HTML("Transmission rate (&beta;):")),min=0,max=5,step=0.1,value=1),
          #                      sliderInput("Gslider",div(HTML("Recovery rate (&gamma;):")),min=0,max=1,step=0.05,value=0.2),
          #                      sliderInput("Sslider",div(HTML("Incubation rate (&sigma;):")),min=0,max=1,step=0.05,value=0.2)
          #                  ),
          #                  box(status='danger',width=4,
          #                      sliderInput("Vslider",div(HTML("Vaccination rate (&nu;):")),min=0,max=1,step=0.05,value=0.1),
          #                      sliderInput("Dslider",div(HTML("Immunity rate (&delta;):")),min=0,max=1,step=0.05,value=0.5),
          #                      sliderInput("Eslider",div(HTML("Vaccine efficacy (&epsilon;):")),min=0,max=1,step=0.05,value=0.5)
          #                  )
          #           )
          #         )
          # )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "epidemodeller"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
