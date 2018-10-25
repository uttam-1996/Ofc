
dashboardPage(title = "CW Analytics",
              dashboardHeader(title = "OFC",titleWidth = "200px",
                              dropdownMenu(type=c("messages")),
                              dropdownMenu(type="notifications"),
                              dropdownMenu(type="tasks")
                              
              ),
              dashboardSidebar(disable = F,
                               sidebarMenu(id="tabs",
                                           menuItem("Sales Dashboard",tabName = "dash1", 
                                                    badgeLabel = "New",
                                                    badgeColor = "yellow",
                                                    icon=icon("product-hunt")),
                                           menuItem("Sales By Level",tabName = "dash2",
                                                    badgeLabel = "Sales",
                                                    badgeColor = "green",
                                                    icon = icon("rupee-sign")),
                                           menuItem("Finance Performance",
                                                    badgeLabel = "Finance",
                                                    badgeColor = "aqua",
                                                    icon = icon("arrow-alt-circle-up"))
                                           
                                           
                                           
                                           
                                           
                               )
                               
                               
                               
              ),
            
              dashboardBody(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                
                
                
                
                #start of dashboard 2
                
                
                
                tabItems(
                  tabItem(tabName = "dash2",
                          
                          fixedRow(
                            
                            column(2,
                                   uiOutput("d2client")  
                                   
                            ),
                            
                            
                            
                            
                            column(2,
                                   
                                   uiOutput("d2store")
                                   
                                   
                                   
                            ),
                            column(2,
                                   pickerInput("d2level",
                                               "Customer Level",
                                               choices = as.character(unique(ofc$level)),
                                               selected = as.character(unique(ofc$level)),
                                               multiple = TRUE,
                                               width = 150)
                                   
                                   
                            ),
                            column(2,
                                   pickerInput(
                                     inputId = "d2month",
                                     label = "Month",
                                     choices = as.character(levels(ofc$month)),
                                     multiple = T,
                                     selected = as.character(unique(ofc$month))
                                              )#Picker input 
                                   
                            ),#column
                            column(2,
                                   
                                   pickerInput(
                                     inputId = "d2year",
                                     label = "Year",
                                     choices = "2018",
                                     multiple = T,
                                     selected = "2018"))#column
          
                          ),#fluidrow End for inputs
                          
                          
                          #row start
                          fluidRow(
                            
                            infoBoxOutput("d2documentnumber"),
                            infoBoxOutput("d2netamount"),
                            infoBoxOutput("d2unique")
                          ),#fluidrow end for infobox
                          
                          #Rowstart
                          fluidRow(
                            column(6,
                                   plotlyOutput("d2graph1")
                                   ),#column end
                            
                            
                            column(6,
                                   plotlyOutput("d2graph3")
                                    )#columnEnd
                            ),#rowend
                          
                          #row Start Dash2
                          
                          fluidRow(
                            column(12,
                                   plotlyOutput("d2graph4")
                            )#columnEnd
                          )#rowend
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          ),#end of Dashboard of 2
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #start of dashboard1
                tabItem(tabName = "dash1",
                fixedRow(
                  
                  column(2,
                         uiOutput("d1client")  
                         
                  ),
                  
                  
                  
                  
                  column(2,
                         
                         uiOutput("d1store")
                         
                         
                         
                  ),
                  column(2,
                         uiOutput("d1weekday")
                         
                         
                  ),
                  column(2,
                         uiOutput("d1month")
                           
                           
                         
                         
                  ),
                  column(2,
                         
                         pickerInput(
                           inputId = "d1year",
                           label = "Year",
                           choices = "2018",
                           multiple = T,
                           selected = "2018"
                           
                           
                           
                           
                         )
                  )
                  
                  
                ),
                
                
                #infobox
                fluidRow(
                  
                  infoBoxOutput("d1documentnumber"),
                  infoBoxOutput("d1netamount"),
                  infoBoxOutput("d1unique")
                ),
                
                fluidRow(
                  column(8,
                         
                                              
                                              
                                              plotlyOutput("d1graph1")
                                              
                                              
                                              
                                     )
                                     
                                              
                                     
                                     
                                     
                        ,
                  # column(4,
                  #        tabsetPanel(type = "tabs",
                  #                    
                  #                    tabPanel("Plot",
                  #                             box(height=300,
                  #                                 width = 305,
                  #                                 uiOutput("graph2")
                  #                             )
                  
                  
                  #  ),
                  # tabPanel("table",
                  #          box(height = 300,
                  #              width = 305,
                  #              style = 'overflow-y:auto;max-height -300px;overflow-x:auto;max-height:300px',
                  #              tableOutput("Tab2")))
                  # 
                  
                  
                  
                  # )),
                  column(4,
                         
                                              
                                              plotlyOutput("d1graph3")
                                              
                                              
                                     )
                        
                  
                  
                  
                  
                ),
                
                fluidRow(),
                fluidRow(
                  column(12,
                         
                                              
                                              plotlyOutput("d1graph4")
                                     )
                                     
                                              )
                                     
                                     
                         )
                         
                         
                         
                  )
                  
                  # column(8,
                  #        tabsetPanel(type = "tabs",
                  #                    tabPanel("Plot",
                  #                             box(height = 450,
                  # 
                  #                                 uiOutput("graph5")
                  # 
                  # 
                  #                             )),
                  #                    tabPanel("table",
                  #                             box(height = 300,
                  #                                 width = 305,
                  #                                 style = 'overflow-y:auto;max-height -300px;overflow-x:auto;max-height:300px',
                  #                                 tableOutput("Tab5")%>%withSpinner(type = getOption("spinner.type", default = 6),
                  #                                                                   color = getOption("spinner.color", default = "#0275D8"),
                  #                                                                   size = getOption("spinner.size", default = 1),
                  #                                                                   color.background = getOption("spinner.color.background",default = "#34C510"),
                  #                                                                   custom.css = FALSE)
                  # 
                  #                             ))))
                  
                  
                  
              
              
              
              
              
              
              
              
              
              
              
              
                
                
                
                
           ))  
                
                
           
                
                




