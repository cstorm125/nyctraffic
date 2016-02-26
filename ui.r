
#Set initial date range
today<-Sys.Date()
mago<-today -months(1)

#Set header
header <- dashboardHeader(
    title = "NYC Traffic Incident"
)

body <- dashboardBody(
    fluidRow(
        column(width =4,
               tabBox(width=12,
               tabPanel('Map',
               box(width=NULL,status = "warning",
                   dateRangeInput("dates", 
                                  "Date range",
                                  start = as.character(mago), 
                                  end = as.character(today)),
                   sliderInput("times", "Time Range", 1, 
                               24,
                               value = c(1,24), step = 1
                   ),
                   sliderInput("injuries", "Injured", min(nycevents$NUMBER.OF.PERSONS.INJURED), 
                               max(nycevents$NUMBER.OF.PERSONS.INJURED),
                               value = range(nycevents$NUMBER.OF.PERSONS.INJURED), step = 1
                   ),
                   sliderInput("kills", "Killed", min(nycevents$NUMBER.OF.PERSONS.KILLED), 
                               max(nycevents$NUMBER.OF.PERSONS.KILLED),
                               value = range(nycevents$NUMBER.OF.PERSONS.KILLED), step = 1
                   ),
                   submitButton(text = "Apply Changes"))
                   ),
               tabPanel('Cause',
                        box(width=NULL,status = "warning",
                            selectInput("contrib", "Contributing factor",
                                        as.character(unique(nycevents$CONTRIBUTING.FACTOR.VEHICLE.1)),
                                        'Driver Inattention/Distraction',
                                        multiple = TRUE
                            ))
               ),
               tabPanel('Vehicle Type',
                        box(width=NULL,status = "warning",
                            selectInput("veh", "Vehicle Type",
                                        as.character(unique(nycevents$VEHICLE.TYPE.CODE.1)),
                                        'PASSENGER VEHICLE',
                                        multiple = TRUE
                                        
                            ))
               )
                   )),
        column(width = 8,
               tabBox(width=12,
               tabPanel('Map',
                   leafletOutput("map", height = 500)
               ),
               tabPanel('Summary',
               splitLayout(cellWidths = c("50%", "50%"), 
                           plotOutput("p1",height=250,width=250), 
                           plotOutput("p2",height=250,width=250)),
               splitLayout(cellWidths = c("50%", "50%"), 
                           plotOutput("p3",height=250,width=250), 
                           plotOutput("p4",height=250,width=250)),
               splitLayout(cellWidths = c("50%", "50%"), 
                           plotOutput("p5",height=250,width=250), 
                           plotOutput("p6",height=250,width=250))
               ),
               tabPanel('Trends',
               plotOutput('l1'),
               plotOutput('l2'),
               plotOutput('l3')
               )))
    ))

dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)