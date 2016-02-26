

shinyServer(
    function(input, output, session) {
        # Reactive expression for the data subsetted to what the user selected
        filteredData <- reactive({
            nycevents %>%
                filter(
                    DATE >= ymd(input$dates[1]),
                    DATE <= ymd(input$dates[2]),
                    TIME >= hm(paste(input$times[1],':00')),
                    TIME <= hm(paste(input$times[2],':00')),
                    NUMBER.OF.PERSONS.KILLED >= input$kills[1],
                    NUMBER.OF.PERSONS.KILLED <= input$kills[2],
                    NUMBER.OF.PERSONS.INJURED >= input$injuries[1],
                    NUMBER.OF.PERSONS.INJURED <= input$injuries[2],
                    CONTRIBUTING.FACTOR.VEHICLE.1==input$contrib,
                    VEHICLE.TYPE.CODE.1==input$veh
                )
        })
        #Pie charts
        #Weekdays
        output$p1 <- renderPlot({
            t1 <-as.data.frame(table(nycevents$weekdays))
            colnames(t1)<-c('Weekdays','Number')
            p1 <- ggplot(data=t1, aes(x=factor(""), y=Number, fill=Weekdays)) + 
                geom_bar(width = 1, stat = "identity")+ 
                coord_polar(theta = "y")+xlab('')+ylab('')+
                theme(plot.margin = unit(c(0,0,0,0), "mm"),axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      legend.title=element_blank())+
                ggtitle('By Weekdays')
            p1
        })
        #Boroughs
        output$p2 <- renderPlot({
            t2 <-as.data.frame(table(nycevents$BOROUGH))
            colnames(t2)<-c('Boroughs','Number')
            t2$Boroughs<-as.character(t2$Boroughs)
            t2[t2$Boroughs=="",]$Boroughs<-'OTHERS'
            p2 <- ggplot(data=t2, aes(x=factor(""), y=Number, fill=Boroughs)) + 
                geom_bar(width = 1, stat = "identity")+ 
                coord_polar(theta = "y")+xlab('')+ylab('')+
                theme(plot.margin = unit(c(0,0,0,0), "mm"),axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      legend.title=element_blank())+
                ggtitle('By Boroughs')
            p2
        })
        #Killed
        output$p3 <- renderPlot({
            t3 <-as.data.frame(rbind(c('Cyclists',sum(nycevents$NUMBER.OF.CYCLIST.KILLED)),
                       c('Motorists',sum(nycevents$NUMBER.OF.MOTORIST.KILLED)),
                       c('Pedestrians',sum(nycevents$NUMBER.OF.PEDESTRIANS.KILLED))))
            colnames(t3)<-c('Killed','Number')
            p3 <- ggplot(data=t3, aes(x=factor(""), y=Number, fill=Killed)) + 
                geom_bar(width = 1, stat = "identity")+ 
                coord_polar(theta = "y")+xlab('')+ylab('')+
                theme(plot.margin = unit(c(0,0,0,0), "mm"),axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      legend.title=element_blank())+
                ggtitle('By Type of Persons Killed')
            p3
        })
        #Injured
        output$p4 <- renderPlot({
            t4 <-as.data.frame(rbind(c('Cyclists',sum(nycevents$NUMBER.OF.CYCLIST.INJURED)),
                                     c('Motorists',sum(nycevents$NUMBER.OF.MOTORIST.INJURED)),
                                     c('Pedestrians',sum(nycevents$NUMBER.OF.PEDESTRIANS.INJURED))))
            colnames(t4)<-c('Injured','Number')
            p4 <- ggplot(data=t4, aes(x=factor(""), y=Number, fill=Injured)) + 
                geom_bar(width = 1, stat = "identity")+ 
                coord_polar(theta = "y")+xlab('')+ylab('')+
                theme(plot.margin = unit(c(0,0,0,0), "mm"),axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      legend.title=element_blank())+
                ggtitle('By Type of Persons Injured')
            p4
        })
        #Contrib
        output$p5 <- renderPlot({
            t5 <-as.data.frame(table(nycevents$CONTRIBUTING.FACTOR.VEHICLE.1))
            colnames(t5)<-c('Causes','Number')
            t5$Causes<-as.character(t5$Causes)
            t5[t5$Causes=="",]$Causes<-'Others'
            t5<-t5[order(-t5$Number),][1:5,]
            p5 <- ggplot(data=t5, aes(x=factor(""), y=Number, fill=Causes)) + 
                geom_bar(width = 1, stat = "identity")+xlab('')+ylab('')+
                theme(plot.margin = unit(c(0,0,0,0), "mm"),axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      legend.title=element_blank())+
                ggtitle('Top Five Causes')
            p5
        })
        #Vehicle Type
        output$p6 <- renderPlot({
            t6 <-as.data.frame(table(nycevents$VEHICLE.TYPE.CODE.1))
            colnames(t6)<-c('Vehicles','Number')
            t6$Vehicles<-as.character(t6$Vehicles)
            t6[t6$Vehicles=="",]$Vehicles<-'Others'
            t6<-t6[order(-t6$Number),][1:5,]
            p6 <- ggplot(data=t6, aes(x=factor(""), y=Number, fill=Vehicles)) + 
                geom_bar(width = 1, stat = "identity")+xlab('')+ylab('')+
                theme(plot.margin = unit(c(0,0,0,0), "mm"),axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      legend.title=element_blank(),
                      plot.title = element_text(hjust = 0))+
                ggtitle('Top Five Vehicle Types')
            p6
        })
        
        #Trends
        #Overall
        output$l1<-renderPlot({
            l1<-ggplot(data=all,aes(x=Dates,y=Count))+
                geom_line()+
                stat_smooth(method="glm",fullrange=TRUE)+
                xlim(min(all$Dates),max(all$Dates)+years(1))+
                ggtitle('All Traffic Incidents Trend')
            l1
        })
        #Killed
        output$l2<-renderPlot({
            l2<-ggplot(data=kill,aes(x=Dates,y=Count))+
                geom_line()+
                stat_smooth(method="glm",fullrange=TRUE)+
                xlim(min(all$Dates),max(all$Dates)+years(1))+
                ggtitle('Persons Killed Trend')
            l2
            
        })
        output$l3<-renderPlot({
            l3<-ggplot(data=inj,aes(x=Dates,y=Count))+
                geom_line()+
                stat_smooth(method="glm",fullrange=TRUE)+
                xlim(min(all$Dates),max(all$Dates)+years(1))+
                ggtitle('Persons Injured Trend')
            l3
            
        })
        
        #Output map
        output$map <- renderLeaflet({
            leaflet(nycevents) %>%
                #Set bound
                fitBounds(~min(nycevents$LONGITUDE), ~min(nycevents$LATITUDE), 
                          ~max(nycevents$LONGITUDE), ~max(nycevents$LATITUDE)) %>%
                addProviderTiles("CartoDB.Positron")
        })
        #Update markers according to reactive filtered data
        observe({
            leafletProxy('map', data = filteredData()) %>%
                clearMarkers() %>%
                addCircleMarkers(popup=~str, stroke=FALSE,color='red',radius=3,fillOpacity =0.5)})
    }
)