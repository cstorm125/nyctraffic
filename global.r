library(lubridate) #for dates
library(shiny) #web app
library(leaflet) #for mapping
library(magrittr) #for %>%
library(dplyr) #for filtering
library(shinydashboard) #for dashboard ui
library(ggplot2) #graphs
library(grid) #remove whitespace around plots

#Download and read data
if (!file.exists('nycevents.csv')) {
    download.file(url='https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD',
                  dest='nycevents.csv')
    nycevents<-read.csv('nycevents.csv')
    #Get complete cases for latitude and longitude 755,762->636,727 observations
    nycevents<-nycevents[complete.cases(nycevents$LATITUDE) & complete.cases(nycevents$LONGITUDE),]
    #Convert date and time
    nycevents$DATE<- mdy(nycevents$DATE)
    nycevents$TIME<-hm(nycevents$TIME)
    #Make days of week
    nycevents$weekdays <-weekdays(nycevents$DATE)
    #Save to RDS for fast loading
    saveRDS(nycevents,file='nycevents.rds')
    #Aggregate all incidents
    all<-aggregate(TIME~DATE,data=nycevents,FUN=function(x) sum(!is.na(x)))
    colnames(all)<-c('Dates','Count')
    saveRDS(all,file='all.rds')
    #Agggregate by kills
    kill<-aggregate(NUMBER.OF.PERSONS.KILLED~DATE,data=nycevents,
                    FUN=sum)
    colnames(kill)<-c('Dates','Count')
    saveRDS(kill,file='kill.rds')
    #Aggregate by injuries
    inj<-aggregate(NUMBER.OF.PERSONS.INJURED~DATE,data=nycevents,
                    FUN=sum)
    colnames(inj)<-c('Dates','Count')
    saveRDS(inj,file='inj.rds')
}

#Read rds
nycevents<-readRDS('nycevents.rds')
all<-readRDS('all.rds')
kill<-readRDS('kill.rds')
inj<-readRDS('inj.rds')

#Make pop-up info
head<-'<b>Event Info</b>'
datetime <- paste(nycevents$DATE,nycevents$TIME, sep=' ')
contrib<-paste('Cause:',nycevents$CONTRIBUTING.FACTOR.VEHICLE.1,sep=' ')
veh <- paste('Vehicle Type:',nycevents$VEHICLE.TYPE.CODE.1,sep=' ')
injured <- paste('Injureded:',nycevents$NUMBER.OF.PERSONS.INJURED,sep=' ')
killed <- paste('Killed:',nycevents$NUMBER.OF.PERSONS.KILLED,sep=' ')
str<-paste(head,datetime,contrib,veh,injured,killed,sep='</br>')
nycevents$str<-str

