#Let's import the library.
install.packages("package:base")
install.packages("package:stats")
install.packages("gridExtra")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(reshape2)
library(shiny)
library(gridExtra)


#Load our data set
area = read.csv("CrimeData.csv",header = T, check.names = F)

######################### Data Prepration ###############################
weg = function(x){
  
  ifelse(grepl("of",x),
         paste(toupper(substr(x,1,1)),
               substr(x,2,regexpr("of",x)+2),
               toupper(substr(x,regexpr("of",x)+3,regexpr("of",x)+3)),
               tolower(substr(x,regexpr("of",x)+4,nchar(x))),sep = ""), 
         ifelse(grepl("and",x),
                paste(toupper(substr(x,1,1)),
                      substr(x,2,regexpr("and",x)+3),
                      toupper(substr(x,regexpr("and",x)+4,regexpr("and",x)+4)),
                      tolower(substr(x,regexpr("and",x)+5,nchar(x))),sep = ""), 
                ifelse(grepl("upon",x),
                       paste(toupper(substr(x,1,1)),
                             substr(x,2,regexpr("and",x)+4),
                             toupper(substr(x,regexpr("and",x)+5,regexpr("and",x)+5)),
                             tolower(substr(x,regexpr("and",x)+6,nchar(x))),sep = ""),      
                       
                       paste(toupper(substr(x,1,1)),substr(x,2,nchar(x)),sep = "")
                )))
  
}

#Extract location of London and all those which are not the part of london we will remove them.

#Let's find 0 in the postcode using regexpr.

area$find = regexpr(0,area$`LSOA name`)

#Lets remove the data outside london.
area = area %>% subset(find != -1)

#Now let's save the Local areas of London.
area$loc.name = area$`LSOA name` %>% substr(1,area$find-2) %>% tolower()
London = c(
  "Camden",
  "City of London",
  "Hackney",
  "Hammersmith and Fulham",
  "Haringey",
  "Islington",
  "Kensington and Chelsea",
  "Lambeth",
  "Lewisham",
  "Newham",
  "Southwark",
  "Tower Hamlets",
  "Wandsworth",
  "Westminster",
  "Barking and Dagenham",
  "Barnet",
  "Bexley",
  "Brent",
  "Bromley",
  "Croydon",
  "Ealing",
  "Enfield",
  "Greenwich",
  "Harrow",
  "Havering",
  "Hillingdon",
  "Hounslow",
  "Kingston upon Thames",
  "Merton",
  "Redbridge",
  "Richmond upon Thames",
  "Sutton",
  "Waltham Forest"
) %>% sort() %>% tolower()

area$loc.name = tolower(area$loc.name)
area = area %>% subset(loc.name %in% London)
area$loc.name = area$loc.name %>% weg()

#Lets set the Months

area$Month.factor = area$Month
area$Month = paste(area$Month,"01",sep="-") %>% as.Date("%Y-%m-%d")
colnames(area)[match("Crime type",colnames(area))] = "Crime.type"


#Lets extract the the Clean data of london into new csv for further use.
write.csv(area,"londonareaclean.csv",row.names = F)

###################### Data Visualisation #################################### 


#First read our cleaned data set.

area = read.csv("londonareaclean.csv",header = T, check.names = F)
area$Month = area$Month %>% as.Date("%Y-%m-%d")

#Let's change the slider into the dates. So it become easy for us to filter data.  

datetransfrom = function(x){
  x = as.POSIXlt(x)
  x$mday = "01"
  x = as.Date(x)
}


# Let's make our chart tidy.

Chart_set = theme(panel.background = element_blank(), axis.line = element_line("grey"),axis.ticks = element_blank())

# Input some colors to our map.

map_color = brewer.pal(8,"Set2")
map_color = colorRampPalette(map_color)(area$Crime.type %>% unique() %>% length())
pal = colorFactor(map_color,unique(area$Crime.type))



################ Dashboard using Shiny ################

#Let's setup our User interface
ui = 
  
  fluidPage(
    
    fluidRow(
      column(6,"Check my place in London",style = "font-size: 40pt"),
      column(2,sliderInput(inputId = "month",label = "Month",min = min(area$Month),max = max(area$Month),value = c(as.Date("2017-01-01")
                                                                                                                   ,max(area$Month)), timeFormat = "%b %Y")),
      column(2,selectInput(inputId = "crime.type", label = "Crime Type",choices = unique(area$Crime.type)%>% sort(),selected = "Anti-social behaviour",multiple = T)),
      column(2,selectInput(inputId = "area", label = "Area", choices = unique(area$loc.name) %>% sort(),selected = "Camden",multiple = T))),
    fluidRow(
      column(7,leafletOutput("map", height = 800)),
      fluidRow(
        column(5,plotOutput("plot1")),
        column(5,plotOutput("plot2"))
      )))


#Let's setup our server
server = function(input,output) {
  
  sliderDate = reactiveValues()
  
  # slider Date
  observe({
    sliderDate$date1 = datetransfrom(input$month[1])
    sliderDate$date2 = datetransfrom(input$month[2])
  })
  
  # Let's set our Map
  map.data =   reactive({
    area[area$Crime.type %in% input$crime.type & area$Month >= sliderDate$date1 & area$Month <= sliderDate$date2 & area$loc.name %in% input$area,] %>% group_by(Longitude,Latitude,Month,Crime.type,loc.name) %>% summarise(count = n())
  })
  
  output$map = renderLeaflet({
    
    isolate({
      leaflet() %>% addProviderTiles(providers$Stamen.TonerLite) %>% fitBounds(min(map.data()$Longitude),min(map.data()$Latitude),max(map.data()$Longitude),max(map.data()$Latitude))}) 
  })
  
  observe({
    leafletProxy("map") %>% clearShapes() %>% addCircles(radius = map.data()$count, map.data()$Longitude,map.data()$Latitude,color = pal(map.data()$Crime.type), fillOpacity = 1) %>% clearControls() %>% addLegend("topright",pal = pal,values = map.data()$Crime.type)
  })
  
  
  #First Chart
  
  chart_1 =   reactive({
    area[area$Crime.type %in% input$crime.type & area$loc.name %in% input$area,] %>% group_by(Month,loc.name) %>% summarise(count = n())
  })
  
  output$plot1 = renderPlot({
    p = ggplot(chart_1(),aes(x = Month,y = count,group = loc.name,colour = loc.name,alpha = ifelse(Month >= sliderDate$date1 & Month <= sliderDate$date2,1,0.5)))
    p = p + geom_line(stat = "identity",size = 2) 
    p = p + Chart_set + scale_color_brewer(palette = "Set1") + guides(alpha = FALSE) 
    p = p + theme(legend.position = "top",legend.key = element_blank(),legend.text = element_text(size = 15),legend.title = element_blank()) + ylab ("Count - All Crimes")
    print(p)
  })
  
  #Second Chart
  
  chart_2 =   reactive({
    area[area$Month >= sliderDate$date1 & area$Month <= sliderDate$date2 & area$loc.name %in% input$area,] %>% group_by(Crime.type,loc.name) %>% summarise(count = n()) %>% melt() %>% arrange(desc(value))
  })
  
  
  output$plot2 = renderPlot({
    p = ggplot(chart_2(),aes(x = reorder(Crime.type,value),y = value,fill = loc.name,alpha = ifelse(Crime.type %in% input$crime.type,1,0.5))) 
    p = p + geom_bar(stat = "identity",position = "dodge") + guides(fill = FALSE) + guides(alpha = FALSE)  + coord_flip() 
    p = p + Chart_set + scale_fill_brewer(palette = "Set1") + xlab ("Crime Type")
    print(p) 
  })
  
}

#Let's look at our Dashboard
shinyApp(ui = ui, server = server)


