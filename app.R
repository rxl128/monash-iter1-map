# setwd("/Users/shashanksharma/Documents/leaflet r iie/")

## TO DO: Pivot to change it to be only waste stats
## Remove box where no stats available.

##libraries used
library(leaflet)
library(rgdal)
library(plyr)
library(reshape2)
library(ggplot2)

##local code file used for naming the suburbs
citycode = read.csv("LOCAL_CODE.csv",header = TRUE)
citycode$Name = trimws(citycode$Name) # Trim ws

##renaming thew columns
# names(citycode)[names(citycode) == "X...Name"] <- "Name"
# names(citycode)[names(citycode) %like% "Name"] <- "Name"

names(citycode)[names(citycode) == "Code"] <- "LG_PLY_PID"


# head(citycode)

##data from the venar list fole named clean_data
waste_data = read.csv("clean_Data.csv",header = TRUE)
#waste_data

# Fix for Mornington Peninsula
waste_data$Local.Government = revalue(waste_data$Local.Government, c("PENINSULA" = 'MORNINGTON PENINSULA'))

##removing commas from numbers and then converting it ti numeric form 
waste_data$X2018 <- as.numeric(as.character(gsub(",","",waste_data$X2018)))
waste_data$X2035 <- as.numeric(as.character(gsub(",","",waste_data$X2035)))



##adding the 2018 and 2035 waste generated per LG
waste_sum = setNames(aggregate(list(waste_data$X2018,waste_data$X2035), by=list(Category=waste_data$Local.Government), FUN=sum),c('Name',2018,2035))
waste_sum$Name = trimws(waste_sum$Name)

waste_sum = setorder(waste_sum, by='Name')

##counting collectin points per lg
count_waste = setNames(as.data.frame(table(waste_data$Local.Government)),c('Name','Count'))
#count_waste

##loading shapefile
spdf = readOGR(dsn = getwd(), layer = "VIC_LGA_POLYGON_shp")

##merging all the files to the shapdefile data
spdf@data = data.frame(spdf@data, citycode[match(spdf@data$LG_PLY_PID, citycode$LG_PLY_PID),])
spdf@data = data.frame(spdf@data, waste_sum[match(spdf@data$Name, waste_sum$Name),])
spdf@data = data.frame(spdf@data, count_waste[match(spdf@data$Name, count_waste$Name),])
  
##removing nas in the data
# spdf@data$`X2018`[is.na(spdf@data$`X2018`)] = 0
# spdf@data$`X2035`[is.na(spdf@data$`X2035`)] = 0
spdf@data$Count[is.na(spdf@data$Count)] = 0


#unique(spdf@data$Count)

#melting for grouped bar chart
boxplot_data <- melt(waste_sum, id=c("Name"))

##colouring the map based on number of collection points in region
pal <- colorFactor(
  palette = c('darkgreen','yellow','darkred'),
#  domain = spdf@data$Count
  domain = spdf@data$`X2018`
)


ui <- fluidPage(
  tabsetPanel(
            tabPanel(
              h6(strong("All Regions"),style = "color:blue;font-family: 'georgia';"), value = 1,##for all regions 
              sidebarLayout(
                sidebarPanel(
                 
                  tags$ul(
                    # tags$li(strong("Red regions- 0 collection agency")),                    ##edit if needed this is to explani the colours
                    # tags$li(strong("Yellow regions- 1-2 collection agencies")),
                    # tags$li(strong("Green regions- Greater than 2 collection agencies"))
                    tags$li(strong("Local government areas with missing e-waste data are greyed out."))
                    
                  
                  )
                  )
                
                  
                ,
              
                mainPanel(
                  conditionalPanel(
                    condition = "(input.tabs == 1)",
                    h3("E-waste genereration in Victoria", align = "left", style = "color:blue;font-family: 'georgia';"),
                    leafletOutput("mymap",width = 1000, height = 600)
                    )
                )
              )
              )
              ,
            
  tabPanel(
              h6(strong("Compare Regions"),style = "color:blue;font-family: 'georgia';"), value = 2,##for comparison
              sidebarLayout(
                sidebarPanel(
                  selectInput("first",                                                 ##selection input
                              h6(strong("Select first region"),style = "color:brown;font-family: 'georgia';"),
                              choices = unique(boxplot_data$Name)
                  ),
                  selectInput("second",                                                 ##selection input
                              h6(strong("Select second region"),style = "color:brown;font-family: 'georgia';"),
                              choices = unique(boxplot_data$Name),
                              selected = c("ARARAT")
                                               
                  )
                )
              ,
              mainPanel(
                plotOutput("comparison") 
              )
              ) 
          ),
            id= 'tabs'
          )
)
  

  
  


server <- function(input, output, session) {
  
    output$mymap <- renderLeaflet({          ##leaflet output
      ##leaflet visualising the data
      ##leaflet visualising the data
      leaflet(data=spdf) %>% 
        addTiles() %>% 
        setView(lat=-36.6, lng=145.5 , zoom=6.75) %>% 
        addPolygons(fillColor = ~pal(`X2018`),
                    fillOpacity = 0.5,
                    highlight = highlightOptions(weight = 8,
                                                 color = "red",
                                                 fillOpacity = 0.70,
                                                 bringToFront = TRUE),
                    popup= paste("Council name:",spdf@data$Name,"<br>",
                                 "E-waste generated in 2018:",spdf@data$`X2018`," tonnes<br>",
                                 "E-waste estimated in 2035:",spdf@data$`X2035`," tonnes<br>")
                    ,
                    label=~Name)
  })
    
    output$comparison <- reactivePlot(function()     ##boxplot output
    {
      first_selection <- as.character(input$first)
      second_selection <- as.character(input$second)
      plot_data = boxplot_data[which ((boxplot_data$Name == first_selection) | (boxplot_data$Name == second_selection)), ]
      
      ggplot(plot_data, aes(factor(plot_data$Name), plot_data$value, fill =plot_data$variable)) + 
        geom_bar(stat="identity", position = "dodge") +  ggtitle("Comparing e-Waste generated") +
        scale_fill_brewer(palette = "Set1") +xlab("Regions")+ylab("E-waste generated") + labs(fill = "Year")
      
    }, height = 700, width = 700)
}

shinyApp(ui, server)
