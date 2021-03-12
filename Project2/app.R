#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)
library(leaflet)
library("readxl")
library(rgeos)
library(maps)
library(mapdata)
library(maptools)
library(ggthemes)
library(sp)
library(stringr)
library(plyr)

#Read in the data for part 1 (Illinois 2018)
data2018 <- read_excel("Simplified_Data/2018_plant_data_simplified.xlsx")
data2018 <- data2018[-1, ] 

#Remove all NA values 
# cleaned <- na.omit(data2018)
cleaned <- data2018


#Convert the names of the columns to simpler names for usage
names(cleaned)[names(cleaned) == "Plant state abbreviation"] <- "State"
names(cleaned)[names(cleaned) == "Plant name"] <- "Name"
names(cleaned)[names(cleaned) == "Plant latitude"] <- "Latitude"
names(cleaned)[names(cleaned) == "Plant longitude"] <- "Longitude"
names(cleaned)[names(cleaned) == "Plant annual coal net generation (MWh)"] <- "COAL"
names(cleaned)[names(cleaned) == "Plant annual oil net generation (MWh)"] <- "OIL"
names(cleaned)[names(cleaned) == "Plant annual gas net generation (MWh)"] <- "GAS"
names(cleaned)[names(cleaned) == "Plant annual nuclear net generation (MWh)"] <- "NUCLEAR"
names(cleaned)[names(cleaned) == "Plant annual hydro net generation (MWh)"] <- "HYDRO"
names(cleaned)[names(cleaned) == "Plant annual biomass net generation (MWh)"] <- "BIOMASS"
names(cleaned)[names(cleaned) == "Plant annual wind net generation (MWh)"] <- "WIND"
names(cleaned)[names(cleaned) == "Plant annual solar net generation (MWh)"] <- "SOLAR"
names(cleaned)[names(cleaned) == "Plant annual geothermal net generation (MWh)"] <- "GEOTHERMAL"
names(cleaned)[names(cleaned) == "Plant annual other fossil net generation (MWh)"] <- "FOSSIL"
names(cleaned)[names(cleaned) == "Plant annual other unknown/ purchased fuel net generation (MWh)"] <- "UNKNOWN"

#Calculate the percent values from the new values 
cleaned[is.na(cleaned)] = '0'
cleaned <- cleaned[!(cleaned$Latitude == "N/A" | cleaned$Longitude == "N/A"), ]

numeric_data <- data.matrix(cleaned[c(-1, -2)])

other <- c()
total <- c() 
percent <- matrix(NA, nrow = nrow(numeric_data), ncol = 10) 
total_renew <- c()
total_nonrenew <- c()
percent_total_renew <- c()
percent_total_nonrenew <- c()



for (i in seq(from=1, to=nrow(numeric_data), by=1)) {
    other_sum <- sum(numeric_data[i, 12:13])
    total_sum <- sum(numeric_data[i, 3:13])
    
    other <- c(other, other_sum) 
    total <- c(total, total_sum) 
    
    
    for(j in seq(from=3, to=11, by=1)) {
        percent[i, j - 2] <- (numeric_data[i, j]/total_sum)
    }
    
    percent[i, 10] <- other_sum/total_sum
    
    total_renewable <- sum(numeric_data[i, c(7,8,9,10,11)])
    total_renew <- c(total_renew, total_renewable)
    
    total_nonrenewable <- sum(numeric_data[i, c(3,4,5,6,12,13)])
    total_nonrenew <- c(total_nonrenew, total_nonrenewable)
    
    
    percent_total_renew <- c(percent_total_renew, total_renewable/total_sum)
    percent_total_nonrenew <- c(percent_total_nonrenew, total_nonrenewable/total_sum)
    
}

total_data2018 <- cleaned
total_data2018['OTHER'] <- other
total_data2018['Total'] <- total

percent_total_renew[is.nan(percent_total_renew)] <- 0
percent_total_nonrenew[is.nan(percent_total_nonrenew)] <- 0
percent[is.nan(percent)] <- 0

total_data2018['Coal_Percent'] <- percent[1:nrow(numeric_data), 1]
total_data2018['Oil_Percent'] <- percent[1:nrow(numeric_data), 2]
total_data2018['Gas_Percent'] <- percent[1:nrow(numeric_data), 3]
total_data2018['Nuclear_Percent'] <- percent[1:nrow(numeric_data), 4]
total_data2018['Hydro_Percent'] <- percent[1:nrow(numeric_data), 5]
total_data2018['Biomass_Percent'] <- percent[1:nrow(numeric_data), 6]
total_data2018['Wind_Percent'] <- percent[1:nrow(numeric_data), 7]
total_data2018['Solar_Percent'] <- percent[1:nrow(numeric_data), 8]
total_data2018['Geothermal_Percent'] <- percent[1:nrow(numeric_data), 9]
total_data2018['Other_Percent'] <- percent[1:nrow(numeric_data), 10]


total_data2018['Total_Renewable'] <- total_renew
total_data2018['Total_Nonrenewable'] <- total_nonrenew

total_data2018['Percent_Renewable'] <- percent_total_renew
total_data2018['Percent_Nonrenewable'] <- percent_total_nonrenew

for (i in seq(3, ncol(total_data2018), 1)) {
    total_data2018[colnames(total_data2018)[i]] <- as.numeric(unlist(total_data2018[colnames(total_data2018)[i]]))
}

total_data2018$`Latitude` <- jitter(total_data2018$Latitude, factor = 0.0001)
total_data2018$`Longitude` <- jitter(total_data2018$Longitude, factor = 0.0001)

illinois_data <- total_data2018[total_data2018$State == 'IL', ] 




#Setup data for the Second Page: 
data2000 <- read_excel("Simplified_Data/2000_plant_data_simplified.xlsx")

data2000[is.na(data2000)] = 0

names(data2000)[names(data2000) == 'PSTATABB\r\nState abbreviation'] <- "State"
names(data2000)[names(data2000) == 'PNAME\r\nPlant name'] <- "Name"
names(data2000)[names(data2000) == 'LAT\r\nPlant latitude'] <- "Latitude"
names(data2000)[names(data2000) == 'LON\r\nPlant longitude'] <- "Longitude"
names(data2000)[names(data2000) == 'PLGENACL\r\nPlant 2000 annual coal net generation (MWh)'] <- "COAL"
names(data2000)[names(data2000) == 'PLGENAOL\r\nPlant 2000 annual oil net generation (MWh)'] <- "OIL"
names(data2000)[names(data2000) == "PLGENAGS\r\nPlant 2000 annual gas net generation (MWh)"] <- "GAS"
names(data2000)[names(data2000) == "PLGENANC\r\nPlant 2000 annual nuclear net generation (MWh)"] <- "NUCLEAR"
names(data2000)[names(data2000) == 'PLGENAHY\r\nPlant 2000 annual hydro net generation (MWh)'] <- "HYDRO"
names(data2000)[names(data2000) == 'PLGENABM\r\nPlant 2000 annual biomass/ wood net generation (MWh)'] <- "BIOMASS"
names(data2000)[names(data2000) == 'PLGENAWI\r\nPlant 2000 annual wind net generation (MWh)'] <- "WIND"
names(data2000)[names(data2000) == 'PLGENASO\r\nPlant 2000 annual solar net generation (MWh)'] <- "SOLAR"
names(data2000)[names(data2000) == 'PLGENAGT\r\nPlant 2000 annual geothermal net generation (MWh)'] <- "GEOTHERMAL"
names(data2000)[names(data2000) == 'PLGENAOF\r\nPlant 2000 annual other fossil (tires, batteries, chemicals, etc.) net generation (MWh)'] <- "FOSSIL"
names(data2000)[names(data2000) == 'PLGENASW\r\nPlant 2000 annual solid waste net generation (MWh)'] <- "OTHER"


data2000[is.na(data2000)] = 0

data2000 <- data2000[!(data2000$Latitude == "N/A" | data2000$Longitude == "N/A"), ]

numeric_data <- data.matrix(data2000[c(-1, -2)])

other <- c()
total <- c()
percent <- matrix(NA, nrow = nrow(numeric_data), ncol = 10)
total_renew <- c()
total_nonrenew <- c()
percent_total_renew <- c()
percent_total_nonrenew <- c()

for (i in seq(from=1, to=nrow(numeric_data), by=1)) {
    other_sum <- sum(numeric_data[i, 12:13])
    total_sum <- sum(numeric_data[i, 3:13])

    other <- c(other, other_sum)
    total <- c(total, total_sum)


    for(j in seq(from=3, to=11, by=1)) {
        percent[i, j - 2] <- (numeric_data[i, j]/total_sum)
    }

    percent[i, 10] <- other_sum/total_sum

    total_renewable <- sum(numeric_data[i, c(7,8,9,10,11)])
    total_renew <- c(total_renew, total_renewable)

    total_nonrenewable <- sum(numeric_data[i, c(3,4,5,6,12,13)])
    total_nonrenew <- c(total_nonrenew, total_nonrenewable)


    percent_total_renew <- c(percent_total_renew, total_renewable/total_sum)
    percent_total_nonrenew <- c(percent_total_nonrenew, total_nonrenewable/total_sum)

}

percent_total_renew[is.nan(percent_total_renew)] <- 0
percent_total_nonrenew[is.nan(percent_total_nonrenew)] <- 0
percent[is.nan(percent)] <- 0

total_data2000 <- data2000

total_data2000['Coal_Percent'] <- percent[1:nrow(numeric_data), 1]
total_data2000['Oil_Percent'] <- percent[1:nrow(numeric_data), 2]
total_data2000['Gas_Percent'] <- percent[1:nrow(numeric_data), 3]
total_data2000['Nuclear_Percent'] <- percent[1:nrow(numeric_data), 4]
total_data2000['Hydro_Percent'] <- percent[1:nrow(numeric_data), 5]
total_data2000['Biomass_Percent'] <- percent[1:nrow(numeric_data), 6]
total_data2000['Wind_Percent'] <- percent[1:nrow(numeric_data), 7]
total_data2000['Solar_Percent'] <- percent[1:nrow(numeric_data), 8]
total_data2000['Geothermal_Percent'] <- percent[1:nrow(numeric_data), 9]
total_data2000['Other_Percent'] <- percent[1:nrow(numeric_data), 10]

total_data2000['Total_Renewable'] <- total_renew
total_data2000['Total_Nonrenewable'] <- total_nonrenew

total_data2000['Percent_Renewable'] <- percent_total_renew
total_data2000['Percent_Nonrenewable'] <- percent_total_nonrenew

for (i in seq(3, ncol(total_data2000), 1)) {
    total_data2000[colnames(total_data2000)[i]] <- as.numeric(unlist(total_data2000[colnames(total_data2000)[i]]))
}

total_data2000$`Longitude` <- -1 * total_data2000$`Longitude`

total_data2000$`Latitude` <- jitter(total_data2000$Latitude, factor = 0.0001)
total_data2000$`Longitude` <- jitter(total_data2000$Longitude, factor = 0.0001)


total_data2000 <- total_data2000[complete.cases(total_data2000), ]


data2010 = read_excel("Simplified_Data/2010_plant_data_simplified.xlsx")
names(data2010)[names(data2010) == "Plant state abbreviation"] <- "State"
names(data2010)[names(data2010) == "Plant name"] <- "Name"
names(data2010)[names(data2010) == "Plant latitude"] <- "Latitude"
names(data2010)[names(data2010) == "Plant longitude"] <- "Longitude"
names(data2010)[names(data2010) == "Plant annual coal net generation (MWh)"] <- "COAL"
names(data2010)[names(data2010) == "Plant annual oil net generation (MWh)"] <- "OIL"
names(data2010)[names(data2010) == "Plant annual gas net generation (MWh)"] <- "GAS"
names(data2010)[names(data2010) == "Plant annual nuclear net generation (MWh)"] <- "NUCLEAR"
names(data2010)[names(data2010) == "Plant annual hydro net generation (MWh)"] <- "HYDRO"
names(data2010)[names(data2010) == "Plant annual biomass net generation (MWh)"] <- "BIOMASS"
names(data2010)[names(data2010) == "Plant annual wind net generation (MWh)"] <- "WIND"
names(data2010)[names(data2010) == "Plant annual solar net generation (MWh)"] <- "SOLAR"
names(data2010)[names(data2010) == "Plant annual geothermal net generation (MWh)"] <- "GEOTHERMAL"
names(data2010)[names(data2010) == "Plant annual other fossil net generation (MWh)"] <- "FOSSIL"
names(data2010)[names(data2010) == "Plant annual other unknown/ purchased fuel net generation (MWh)"] <- "OTHER"

numeric_data <- data.matrix(data2010[c(-1, -2)])

other <- c()
total <- c()
percent <- matrix(NA, nrow = nrow(numeric_data), ncol = 10)
total_renew <- c()
total_nonrenew <- c()
percent_total_renew <- c()
percent_total_nonrenew <- c()

for (i in seq(from=1, to=nrow(numeric_data), by=1)) {
    other_sum <- sum(numeric_data[i, 12:13])
    total_sum <- sum(numeric_data[i, 3:13])

    other <- c(other, other_sum)
    total <- c(total, total_sum)


    for(j in seq(from=3, to=11, by=1)) {
        percent[i, j - 2] <- (numeric_data[i, j]/total_sum)
    }

    percent[i, 10] <- other_sum/total_sum

    total_renewable <- sum(numeric_data[i, c(7,8,9,10,11)])
    total_renew <- c(total_renew, total_renewable)

    total_nonrenewable <- sum(numeric_data[i, c(3,4,5,6,12,13)])
    total_nonrenew <- c(total_nonrenew, total_nonrenewable)


    percent_total_renew <- c(percent_total_renew, total_renewable/total_sum)
    percent_total_nonrenew <- c(percent_total_nonrenew, total_nonrenewable/total_sum)

}

percent_total_renew[is.nan(percent_total_renew)] <- 0
percent_total_nonrenew[is.nan(percent_total_nonrenew)] <- 0
percent[is.nan(percent)] <- 0

total_data2010 <- data2010

total_data2010['Coal_Percent'] <- percent[1:nrow(numeric_data), 1]
total_data2010['Oil_Percent'] <- percent[1:nrow(numeric_data), 2]
total_data2010['Gas_Percent'] <- percent[1:nrow(numeric_data), 3]
total_data2010['Nuclear_Percent'] <- percent[1:nrow(numeric_data), 4]
total_data2010['Hydro_Percent'] <- percent[1:nrow(numeric_data), 5]
total_data2010['Biomass_Percent'] <- percent[1:nrow(numeric_data), 6]
total_data2010['Wind_Percent'] <- percent[1:nrow(numeric_data), 7]
total_data2010['Solar_Percent'] <- percent[1:nrow(numeric_data), 8]
total_data2010['Geothermal_Percent'] <- percent[1:nrow(numeric_data), 9]
total_data2010['Other_Percent'] <- percent[1:nrow(numeric_data), 10]

total_data2010['Total_Renewable'] <- total_renew
total_data2010['Total_Nonrenewable'] <- total_nonrenew

total_data2010['Percent_Renewable'] <- percent_total_renew
total_data2010['Percent_Nonrenewable'] <- percent_total_nonrenew

for (i in seq(3, ncol(total_data2010), 1)) {
    total_data2010[colnames(total_data2010)[i]] <- as.numeric(unlist(total_data2010[colnames(total_data2010)[i]]))
}

total_data2010[is.na(total_data2010)] = 0


total_data2018$`State` <- as.factor(total_data2018$`State`)
state_names <- state.name[unique(total_data2018$`State`)]
state_names <- state_names[complete.cases(state_names)]

usa <- map_data("state")
getLabelPoint <- function(state) {Polygon(state[c('long', 'lat')])@labpt}
centroids = by(usa, usa$region, getLabelPoint)
centroids2 <- do.call("rbind.data.frame", centroids)
centroids2$region = rownames(centroids)
names(centroids2) <- c('clong', 'clat', "region")



# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    #create dashboard and elements
    dashboardHeader(title = "CS 424 Project 2"),
    
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     #menu bar with all 3 panels and about page
                     sidebarMenu(
                         menuItem("Illinois 2018 Visualization", tabName = "Illinois_2018", icon = NULL),
                         menuItem("State to State Comparison", tabName = "State_Compare", icon = NULL),
                         menuItem("Country Over Time", tabName = "Country_compare", icon = NULL),
                         menuItem("About Page", tabName = "About", icon = NULL)
                     )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName="Illinois_2018",
                      fluidRow(
                          
                          column(10, offset=1,
                          checkboxGroupInput("Illinois_Energy", 
                                             h3("Select Energy Sources:"), 
                                             choices = c("All" = "All", "Renewable" = "Renewable", "Nonrenewable" = "Nonrenewable", 
                                                         'Coal' = 'Coal', "Oil" = "Oil", "Gas" = "Gas",
                                                         "Nuclear" = "Nuclear" , "Hydro" = "Hydro", "Biomass" = "Biomass", "Wind" = "Wind", "Solar" = "Solar", "Geothermal" = "Geothermal", "Other" = "Other"),
                                             selected = "All", 
                                             inline = TRUE
                          )),
                          
                        column(1,
                               actionButton("reset_button_first_page", "Reset view"))
                        
                    ),
                    
                leafletOutput("leaf_Illinois", height = 800),
                
               
        )
        ,
        
        tabItem(tabName = "State_Compare",
                
                
                fluidRow(
                    
                    column(2,

               selectInput("State1", h5("Choose the first State"),
                           choices = state_names,
                           selected = "Illinois",
                          


               ),
               selectInput("StateYear1", h5("Choose the year for the first State"),
                           choices = c(2000, 2010, 2018),
                           selected = 2000,
                       

               ),
               
               actionButton("reset_state1", "Reset Map 1 view"),
               
               checkboxGroupInput("State1Energy", 
                                  h3("Select Energy Sources:"), 
                                  choices = c("All" = "All", "Renewable" = "Renewable", "Nonrenewable" = "Nonrenewable", 
                                              'Coal' = 'Coal', "Oil" = "Oil", "Gas" = "Gas",
                                              "Nuclear" = "Nuclear" , "Hydro" = "Hydro", "Biomass" = "Biomass", "Wind" = "Wind", "Solar" = "Solar", "Geothermal" = "Geothermal", "Other" = "Other")
                                  
               )
               
               ),

                
                column(4, leafletOutput("leaf_State1", height = 800, width=560)),
        
            
                column(2, padding=0, div(style='padding:0px; margin-left:-2em;;'),
                       selectInput("State2",  h5("Choose the second State"),
                                   choices = state_names,
                                   selected = "Illinois",


                       ),
                       selectInput("StateYear2", h5("Choose the year for the second  State"),
                                   choices = c(2000, 2010, 2018),
                                   selected = 2018,


                       ),

                       actionButton("reset_state2", "Reset Map 2 view"),
                       
                
                checkboxGroupInput("State2Energy",
                                   h3("Select Energy Sources:"),
                                   choices = c("All" = "All", "Renewable" = "Renewable", "Nonrenewable" = "Nonrenewable",
                                               'Coal' = 'Coal', "Oil" = "Oil", "Gas" = "Gas",
                                               "Nuclear" = "Nuclear" , "Hydro" = "Hydro", "Biomass" = "Biomass", "Wind" = "Wind", "Solar" = "Solar", "Other" = "Other"))),
                column(2, leafletOutput("leaf_State2", height = 800, width = 560))
            ),
            fluidRow(column(1, padding=6, checkboxInput("Sync", label = "Sync", value = FALSE)))

        )
        
        
        
        
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    initial_IL_lat = 40
    initial_IL_lng = -89
    initial_IL_zoom = 7
    
    
    observe({

        
            illinois_map <- leaflet(illinois_data) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
                
                setView(
                    lng = initial_IL_lng,
                    lat = initial_IL_lat, 
                    zoom = initial_IL_zoom
                ) %>%
     
        
            addCircleMarkers(data = subset(illinois_data, COAL > 0 & ("All" %in% input$Illinois_Energy | "Nonrenewable" %in% input$Illinois_Energy | "Coal" %in% input$Illinois_Energy)),  lng=~Longitude, lat=~Latitude, group = "COAL", popup=~COAL, color="Red", fillOpacity = 0.3, stroke = FALSE ) %>%
            
            addCircleMarkers(data = subset(illinois_data, OIL > 0 & ("All" %in% input$Illinois_Energy | "Nonrenewable" %in% input$Illinois_Energy | "Oil" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "OIL", popup=~OIL, color="Blue", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, GAS > 0 & ("All" %in% input$Illinois_Energy | "Nonrenewable" %in% input$Illinois_Energy | "Gas" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "GAS", popup = ~GAS, color="Green", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, NUCLEAR > 0 & ("All" %in% input$Illinois_Energy | "Nonrenewable" %in% input$Illinois_Energy | "Nuclear" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "NUCLEAR", popup = ~NUCLEAR, color="Yellow", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, HYDRO > 0 & ("All" %in% input$Illinois_Energy | "Renewable" %in% input$Illinois_Energy | "Hydro" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "HYDRO", popup = ~HYDRO, color="Orange", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, BIOMASS > 0 & ("All" %in% input$Illinois_Energy | "Renewable" %in% input$Illinois_Energy | "Biomass" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "BIOMASS", popup = ~BIOMASS, color="Purple", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, WIND > 0 & ("All" %in% input$Illinois_Energy | "Renewable" %in% input$Illinois_Energy | "Wind" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "WIND", popup = ~WIND, color="Cyan", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, SOLAR > 0 & ("All" %in% input$Illinois_Energy | "Renewable" %in% input$Illinois_Energy | "Solar" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "SOLAR", popup = ~SOLAR, color="Brown", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, GEOTHERMAL > 0 & ("All" %in% input$Illinois_Energy | "Renewable" %in% input$Illinois_Energy | "Geothermal" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "GEOTHERMAL", popup = ~GEOTHERMAL, color="Pink", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(illinois_data, OTHER > 0 & ("All" %in% input$Illinois_Energy | "Nonrenewable" %in% input$Illinois_Energy | "Other" %in% input$Illinois_Energy)), lng=~Longitude, lat=~Latitude, group = "OTHER", popup = ~OTHER,  color="Grey", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addLegend("bottomright", colors = c("#FF0000", "#0000FF","#00FF00", "#FFFF00", "#FFa500", "#800080", "#00FFFF", "#FF0000", "#964B00", "#808080"), 
                      labels=c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other"), title="Energy Source") #%>%
        
        
        
        output$leaf_Illinois <- renderLeaflet({
            illinois_map
        })
    })
    
    observe({
        input$reset_button_first_page
        leafletProxy("leaf_Illinois") %>% setView(lat = initial_IL_lat, lng = initial_IL_lng, zoom = initial_IL_zoom)
    })
    
    
    
    
    
    observe({
        if (input$Sync == TRUE) {
            updateCheckboxGroupInput(session, "State2Energy",
                selected = input$State1Energy
            )
            
            updateCheckboxGroupInput(session, "State1Energy",
                                     selected = input$State2Energy
            )
        }
        
    })
    
    
    
    observe({
        
        State1_lng = initial_IL_lng
        State1_lat = initial_IL_lat
        State1_zoom = 6
        
        state1_data <- total_data2000
        if (input$StateYear1 == 2018) {
            state1_data <- total_data2018[total_data2018$State == state.abb[grep(input$State1, state.name)], ]
        } else if (input$StateYear1 == 2000) {
            state1_data <- total_data2000[total_data2000$State == state.abb[grep(input$State1, state.name)], ]
        } else if (input$StateYear1 == 2010) {
            state1_data <- total_data2010[total_data2010$State == state.abb[grep(input$State1, state.name)], ]
        } else {
            state1_data <- total_data2000[total_data2000$State == "IL", ]
            
        }
        
        
        State1_lng = centroids2[centroids2$region == tolower(input$State1), ][['clong']]
        State1_lat = centroids2[centroids2$region == tolower(input$State1), ][['clat']]
        
        state1_map <- leaflet(state1_data) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            
            setView(
                lng = State1_lng,
                lat = State1_lat,
                zoom = State1_zoom
            ) %>%
            
            
            addCircleMarkers(data = subset(state1_data, COAL > 0 & ("All" %in% input$State1Energy | "Nonrenewable" %in% input$State1Energy | "Coal" %in% input$State1Energy)),  lng=~Longitude, lat=~Latitude, group = "COAL", popup=~COAL, color="Red", fillOpacity = 0.3, stroke = FALSE ) %>%
            
            addCircleMarkers(data = subset(state1_data, OIL > 0 & ("All" %in% input$State1Energy | "Nonrenewable" %in% input$State1Energy | "Oil" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "OIL", popup=~OIL, color="Blue", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, GAS > 0 & ("All" %in% input$State1Energy | "Nonrenewable" %in% input$State1Energy | "Gas" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "GAS", popup = ~GAS, color="Green", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, NUCLEAR > 0 & ("All" %in% input$State1Energy | "Nonrenewable" %in% input$State1Energy | "Nuclear" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "NUCLEAR", popup = ~NUCLEAR, color="Yellow", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, HYDRO > 0 & ("All" %in% input$State1Energy | "Renewable" %in% input$State1Energy | "Hydro" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "HYDRO", popup = ~HYDRO, color="Orange", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, BIOMASS > 0 & ("All" %in% input$State1Energy | "Renewable" %in% input$State1Energy | "Biomass" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "BIOMASS", popup = ~BIOMASS, color="Purple", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, WIND > 0 & ("All" %in% input$State1Energy | "Renewable" %in% input$State1Energy | "Wind" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "WIND", popup = ~WIND, color="Cyan", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, SOLAR > 0 & ("All" %in% input$State1Energy | "Renewable" %in% input$State1Energy | "Solar" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "SOLAR", popup = ~SOLAR, color="Brown", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, GEOTHERMAL > 0 & ("All" %in% input$State1Energy | "Renewable" %in% input$State1Energy | "Geothermal" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "GEOTHERMAL", popup = ~GEOTHERMAL, color="Pink", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state1_data, OTHER > 0 & ("All" %in% input$State1Energy | "Nonrenewable" %in% input$State1Energy | "Other" %in% input$State1Energy)), lng=~Longitude, lat=~Latitude, group = "OTHER", popup = ~OTHER,  color="Grey", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addLegend("bottomright", colors = c("#FF0000", "#0000FF","#00FF00", "#FFFF00", "#FFa500", "#800080", "#00FFFF", "#FF0000", "#964B00", "#808080"), 
                      labels=c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other"), title="Energy Source") #%>%
        
        
        output$leaf_State1 <- renderLeaflet({
            state1_map
        })
    })
    
    
    observe({
        
        State2_lng = initial_IL_lng
        State2_lat = initial_IL_lat
        State2_zoom = 6
        
        
        state2_data <- total_data2010
        if (input$StateYear2 == 2018) {
            state2_data <- total_data2018[total_data2018$State == state.abb[grep(input$State2, state.name)], ]
        } else if (input$StateYear2 == 2000) {
            state2_data <- total_data2000[total_data2000$State == state.abb[grep(input$State2, state.name)], ]
        } else if (input$StateYear2 == 2000) {
            state2_data <- total_data2010[total_data2010$State == state.abb[grep(input$State2, state.name)], ]
        } else {
            state2_data <- total_data2018[total_data2018$State == "IL", ]
        }
        
        # print(tolower(input$State2))
        # print(centroids2[centroids2$region == tolower(input$State2), ]['clong'])
        
        State2_lng = centroids2[centroids2$region == tolower(input$State2), ][['clong']]
        State2_lat = centroids2[centroids2$region == tolower(input$State2), ][['clat']]
        
        # print(State2_lng)
        # print(State2_lat)
        
        state2_map <- leaflet(state2_data) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            
            
            
            setView(
                lng = State2_lng,
                lat = State2_lat,
                zoom = State2_zoom
            ) %>%
            
            
            addCircleMarkers(data = subset(state2_data, COAL > 0 & ("All" %in% input$State2Energy | "Nonrenewable" %in% input$State2Energy | "Coal" %in% input$State2Energy)),  lng=~Longitude, lat=~Latitude, group = "COAL", popup=~COAL, color="Red", fillOpacity = 0.3, stroke = FALSE ) %>%
            
            addCircleMarkers(data = subset(state2_data, OIL > 0 & ("All" %in% input$State2Energy | "Nonrenewable" %in% input$State2Energy | "Oil" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "OIL", popup=~OIL, color="Blue", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, GAS > 0 & ("All" %in% input$State2Energy | "Nonrenewable" %in% input$State2Energy | "Gas" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "GAS", popup = ~GAS, color="Green", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, NUCLEAR > 0 & ("All" %in% input$State2Energy | "Nonrenewable" %in% input$State2Energy | "Nuclear" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "NUCLEAR", popup = ~NUCLEAR, color="Yellow", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, HYDRO > 0 & ("All" %in% input$State2Energy | "Renewable" %in% input$State2Energy | "Hydro" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "HYDRO", popup = ~HYDRO, color="Orange", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, BIOMASS > 0 & ("All" %in% input$State2Energy | "Renewable" %in% input$State2Energy | "Biomass" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "BIOMASS", popup = ~BIOMASS, color="Purple", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, WIND > 0 & ("All" %in% input$State2Energy | "Renewable" %in% input$State2Energy | "Wind" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "WIND", popup = ~WIND, color="Cyan", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, SOLAR > 0 & ("All" %in% input$State2Energy | "Renewable" %in% input$State2Energy | "Solar" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "SOLAR", popup = ~SOLAR, color="Brown", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, GEOTHERMAL > 0 & ("All" %in% input$State2Energy | "Renewable" %in% input$State2Energy | "Geothermal" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "GEOTHERMAL", popup = ~GEOTHERMAL, color="Pink", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addCircleMarkers(data = subset(state2_data, OTHER > 0 & ("All" %in% input$State2Energy | "Nonrenewable" %in% input$State2Energy | "Other" %in% input$State2Energy)), lng=~Longitude, lat=~Latitude, group = "OTHER", popup = ~OTHER,  color="Grey", fillOpacity = 0.3, stroke = FALSE) %>%
            
            addLegend("bottomright", colors = c("#FF0000", "#0000FF","#00FF00", "#FFFF00", "#FFa500", "#800080", "#00FFFF", "#FF0000", "#964B00", "#808080"), 
                      labels=c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other"), title="Energy Source") #%>%
        
        
        output$leaf_State2 <- renderLeaflet({
            state2_map
        })
    })
    
    
    


    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
