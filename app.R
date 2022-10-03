#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Coastal Seasonal MHWs around NZ app
# Adapted from global study C:\Users\thoralf\OneDrive - NIWA\Documents\Papers_Published_Methods\Thoral2021_GlobalCoastalSeasonalMHW\app.R


library(shiny)
library(shinybusy)
library(tidyverse)
library(leaflet)
library(leafpop)
library(plotly)
library(sf)
library(sp)
library(viridis)
library(maps)
library(DT)
#library(rnaturalearth)

## Load full MHW Attributes outputs MEOW
MHW_dplyr <- read_csv('MHW_Trends_FULL_Ecoregion_MEOW12NM_Pixels_SmallSize_FullTS.csv') %>% 
    mutate(ECOREGION = factor(ECOREGION,levels=c("Kermadec Island", "Three Kings-North Cape", "Northeastern New Zealand",
                                                 "Central New Zealand","Chatham Island","South New Zealand", 
                                                 "Bounty and Antipodes Islands","Snares Island","Auckland Island","Campbell Island")),
           Season = factor(season,levels=c("Summer","Autumn","Winter","Spring")),
           Metrics = factor(Metrics,levels=c("Number_MHW_days", "Nevents", "Mean_Intensity", "Maximum_Intensity", "Cumulative_Intensity")))

MHW_trends_FULL <- read_csv('MHW_Trends_FULL_NZBioregion_MEOW12NM_Pixels_FullTS.csv')


mhw_metrics_mean_realm <- readRDS("YearSeason_NZ_BIOREGION_MeanMetrics_MHWDays_Pixels_1982_2021_OISST_MostCoastal.Rds")
npix <- mhw_metrics_mean_realm %>% 
  group_by(lon,lat,ECOREGION) %>% 
  tally() %>% 
  st_as_sf(.,coords=c('lon','lat'),crs=4326)
##

## Load Coastal Realms MEOW (Costello et al., 2007)
coastal_spal <- st_read('Marine_Ecoregions_Of_the_World__MEOW_.shp') %>% 
    filter(ECOREGION %in% c("Kermadec Island", "Three Kings-North Cape", "Northeastern New Zealand",
                            "Central New Zealand","Chatham Island","South New Zealand", 
                            "Bounty and Antipodes Islands","Snares Island","Auckland Island","Campbell Island")) %>% 
    mutate(ECOREGION = factor(ECOREGION,levels=c("Kermadec Island", "Three Kings-North Cape", "Northeastern New Zealand",
                                                 "Central New Zealand","Chatham Island","South New Zealand", 
                                                 "Bounty and Antipodes Islands","Snares Island","Auckland Island","Campbell Island")))

coastal_spal_lonlat <- st_transform(coastal_spal,CRS("+proj=longlat +datum=WGS84 +no_defs")) 

coastal_spal_lonlat <- coastal_spal_lonlat %>% group_by(ECOREGION) %>% st_shift_longitude() %>% summarize(geometry = st_combine(geometry))

#factpal <- colorFactor(viridis(12,begin = 0, end = .75), coastal_spal_lonlat$REALM)
factpal <- colorFactor(hcl(seq(15,330,length=12),l = 65, c = 100), coastal_spal_lonlat$ECOREGION)
##

# To add DOC part
## Load Coastal regions DOC
MHW_dplyrDOC <- read_csv('MHW_Trends_FULL_NZ_DOC_CoastalMarineHabitat_SmallSize_FullTS_Omly8regions.csv') %>%
  mutate(Region = factor(Region,levels=c("Western North Island","Eastern North Island","North Cook Strait",
                                         "South Cook Strait","West Coast South Island","East Coast South Island",
                                         "Southland","Fiordland")),
         Season = factor(season,levels=c("Summer","Autumn","Winter","Spring")),
         Metrics = factor(Metrics,levels=c("Number_MHW_days", "Nevents", "Mean_Intensity", "Maximum_Intensity", "Cumulative_Intensity")))

# 
# coastalmarinehab <- st_read('14_Coastal_NZ_Biogeographic_Regions_12nm.shp') %>% 
#   mutate(Region = factor(Region,levels=c("Kermadec Islands", "Three Kings Islands", "North Eastern",
#                                          "Western North Island","Eastern North Island","North Cook Strait", 
#                                          "South Cook Strait","West Coast South Island","East Coast South Island","Chatham Islands","Southland",
#                                          "Fiordland","Snares Islands","Subantarctic Islands")))

MHW_DOC_trends_FULL <- read_csv('MHW_Trends_FULL_NZ_DOC_CoastalMarineHabitat_clean_FullTS_Only8regions.csv') %>% 
    mutate(Region_Name = factor(Region_Name,levels=c("Western North Island","Eastern North Island","North Cook Strait",
                                           "South Cook Strait","West Coast South Island","East Coast South Island","Southland",
                                           "Fiordland")))

## Load coastline
#coast <- ne_coastline(scale = "medium", returnclass = "sf") %>% 
#    st_cast(.,"MULTIPOLYGON")
# Issue with rnaturalearth package on Shinapps.io server
# cf https://community.rstudio.com/t/shiny-app-works-on-rstudio-but-does-not-deploy/93962
# So get shp file from http://www.naturalearthdata.com/downloads/50m-physical-vectors/

coast <- st_read('ne_50m_land.shp')

## New facet label names for Metrics
metrics.labs <- c(`Cumulative_Intensity` = "Cumulative Intensity (DegC Days)",
                  `Number_MHW_days` = "MHW days",
                  `Mean_Intensity` = "Mean Intensity (DegC)",
                  `Maximum_Intensity` = "Maximum Intensity (DegC)",
                  `Nevents` = "Number of Events")
##

## Load Trends
MHW_Trends <- read_csv('MHW_Trends_FULL_NZBioregion_MEOW12NM_Pixels_clean_FullTS.csv') #Contains output of trend analysis (Slope, breakpoint, p-values) - for global and NZ


##

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Coastal MHW", id="nav",
               
               tabPanel("Interactive map MEOW",
                        div(class="outer",
                            h2("Click on the polygons to see seasonal trends in MHW metrics."),
                            h4("Then go to the Trend Explorer Tab for exploring the data more."),
                            # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            #plotlyOutput("map", width=1600, height=800),
                            leafletOutput("map", width=1600, height=800),
                            tags$div(id="cite",
                                     'Data compiled for ', tags$em('Montie et al., 2022 - Seasonal Trends in Marine Heatwaves Highlight Vulnerable Coastal Bioregions and Historic Change Points in New Zealand.'), 'In prep.'
                            )
                        ),
                        add_busy_spinner(timeout=1000,color='blue')
               ),
               
               tabPanel("Trends explorer MEOW",
                        fluidRow(
                            column(3,
                                   selectInput("ecoregion", "MEOW Ecoregion", c("Kermadec Island", "Three Kings-North Cape", "Northeastern New Zealand",
                                                                           "Central New Zealand","Chatham Island","South New Zealand", 
                                                                           "Bounty and Antipodes Islands","Snares Island","Auckland Island","Campbell Island"), 
                                               selected='Kermadec Island',multiple=F)
                            )
                        ),
                        #plotlyOutput("trendplot", width="100%", height="100%"),
                        plotlyOutput("trendplot", width=800, height=800),
                        
                        #plotlyOutput("trendplot", width="auto", height="auto"),
                        hr(),
                        DT::dataTableOutput("table")
                        #dataTableOutput("table")
                        
               ),
# To add DOC part
               # tabPanel("Interactive map DOC",
               #          div(class="outer",
               #              h2("Click on the polygons to see seasonal trends in MHW metrics."),
               #              h4("Then go to the Trend Explorer Tab for exploring the data more."),
               #              # If not using custom CSS, set height of leafletOutput to a number instead of percent
               #              ##plotlyOutput("map", width=1600, height=800),
               #              #leafletOutput("map", width=1600, height=800),
               #              
               #              tags$div(id="cite",
               #                       'Data compiled for ', tags$em('Montie et al., 2022 - Seasonal Trends in Marine Heatwaves Highlight Vulnerable Coastal Bioregions and Historic Change Points in New Zealand.'), 'In prep.'
               #              )
               #          ),
               #          add_busy_spinner(timeout=1000,color='blue')
               # ),
               # 
               tabPanel("Trends explorer DOC",
                        fluidRow(
                          column(3,
                                 selectInput("bioregion", "DOC Coastal Region", c("Western North Island","Eastern North Island","North Cook Strait",
                                                                                  "South Cook Strait","West Coast South Island","East Coast South Island",
                                                                                  "Southland","Fiordland"),
                                             selected='Western North Island',multiple=F)
                          )
                        ),
                        ##plotlyOutput("trendplot", width="100%", height="100%"),
                        plotlyOutput("trendplotDOC", width=800, height=800),

                        #plotlyOutput("trendplot", width="auto", height="auto"),
                        hr(),
                        DT::dataTableOutput("table2")
                        #dataTableOutput("table")

               ),
                              
               conditionalPanel("false", icon("crosshair"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #To pop a plot on map at click, cf:https://community.rstudio.com/t/leafpop-popupgraph-doenst-work-with-plumber-r/89073/2 using package leafpop::
    my_list <- list()  
    loop<-for (i in unique(MHW_dplyr$ECOREGION)) {
        MHW_dplyr_realm_sub <- MHW_dplyr %>% filter(ECOREGION == i)
        
        plot <- ggplot(MHW_dplyr_realm_sub,aes(year,values,col=season)) + 
            facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
            geom_line(size=.5) + 
            geom_smooth(se=T,size=1.5) + 
            ylab(paste0(unique(MHW_dplyr_realm_sub$ECOREGION))) + xlab('Year') + 
            scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
            theme_bw() + 
            theme(legend.position="bottom",
                  legend.title = element_text(size=12),
                  legend.text = element_text(size=12),
                  strip.text = element_text(size=12), 
                  axis.text=element_text(size=12),
                  axis.title = element_text(size=12))+
            guides(colour = guide_legend(override.aes = list(shape=15,size=4)))
        
        my_list[[i]] <- plot
    }
    
    output$map <- renderLeaflet({
        # leaflet(coastal_spal_lonlat) %>% 
        #     setView(lng = 180, lat = -42, zoom = 5) %>%
        #     addTiles() %>% 
        #     addPolygons(#data=coastal_spal_lonlat,
        #                 stroke = T, smoothFactor = 0.2, fillOpacity = 1,fillColor = ~factpal(ECOREGION),
        #                highlightOptions = highlightOptions(color = "white", weight = 2,
        #                                                    bringToFront = F),#T
        #                #layerId = ~REALM,
        #                popup = paste0("ECOREGION: ",coastal_spal_lonlat$ECOREGION,"<br/>", popupGraph(my_list,width=800,height=500))) %>% 
        #                #popup = paste0(" Realm: ",coastal_spal_lonlat$REALM, "<br/>  Province: ",coastal_spal_lonlat$PROVINCE)) %>% 
        #     
        #     addProviderTiles("Esri.WorldPhysical") %>%
        #     addPolygons(data=coast,col='grey',fillOpacity = 1)
        #     #addPopupGraphs(my_list, type='html')
      
      leaflet(coastal_spal_lonlat) %>% 
        setView(lng = 180, lat = -42, zoom = 5) %>%
        addTiles() %>% 
        addPolygons(#data=coastal_spal_lonlat,
          stroke = T, smoothFactor = 0.2, fillOpacity = 1,fillColor = ~factpal(ECOREGION),
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = F),#T
          #layerId = ~REALM,
          popup = paste0("ECOREGION: ",coastal_spal_lonlat$ECOREGION,"<br/>", popupGraph(my_list,width=800,height=500))) %>% 
        #popup = paste0(" Realm: ",coastal_spal_lonlat$REALM, "<br/>  Province: ",coastal_spal_lonlat$PROVINCE)) %>% 
        addCircleMarkers(data=npix) %>% 
        addProviderTiles("Esri.WorldPhysical") %>%
        addPolygons(data=coast,col='grey',fillOpacity = 1)
      
    })

# To add DOC part        
    # my_listDOC <- list()  
    # loop<-for (i in unique(MHW_dplyr$ECOREGION)) {
    #   MHW_dplyr_realm_sub <- MHW_dplyr %>% filter(ECOREGION == i)
    #   
    #   plot <- ggplot(MHW_dplyr_realm_sub,aes(year,values,col=season)) + 
    #     facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
    #     geom_line(size=.5) + 
    #     geom_smooth(se=T,size=1.5) + 
    #     ylab(paste0(unique(MHW_dplyr_realm_sub$ECOREGION))) + xlab('Year') + 
    #     scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
    #     theme_bw() + 
    #     theme(legend.position="bottom",
    #           legend.title = element_text(size=12),
    #           legend.text = element_text(size=12),
    #           strip.text = element_text(size=12), 
    #           axis.text=element_text(size=12),
    #           axis.title = element_text(size=12))+
    #     guides(colour = guide_legend(override.aes = list(shape=15,size=4)))
    #   
    #   my_list[[i]] <- plot
    # }
    # 
    # output$map <- renderLeaflet({
    #   leaflet(coastal_spal_lonlat) %>% 
    #     setView(lng = 180, lat = -42, zoom = 5) %>%
    #     addTiles() %>% 
    #     addPolygons(#data=coastal_spal_lonlat,
    #       stroke = T, smoothFactor = 0.2, fillOpacity = 1,fillColor = ~factpal(ECOREGION),
    #       highlightOptions = highlightOptions(color = "white", weight = 2,
    #                                           bringToFront = F),#T
    #       #layerId = ~REALM,
    #       popup = paste0("ECOREGION: ",coastal_spal_lonlat$ECOREGION,"<br/>", popupGraph(my_list,width=800,height=500))) %>% 
    #     #popup = paste0(" Realm: ",coastal_spal_lonlat$REALM, "<br/>  Province: ",coastal_spal_lonlat$PROVINCE)) %>% 
    #     addCircleMarkers(data=npix) %>% 
    #     addProviderTiles("Esri.WorldPhysical") %>%
    #     addPolygons(data=coast,col='grey',fillOpacity = 1)
    #   
    # })
    
    
    
    ## Interactive Plot ###########################################
    MHW_dplyr_realm <- reactive({
         MHW_dplyr %>% 
            dplyr::filter(ECOREGION==input$ecoregion)

    })
    
    output$trendplot <- renderPlotly({    
        p <- ggplot(MHW_dplyr_realm(),aes(year,values,col=season)) + 
            facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
            geom_line(size=.5) + 
            geom_smooth(se=T,size=1.5) + 
            ylab('') + xlab('Year') + 
            scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
            theme_bw() + 
            theme(legend.position="bottom",
                  legend.title = element_text(size=16),
                  legend.text = element_text(size=16),
                  strip.text = element_text(size=12), 
                  axis.text=element_text(size=12),
                  axis.title = element_text(size=16))+
            guides(colour = guide_legend(override.aes = list(shape=15,size=8)))
        ggplotly(p) %>% layout(legend = list(
            orientation = "h", x = 0.4, y = -0.2))
    })
    
    MHW_dplyr_DOC <- reactive({
      MHW_dplyrDOC %>% 
        dplyr::filter(Region==input$bioregion)
      
    })
    
    output$trendplotDOC <- renderPlotly({    
      p <- ggplot(MHW_dplyr_DOC(),aes(year,values,col=season)) + 
        facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
        geom_line(size=.5) + 
        geom_smooth(se=T,size=1.5) + 
        ylab('') + xlab('Year') + 
        scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
        theme_bw() + 
        theme(legend.position="bottom",
              legend.title = element_text(size=16),
              legend.text = element_text(size=16),
              strip.text = element_text(size=12), 
              axis.text=element_text(size=12),
              axis.title = element_text(size=16))+
        guides(colour = guide_legend(override.aes = list(shape=15,size=8)))
      ggplotly(p) %>% layout(legend = list(
        orientation = "h", x = 0.4, y = -0.2))
    })
    
    ## Stats Table ###########################################
    MHW_Trends_realm <- reactive({
        MHW_Trends %>% 
            dplyr::filter(Region_Name==input$ecoregion) 
    })
    
    output$table <- DT::renderDataTable(datatable(MHW_Trends_realm(),
                                                  options = list(
                                                      pageLength = 30)) %>% 
                                            DT::formatStyle('P_Value',target = 'row',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('P_Value_pre',target = 'cell',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('P_Value_post',target = 'cell',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('Trend_Decadal',target = 'row',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>% 
                                            DT::formatStyle('Trend_Decadal_pre',target = 'cell',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>% 
                                            DT::formatStyle('Trend_Decadal_post',target = 'cell',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>% 
                                            formatRound(c(5,6,8:12), 4) %>% 
                                            formatStyle(columns = c(1:12), 'text-align' = 'center')
                                        )
    MHW_DOC_Trends <- reactive({
      MHW_DOC_trends_FULL %>% 
        dplyr::filter(Region_Name==input$bioregion) 
    })
    
    output$table2 <- DT::renderDataTable(datatable(MHW_DOC_Trends(),
                                                  options = list(
                                                  pageLength = 30)) %>% 
                                          DT::formatStyle('P_Value',target = 'row',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                          DT::formatStyle('P_Value_pre',target = 'cell',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                          DT::formatStyle('P_Value_post',target = 'cell',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                          DT::formatStyle('Trend_Decadal',target = 'row',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>%
                                          DT::formatStyle('Trend_Decadal_pre',target = 'cell',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>%
                                          DT::formatStyle('Trend_Decadal_post',target = 'cell',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>%
                                          formatRound(c(5,6,8:12), 4) %>%
                                          formatStyle(columns = c(1:12), 'text-align' = 'center')
    )    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
