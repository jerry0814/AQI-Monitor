#source("check_packages.R")
#check_packages(c("shiny","XML","leaflet","geosphere", "zipcode","darksky","dplyr","ggplot2","ggrepel","rvest","rdrop2")) 
library(shiny)
library(XML)
library(leaflet)
library(geosphere)
library(zipcode)
library(darksky)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rvest)
library(rdrop2)


data(zipcode)
load("counties.rda")
load("info_usa.rda")
load("statecode.rda")


token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
drop_get("drop_test/realtime_usa.rda")
load("realtime_usa.rda")
file.remove("realtime_usa.rda")

ui <- fluidPage(tags$head(
        tags$style(HTML(".navbar .navbar-nav {float: right}
                        "))
        ),
        navbarPage(HTML("<style>h{color: white;text-align:center;font-family:verdana;font-size:24px}
                        l{color: white;text-align:center;font-family: verdana;font-weight:100;font-size:16px}
                        </style>
                        <body><h >Air Quality Monitor</h></body>"),inverse=TRUE,
                   theme = "style.css",
                   tabPanel(HTML("<body><l>Home</l></body>"),
                            absolutePanel(top = 0,right ="0%",left="0%",
                                          column(img(src='https://cdn.theatlantic.com/assets/media/img/photo/2013/01/chinas-toxic-sky/c01_59565822/main_1200.jpg',heigh="440%",width="440%",align="top",style = 'opacity: 0.30'),width=3)
                            ),
                            absolutePanel(top = 70,width = "95%",
                                          br(),
                                          helpText(HTML("<style>h1{color:black;font-family:impact;text-align:center;font-size:56px;line_height:150%}p{color:black;text-align:center;font-family:verdana;verdana:20px;font-weight:light;}b{color: black;text-align:center;font-size:22px}</style>
                                                        <body>
                                                        <h1>Welcome!</h1>
                                                        
                                                        <p>There is invisible threat around you!</p>
                                                        <p>I am talking about air pollution!</p>
                                                        <p>I am going to help you monitor it.</p>
                                                        <p>You can find out real time pollution level in the <b>Overview</b> page.</p> 
                                                        <p>You can explore real time and future pollution at certain location by enter zipcode in the <b>Details</b> page.</p>
                                                        <p>You can get advice on how to avoid heavy air pollution in the <b>Advice</b> page.</p>
                                                        <p>To start, please clik the button below.</p>
                                                        </body>")
                                          )
                                          ),
                            absolutePanel(id = "control0",top = 450,width="100%",
                                          tags$head(tags$script('
                                                                Shiny.addCustomMessageHandler("myCallbackHandler0",
                                                                function(typeMessage) {console.log(typeMessage)
                                                                if(typeMessage == 2){
                                                                console.log("got here");
                                                                $("a:contains(Overview)").click();
                                                                }
                                                                });
                                                                ')
                                          ),
                                          column(4,
                                                 h3("")),
                                          column(4,
                                                 actionButton("submit","Let's Start",width = "80%")),
                                          column(4,
                                                 h3(""))
                                          )
                            
                                          ),
                   tabPanel(HTML("<body><l>Overview</l></body>"),
                            br(),
                            br(),
                            absolutePanel(id = "loading",top = 40,width = "95%",
                                          img(src="Teddy_Bear_Loading.gif",width = "100%",style = 'opacity: 0.80')),
                            leafletOutput("map",height = 750),
                            absolutePanel(id = "control1",top = 55,right = 30,fixed = FALSE,
                                          tags$head(tags$script('
                                                                Shiny.addCustomMessageHandler("myCallbackHandler",
                                                                function(typeMessage) {console.log(typeMessage)
                                                                if(typeMessage == 1){
                                                                console.log("got here");
                                                                $("a:contains(Details)").click();
                                                                }
                                                                if(typeMessage == 2){
                                                                $("a:contains(Select Data range)").click();
                                                                }
                                                                });
                                                                ')),
                                          actionButton("submit0",h5("Explore AQI at Your Location"))
                                          ),
                            br(),
                            br(),
                            h2("Nationwide Statistic",align = "center"),
                            hr(),
                            fluidRow(
                                    h3("Pollution Levels among States",align = "center"),
                                    br(),
                                    column(4,
                                           plotOutput("pie_pm25")),
                                    column(4,
                                           plotOutput("pie_so2"),
                                           img(src = "legend.png",width = "100%")),
                                    column(4,
                                           plotOutput("pie_o3"))
                            ),
                            br(),
                            br(),
                            
                            fluidRow(
                                    h3("Most Polluted Area",align = "center"),
                                    column(4,
                                           plotOutput("most_pm25")),
                                    column(4,
                                           plotOutput("most_so2")),
                                    column(4,
                                           plotOutput("most_o3"))
                            )
                            ),
                   tabPanel(HTML("<body><l>Details</l></body>"),
                            
                            fluidRow(column(4,
                                            br(),
                                            br(),
                                            br(),
                                            h4(textOutput("text0")),
                                            tableOutput("caqitable"),
                                            br(),
                                            h4(textOutput("text1")),
                                            tableOutput("aqitable")),
                                     column(8,
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            leafletOutput("map2",height = 500))
                                     
                            ),
                            div(class="outer",
                                
                                
                                absolutePanel(id = "controls", top = 60, right = 20,
                                              width = 270, height = 120,
                                              fluidRow(
                                                      column(8,
                                                             textInput("zipcode",label = NULL)),
                                                      column(4,
                                                             actionButton("submit1","GO"))
                                              ),
                                              HTML("<style>p1{color: #4C4A4A;text-align:left;font-size: 15px;font-family: verdana}</style>
                                                   <body><p1>Please enter zipcode to get detailed information</p1></body>")
                                              ))
                                ),
                   tabPanel(HTML("<body><l>Advice</l></body>"),
                            fluidRow(
                                    column(12,
                                           
                                           column(4,id = "healthinput",
                                                  
                                                  h4("Please Input Your Health Data",style = "font-family: verdana;font-weight:bold"),
                                                  numericInput("age",label="Please Enter Your Age:",0,0,100,1,width = "95%"),
                                                  selectInput("gender",label="Please Select Your Sex:",choices=c("","Male","Female"),width = "95%"),
                                                  radioButtons("heightunit","Please choose the unit for your height",c("Feet"="ft","Centimeter"="cm"),inline=TRUE),
                                                  numericInput("height",label="Please Enter Your Height(cm):",160,0,250,1,width = "95%")),
                                           
                                           
                                           column(4,
                                                  br(),
                                                  radioButtons("weightunit","Please choose the unit for your weight",c("Pound"="lb","Kilogram"="kg"),inline=TRUE),
                                                  numericInput("weight",label="Please Enter Your Weight(kg):",50,0,250,1,width = "95%"),
                                                  numericInput("vitalcap",label="Please Enter Your Vital Capacity:",3000,0,10000,100,width = "95%"),
                                                  selectInput("disease",label="Please Indicate Whether You Have the Following Condition:",choices=c("None","Asthma","Lung Cancer","Cardiovascular Disease"),width = "95%")
                                           ),
                                           column(4,
                                                  h4("Please Input Your Zipcode",style = "font-family: verdana;font-weight:bold"),
                                                  textInput("zipcodead",label = NULL,value="",width = "95%"),
                                                  actionButton("submit2","Submit"),
                                                  br(),
                                                  br(),
                                                  hr(),
                                                  div("Hint:",size=12),
                                                  div("Advices will be given based on both your personal health data and pollution level at your location.",align = "left"),
                                                  div("Please input all the information to get advices.",align = "left"))),
                                    br(),
                                    br(),
                                    h2("More Advices for you",align = "center"),
                                    hr(),
                                    column(12,
                                           h4("Hourly prediction of AQI"),
                                           plotOutput("hourpre"),
                                           
                                           h4("Outdoor Exercise Recomendation"),
                                           uiOutput("exercise")
                                    )
                            )
                            
                   ),
                   tabPanel(HTML("<body><l>Help</l></body>"),
                            h3("What Is Air Quality Index(AQI)?"),
                            br(),
                            p("An air quality index (AQI) is a number used by government agencies to communicate to the public how polluted the air currently is or how polluted it is forecast to become. As the AQI increases, an increasingly large percentage of the population is likely to experience increasingly severe adverse health effects.",style="color: black;text-align:left;font-size: 18px;font-family:arial"),
                            br(),
                            br(),
                            h3("How Do We Measure Air Quality and Pollution?",style="text-align:left"),
                            HTML("<head>
                                 <style>
                                 table {font-family: arial, sans-serif;border-collapse: collapse;width: 100%;}
                                 td, th {border: 1px solid #dddddd;text-align: left;padding: 8px;}
                                 tr:nth-child(even) {background-color: #dddddd;}
                                 </style>
                                 </head>
                                 <body>
                                 <table>
                                 <tr>
                                 <th>Air Quality Index(AQI) Values</th>
                                 <th>Levels of Health Concern</th>
                                 </tr>
                                 <tr>
                                 <td>0 to 50</td>
                                 <td>Good</td>
                                 </tr>
                                 <tr>
                                 <td>51 to 100</td>
                                 <td>Moderate</td>
                                 </tr>
                                 <tr>
                                 <td>101 to 150</td>
                                 <td>Unhealthy for Sensitive Groups</td>
                                 </tr>
                                 <tr>
                                 <td>151 to 200</td>
                                 <td>Unhealthy</td>
                                 </tr>
                                 <tr>
                                 <td>201 or above</td>
                                 <td>Very Unhealty</td>
                                 </tr>
                                 </table>
                                 </body>
                                 "),
                            br(),
                            br(),
                            h3("How Do We Predict Air Pollution?"),
                            fluidRow(id="Model1",img(src="Model1.png",width = "70%")),
                            fluidRow(id="Model2",img(src="Model2.png",width = "70%")),
                            fluidRow(id="Model3",img(src="Model3.png",width = "70%")),
                            br(),
                            br(),
                            h3("How Do We Give Advice?"),
                            p("Based on the research conducted by Giles, L.V. and Koehle titled 'The Health Effects of Exercising in Air Pollution', we could see that air pollution affect exercise performance and could even adverse health effect. Thus, during severe air pollution time, people should choose their exercise type and exercise place carefully.",style="color: black;text-align:left;font-size: 16px;font-family:arial"),
                            p("According to Centers for Disease Control and Prevention and American College of Sports Medicine guidelines, sports that consume 3.5-7 kcal energy per minute are consider moderate activity while sports that consume more than 7 kcal energy per minute are consider vigorous activity. We categorized common type of sports which will be used in advice panel of our app. The detailed categorized result is as following:",style="color: black;text-align:left;font-size: 16px;font-family:arial"),
                            HTML("<head>
                                 <style>
                                 table {font-family: arial, sans-serif;border-collapse: collapse;width: 100%;}
                                 td, th {border: 1px solid #dddddd;text-align: left;padding: 8px;}
                                 tr:nth-child(even) {background-color: #dddddd;}
                                 </style>
                                 </head>
                                 <body>
                                 <table>
                                 <tr>
                                 <th>Moderate level</th>
                                 <th>Vigorous level</th>
                                 </tr>
                                 <tr>
                                 <td>Hiking</td>
                                 <td>Relay race</td>
                                 </tr>
                                 <tr>
                                 <td>Skateboarding</td>
                                 <td>Taekwondo</td>
                                 </tr>
                                 <tr>
                                 <td>Shot put</td>
                                 <td>Karate</td>
                                 </tr>
                                 <tr>
                                 <td>Boxing</td>
                                 <td>Judo</td>
                                 </tr>
                                 <tr>
                                 <td>Table tennis</td>
                                 <td>Basketball</td>
                                 </tr>
                                 <tr>
                                 <td>Swimming</td>
                                 <td>Soccer</td>
                                 </tr>
                                 <tr>
                                 <td>Diving</td>
                                 <td>Handball</td>
                                 </tr>
                                 <tr>
                                 <td>Parallel bars</td>
                                 <td>Synchronized swimming</td>
                                 </tr>
                                 <tr>
                                 <td>Trampoline</td>
                                 <td>Water polo</td>
                                 </tr>
                                 <tr>
                                 <td>Bowling</td>
                                 <td>Ice hockey</td>
                                 </tr>
                                 <tr>
                                 <td>Javelin throw</td>
                                 <td>Volleyball</td>
                                 </tr>
                                 <tr>
                                 <td>Golf</td>
                                 <td>Mountain bike cycling</td>
                                 </tr>
                                 <tr>
                                 <td>Weightlifting</td>
                                 <td>Speed skating</td>
                                 </tr>
                                 <tr>
                                 <td>Darting</td>
                                 <td>Badminton</td>
                                 </tr>
                                 <tr>
                                 <td>Motorcycling</td>
                                 <td>Skiing</td>
                                 </tr>
                                 <tr>
                                 <td>Pommel</td>
                                 <td>Hockey</td>
                                 </tr>
                                 <tr>
                                 <td>Motorboating</td>
                                 <td>Hurdle racing</td>
                                 </tr>
                                 <tr>
                                 <td>Ski jumping</td>
                                 <td>Rowing</td>
                                 </tr>
                                 <tr>
                                 <td>Equestrian events</td>
                                 <td>Fencing</td>
                                 </tr>
                                 <tr>
                                 <td>Air rifle shooting</td>
                                 <td>Canoeing</td>
                                 </tr>
                                 <tr>
                                 <td>Rifle prone</td>
                                 <td>Sailing</td>
                                 </tr>
                                 <tr>
                                 <td>Kart racing</td>
                                 <td>Wrestling</td>
                                 </tr>
                                 </table>
                                 </body>
                                 "),
                            p("We divided air pollution risk into 5 level based on AQI. According to Nikolaos I. S., children, elderly and people with certain disease (e.g. lung cancer, asthma and Cardiovascular Disease). If user belong to one of these categories listed above, we would increase their risk level by one. If risk level is one or two, users receive suggestion that they can do both moderate and vigorous exercise. If risk level reaches three, we give advice that users had better only do moderate exercise. If risk level is larger than three, we suggest users stay indoors and do not go out for exercise.",style="color: black;text-align:left;font-size: 16px;font-family:arial"),
                            br(),
                            br(),
                            h3("References"),
                            helpText(HTML("<p style='color:black;text-align: left;font-size: 15px'>[1] Yang, I. A., Fong, K. M., Zimmerman, P. V., Holgate, S. T., & Holloway, J. W. (2009). Genetic susceptibility to the respiratory effects of air pollution. <i>Postgraduate Medical Journal,</i> 85(1006), 428-436. doi:10.1136/thx.2007.079426</p>
                                          <p style='color:black;text-align: left;font-size: 15px'>[2] Gilliland, F. D. (2009). Outdoor Air Pollution, Genetic Susceptibility, and Asthma Management: Opportunities for Intervention to Reduce the Burden of Asthma.<i> Pediatrics,</i> 123(Supplement). doi:10.1542/peds.2008-2233g</p>
                                          <p style='color:black;text-align: left;font-size: 15px'>[3] Levy, J. I., Greco, S. L., & Spengler, J. D. (2002). The Importance of Population Susceptibility for Air Pollution Risk Assessment: A Case Study of Power Plants Near Washington, DC. <i>Environmental Health Perspectives,</i> 110(12), 1253-1260. doi:10.1289/ehp.021101253</p>
                                          <p style='color:black;text-align: left;font-size: 15px'>[4] Feng, X., Li, Q., Zhu, Y., Hou, J., Jin, L., & Wang, J. (2015). Artificial neural networks forecasting of PM2.5 pollution using air mass trajectory based geographic model and wavelet transformation. <i>Atmospheric Environment,</i> 107, 118-128. doi:10.1016/j.atmosenv.2015.02.030</p>
                                          <p style='color:black;text-align: left;font-size: 15px'>[5] Bahari, R. A., Abbaspour, R. A., & Pahlavani, P. (2014). Prediction Of Pm2.5 Concentrations Using Temperature Inversion Effects Based On An Artificial Neural Network. <i>ISPRS - International Archives of the Photogrammetry, Remote Sensing and Spatial Information Sciences,</i> XL-2/W3, 73-77. doi:10.5194/isprsarchives-xl-2-w3-73-2014</p>
                                          "))
                            )
                            ))






server <- function(input, output, session) {
        
        observe({
                if(input$submit > 0){
                        print('2')
                        session$sendCustomMessage("myCallbackHandler0", "2")
                }
        })
        
        
        
        #Get the county data from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_csa_5m.zip
        #counties <- readOGR("D:/BROWN/2016 courses/PHP 2560/cb_2015_us_county_500k/cb_2015_us_county_500k.shp",
        #layer = "cb_2015_us_county_500k", verbose = FALSE)
        
        #match the counties with the monitoring sites
        county_lonlat <- counties@polygons[[1]]@labpt
        for (i in 2:length(counties@polygons)) {
                county_lonlat <- rbind(county_lonlat,counties@polygons[[i]]@labpt)
        }
        county_lonlat <- data.frame(matrix(county_lonlat,ncol = 2,byrow = FALSE))
        names(county_lonlat) <- c("longitude","latitude")
        countyname <- data.frame(counties$NAME,stringsAsFactors = FALSE)
        county_lonlat <- data.frame(countyname,county_lonlat,stringsAsFactors = FALSE)
        
        #compute the pollution for every county
        
        county_pollute <- function(x,y,type){
                sites_lonlat <- as.matrix(realtime_usa[which(!is.na(realtime_usa[type])),2:3])
                pollute <- realtime_usa[which(!is.na(realtime_usa[type])),]
                pollute <- cbind(pollute[,1],pollute[type])
                lonlat <- matrix(rep(c(x,y),dim(sites_lonlat)[1]),ncol = 2,byrow = TRUE)
                dist <- distGeo(sites_lonlat,lonlat)
                dist <- cbind(pollute,dist)
                dist <- dist[order(dist$dist),]
                return(dist[1,2])
        }
        
        #compute the PM2.5,SO2,O3 for all the counties around the nation
        county_pm25 <- mapply(county_pollute,x = county_lonlat[,2],y = county_lonlat[,3],type = "PM2.5")
        county_SO2 <- mapply(county_pollute,x = county_lonlat[,2],y = county_lonlat[,3],type = "SO2")
        county_O3 <- mapply(county_pollute,x = county_lonlat[,2],y = county_lonlat[,3],type = "O3")
        
        #draw the nationwide map  
        pal1 <- colorNumeric(
                palette = c("#FFEDA0","tomato2"),
                domain = county_pm25
        )
        pal2 <- colorNumeric(
                palette = c("darkolivegreen1","gold4"),
                domain = county_SO2
        )
        pal3 <- colorNumeric(
                palette = c("skyblue1","midnightblue"),
                domain = county_O3
        )
        
        siteIcon <- makeIcon(
                iconUrl = "http://image.flaticon.com/icons/png/512/283/283386.png",
                iconWidth = 20, iconHeight = 20,
                iconAnchorX = 0, iconAnchorY = 50
        )
        pop <- function(x){
                return(paste(x["sites"],"\n","PM2.5:",x["PM2.5"],"\n","SO2:",x["SO2"],"\n","O3:",x["O3"]))
        }
        popups <- realtime_usa[,1]
        for (i in 1:dim(realtime_usa)[1]) {
                popups[i] <- pop(realtime_usa[i,])
        }
        
        
        output$map <- renderLeaflet({
                leaflet(counties) %>%
                        addTiles(group = "OSM (default)") %>%
                        setView(-95,39,zoom = 5) %>%
                        addMarkers(lng = realtime_usa[,2],lat = realtime_usa[,3],icon = siteIcon,popup = popups,group = "monitor sites") %>%
                        addPolygons(
                                stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "PM2.5",
                                color = ~pal1(county_pm25)
                        ) %>% 
                        addPolygons(
                                stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "SO2",
                                color = ~pal2(county_SO2)
                        ) %>%
                        addPolygons(
                                stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "O3",
                                color = ~pal3(county_O3)
                        ) %>%
                        addLegend("bottomright", pal = pal3, values = county_O3,
                                  opacity = 1,title = "O3   AQI",bins=5
                        ) %>%
                        addLegend("bottomright", pal = pal2, values = county_SO2,
                                  opacity = 1,title = "SO2  AQI",bins=5
                        ) %>%
                        addLegend("bottomright", pal = pal1, values = county_pm25,
                                  opacity = 1,title = "PM2.5 AQI",bins=5
                        ) %>%
                        addLayersControl(
                                overlayGroups = c("PM2.5", "SO2","O3","monitor sites"),
                                options = layersControlOptions(collapsed = FALSE)
                        ) %>%
                        hideGroup(c("SO2","O3","monitor sites"))
        })
        
        #draw some statistical plot for the nationwide data.
        
        countypollute <- cbind(as.character(countyname[[1]]),data.frame(as.numeric(counties$STATEFP)),county_pm25,county_SO2,county_O3)
        names(countypollute) <- c("county","state","PM2.5","SO2","O3")
        library(ggplot2)
        library(dplyr)
        
        
        #plot
        statepollute <- countypollute %>% select(state,PM2.5,SO2,O3) %>% group_by(state) %>% 
                summarise(stateplt1 = mean(PM2.5,na.rm=TRUE),
                          stateplt2 = mean(SO2,na.rm=TRUE),
                          stateplt3 = mean(O3,na.rm=TRUE))
        statepollute <- statepollute[-c(3,7,14,43),]
        statepollute$name <- statecode[1:52,1]
        evaluation <- function(m){
                if (is.na(m)) {return(NA)} 
                else {
                        if (m<= 50) {return("Good")}
                        else {if (m <= 100) {return("Moderate")}
                                else {if (m <= 150) {return ("Unhealthy for Sensitive Group")}
                                        else {if (m <= 200) {return("Unhealthy")}
                                                else {return("Very Unhealty")}}}}
                }
        }
        statepollute$eva1 <- mapply(evaluation,data.frame(unlist(statepollute[,2]))[,1]) 
        #pm2.5 plot 
        output$pie_pm25 <- renderPlot({
                ggplot(statepollute, aes(x = factor(1),fill = factor(eva1))) +
                        geom_bar(width = 1)+coord_polar(theta = "y") +
                        scale_fill_manual(values = c("darkolivegreen1", "gold1","salmon","indianred2","indianred4")) +
                        theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              legend.position="none",
                              plot.title = element_text(hjust = 0.5,size = 18)) + ggtitle("PM2.5")
        })
        
        
        statepollute1 <- statepollute[order(statepollute$stateplt1,decreasing = TRUE),]
        output$most_pm25 <- renderPlot({
                ggplot(statepollute1[1:10,],aes(x = factor(name[1:10],levels = name[1:10]),y = stateplt1[1:10], fill = factor(1)))+
                        scale_fill_manual(values = c("salmon1")) +
                        layer(geom = "bar",stat = "identity",position = "identity") + 
                        xlab("State") +  ylab("PM2.5   AQI") + theme(legend.position = "none")
        })
        
        #SO2 plot
        statepollute$eva2 <- mapply(evaluation,data.frame(unlist(statepollute[,3]))[,1]) 
        output$pie_so2 <- renderPlot({
                ggplot(statepollute, aes(x = factor(1),fill = factor(eva2))) +
                        geom_bar(width = 1)+coord_polar(theta = "y") +
                        scale_fill_manual(values = c("darkolivegreen1", "gold1","salmon","indianred2","indianred4")) +
                        theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              legend.position="none",
                              plot.title = element_text(hjust = 0.5,size = 18)) + ggtitle("SO2")
        })
        
        statepollute2 <- statepollute[order(statepollute$stateplt2,decreasing = TRUE),]
        output$most_so2 <- renderPlot({
                ggplot(statepollute2[1:10,],aes(x = factor(name[1:10],levels = name[1:10]),y = stateplt2[1:10], fill = factor(1) ))+
                        scale_fill_manual(values = c("darkolivegreen3")) +
                        layer(geom = "bar",stat = "identity",position = "identity") + 
                        xlab("State") +  ylab("SO2   AQI") + theme(legend.position = "none")
        })
        
        #O3 plot
        statepollute$eva3 <- mapply(evaluation,data.frame(unlist(statepollute[,4]))[,1]) 
        output$pie_o3 <- renderPlot({
                ggplot(statepollute, aes(x = factor(1),fill = factor(eva3))) +
                        geom_bar(width = 1)+coord_polar(theta = "y") +
                        scale_fill_manual(values = c("darkolivegreen1", "gold1","salmon","indianred2","indianred4")) +
                        theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              legend.position="none",
                              plot.title = element_text(hjust = 0.5,size = 18)) + ggtitle("O3")
        })
        
        statepollute3 <- statepollute[order(statepollute$stateplt3,decreasing = TRUE),]
        output$most_o3 <- renderPlot({
                ggplot(statepollute3[1:10,],aes(x = factor(name[1:10],levels = name[1:10]),y = stateplt3[1:10], fill = factor(1) ))+
                        scale_fill_manual(values = c("skyblue2")) +
                        layer(geom = "bar",stat = "identity",position = "identity") + 
                        xlab("State") +  ylab("O3   AQI") + theme(legend.position = "none")
        })
        
        #link the button to the details page        
        observe({
                if(input$submit0 > 0){
                        print('1')
                        session$sendCustomMessage("myCallbackHandler", "1")
                }
        })
        
        prediction <- function(x) {
                data(zipcode)
                #load("counties.rda")
                #load("realtime_usa.rda")
                
                zip_info <- subset(zipcode,zipcode == x)
                ############functions########################################################
                # AQI to Concentration
                Invaqi <- function(aqihigh,aqilow,conchigh,conclow,aqi){
                        c <- ((aqi - aqilow)/(aqihigh - aqilow))*(conchigh - conclow) + conclow
                        return(c)
                }
                conc_pm2.5 <- function(aqi){
                        if (aqi >= 0 && aqi <= 50) conc <- Invaqi(50,0,12,0,aqi)
                        else if (aqi >= 51 && aqi <= 100) conc <- Invaqi(100,51,35.4,12.1,aqi)
                        else if (aqi >= 101 && aqi <= 150) conc <- Invaqi(150,101,55.4,35.5,aqi)
                        else if (aqi >= 151 && aqi <= 200) conc <- Invaqi(200,151,150.4,55.5,aqi)
                        else if (aqi >= 201 && aqi <= 300) conc <- Invaqi(300,201,250.4,150.5,aqi)
                        else if (aqi >= 400 && aqi <= 301) conc <- Invaqi(400,301,350.4,250.5,aqi)
                        else if (aqi >= 500 && aqi <= 401) conc <- Invaqi(500,401,500.4,350.5,aqi)
                        return(round(conc))
                }
                
                
                ## concentration to AQI
                aqi_conc <- function(aqihigh,aqilow,conchigh,conclow,conc){
                        a <- ((conc - conclow)/(conchigh - conclow))*(aqihigh - aqilow) + aqilow
                        return(a)
                }
                pm2.5_aqi <- function(c){
                        if (c < 0) conc <- local$PM2.5[1]
                        else if (c >= 0 && c < 12.1) conc <- aqi_conc(50,0,12,0,c)
                        else if (c >= 12.1 && c < 35.5) conc <- aqi_conc(100,51,35.4,12.1,c)
                        else if (c >= 35.5 && c < 55.5) conc <- aqi_conc(150,101,55.4,35.5,c)
                        else if (c >= 55.5 && c < 150.5) conc <- aqi_conc(200,151,150.4,55.5,c)
                        else if (c >= 150.5 && c < 250.5) conc <- aqi_conc(300,201,250.4,150.5,c)
                        else if (c >= 250.5 && c < 350.5) conc <- aqi_conc(400,301,350.4,250.5,c)
                        else if (c >= 350.5 && c < 500.5) conc <- aqi_conc(500,401,500.4,350.5,c)
                        return(round(conc))
                }
                ###########################################################################
                ##match the zipcode with monitor sites
                
                
                sites_lonlat <- as.matrix(info_usa[,2:3])
                zip_lonlat <- matrix(rep(c(zip_info$longitude,zip_info$latitude),dim(sites_lonlat)[1]),ncol = 2,byrow = TRUE)
                dist <- distGeo(sites_lonlat,zip_lonlat)
                dist <- cbind(info_usa,dist)
                dist <- dist[order(dist$dist),]
                
                get_pollute <- function(x) {
                        url <- read_html(x)
                        table <- html_table(html_nodes(url,"div#citydivmain table"),fill = TRUE)
                        if (length(which(grepl("PM2.5|PM10|SO2|O3|NO2|CO",table))) == 0) {
                                pollute <- rep(NA,6)
                        } else {
                                table <- table[which(grepl("PM2.5|PM10|SO2|O3|NO2|CO",table))]
                                table <- data.frame(table)[,c(1,2)]
                                patterns <- c("PM2.5","PM10","SO2","O3","NO2","CO")
                                find_pollute <- function(x) {
                                        return(ifelse(sum(grepl(x,table[,1])) != 0,table[grepl(x,table[,1]),2],NA))
                                }
                                pollute <- sapply(patterns,find_pollute)
                                pollute <- as.numeric(unlist(pollute))
                                return(pollute)
                        }
                }
                k=0
                pol <- c(NA,NA,NA,NA,NA,NA)
                for (i in 1:dim(dist)[1]) {
                        pol <- rbind(pol,get_pollute(dist[i,5]))
                        if (!is.na(pol[i+1,1])) {k <- k+1}
                        if (k == 3) {break}
                }
                pol <- data.frame(pol[2:dim(pol)[1],])
                names(pol) <- c("PM2.5","PM10","SO2","O3","NO2","CO")
                # put away non-NA
                dist <- cbind(dist[1:dim(pol)[1],],pol)
                dist1 <- dist[which(!is.na(dist$PM2.5)),]
                
                ##call the matched monitor sites dataset "local" 
                local <- dist1[1:3,]
                
                #deal with the same distance situation
                if (length(unique(local$dist))<3){
                        local$dist[1] <- local$dist[1] + rnorm(1, mean = 0, sd = 0.01)
                        local$dist[2] <- local$dist[2] + rnorm(1, mean = 0, sd = 0.01)
                        local$dist[3] <- local$dist[3] + rnorm(1, mean = 0, sd = 0.01)
                        
                }
                
                
                # azimuth calculation
                rad_earth <- 6371.3*1000
                # Projection on cartesian coordinate
                cartesian <- function(lat, long){
                        z1 <- rad_earth*sin(lat*pi/180)
                        x1 <- rad_earth*cos(lat*pi/180)*cos(long*pi/180)
                        y1 <- rad_earth*cos(lat*pi/180)*sin(long*pi/180)
                        return(c(x1,y1,z1))
                }
                #cartesian coordinate for four point
                local0 <- cartesian(zip_info$latitude, zip_info$longitude)
                local1 <- cartesian(local$latitude[1], local$longitude[1])
                local2 <- cartesian(local$latitude[2], local$longitude[2])
                local3 <- cartesian(local$latitude[3], local$longitude[3])
                
                # calculate azimuth for three sites compared to zipcode location 
                # central angel of an arc
                ab <- acos((local0[1]*local1[1]+local0[2]*local1[2]+local0[3]*local1[3])/rad_earth^2)
                ac <- (90 - zip_info$latitude)*pi/180
                bc <- (90 - local$latitude[1])*pi/180
                
                p <- (ab+ac+bc)/2
                azimuth1 <- 2*acos(sqrt((sin(p)*sin(p-bc))/(sin(ac)*sin(ab))))
                
                ab <- acos((local0[1]*local2[1]+local0[2]*local2[2]+local0[3]*local2[3])/rad_earth^2)
                ac <- (90 - zip_info$latitude)*pi/180
                bc <- (90 - local$latitude[2])*pi/180
                
                p <- (ab+ac+bc)/2
                azimuth2 <- 2*acos(sqrt((sin(p)*sin(p-bc))/(sin(ac)*sin(ab))))
                
                ab <- acos((local0[1]*local3[1]+local0[2]*local3[2]+local0[3]*local3[3])/rad_earth^2)
                ac <- (90 - zip_info$latitude)*pi/180
                bc <- (90 - local$latitude[3])*pi/180
                
                p <- (ab+ac+bc)/2
                azimuth3 <- 2*acos(sqrt((sin(p)*sin(p-bc))/(sin(ac)*sin(ab))))
                local$azimuth <- round(c(azimuth1, azimuth2, azimuth3), digits = 3)
                # AQI prediction based on zipcode inputed 
                
                #weather data
                Sys.setenv(DARKSKY_API_KEY = "fadab9d0a98eb611cf27b65452f46985")
                #key = 03513ab72cb66a7c2854e3628e395953
                weather <- get_current_forecast(zip_info$latitude, zip_info$longitude, units = "us", language = "en",
                                                exclude = "minutely,alerts,flags,currently")
                weather_hourly <- data.frame(weather[1])[1:12,]
                weather_hourly <- weather_hourly[,c("hourly.temperature","hourly.humidity","hourly.windSpeed","hourly.windBearing",
                                                    "hourly.cloudCover","hourly.pressure")]
                
                weather_daily <- data.frame(weather[2])
                weather_daily <- weather_daily[,c("daily.time","daily.temperatureMin","daily.temperatureMax",
                                                  "daily.humidity","daily.windSpeed","daily.windBearing","daily.cloudCover","daily.pressure")]
                
                # target zipcode's AQI
                # normalized inverse distance weights for the three closest monitoring site
                
                a <- local$dist[1]
                b <- local$dist[2]
                c <- local$dist[3]
                
                w1 <- 1/a^2
                w2 <- 1/b^2
                w3 <- 1/c^2
                s <- w1+w2+w3
                w1<- w1/s
                w2 <- w2/s
                w3 <- w3/s
                # wind speed based on hourly
                I <- mean(weather_hourly$hourly.windSpeed)
                
                
                local$conc_pm2.5 <- apply(local["PM2.5"],1,conc_pm2.5)
                
                # criteria for azimuth
                wind_angle <- mean(weather_hourly$hourly.windBearing)
                range_angle1 <- local$azimuth + 90
                range_angle2 <- local$azimuth + 270
                
                # define a monitoring site as nagative or positive
                if (wind_angle<=range_angle1[1]|wind_angle >= range_angle2[1]){
                        a1 <- -0.01*cos(local$azimuth[1]-wind_angle+360)
                } else{a1 <- 0.01*cos(local$azimuth[1]-wind_angle+360)}
                if (wind_angle<=range_angle1[2]|wind_angle >= range_angle2[2]){
                        a2 <- -0.01*cos(local$azimuth[2]-wind_angle+360)
                } else{a2 <- 0.01*cos(local$azimuth[2]-wind_angle+360)}
                if (wind_angle<=range_angle1[3]|wind_angle >= range_angle2[3]){
                        a3 <- -0.01*cos(local$azimuth[3]-wind_angle+360)
                } else{a3 <- 0.01*cos(local$azimuth[3]-wind_angle+360)}
                
                # Apply the equation from reference to concentration of PM2.5 
                conc_aqi <- ceiling(local$conc_pm2.5[1]*(w1 + a1*I*w1/(w1+w2)) + 
                                            local$conc_pm2.5[2]*(w2 + a2*I*w2/(w1+w2)) + 
                                            local$conc_pm2.5[3]*(w3 + a3*I))
                
                
                # predicted AQI on "click" moment
                aqi <- pm2.5_aqi(conc_aqi)
                
                
                currentlocal <- colMeans(local[,c("PM2.5","SO2","O3")],na.rm = TRUE)
                
                for (i in 1:3) {
                        if(is.nan(currentlocal[i])) {currentlocal[i] <- NA}
                }
                currentlocal[1] <- aqi
                evaluation <- function(x){
                        if (is.na(x)) {return(NA)} 
                        else {
                                if (x <= 50) {return("Good")}
                                else {if (x <= 100) {return("Moderate")}
                                        else {if (x <= 150) {return ("Unhealthy for Sensitive Group")}
                                                else {if (x <= 200) {return("Unhealthy")}
                                                        else {return("Very Unhealty")}}}}
                        }
                }
                currentlocal <- round(currentlocal)
                eva <- mapply(evaluation,currentlocal)
                caqitable <- data.frame(c("PM2.5","SO2","O3"),currentlocal,eva)
                names(caqitable) <- c("Pollution", "AQI","Evaluation")
                
                
                
                
                # day-forecasting model
                
                ## calculate the elevation of zipcode-input location
                # x-longitude, y-latitude
                elevation <- function(x,y){
                        url <- paste0("http://ned.usgs.gov/epqs/pqs.php?x=", x, "&y=", y, "&units=Meters&output=xml")
                        html_text(read_html(url))
                        m <- regexpr("\\d+\\.\\d+", html_text(read_html(url)))
                        elevation <- as.numeric(regmatches(html_text(read_html(url)),m))
                        return(elevation)
                }
                
                elevation0 <- elevation(zip_info$longitude, zip_info$latitude)
                
                ##PM2.5 transportation
                # PM2.5 reside 3-5 days in the atmosphere
                # average wind speed 5m/s
                # On the average, PM2.5 particles are transported 1000 or more km 
                # set up residence time in our model
                res_time <- function(x){
                        y <- x/1000
                        if (y < 1.5) res_time <- round(runif(1,3,5)) # 3-5 days
                        else if (y > 1.5 && y <= 10){
                                res_time <- round(sqrt(45*y)) # based on relationship between height and residence time
                        }
                        return(res_time)
                }
                
                # range of transporation = wind speed * residence time 
                spatial <- weather_daily$daily.windSpeed*res_time(elevation0)*24*60*60/1000
                
                
                ##predict the AQI for input zipcode
                # pollution source move based on wind
                # calculation of local elevation
                elevation1 <- elevation(local$longitude[1], local$latitude[1])
                elevation2 <- elevation(local$longitude[2], local$latitude[2])
                elevation3 <- elevation(local$longitude[3], local$latitude[3])
                local$elevation <- c(elevation1, elevation2, elevation3)
                
                #modified wind speed with Pythagorean theorem
                conc_aqi_new <- rep(0,8)
                
                for (i in 1:res_time(elevation0)){
                        I <- weather_daily$daily.windSpeed[i]
                        w1 <- 1/(sqrt(local$dist[1]^2 + (elevation1-elevation0)^2) + a1*10*spatial[i])^2
                        w2 <- 1/(sqrt(local$dist[2]^2 + (elevation2-elevation0)^2) + a2*10*spatial[i])^2
                        w3 <- 1/(sqrt(local$dist[3]^2 + (elevation3-elevation0)^2) + a3*10*spatial[i])^2
                        s <- w1+w2+w3
                        w1<- w1/s
                        w2 <- w2/s
                        w3 <- w3/s
                        conc_aqi_new[i] <- ceiling(local$conc_pm2.5[1]*(w1 + a1*I*w1/(w1+w2)) + 
                                                           local$conc_pm2.5[2]*(w2 + a2*I*w2/(w1+w2)) + 
                                                           local$conc_pm2.5[3]*(w3 + a3*I))
                }
                
                aqi_new <- mapply(pm2.5_aqi,conc_aqi_new)
                
                
                aqi_table <- c(aqi, aqi_new[aqi_new!=0])
                eva <- mapply(evaluation, aqi_table)
                aqitable <- data.frame(aqi_table,eva)
                names(aqitable) <- c("AQI","Evaluation")
                x <- paste("The next", 1:length(aqi_new[aqi_new!=0]), "day")
                rownames(aqitable) <- c("Today", x)
                
                
                
                
                # hour-forecasting model
                # simple linear model based on wind speed
                ## relationship between wind speed and concentration
                ## wind-bearing define positive or negative effect
                if (a1 + a2 + a3 > 0) x1 <- 1*cos(mean(local$azimuth, na.rm = TRUE) - wind_angle + 360)
                if (a1 + a2 + a3 < 0) x1 <- -1*cos(mean(local$azimuth, na.rm = TRUE) - wind_angle + 360)
                aqitable2 <- rep(0,12)
                aqitable2[1] <-  -x1*0.01*(weather_hourly$hourly.windSpeed[1]) + conc_pm2.5(aqi)
                for (i in 2:12){
                        
                        aqitable2[i] <- -x1*0.01*(weather_hourly$hourly.windSpeed[i]) + aqitable2[i-1]
                }
                
                aqi_hour <- mapply(pm2.5_aqi, aqitable2)
                return(return(list(aqi,caqitable,aqitable,aqi_hour,weather_hourly)))
        } 
        
        #compute everything for the details page once the "go" button being clicked 
        zipcode1 <- eventReactive(input$submit1, input$zipcode)
        
        output$text0 <- renderText("Current Pollution Level:")
        output$caqitable <- renderTable({
                if (zipcode1() %in% zipcode$zip) {
                        aqitable <- prediction(zipcode1())[[2]]
                        aqitable
                } else {0}},
                width = "100%",align = 'c',hover = TRUE,striped = FALSE,bordered = TRUE,rownames = TRUE)
        
        output$text1 <-renderText("Prediction of Air Quality Index:")
        output$aqitable <- renderTable({
                if (zipcode1() %in% zipcode$zip) {
                        aqitable <- prediction(zipcode1())[[3]]
                        aqitable
                } else {0}},
                width = "100%",align = 'c',hover = TRUE,striped = FALSE,bordered = TRUE,rownames = TRUE)
        
        output$map2 <- renderLeaflet({
                zip_info <- subset(zipcode,zipcode == zipcode1())
                leaflet(counties) %>%
                        addTiles(group = "OSM (default)") %>%
                        setView(zip_info$longitude,zip_info$latitude,zoom = 10)%>%
                        addPolygons(
                                stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0, group = "PM2.5",
                                color = ~pal1(county_pm25)
                        ) %>%
                        addPopups(lng = zip_info$longitude,lat = zip_info$latitude, paste0(zip_info$city,",",zip_info$state),options = popupOptions(closeButton = FALSE))%>%
                        addLegend(position = "bottomright", pal = pal1, value = county_pm25, title = "AQI Level" )
        })
        
        
        
        #compute information for advice page
        
        zipcode2 <- eventReactive(input$submit2, list(input$zipcodead,matrix(c(input$heightunit,input$height),ncol=2),matrix(c(input$weightunit,input$weight),ncol=2),input$age,input$disease))
        #weight <- eventReactive(input$submit2, input$weight)
        #height <- eventReactive(input$submit2, input$height)
        #age <- eventReactive(input$submit2, input$age)
        #disease <- eventReactive(input$submit2, input$disease)
        #zipcode2 <- eventReactive(input$submit2,list(input$zipcoded,matrix(c(input$heightunit,input$height),ncol=2)))
        output$hourpre <- renderPlot({
          
          aqi_hour <- prediction(zipcode2()[[1]])[[4]]
          evaluation <- function(x){
            if (is.na(x)) {return(NA)} 
            else {
              if (x <= 50) {return("Can Go Outside")}
              else {if (x <= 100) {return("Can Go Outside")}
                else {if (x <= 150) {return ("Better Not Go Outside")}
                  else {if (x <= 200) {return("Don't Go Outside")}
                    else {return("Don't Go Outside")}}}}
            }
          }
          hour_eva <- mapply(evaluation,aqi_hour)
          
          hourpre <- data.frame(aqi_hour,hour_eva,c(0,1,2,3,4,5,6,7,8,9,10,11))
          names(hourpre) <- c("aqi","eva","time")
          cols <- c("Can Go Outside"="darkolivegreen1","Better Not Go Outside"="salmon","Don't Go Outside"="indianred4")
          ggplot(hourpre,aes(x = time, y = 1.2*(max(aqi) - min(aqi)), fill = factor(eva)))+ 
            geom_bar(stat="identity",position = "identity",width = 1,alpha=0.8) +
            
            geom_line(mapping = aes(x=time,y=aqi-min(aqi)),color = "skyblue3",size = 1.5) +
            scale_fill_manual(name = "",values = cols) +
            scale_x_continuous(name="Time From Now (hours)",breaks = 0:11) +
            
            scale_y_continuous(name="AQI",labels = function(x) x + min(hourpre$aqi)) +
            theme(panel.grid.minor = element_blank(),
                  axis.title = element_text(size=12),
                  legend.position="bottom")+
            guides(fill = guide_legend(keywidth = 8, keyheight = 1,label.position = "right",title.theme = element_text(size=15,angle = 0),label.theme = element_text(size=10,angle = 0)))
          
        })
        
        
        
        output$exercise <- renderUI({
          caqitable <- prediction(zipcode2()[[1]])[[2]]
          aqi <- max(caqitable$AQI,na.rm = TRUE)
          evaluation <- function(x){
            if (is.na(x)) {return(1)} 
            else {
              if (x <= 50) {return(1)}
              else {if (x <= 100) {return(2)}
                else {if (x <= 150) {return (3)}
                  else {if (x <= 200) {return(4)}
                    else {return(5)}}}}
            }
          }
          
          eva_level <- evaluation(aqi)
          if (zipcode2()[[2]][1,1]=="cm"){heighttrue<-as.numeric(zipcode2()[[2]][1,2])}
          else{heighttrue<-as.numeric(zipcode2()[[2]][1,2])*30.48}
          if (zipcode2()[[3]][1,1]=="kg"){weighttrue<-as.numeric(zipcode2()[[3]][1,2])}
          else{weighttrue<-as.numeric(zipcode2()[[3]][1,2])*0.454}
          bmi<- weighttrue/((heighttrue/100)^2)
          while(eva_level>1){
            if (zipcode2()[[4]]<=5|zipcode2()[[4]]>=65){eva_level=eva_level+1}
            if (bmi>=30){eva_level=eva_level+1}
            if (zipcode2()[[5]]=="Asthma"|zipcode2()[[5]]=="Lung Cancer"|zipcode2()[[5]]=="Cardiovascular Disease"){eva_level=eva_level+1}}
          
          if (eva_level==1|eva_level==2){ a = "exerciseforadvice.png"}
          else{
            if (eva_level==3){a = "exercise1.png"}
            else{a = "exercise2.png"}
          } 
          tags$img(src = a,width = "100%")
        })
        
        
        
}
shinyApp(ui, server)