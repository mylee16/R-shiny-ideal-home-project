##### ISSS 602 Applied Statistics in R 
##### Group Project
##### Members : Meng Yong Lee, Alvin Hui Shan Lee, Hai Soon Ang

###########################Libraries Intended
library(shiny)
library(tidyverse)
library(shinythemes)
library(dplyr)
library(ggplot2)


###########################Main Data File
house <-read.csv("Data/Final.csv",header = T,sep = ",",stringsAsFactors = F)
  
#house <- read.csv("C:/Users/user/Desktop/Statistics Project/Final.csv",header=T,sep=",",stringsAsFactors=F)
  
#####################Extraction of Data (Meng Yong)
data_tab2 <<-
  house[c(
    'month',
    'year',
    'resale_price',
    'town',
    'flat_type',
    'postal',
    'flat_model',
    'storey_range',
    'remaining_lease',
    'floor_area_sqm',
    'distance_from_house_in_km' #added to accomodate MY's code - try combine data set
  )]
####################Extraction of Data (Hai Soon and Alvina)
data_hs <-
  house[c(
    'year',
    'town',
    'storey_range',
    'flat_type',
    'resale_price',
    'remaining_lease',
    'floor_area_sqm',
    'flat_model'
  )]
####################Sieve out the unique levels in each type of data
####################Streamline the number of choices in menu

year_choice = sort(unique(data_hs$year))
town_choice = sort(unique(data_hs$town))
storey_range = sort(unique(data_hs$storey_range))
flat_type_choice = sort(unique(data_hs$flat_type))

remaining_lease_choice=sort(unique(data_hs$remaining_lease))
floor_choice = sort(unique(data_hs$floor_area_sqm))
flat_model_choice = sort(unique(data_hs$flat_model))

###################  Regression Formula
###################  Initial attempt to place it in the tab failed.
hf<-lm(data_hs$resale_price~data_hs$town+data_hs$flat_type+data_hs$flat_model+data_hs$floor_area_sqm+data_hs$remaining_lease, data=data_hs)

##################################################################User Interface
ui = tagList(
  navbarPage(
    
    tags$a(href = "https://www.hdb.gov.sg/cs/infoweb/homepage","HDB Resale Pricing"),
    theme = shinytheme("flatly"),

############################ UI for Exploration Starts Here ###################################
        
    tabPanel("Data Explorer",
             sidebarPanel(
               style = "position:fixed;width:inherit;padding-right:20px;",
               sliderInput(
                 "resale_price",
                 label = "Resale Price",
                 min = 0,
                 max = 1000000 ,
                 value = 1000000,
                 step = 10000
               ),
               sliderInput(
                 "remaining_lease",
                 label = "Remaining Lease",
                 min = 1,
                 max = 100 ,
                 value = 100
               ),
               sliderInput(
                 "floor_area_sqm",
                 label = "Floor Area (sqm)",
                 min = 50,
                 max = 260 ,
                 value = 260
               ),
               sliderInput(
                 "distance_from_house_in_km",
                 label = "Distance from Primary School (km)",
                 min = 0,
                 max = 3.5 ,
                 value = 3.5
               ),
               selectInput(
                 "town",
                 label = "Town",
                 choices = c(
                   'Ang Mo Kio',
                   'Bedok',
                   'Bishan',
                   'Bukit Batok',
                   'Bukit Merah',
                   'Bukit Panjang',
                   'Bukit Timah',
                   'Chua Chu Kang',
                   'Clementi',
                   'Dover',
                   'Geylang',
                   'Hougang',
                   'Jurong East',
                   'Jurong West',
                   'Kallang',
                   'Marine Parade',
                   'Novena',
                   'Pasir Ris',
                   'Punggol',
                   'Queenstown',
                   'Rochor',
                   'Sembawang',
                   'Sengkang',
                   'Serangoon',
                   'Tampines',
                   'Toa Payoh',
                   'Woodlands',
                   'Yishun'
                 ),
                 selected = "Bedok"
               ),
               checkboxGroupInput(
                 "flat_type",
                 label = "Flat Type",
                 choices = c("3 Room", "4 Room", "5 Room"),
                 selected = "4 Room"
               ),
               checkboxGroupInput(
                 "year",
                 label = "Year",
                 choices = c("2016", "2017", "2018"),
                 selected = c("2016", "2017", "2018")
               )
               
             ),
             mainPanel(
               tabPanel(
                 "Plot"
                 ,
                 plotOutput("scatterplot1")
                 ,
                 verbatimTextOutput("summary1")
                 ,
                 plotOutput("scatterplot2")
                 ,
                 verbatimTextOutput("summary2")
                 ,
                 plotOutput("scatterplot3")
                 ,
                 verbatimTextOutput("summary3")
                 
               )#MainPanel-tabPanel
             )#mainPanel
    ),#tabPanel Data Explorer

############################ UI for Prediction Starts Here ####################################
    
    tabPanel("Linear Regression",

    sidebarPanel(
      style = "position:fixed;width:inherit;padding-right:20px;",
     
       selectInput(
        inputId = "town_hs",
        label = "Town",
        choices = town_choice,
        selected="Bedok"
      ),
      
      selectInput(
        inputId = "remaining_lease_hs",
        label = "Remaining Lease",
        choices = remaining_lease_choice,
        selected=50
      ),
      
      selectInput(
        inputId = "floor_area_sqm_hs",
        label = "Floor Area (sqm)",
        choices = floor_choice,
        selected=100
      ), 
      
      selectInput(
        inputId = "flat_type_hs",
        label = "Flat Type",
        choices = flat_type_choice,
        selected="5 Room"
      ),
      
      selectInput(
        inputId = "flat_model_hs",
        label = "Flat Model",
        choices = flat_model_choice,
        selected="New Generation"
      ),
      
      sliderInput(
        "confidence_interval_hs",
        label = "Confidence Interval (%)",
        min = 1,
        max = 100 ,
        value = 95
      )

      
      
      
     ),
      
      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Estimated Price",
                   textOutput(outputId = "reg_announce1"),
                   textOutput(outputId = "reg_announce2"),
                   textOutput(outputId = "reg_announce3"),
                   textOutput(outputId = "reg_announce4"),
                   textOutput(outputId = "reg_announce5"),
                   
                   verbatimTextOutput(outputId = "reg_announce_verbatim"
                              )),
         
          tabPanel("Coefficients in Model",
                   verbatimTextOutput(outputId = "reg_c")),

          tabPanel("Distribution of Residuals",
                   plotOutput(outputId = "reg_r")),

          tabPanel("Residuals versus Actual Resale Price",
                   plotOutput(outputId = "reg_r_arp")),
          
          tabPanel("Predicted versus Actual Resale Price",
                    plotOutput(outputId = "reg_p_arp"))
          
        )
    )#mainPanel
    )#tabbar Panel
  )#NavBar (Old)
)#UI

#######################################################################
server <- function(input, output) {

 ###################Exploration Tab Functions
  
  output$scatterplot1 <- renderPlot({
    ggplot(data = data_tab2[data_tab2$town == input$town
                            & data_tab2$flat_type == input$flat_type
                            & data_tab2$year %in% input$year
                            & data_tab2$floor_area_sqm <= input$floor_area_sqm
                            & data_tab2$resale_price <= input$resale_price
                            & data_tab2$remaining_lease <= input$remaining_lease, ]
           , aes(x = floor_area_sqm, y = resale_price / 1000)) +
      geom_point() +
      ggtitle(paste0("Floor Area vs Resale Price")) +
      
      geom_smooth(method = "lm") +
      xlab("Floor Area (sqm)") +
      ylab("Average Resale Price (in Thousands SGD)") +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 20
        ),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, angle = 90)
      )
  })
  
  output$summary1 = renderPrint({
    fit1 <-
      lm(resale_price ~ floor_area_sqm, data = data_tab2[data_tab2$town == input$town
                                                         & data_tab2$flat_type == input$flat_type
                                                         & data_tab2$year %in% input$year
                                                         & data_tab2$floor_area_sqm <= input$floor_area_sqm
                                                         & data_tab2$resale_price <= input$resale_price
                                                         & data_tab2$remaining_lease <= input$remaining_lease, ])
    
    print(summary(fit1)$coefficients[])
    print(paste0('r-squared: ', summary(fit1)$r.squared))
    
    if (summary(fit1)$coefficients[2, 4] < 0.01) {
      print(
        paste0(
          'Variables Correlations are Significant at 99% level with P-Value = ',
          summary(fit1)$coefficients[2, 4]
        )
      )
    } else if (summary(fit1)$coefficients[2, 4] < 0.05) {
      print(
        paste0(
          'Variables Correlations are Significant at 95% level with P-Value = ',
          summary(fit1)$coefficients[2, 4]
        )
      )
    } else
      print(
        paste0(
          'Variables Correlations are NOT Significant with P-Value = ',
          summary(fit1)$coefficients[2, 4]
        )
      )
  })
  
  ### Output 2 - Remaining_Lease
  
  output$scatterplot2 <- renderPlot({
    ggplot(data = data_tab2[data_tab2$town == input$town
                            & data_tab2$flat_type == input$flat_type
                            & data_tab2$year %in% input$year
                            & data_tab2$floor_area_sqm <= input$floor_area_sqm
                            & data_tab2$resale_price <= input$resale_price
                            & data_tab2$remaining_lease <= input$remaining_lease, ]
           , aes(x = remaining_lease, y = resale_price / 1000)) +
      geom_point() +
      ggtitle(paste0("Remaining Lease vs Resale Price")) +
      
      geom_smooth(method = "lm") +
      xlab("Remaining Lease (Years)") +
      ylab("Average Resale Price (in Thousands SGD)") +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 20
        ),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, angle = 90)
      )
  })
  
  output$summary2 = renderPrint({
    fit2 <-
      lm(resale_price ~ remaining_lease, data = data_tab2[data_tab2$town == input$town
                                                          & data_tab2$flat_type == input$flat_type
                                                          & data_tab2$year %in% input$year
                                                          & data_tab2$floor_area_sqm <= input$floor_area_sqm
                                                          & data_tab2$resale_price <= input$resale_price
                                                          & data_tab2$remaining_lease <= input$remaining_lease, ])
    
    print(summary(fit2)$coefficients[])
    print(paste0('r-squared: ', summary(fit2)$r.squared))
    
    if (summary(fit2)$coefficients[2, 4] < 0.01) {
      print(
        paste0(
          'Variables Correlations are Significant at 99% level with P-Value = ',
          summary(fit2)$coefficients[2, 4]
        )
      )
    } else if (summary(fit2)$coefficients[2, 4] < 0.05) {
      print(
        paste0(
          'Variables Correlations are Significant at 95% level with P-Value = ',
          summary(fit2)$coefficients[2, 4]
        )
      )
    } else
      print(
        paste0(
          'Variables Correlations are NOT Significant with P-Value = ',
          summary(fit2)$coefficients[2, 4]
        )
      )
  })
  
  ### Output 3 - Distance from P School
  
  output$scatterplot3 <- renderPlot({
    ggplot(data = data_tab2[data_tab2$town == input$town
                            & data_tab2$flat_type == input$flat_type
                            & data_tab2$year %in% input$year
                            & data_tab2$floor_area_sqm <= input$floor_area_sqm
                            & data_tab2$resale_price <= input$resale_price
                            & data_tab2$remaining_lease <= input$remaining_lease, ]
           ,
           aes(x = distance_from_house_in_km, y = resale_price / 1000)) +
      geom_point() +
      ggtitle(paste0("Distance from Nearest Primary School vs Resale Price")) +
      
      geom_smooth(method = "lm") +
      xlab("Distance from Nearest Primary School (KM)") +
      ylab("Average Resale Price (in Thousands SGD)") +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 20
        ),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, angle = 90)
      )
  })
  
  output$summary3 = renderPrint({
    fit3 <-
      lm(resale_price ~ distance_from_house_in_km, data = data_tab2[data_tab2$town == input$town
                                                                    & data_tab2$flat_type == input$flat_type
                                                                    & data_tab2$year %in% input$year
                                                                    & data_tab2$floor_area_sqm <= input$floor_area_sqm
                                                                    & data_tab2$resale_price <= input$resale_price
                                                                    & data_tab2$remaining_lease <= input$remaining_lease, ])
    
    print(summary(fit3)$coefficients[])
    print(paste0('r-squared: ', summary(fit3)$r.squared))
    
    if (summary(fit3)$coefficients[2, 4] < 0.01) {
      print(
        paste0(
          'Variables Correlations are Significant at 99% level with P-Value = ',
          summary(fit3)$coefficients[2, 4]
        )
      )
    } else if (summary(fit3)$coefficients[2, 4] < 0.05) {
      print(
        paste0(
          'Variables Correlations are Significant at 95% level with P-Value = ',
          summary(fit3)$coefficients[2, 4]
        )
      )
    } else
      print(
        paste0(
          'Variables Correlations are NOT Significant with P-Value = ',
          summary(fit3)$coefficients[2, 4]
        )
      )
  }
  )
  
########################Estimate Price Functions
# 

       output$reg_announce1 <- renderText({ 
         paste("Town: ", input$town_hs)
         })
       output$reg_announce2 <- renderText({ 
         paste("Remaining Lease: ", input$remaining_lease_hs)
       })
       output$reg_announce3 <- renderText({ 
         paste("Floor Area: ",input$floor_area_sqm_hs)
       })      
       output$reg_announce4 <- renderText({ 
         paste("Flat Type: ",input$flat_type_hs)
       })       
       output$reg_announce5 <- renderText({ 
         paste("Flat Model: ",input$flat_model_hs)
       })       
       output$reg_announce5 <- renderText({ 
         paste("Confidence Interval: ",input$confidence_interval_hs, '%')
       })     
              
#p.s. not the most sexy code but just bear with it ba
       
         output$reg_announce_verbatim <- renderPrint({ 
         
        print(paste0("Intercept: ",round(summary(hf)$coefficients["(Intercept)",1],digits=0)))
         
        z<-round(tryCatch((summary(hf)$coefficients["(Intercept)",1]), error=function(e) 0))
           
        x1<- 'data_hs$town'
        y1<- paste0(x1,input$town_hs, collapse ='')
        z1<- round(tryCatch((summary(hf)$coefficients[y1,1]), error=function(e) 0))
        print(paste0("Town Factor: ", z1, digits=0))
        
        x2<- 'data_hs$remaining_lease'
        y2<- x2
        z2<-round(tryCatch((summary(hf)$coefficients[y2,1]), error=function(e) 0))
        print(paste0("Remaining Lease Factor: ",z2))
        
        x3<- 'data_hs$floor_area_sqm'
        y3<- x3
        z3<-round(tryCatch((summary(hf)$coefficients[y3,1]), error=function(e) 0))
        print(paste0("Remaining Floor Area: ",z3))
        
        x4<- 'data_hs$flat_type'
        y4<- paste0(x4,input$flat_type_hs, collapse ='')
        z4<-round(tryCatch((summary(hf)$coefficients[y4,1]), error=function(e) 0))
        print(paste0("Flat Type Factor: ",z4))        
        
        x5<- 'data_hs$flat_model'
        y5<- paste0(x5,input$flat_model_hs, collapse ='')
        z5<-round(tryCatch((summary(hf)$coefficients[y5,1]), error=function(e) 0))
        print(paste0("Flat Model Factor: ",z5))      

        estimate<- round(z + z1 + z2*as.numeric(input$remaining_lease_hs) + z3*as.numeric(input$floor_area_sqm_hs) + z4 + z5)
        print(paste0('Estimated Flat Price: ',estimate))
       
        m=summary(hf)
        
        print(paste0('Confidence Interval: (',estimate + round(m$sigma*qnorm(as.numeric(input$confidence_interval_hs)/100/2)),',',estimate - round(m$sigma*qnorm(as.numeric(input$confidence_interval_hs)/100/2)),')'    ))
        
       })
      
       output$reg_c<-renderPrint({

        print(paste0('r-squared: ', summary(hf)$r.squared))
        print(summary(hf))
        })
      
      output$reg_r<-renderPlot({
        
        ggplot(hf, aes(hf$residuals)) + geom_density() + geom_vline(aes(xintercept=mean(hf$residuals)),
                      color="blue", linetype="dashed", size=1)
        
      })
      
      output$reg_r_arp<-renderPlot({
        
        ggplot(hf, aes(x=data_hs$resale_price, y=hf$residuals)) + 
          geom_point(aes(color=hf$residuals))
        
      })
      
      output$reg_p_arp<-renderPlot({
        
        ggplot(hf, aes(x=data_hs$resale_price, y=hf$fitted.values)) + 
          geom_point(aes(color=hf$residuals))
        
      })
      
  ###########################################################
  
}

shinyApp(ui=ui, server=server)
