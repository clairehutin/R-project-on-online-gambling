# Import required Packages
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(readr)) install.packages('readr')
library(readr)

if (!require(leaflet)) install.packages('leaflet')
library(leaflet)

if (!require(sas7bdat)) install.packages('sas7bdat')
library(shiny)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

if (!require(extrafont)) install.packages('extrafont')
library(extrafont)

if (!require(DT)) install.packages('DT')
library(DT)

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

#Import Helvetica font for Graph headers
#font_import()
#loadfonts(device = "win")

# Import Datamart
#path = "C:/Users/pborchert/Documents/GitHub/R_Group_Assignment/"
#datamart <- read_csv(paste0(path, "datamart.csv"))

datamart <- read.csv("datamart.csv")

# Import Info Table for Analysis
#informative_table <- read_csv(paste0(path, "informative_table.csv"))
informative_table <- read.csv("informative_table.csv")

# Define Shiny App UI Structure
ui <- fluidPage(

    # Application title
    titlePanel("Datamart Descriptive Analysis"),
    
    tabsetPanel(
        tabPanel("Map", fluid=TRUE,
                 
                 # 1st Tab
                 # World Map: Counts UserID's per Country and Clusters them visually
                 
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "Continent",
                                label = "Choose Continent:",
                                choices = c("Worldwide",as.character(unique(datamart$Continent_Name[order(datamart$Continent_Name)])))
                                )),
        
    
                mainPanel (h3("Worldwide Nr. of Customers"),
                    leafletOutput("map", "100%", 450)
                )
            )
        ),
    tabPanel("Customer", fluid=TRUE,
             
             # 2nd Tab
             # Displays Customer Age Histogram (filter for Gender)
             
             sidebarLayout(
                 sidebarPanel(
                     h4("Select Customer:"),
                     sliderInput(
                         inputId = "Age",
                         label   = "Select Age range:",
                         min     = min(datamart$Age[!is.na(datamart$Age)]),
                         max     = max(datamart$Age[!is.na(datamart$Age)]),
                         value   = c(min(datamart$Age[!is.na(datamart$Age)]),max(datamart$Age[!is.na(datamart$Age)]))
                     ),
                     radioButtons(inputId = "Gender", 
                                  label = "Gender:", 
                                  choices = c("All","Female", "Male"), 
                                  selected = "All"),
                     hr(),
                     h4("Define Active Customer:"),
                     sliderInput(
                         inputId = "ActiveDays",
                         label   = "Total Days Active:",
                         min     = min(datamart$TotalDaysActive, na.rm = TRUE),
                         max     = max(datamart$TotalDaysActive, na.rm = TRUE),
                         value   = 50
                     ),
                     sliderInput(
                         inputId = "ActiveMonths",
                         label   = "Active in the last X months:",
                         min     = 1,
                         max     = 12,
                         value   = 3
                     ),
                     submitButton(text="Submit")
                 ),
                 mainPanel(
                     h3("Customer Age and Gender"),
                     plotOutput("age_plot"),
                     plotOutput("status_plot")
                 )
             )
        ),
    tabPanel("Route to BWIN", fluid=TRUE,
             
             # 3rd Tab
             # Customer Route to the BWIN Website (where do the customers come from?)
             
             sidebarLayout(
                 sidebarPanel(
                     sliderInput(
                         inputId = "User",
                         label   = "Select Nr. User Range:",
                         min     = 0,
                         max     = 25000,
                         value   = c(0.01,25000)
                     ), 
                     submitButton(text="Submit")
                ),
                 mainPanel (h3("Route of Access to BWIN (in Tsd.)"),
                            plotOutput("route_plot")
                 )
             )
    ),
    tabPanel("Products", fluid = TRUE,
             
             # 4th Tab
             # Placeholder
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "Country",
                                 label = "Choose Country:",
                                 choices = c("Worldwide",as.character(unique(datamart$country_name[order(datamart$country_name)]))),
                                 selected = "Worldwide"
                     ),
                     radioButtons(inputId = "Variable1", 
                                  label = "Display:", 
                                  choices = c("Total amount (Mio Euro)","Avg winnings (Mio Euro)"), 
                                  selected = "Total amount (Mio Euro)"),
                     submitButton(text="Submit")
                ),
                mainPanel(
                    plotOutput("product_plot")
                )
            )
        ),
    tabPanel("Total Bets / Buy Conversions", fluid = TRUE,
             
             # 5th Tab
             # Analyze Customer Origin (Continent & Country) for relevant variables (Total Bets, Total Buy Conversions)
             
             sidebarLayout(
                 sidebarPanel(
                     radioButtons(inputId = "Variable", 
                                  label = "Variable to Analyze:", 
                                  choices = c("Total Bets","Total Buy Conversions"), 
                                  selected = "Total Bets"),
                     selectInput(inputId = "Continent1",
                                 label = "Choose Continent:",
                                 choices = c("Worldwide",as.character(unique(datamart$Continent_Name[order(datamart$Continent_Name)]))),
                                 selected = "Europe"
                                 ),
                     submitButton(text="Submit")
                     ),
                 mainPanel(
                     plotOutput("total_bets"),
                     plotOutput("total_bets1")
                 )
             )
        ),
    tabPanel("Datamart", fluid=TRUE,
             
             sidebarLayout(sidebarPanel(width=0),
                 mainPanel(h3("Datamart"),
                            DTOutput(outputId = "table")
                 )
             )
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$map <- renderLeaflet({
        
        # 1st Tab
        # Leaflet Map Output
        
        if (input$Continent == "Worldwide"){
            df <- datamart[!is.na(datamart$latitude),]
        } else {
            df <- datamart[(datamart$Continent_Name == input$Continent) & !is.na(datamart$latitude),]
        }
        leaflet()  %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            #addCircles(data = df,
             #          radius=1000,
              #         color="red") %>%
            addMarkers(df$longitude, df$latitude,
                       clusterOptions = markerClusterOptions(), 
                       label = df$nr_conversions,
                       labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
            setView(lat= 30, lng=10, zoom = 2)
    })
    output$age_plot <- renderPlot({
        
        # 2rd Tab
        # Plot histogram based on age and gender input
        
        if (input$Gender == "All"){
            x    <- datamart[!is.na(datamart$Age),]
        } else if (input$Gender == "Female"){
            x    <- datamart[!is.na(datamart$Age) & datamart$Gender == 0,]
        } else if (input$Gender == "Male"){
            x    <- datamart[!is.na(datamart$Age) & datamart$Gender == 1,]
            
        }
        
        x <- x[(x$Age >= input$Age[1]) & (x$Age <= input$Age[2]),]
        
        ggplot(x, aes(x=Age)) +
            geom_histogram(alpha=0.8, bins=10, fill="#0f9fff") + 
            theme(panel.background = element_rect(fill="white"))+
            xlim(0, max(datamart$Age[!is.na(datamart$Age)]))
    })
    
    output$status_plot <- renderPlot({
        
        if (input$Gender == "All"){
            x    <- datamart[!is.na(datamart$Age),]
        } else if (input$Gender == "Female"){
            x    <- datamart[!is.na(datamart$Age) & datamart$Gender == 0,]
        } else if (input$Gender == "Male"){
            x    <- datamart[!is.na(datamart$Age) & datamart$Gender == 1,]
        }
        
        x <- x[(x$Age >= input$Age[1]) & (x$Age <= input$Age[2]),]
        
        x <- x[is.na(x$TotalDaysActive) == FALSE & is.na(x$LastActiveDate) == FALSE, ]
        
        x$dummy <- "dummy"
        
        input_months <- ifelse((9 - input$ActiveMonths) < 1, 1, 9 - input$ActiveMonths)
        input_days <- input$ActiveDays
        # Active for more than 100 days (added FO and LA) & Active within the last 3 months (9 (September) - 3 months = 6)
        x <- x %>%
            mutate(status = ifelse(TotalDaysActive > input_days & month(LastActiveDate) >= input_months, "active", "inactive"))
        
        ggplot(x, aes(x=dummy, y=UserID, fill=status)) +
            geom_bar(stat="identity") +
            scale_fill_manual(values=c("#0f9fff", "grey"))+
            theme(panel.background = element_rect(fill="white"),
                  axis.text.y      = element_blank(),
                  axis.ticks.y     = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.ticks.x     = element_blank(),
                  plot.title       = element_text(family="Helvetica" ,size = 25))+
            xlab(NULL)+
            ylab(NULL)+
            coord_flip()+
            ggtitle("Customer Status (Nr. of Customers)") +
            annotate("text",x=1,y=-Inf,hjust=-17,vjust=0,label=paste(sum(x$status == "inactive")))+
            annotate("text",x=1,y=Inf,hjust=10,vjust=0,label=sum(x$status == "active"))
        
    })
    
    output$route_plot <- renderPlot({
        
        # 3rd Tab
        
        input_var1 <- input$User[1]/1000
        input_var2 <- input$User[2]/1000
        
        x<- datamart[!is.na(datamart$application_description), ]%>% 
            group_by(application_description) %>% 
            summarise(avg_bets=sum(mean_money_bets, na.rm = TRUE),nb_users=n()/1000)%>%
            arrange(desc(nb_users))
        
        x <- x[x$nb_users >= input_var1 & x$nb_users <= input_var2, ]
        
        ggplot(x, aes(x=application_description, y=nb_users, label=round(nb_users,1), size=x$avg_bets/1000)) +
            geom_segment( aes(x=reorder(application_description, nb_users), xend=application_description, y=nb_users, yend=nb_users), color="skyblue") +
            geom_bar(stat = "identity",fill = "black", width = 0.08, size=0.5, alpha=0.6)+
            geom_point(color="#0f9fff", alpha=0.6, size = ifelse(x$avg_bets/50000 < 1, 1.5,x$avg_bets/50000)) +
            theme_light() +
            xlab(NULL) +
            coord_flip() +
            labs(size='AVG Bets (in Tsd.)')+
            theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position="right",
                legend.key = element_rect(fill = NA, colour = "White")
            ) 
    })
    
    output$product_plot <- renderPlot({
        
        # 4th Tab
        
        input_var <- input$Country
        
        if (input_var == "Worldwide"){
            x <- informative_table%>% 
                #filter(country_name == input_var) %>%
                group_by(product_description)%>%
                summarise( sum_bets         = round(sum(total_number_bets)/1000000,2), 
                           total_money_bets = round(sum(total_money_bets)/1000000,2),
                           avg_winnings     = round(sum(avg_winnings)/1000000,2))
        } else {
            x <- informative_table%>% 
                filter(country_name == input_var) %>%
                group_by(product_description)%>%
                summarise( sum_bets         = round(sum(total_number_bets)/1000000,2), 
                           total_money_bets = round(sum(total_money_bets)/1000000,2),
                           avg_winnings     = round(sum(avg_winnings)/1000000,2))
        }
        
        if (input$Variable1 == "Total amount (Mio Euro)"){
            
            ggplot(x, aes(x=reorder(product_description,-sum_bets), y=sum_bets, label=sum_bets, alpha = sum_bets)) + 
                geom_bar(stat = "identity",fill = "#0f9fff")+
                geom_text(aes(label=paste("Nr Bets:\n",sum_bets,"\n\n", "Total amount:\n",total_money_bets)),
                          size=3, 
                          position=position_stack(vjust=0.5),
                          y = ifelse(x$sum_bets < 1, 1, x$sum_bets/2),
                          color = "black",
                          alpha=1)+
                theme(axis.text.x=element_text(angle=90, hjust=1), 
                      panel.background = element_rect(fill="white"),
                      plot.title = element_text(family="Helvetica" ,size = 25))+
                scale_alpha(range = c(0.2, 1), guide=FALSE)+
                ggtitle(paste0("Nr. Bets and ",input$Variable1, " per Product")) 
            
            
        } else if (input$Variable1 == "Avg winnings (Mio Euro)"){
            
            #replace sum_bets with avg_winnings
            
            ggplot(x, aes(x=reorder(product_description,-avg_winnings), y=avg_winnings, label=avg_winnings, alpha = avg_winnings)) + 
                geom_bar(stat = "identity",fill = "#0f9fff")+
                geom_text(aes(label=paste("Nr Bets:\n",avg_winnings,"\n\n", "Avg winnings:\n",avg_winnings)),
                          size=3, 
                          position=position_stack(vjust=0.5),
                          y = ifelse(x$avg_winnings < 1, 1, x$avg_winnings/2),
                          color = "black",
                          alpha=1)+
                theme(axis.text.x=element_text(angle=90, hjust=1), 
                      panel.background = element_rect(fill="white"),
                      plot.title = element_text(family="Helvetica" ,size = 25))+
                scale_alpha(range = c(0.2, 1), guide=FALSE)+
                ggtitle(paste0("Nr. Bets and ",input$Variable1, " per Product")) 
        }
        
    })
    
    output$total_bets <- renderPlot({
        
        # 5th Tab (Top)
        # Analysis Continent
        
        if (input$Variable == "Total Bets"){
            
            x <- datamart[!is.na(datamart$total_money_bets),] %>% 
                select(country_name, Continent_Name, total_money_bets) %>%
                group_by(Continent_Name) %>%
                summarise(total_money_bets = sum(total_money_bets)/1000000)
            
            # Order countries based on total_money_bets (descending)
            x <- x[order(-x$total_money_bets),]
            
            input_var <- input$Continent1
            cols <- x[order(x$total_money_bets),] %>% mutate(color = "grey50")
            cols$color[cols$Continent_Name == input_var] <- "#0f9fff"
            cols$color <- cols$color[order(cols$total_money_bets)]
            
            ggplot(x, aes(fill=reorder(Continent_Name, total_money_bets), x=reorder(Continent_Name, total_money_bets), y=total_money_bets)) +
                geom_bar(stat="identity") +
                geom_text(aes(label= round(total_money_bets, 2)), 
                          position = position_stack(vjust = 0.5), 
                          size=4, 
                          y = ifelse(x$total_money_bets < 10, 10, x$total_money_bets/2),
                          color="black") +
                scale_fill_manual(values=cols$color)+
                xlab(NULL)+
                theme(legend.position="none", panel.background = element_rect(fill="white"),
                      axis.text.y = element_text(color=cols$color),
                      plot.title = element_text(family="Helvetica" ,size = 25))+
                geom_segment(aes(x = 0, y = 0, xend = 0, yend = 125))+
                ggtitle(paste0(input$Variable, " in Mio. Euro")) +
                #geom_segment(aes(x = 0, y = 0, xend = 7, yend = 0))+
                coord_flip()
            
        } else if (input$Variable == "Total Buy Conversions"){
            
            x <- datamart[!is.na(datamart$total_buy_amount),] %>% 
                select(country_name, Continent_Name, total_buy_amount) %>%
                group_by(Continent_Name) %>%
                summarise(total_buy_amount = sum(total_buy_amount)/1000)
            
            # Order countries based on total_buy_amount (descending)
            x <- x[order(-x$total_buy_amount),]
            
            input_var <- input$Continent1
            cols <- x[order(x$total_buy_amount),] %>% mutate(color = "grey50")
            cols$color[cols$Continent_Name == input_var] <- "#0f9fff"
            cols$color <- cols$color[order(cols$total_buy_amount)]
            
            ggplot(x, aes(fill=reorder(Continent_Name, total_buy_amount), x=reorder(Continent_Name, total_buy_amount), y=total_buy_amount)) +
                geom_bar(stat="identity") +
                geom_text(aes(label= round(total_buy_amount, 2)), 
                          position = position_stack(vjust = 0.5), 
                          size=4, 
                          y = ifelse(x$total_buy_amount < 10, 10, x$total_buy_amount/2),
                          color="black") +
                scale_fill_manual(values=cols$color)+
                xlab(NULL)+
                ylim(0, max(x$total_buy_amount))+
                theme(legend.position="none", panel.background = element_rect(fill="white"),
                      axis.text.y = element_text(color=cols$color),
                      plot.title = element_text(family="Helvetica" ,size = 25))+
                geom_segment(aes(x = 0, y = 0, xend = 0, yend = 125))+
                ggtitle(paste0(input$Variable, " in Tsd. Euro")) +
                #geom_segment(aes(x = 0, y = 0, xend = 7, yend = 0))+
                coord_flip()
            
        } 
        
        
    })
    output$total_bets1 <- renderPlot({
        
        # 5th Tab (Bottom)
        # Country Analysis
        
        input_var <- input$Continent1
        
        if (input$Variable == "Total Bets"){
        
            x1 <- datamart[!is.na(datamart$total_money_bets),] %>% 
                select(country_name, Continent_Name, total_money_bets) %>%
                filter(Continent_Name == input_var) %>%
                group_by(country_name) %>%
                summarise(total_money_bets = sum(total_money_bets)/1000000)
            
            # Order countries based on total_money_bets (descending)
            x1 <- x1[order(-x1$total_money_bets),]
            
            maxrow <- ifelse(nrow(x1) < 5, nrow(x1), 5 )
            
            ggplot(x1[1:maxrow,], aes(x=reorder(country_name, -total_money_bets), y=total_money_bets, alpha = total_money_bets)) +
                geom_bar(stat="identity", fill = "#0f9fff") +
                scale_alpha(range = c(0.2, 1), guide=FALSE) +
                geom_text(aes(label= round(total_money_bets, 2)),
                          alpha=1,
                          position = position_stack(vjust = 0.5), 
                          size=4, 
                          #y = ifelse(x$total_money_bets < 1, 1, x$total_money_bets/2),
                          color="black") +
                xlab(NULL)+
                ylim(NA, max(x1$total_money_bets))+
                theme(legend.position="none", panel.background = element_rect(fill="white"),
                      plot.title = element_text(family="Helvetica" ,size = 25))+
                geom_segment(aes(x = 0, y = 0, xend = 0, yend = max(x1$total_money_bets))) +
                ggtitle(paste0(input$Variable, " in ", input_var, " (Top 5 Countrys)")) #+
            #coord_flip()
            
        } else if (input$Variable == "Total Buy Conversions"){
            
            x1 <- datamart[!is.na(datamart$total_buy_amount),] %>% 
                select(country_name, Continent_Name, total_buy_amount) %>%
                filter(Continent_Name == input_var) %>%
                group_by(country_name) %>%
                summarise(total_buy_amount = sum(total_buy_amount)/1000)
            
            # Order countries based on total_buy_amount (descending)
            x1 <- x1[order(-x1$total_buy_amount),]
            
            maxrow <- ifelse(nrow(x1) < 5, nrow(x1), 5 )
            
            ggplot(x1[1:maxrow,], aes(x=reorder(country_name, -total_buy_amount), y=total_buy_amount, alpha = total_buy_amount)) +
                geom_bar(stat="identity", fill = "#0f9fff") +
                scale_alpha(range = c(0.2, 1), guide=FALSE) +
                geom_text(aes(label= round(total_buy_amount, 2)),
                          alpha= 1,
                          position = position_stack(vjust = 0.5),
                          #y = ifelse(x$total_buy_amount < 1, 1, x$total_buy_amount/2),
                          size=4,
                          color="black") +
                xlab(NULL)+
                ylim(NA, max(x1$total_buy_amount))+
                theme(legend.position="none", panel.background = element_rect(fill="white"),
                      plot.title = element_text(family="Helvetica" ,size = 25))+
                geom_segment(aes(x = 0, y = 0, xend = 0, yend = max(x1$total_buy_amount))) +
                ggtitle(paste0(input$Variable, " in ", input_var, " (Top 5 Countrys) in Tsd.")) #+
            #coord_flip()
        }
    })
    output$table <- renderDT(datamart)
}

# Run the application 
shinyApp(ui = ui, server = server)
