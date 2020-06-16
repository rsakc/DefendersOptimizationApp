#Loading Packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(broom)
library(gdata)
library(stats)
library(curl)


#Loading in Defenders Data
data <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 

#Keeping Data after August 2nd
data <- data %>% mutate(Date = str_sub(Date, 1, 10))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data <- data %>% filter(Date >= as.Date("08/02/2019", format = "%m/%d/%Y"))

#Making Location and Virus a factor
data$Location <- as.factor(data$Location)
data$Virus <- as.factor(data$Virus)


#To use for inputs
all_groups <- sort(unique(data$GroupID))
all_players <- sort(unique(data$PlayerID))
all_levels <- sort(unique(data$Level))
all_waves <- sort(unique(data$Wave))


#UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  #App Title
  titlePanel("Defenders Optimization"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  c("all", all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "playerID",
                  label = "Player ID:",
                  choices =  c("all", all_players),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput("levels", "Level:",
                  choices = all_levels,
                  multiple = FALSE,
                  selected = "1"),
      
      selectInput("waves", "Wave:",
                  choices = all_waves,
                  multiple = FALSE,
                  selected = "1"),
      
      selectInput("viruses", "Virus:",
                  choices = c("Red", "Blue", "Both"),
                  multiple = FALSE,
                  selected = "Both"),
      
      selectInput("plot", "Plot Type:",
                  choices = c("Main Effects Plot", "Interaction Plot"),
                  multiple = FALSE,
                  selected = "Main Effects Plot"),
      
      selectInput("yvar", "Y Variable:",
                  choices = c("Destroyed", "Shot", "PercentDestroyed"),
                  multiple = FALSE,
                  selected = "Destroyed"),
      
      uiOutput("xvar_out"),
      
      selectInput("table", "Table Type:",
                  choices = c("None", "ANOVA", "Counts"),
                  multiple = FALSE,
                  selected = "None"),
      
      selectInput("upgrades", "Allow Upgrades:",
                  choices = c("Yes", "No"),
                  multiple = FALSE,
                  selected = "No"),
      
      uiOutput("facets_out"),
      
      downloadButton('downloadData', label = "Defenders Data")
      
      
      
    ),
    
    #Outputs
    mainPanel(
      
      plotOutput(outputId = "Plot_out"),
      tableOutput("table_out"),
      verbatimTextOutput("ANOVA")

  )
  
  )
)


server <- function(input, output, session) {
  
  
  plotDataR <- reactive({
  
    ##Restricting Data
    
    #New Name
    data1 <- data
    
    #Filtering by GroupID and PlayerID
    if("all" %in% input$groupID){
      if("all" %in% input$playerID){
        data1 <- data1
      
      } else{
          data1 <- data1 %>% filter(PlayerID %in% input$playerID)
        }
      
    } else{
      if("all" %in% input$playerID){
        data1 <- data1 %>% filter(GroupID %in% input$groupID)
      
      } else{
          data1 <- data1 %>% filter(GroupID %in% input$groupID, PlayerID %in% input$playerID)
        }
    }
  
    
    #Require input
    req(input$upgrades)
    
    
    ###For No Upgrades
    if(input$upgrades == "No"){
    
    #Arranging data
    data1 <- data1 %>% arrange(GameNum)
    
    #Filering level and wave
    data1 <- data1 %>% filter(Level == input$levels, Wave == input$waves)
    
    #Removing Gamenum -1
    data1 <- data1 %>% filter(!(GameNum == -1))
    
    #1. Removing if there werent exactly 2 locations
    #2. Filtering out players who used any upgrades
    #3. Filtering out players who have 5 or more rows
    #4. Filtering out players if their two turrets only shot at one type of virus
    Index <- numeric()
    Index2 <- numeric()
    Index3 <- numeric()
    Index4 <- numeric()
    
    for(i in unique(data1$GameNum)){
      
      temp <- data1 %>% filter(GameNum == i)
      temp$Location <- drop.levels(temp$Location)
      temp$Virus <- drop.levels(temp$Virus)
      
      if(nlevels(temp$Location) != 2){
        Index <- append(Index, i)
      }
      
      if("1Fast" %in% temp$Upgrade | 
         "1Far" %in% temp$Upgrade){
        Index2 <- append(Index2, i)
      }
      
      if(nrow(temp) >= 5){
        Index3 <- append(Index3, i)
      }
      
      if(nlevels(temp$Virus) != 2){
        Index4 <- append(Index4, i)
      }
      
      
    }
    
    data1 <- data1 %>% filter(!(data1$GameNum %in% Index),
                              !(data1$GameNum %in% Index2),
                              !(data1$GameNum %in% Index3),
                              !(data1$GameNum %in% Index4))
    
    ##Making Restructered Data Frame
    #First Half
    
    #Setting up
    LocationCombination <- character()
    MedicineCombination <- character()
    Health <- numeric()
    Funds <- numeric()
    
    #Filling up vectors for the above 4 columns
    for(i in unique(data1$GameNum)){
      
      temp <- data1 %>% filter(GameNum == i)
      
      Funds <- append(Funds, temp$Funds[1])
      Health <- append(Health, temp$Health[1])
      
      #Location Combination
      loc <- sort(unique(temp$Location))
      LocationCombination <- append(LocationCombination, paste(loc[1], loc[2], sep = ""))
      
      #Medicine Combination
      temp1 <- temp %>% filter(Location == loc[1])
      temp2 <- temp %>% filter(Location == loc[2])
      
      med1st <- temp1$Medicine[1]
      med2nd <- temp2$Medicine[1]
      
      med1st <- str_sub(med1st, start = 5)
      med2nd <- str_sub(med2nd, start = 5)
      
      MedicineCombination <- append(MedicineCombination, paste(med1st, med2nd, sep = ""))
    }
    
    
    #Second Half
   
     #Fitering by player Virus choice
      if(input$viruses == "Red"){
      data1 <- data1 %>% filter(Virus == "red")
    
    } else if(input$viruses == "Blue"){
      data1 <- data1 %>% filter(Virus == "blue")
    }
    
    #Arrange data again
    data1 <- data1 %>% arrange(GameNum)
    

    #Setting Up
    Destroyed <- numeric()
    Shot <- numeric()
    
    #Filling up vectors 
    for(i in unique(data1$GameNum)){
      
      temp <- data1 %>% filter(GameNum == i)
      
      Destroyed <- append(Destroyed, sum(temp$Destroyed))
      Shot <- append(Shot, sum(temp$Shot))
    }
    
    
    #Restrucured Data 
    data_restructured <- data.frame(GameNum = sort(unique(data1$GameNum)),
                                    LocationCombination = LocationCombination,
                                    MedicineCombination = MedicineCombination,
                                    Health = Health, Funds = Funds,
                                    Shot = Shot, Destroyed = Destroyed)
    
    #Adding Missed and Percent Destroyed Column
    data_restructured <- data_restructured %>% mutate(Missed = Shot - Destroyed,
                                                      PercentDestroyed = round((Destroyed/Shot)*100, 2))
    
    
    
    ###Upgrades are Allowed
    } else {
      
      
      #Arranging data
      data1 <- data1 %>% arrange(GameNum)
      
      #Filering level and wave
      data1 <- data1 %>% filter(Level == input$levels, Wave == input$waves)
      
      #Removing Gamenum -1
      data1 <- data1 %>% filter(!(GameNum == -1))
      
      #1. Removing if there werent exactly 2 locations
      #3. Filtering out players who have 5 or more rows
      #4. Filtering out players if their two turrets only shot at one type of virus
      
      Index <- numeric()
      Index3 <- numeric()
      Index4 <- numeric()
      
      for(i in unique(data1$GameNum)){
        
        temp <- data1 %>% filter(GameNum == i)
        temp$Location <- drop.levels(temp$Location)
        temp$Virus <- drop.levels(temp$Virus)
        
        if(nlevels(temp$Location) != 2){
          Index <- append(Index, i)
        }
        
        if(nrow(temp) >= 5){
          Index3 <- append(Index3, i)
        }
        
        if(nlevels(temp$Virus) != 2){
          Index4 <- append(Index4, i)
        }
        
      }
      
      data1 <- data1 %>% filter(!(data1$GameNum %in% Index),
                                !(data1$GameNum %in% Index3),
                                !(data1$GameNum %in% Index4))

      
      ##Making Restructered Data Frame
      #First Half
      
      #Setting up
      LocationCombination <- character()
      MedicineCombination <- character()
      UpgradeCombination <- character()
      Health <- numeric()
      Funds <- numeric()
      
      #Filling up vectors for the above 4 columns
      for(i in unique(data1$GameNum)){
        
        temp <- data1 %>% filter(GameNum == i)
        
        Funds <- append(Funds, temp$Funds[1])
        Health <- append(Health, temp$Health[1])
        
        #Location Combination
        loc <- sort(unique(temp$Location))
        LocationCombination <- append(LocationCombination, paste(loc[1], loc[2], sep = ""))
        
        #Medicine Combination
        temp1 <- temp %>% filter(Location == loc[1])
        temp2 <- temp %>% filter(Location == loc[2])
        
        med1st <- temp1$Medicine[1]
        med2nd <- temp2$Medicine[1]
        
        med1st <- str_sub(med1st, start = 5)
        med2nd <- str_sub(med2nd, start = 5)
        
        MedicineCombination <- append(MedicineCombination, paste(med1st, med2nd, sep = ""))
        
        #Upgrade Combination
        up1st <- temp1$Upgrade[1]
        up2nd <- temp2$Upgrade[1]
        
        UpgradeCombination <- append(UpgradeCombination, paste(up1st, up2nd, sep = ""))
      
      }
      
      
      #Second Half
      
      #Fitering by player Virus choice
      if(input$viruses == "Red"){
        data1 <- data1 %>% filter(Virus == "red")
        
      } else if(input$viruses == "Blue"){
        data1 <- data1 %>% filter(Virus == "blue")
      }
      
      
      #Arrange data again
      data1 <- data1 %>% arrange(GameNum)
      
      
      #Setting Up
      Destroyed <- numeric()
      Shot <- numeric()
      
      #Filling up vectors 
      for(i in unique(data1$GameNum)){
        
        temp <- data1 %>% filter(GameNum == i)
        
        Destroyed <- append(Destroyed, sum(temp$Destroyed))
        Shot <- append(Shot, sum(temp$Shot))
      }
      
      
      #Restrucured Data 
      data_restructured <- data.frame(GameNum = sort(unique(data1$GameNum)),
                                      LocationCombination = LocationCombination,
                                      MedicineCombination = MedicineCombination,
                                      UpgradeCombination = UpgradeCombination,
                                      Health = Health, Funds = Funds,
                                      Shot = Shot, Destroyed = Destroyed)
      
      #Adding Missed and Percent Destroyed Column
      data_restructured <- data_restructured %>% mutate(Missed = Shot - Destroyed,
                                                        PercentDestroyed = round((Destroyed/Shot)*100, 2))
      
  
    }
    
    return(data_restructured)
    
  })
  
  #Dynamic PlayerID Input
  observe({
    
    #Require input
    req(input$groupID)
  
    
    if("all" %in% input$groupID){
      input_data <- data
    
    } else{
        input_data <- data %>% filter(GroupID %in% input$groupID)
        }
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(input_data$PlayerID))),
                      selected = "all")
  })
  
  
  #Dynamic Wave Input
  observe({
    
    #Require input
    req(input$levels)
    
    data_wave <- data %>% filter(Level == input$levels)
    wave_options <- sort(unique(data_wave$Wave))
    
    updateSelectInput(session, 
                      "waves",
                      choices = wave_options,
                      selected = wave_options[1])
  
  })
  
  
  #Dynamic X Variable Input
  
  output$xvar_out <- renderUI({
    
    if(input$plot == "Main Effects Plot"){
      
      selectInput("xvar", "X Variable:",
                  choices = c("LocationCombination", "MedicineCombination"),
                  multiple = FALSE,
                  selected = "LocationCombination")
    
    }
  })
  
  
  #Dynamic Facet Input
  output$facets_out <- renderUI({
    
    if(input$upgrades == "Yes"){
      
      checkboxInput("facets", "Facet by Upgrade Combination",
                    value = FALSE)
      
    }
  })
  
  #Creating Visualization
  output$Plot_out <- renderPlot({

    #Reactive Data
    plot_data <- plotDataR()
    
    #Y Variable
    yvar <- sym(input$yvar)
    
    #For Subtitle and Main Effects Color
    if(input$viruses == "Both"){
      subt <- "Data: Both Viruses"
      col <- "purple2"
    
    } else if(input$viruses == "Red"){
      subt <- "Data: Red Virus"
      col <- "red1"
    
    } else if(input$viruses == "Blue"){
      subt <- "Data: Blue Virus"
      col <- "blue1"
    }
    
    #No Upgrades
    if(input$upgrades == "No"){

      #Main Effects Plot
      if(input$plot == "Main Effects Plot"){
        
        #Require X Variable
        req(input$xvar)

        main_data <- plot_data %>% group_by_at(input$xvar) %>%
          summarize(Mean = mean(!!yvar))


        my_plot <- ggplot(data = main_data, aes_string(x = input$xvar, y = "Mean", group = 1)) +
          geom_point(color = col) +
          geom_path(color = col) +
          labs(title = paste("Main Effects Plot:", input$xvar, sep = " "),
               x = input$xvar, y = input$yvar,
               subtitle = subt) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 18), 
                axis.text.y = element_text(size = 14))
        
              

        
      #Interaction Plot  
      } else{ 
      
      int_data <- plot_data %>% group_by(LocationCombination, MedicineCombination) %>% 
                                           summarize(Mean = mean(!!yvar))
      
     my_plot <-  ggplot(data = int_data, aes(x = LocationCombination, y = Mean, color = MedicineCombination, group = MedicineCombination)) +
        geom_point() +
        geom_line() +
        labs(title = "Interaction Plot",
             x = "Location Combination", y = input$yvar,
             subtitle = subt) +
        guides(col=guide_legend("Medicine Combination")) +
        scale_color_manual(values = c("blue1", "purple1", "violetred", "red1")) +
       theme_bw() +
       theme(axis.text.x = element_text(size = 18, hjust = 1), 
             axis.title = element_text(size = 20), 
             plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
             plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
             legend.text = element_text(size = 16), 
             legend.title = element_text(size = 18),
             axis.text.y = element_text(size = 14))
       
      
     }
    
      
    #With Upgrades
    } else{
      
      #No Facet Option  
      if(input$facets == FALSE){ 
      
        #Main Effects Plot
        if(input$plot == "Main Effects Plot"){
          
        #Require X Variable
          req(input$xvar)
      
      main_data_u <- plot_data %>% group_by_at(input$xvar) %>% 
       summarize(Mean = mean(!!yvar))
      
     my_plot <- ggplot(data = main_data_u, aes_string(x = input$xvar, y = "Mean", group = 1)) +
        geom_point(color = col) +
        geom_path(color = col) +
        labs(title = paste("Main Effects Plot:", input$xvar, sep = " "),
             x = input$xvar, y = input$yvar,
             subtitle = subt) +
       theme_bw() +
       theme(axis.text.x = element_text(size = 18, hjust = 1), 
             axis.title = element_text(size = 20), 
             plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
             plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
             legend.text = element_text(size = 16), 
             legend.title = element_text(size = 18),
             axis.text.y = element_text(size = 14))
     
       
    #Interaction Plot 
    } else{
      
    int_data_u <- plot_data %>% group_by(LocationCombination, MedicineCombination) %>% 
        summarize(Mean = mean(!!yvar))
    
    my_plot <- ggplot(data = int_data_u, aes(x = LocationCombination, y = Mean, color = MedicineCombination, group = MedicineCombination)) +
      geom_point() +
      geom_line() +
      labs(title = "Interaction Plot",
           x = "Location Combination", y = input$yvar,
           subtitle = subt) +
      guides(col=guide_legend("Medicine Combination")) +
      scale_color_manual(values = c("blue1", "purple1", "violetred", "red1")) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 18, hjust = 1), 
            axis.title = element_text(size = 20), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
            legend.text = element_text(size = 16), 
            legend.title = element_text(size = 18),
            axis.text.y = element_text(size = 14))
     
    }
        
      #With Facet Option  
      } else{
        
      
        
        #Main Effects Plot
        if(input$plot == "Main Effects Plot"){
          
          #Require X Variable
          req(input$xvar)
          
          #X Variable
          xvar <- sym(input$xvar)
          
          main_data_uf <- plot_data %>% group_by(!!xvar, UpgradeCombination) %>% 
            summarize(Mean = mean(!!yvar))
        
          my_plot <- ggplot(data = main_data_uf, aes_string(x = input$xvar, y = "Mean", group = 1)) +
            geom_point(color = col) +
            geom_path(color = col) +
            labs(title = paste("Main Effects Plot:", input$xvar, sep = " "),
                 x = input$xvar, y = input$yvar,
                 subtitle = subt) +
            theme_bw() +
            theme(axis.text.x = element_text(size = 18, hjust = 1), 
                  axis.title = element_text(size = 20), 
                  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
                  legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 18),
                  axis.text.y = element_text(size = 14),
                  strip.text = element_text(size = 16)) +
            facet_wrap(. ~ UpgradeCombination)
          
         
        #Interaction Plot   
        } else{
          
          int_data_uf<- plot_data %>% group_by(LocationCombination, MedicineCombination, UpgradeCombination) %>% 
            summarize(Mean = mean(!!yvar))
          
          my_plot <- ggplot(data = int_data_uf, aes(x = LocationCombination, y = Mean, color = MedicineCombination, group = MedicineCombination)) +
            geom_point() +
            geom_line() +
            labs(title = "Interaction Plot",
                 x = "Location Combination", y = input$yvar,
                 subtitle = subt) +
            guides(col=guide_legend("Medicine Combination")) +
            scale_color_manual(values = c("blue1", "purple1", "violetred", "red1")) +
            theme_bw() +
            theme(axis.text.x = element_text(size = 18, hjust = 1), 
                  axis.title = element_text(size = 20), 
                  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
                  legend.text = element_text(size = 16), 
                  legend.title = element_text(size = 18),
                  axis.text.y = element_text(size = 14),
                  strip.text = element_text(size = 16)) +
            facet_wrap(. ~ UpgradeCombination) 
        }
      }
    }
    
    return(my_plot)
    
  })
  

  #Table Output
  output$table_out <- renderTable({

    #Reactive Data
    plot_data <- plotDataR()
    
    #If Table input is Count
    if(input$table == "Counts"){

      my_table <- plot_data %>% count(LocationCombination, MedicineCombination)
    
    #If Table input is None
    } else if(input$table == "None"){
        
      my_table <- data.frame()
    }
  })
    
    
 
  #ANOVA Output
  output$ANOVA <- renderPrint({
    
    if(input$table == "ANOVA"){
    
    #Reactive Data
    plot_data <- plotDataR()
    
    #Setting Up
    XVariable <- plot_data %>% pull(input$xvar)
    XVariable <- drop.levels(XVariable)
    LocationVariable <- plot_data %>% pull(LocationCombination)
    LocationVariable <- drop.levels(LocationVariable)
    MedicineVariable <- plot_data %>% pull(MedicineCombination)
    MedicineVariable <- drop.levels(MedicineVariable)
    YVariable <- plot_data %>% pull(input$yvar)
    
   
    #No Upgrades
    if(input$upgrades == "No"){

    #One Way ANOVA
    if(input$plot == "Main Effects Plot"){

      if(nlevels(XVariable) > 1){
      anova_model <- aov(YVariable ~ XVariable)
      
      #Making Tidy table and adding columns/rows
      tidyanova = tidy(anova_model)
      sum_df = sum(tidyanova$df)
      sum_ss = sum(tidyanova$'sumsq')
      tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
      tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
      tidyanova$meansq = round(tidyanova$meansq, digits = 2)
      tidyanova$statistic = round(tidyanova$statistic, digits = 2)
      
      return(tidyanova)

      } else{
        
        "More than one level needed to run the ANOVA."
      }

    #Two Way ANOVA
    } else {

      if(nlevels(LocationVariable) > 1 & nlevels(MedicineVariable) > 1){
      anova_model <- aov(YVariable ~ (LocationVariable + MedicineVariable)^2)
      
      #Making Tidy table and adding columns/rows
      tidyanova = tidy(anova_model)
      sum_df = sum(tidyanova$df)
      sum_ss = sum(tidyanova$'sumsq')
      tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
      tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
      tidyanova$meansq = round(tidyanova$meansq, digits = 2)
      tidyanova$statistic = round(tidyanova$statistic, digits = 2)
      
      return(tidyanova)

      } else{
        "More than one level needed to run the ANOVA."
      }
    }

    #With Upgrades
    } else {

      #Setting Up
      UpgradeVariable <- plot_data %>% pull(UpgradeCombination)
      UpgradeVariable <- drop.levels(UpgradeVariable)

      if(input$facets == TRUE){

        #Two Way ANOVA
        if(input$plot == "Main Effects Plot"){

          if(nlevels(XVariable) > 1 & nlevels(UpgradeVariable) > 1){
            anova_model <- aov(YVariable ~ (XVariable + UpgradeVariable)^2)
            
            
            #Making Tidy table and adding columns/rows
            tidyanova = tidy(anova_model)
            sum_df = sum(tidyanova$df)
            sum_ss = sum(tidyanova$'sumsq')
            tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
            tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
            tidyanova$meansq = round(tidyanova$meansq, digits = 2)
            tidyanova$statistic = round(tidyanova$statistic, digits = 2)
            
            return(tidyanova)

             } else{
              "More than one level needed to run the ANOVA."
              }

        #Three Way ANOVA
        } else {

          if(nlevels(LocationVariable) > 1 & nlevels(MedicineVariable) > 1 &
             nlevels(UpgradeVariable) > 1){

            anova_model <- aov(YVariable ~ (LocationVariable +
                                              MedicineVariable + UpgradeVariable)^2)
            
            
            #Making Tidy table and adding columns/rows
            tidyanova = tidy(anova_model)
            sum_df = sum(tidyanova$df)
            sum_ss = sum(tidyanova$'sumsq')
            tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
            tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
            tidyanova$meansq = round(tidyanova$meansq, digits = 2)
            tidyanova$statistic = round(tidyanova$statistic, digits = 2)
            
            return(tidyanova)

            
          } else{
            "More than one level needed to run the ANOVA."
          }
        }
      
      } else if(input$facets == FALSE){
  
         "The facet checkbox must be selected to run the ANOVA."
        
        }

    }

}
  })

  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
      
    })
  
  
  
  
}



#Creating Shiny App
shinyApp(ui = ui, server = server)





