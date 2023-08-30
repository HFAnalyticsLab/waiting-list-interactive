## Elective care pledge Shiny app ##


# clear workspace
rm(list = ls()) 

# load libraries
library(janitor)
library(tidyverse)
library(shiny)

##### Load data #####

rtt_data <- readRDS("rtt_data.RDS")
seasonality <- readRDS("seasonality.RDS")

##### Calculations outside shiny ####

# point values of latest available data for plotting
latest_data <- ymd("2023-06-01")

latest_referrals <- rtt_data[rtt_data$month_year == latest_data,]$referrals_trend
latest_outflow <- rtt_data[rtt_data$month_year == latest_data,]$activity_trend
latest_waitlist <- rtt_data[rtt_data$month_year == latest_data,]$waiting_list
latest_referrals_actual <- rtt_data[rtt_data$month_year == latest_data,]$new_referrals
latest_outflow_actual <- rtt_data[rtt_data$month_year == latest_data,]$total_activity

# fixed assumptions
jr_dr_perc_consultant_led <- 0.80
jr_dr_daily_cancel <- 33100
consultant_daily_cancel <- 32800 

# function for monthly rate
monthlyRate <- function(x) {
  (1+(x/100))^(1/12)
}

##### User interface #####
ui <- fluidPage(
  titlePanel("Create your own waiting list scenarios"),
  
  # set the layout
  sidebarLayout(position = "left",
                
                # add toggles in the sidebar
                sidebarPanel(
                  
                  # add help text at top
                  helpText("Choose parameters"),
                  
                  # include seasonality
                  radioButtons("seasonality",
                               "Include seasonal variation in predictions?",
                               c("Yes - show seasonality" = "seasonal",
                                 "No - use straight lines" = "linear"
                                ),
                               inline = FALSE,
                               selected = "linear"
                    
                  ),
        
                  # number to choose referrals increases
                  numericInput("referrals_change", 
                               h3("Referrals percent change per year"), 
                               min = -100,
                               max = 100, 
                               value = 5
                              ),
                  
                  # number to choose outflow increases
                  numericInput("outflow_change", 
                               h3("Completed pathways percent change per year"), 
                               min = -100,
                               max = 100, 
                               value = 5
                              ),
                  
                  # help text on outflow
                  helpText("The amount of change in outflow is calculated using the percentage change
                           inputted above for completed pathways, plus an additional percentage accounting 
                           for unreported removals, fixed at 13% of the new outflow rate"),
                  
                  # number of junior doctor strike days to include
                  numericInput("jr_drs", 
                               h3("Number of junior doctor strike days to include"), 
                               min = 0,
                               max = 57, 
                               value = 6 
                             ),
                  
                  # number of consultant strike days to include
                  numericInput("consultant", 
                               h3("Number of consultant strike days to include"), 
                               min = 0,
                               max = 38, 
                               value = 4 
                              ),
                 
                   # strike intensity
                  numericInput("intensity", 
                               h3("Strike intensity %"), 
                               min = 0,
                               max = 100, 
                               value = 85
                              ),
                  
                  # help text on strikes
                  helpText("Strike days will be incorporated in the first few months, 
                           with 3 days per month (for junior doctors) or 2 days per month
                           (for consultants) until the number of inputted strike days is reached")
                  
                ),
                
                # add graph and title in main panel
                mainPanel(
                  
                  # title
                  h1("New referrals and completed pathways", align = "center"),
                  
                  # plot referrals and outflow
                  plotOutput("referrals_plot"),
                  
                  # plot waiting list
                  plotOutput("waiting_list_plot")
                  
                )
    
  )
)

##### Server logic #####

# below are some input values for testing outside app -- should comment out otherwise
# input <- data.frame(seasonality = "seasonal", jr_drs = 7, consultant = 5, referrals_change = 5, outflow_change = 5, intensity = 85)

server <- function(input, output) {
  
  #### Make predictions ####

  # Make a reactive dataframe of months to calculate for (up to Jan 25)
  predictions <- reactive(
    {
    data.frame(month_year = prediction_time <- seq(latest_data, ymd("2025-01-01"), by = "months") #  get all the dates for the 20 months
      , month_no = seq(0, 19) # index for multiplying monthly rate -- start at 0 because not including june
      )  %>%
      
      # calculate predictions for referrals, outflow, and waiting list (not including seasonality or strikes)
      # here we get the predicted values using the final value of the linear trend (rather than actual value) 
      mutate(referrals_pred = latest_referrals * monthlyRate(input$referrals_change)^month_no # get referrals increases
               , outflow_pred = latest_outflow * monthlyRate(input$outflow_change)^month_no # here we assume all outflow increases by this (i.e. missing data important)
              ) %>% 
        
      # include effect of seasonality
      mutate(month = month(month_year)) %>% 
      left_join(seasonality, by = "month") %>% 
      mutate(referrals_pred_seasonal = if_else(month_no == 0, latest_referrals_actual, referrals_pred * referrals_seasonality)
             , outflow_pred_seasonal = if_else(month_no == 0, latest_outflow_actual, outflow_pred * activity_seasonality)) %>% 
        
      # include effect of strikes
        # if month index is less than the round-up input value of strike days (divided by ), don't add strike days
        # if it is equal to number of round-up input, assign the remainder of days. otherwise give a 3.
      mutate(jr_dr_strike_days = case_when(ceiling(input$jr_drs/3) > month_no & month_no > 0 ~ 3
                                           , ceiling(input$jr_drs/3) == month_no ~ input$jr_drs %% 3
                                           , TRUE ~ 0)
             , consultant_strike_days =  case_when(ceiling(input$consultant/2) > month_no & month_no > 0 ~ 2
                                                   , ceiling(input$consultant/2) == month_no ~ input$consultant %% 2
                                                   , TRUE ~ 0)
             , jr_dr_cancellations = jr_dr_strike_days * jr_dr_perc_consultant_led * jr_dr_daily_cancel * (input$intensity/100)^(month_no-1)
             , consultant_cancellations = consultant_strike_days * consultant_daily_cancel * (input$intensity/100)^(month_no-1)
             ) %>%

      mutate(outflow_pred = outflow_pred - jr_dr_cancellations - consultant_cancellations
             , outflow_pred_seasonal = outflow_pred_seasonal - jr_dr_cancellations - consultant_cancellations) %>%
      
      # get new waiting list number
      # cumulative sum of referrals (up to t-1), - outflow (at t-1), and adding to june 23 waiting list
        
      mutate(waiting_list_pred = latest_waitlist + cumsum(lag(referrals_pred, default = 0)) - cumsum(lag(outflow_pred, default = 0))
             , waiting_list_pred_seasonal = latest_waitlist + cumsum(lag(referrals_pred_seasonal, default = 0)) - cumsum(lag(outflow_pred_seasonal, default = 0))) %>% 
        
      # join the original dataset
      full_join(rtt_data, by = "month_year") 
    }
    
  )
  

    
  #### Referrals and completed plot ####
  output$referrals_plot <- renderPlot(
    {
      if (input$seasonality == "linear") {
      # Plot referrals and completeds on same graph
        predictions() %>% 
          ggplot(aes(x = month_year)) +
          geom_line(aes(y = total_activity), color = "blue") +
          geom_line(aes(y = new_referrals), color = "red") +
          geom_line(aes(y = referrals_trend), color = "red") +
          geom_line(aes(y = activity_trend), color = "blue") +
          geom_line(aes(y = referrals_pred)) +
          geom_line(aes(y = outflow_pred)) +
          scale_x_date(limits = c(ymd("2016-04-01"), ymd("2025-02-01"))) 
      }
      
      else {
        # Plot referrals and completeds on same graph
        predictions() %>% 
          ggplot(aes(x = month_year)) +
          geom_line(aes(y = total_activity), color = "blue") +
          geom_line(aes(y = new_referrals), color = "red") +
          geom_line(aes(y = referrals_pred_seasonal)) +
          geom_line(aes(y = outflow_pred_seasonal)) +
          scale_x_date(limits = c(ymd("2016-04-01"), ymd("2025-02-01")))   
      }
        
    }
  )
  
  #### Waiting list plot ####
  
  # Predict waiting list

  output$waiting_list_plot <- renderPlot(
    {
      if (input$seasonality == "linear"){
        # Plot waiting list without seasonality
        predictions() %>% 
          ggplot(aes(x = month_year)) +
          geom_col(aes(y = waiting_list)) +
          geom_col(aes(y = waiting_list_pred), fill = "red") +
          scale_x_date(limits = c(ymd("2016-04-01"), ymd("2025-02-01")))
      }
      
      else {
        # Plot waiting list with seasonality
        predictions() %>% 
          ggplot(aes(x = month_year)) +
          geom_col(aes(y = waiting_list)) +
          geom_col(aes(y = waiting_list_pred_seasonal), fill = "red") +
          scale_x_date(limits = c(ymd("2016-04-01"), ymd("2025-02-01")))
      }
    }
  )
}



##### Run app #####

shinyApp(ui = ui, server = server)

