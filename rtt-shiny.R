## Elective care pledge Shiny app ##

# load packages
library(janitor)
library(plotly)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

##### Load data #####

rtt_data <- readRDS("data/rtt_data.RDS")
seasonality <- readRDS("data/seasonality.RDS")

##### Calculations outside shiny ####

# point values of latest available data for plotting
latest_data <- ymd("2023-06-30")

latest_referrals <- rtt_data[rtt_data$month_year == latest_data,]$referrals_trend
latest_outflow <- rtt_data[rtt_data$month_year == latest_data,]$activity_trend
latest_waitlist <- rtt_data[rtt_data$month_year == latest_data,]$waiting_list
latest_referrals_actual <- rtt_data[rtt_data$month_year == latest_data,]$new_referrals
latest_outflow_actual <- rtt_data[rtt_data$month_year == latest_data,]$total_activity
waiting_list_at_pledge <- rtt_data[rtt_data$month_year == "2023-01-31",]$waiting_list

# time dataframe

time_df <- data.frame(month_year = prediction_time <- seq(latest_data, ymd("2025-01-31"), by = "months") #  get all the dates for the 20 months
                      , month_no = seq(0, 19) # index for multiplying monthly rate -- start at 0 because not including june
)

# fixed assumptions
jr_dr_perc_consultant_led <- 0.73
jr_dr_daily_cancel <- 29100
consultant_daily_cancel <- 22900 
perc_result_completed_pathway <- 0.23
jr_dr_strike_days_per_month <- 3
consultant_strike_days_per_month <- 2

jr_dr_start_val <- jr_dr_daily_cancel * jr_dr_perc_consultant_led * perc_result_completed_pathway * jr_dr_strike_days_per_month
consultant_start_val <- consultant_daily_cancel * perc_result_completed_pathway * consultant_strike_days_per_month

# include actual cancellations in july and august to predicted
jr_dr_jul23_actual_cancellations <- 102565
jr_dr_aug23_actual_cancellations <- 61200
jr_dr_jul23 <- jr_dr_jul23_actual_cancellations * jr_dr_perc_consultant_led * perc_result_completed_pathway
jr_dr_aug23 <- jr_dr_aug23_actual_cancellations * jr_dr_perc_consultant_led * perc_result_completed_pathway

consultant_jul23_actual_cancellations <- 65557
consultant_aug23_actual_cancellations <- 45827
consultant_jul23 <- consultant_jul23_actual_cancellations * perc_result_completed_pathway
consultant_aug23 <- consultant_aug23_actual_cancellations * perc_result_completed_pathway




# function for monthly rate
monthlyRate <- function(x) {
  (1+(x/100))^(1/12)
}

# colours and settings

linesize <- 1.2
thf_blue <- "#53a9cd"
thf_lightblue <- "#7ebfda"
thf_red <- "#dd0031"
thf_pink <- "#ee7174"
thf_purple <- "#744284"
thf_teal <- "#2a7979"

colors <- c("New referrals" = thf_red
            , "Total outflow" = thf_blue
            , "Predicted referrals" = thf_pink
            , "Predicted outflow" = thf_lightblue
            , "Waiting list" = thf_purple
            , "Predicted waiting list" = thf_teal)

##### User interface #####
ui <- fluidPage(
  titlePanel("Waiting list interactive calculator"),
  
  # set the layout
  sidebarLayout(position = "left",
                
                # add toggles in the sidebar
                sidebarPanel(
                  
                  # add help text at top
                  h4("Choose parameters"),
                  
                  # include seasonality
                  radioButtons("seasonality",
                               "Include seasonal variation in predictions?",
                               c("Yes - show seasonality" = "seasonal",
                                 "No - use straight lines" = "linear"
                               ),
                               inline = FALSE,
                               selected = "linear"
                               
                  ),
                  
                  
                  fluidRow(
                    column(6, 
                           
                           # number to choose referrals increases
                           numericInput("referrals_change", 
                                        "Referrals % change per year", 
                                        min = -100,
                                        max = 100, 
                                        value = 5
                           )),
                    column(6,
                           numericInput("outflow_change", 
                                        "Completed pathways % change per year", 
                                        min = -100,
                                        max = 100, 
                                        value = 5
                           )
                    )),
                  
                  # number to choose outflow increases
                  # help text on outflow
                  helpText("The amount of change in outflow is calculated using the percentage change
                           inputted above for completed pathways, plus an additional percentage accounting 
                           for unreported removals, fixed at 13% of the new outflow rate"),
                  
                  fluidRow(
                    column(6, 
                           # number of junior doctor strike days to include
                           numericInput("jr_drs", 
                                        "Number of months of junior doctor strikes to include", 
                                        min = 0,
                                        max = 18, 
                                        value = 2 
                           )),
                    column(6, 
                           # number of consultant strike days to include
                           numericInput("consultant", 
                                        "Number of of months of consultant strikes to include", 
                                        min = 0,
                                        max = 18, 
                                        value = 2 
                           ))
                  ),
                  
                  fluidRow(
                    column(6, 
                           # strike intensity
                           numericInput("intensity", 
                                        "Strike intensity %", 
                                        min = 0,
                                        max = 100, 
                                        value = 85
                           )),
                    column(6)
                  ),
                  
                  # help text on strikes
                  helpText("Strike days will be incorporated in the first few months, 
                           with 3 days per month (for junior doctors) or 2 days per month
                           (for consultants) until the number of inputted strike days is reached"),
                  
                  hr(),
                  
                  downloadButton("download_data", "Download data"),
                  
                  width = 3
                  
                ),
                
                # add graph and title in main panel
                mainPanel(
                  
                  # plot referrals and outflow
                  plotly::plotlyOutput("referrals_plot"),
                  
                  # plot waiting list
                  plotly::plotlyOutput("waiting_list_plot")
                  
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
      time_df %>%
        
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
        mutate(jr_dr_cancellations = case_when(input$jr_drs > month_no & month_no > 2 ~ jr_dr_start_val * (input$intensity/100)^(month_no - 2)
                                               , month_no == 1 ~ jr_dr_jul23
                                               , month_no == 2 ~ jr_dr_aug23
                                               , TRUE ~ 0)
               , consultant_cancellations =  case_when(input$consultant > month_no & month_no > 2 ~ consultant_start_val * (input$intensity/100)^(month_no - 2)
                                                       , month_no == 1 ~ consultant_jul23
                                                       , month_no == 2 ~ consultant_aug23                                                       
                                                       , TRUE ~ 0)
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
  # To do: can the if/else be implemented into one ggplot code so easier to update with colours etc.?
  
  output$referrals_plot <- plotly::renderPlotly(
    { 
      # Plot referrals and completeds on same graph
      to_plot <- predictions() %>% 
        ggplot(aes(x = month_year)) +
        geom_line(aes(y = total_activity, color = "Total outflow"), size = linesize) +
        geom_line(aes(y = new_referrals, color = "New referrals"), size = linesize) +
        scale_x_date(date_breaks = "1 year"
                     , date_minor_breaks = "3 months"
                     , limits = c(ymd("2016-04-01"), ymd("2025-01-01"))
                     , date_labels = "%Y") +
        scale_y_continuous(label = comma) +
        theme_minimal() +
        xlab("Months") +
        ylab("Number of pathways") +
        ggtitle("New referrals and completed pathways") +
        scale_color_manual(values = colors) +
        labs(color = "") +
        theme(text = element_text(size = 18)) +
        annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2021-04-01"), ymin = 0, ymax = Inf, fill = "grey", alpha = 0.2) +
        annotate("rect", xmin = ymd("2024-12-01"), xmax = ymd("2025-01-01"), ymin = 0, ymax = Inf, fill = "grey", alpha = 0.2) +
        annotate("text", x = ymd("2020-02-15"), y = 250000, label = "COVID-19") +
        annotate("text", x = ymd("2024-11-15"), y = 700000, label = "Deadline for next\n general election")
      
      if (input$seasonality == "linear") {
        
        to_plot <- to_plot + 
          geom_line(aes(y = referrals_trend, color = "Predicted referrals"), size = linesize) +
          geom_line(aes(y = activity_trend, color = "Predicted outflow"), size = linesize) +
          geom_line(aes(y = referrals_pred, color = "Predicted referrals"), linetype = 2, size = linesize) +
          geom_line(aes(y = outflow_pred, color = "Predicted outflow"), linetype = 2, size = linesize)
        
      }
      
      else {
        
        # Plot referrals and completeds on same graph
        
        to_plot <- to_plot + 
          geom_line(aes(y = referrals_pred_seasonal, color = "Predicted referrals"), 
                    linetype = 2, size = linesize) +
          geom_line(aes(y = outflow_pred_seasonal, color = "Predicted outflow"), 
                    linetype = 2, size = linesize)
      }
      
      final_plot <- ggplotly(to_plot)
      
      final_plot[['x']][['layout']][['shapes']] <- c()
      
      final_plot <- layout(final_plot,
                           
                           shapes = list(
                             list(type = "rect",
                                  
                                  fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                                  
                                  x0 = as.numeric(ymd("2020-03-01")), x1 = as.numeric(ymd("2021-04-01")), xref = "x",
                                  
                                  y0 = 0, y1 = 1, yref = "paper"),
                             
                             list(type = "rect",
                                  
                                  fillcolor = "grey", line = list(color = "grey"), opacity = 0.2,
                                  
                                  x0 = as.numeric(ymd("2024-12-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                                  
                                  y0 = 0, y1 = 1, yref = "paper")))
      
      
      final_plot
    }
  )
  
  #### Waiting list plot ####
  
  # Predict waiting list
  
  output$waiting_list_plot <- plotly::renderPlotly({
    
    to_plot <- predictions() %>%
      ggplot(aes(x = month_year)) +
      geom_col(aes(y = waiting_list, fill = "Waiting list")) +
      scale_x_date(date_breaks = "1 year"
                   , date_minor_breaks = "3 months"
                   , limits = c(ymd("2016-04-01"), ymd("2025-01-01"))
                   , date_labels = "%Y") +
      scale_y_continuous(label = comma) +
      theme_minimal() +
      xlab("Months") +
      ylab("Waiting list size") +
      ggtitle("Waiting list") +
      scale_fill_manual(values = colors) +
      labs(fill = "") +
      theme(text = element_text(size = 18)) +
      annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2021-04-01"), ymin = 0, ymax = Inf, fill = "grey", alpha = 0.2) +
      annotate("rect", xmin = ymd("2024-12-01"), xmax = ymd("2025-01-01"), ymin = 0, ymax = Inf, fill = "grey", alpha = 0.2) +
      geom_segment(aes(x = ymd("2023-01-01"), xend = ymd("2025-01-01"), y = waiting_list_at_pledge, yend = waiting_list_at_pledge), linetype = 2, color = "white", alpha = 0.8)
    
    
    if (input$seasonality == "linear"){
      
      to_plot <- to_plot + 
        geom_col(aes(y = waiting_list_pred, fill = "Predicted waiting list"))
    } else {
      
      to_plot <- to_plot + 
        geom_col(aes(y = waiting_list_pred_seasonal, fill = "Predicted waiting list"))
    }
    
    final_plot <- ggplotly(to_plot)
    
    final_plot[['x']][['layout']][['shapes']] <- c()
    
    final_plot <- layout(final_plot,
                         
                         shapes = list(
                           list(type = "rect",
                                
                                fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                                
                                x0 = as.numeric(ymd("2020-03-01")), x1 = as.numeric(ymd("2021-04-01")), xref = "x",
                                
                                y0 = 0, y1 = 1, yref = "paper"),
                           
                           list(type = "rect",
                                
                                fillcolor = "grey", line = list(color = "grey"), opacity = 0.2,
                                
                                x0 = as.numeric(ymd("2024-12-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                                
                                y0 = 0, y1 = 1, yref = "paper")))
    
    final_plot
  })
  
  output$download_data <- downloadHandler(
    
    filename = "data.csv",
    content = function(file) {
      write.csv(predictions() %>%
                  select(-month, -month_no), file, row.names = F)
    }
  )
}

##### Run app #####

shinyApp(ui = ui, server = server)

# profvis::profvis(runApp(shinyApp(ui, server)))

