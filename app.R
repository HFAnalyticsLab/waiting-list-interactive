## Elective care pledge Shiny app ##

# load packages
library(janitor)
library(plotly)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(shiny)
library(showtext)

##### Load data #####

rtt_data <- readRDS("data/rtt_data.RDS")
seasonality <- readRDS("data/seasonality.RDS")
workdays_table <- readRDS("data/workdays_table.RDS")

##### Calculations outside shiny ####

####### point values of latest available data for plotting ######
latest_data <- ymd("2023-07-01")

latest_workdays <- rtt_data[rtt_data$month_year == latest_data,]$workdays
latest_referrals <- rtt_data[rtt_data$month_year == latest_data,]$referrals_trend 
latest_outflow <- rtt_data[rtt_data$month_year == latest_data,]$activity_trend
latest_waitlist <- rtt_data[rtt_data$month_year == latest_data,]$waiting_list
latest_referrals_actual <- rtt_data[rtt_data$month_year == latest_data,]$new_referrals
latest_outflow_actual <- rtt_data[rtt_data$month_year == latest_data,]$total_activity
waiting_list_at_pledge <- rtt_data[rtt_data$month_year == "2023-01-01",]$waiting_list

######## time dataframe ######

time_df <- data.frame(month_year = prediction_time <- seq(latest_data, ymd("2025-01-01"), by = "months") #  get all the dates for the 20 months
                      , month_no = seq(0, interval(latest_data, ymd("2025-01-01")) %/% months(1)) # index for multiplying monthly rate -- start at 0 because not including latest data
                      )

time_df <- time_df %>% 
  left_join(workdays_table, by = "month_year") %>% 
  mutate(month = month(month_year)) %>% 
  left_join(seasonality, by = "month")

###### fixed assumptions ######
jr_dr_perc_consultant_led <- 0.73
jr_dr_daily_cancel <- 22200
consultant_daily_cancel <- 22900 
perc_result_completed_pathway <- 0.2
jr_dr_strike_days_per_month <- 3
consultant_strike_days_per_month <- 2

# for junior doctors, not all appointments are consultant led so need to account for this and add this to procedures, which are all consultant led
jr_dr_start_val <- jr_dr_daily_cancel * perc_result_completed_pathway * jr_dr_strike_days_per_month
consultant_start_val <- consultant_daily_cancel * perc_result_completed_pathway * consultant_strike_days_per_month

# include actual cancellations in july and august to predicted
jr_dr_aug23_actual_cancellations_appt <- 53437
jr_dr_aug23_actual_cancellations_proc <- 7763

jr_dr_aug23 <- ((jr_dr_aug23_actual_cancellations_appt * jr_dr_perc_consultant_led) + jr_dr_aug23_actual_cancellations_proc) * perc_result_completed_pathway

consultant_aug23_actual_cancellations <- 45827
consultant_aug23 <- consultant_aug23_actual_cancellations * perc_result_completed_pathway


###### choices dataframe #####

choice_df <- data.frame("referrals_change" = c(5, 5, 5),
                        "outflow_change" = c(7.8, 7.8, 16.9),
                        "jr_drs" = c(17, 2, 2),
                        "consultant" = c(17, 2, 2),
                        "intensity" = c(90, 90, 90))


####### function for monthly rate  #######
monthlyRate <- function(x) {
  (1+(x/100))^(1/12)
}


###### colours and settings #####

linesize <- .8
trendlinesize <- linesize*.75
thf_blue <- "#53a9cd"
thf_lightblue <- "#A9D4E6"
thf_red <- "#dd0031"
thf_pink <- "#EE8098"
thf_purple <- "#744284"
thf_teal <- "#2a7979"
thf_lightpurple <- "#BAA1C2"
thf_annotations <- "#CCC9C8"

colors <- c("New referrals" = thf_blue
            , "Total outflow" = thf_red
            , "Projected referrals" = thf_lightblue
            , "Projected outflow" = thf_pink
            , "Referrals linear trend" = thf_lightblue
            , "Outflow linear trend" = thf_pink
            , "Waiting list" = thf_purple
            , "Projected waiting list" = thf_lightpurple)

## font:

textsize <- 12

#font_paths("www/")
#font_add(family = "LTUnivers 330 BasicLight", regular = "LTUnivers 330 BasicLight.ttf")

##### User interface #####
ui <- fluidPage(
  
      includeCSS("www/CSS.css"),
      
      tags$head(tags$style(HTML('* {font-family: "LTUnivers 330 BasicLight"};'))),
  
      title = ("Waiting list interactive calculator"),

                  # add help text at top
                  h3("Waiting list interactive calculator"),
                  
                  fluidRow(column(12, h5("Use the interactive calculator to test out our example scenarios, 
                              or try your own to understand how the waiting list will change 
                              between now and December 2024 as a result of changing referrals, 
                              completed pathways and industrial action"))),
                  
                  fluidRow(
                    column(4,
                           h4("Choose from an example scenario"),
                           
                           uiOutput("preset_server")),
                    
                    column(4, 
                           h4("Change the percent by which new referrals and completed pathways increase or decrease"),
                           # number to choose referrals increases
                           numericInput("referrals_change", 
                                        "Referrals % change per year", 
                                        min = -20,
                                        max = 20, 
                                        value = 5
                                       ),
                           # number to choose outflow increases
                           numericInput("outflow_change", 
                                        "Completed pathways % change per year", 
                                        min = -20,
                                        max = 20, 
                                        value = 7.8
                                        )
                            ),
                    column(4, 
                           h4("Change the number of strikes to add into the model"),
                           # number of junior doctor strike days to include
                           numericInput("jr_drs", 
                                        "Number of months of junior doctor strikes to include", 
                                        min = 0,
                                        max = 17, 
                                        value = 17 
                                        ),
                           # number of consultant strike days to include
                           numericInput("consultant", 
                                        "Number of months of consultant strikes to include", 
                                        min = 0,
                                        max = 17, 
                                        value = 17 
                                        ),
                           # strike intensity
                           numericInput("intensity", 
                                        "Strike intensity %", 
                                        min = 0,
                                        max = 100, 
                                        value = 90
                                        ),
                           # help text on strikes
                           helpText("One strike will be incorporated every month from the first month until the number of inputted strike months is reached. 
                                    Strike intensity informs the proportion of cancellations from the previous month seen in the current month "),
                           
                           )
                        ),

                  
                  hr(),
                  
                  # plot referrals and outflow
                  plotly::plotlyOutput("referrals_plot"),
                  
                  # plot waiting list
                  plotly::plotlyOutput("waiting_list_plot"),
  
                  hr(),
  
                  downloadButton("download_data", "Download data")

)

##### Server logic #####

# below are some input values for testing outside app -- should comment out otherwise
# input <- data.frame(seasonality = "seasonal", jr_drs = 7, consultant = 5, referrals_change = 5, outflow_change = 5, intensity = 85)

server <- function(input, output, session) {
  
  output$preset_server <- renderUI({
    
    strikes_cont <- "Scenario 1: Current growth rates, and strikes continue every month into January 2025"
    no_strikes <- "Scenario 2: Current growth rates, with no further strike action after October 2023"
    reach_130 <- "Scenario 3: Goal of reaching 30% more activity by 24/25 is reached, with no further strike action after October 2023" 
    
    choice_values <- 1:3
    
    names(choice_values) <- c(strikes_cont, no_strikes, reach_130)
    
    radioButtons("preset", "Choose an example scenario", choices = choice_values)
  })
  
  observe({
    
    purrr::map(names(choice_df), function(x){
      
      updateNumericInput(session, x, value = choice_df[input$preset, x])
      
    })
    
  })
  
  #### Make predictions ####
  
  # Make a reactive dataframe of months to calculate for (up to Jan 25)
  predictions <- reactive(
    {
      time_df %>%
        mutate(referrals_pred_seasonal = if_else(month_no == 0
                                               , latest_referrals_actual
                                               , (latest_referrals/latest_workdays) * monthlyRate(input$referrals_change)^month_no * workdays * referrals_seasonality)      
             , outflow_pred_seasonal = if_else(month_no == 0
                                               , latest_outflow_actual
                                               , latest_outflow/latest_workdays * monthlyRate(input$outflow_change)^month_no * workdays * activity_seasonality)
        ) %>% 

        
        # include effect of strikes
        # if month index is less than the round-up input value of strike days (divided by ), don't add strike days
        # if it is equal to number of round-up input, assign the remainder of days. otherwise give a 3.
        mutate(jr_dr_cancellations = case_when(input$jr_drs > month_no & month_no > 2 ~ jr_dr_start_val * (input$intensity/100)^(month_no - 1)
                                               , month_no == 1 ~ jr_dr_aug23
                                               , TRUE ~ 0)
               , consultant_cancellations =  case_when(input$consultant > month_no & month_no > 2 ~ consultant_start_val * (input$intensity/100)^(month_no - 1)
                                                       , month_no == 1 ~ consultant_aug23                                                   
                                                       , TRUE ~ 0)
        ) %>%
        
        mutate(outflow_pred_seasonal = outflow_pred_seasonal - jr_dr_cancellations - consultant_cancellations) %>%
        
        mutate(outflow_pred = predict(lm(outflow_pred_seasonal ~ month_no, data = data.frame(month_no = seq(0, interval(latest_data, ymd("2025-01-01")) %/% months(1)))
                                         )
                                      )
               ) %>% 
        
        mutate(referrals_pred = predict(lm(referrals_pred_seasonal ~ month_no, data.frame(month_no = seq(0, interval(latest_data, ymd("2025-01-01")) %/% months(1)))
                                          )
                                        )
        ) %>% 
        
        # get new waiting list number
        # cumulative sum of referrals (up to t-1), - outflow (at t-1), and adding to latest waiting list
        
        mutate(waiting_list_pred_seasonal = latest_waitlist + cumsum(lag(referrals_pred_seasonal, default = 0)) - cumsum(lag(outflow_pred_seasonal, default = 0))) %>% 
        
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
        geom_line(aes(y = total_activity, color = "Total outflow",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Total outflow:", format(round(as.numeric(total_activity), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize) +
        geom_line(aes(y = new_referrals, color = "New referrals", 
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>New referrals:", format(round(as.numeric(new_referrals), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize) +
        scale_x_date(date_breaks = "1 year"
                     , date_minor_breaks = "3 months"
                     , limits = c(ymd("2016-04-01"), ymd("2025-01-01"))
                     , date_labels = "%b-%y") +
        scale_y_continuous(label = comma) +
        theme_minimal() +
        geom_line(aes(y = referrals_trend, color = "Referrals linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Referrals linear trend:", format(round(as.numeric(referrals_trend), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = activity_trend, color = "Outflow linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Outflow linear trend:", format(round(as.numeric(activity_trend), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = referrals_pred, color = "Referrals linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Referrals linear trend:", format(round(as.numeric(referrals_pred), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = outflow_pred, color = "Outflow linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Outflow linear trend:", format(round(as.numeric(outflow_pred), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = referrals_pred_seasonal, color = "Projected referrals",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Projected referrals:", format(round(as.numeric(referrals_pred_seasonal), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize, alpha = 0.8) +
        geom_line(aes(y = outflow_pred_seasonal, color = "Projected outflow",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Projected outflow:", format(round(as.numeric(outflow_pred_seasonal), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize, alpha = 0.8) +
        xlab("") +
        ylab("Number of pathways") +
        # ggtitle("New referrals and completed pathways") +
        scale_color_manual(values = colors) +
        labs(color = "") +
        theme(text = element_text(size = textsize, family = "LTUnivers 330 BasicLight"), legend.position = "top") +
        annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2021-04-01"), ymin = 0, ymax = Inf, fill = thf_annotations, alpha = 0.2) +
        annotate("rect", xmin = ymd("2024-12-01"), xmax = ymd("2025-01-01"), ymin = 0, ymax = Inf, fill = thf_annotations, alpha = 0.2)

      final_plot <- ggplotly(to_plot, tooltip = "text") %>%  #Need to add tooltip argument so only text that is manually created above is displayed, not also the default 
        add_annotations(
            text = "COVID-19",
            x = as.numeric(ymd("2020-02-01")),
            y = 250000,
            showarrow = FALSE,
            xref = "x",
            yref = "y",
            textangle = 270,
            font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight")
           ) %>%
         add_annotations(
              text = "Deadline for next\n general election",
            x = as.numeric(ymd("2024-11-15")),
            y = 500000,
            showarrow = FALSE,
            xref = "x",
            yref = "y",
            textangle = 270,
            font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight")
            )
      
      final_plot[['x']][['layout']][['shapes']] <- c()
      
      final_plot <- layout(final_plot,
                           
                           shapes = list(
                             list(type = "rect",
                                  
                                  fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                                  
                                  x0 = as.numeric(ymd("2020-03-01")), x1 = as.numeric(ymd("2021-04-01")), xref = "x",
                                  
                                  y0 = 0, y1 = 1, yref = "paper"),
                             
                             list(type = "rect",
                                  
                                  fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                                  
                                  x0 = as.numeric(ymd("2024-12-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                                  
                                  y0 = 0, y1 = 1, yref = "paper")),
                           
                           list(x = as.numeric(ymd("2024-11-15")), y = 700000, text = "Deadline for next\n general election"),
                           
                           xaxis = list(tickangle = 315),
                           
                           legend = list(x = ymd("2018-01-01"), y = 2500000, orientation = 'h')
                           
                           )
      
      
      final_plot
    }
  )
  
  #### Waiting list plot ####
  
  # Predict waiting list
  
  output$waiting_list_plot <- plotly::renderPlotly({
    
    to_plot <- predictions() %>%
      ggplot(aes(x = month_year)) +
      geom_col(aes(y = waiting_list_pred_seasonal, fill = "Projected waiting list",
                   text = paste(
                     format(month_year, "%B %Y"), 
                     "<br>Projected waiting list:", format(round(as.numeric(waiting_list_pred_seasonal), 1), nsmall=1, big.mark=",")))) + # plot this first so latest date doesn't get overwritten
      geom_col(aes(y = waiting_list, fill = "Waiting list",
                   text = paste(
                     format(month_year, "%B %Y"), 
                     "<br>Waiting list:", format(round(as.numeric(waiting_list), 1), nsmall=1, big.mark=","))))  +
      scale_x_date(date_breaks = "1 year"
                   , date_minor_breaks = "3 months"
                   , limits = c(ymd("2016-04-01"), ymd("2025-01-01"))
                   , date_labels = "%b-%y") +
      scale_y_continuous(label = comma) +
      theme_minimal() +
      xlab("") +
      ylab("Waiting list size") +
      # ggtitle("Waiting list") +
      scale_fill_manual(values = colors) +
      labs(fill = "") +
      theme(text = element_text(size = textsize, family = "LTUnivers 330 BasicLight"), legend.position = "top") +
      annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2021-04-01"), ymin = 0, ymax = Inf, fill = thf_annotations, alpha = 0.2) +
      annotate("rect", xmin = ymd("2024-12-01"), xmax = ymd("2025-01-01"), ymin = 0, ymax = Inf, fill = thf_annotations, alpha = 0.2) +
      geom_segment(aes(x = ymd("2023-01-01"), xend = ymd("2025-01-01"), y = waiting_list_at_pledge, yend = waiting_list_at_pledge), linetype = 3, color = "white", alpha = 0.8) 

    
    final_plot <- ggplotly(to_plot, tooltip = "text") %>% 
      add_annotations(
        text = "COVID-19",
        x = as.numeric(ymd("2020-02-01")),
        y = 2000000,
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        textangle = 270,
        font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight")
      ) %>%
      add_annotations(
        text = "Deadline for next\n general election",
        x = as.numeric(ymd("2024-11-15")),
        y = 2000000,
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        textangle = 270,
        font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight")
      ) %>%
      add_annotations(
        text = "Waiting list at pledge, ~7.2M",
        x = as.numeric(ymd("2023-01-01")),
        y = waiting_list_at_pledge,
        showarrow = TRUE,
        xref = "x",
        yref = "y",
        ax = 0,
        ay = -40,
        arrowsize = 0.5,
        arrowcolor = "#676361",
        textangle = 0,
        font = list(color = "#676361", size = textsize-1)
      )
    
    final_plot[['x']][['layout']][['shapes']] <- c()
    
    final_plot <- layout(final_plot,
                         
                         shapes = list(
                           list(type = "rect",
                                
                                fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                                
                                x0 = as.numeric(ymd("2020-03-01")), x1 = as.numeric(ymd("2021-04-01")), xref = "x",
                                
                                y0 = 0, y1 = 1, yref = "paper"),
                           
                           list(type = "rect",
                                
                                fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                                
                                x0 = as.numeric(ymd("2024-12-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                                
                                y0 = 0, y1 = 1, yref = "paper")),
                         
                         xaxis = list(tickangle = 315),
                         
                         legend = list(x = ymd("2018-01-01"), y = 2500000, orientation = 'h')
                         
                         )
    
    final_plot
    
  })
  
  output$download_data <- downloadHandler(
    
    filename = "data.csv",
    content = function(file) {
      write.csv(predictions(), file, row.names = F)
    }
  )
}

##### Run app #####

shinyApp(ui = ui, server = server)

# profvis::profvis(runApp(shinyApp(ui, server)))

