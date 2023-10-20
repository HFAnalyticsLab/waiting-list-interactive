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
latest_data <- ymd("2023-08-01")

latest_workdays <- rtt_data[rtt_data$month_year == latest_data,]$workdays
latest_referrals <- rtt_data[rtt_data$month_year == latest_data,]$referrals_trend 
latest_completed <- rtt_data[rtt_data$month_year == latest_data,]$activity_trend
latest_waitlist <- rtt_data[rtt_data$month_year == latest_data,]$waiting_list
latest_referrals_actual <- rtt_data[rtt_data$month_year == latest_data,]$new_referrals
latest_completed_actual <- rtt_data[rtt_data$month_year == latest_data,]$total_activity
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
jr_dr_daily_cancel <- 26100
joint_daily_cancel <- 39300 
perc_result_completed_pathway <- 0.2
jr_dr_strike_days_per_month <- 2
joint_strike_days_per_month <- 3

# create start values for how many pathways would not be complete as a result of a strike
jr_dr_start_val <- jr_dr_daily_cancel * perc_result_completed_pathway * jr_dr_strike_days_per_month
joint_start_val <- joint_daily_cancel * perc_result_completed_pathway * joint_strike_days_per_month

# include actual cancellation data in september and october 
jr_dr_sep23_actual_cancellations <- 52281
jr_dr_oct23_actual_cancellations <- 0

jr_dr_sep23 <- jr_dr_sep23_actual_cancellations * perc_result_completed_pathway

joint_sep23_actual_cancellations <- 77632
joint_sep23 <- joint_sep23_actual_cancellations * perc_result_completed_pathway

joint_oct23_actual_cancellations <- 118026
joint_oct23 <- joint_oct23_actual_cancellations * perc_result_completed_pathway


###### choices dataframe #####

choice_df <- data.frame("referrals_change" = c(5, 5, 5, 5),
                        "completed_change" = c(7.8, 7.8, 5.2, 10.4),
                        "jr_drs" = c(0, 0, 0, 0),
                        "joint" = c(0, 17, 17, 0),
                        "intensity" = c(95, 95, 95, 95))


####### function for monthly rate  #######
monthlyRate <- function(x) {
  (1+(x/100))^(1/12)
}


###### colours and settings #####

linesize <- .4
trendlinesize <- linesize
thf_blue <- "#53a9cd"
thf_lightblue <- "#A9D4E6"
thf_red <- "#dd0031"
thf_pink <- "#EE8098"
thf_purple <- "#744284"
thf_teal <- "#2a7979"
thf_lightpurple <- "#BAA1C2"
thf_annotations <- "#C8C3BE"
referrals_colour <- "#7EBFDA"
referrals_trend_colour <- "#005078"
completed_colour <- "#EE9B90"
completed_trend_colour <- "#dd0031"
axis_colour <- "#999390"
grid_colour <- "#E2DFD8"
projection_annotations <- "#EE9B90"


colors <- c("New referrals" = referrals_colour
            , "Referrals linear trend" = referrals_trend_colour
            # , "Projected referrals" = referrals_colour
            , "Completed pathways" = completed_colour
            , "Completed pathways linear trend" = completed_trend_colour
            # , "Projected completed pathways" = completed_colour
            , "Waiting list" = thf_purple
            , "Projected waiting list" = thf_lightpurple)

## font:

textsize <- 12


##### User interface #####
ui <- fluidPage(
  
      includeCSS("www/CSS.css"),
      
      tags$head(tags$style(
        HTML('* {font-family: "LTUnivers 330 BasicLight"}
             ')
      )
            , includeHTML("www/google-tags.html")
                ),
  
      title = ("Waiting list interactive calculator"),

                  # add help text at top
                  h2("Waiting list interactive calculator", style = "font-family: 'Linotype Univers 530 Medium';"),
                  
                  fluidRow(column(12, h5("Use the interactive calculator to test out our example scenarios, 
                              or try your own to understand how the waiting list could change 
                              between now and January 2025 as a result of changing referrals, 
                              completed pathways and industrial action."))),
                  
                  fluidRow(
                    column(4,
                           h4("Choose from an example scenario"),
                           
                           uiOutput("preset_server")),
                    
                    column(8, 
                           h4("Change the percent by which new referrals and completed pathways increase or decrease"),
                           # number to choose referrals increases
                           div(style = "display: inline-block",
                               numericInput("referrals_change", 
                                             "Referrals % change per year", 
                                              min = -20,
                                              max = 20, 
                                              value = 5,
                                            width = "260px"
                                            )),
                           # number to choose completed pathways increases
                           div(style = "display: inline-block",
                               numericInput("completed_change", 
                                            "Completed pathways % change per year", 
                                            min = -20,
                                            max = 20, 
                                            value = 7.8,
                                            width = "260px"
                                            )),
                             
                           
                           ## Added an actionLink that gives a popup of the helpText
                           
                           h4(span(style = "display: inline",
                                   "Change the number of strikes to add into the model",
                              tags$sup(actionLink("helpText",
                                      style = "display: inline; font-size: 10px",
                                      label = NULL, 
                                      icon = icon(name = "question",
                                                  lib = "font-awesome"))))),
                           
                           # number of junior doctor strike days to include
                           # number of joint strike days to include
                           div(style = "display: inline-block",
                               numericInput("joint", 
                                        "Number of months of joint consultant and junior doctor strikes to include", 
                                        min = 0,
                                        max = 17, 
                                        value = 17,
                                        width = "220px"
                           )),
                           div(style = "display: inline-block",
                               numericInput("jr_drs", 
                                        "Number of months of additional junior doctor strikes to include", 
                                        min = 0,
                                        max = 17, 
                                        value = 17,
                                        width = "220px"
                                        )),
                           # strike intensity
                           div(style = "display: inline-block",
                               numericInput("intensity", 
                                        "Strike intensity %", 
                                        min = 0,
                                        max = 100, 
                                        value = 95,
                                        width = "220px"
                                        )),
                           
                           ## Removed the helpText as it is now in a popup
                           ## NOTE: Commented out in case it needs to be reinstated
                           
                           # help text on strikes
                           # helpText("One strike will be incorporated every month from the first month until the number of inputted strike months is reached. 
                           #          Strike intensity is the proportion of cancellations from the previous month seen in the current month."),
                           # 
                           )
                        ),

                  
                  hr(),
                  
                  ## Changed the order of plots
      
                  # plot waiting list
                  plotly::plotlyOutput("waiting_list_plot"),
                  
                  # plot referrals and completed
                  plotly::plotlyOutput("referrals_plot"),
                  
                  downloadButton("download_data", "Download data"),
  
                  hr(),
  
                  fluidRow(
                    column(4, img(src= "THF-copyright.png", align = "left", height = "50%", width = "50%")),
                    column(8, HTML("Source: <a href=https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/>NHS England Consultant-led Referral to Treatment Waiting Times Data</a>. <br> Data to August 2023, per 12 October 2023 release."), align = "right")
                    )
                           

)

##### Server logic #####

# below are some input values for testing outside app -- should comment out otherwise
# input <- data.frame(seasonality = "seasonal", jr_drs = 7, joint = 5, referrals_change = 5, completed_change = 5, intensity = 85)

server <- function(input, output, session) {
  
  ## Created an observer that pops up a dialog box with the help text inside
  
  observeEvent(input$helpText, {
    showModal(modalDialog(
      title = NULL,
      "One strike will be incorporated every month from the first month until the number of inputted strike months is reached. 
                                    Strike intensity is the proportion of cancellations from the previous month seen in the current month.",
      easyClose = TRUE,
      footer = NULL,
      size = "s"
    ))
  })
  
  output$preset_server <- renderUI({
    
    ## Bolded the Scenarios to make them stand out
    same_activity_strikes <- HTML("<b>Scenario 1:</b> Current growth rates, with no further strike action after October 2023")
    same_activity_no_strikes <- HTML("<b>Scenario 2:</b> Current growth rates, and joint strikes continue every month into January 2025")
    less_activity_strikes <- HTML("<b>Scenario 3:</b> Completed pathways activity slows, and joint strikes continue every month into January 2025") 
    more_activity_no_strikes <- HTML("<b>Scenario 4:</b> Completed pathways activity increases, with no further strike action after October 2023")
    
    choice_values <- list(1, 2, 3, 4)
    
    choice_names <- list(same_activity_strikes, same_activity_no_strikes, less_activity_strikes, more_activity_no_strikes)
    
    ## Removed the label (but kept a line break) as its redundant since the column has a title
    radioButtons("preset", 
                 width = "100%",
                 label = NULL, 
                 choiceNames = choice_names,
                 choiceValues = choice_values)
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
        mutate(projected_referrals = if_else(month_no == 0
                                               , latest_referrals_actual
                                               , (latest_referrals/latest_workdays) * monthlyRate(input$referrals_change)^month_no * workdays * referrals_seasonality)      
             , projected_completed_pathways = if_else(month_no == 0
                                               , latest_completed_actual
                                               , (latest_completed/latest_workdays) * monthlyRate(input$completed_change)^month_no * workdays * activity_seasonality)
        ) %>% 

        
        # include effect of strikes
        # if month index is less than the round-up input value of strike days (divided by ), don't add strike days
        # if it is equal to number of round-up input, assign the remainder of days. otherwise give a 3.
        mutate(jr_dr_cancellations = case_when(input$jr_drs + 2 > month_no & month_no >= 2 ~ jr_dr_start_val * (input$intensity/100)^(month_no - 2)
                                               , month_no == 1 ~ jr_dr_sep23
                                               , TRUE ~ 0)
               , joint_cancellations =  case_when(input$joint + 2 > month_no & month_no >= 2 ~ joint_start_val * (input$intensity/100)^(month_no - 2)
                                                       , month_no == 1 ~ joint_sep23                                                   
                                                       , TRUE ~ 0)
        ) %>%
        
        mutate(projected_completed_pathways = projected_completed_pathways - jr_dr_cancellations - joint_cancellations) %>%
        
        mutate(projected_completed_pathways_linear = if_else(month_no > 0, predict(lm(projected_completed_pathways ~ month_no, data = data.frame(month_no = seq(0, interval(latest_data, ymd("2025-01-01")) %/% months(1)))
                                         )
                                      ), NA_real_)
               ) %>% 
        
        mutate(projected_referrals_linear = if_else(month_no > 0, predict(lm(projected_referrals ~ month_no, data.frame(month_no = seq(0, interval(latest_data, ymd("2025-01-01")) %/% months(1)))
                                          )
                                        ), NA_real_)
        ) %>% 
        
        # get new waiting list number
        # cumulative sum of referrals (up to t-1), - completed (at t-1), and adding to latest waiting list
        
        mutate(projected_waiting_list = latest_waitlist + cumsum(lag(projected_referrals, default = 0)) - cumsum(lag(projected_completed_pathways, default = 0))) %>% 
        
        # join the original dataset
        full_join(rtt_data, by = c("month_year", "workdays", "referrals_seasonality", "activity_seasonality")) 
    }
    
  )
  
  
  
  #### Referrals and completed plot ####
  # To do: can the if/else be implemented into one ggplot code so easier to update with colours etc.?
  
  output$referrals_plot <- plotly::renderPlotly(
    { 
      # Plot referrals and completeds on same graph
      to_plot <- predictions() %>% 
        ggplot(aes(x = month_year)) +
        geom_line(aes(y = new_referrals, color = "New referrals", 
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>New referrals:", format(round(as.numeric(new_referrals), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA), label = unit_format(unit = "M", scale = 1e-6)) +
        theme_classic() +
        geom_line(aes(y = referrals_trend, color = "Referrals linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Referrals linear trend:", format(round(as.numeric(referrals_trend), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = projected_referrals, color = "New referrals",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Projected referrals:", format(round(as.numeric(projected_referrals), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize, alpha = 0.8) +
        geom_line(aes(y = projected_referrals_linear, color = "Referrals linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Projected referrals linear trend:", format(round(as.numeric(projected_referrals_linear), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = total_activity, color = "Completed pathways",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Total completed pathways:", format(round(as.numeric(total_activity), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize) +
        scale_x_date(date_breaks = "1 year"
                     , date_minor_breaks = "3 months"
                     ## Changed start date of chart to January 2018
                     , limits = c(ymd("2018-01-01"), ymd("2025-01-15"))
                     , date_labels = "%b-%y"
                     , expand = c(0, 0)) +
        geom_line(aes(y = activity_trend, color = "Completed pathways linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Completed pathways linear trend:", format(round(as.numeric(activity_trend), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        geom_line(aes(y = projected_completed_pathways, color = "Completed pathways",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Projected completed pathways:", format(round(as.numeric(projected_completed_pathways), 1), nsmall=1, big.mark=","))),
                  linewidth = linesize, alpha = 0.8) + 
        geom_line(aes(y = projected_completed_pathways_linear, color = "Completed pathways linear trend",
                      group=1,
                      text = paste(
                        format(month_year, "%B %Y"), 
                        "<br>Projected completed pathways linear trend:", format(round(as.numeric(projected_completed_pathways_linear), 1), nsmall=1, big.mark=","))),
                  linetype = 3, linewidth = trendlinesize, alpha = 0.8) +
        xlab("") +
        ylab("Number of pathways (millions)") +
        # ggtitle("New referrals and completed pathways") +
        scale_color_manual(values = colors) +
        labs(color = "") +
        theme(text = element_text(size = textsize, family = "LTUnivers 330 BasicLight")
              , legend.position = "top"
              , axis.line = element_line(colour = axis_colour)
              , axis.ticks.x = element_line(color = axis_colour)
              , axis.ticks.y = element_blank()
              , panel.grid.major.x = element_blank()
              , panel.grid.major.y = element_line(color = grid_colour)
              )
      
      # get max y for plotting annotations
      max_y <- predictions() %>% 
        select(projected_referrals, projected_completed_pathways) %>% 
        unlist() %>% 
        max(na.rm = T)

      final_plot <- ggplotly(to_plot, tooltip = "text") %>%  #Need to add tooltip argument so only text that is manually created above is displayed, not also the default 
        add_annotations(
            text = "COVID-19",
            x = as.numeric(ymd("2020-06-15")),
            y = max_y*.95,
            showarrow = FALSE,
            xref = "x",
            yref = "y",
            textangle = 0,
            font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight"),
            align = "left"
            ) %>%
        add_annotations(
          text = "Deadline for next\n general election",
          x = as.numeric(ymd("2025-01-01")),
          y = max_y,
          showarrow = TRUE,
          xref = "x",
          yref = "y",
          ax = 0,
          ay = -20,
          arrowsize = 0.5,
          arrowcolor = "#676361",
          textangle = 0,
          xanchor = "right",
          font = list(color = "#676361", size = textsize-1, family = "LTUnivers 330 BasicLight")
        ) %>%
        add_annotations(
          text = "Projections",
          x = as.numeric(ymd("2023-11-15")),
          y = max_y*.95,
          showarrow = FALSE,
          xref = "x",
          yref = "y",
          textangle = 0,
          font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight"),
          align = "left"
        )
      
      final_plot[['x']][['layout']][['shapes']] <- c()
      
      final_plot <- layout(final_plot,
                           
                           shapes = list(
                             list(type = "rect",
                                  
                                  ## Moved the Covid-19 shading below the lines
                                  layer = "below",
                                  
                                  fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                                  
                                  x0 = as.numeric(ymd("2020-03-01")), x1 = as.numeric(ymd("2021-04-01")), xref = "x",
                                  
                                  y0 = 0, y1 = 1, yref = "paper"),
                             
                             ## Removed the General Election shaded area
                             ## NOTE: Have done so by commenting out in case it needs to be reinstated
                             # list(type = "rect",
                             #      
                             #      fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                             #      
                             #      x0 = as.numeric(ymd("2024-12-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                             #      
                             #      y0 = 0, y1 = 1, yref = "paper"),
                             
                             list(type = "rect",
                                  
                                  fillcolor = projection_annotations, line = list(color = projection_annotations), opacity = 0.1,
                                  
                                  x0 = as.numeric(ymd("2023-08-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                                  
                                  y0 = 0, y1 = 1, yref = "paper")
                             ),
                           

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
      geom_col(aes(y = projected_waiting_list, fill = "Projected waiting list",
                   text = paste(
                     format(month_year, "%B %Y"), 
                     "<br>Projected waiting list:", format(round(as.numeric(projected_waiting_list), 0), nsmall=0, big.mark=",")))) + # plot this first so latest date doesn't get overwritten
      geom_col(aes(y = waiting_list, fill = "Waiting list",
                   text = paste(
                     format(month_year, "%B %Y"), 
                     "<br>Waiting list:", format(round(as.numeric(waiting_list), 0), nsmall=0, big.mark=","))))  +
      scale_x_date(date_breaks = "1 year"
                   , date_minor_breaks = "3 months"
                   ## Changed start date of chart to January 2018
                   ## NOTE: Start date defined as 2017-12-19 in order to make sure that January 2018 bar displays on chart
                   , limits = c(ymd("2017-12-19"), ymd("2025-01-15"))
                   , date_labels = "%b-%y"
                   , expand = c(0,0)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA), label = unit_format(unit = "M", scale = 1e-6)) +
      theme_classic() +
      xlab("") +
      ylab("Waiting list size (millions)") +
      # ggtitle("Waiting list") +
      scale_fill_manual(values = colors) +
      labs(fill = "") +
      theme(text = element_text(size = textsize, family = "LTUnivers 330 BasicLight")
            , legend.position = "top"
            , axis.line = element_line(colour = axis_colour)
            , axis.ticks.x = element_line(color = axis_colour)
            , axis.ticks.y = element_blank()
            , panel.grid.major.x = element_blank()
            , panel.grid.major.y = element_line(color = grid_colour)
            ) +
      geom_segment(aes(x = ymd("2023-01-01"), xend = ymd("2025-01-15"), y = waiting_list_at_pledge, yend = waiting_list_at_pledge), linetype = 3, color = "white", alpha = 0.8) 

    # get max y for plotting annotations
    max_y_wl <- predictions() %>% 
      select(projected_waiting_list) %>% 
      unlist() %>% 
      max(na.rm = T)
    
    final_plot <- ggplotly(to_plot, tooltip = "text") %>% 
      add_annotations(
        text = "COVID-19",
        x = as.numeric(ymd("2020-06-15")),
        y = max_y_wl*.95,
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        textangle = 0,
        font = list(color = "#676361", size = textsize, family = "LTUnivers 330 BasicLight")
      ) %>%
      add_annotations(
        text = "Deadline for next\n general election",
        x = as.numeric(ymd("2025-01-01")),
        y = max_y_wl,
        showarrow = TRUE,
        xref = "x",
        yref = "y",
        ax = 0,
        ay = -20,
        arrowsize = 0.5,
        arrowcolor = "#676361",
        textangle = 0,
        xanchor = "right",
        font = list(color = "#676361", size = textsize-1, family = "LTUnivers 330 BasicLight")
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
        xanchor = "right",
        arrowcolor = "#676361",
        textangle = 0,
        font = list(color = "#676361", size = textsize-1)
      )
    
    final_plot[['x']][['layout']][['shapes']] <- c()
    
    final_plot <- layout(final_plot,
                         
                         shapes = list(
                           list(type = "rect",
                                
                                ## Moved the Covid-19 shading below the lines
                                layer = "below",
                                
                                fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                                
                                x0 = as.numeric(ymd("2020-03-01")), x1 = as.numeric(ymd("2021-04-01")), xref = "x",
                                
                                y0 = 0, y1 = 1, yref = "paper")#,
                           
                           ## Removed the General Election shaded area
                           ## NOTE: Have done so by commenting out in case it needs to be reinstated
                           # list(type = "rect",
                           #      
                           #      fillcolor = thf_annotations, line = list(color = thf_annotations), opacity = 0.2,
                           #      
                           #      x0 = as.numeric(ymd("2024-12-01")), x1 = as.numeric(ymd("2025-01-01")), xref = "x",
                           #      
                           #      y0 = 0, y1 = 1, yref = "paper")
                           ),
                         
                         xaxis = list(tickangle = 315),
                         
                         legend = list(x = ymd("2018-01-01"), y = 2500000, orientation = 'h', traceorder = 'reversed')
                         
                         )
    
    final_plot
    
  })
  
  output$download_data <- downloadHandler(
    
    filename = "data.csv",
    content = function(file) {
      write.csv(predictions() %>% 
                  filter(month_year >= latest_data) %>% 
                  select(month_year
                         , projected_referrals
                         , projected_referrals_linear
                         , projected_completed_pathways
                         , projected_completed_pathways_linear
                         , projected_waiting_list)
                , file, row.names = F)
    }
  )
}

##### Run app #####

shinyApp(ui = ui, server = server)

# profvis::profvis(runApp(shinyApp(ui, server)))

