library(shiny) 
library(tidyverse) 
library(tidymodels)
library(tidyquant)
library(readxl)
library(timetk)
library(plotly)
library(feasts)
library(tsibble)
library(ggHoriPlot)
library(ggplot2)
library(ggthemes)
library(modeltime)
library(reactable)
library(shinybusy)
library(kernlab)
library(randomForest)
library(glmnet)
library(parsnip)
library(rvest)
library(shinythemes)
library(bslib)
library(rsconnect)


# Import top 30 cryptocurrencies symbols downloaded from Yahoo Finance
top30 <- read_xlsx("data/top30coins.xlsx")


symbols <- top30 %>%
  select(`Symbol`)

top30symbol <- as.vector(symbols$Symbol)
#from_date = "2020-01-01"
#to_date = "2022-02-28"
period_type = "days"  # daily prices chosen

# Auto fetch
symbols <- top30 %>% select(`Symbol`)
symbol = 'symbols$Symbol'
url = paste0('https://finance.yahoo.com/quote/', symbol,'/financials?p=', symbol)
html_data = read_html(url) %>% html_node('body') %>% html_text()
top30symbol <- as.vector(symbols$Symbol)
to_date = as.Date(Sys.Date())
from_date = ymd(to_date) - years(2)

crypto <- tq_get(top30symbol,
                 get = "stock.prices",
                 from = from_date,
                 to = to_date) %>%
  group_by(symbol) %>% 
  tq_transmute(select= NULL, 
               mutate_fun = to.period, 
               period  = period_type)

# Defining variables

yLabel = "Price Difference"



# Define UI for application
ui <- fluidPage (
  theme = bs_theme(bootswatch = "cerulean"),
  navbarPage("ShinyCoin",
             tabPanel("Summary",
                      sidebarLayout(position = "right",
                                    sidebarPanel(width = 4,
                                                 h4("Behind the scene", style = "color:black"),
                                                 p("This app is mainly developed using the following  R packages:"),
                                                 img(src = "rpackages.png", width = 350, height = 200)
                                    ),
                                    mainPanel(
                                      h2("Introducing ShinyCoin"),
                                      p("This app is developed to allow users to identify and visualise generalized patterns of 
                          multiple cryptocurrencies at one-go easily. Unlike most applications that focuses on Bitcoin only, our app includes the top 30 cryptocurrencies downloaded from Yahoo Finance to bring diverse perspectives of the cryptocurrencies world.
                          "),
                                      h3("Use Cases"),
                                      p("In ShinyApp users are recommended to go through the following categories sequentially:"),
                                      
                                      strong("1. Exploratory"), 
                                      p("Horizon Graph: Provides an overview of price fluctuations of the top 30 crytocurrencies"),
                                      p("Time Series Plots & Anomaly Detection: Simple time series charts to achieve basic understanding of selected crytocurrencies and visualise anomalies."),
                                      p("User can based on the findings from this stage to identify suitable cryptocurrencies for further analysis e.g. those that appear to have seasonality."),
                                      
                                      strong("2. Seasonality & Correlation"),
                                      p("Seasonal Diagnostics & Time Series Decomposition: For deeper understanding and identification of seasonality patterns of different time intervals and applying popular time series decomposition method (STL)"),
                                      p("Autocorrelation: For more detailed analysis on the selected stock based on ACF and PCF."),
                                      
                                      strong("3. Prediction"),
                                      p("To forecast future prices of selected cryptocurrencies using classic time series forecasting methods like Arima and newer machine learning algorithms like prohpet."),
                                      p("For more information on this application, please visit our", a(href="https://shinycoin.netlify.app/#", "website."))
                                    )
                                    
                      )),
             navbarMenu("Exploratory",
                        tabPanel("Horizon Graph",
                                 sidebarLayout(
                                   sidebarPanel(width=3,
                                                selectInput(inputId = "hpricetype",
                                                            label = h6("Choose a value:"),
                                                            choices = list("Adjusted"="adjusted", 
                                                                           "High" = "high",
                                                                           "Low" = "low",
                                                                           "Open" = "open",
                                                                           "Close" = "close",
                                                                           "Volume" = "volume"),
                                                            selected = "adjusted"),
                                                dateRangeInput(inputId = "dateh",
                                                               label = h6("Date range"),
                                                               start = Sys.Date()-365,
                                                               end = Sys.Date()-2,
                                                               max = Sys.Date()-2),
                                   ),
                                   mainPanel(plotOutput("hori"))
                                 )
                        ),
                        tabPanel("Time Series & Anomaly Detection",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                
                                                # Let user pick coins
                                                selectizeInput(
                                                  inputId = "coinsa",
                                                  label = h6("Select a Coin"),
                                                  choices =c(top30symbol),
                                                  selected = "BTC-USD", 
                                                  multiple = T,
                                                  options = list(maxItems = 4)
                                                ),
                                                helpText("Select up to 4 symbols"),
                                                selectInput(inputId = "tpricetype",
                                                            label = h6("Choose a value:"),
                                                            choices = list("Adjusted"="adjusted", 
                                                                           "High" = "high",
                                                                           "Low" = "low",
                                                                           "Open" = "open",
                                                                           "Close" = "close",
                                                                           "Volume" = "volume")
                                                ),
                                                
                                                
                                                # Pick time period
                                                dateRangeInput(inputId = "dateZ", 
                                                               label = h6("Date range"),
                                                               start = Sys.Date() - 90,
                                                               end = Sys.Date() -2,
                                                               max = Sys.Date()-2),
                                                
                                   ),
                                   
                                   # Plot results
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Simple Time Series", plotlyOutput("coins_tsplot")),
                                       tabPanel("Anomaly Diagnostics", plotlyOutput("anom")
                                       ))
                                   )
                                   
                                 )
                                 
                        )),
             
             
             #Statistical Analysis 
             tabPanel("Seasonality & Correlation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "coinsb",
                                      label = h6("Select a coin:"),
                                      choices = c(top30symbol),
                                      selected = ""),
                          selectInput(inputId = "pricetype",
                                      label = h6("Choose a value:"),
                                      choices = list("Adjusted"="adjusted", 
                                                     "High" = "high",
                                                     "Low" = "low",
                                                     "Open" = "open",
                                                     "Close" = "close",
                                                     "Volume" = "volume"),
                                      selected = ""),
                          selectizeInput(inputId = "timeperiod",
                                         label = h6("Select time interval(s) for seasonal diagnostics:"),
                                         choices = list("Weekday" = "wday.lbl",
                                                        "Week" = "week",
                                                        "Month" = "month.lbl",
                                                        "Quarter" = "quarter",
                                                        "Year" = "year"),
                                         multiple = T,
                                         selected = list("wday.lbl", "month.lbl"),
                                         options = list(maxItems = 3)
                          ),
                          selectizeInput(inputId = "decompositions",
                                         label = h6("Select decompostion component(s):"),
                                         choices = list("Observed" = "observed",
                                                        "Season" = "season",
                                                        "Trend" = "trend",
                                                        "Remainder" = "remainder",
                                                        "Seasonal adjusted" = "seasadj"),
                                         multiple = T,
                                         selected = list("season", "trend", "remainder"),
                          ),
                          sliderInput(inputId = "lags",
                                      label = h6("Lag Specification (for auto-correlation):"),
                                      min = 1,
                                      max = 90,
                                      value=30),
                          dateRangeInput(inputId = "date", 
                                         label = h6("Date range"),
                                         start = Sys.Date() - 90,
                                         end = Sys.Date() -2,
                                         max = Sys.Date()-2),
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Seasonal Diagnostics", plotlyOutput("tsdiag")),
                            tabPanel("Time Series Decomposition", plotlyOutput("tsstl")),
                            tabPanel("Auto-correlation", plotlyOutput("tscorr"))
                          )
                        )
                      )
             ),
             
             #Prediction
             tabPanel("Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "coinsp",
                                      label = "Coins:",
                                      choices = c(crypto['symbol']),
                                      selected = ""),
                          dateRangeInput(inputId = "datep",
                                         label = "Date Range",
                                         start = Sys.Date()-365, 
                                         end = Sys.Date()-2,
                                         max = Sys.Date()-2),
                          helpText("Interval needs to be more than 3 months"),
                          sliderInput(inputId = "validation", 
                                      label = "Training - Validation Split",
                                      min = 0.6,
                                      max = 0.9,
                                      value = 0.8,
                                      step = 0.05),
                      
                          actionButton(inputId = "fit", 
                                       label = "Predict Future Prices",
                                       width = "100%",
                                       class = "btn-danger")),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Cross Validation Plan", plotlyOutput("TSPredict")),
                            tabPanel("Prediction", 
                                     fluidRow(column(width = 5,
                                                     plotlyOutput("TSForecast",
                                                                  width = "100%",
                                                                  height = "760px")), 
                                              column(width = 5,
                                                     plotlyOutput("TSRefit", 
                                                                  width = "100%",
                                                                  height = "760px")), 
                                              column(width = 5,
                                                     plotlyOutput("TSResiduals",
                                                                  width = "100%",
                                                                  height = "760px")), 
                                              column(width = 5,
                                                     uiOutput("PPerformance",
                                                              width = "100%",
                                                              height = "760px")))),
                            
                          )
                        )
                      ))
             
  )
)


# Define server logic

server <- function(input, output, session) {
  
  
  #horizonplot
  coinshori <- reactive(tq_get(top30symbol,get ="stock.prices", from = input$dateh[1],
                               to = input$dateh[2]))
  coins_h <- reactive({
    
    #EXPLORATORY
    input$gomap
    coinshori() %>% ggplot() +
      geom_horizon(aes(date,get(input$hpricetype)), origin = "midpoint") +
      #origin of horizon plot set as midpoint between the data range (default option)
      scale_fill_hcl(palette = 'RdBu', reverse = F) +
      facet_grid(symbol~.) +
      theme_few() +
      theme(
        panel.spacing.y=unit(0, "lines"),
        strip.text.y = element_text(size = 8, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none',
        axis.title=element_text(size=20),
        plot.title = element_text(size=28, face = "bold"),
        plot.subtitle = element_text(size=20, face = "bold")
      )+
      scale_x_date(expand=c(0,0), 
                   date_breaks = "1 month", 
                   date_labels = "%b%Y") +
      xlab("") +
      ggtitle('Top 30 Cryptocurrencies by Market Capitalisation')+
      guides(fill= guide_legend(title="stock price +(blue) or -(red)",
                                title.position = "top"))
  })
  output$hori <- renderPlot(coins_h(),width = 800, height = 600)
  
  
  
  output$TSLine <- renderPlotly(coins_plots())
  #anomaly detection - timetk
  
  #observeEvent(c(input$period,input$coinsba), {
    
    prices<- reactive(tq_get(input$coinsa,get="stock.prices", from = input$dateZ[1], to = input$dateZ[2]))

    # Create plot
    output$anom <- renderPlotly({
      prices() %>%
        group_by(symbol) %>%
        plot_anomaly_diagnostics(
          date,
          get(input$tpricetype),
          .facet_vars = NULL,
          .frequency = "auto",
          .trend = "auto",
          .alpha = 0.05,
          .max_anomalies = 0.2,
          .message = TRUE,
          .facet_ncol = 2,
          .facet_scales = "free",
          .facet_dir = "h",
          .line_color = "dodgerblue4",
          .line_size = 0.5,
          .line_type = 1,
          .line_alpha = 1,
          .anom_color = "#e31a1c",
          .anom_alpha = 1,
          .anom_size = 0.8,
          .ribbon_fill = "grey70",
          .ribbon_alpha = 0.2,
          .legend_show = TRUE,
          .title = "Anomaly Diagnostics",
          .x_lab = "",
          .y_lab = "price",
          .color_lab = "Anomaly",
          .interactive = TRUE
        ) %>%
        layout(height = 500)   
      
    })
    
    # Time Series
    
    output$coins_tsplot<- renderPlotly ({
      prices() %>%
        group_by(symbol) %>%
        plot_time_series(date, get(input$tpricetype), .facet_ncol = 2, .color_var= symbol,.line_size = 0.4)%>%
        layout(height = 500)    })
  
  
  #STATISTICAL
  
  coins <- reactive(tq_get(input$coinsb, get ="stock.prices", 
                           from = input$date[1], to = input$date[2], 
                           complete_cases = T))
  
  #Seasonal Diagnostics
  output$tsdiag <- renderPlotly ({
    coins() %>%
      #group_by(symbol) %>%
      plot_seasonal_diagnostics(date, get(input$pricetype), .feature_set = input$timeperiod, 
                                .title = input$coinsb, .geom_color = "dodgerblue4") %>%
      layout(height = 700)
  })
  
  
  #STL Decomposition
  output$tsstl <- renderPlotly({
    coins() %>%
      #group_by(symbol) %>%
      plot_stl_diagnostics(date, get(input$pricetype), 
                           .frequency = "auto", 
                           .feature_set= input$decompositions,
                           .trend="auto", 
                           .title = input$coinsb, 
                           .line_color = "dodgerblue4") %>%
      layout(height = 700)
  })
  
  
  #Autocorrelation - timetk
  output$tscorr <- renderPlotly ({
    coins() %>%
      group_by(symbol) %>%
      plot_acf_diagnostics(date, get(input$pricetype),
                           .title = input$coinsb,
                           .lags = input$lags,
                           .plotly_slider = T, 
                           .line_color = "dodgerblue4",
                           .line_size = 0.4,
                           .point_color = "dodgerblue3",
                           .point_size = 0.7,
                           .white_noise_line_type = 3) %>%
      layout(height = 700)
  })
  
  
  #Prediction - Forecast
  
  observeEvent(input$fit, {
    
    show_modal_spinner(
      spin = "pixel",
      color = "#112446",
      text = "Looking Into The Future",
      session = shiny::getDefaultReactiveDomain()
    )
    
    coinsp <- tq_get(input$coinsp, get ="stock.prices", from = input$datep[1], to = input$datep[2])
    
    ##########################
    # Time Series Cross Validation Plan
    splits <- coinsp %>%
      time_series_split(assess = "3 months", cumulative = TRUE)
    
    
    pred_plot <- splits %>%
      tk_time_series_cv_plan() %>%
      plot_time_series_cv_plan(date, adjusted)
    
    
    output$TSPredict <- renderPlotly(pred_plot)
    
    ##########################
    
    stock_tbl <- coinsp %>%
      select(date, adjusted) %>%
      set_names(c("date", "value"))
    
    stock_tbl <- stock_tbl %>%
      mutate(value = diff_vec(value, lag = 1)) %>%
      mutate(value = replace_na(value, 0))
    
    days_diff <- as.numeric(difftime(input$datep[2], input$datep[1], units = "days"))
    
    splits <- stock_tbl %>%
      time_series_split(assess = sprintf("%s days", round(days_diff*(1-input$validation))), 
                        cumulative = TRUE)
    
    recipe_spec <- recipe(value ~ date, training(splits))
    
    recipe_spec <- recipe_spec %>%
      step_timeseries_signature(date) %>%
      step_rm(contains("am.pm"), contains("hour"), contains("minute"),
              contains("second"), contains("xts"), contains("half"),
              contains(".iso")) %>%
      step_normalize(date_index.num)
    
    recipe_spec <- recipe_spec %>%
      step_fourier(date, period = 12, K = 1)
    
    recipe_spec <- recipe_spec %>% 
      step_dummy(all_nominal())
    
    recipe_spec_parsnip <- recipe_spec %>%
      update_role(date, new_role = "ID")
    
    ##### ARIMA #####
    model_fit_arima <- arima_reg(
      non_seasonal_ar = 2,
      non_seasonal_differences = 1,
      non_seasonal_ma = 2
    ) %>%
      set_engine("auto_arima") %>%
      fit(value ~ date, training(splits))
    ##### ARIMA #####
    
    ##### Prophet #####
    workflow_fit_prophet <- workflow() %>%
      add_model(
        prophet_reg(
          changepoint_num = 1,
          changepoint_range = 0.8
        ) %>% set_engine("prophet")
      ) %>%
      add_recipe(recipe_spec) %>%
      fit(training(splits))
    ##### Prophet #####
    
    ##### ElasticNet #####
    model_spec_glmnet <- linear_reg(
      penalty = 0.01, 
      mixture = 0.5
    ) %>%
      set_engine("glmnet")
    
    workflow_fit_glmnet <- workflow() %>%
      add_model(model_spec_glmnet) %>%
      add_recipe(recipe_spec_parsnip) %>%
      fit(training(splits))
    ##### ElasticNet #####
    
    ##### Random Forest #####
    model_spec_rf <- rand_forest(
      trees = 500, 
      min_n = 1
    ) %>%
      set_engine("randomForest")
    
    workflow_fit_rf <- workflow() %>%
      add_model(model_spec_rf) %>%
      add_recipe(recipe_spec_parsnip) %>%
      fit(training(splits))
    ##### Random Forest #####
    
    ##### XGBoost #####
    workflow_fit_xgboost <- workflow() %>%
      add_model(
        boost_tree(
          trees = 500,
          min_n = 1,
          tree_depth = 15
        ) %>% set_engine("xgboost")
      ) %>%
      add_recipe(recipe_spec_parsnip) %>%
      fit(training(splits))
    ##### XGBoost #####
    
    ##### SVM #####
    workflow_fit_svm <- workflow() %>%
      add_model(
        svm_rbf(
          cost = 1,
          margin = 0.5
        ) %>% 
          set_engine("kernlab") %>%
          set_mode("regression")
      ) %>%
      add_recipe(recipe_spec_parsnip) %>%
      fit(training(splits))
    ##### SVM #####
    
    ##### ARIMA Boosted #####
    workflow_fit_arima_boosted <- workflow() %>%
      add_model(
        arima_boost(
          non_seasonal_ar = 2,
          non_seasonal_differences = 1,
          non_seasonal_ma = 2,
          trees = 500,
          min_n = 1,
          tree_depth = 15
        ) %>%
          set_engine(engine = "auto_arima_xgboost")
      ) %>%
      add_recipe(recipe_spec) %>%
      fit(training(splits))
    ##### ARIMA Boosted #####
    
    ##### Prophet Boosted #####
    model_spec_prophet_boost <- prophet_boost(
      changepoint_num = 25,
      changepoint_range = 0.8,
      trees = 500,
      min_n = 1,
      tree_depth = 15) %>%
      set_engine("prophet_xgboost") 
    
    workflow_fit_prophet_boost <- workflow() %>%
      add_model(model_spec_prophet_boost) %>%
      add_recipe(recipe_spec) %>%
      fit(training(splits))
    ##### Prophet Boosted #####
    
    model_table <- modeltime_table(
      model_fit_arima,
      workflow_fit_prophet,
      workflow_fit_glmnet,
      workflow_fit_rf,
      workflow_fit_xgboost,
      workflow_fit_svm,
      workflow_fit_arima_boosted,
      workflow_fit_prophet_boost) %>%
      update_model_description(1, "ARIMA") %>%
      update_model_description(2, "Prophet") %>%
      update_model_description(3, "ElasticNet") %>%
      update_model_description(4, "Random Forest") %>%
      update_model_description(5, "XGBoost") %>%
      update_model_description(6, "SVM") %>%
      update_model_description(7, "ARIMA Boosted") %>%
      update_model_description(8, "Prophet Boosted")
    
    calibration_table <- model_table %>%
      modeltime_calibrate(testing(splits))
    
    # Prediction
    
    forecast_table <- calibration_table %>%
      modeltime_forecast(actual_data = stock_tbl)
    
    forecast_plots <- forecast_table %>%
      filter(.model_desc != "ACTUAL") %>%
      plot_modeltime_forecast(.facet_vars = .model_desc, 
                              .facet_ncol = 2, 
                              .interactive = FALSE,
                              .facet_scales = "fixed",
                              .legend_show = FALSE) +
      geom_line(data = forecast_table %>% 
                  filter(.model_desc == "ACTUAL") %>%
                  select(.index, .value),
                color = "#2c3e50",
                size = 0.5) +
      labs(title = paste("Validation Plot for", input$coinsp),
           y = yLabel) +
      scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                     "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f")) +
      theme(panel.spacing = unit(0.25, "lines")
      )
    
    forecast_accuracy_table <- calibration_table %>%
      modeltime_accuracy() %>%
      select(.model_desc, mae, mape, mase, rmse) %>%
      reactable(
        searchable = FALSE,
        filterable = FALSE,
        showPageSizeOptions = FALSE,
        striped = TRUE,
        highlight = TRUE,
        columns = list(
          .model_desc = colDef(name = "Model"),
          mae = colDef(format = colFormat(digits = 2)),
          mape = colDef(format = colFormat(digits = 2)),
          mase = colDef(format = colFormat(digits = 2)),
          rmse = colDef(format = colFormat(digits = 2))
        )
      )
    
    forecast_refit_table <- calibration_table %>%
      modeltime_refit(stock_tbl) %>%
      modeltime_forecast(h = sprintf("%s days", 10), actual_data = stock_tbl)
    
    forecast_refit_plots <- forecast_refit_table %>%
      filter(.model_desc != "ACTUAL") %>%
      plot_modeltime_forecast(.facet_vars = .model_desc, 
                              .facet_ncol = 2, 
                              .interactive = FALSE,
                              .facet_scales = "fixed",
                              .legend_show = FALSE) +
      geom_line(data = forecast_table %>% 
                  filter(.model_desc == "ACTUAL") %>%
                  select(.index, .value),
                color = "#2c3e50",
                size = 0.5) +
      labs(title = paste("Forecast Plot for", input$coinsp),
           y = yLabel) +
      scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                     "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f")) +
      theme(panel.spacing = unit(0.25, "lines"))
    
    forecast_refit_plots_multi <- forecast_refit_table %>%
      plot_modeltime_forecast(.interactive = FALSE,
                              .legend_show = TRUE,
                              .legend_max_width = 25,
                              .conf_interval_show = FALSE) +
      labs(title = paste("Forecast Plot for", input$coinsp),
           y = yLabel)
    
    # Residuals
    
    residuals_table <- calibration_table %>%
      modeltime_residuals()
    
    residuals_plots <- residuals_table %>%
      filter(.model_desc != "ACTUAL") %>%
      plot_modeltime_residuals(.type = "timeplot",
                               .facet_vars = .model_desc, 
                               .facet_ncol = 2, 
                               .interactive = FALSE,
                               .facet_scales = "fixed") +
      labs(title = paste("Residuals Plot for", input$coinsp),
           y = "Residuals") +
      scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                     "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f")) +
      theme(panel.spacing = unit(0.25, "lines"),
            legend.position='none')
    
    
    
    output$TSForecast <- renderPlotly({
      ggplotly(forecast_plots)
    })
    
    output$TSRefit <- renderPlotly({
      ggplotly(forecast_refit_plots)
    })
    
    output$TSResiduals <- renderPlotly({
      residuals_plots
    })
    
    output$PPerformance <- renderUI({
      tagList(
        reactableOutput(session$ns("performanceTable")
        )
      )
    })
    
    output$performanceTable <- renderReactable({
      forecast_accuracy_table
    })
    
    remove_modal_spinner(session = getDefaultReactiveDomain())
    
    
  } 
  )# End Event
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
