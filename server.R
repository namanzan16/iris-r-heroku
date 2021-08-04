

server <- function(input, output, session) {
  
  
  train <- fread("train1.csv",stringsAsFactors=FALSE)
  train$date <- dmy(train$date)
  train$Month <- as.yearmon(train$date)
  
  
  output$sale_plot <- renderDygraph({
    if(input$select_time == 1){
      MSP <- aggregate(sales ~date, train, mean)
      
      daterange <- xts(x = MSP$sales,
                       order.by = MSP$date)
      
      dygraph(daterange, main = "The Growth of Sales by date") %>%
        dyOptions(fillGraph = T, fillAlpha = 0.4)%>%
        dyRangeSelector(dateWindow = c("2013-01-01","2017-12-31"),
                        height = 20) %>%
        dyAxis("y", label = "Sale")%>%dySeries("V1", label = "Value")
    } else if(input$select_time == 2){
      MSP2 <- aggregate(sales ~Month, train, mean)
      
      daterange <- xts(x = MSP2$sales,
                       order.by = MSP2$Month)
      dygraph(daterange, main = "The Growth of Sales by Month") %>%
        dyOptions(fillGraph = T, fillAlpha = 0.4)%>%
        dyRangeSelector(dateWindow = c("2013-01-01","2017-12-31"),
                        height = 20) %>%
        dyAxis("y", label = "Sale")%>%dySeries("V1", label = "Value")
    } else {
      MSP3 <- aggregate(sales ~Year, train, mean, )
      MSP3$Year <- lubridate::ymd(MSP3$Year, truncated = 2L)
      daterange <- xts(x = MSP3$sales,
                       order.by = as.Date(MSP3$Year, format="%Y"))
      dygraph(daterange, main = "The Growth of Sales by Year") %>%
        dyOptions(fillGraph = T, fillAlpha = 0.4)%>%
        dyRangeSelector(dateWindow = c("2013-01-01","2017-01-01"),
                        height = 20) %>%
        dyAxis("y", label = "Sale")%>%dySeries("V1", label = "Value")
    }
  })
  
  
  
  output$dist_plot <- renderPlot({
    x = input$select_distribution
    if(x==1){
      ggplot(train, aes(sales))+
        geom_histogram(fill = "blue")+
        ggtitle("Distribution of Sales")+
        theme(plot.title = element_text(hjust = .5))
    }
    else if(x == 2){
      store_sub = subset(train, `store` %in% input$select_store ,
                         select = c(store,sales))
      ggplot(store_sub, aes(sales, fill = as.factor(store)))+
        geom_histogram()+
        facet_wrap(~as.factor(store))+
        ggtitle("Distribution of Sales by Store")+
        theme(plot.title = element_text(hjust = .5))+
        guides(fill=guide_legend(title="Store")) 
    }
    else{
      item_sub = subset(train, `item` %in% input$select_item ,
                        select = c(item,sales))
      ggplot(item_sub, aes(sales, fill = as.factor(item)))+
        geom_histogram()+
        facet_wrap(~as.factor(item))+
        ggtitle("Distribution of Sales by Item")+
        theme(plot.title = element_text(hjust = .5))+
        guides(fill=guide_legend(title="Item"))
    }
    
  })
  
  # prophet_daily <- function(y_train) {
  #    years <- c(2013,2014,2015,2016,2017)
  #    country.name <- 'IN'
  #    holidays<- prophet:::make_holidays_df(years, country.name)
  #    
  #    model_prophet <- prophet()
  #    model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
  #    model_prophet <- prophet(y_train, holidays = holidays,daily.seasonality = T)
  #    
  #    
  #    future <- make_future_dataframe(model_prophet, periods = 365)
  #    forecast <- predict(model_prophet, future)
  #    forecast_final<- forecast[, c("ds","yhat")]
  #    
  #    
  #    prediction_final<-as.data.frame(forecast_final)
  #    # prediction_final<-  xts::last(prediction_final1[, c("ds","yhat")],365)
  #    colnames(prediction_final)<-c("Date","Prediction(Sales)")
  #    prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
  #    prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
  # return(prediction_final)
  #   
  # }
  
  # tbats_daily <- function(training) {
  # tbats_model <- tbats(training)
  # tbats_forecast <- forecast(tbats_model, h=365)
  # prediction_final<- sw_sweep(tbats_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
  # prediction_final<- prediction_final %>% select(Date, y)
  # # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
  # colnames(prediction_final)<-c("Date","Prediction(Sales)")
  # prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
  # prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
  # return(prediction_final)
  #    
  # }
  # 
  # naive_daily <- function(training) {
  # naive <- snaive(training, h=365)
  # prediction_final<- sw_sweep(naive,timetk_idx = TRUE, fitted = F, rename_index = "Date")
  # prediction_final<- prediction_final %>% select(Date, y)
  # # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
  # colnames(prediction_final)<-c("Date","Prediction(Sales)")
  # prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
  # prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
  # 
  # return(prediction_final)
  #    
  # }
  # 
  # nnetar_daily <- function(training){
  #    
  # nn1 <- nnetar(training)
  # fc1 <- forecast(nn1,h=365)
  # prediction_final<- sw_sweep(fc1,timetk_idx = TRUE, fitted = F, rename_index = "Date")
  # prediction_final<- prediction_final %>% select(Date, y)
  # # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
  # colnames(prediction_final)<-c("Date","Prediction(Sales)")
  # prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
  # prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
  #    
  # return(prediction_final)
  # }
  # 
  # ets_daily<- function(training){
  # ets_model <- ets(training, allow.multiplicative.trend = TRUE)
  # ets_forecast <- forecast(ets_model, h=365)
  # prediction_final<- sw_sweep(ets_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
  # prediction_final<- prediction_final %>% select(Date, y)
  # # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
  # colnames(prediction_final)<-c("Date","Prediction(Sales)")
  # prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
  # prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
  #    
  # return(prediction_final)
  # }
  
  
  
  
  datasets <- eventReactive(input$gobutton,{
    
    prophet_flag=0
    tbats_flag=0
    naive_flag=0
    nn_flag=0
    ets_flag=0
    if("1" %in% input$select_model)
      prophet_flag=1
    if("2" %in% input$select_model)
      tbats_flag=1
    if("3" %in% input$select_model)
      naive_flag=1
    if("4" %in% input$select_model)
      nn_flag=1
    if("5" %in% input$select_model)
      ets_flag=1
    
    df.names <- c("prophet", "tbats", "naive", "nn", "ets", "mon","pra","tba","naa","nna","eta") 
    df <- vector("list", length(df.names))
    names(df) <- df.names
    
    if(!is.null(input$select_region) && is.null(input$select_store1) && 
       is.null(input$select_item1)){
      
      train = train %>% filter(region %in% input$select_region) %>% subset(select = c(date,sales))
      train = aggregate(train$sales, by=list(train$date), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,1461)
      y_test = tail(train,365)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- tk_ts(y_train, start = 2013, end = 2017, frequency = 365, silent = TRUE)
      validation <- tk_ts(y_test, start = 2017, frequency = 365, silent = TRUE)
      
      if(prophet_flag == 1){
        years <- c(2013,2014,2015,2016,2017)
        country.name <- 'IN'
        holidays<- prophet:::make_holidays_df(years, country.name)
        
        model_prophet <- prophet()
        model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
        model_prophet <- prophet(y_train, holidays = holidays,daily.seasonality = T)
        
        
        future <- make_future_dataframe(model_prophet, periods = 365)
        forecast <- predict(model_prophet, future)
        forecast_final<- forecast[, c("ds","yhat")]
        
        
        prediction_final<-as.data.frame(forecast_final)
        am <- tail(prediction_final,365)
        # prediction_final<-  xts::last(prediction_final1[, c("ds","yhat")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        
        df$prophet <- prediction_final
        df$pra <- MAPE(y_test$y, am$yhat) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=365)
        prediction_final<- sw_sweep(tbats_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=365)
        prediction_final<- sw_sweep(naive,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$naive <- prediction_final
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=365)
        prediction_final<- sw_sweep(fc1,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$nn <- prediction_final
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=365)
        prediction_final<- sw_sweep(ets_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$ets <- prediction_final
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    if(!is.null(input$select_region) && !is.null(input$select_store1) && 
       is.null(input$select_item1)){
      train = train %>% filter(region %in% input$select_region & 
                                 store %in% input$select_store1) %>% subset(select = c(date,sales))
      train = aggregate(train$sales, by=list(train$date), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,1461)
      y_test = tail(train,365)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- tk_ts(y_train, start = 2013, end = 2017, frequency = 365, silent = TRUE)
      validation <- tk_ts(y_test, start = 2017, frequency = 365, silent = TRUE)
      
      if(prophet_flag == 1){
        years <- c(2013,2014,2015,2016,2017)
        country.name <- 'IN'
        holidays<- prophet:::make_holidays_df(years, country.name)
        
        model_prophet <- prophet()
        model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
        model_prophet <- prophet(y_train, holidays = holidays,daily.seasonality = T)
        
        
        future <- make_future_dataframe(model_prophet, periods = 365)
        forecast <- predict(model_prophet, future)
        forecast_final<- forecast[, c("ds","yhat")]
        
        
        prediction_final<-as.data.frame(forecast_final)
        am <- tail(prediction_final,365)
        # prediction_final<-  xts::last(prediction_final1[, c("ds","yhat")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        
        df$prophet <- prediction_final
        df$pra <- MAPE(y_test$y, am$yhat) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=365)
        prediction_final<- sw_sweep(tbats_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=365)
        prediction_final<- sw_sweep(naive,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$naive <- prediction_final
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=365)
        prediction_final<- sw_sweep(fc1,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$nn <- prediction_final
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=365)
        prediction_final<- sw_sweep(ets_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$ets <- prediction_final
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    if(!is.null(input$select_region) && !is.null(input$select_store1) && 
       !is.null(input$select_item1)){
      
      train = train %>% filter(region %in% input$select_region &
                                 store %in% input$select_store1 &
                                 item %in% input$select_item1) %>%
        subset(select = c(date,sales))
      train = aggregate(train$sales, by=list(train$date), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,1461)
      y_test = tail(train,365)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- tk_ts(y_train, start = 2013, end = 2017, frequency = 365, silent = TRUE)
      validation <- tk_ts(y_test, start = 2017, frequency = 365, silent = TRUE)
      
      if(prophet_flag == 1){
        years <- c(2013,2014,2015,2016,2017)
        country.name <- 'IN'
        holidays<- prophet:::make_holidays_df(years, country.name)
        
        model_prophet <- prophet()
        model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
        model_prophet <- prophet(y_train, holidays = holidays,daily.seasonality = T)
        
        
        future <- make_future_dataframe(model_prophet, periods = 365)
        forecast <- predict(model_prophet, future)
        forecast_final<- forecast[, c("ds","yhat")]
        
        
        prediction_final<-as.data.frame(forecast_final)
        am <- tail(prediction_final,365)
        # prediction_final<-  xts::last(prediction_final1[, c("ds","yhat")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        
        df$prophet <- prediction_final
        df$pra <- MAPE(y_test$y, am$yhat) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=365)
        prediction_final<- sw_sweep(tbats_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=365)
        prediction_final<- sw_sweep(naive,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$naive <- prediction_final
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=365)
        prediction_final<- sw_sweep(fc1,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$nn <- prediction_final
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=365)
        prediction_final<- sw_sweep(ets_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$ets <- prediction_final
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    
    
    if(!is.null(input$select_region) && is.null(input$select_store1) && 
       !is.null(input$select_item1)){
      
      train = train %>% filter(region %in% input$select_region &
                                 item %in% input$select_item1) %>%
        subset(select = c(date,sales))
      train = aggregate(train$sales, by=list(train$date), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,1461)
      y_test = tail(train,365)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- tk_ts(y_train, start = 2013, end = 2017, frequency = 365, silent = TRUE)
      validation <- tk_ts(y_test, start = 2017, frequency = 365, silent = TRUE)
      
      if(prophet_flag == 1){
        years <- c(2013,2014,2015,2016,2017)
        country.name <- 'IN'
        holidays<- prophet:::make_holidays_df(years, country.name)
        
        model_prophet <- prophet()
        model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
        model_prophet <- prophet(y_train, holidays = holidays,daily.seasonality = T)
        
        
        future <- make_future_dataframe(model_prophet, periods = 365)
        forecast <- predict(model_prophet, future)
        forecast_final<- forecast[, c("ds","yhat")]
        
        
        prediction_final<-as.data.frame(forecast_final)
        am <- tail(prediction_final,365)
        # prediction_final<-  xts::last(prediction_final1[, c("ds","yhat")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        
        df$prophet <- prediction_final
        df$pra <- MAPE(y_test$y, am$yhat) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=365)
        prediction_final<- sw_sweep(tbats_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=365)
        prediction_final<- sw_sweep(naive,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$naive <- prediction_final
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=365)
        prediction_final<- sw_sweep(fc1,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$nn <- prediction_final
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=365)
        prediction_final<- sw_sweep(ets_forecast,timetk_idx = TRUE, fitted = F, rename_index = "Date")
        prediction_final<- prediction_final %>% select(Date, y)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final)<-c("Date","Prediction(Sales)")
        prediction_final$Date <- as.POSIXct(prediction_final$Date, format = "%d-%m-%Y")
        prediction_final$`Prediction(Sales)` <- round(prediction_final$`Prediction(Sales)`, 0)
        df$ets <- prediction_final
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    df
  })
  
  observe({
    upd_df <- subset(train,region %in% input$select_region)
    upd <- unique(upd_df[['store']])
    updatePickerInput(session = session, 
                      inputId = "select_store1",
                      choices = upd)
  })
  
  
  output$accuracy_plot <- renderGirafe({
    m_df<-datasets()
    prophet_flag1=0
    tbats_flag1=0
    naive_flag1=0
    nn_flag1=0
    ets_flag1=0
    if("1" %in% input$select_model)
      prophet_flag1=1
    if("2" %in% input$select_model)
      tbats_flag1=1
    if("3" %in% input$select_model)
      naive_flag1=1
    if("4" %in% input$select_model)
      nn_flag1=1
    if("5" %in% input$select_model)
      ets_flag1=1
    
    acc_data <- data.frame(Models = character(0), MAPE = numeric(0), stringsAsFactors = F)
    
    if(prophet_flag1==1){
      acc_data[nrow(acc_data)+1,]<-  c("PROPHET", round(m_df$pra,2))
    }
    if(tbats_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("TBATS", round(m_df$tba,2))
    }
    if(naive_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("NAIVE", round(m_df$naa,2))
    }
    if(nn_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("NNETAR", round(m_df$nna,2))
    }
    if(ets_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("ETS", round(m_df$eta,2))
    }
    # %>% formatDate(1, "toLocaleDateString")
    accp<- ggplot(acc_data, aes(x=Models, y= as.integer(MAPE), fill=Models)) +
      geom_bar_interactive(aes(x=Models, y= as.integer(MAPE), fill=Models,
                               tooltip= MAPE, data_id=MAPE), 
                           stat="identity", width = 0.4)+
      theme_minimal()+
      theme(axis.title.x = element_blank())+
      scale_fill_brewer(palette="Dark2")+
      labs(y="MAPE")+
      labs(title = "Mean Absolute Percentage Error (MAPE) value for Model evaluation")
    girafe(ggobj = accp, height_svg = 6.2,width_svg = 12.6,
           options = list(
             opts_sizing(rescale = F),
             opts_hover_inv(css = "opacity:0.1;"),
             opts_hover(css = ":autofill;"),
             opts_toolbar(position = 'bottomright')))
  })
  
  
  output$forecast_table <- renderDataTable({
    m_df <- datasets()
    actual <- xts(x = m_df$mon$y,order.by = as.Date(m_df$mon$ds))
    prophet_flag1=0
    tbats_flag1=0
    naive_flag1=0
    nn_flag1=0
    ets_flag1=0
    if("1" %in% input$select_model)
      prophet_flag1=1
    if("2" %in% input$select_model)
      tbats_flag1=1
    if("3" %in% input$select_model)
      naive_flag1=1
    if("4" %in% input$select_model)
      nn_flag1=1
    if("5" %in% input$select_model)
      ets_flag1=1
    actual_value = actual
    if(prophet_flag1 ==1){
      prophet <- xts(x = xts::last(m_df$prophet$`Prediction(Sales)`,365),
                     order.by = as.Date(xts::last(m_df$prophet$Date,365)))
      actual_value = cbind(actual_value,prophet)
    }
    if(tbats_flag1 ==1){
      tbats <- xts(x = xts::last(m_df$tbats$`Prediction(Sales)`,365),
                   order.by = as.Date(xts::last(m_df$tbats$Date,365)))
      actual_value = cbind(actual_value,tbats)
    }
    if(naive_flag1 ==1){
      naive <- xts(x = xts::last(m_df$naive$`Prediction(Sales)`,365),
                   order.by = as.Date(xts::last(m_df$naive$Date,365)))
      actual_value = cbind(actual_value,naive)
    }
    if(nn_flag1 ==1){
      nnetar <- xts(x = xts::last(m_df$nn$`Prediction(Sales)`,365),
                    order.by = as.Date(xts::last(m_df$nn$Date,365)))
      actual_value = cbind(actual_value,nnetar)
    }
    if(ets_flag1 ==1){
      ets <- xts(x = xts::last(m_df$ets$`Prediction(Sales)`,365),
                 order.by = as.Date(xts::last(m_df$ets$Date,365)))
      actual_value = cbind(actual_value,ets)
    }
    actual_value <- tail(actual_value,365)
    df3 = data.frame(date=index(actual_value), coredata(actual_value))
    df3$date <- format(as.POSIXct(df3$date),format = "%m/%d/%Y")
    # data_table <- df3
    # data_table<-  xts::last(data_table,365)
    
    
    DT::datatable(df3,rownames=F) %>% formatDate(
      columns = 1, 
      method =  "toLocaleDateString", 
      params = list(
        'en-GB', 
        list(
          year = 'numeric', 
          month = 'numeric',
          day = 'numeric')
      )
    )
    
  })
  
  output$forecast_plot <- renderDygraph({
    m_df <- datasets()
    actual <- xts(x = m_df$mon$y,order.by = as.Date(m_df$mon$ds))
    prophet_flag1=0
    tbats_flag1=0
    naive_flag1=0
    nn_flag1=0
    ets_flag1=0
    if("1" %in% input$select_model)
      prophet_flag1=1
    if("2" %in% input$select_model)
      tbats_flag1=1
    if("3" %in% input$select_model)
      naive_flag1=1
    if("4" %in% input$select_model)
      nn_flag1=1
    if("5" %in% input$select_model)
      ets_flag1=1
    actual_value = actual
    if(prophet_flag1 ==1){
      prophet <- xts(x = xts::last(m_df$prophet$`Prediction(Sales)`,365),
                     order.by = as.Date(xts::last(m_df$prophet$Date,365)))
      actual_value = cbind(actual_value,prophet)
    }
    if(tbats_flag1 ==1){
      tbats <- xts(x = xts::last(m_df$tbats$`Prediction(Sales)`,365),
                   order.by = as.Date(xts::last(m_df$tbats$Date,365)))
      actual_value = cbind(actual_value,tbats)
    }
    if(naive_flag1 ==1){
      naive <- xts(x = xts::last(m_df$naive$`Prediction(Sales)`,365),
                   order.by = as.Date(xts::last(m_df$naive$Date,365)))
      actual_value = cbind(actual_value,naive)
    }
    if(nn_flag1 ==1){
      nnetar <- xts(x = xts::last(m_df$nn$`Prediction(Sales)`,365),
                    order.by = as.Date(xts::last(m_df$nn$Date,365)))
      actual_value = cbind(actual_value,nnetar)
    }
    if(ets_flag1 ==1){
      ets <- xts(x = xts::last(m_df$ets$`Prediction(Sales)`,365),
                 order.by = as.Date(xts::last(m_df$ets$Date,365)))
      actual_value = cbind(actual_value,ets)
    }
    actual_value <- tail(actual_value,730)
    
    dygraph(actual_value, main = "Sales Forecast") %>%
      dyOptions(fillGraph = F, fillAlpha = 0.4)%>%
      dyAxis("y", label = "Sales")%>%
      dyRangeSelector(height = 20) %>%
      dyShading(from = "2017-01-01", to = "2017-12-31", color = "#ccccff") %>%
      dyShading(from = "2013-01-01", to = "2016-12-31", color = "#ebebfc")
    
  })
  
  m_datasets <- eventReactive(input$gobutton1,{
    
    arima_flag=0
    tbats_flag=0
    naive_flag=0
    nn_flag=0
    ets_flag=0
    if("1" %in% input$select_m_model)
      arima_flag=1
    if("2" %in% input$select_m_model)
      tbats_flag=1
    if("3" %in% input$select_m_model)
      naive_flag=1
    if("4" %in% input$select_m_model)
      nn_flag=1
    if("5" %in% input$select_m_model)
      ets_flag=1
    
    df.names <- c("arima", "tbats", "naive", "nn", "ets", "mon","ara","tba","naa","nna","eta") 
    df <- vector("list", length(df.names))
    names(df) <- df.names
    
    
    if(!is.null(input$select_m_region) && is.null(input$select_m_store1) && 
       is.null(input$select_m_item1)){
      
      train = train %>% filter(region %in% input$select_m_region) %>% subset(select = c(Month,sales))
      train = aggregate(train$sales, by=list(train$Month), FUN=sum)
      colnames(train) <- c("ds","y")
      df$mon <- train
      y_train <- head(train,48)
      y_test <- tail(train,12)
      colnames(y_test) <- c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- ts(y_train$y, start = 2013, end = c(2016,12), frequency = 12)
      validation <- ts(y_test$y, start = 2017, frequency = 12)
      
      
      if(arima_flag == 1){
        arima_optimal <- auto.arima(training)
        arima_forecast <- forecast::forecast(arima_optimal, h=12)
        prediction_final1<- sw_sweep(arima_forecast, fitted = F, rename_index = "Date")
        prediction_final1<- prediction_final1 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final1)<-c("Date","Prediction(Sales)")
        prediction_final1$Date <- format(as.Date(as.yearmon(prediction_final1$Date)),format = "%B-%Y")
        prediction_final1$`Prediction(Sales)` <- round(prediction_final1$`Prediction(Sales)`, 0)
        df$arima <- prediction_final1
        df$ara <- MAPE(arima_forecast$mean, validation) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=12)
        prediction_final2<- sw_sweep(tbats_forecast, fitted = F, rename_index = "Date")
        prediction_final2<- prediction_final2 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final2)<-c("Date","Prediction(Sales)")
        prediction_final2$Date <- format(as.Date(as.yearmon(prediction_final2$Date)),format = "%B-%Y")
        prediction_final2$`Prediction(Sales)` <- round(prediction_final2$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final2
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=12)
        prediction_final3<- sw_sweep(naive, fitted = F, rename_index = "Date")
        prediction_final3<- prediction_final3 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final3)<-c("Date","Prediction(Sales)")
        prediction_final3$Date <- format(as.Date(as.yearmon(prediction_final3$Date)),format = "%B-%Y")
        prediction_final3$`Prediction(Sales)` <- round(prediction_final3$`Prediction(Sales)`, 0)
        df$naive <- prediction_final3
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=12)
        prediction_final4<- sw_sweep(fc1, fitted = F, rename_index = "Date")
        prediction_final4<- prediction_final4 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final4)<-c("Date","Prediction(Sales)")
        prediction_final4$Date <- format(as.Date(as.yearmon(prediction_final4$Date)),format = "%B-%Y")
        prediction_final4$`Prediction(Sales)` <- round(prediction_final4$`Prediction(Sales)`, 0)
        df$nn <- prediction_final4
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=12)
        prediction_final5<- sw_sweep(ets_forecast, fitted = F, rename_index = "Date")
        prediction_final5<- prediction_final5 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final5)<-c("Date","Prediction(Sales)")
        prediction_final5$Date <- format(as.Date(as.yearmon(prediction_final5$Date)),format = "%B-%Y")
        prediction_final5$`Prediction(Sales)` <- round(prediction_final5$`Prediction(Sales)`, 0)
        df$ets <- prediction_final5
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    if(!is.null(input$select_m_region) && !is.null(input$select_m_store1) && 
       is.null(input$select_m_item1)){
      train = train %>% filter(region %in% input$select_m_region & 
                                 store %in% input$select_m_store1) %>% subset(select = c(Month,sales))
      train = aggregate(train$sales, by=list(train$Month), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,48)
      y_test = tail(train,12)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- ts(y_train$y, start = 2013, end = c(2016,12), frequency = 12)
      validation <- ts(y_test$y, start = 2017, frequency = 12)
      
      if(arima_flag == 1){
        arima_optimal <- auto.arima(training)
        arima_forecast <- forecast::forecast(arima_optimal, h=12)
        prediction_final1<- sw_sweep(arima_forecast, fitted = F, rename_index = "Date")
        prediction_final1<- prediction_final1 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final1)<-c("Date","Prediction(Sales)")
        prediction_final1$Date <- format(as.Date(as.yearmon(prediction_final1$Date)),format = "%B-%Y")
        prediction_final1$`Prediction(Sales)` <- round(prediction_final1$`Prediction(Sales)`, 0)
        df$arima <- prediction_final1
        df$ara <- MAPE(arima_forecast$mean, validation) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=12)
        prediction_final2<- sw_sweep(tbats_forecast, fitted = F, rename_index = "Date")
        prediction_final2<- prediction_final2 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final2)<-c("Date","Prediction(Sales)")
        prediction_final2$Date <- format(as.Date(as.yearmon(prediction_final2$Date)),format = "%B-%Y")
        prediction_final2$`Prediction(Sales)` <- round(prediction_final2$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final2
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=12)
        prediction_final3<- sw_sweep(naive, fitted = F, rename_index = "Date")
        prediction_final3<- prediction_final3 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final3)<-c("Date","Prediction(Sales)")
        prediction_final3$Date <- format(as.Date(as.yearmon(prediction_final3$Date)),format = "%B-%Y")
        prediction_final3$`Prediction(Sales)` <- round(prediction_final3$`Prediction(Sales)`, 0)
        df$naive <- prediction_final3
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=12)
        prediction_final4<- sw_sweep(fc1, fitted = F, rename_index = "Date")
        prediction_final4<- prediction_final4 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final4)<-c("Date","Prediction(Sales)")
        prediction_final4$Date <- format(as.Date(as.yearmon(prediction_final4$Date)),format = "%B-%Y")
        prediction_final4$`Prediction(Sales)` <- round(prediction_final4$`Prediction(Sales)`, 0)
        df$nn <- prediction_final4
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=12)
        prediction_final5<- sw_sweep(ets_forecast, fitted = F, rename_index = "Date")
        prediction_final5<- prediction_final5 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final5)<-c("Date","Prediction(Sales)")
        prediction_final5$Date <- format(as.Date(as.yearmon(prediction_final5$Date)),format = "%B-%Y")
        prediction_final5$`Prediction(Sales)` <- round(prediction_final5$`Prediction(Sales)`, 0)
        df$ets <- prediction_final5
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    if(!is.null(input$select_m_region) && !is.null(input$select_m_store1) && 
       !is.null(input$select_m_item1)){
      
      train = train %>% filter(region %in% input$select_m_region &
                                 store %in% input$select_m_store1 &
                                 item %in% input$select_m_item1) %>%
        subset(select = c(Month,sales))
      train = aggregate(train$sales, by=list(train$Month), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,48)
      y_test = tail(train,12)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- ts(y_train$y, start = 2013, end = c(2016,12), frequency = 12)
      validation <- ts(y_test$y, start = 2017, frequency = 12)
      
      
      if(arima_flag == 1){
        arima_optimal <- auto.arima(training)
        arima_forecast <- forecast::forecast(arima_optimal, h=12)
        prediction_final1<- sw_sweep(arima_forecast, fitted = F, rename_index = "Date")
        prediction_final1<- prediction_final1 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final1)<-c("Date","Prediction(Sales)")
        prediction_final1$Date <- format(as.Date(as.yearmon(prediction_final1$Date)),format = "%B-%Y")
        prediction_final1$`Prediction(Sales)` <- round(prediction_final1$`Prediction(Sales)`, 0)
        df$arima <- prediction_final1
        df$ara <- MAPE(arima_forecast$mean, validation) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=12)
        prediction_final2<- sw_sweep(tbats_forecast, fitted = F, rename_index = "Date")
        prediction_final2<- prediction_final2 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final2)<-c("Date","Prediction(Sales)")
        prediction_final2$Date <- format(as.Date(as.yearmon(prediction_final2$Date)),format = "%B-%Y")
        prediction_final2$`Prediction(Sales)` <- round(prediction_final2$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final2
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=12)
        prediction_final3<- sw_sweep(naive, fitted = F, rename_index = "Date")
        prediction_final3<- prediction_final3 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final3)<-c("Date","Prediction(Sales)")
        prediction_final3$Date <- format(as.Date(as.yearmon(prediction_final3$Date)),format = "%B-%Y")
        prediction_final3$`Prediction(Sales)` <- round(prediction_final3$`Prediction(Sales)`, 0)
        df$naive <- prediction_final3
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=12)
        prediction_final4<- sw_sweep(fc1, fitted = F, rename_index = "Date")
        prediction_final4<- prediction_final4 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final4)<-c("Date","Prediction(Sales)")
        prediction_final4$Date <- format(as.Date(as.yearmon(prediction_final4$Date)),format = "%B-%Y")
        prediction_final4$`Prediction(Sales)` <- round(prediction_final4$`Prediction(Sales)`, 0)
        df$nn <- prediction_final4
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=12)
        prediction_final5<- sw_sweep(ets_forecast, fitted = F, rename_index = "Date")
        prediction_final5<- prediction_final5 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final5)<-c("Date","Prediction(Sales)")
        prediction_final5$Date <- format(as.Date(as.yearmon(prediction_final5$Date)),format = "%B-%Y")
        prediction_final5$`Prediction(Sales)` <- round(prediction_final5$`Prediction(Sales)`, 0)
        df$ets <- prediction_final5
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }   
    if(!is.null(input$select_m_region) && is.null(input$select_m_store1) && 
       !is.null(input$select_m_item1)){
      
      train = train %>% filter(region %in% input$select_m_region &
                                 item %in% input$select_m_item1) %>%
        subset(select = c(Month,sales))
      train = aggregate(train$sales, by=list(train$Month), FUN=sum)
      colnames(train) = c("ds","y")
      df$mon <- train
      y_train = head(train,48)
      y_test = tail(train,12)
      colnames(y_test) = c("ds","y")
      colnames(y_train)<-c("ds","y")
      training <- ts(y_train$y, start = 2013, end = c(2016,12), frequency = 12)
      validation <- ts(y_test$y, start = 2017, frequency = 12)
      
      if(arima_flag == 1){
        arima_optimal <- auto.arima(training)
        arima_forecast <- forecast::forecast(arima_optimal, h=12)
        prediction_final1<- sw_sweep(arima_forecast, fitted = F, rename_index = "Date")
        prediction_final1<- prediction_final1 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final1)<-c("Date","Prediction(Sales)")
        prediction_final1$Date <- format(as.Date(as.yearmon(prediction_final1$Date)),format = "%B-%Y")
        prediction_final1$`Prediction(Sales)` <- round(prediction_final1$`Prediction(Sales)`, 0)
        df$arima <- prediction_final1
        df$ara <- MAPE(arima_forecast$mean, validation) * 100
      }
      if(tbats_flag == 1){
        tbats_model <- tbats(training)
        tbats_forecast <- forecast(tbats_model, h=12)
        prediction_final2<- sw_sweep(tbats_forecast, fitted = F, rename_index = "Date")
        prediction_final2<- prediction_final2 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final[, c("Date","y")],365)
        colnames(prediction_final2)<-c("Date","Prediction(Sales)")
        prediction_final2$Date <- format(as.Date(as.yearmon(prediction_final2$Date)),format = "%B-%Y")
        prediction_final2$`Prediction(Sales)` <- round(prediction_final2$`Prediction(Sales)`, 0)
        df$tbats <- prediction_final2
        df$tba <- MAPE(tbats_forecast$mean, validation) * 100
      }
      if(naive_flag == 1){
        naive <- snaive(training, h=12)
        prediction_final3<- sw_sweep(naive, fitted = F, rename_index = "Date")
        prediction_final3<- prediction_final3 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final3)<-c("Date","Prediction(Sales)")
        prediction_final3$Date <- format(as.Date(as.yearmon(prediction_final3$Date)),format = "%B-%Y")
        prediction_final3$`Prediction(Sales)` <- round(prediction_final3$`Prediction(Sales)`, 0)
        df$naive <- prediction_final3
        df$naa <- MAPE(naive$mean, validation) * 100
      }
      if(nn_flag == 1){
        nn1 <- nnetar(training)
        fc1 <- forecast(nn1,h=12)
        prediction_final4<- sw_sweep(fc1, fitted = F, rename_index = "Date")
        prediction_final4<- prediction_final4 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final4)<-c("Date","Prediction(Sales)")
        prediction_final4$Date <- format(as.Date(as.yearmon(prediction_final4$Date)),format = "%B-%Y")
        prediction_final4$`Prediction(Sales)` <- round(prediction_final4$`Prediction(Sales)`, 0)
        df$nn <- prediction_final4
        df$nna <- MAPE(fc1$mean, validation) * 100
      }
      if(ets_flag == 1){
        ets_model <- ets(training, allow.multiplicative.trend = TRUE)
        ets_forecast <- forecast(ets_model, h=12)
        prediction_final5<- sw_sweep(ets_forecast, fitted = F, rename_index = "Date")
        prediction_final5<- prediction_final5 %>% select(Date, value)
        # prediction_final<-  xts::last(prediction_final1[, c("Date","y")],365)
        colnames(prediction_final5)<-c("Date","Prediction(Sales)")
        prediction_final5$Date <- format(as.Date(as.yearmon(prediction_final5$Date)),format = "%B-%Y")
        prediction_final5$`Prediction(Sales)` <- round(prediction_final5$`Prediction(Sales)`, 0)
        df$ets <- prediction_final5
        df$eta <- MAPE(ets_forecast$mean, validation) * 100
      }
    }
    return(df)
  })
  
  observe({
    upd_df <- subset(train,region %in% input$select_m_region)
    upd <- unique(upd_df[['store']])
    updatePickerInput(session = session, 
                      inputId = "select_m_store1",
                      choices = upd)
  })
  
  
  
  output$accuracy_m_plot <- renderGirafe({
    m_df<-m_datasets()
    arima_flag1=0
    tbats_flag1=0
    naive_flag1=0
    nn_flag1=0
    ets_flag1=0
    if("1" %in% input$select_m_model)
      arima_flag1=1
    if("2" %in% input$select_m_model)
      tbats_flag1=1
    if("3" %in% input$select_m_model)
      naive_flag1=1
    if("4" %in% input$select_m_model)
      nn_flag1=1
    if("5" %in% input$select_m_model)
      ets_flag1=1
    
    acc_data <- data.frame(Models = character(0), MAPE = numeric(0), stringsAsFactors = F)
    
    if(arima_flag1==1){
      acc_data[nrow(acc_data)+1,]<-  c("ARIMA", round(m_df$ara,2))
    }
    if(tbats_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("TBATS", round(m_df$tba,2))
    }
    if(naive_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("NAIVE", round(m_df$naa,2))
    }
    if(nn_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("NNETAR", round(m_df$nna,2))
    }
    if(ets_flag1==1){
      acc_data[nrow(acc_data)+1,] <-  c("ETS", round(m_df$eta,2))
    }
    # %>% formatDate(1, "toLocaleDateString")
    accp<- ggplot(acc_data, aes(x=Models, y= MAPE, fill=Models)) +
      geom_bar_interactive(aes(x=Models, y= MAPE, fill=Models,
                               tooltip= MAPE, data_id=MAPE), 
                           stat="identity", width = 0.4)+
      theme_minimal()+
      theme(axis.title.x = element_blank())+
      scale_fill_brewer(palette="Dark2")+
      labs(title = "Mean Absolute Percentage Error (MAPE) value for Model evaluation")
    girafe(ggobj = accp, height_svg = 6.2,width_svg = 12.6,
           options = list(
             opts_sizing(rescale = F),
             opts_hover_inv(css = "opacity:0.1;"),
             opts_hover(css = ":autofill;"),
             opts_toolbar(position = 'bottomright')))
  })
  
  output$forecast_m_plot <- renderDygraph({
    m_df <- m_datasets()
    actual <- xts(x = m_df$mon$y,order.by = as.Date(as.yearmon(m_df$mon$ds, "%B-%Y")))
    
    arima_flag1=0
    tbats_flag1=0
    naive_flag1=0
    nn_flag1=0
    ets_flag1=0
    if("1" %in% input$select_m_model)
      arima_flag1=1
    if("2" %in% input$select_m_model)
      tbats_flag1=1
    if("3" %in% input$select_m_model)
      naive_flag1=1
    if("4" %in% input$select_m_model)
      nn_flag1=1
    if("5" %in% input$select_m_model)
      ets_flag1=1
    actual_value = actual
    
    if(arima_flag1 ==1){
      arima <- xts(x = xts::last(m_df$arima$`Prediction(Sales)`,12),
                   order.by = as.Date(as.yearmon(xts::last(m_df$arima$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,arima)
      
    }
    if(tbats_flag1 ==1){
      tbats <- xts(x = xts::last(m_df$tbats$`Prediction(Sales)`,12),
                   order.by = as.Date(as.yearmon(xts::last(m_df$tbats$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,tbats)
    }
    if(naive_flag1 ==1){
      naive <- xts(x = xts::last(m_df$naive$`Prediction(Sales)`,12),
                   order.by = as.Date(as.yearmon(xts::last(m_df$naive$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,naive)
    }
    if(nn_flag1 ==1){
      nnetar <- xts(x = xts::last(m_df$nn$`Prediction(Sales)`,12),
                    order.by = as.Date(as.yearmon(xts::last(m_df$nn$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,nnetar)
    }
    if(ets_flag1 ==1){
      ets <- xts(x = xts::last(m_df$ets$`Prediction(Sales)`,12),
                 order.by = as.Date(as.yearmon(xts::last(m_df$ets$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,ets)
    }
    actual_value <- tail(actual_value,24)
    dygraph(actual_value, main = "Sales Forecast") %>%
      dyOptions(fillGraph = F, fillAlpha = 0.4)%>%
      dyAxis("y", label = "Sales")%>%
      dyRangeSelector(height = 20) %>%
      dyShading(from = "2017-01-01", to = "2017-12-31", color = "#ccccff") %>%
      dyShading(from = "2013-01-01", to = "2016-12-31", color = "#ebebfc")
    
    
  })
  
  output$forecast_m_table <- renderDataTable({
    m_df <- m_datasets()
    actual <- xts(x = m_df$mon$y,order.by = as.Date(as.yearmon(m_df$mon$ds, "%B-%Y")))
    arima_flag1=0
    tbats_flag1=0
    naive_flag1=0
    nn_flag1=0
    ets_flag1=0
    if("1" %in% input$select_m_model)
      arima_flag1=1
    if("2" %in% input$select_m_model)
      tbats_flag1=1
    if("3" %in% input$select_m_model)
      naive_flag1=1
    if("4" %in% input$select_m_model)
      nn_flag1=1
    if("5" %in% input$select_m_model)
      ets_flag1=1
    actual_value = actual
    if(arima_flag1 ==1){
      arima <- xts(x = xts::last(m_df$arima$`Prediction(Sales)`,12),
                   order.by = as.Date(as.yearmon(xts::last(m_df$arima$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,arima)
    }
    if(tbats_flag1 ==1){
      tbats <- xts(x = xts::last(m_df$tbats$`Prediction(Sales)`,12),
                   order.by = as.Date(as.yearmon(xts::last(m_df$tbats$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,tbats)
    }
    if(naive_flag1 ==1){
      naive <- xts(x = xts::last(m_df$naive$`Prediction(Sales)`,12),
                   order.by = as.Date(as.yearmon(xts::last(m_df$naive$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,naive)
    }
    if(nn_flag1 ==1){
      nnetar <- xts(x = xts::last(m_df$nn$`Prediction(Sales)`,12),
                    order.by = as.Date(as.yearmon(xts::last(m_df$nn$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,nnetar)
    }
    if(ets_flag1 ==1){
      ets <- xts(x = xts::last(m_df$ets$`Prediction(Sales)`,12),
                 order.by = as.Date(as.yearmon(xts::last(m_df$ets$Date,12), "%B-%Y")))
      actual_value = cbind(actual_value,ets)
    }
    actual_value <- tail(actual_value,12)
    df3 = data.frame(date=index(actual_value), coredata(actual_value))
    data_table <- df3
    # data_table<-  xts::last(data_table,365)
    DT::datatable(data_table,rownames=F) %>% formatDate(
      columns = 1, 
      method =  "toLocaleDateString", 
      params = list(
        'en-GB', 
        list(
          year = 'numeric', 
          month = 'numeric')
      )
    )
  })
  
  
  
}
