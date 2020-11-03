suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(reshape2))
suppressWarnings(library(tidymodels))
suppressWarnings(library(modeltime))
suppressWarnings(library(tidyverse))
suppressWarnings(library(lubridate))
suppressWarnings(library(timetk))
suppressWarnings(library(timeDate))
con <- dbConnect(odbc::odbc(), "Azure_test", uid = "samddiscuser", 
                 pwd = "Yx#5qiZA#SMB", timeout = 10)
data<-dbGetQuery(con,"select * from scdev.sctemp.md_marc_mlc_rpt_ob_summary")

## latest data should be tuesday 
data_orderlines<-data %>% group_by(date) %>% 
  summarise(order_lines=sum(order_lines),pick_units=sum(pick_units)) %>% 
  select(date,order_lines) %>% 
  mutate(day= wday(date,label = TRUE))%>% 
  filter(date>"2017-10-01"& date<Sys.Date()-1 &day!=c("Sat")) %>% 
  set_names(c("date","value","day")) %>% 
  mutate(value=as.numeric(value) )%>% 
  select(date,value)

data_pickunits<-data %>% group_by(date) %>% 
  summarise(order_lines=sum(order_lines),pick_units=sum(pick_units)) %>% 
  select(date,pick_units) %>% 
  mutate(day= wday(date,label = TRUE))%>% 
  filter(date>"2017-10-01"& date<Sys.Date()-1 &day!=c("Sat")) %>% 
  set_names(c("date","value","day")) %>% 
  mutate(value=as.numeric(value) )%>% 
  select(date,value)

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date), ordered = F),
      data = data_orderlines)



calibrate_orderlines <- model_fit_arima_boosted %>% 
  modeltime_calibrate(new_data = data_orderlines)

## forecast should be Tuesday +13 days (only weekdays)
#so first forecast is a Weds and last day friday 
forecast_table_orderlines<-calibrate_orderlines %>% 
  modeltime_forecast(h=13)



model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date), ordered = F),
      data = data_pickunits)


calibrate_pickunits <- model_fit_arima_boosted %>% 
  modeltime_calibrate(new_data = data_pickunits)

## forecast should be Tuesday +13 days (only weekdays)
#so first forecast is a Weds and last day friday 
forecast_table_pick_units<-calibrate_pickunits %>% 
  modeltime_forecast(h=13)


##sys.date needs to be run on thursday 
prediction_dates<-c(Sys.Date() + c(-1:15))
prediction_dates <- prediction_dates[!weekdays(prediction_dates) %in% c('Saturday','Sunday')]


forecast_table_orderlines<-forecast_table_orderlines %>% select(.index,.value) %>%  set_names(c("forecast_date","forecast_order_lines")) %>% 
  mutate(forecast_date=prediction_dates ) %>% mutate(run_date=Sys.Date())

forecast_table_pick_units<-forecast_table_pick_units %>% select(.index,.value) %>%  set_names(c("forecast_date","forecast_pick_units")) %>% 
  mutate(forecast_date=prediction_dates ) %>% mutate(run_date=Sys.Date())

final_table<- full_join(forecast_table_orderlines,forecast_table_pick_units, by="forecast_date")

final_table <- final_table %>% select(-run_date.y) %>% rename(captured_date=run_date.x) %>% melt(c("forecast_date","captured_date")) %>% 
  rename(forecast_uom = (variable)) %>% mutate(forecast_uom=as.character(forecast_uom))

dbAppendTable(con, SQL("sctemp.md_distribution_mlc_calc_forecast"), final_table)


#dbWriteTable(con, SQL("sctemp.md_distribution_mlc_calc_forecast"), final_table)
dbGetQuery(con, "select * from sctemp.md_distribution_mlc_calc_forecast")



