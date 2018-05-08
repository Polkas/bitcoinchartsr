

pacman::p_load(rvest,
               tidyverse,
               stringr,
               lubridate,
               data.table,
               doParallel,
               parallel,
               scales,
               Hmisc,
               MSBVAR,
               ggthemes,
               rjags)


#Database was built from data offered by [**bitcoincharts**](https://bitcoincharts.com). First of all there was downloaded files in [**csv.gz**](http://api.bitcoincharts.com/v1/csv/) format which contain quotes from active USD Bitcoin markets. Procedure assume that csv files are frequently update so if files are change re-run of script will cause downloading of files with a new upload time-stamp. The certain csv file contains all transactions from a particular bitcoin market at specific time frame. There are 3 attributes describing each file - time-stamp,price and volume of every transaction. This files could be merged and/or aggregate by time-stamp. Aggregating could be helpful because of the big size of decompressed files but at expense of lossing information. Data could be aggregate by every minute,hour or day. Data aggregation with 1 hour interval was implemented. Then statistics like mean,max.min,first and/or last price was evaluated.


##Data

#[**blockexplorer**](https://blockexplorer.com/b/100000) is an API which allows downloading information from certain blocks. It could be used to gather all bitcoin transactions by web scarping subsequent blocks. However it is much faster and easier to use data already downloaded which are offered for free on the Internet.

#[**bitcoincharts**](https://bitcoincharts.com/markets/currency/USD.html) summarize prices on most important markets.
#At the same website we are able to find database of csv files of historic prices on different markets. Some markets are update constantly others with few months delay.

#Csv files with dates

csvs = function(){

markets_raw <- read_html("http://api.bitcoincharts.com/v1/csv/") 

csv_markets = markets_raw %>%
  html_nodes("a") %>%
  html_text() 

csv_dates_m = markets_raw %>%
  html_nodes(xpath="/html/body/pre/text()") %>%
  html_text() %>%
  str_extract("\\d\\d-[a-zA-z].+-\\d\\d\\d\\d \\d\\d:\\d\\d")

data.frame(csv_dates_m,csv_markets)
  
}


###Active Markets List

active_markets = function(curr=c('ARS','AUD','BRL','CAD','CHF','CLP','CZK','DKK','EUR','GBP','HKD','IDR',
                                 'ILS','INR','JPY','KRW','MXN','MYR','NGN','NOK','NZD','PKR','PLN','RUB',
                                 'SEK','SGD','SLL','THB','USD','VEF','VND','ZAR')){
  if(!tryCatch(get("active_BCC_logi"),error = function(e) FALSE)){
  a_markets=read_html("https://bitcoincharts.com/markets/list/") %>%
    html_nodes("body > div.container_16.content.padding > div > div.grid_7.alpha > ul") %>% html_text() %>% str_replace_all("\\n","") %>% str_replace_all("\\s+"," ") %>% str_trim()
  a_currency = read_html("https://bitcoincharts.com/markets/list/") %>%
    html_nodes("body > div.container_16.content.padding > div > div.grid_7.alpha > h3") %>% html_text()
  
  DT = data.table(data.frame(a_currency,a_markets))
  setkey(DT,"a_currency")
  DT[curr]
  assign("active_BCC",DT,envir = .GlobalEnv)
  assign("active_BCC_logi",TRUE,envir = .GlobalEnv)
  }
  get("active_BCC")
}

matrix(unlist(strsplit(as.character(active_markets()$a_markets)," \\(|\\) ")),ncol=2,byrow=T) %>% apply(2,function(x) str_replace_all(x,"\\)",""))


sub = gsub(".csv.gz","",csv_markets) %in% active_m
markets_sub = markets[sub]
dates_m_sub = as.Date(strftime(dmy_hm(csv_dates_m[sub]),"%Y-%m-%d"))

###Downloading and Delete old files after update
files_ls = list.files(paste0("~/bitcoinDB/"))
for(i in seq_along(markets_sub)){
  pos = str_detect(files_ls,markets_sub[i])
  file_name_old = files_ls[pos]
  file_name_new = paste0(dates_m_sub[i],"_",markets_sub[i])
  if( (!any(pos))){
    download.file(paste0("http://api.bitcoincharts.com/v1/csv/",markets_sub[i]),
                  paste0("~/bitcoinDB/",file_name_new))
  } else if( any(pos) & as.Date(strsplit(files_ls[pos],"_")[[1]][1]) < as.Date(dates_m_sub[i])){
    system(paste0("bash | rm /mnt/c/Users/user/Documents/bitcoinDB/",file_name_old))
    download.file(paste0("http://api.bitcoincharts.com/v1/csv/",markets_sub[i]),
                  paste0("~/bitcoinDB/",file_name_new))
  } 
} 

for(i in seq_along(markets_sub)){
  pos = str_detect(markets_sub,strsplit(files_ls[i],"_")[[1]][2])
  if(!any(pos)) system(paste0("bash | rm /mnt/c/Users/user/Documents/bitcoinDB/",files_ls[i]))
}


###File size

s = file.info(paste0("C:/Users/user/Documents/bitcoinDB/",list.files("C:/Users/user/Documents/bitcoinDB/")))
s_size = s$size
names(s_size) = basename(rownames(s))

markets = list.files("C:/Users/user/Documents/bitcoinDB/")

markets = markets[!markets %in% basename(rownames(s))[s$size<1000]]

markets_nam = markets %>% 
  str_replace_all("USD.csv.gz","") %>%
  strsplit("_")

markets_nam = do.call(rbind,markets_nam)[,2]

###Binding

cl <- makePSOCKcluster(max(parallel::detectCores()-1,1))
doParallel::registerDoParallel(cl)
bitcoin_merged = foreach(i = 1:length(markets),.combine = "rbind") %dopar%
{
  my_data = readr::read_csv(paste0("C:/Users/user/Documents/bitcoinDB/",markets[i]),col_names = c("time","price","vol"),col_types =  "idd") 
  my_data$name = markets_nam[i]
  my_data$time = as.POSIXct(my_data$time,origin = "1970-01-01")
  my_data
}
stopCluster(cl)


###Aggregating
bitcoin_agg = data.table(bitcoin_merged)
bitcoin_agg  = bitcoin_agg[,.(
  #price_max=max(price,na.rm=T),
  #price_min = min(price,na.rm=T),
  #price_mean = mean(price,na.rm=T),
  price_first = first(price),
  price_last = last(price),
  vol_sum = sum(vol,na.rm=T)),
  #vol_max = max(vol,na.rm=T)),
  by=list(time_YmdH =  floor_date(time,"minute"),name)]

bitcoin_agg[,ret_fl := (log(price_last)-log(price_first)),by=list(name)]

rm(bitcoin_merged)

bitcoin_spread = dcast(bitcoin_agg[,.(time_YmdH,name,ret_fl)],time_YmdH ~ name,value.var="ret_fl")


###Deleting markets with hidg NA ratio

low_NA = colnames(bitcoin_spread)[sort(apply(bitcoin_spread,2,function(x) sum(is.na(x))/length(x)),index.return=T)$ix[1:7]]

bitcoin_spread = na.omit(bitcoin_spread[,low_NA,with=F])

dat = bitcoin_agg[time_YmdH>=min(bitcoin_spread$time_YmdH) & time_YmdH<max(bitcoin_spread$time_YmdH) & name %in% low_NA,,][order(time_YmdH)]


##In most cases it is much easier to analyse returns not prices because of stationarity of a former. Example of a stationary process is the white noise process which probability distribution does not changed during time. The returns are calculated as log difference between first and last transaction price at a specific hour.
##List of six active and most volatile USD Bitcoin markets: `r toupper(low_NA[-1])`


g1 = ggplot(dat,
            aes(x=time_YmdH,y=price_last,color=name))  + 
  ggthemes::theme_tufte()+
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("3 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  ggtitle("Bitcoin prices at active and most volatile Bitcoin markets")

g2 = ggplot(dat,
            aes(x=time_YmdH,y=ret_fl,color=name))  + 
  ggthemes::theme_tufte()+
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("3 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  labs(title = "Bitcoin returns at active and most volatile Bitcoin markets - by hour") 

ggplot(dat,
       aes(x=time_YmdH,y=ret_fl,color=name))  + 
  ggthemes::theme_tufte()+
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("3 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  labs(title = "Bitcoin returns at active and most volatile Bitcoin markets - by hour") 

g2
