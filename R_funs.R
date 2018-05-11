
pacman::p_load(dplyr,
               magrittr,
               rvest,
               doParallel,
               stringr,
               lubridate,
               data.table,
               ggplot2,
               scales,
               ggthemes,
               reshape2
               )
  
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

csv_dates = markets_raw %>%
  html_nodes(xpath="/html/body/pre/text()") %>%
  html_text() %>%
  str_extract("\\d\\d-[a-zA-z].+-\\d\\d\\d\\d \\d\\d:\\d\\d")

tibble(csv_dates,csv_markets)
  
}


###Active Markets List

active_markets = function(){
  
  a_markets=read_html("https://bitcoincharts.com/markets/list/") %>%
    html_nodes("body > div.container_16.content.padding > div > div.grid_7.alpha > ul") %>% html_text() %>% str_replace_all("\\n","") %>% str_replace_all("\\s+"," ") %>% str_trim()
  a_currency = read_html("https://bitcoincharts.com/markets/list/") %>%
    html_nodes("body > div.container_16.content.padding > div > div.grid_7.alpha > h3") %>% html_text()
  
  clean_nams = function(x) gsub(")$","",trimws(unlist(str_split(x,"\\(|\\)\\s"))))
  by_pos = function(x,by,pos) x[seq(pos,length(x),by)]
  
  DF = tibble(a_currency,a_markets) %>% rowwise()  %>% mutate(nam1=list(by_pos(clean_nams(a_markets),2,1)),nam2=list(by_pos(clean_nams(a_markets),2,2)))
  
  DF
  
}

active_m = active_markets() %>% filter(grepl("USD",a_currency))

csv_files = csvs()
sub = gsub(".csv.gz","",csv_files$csv_markets) %in% unlist(active_m$nam1)
markets_sub = csv_files$csv_markets[sub]

###Download and Binding Data - around 100 millions rows
### It takes a few minutes

cl <- makePSOCKcluster(max(parallel::detectCores()-1,1))
doParallel::registerDoParallel(cl)
bitcoin_merged = foreach(i = 1:length(markets_sub),.combine = "rbind") %dopar%
{
  my_data = tryCatch(readr::read_csv(paste0("http://api.bitcoincharts.com/v1/csv/",markets_sub[i]),col_names = c("time","price","vol"),col_types =  "idd"),
                     error= function(e) data.frame(time=NA,price=NA,vol=NA))
  
  my_data$name = gsub(".csv.gz","",markets_sub[i])
  my_data$time = as.POSIXct(my_data$time,origin = "1970-01-01")
  my_data
}
stopCluster(cl)

saveRDS(bitcoin_merged,paste0("bitcoinUSD_",Sys.Date(),".Rds"))

#bitcoin_merged = readRDS("bitcoinUSD_2018-05-12.Rds")

###Aggregating - to x interval
setDT(bitcoin_merged)

bitcoin_agg  = bitcoin_merged[,c('time_YmdH') := list(lubridate::floor_date(time,"hour"))] %>%
  .[,.(
  #price_max=max(price,na.rm=T),
  #price_min = min(price,na.rm=T),
  price_mean = mean(price,na.rm=T),
  price_first = first(price),
  price_last = last(price),
  vol_sum = sum(vol,na.rm=T)),
  #vol_max = max(vol,na.rm=T)),
  by=list(name,time_YmdH)]

bitcoin_agg[,ret_fl := (log(price_last)-log(price_first)),by=list(name)]

rm(bitcoin_merged)

bitcoin_spread = dcast(bitcoin_agg[,.(time_YmdH,name,ret_fl)],time_YmdH ~ name,value.var="ret_fl")

###Deleting markets with high NA ratio

low_NA = colnames(bitcoin_spread)[sort(apply(bitcoin_spread,2,function(x) sum(is.na(x))/length(x)),index.return=T)$ix[1:7]]

#bitcoin_spread = na.omit(bitcoin_spread[,low_NA,with=F])

frams = bitcoin_agg[name %in% low_NA,.(min_t=min(time_YmdH),max_t=max(time_YmdH)),by=.(name)][,.(min=max(min_t),max=min(max_t))]

dat = bitcoin_agg[time_YmdH>=frams$min & time_YmdH<frams$max & name %in% low_NA,,][order(time_YmdH)]

g1 = ggplot(dat,
            aes(x=time_YmdH,y=price_mean,color=name))  + 
  ggthemes::theme_tufte()+
  geom_line(size=2) + 
  scale_x_datetime(breaks = date_breaks("3 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  ggtitle("Bitcoin prices at active and most volatile Bitcoin USD markets - by hour")

g2 = ggplot(dat,
            aes(x=time_YmdH,y=ret_fl,color=name))  + 
  ggthemes::theme_tufte()+
  geom_line(size=2) + 
  scale_x_datetime(breaks = date_breaks("3 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  labs(title = "Bitcoin returns at active and most volatile Bitcoin USD markets - by hour") 

ggsave(g1,"Prices_USD.png")
ggsave(g2,"Returns_USD.png")
