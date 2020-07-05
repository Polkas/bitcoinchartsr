
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
  my_data
}
stopCluster(cl)

bitcoin_merged$name = factor(bitcoin_merged$name)

#saveRDS(bitcoin_merged,"bitcoinUSD.Rds")

#bitcoin_merged = readRDS("bitcoinUSD_2018-05-14.Rds")


###Aggregating - to x interval

#bitcoin_merged = readRDS('bitcoinUSD.Rds')

setDT(bitcoin_merged)

bitcoin_merged[,c('ymd_hm','ymd_h'):=list(as.POSIXct(time-time%%60,origin="1970-01-01"),as.POSIXct(time-time%%(60*60),origin="1970-01-01"))]

bitcoin_agg  = bitcoin_merged %>%
  .[,.(
  price_max=max(price,na.rm=T),
  price_min = min(price,na.rm=T),
  price_mean = mean(price,na.rm=T),
  price_median = median(price,na.rm=T),
  price_first = first(price),
  price_last = last(price),
  vol_sum = sum(vol,na.rm=T),
  vol_max = max(vol,na.rm=T)),
  by=list(name,ymd_h)]

bitcoin_agg[,ret_median := (log(price_median)-lag(log(price_median))),by=list(name)]

#rm(bitcoin_merged)

bitcoin_spread = data.table::dcast(bitcoin_agg[,.(ymd_h,name,ret_median)],ymd_h ~ name,value.var="ret_median")

###Deleting markets with high NA ratio

low_NA = colnames(sort(bitcoin_spread[,lapply(.SD,function(x) sum(is.na(x))/length(x))]))[1:5]

#bitcoin_spread = na.omit(bitcoin_spread[,low_NA,with=F])

frams = bitcoin_agg[name %in% low_NA,.(min_t=min(ymd_h),max_t=max(ymd_h)),by=.(name)][,.(min=max(min_t),max=min(max_t))]

dat = bitcoin_agg[ymd_h>=frams$min & ymd_h<frams$max & name %in% low_NA,,][order(ymd_h)]

g1 = ggplot(dat,
            aes(x=ymd_h,y=price_median,col=name))  + 
  ggthemes::theme_tufte()+
  geom_point(size=1,alpha=0.1) + 
  scale_x_datetime(breaks = date_breaks("6 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "fix",ncol=2) + 
  ggtitle("Bitcoin prices at most volatile Bitcoin USD markets \n 1 hour interval")

g2 = ggplot(dat,
            aes(x=ymd_h,y=ret_median,col=name))  + 
  ggthemes::theme_tufte()+
  geom_point(size=1,alpha=0.3) + 
  scale_x_datetime(breaks = date_breaks("6 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  labs(title = "Bitcoin returns at most volatile Bitcoin USD markets \n 1 hour interval - free scale") 

g3 = ggplot(dat,
            aes(x=ymd_h,y=vol_sum,col=name))  + 
  ggthemes::theme_tufte()+
  geom_point(size=1,alpha=0.1) + 
  scale_x_datetime(breaks = date_breaks("6 month"),labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  facet_wrap(~name,scales = "free",ncol=2) + 
  ggtitle("Bitcoin volume at most volatile Bitcoin USD markets \n 1 hour interval - free scale")

ggsave("./png/Prices_USD.png",g1)
ggsave("./png/Returns_USD.png",g2)
ggsave("./png/Volume_USD.png",g3)
