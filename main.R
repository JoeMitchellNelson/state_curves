# Load in packages

require(pacman)

p_load(ggplot2,tidyverse,ggpubr,tidycensus,lubridate,patchwork,extrafont,sf,png,ggthemes)

font_import()
loadfonts(device="win")

# see a list of fonts. 
# fonts()  

#census_api_key("YOUR-KEY-HERE",install=T)

census_data <- get_acs(geography = "state",
                       variables=c(total="B01003_001"), # total population count
                       geometry=T,                      # pull the shapefiles for maps
                       shift_geo=T)





# read in daily state data from nyt
nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
nyt$date <- ymd(nyt$date)
nyt <- nyt %>% dplyr::filter(date >= ymd("2020-03-01"))
nyt <- nyt %>% dplyr::filter(state %in% state.name)

# states don't appear in nyt data until they have a first case, so record 0s rather than missing entries in those cases
nyt2 <- expand.grid(date=unique(nyt$date),state=unique(nyt$state)) 
nyt <- left_join(nyt2,nyt)
nyt <- nyt %>% replace_na(list(cases=0))
nyt <- nyt %>% replace_na(list(deaths=0))
rm(nyt2)

# going to calculate 7-day moving averages. 
# for different length averages, change the value of ma_length.
ma_length <- 7
nyt3 <- nyt %>% mutate(date2 = date + int_length(ma_length))
nyt <- left_join(nyt,nyt3,by=c("state"="state","date"="date2"))
nyt$cases = nyt$cases.x - nyt$cases.y
nyt$deaths = nyt$deaths.x - nyt$deaths.y
nyt$ma = nyt$cases/ma_length
nyt$ma_death = nyt$deaths/ma_length
nyt <- nyt %>% dplyr::filter(date >= ymd("2020-03-15"))

# want cases per 100,000 so multiply the numbers by 100000 and divide by population

census_small <- census_data %>% as.data.frame() %>% dplyr::select(NAME,estimate)
nyt <- left_join(nyt,census_small,by=c("state"="NAME"))
nyt <- nyt %>% mutate(ma_pc = (ma * 100000) / estimate)
nyt <- nyt %>% mutate(ma_pc_death = (ma_death * 100000) / estimate)
nyt <- nyt %>% dplyr::select(date,state,ma_pc,ma_pc_death)


# calculate the number of days since the peaks
# of deaths and cases
peak <- data.frame(state=state.name,case_peak=NA,death_peak=NA,case_ratio=NA,death_ratio=NA)


# calculate moving average for cases and deaths

for (i in 1:50) {
  temp <- nyt %>% dplyr::filter(state==state.name[i])
  cases_date <- temp %>% dplyr::filter(ma_pc == max(ma_pc)) %>% dplyr::filter(date==max(date))
  cases_date <- cases_date$date
  deaths_date <- temp %>% dplyr::filter(ma_pc_death == max(ma_pc_death)) %>% dplyr::filter(date==max(date))
  deaths_date <- deaths_date$date
  max_date <- max(temp$date)
  case_lag <- int_length(interval(cases_date,max_date))/86400
  death_lag <- int_length(interval(deaths_date,max_date))/86400
  peak$case_peak[i] <- case_lag
  peak$death_peak[i] <- death_lag
  
  # also calculate how bad this period was compared to worst period so far
  
  this_pd_cases <- temp$ma_pc[which(temp$date==max(temp$date))] %>% unique
  this_pd_deaths <- temp$ma_pc_death[which(temp$date==max(temp$date))] %>% unique
  worst_pd_cases <- temp$ma_pc[which(temp$ma_pc==max(temp$ma_pc))] %>% unique
  worst_pd_deaths <- temp$ma_pc_death[which(temp$ma_pc_death==max(temp$ma_pc_death))] %>% unique
  
  peak$case_ratio[i] <- this_pd_cases/worst_pd_cases
  peak$death_ratio[i] <- this_pd_deaths/worst_pd_deaths
  
}

census_data <- left_join(census_data,peak,by=c("NAME"="state"))

# create the 50 individual plots, store as named objects (named by state abbrev) for patchwork to use

for (i in 1:50) {

state_name <- state.name[i]
state_name2 <- gsub(" ", "", state_name, fixed = TRUE)

ggplot(census_data[which(census_data$NAME==state_name),]) +
  geom_sf(aes(fill=death_ratio),color=NA,show.legend = F) +
  scale_fill_viridis_c(option="plasma",
                       limits=c(0,1),
                       alpha=.3,
                       direction=1) +
  theme_map()


ggsave(paste0("~/state_curves/state_img/",state_name2,"_outline.png"),last_plot(),width=3,height=3,units="in")

img.file <- system.file("~/state_curves/state_img/",state_name2,"_outline.png",
                        package = "ggpubr")
img <- png::readPNG(paste0("~/state_curves/state_img/",state_name2,"_outline.png"))



temp <- ggplot(nyt[which(nyt$state==state_name),]) +
  background_image(img)+
#  geom_hline(yintercept=0,size=.5,color="grey90") +
  # annotate("text",label = state.abb[which(state.name==state_name)],
  #          size=30,
  #          family="Book Antiqua",
  #          color="grey30",
  #          alpha=.3,
  #          x=ymd("2020-04-05"),
  #          y=mean(c(0,max(nyt[which(nyt$state==state_name),]$ma_pc_death)))) +
  
annotate("text",label = state.abb[which(state.name==state_name)],
         size=30,
         family="Book Antiqua",
         color="grey30",
         alpha=.3,
         x=ymd("2020-04-05"),
         y=mean(c(0,max(nyt$ma_pc_death)))) +

  geom_point(aes(x=date,y=ma_pc_death)) +
  geom_smooth(aes(x=date,y=ma_pc_death),method="lm",formula=y ~ poly(x, 7,raw=F),se=F,color="red") +
  labs(x="",
       y=paste0("Daily deaths per 100k (",ma_length,"-day moving average)")) +
     #  y="") +
 # lims(y=c(0,max(nyt[which(nyt$state==state_name),]$ma_pc_death))) +
  lims(y=c(0,max(nyt$ma_pc_death))) +
  theme(axis.title = element_text(family="Book Antiqua",color="grey70",size=10),
        axis.text = element_text(family="Book Antiqua",size=12),
        axis.line = element_line(color=NA))

assign(state.abb[i],temp)

}

layout <- "
A#########B
#####C###DE
FGHIJKL#MN#
OPQRSTUVWXY
"
top <- AK + ME + 
  WI + VT + NH +
  WA + ID + MT + ND + MN + IL + MI + NY + MA +
  OR + NV + WY + SD + IA + IN + OH + PA + NJ + CT + RI +
  plot_layout(design = layout)

ggsave("~/state_curves/top.png",last_plot(),width=43,height=16,units="in")


layout2 <- "
ABCDEFGHIJ#
#KLMNOPQ###
###RSTUV###
W##X####Y##
"

bottom <- CA + UT + CO + NE + MO + KY + WV + VA + MD + DE +
  AZ + NM + KS + AR + TN + NC + SC +
  OK + LA + MS + AL + GA +
  HI + TX + FL +
  plot_layout(design = layout2)

 
ggsave("~/state_curves/bottom.png",last_plot(),width=43,height=16,units="in")



top / bottom + plot_annotation(title=paste0("Daily Covid-19 deaths per 100,000 people in each state\n(",ma_length,"-day moving average)\n"),
                               caption="Source: New York Times and US Census Bureau        \nMeasures of Covid-19 deaths subject to testing capacity,        \nwhich may vary by state and over time        \nNote that the y-axis scale differs for each state        \nRed fitted line is 7th-degree polynomial        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 

ggsave("~/state_curves/big2.png",last_plot(),width=43,height=35,units="in")

yaxis <- ggplot() +
  labs(y=paste0("Deaths per 100,000 per day\n(",ma_length,"-day moving average)\n")) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size=50,family="Book Antiqua"))
yaxis
ggsave("~/state_curves/yaxis.png",last_plot(),width=2.5,height=32,units="in")
