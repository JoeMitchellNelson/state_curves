# Load in packages

require(pacman)

p_load(ggplot2,tidyverse,ggpubr,tidycensus,lubridate,patchwork,extrafont,sf,png,ggthemes,mosaic,lfe,broom)

### pull testing data from COVID-19 Tracking Project

tests <- read.csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv") %>% 
  dplyr::select(date,state,positive,negative,totalTestResultsIncrease,negativeIncrease,positiveIncrease)
tests$date <- ymd(tests$date)
tests <- tests %>% dplyr::filter(state %in% state.abb)
# set this parameter to control the moving average


font_import()
loadfonts(device="win")

# see a list of fonts. 
# fonts()  

#census_api_key("YOUR-KEY-HERE",install=T)

census_data <- get_acs(geography = "state",
                       variables=c(total="B01003_001"), # total population count
                       geometry=T,                      # pull the shapefiles for maps
                       shift_geo=T)

census_small <- census_data %>% as.data.frame() %>% dplyr::select(NAME,estimate)
names(census_small) <- c("state","pop")
census_small <- census_small %>% dplyr::filter(state %in% state.name)


for (i in 1:nrow(census_small)) {
  census_small$state[i] <- state.abb[which(state.name==census_small$state[i])]
}

tests <- left_join(tests,census_small)
tests <- tests %>% mutate(pos_ratio = positiveIncrease/totalTestResultsIncrease)

today <- max(tests$date)
tests_recent <- tests %>% dplyr::filter(date >= today - int_length(13))

# a few (three) observations have negative values for totalTestResultsIncrease
# just throw those out

tests_recent <- tests_recent %>% dplyr::filter(totalTestResultsIncrease >= 0)

###########
###ratio###
###########

for (i in 1:50) {
  
  state_name <- state.name[i]
  state_abb <- state.abb[i]
  
  temp_dat <- tests_recent[which(tests_recent$state==state_abb),]
  
  # a handful of total test numbers are negative, replace with 0s
  
  temp_dat$wt <- ifelse(temp_dat$totalTestResultsIncrease > 0,temp_dat$totalTestResultsIncrease, 0)
  
  #run weighted OLS to get the state's color
  
  res <- lm(pos_ratio ~ date,data=temp_dat) %>% tidy()
  est <- res$estimate[which(res$term=="date")]
  sig <- res$p.value[which(res$term=="date")]
  
  col <- ifelse(est<0 & sig <= .2,"green","yellow")
  col <- ifelse(est > 0 & sig <= .2,"red",col)

  state_name2 <- gsub(" ", "", state_name, fixed = TRUE)
  
  ggplot(census_data[which(census_data$NAME==state_name),]) +
    geom_sf(fill=col,color=NA,alpha=.5,show.legend = F) +
    theme_map()
  
  
  ggsave(paste0("~/state_curves/state_img/",state_name2,"_outline.png"),last_plot(),width=3,height=3,units="in")
  
  img.file <- system.file("~/state_curves/state_img/",state_name2,"_outline.png",
                          package = "ggpubr")
  img <- png::readPNG(paste0("~/state_curves/state_img/",state_name2,"_outline.png"))
  

  
  
  
  temp <- ggplot(temp_dat) +
    background_image(img) +
    
    annotate("text",label = state_abb,
             size=30,
             family="Book Antiqua",
             color="grey30",
             alpha=.6,
             x=today-int_length(6),
             y=mean(c(min(temp_dat$pos_ratio,na.rm=T),max(temp_dat$pos_ratio,na.rm=T)))) +
    
    geom_point(aes(x=date,y=pos_ratio),size=3,show.legend = F) +
    geom_smooth(aes(x=date,y=pos_ratio,weight=wt),method="lm",se=F,color="blue",size=2) +
    
    labs(x="",
         y=paste0("Ratio of positive tests to total tests")) +
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



top / bottom + plot_annotation(title=paste0("Positive covid-19 tests (proportion)\n"),
                               caption="Source: Covid-19 Tracking Project and US Census Bureau        \nPoint sizes indicate total tests        \nBest fit line weighted by number of cases per day        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 
ggsave("~/state_curves/ratio.png",last_plot(),width=43,height=35,units="in")

###############
#### tests ####
###############

for (i in 1:50) {
  
  state_name <- state.name[i]
  state_abb <- state.abb[i]
  
  temp_dat <- tests_recent[which(tests_recent$state==state_abb),]
  temp_dat$wt <- ifelse(temp_dat$totalTestResultsIncrease > 0,temp_dat$totalTestResultsIncrease, 0)
  
  res <- lm(totalTestResultsIncrease ~ date,data=temp_dat) %>% tidy()
  est <- res$estimate[which(res$term=="date")]
  sig <- res$p.value[which(res$term=="date")]
  
  col <- ifelse(est<0 & sig <= .2,"red","green")

  state_name2 <- gsub(" ", "", state_name, fixed = TRUE)
  
  ggplot(census_data[which(census_data$NAME==state_name),]) +
    geom_sf(fill=col,color=NA,alpha=.5,show.legend = F) +
    theme_map()
  
  
  ggsave(paste0("~/state_curves/state_img/",state_name2,"_outline.png"),last_plot(),width=3,height=3,units="in")
  
  img.file <- system.file("~/state_curves/state_img/",state_name2,"_outline.png",
                          package = "ggpubr")
  img <- png::readPNG(paste0("~/state_curves/state_img/",state_name2,"_outline.png"))
  
  
  
  
  
  temp <- ggplot(tests[which(tests$state==state_abb & tests$date >= today - int_length(13)),]) +
    background_image(img) +
    
    annotate("text",label = state_abb,
             size=30,
             family="Book Antiqua",
             color="grey30",
             alpha=.6,
             x=today - int_length(6),
             y=max(tests$totalTestResultsIncrease[which(tests$state==state_abb)],na.rm=T)/2) +
    
    geom_point(aes(x=date,y=totalTestResultsIncrease)) +
    geom_smooth(data = temp_dat,aes(x=date,y=totalTestResultsIncrease),method="lm",se=F,color="blue",size=2) +
    #  geom_point(aes(x=date,y=pos_ma_pc*5)) +
    #  geom_smooth(aes(x=date,y=pos_ma_pc*5),method="lm",formula=y ~ poly(x, 7,raw=F),se=F,color="blue") +
    
    labs(x="",
         y=paste0("Number of tests per day")) +
    #  y="") +
    lims(y=c(0,max(tests$totalTestResultsIncrease[which(tests$state==state_abb)],na.rm=T))) +
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



top / bottom + plot_annotation(title=paste0("Total Covid-19 tests processed per day\n"),
                               caption="Source: Covid-19 Tracking Project and US Census Bureau        \n        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 
ggsave("~/state_curves/total.png",last_plot(),width=43,height=35,units="in")

#############
####cases####
#############

for (i in 1:50) {
  
  state_name <- state.name[i]
  state_abb <- state.abb[i]
  
  temp_dat <- tests_recent[which(tests_recent$state==state_abb),]
  temp_dat$wt <- ifelse(temp_dat$totalTestResultsIncrease > 0,temp_dat$totalTestResultsIncrease, 0)
  
  
  res <- lm(positiveIncrease ~ date,data=temp_dat,weights=wt) %>% tidy()
  est <- res$estimate[which(res$term=="date")]
  sig <- res$p.value[which(res$term=="date")]
  
  col <- ifelse(est<0 & sig <= .2,"green","yellow")
  col <- ifelse(est > 0 & sig <= .2,"red",col)
  
  state_name2 <- gsub(" ", "", state_name, fixed = TRUE)
  
  ggplot(census_data[which(census_data$NAME==state_name),]) +
    geom_sf(fill=col,color=NA,alpha=.5,show.legend = F) +
    theme_map()
  
  
  ggsave(paste0("~/state_curves/state_img/",state_name2,"_outline.png"),last_plot(),width=3,height=3,units="in")
  
  img.file <- system.file("~/state_curves/state_img/",state_name2,"_outline.png",
                          package = "ggpubr")
  img <- png::readPNG(paste0("~/state_curves/state_img/",state_name2,"_outline.png"))
  
  
  
  
  
  temp <- ggplot(temp_dat) +
    background_image(img) +
    
    annotate("text",label = state_abb,
             size=30,
             family="Book Antiqua",
             color="grey30",
             alpha=.6,
             x=today-int_length(6),
             y=mean(c(min(temp_dat$positiveIncrease,na.rm=T),max(temp_dat$positiveIncrease,na.rm=T)))) +
    
    geom_point(aes(x=date,y=positiveIncrease,size=wt),show.legend = F) +
    geom_smooth(aes(x=date,y=positiveIncrease,weight=wt),method="lm",se=F,color="blue",size=2) +
    
    labs(x="",
         y=paste0("Number of positive tests")) +
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



top / bottom + plot_annotation(title="Positive covid-19 tests by state\n",
                               caption="Source: Covid-19 Tracking Project and US Census Bureau        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 
ggsave("~/state_curves/cases.png",last_plot(),width=43,height=35,units="in")
