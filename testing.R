# This file has four sections (data merging, followed by one section each for the 3 maps). 
# You can use the titles below for ctrl-f searching.

# 1. prep the data
# 2. ratios
# 3. tests
# 4. cases


###########################
#### 1. prep the data #####
###########################

require(pacman)

p_load(ggplot2,tidyverse,ggpubr,tidycensus,lubridate,patchwork,extrafont,sf,png,ggthemes,mosaic,lfe,broom)

# load in fonts because the ggplot default
# is some basic ish
font_import()
loadfonts(device="win")
# run fonts() see a list of fonts. 


### pull testing data from COVID-19 Tracking Project
tests <- read.csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv") %>% 
  dplyr::select(date,state,positive,negative,totalTestResultsIncrease,negativeIncrease,positiveIncrease)
tests$date <- ymd(tests$date)
tests <- tests %>% dplyr::filter(state %in% state.abb)



# pull in shape files from the census
# and grab state populations in case I want to do some
# per capita stuff later

# gotta bring your own api key though
# census_api_key("YOUR-KEY-HERE",install=T)

census_data <- get_acs(geography = "state",
                       variables=c(total="B01003_001"), # total population count
                       geometry=T,                      # pull the shapefiles for maps
                       shift_geo=T)

census_small <- census_data %>% as.data.frame() %>% dplyr::select(NAME,estimate)
names(census_small) <- c("state","pop")
census_small <- census_small %>% dplyr::filter(state %in% state.name)

# make census state names match covid state names
for (i in 1:nrow(census_small)) {
  census_small$state[i] <- state.abb[which(state.name==census_small$state[i])]
}

# merge them suckers
tests <- left_join(tests,census_small)

# calculate percentage of tests that are positive for each state-day
tests <- tests %>% mutate(pos_ratio = positiveIncrease/totalTestResultsIncrease)

# filter out all but the last two weeks of data
today <- max(tests$date)
tests_recent <- tests %>% dplyr::filter(date >= today - int_length(13))

# a few (three) observations have negative values for totalTestResultsIncrease
# just throw those out
tests_recent <- tests_recent %>% dplyr::filter(totalTestResultsIncrease >= 0)

# data is ready to boogie
# The 3 sections below (ratio, tests, cases) create the three maps

###################
#### 2. ratios ####
###################

# loop makes the individual state plots

for (i in 1:50) {
  
  state_name <- state.name[i]
  state_abb <- state.abb[i]
  
  temp_dat <- tests_recent[which(tests_recent$state==state_abb),]
  
  # This section creates state silhouette
  # run OLS to get the state's color
  
  # running OLS unweighted. not sure that's the right call, but the 
  # white house guidelines are super vague 
  res <- lm(pos_ratio ~ date,data=temp_dat) %>% tidy()
  est <- res$estimate[which(res$term=="date")]
  sig <- res$p.value[which(res$term=="date")]
  
  # sig negative = green
  # sig positive = red
  # not sig = yellow
  # use significance level of .2
  
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
  

  # This section builds plot on top of state silhouette
  
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
    geom_smooth(aes(x=date,y=pos_ratio),method="lm",se=F,color="blue",size=2) +
    
    labs(x="",
         y=paste0("Ratio of positive tests to total tests")) +
    theme(axis.title = element_text(family="Book Antiqua",color="grey70",size=10),
          axis.text = element_text(family="Book Antiqua",size=12),
          axis.line = element_line(color=NA))
  
  assign(state.abb[i],temp)
  
}

# state plots all stored in environment under their abbreviations

# these chunks arrange the state plots into a map-like arrangement
# have to arrange top and bottom separately since patchwork can't handle
# more than 26 plots (plots notated by letters)

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

# ggsave("~/state_curves/top.png",last_plot(),width=43,height=16,units="in")


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


# ggsave("~/state_curves/bottom.png",last_plot(),width=43,height=16,units="in")

# join top and bottom plots together, add title/caption

top / bottom + plot_annotation(title=paste0("Positive tests as a percentage of total tests, last 14 days\n"),
                               caption="Source: The COVID Tracking Project        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 
ggsave("~/state_curves/ratio.png",last_plot(),width=43,height=35,units="in")


# Next two sections work the same way
# note that the state silhouettes are saved over the top of silhouettes generated in previous sections

##################
#### 3. tests ####
##################

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
    
    geom_point(aes(x=date,y=totalTestResultsIncrease),size=3) +
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

# ggsave("~/state_curves/top.png",last_plot(),width=43,height=16,units="in")


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


# ggsave("~/state_curves/bottom.png",last_plot(),width=43,height=16,units="in")



top / bottom + plot_annotation(title=paste0("Total Covid-19 tests processed per day\n"),
                               caption="Source: The COVID Tracking Project        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 
ggsave("~/state_curves/total.png",last_plot(),width=43,height=35,units="in")

##################
#### 4. cases ####
##################

for (i in 1:50) {
  
  state_name <- state.name[i]
  state_abb <- state.abb[i]
  
  temp_dat <- tests_recent[which(tests_recent$state==state_abb),]
  
  res <- lm(positiveIncrease ~ date,data=temp_dat) %>% tidy()
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
    
    geom_point(aes(x=date,y=positiveIncrease),size=3,show.legend = F) +
    geom_smooth(aes(x=date,y=positiveIncrease),method="lm",se=F,color="blue",size=2) +
    
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

# ggsave("~/state_curves/top.png",last_plot(),width=43,height=16,units="in")


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


# ggsave("~/state_curves/bottom.png",last_plot(),width=43,height=16,units="in")



top / bottom + plot_annotation(title="Documented Covid-19 cases by state, last 14 days\n",
                               caption="Source: The COVID Tracking Project        \nhttps://github.com/JoeMitchellNelson/state_curves        \n",
                               theme=theme(plot.title = element_text(family="Book Antiqua",size=50,hjust=.5,vjust=.1),
                                           plot.caption = element_text(family="Book Antiqua",size=35,color="grey30"))) 
ggsave("~/state_curves/cases.png",last_plot(),width=43,height=35,units="in")
