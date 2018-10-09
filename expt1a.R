## R code for analysing red pilot experiment (1A)
##
##

#########
# Setup #
#########
setwd('C:/Users/Gavin/Box Sync/Human Attention Lab Folders/Attention Project/Gavin/Log slopes')
folder = getwd()
setwd(folder)
library(tidyverse)
library(stringr)
library(ggplot2)
library(eyelinkR)
library(ez)

number_ticks <- function(n) {function(limits) pretty(limits, n)}

insert_minor <- function(major_labs, n_minor) {labs <- 
  c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
labs[1:(length(labs)-n_minor)]}

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
#############
# Read Data #
#############
sim1 <- "High-similarity"
sim2 <- "Low-similarity"
eyeleave_val <- .15

all_df <- read_data("./Expt 1A/expt1a_data.csv", "pilot")
all_data <- all_df[[1]]
good_data <- clean_data(all_data, .9, eyeleave_val) # remove those with accuracy < 90% and with more than eyeleave_val% eye movements
good_subs <- unique(good_data$sub_id)
descrip <- get_desc(all_data)

accuracy <- descrip %>%
  filter(is.element(sub_id, good_subs)) %>%
  filter(condition == 2) 

mean(accuracy$accuracy)




#################
####  ANALYSES
#################

RT_free <- good_data %>%
  filter(block==1) %>%
  filter(RT < 10000) %>%
  group_by(sub_id, d_id, d_setsize, d_setsize_log) %>%
  summarise(meanrt = mean(RT, na.rm=TRUE))

RT_fixed <- good_data %>%
  filter(block==2 & eye_leave==0) %>%
  group_by(sub_id, d_id, d_setsize, d_setsize_log) %>%
  summarise(meanrt = mean(RT, na.rm=TRUE))

RT_fixed$d_id <- as.factor(RT_fixed$d_id)
RT_fixed$d_setsize <- as.factor(RT_fixed$d_setsize)

ezANOVA(RT_fixed %>% filter(d_setsize!=1),
        dv=meanrt,
        wid=sub_id,
        within=.(d_id, d_setsize))


##############################
## SLOPES ##
###########

target_only_data <- good_data %>%
  filter(d_id == 0)

target_1 <- target_only_data
target_1$d_id <- 1
target_2 <- target_only_data
target_2$d_id <- 2

slopes_data <- rbind(good_data %>% filter(d_id!=0), target_1, target_2)


slopes <- slopes_data %>%
  # mutate(eyeleave_2 = if_else(condition==2 & saccade_latency>0, 1, 0)) %>%
  # mutate(eyeleave_1 = if_else(condition==1 & saccade_latency>0, 0, 1)) %>%
  # filter((condition==2 & is.na(saccade_latency)) | (condition==1 & saccade_latency>0))%>%
  mutate(eyeleave_2 = if_else(condition==2 & saccade_latency>0, 1, 0)) %>%
  # mutate(eyeleave_1 = if_else(condition==1 & saccade_latency>0, 0, 1)) %>%
  filter((condition==2 & is.na(saccade_latency)) | (condition==1))%>%
  group_by(d_id, condition, d_setsize_log) %>%
  summarise(meanrt = mean(RT, na.rm=TRUE)) %>%
  do(slope = lm(meanrt ~ d_setsize_log, data=.)) %>%
  tidy(slope) %>%
  filter(term=="d_setsize_log") %>%
  select(-term, -std.error, -statistic, -p.value)



slopes$condition <- as.factor(slopes$condition)
slopes$d_id <- as.factor(slopes$d_id)

ezANOVA(slopes,
        dv=estimate,
        wid=sub_id,
        within=.(d_id, condition))

t.test((slopes %>% filter(d_id==2, condition ==1))$estimate, (slopes%>%filter(d_id==2, condition==2))$estimate, paired=TRUE)

slopes <- get_slopes(RT_free, RT_fixed)

t.test(slopes$orange_log_fixed, slopes$blue_log_fixed, paired=TRUE)


###############################
## FITS ##
##########
fits <- get_fits(RT_free, RT_fixed)


##################
## AIC ##
#########


# since we have to include target-only into fits, create new variable for AIC/plotting
# then, add another row of target-only (since we need two, one for each lure type)
# then, assign that to the remaining lure type that doesn't have the target-only condition



## CHANGE THIS!!


# not sure what is going on with the data type (graph gets wonky) so re-assign RT_fixed
RT_fixed <- good_data %>%
  filter(block==2 & eye_leave==0) %>%
  group_by(sub_id, d_id, d_setsize, d_setsize_log) %>%
  summarise(meanrt = mean(RT, na.rm=TRUE))

plotdf <- RT_free

RT_plot <- plotdf %>%
  group_by(d_id, d_setsize, d_setsize_log) %>%
  summarise(mean_rt = mean(meanrt), sem = sd(meanrt)/sqrt(nrow(RT_fixed)/7)) %>%
  mutate(plot_id = ifelse((d_id==0 | d_id==1),1,2 ))

RT_plot <- rbind(RT_plot, RT_plot%>%filter(d_id==0))
RT_plot$plot_id[nrow(RT_plot)] <- 2


#### Get AIC

# 1 is orange
AIC(lm(mean_rt~(d_setsize_log), RT_plot %>%filter(plot_id==1)))
AIC(lm(mean_rt~(d_setsize_log), RT_plot %>%filter(plot_id==2)))

AIC(lm(mean_rt~(d_setsize), RT_plot %>%filter(plot_id==1)))
AIC(lm(mean_rt~(d_setsize), RT_plot %>%filter(plot_id==2)))

####  Get r-sq values 

orange_linear <- round((summary(lm(mean_rt~(d_setsize), RT_plot %>% filter(plot_id==1))))$r.squared, digits=3)
orange_log <- round((summary(lm(mean_rt~(d_setsize_log), RT_plot %>% filter(plot_id==1))))$r.squared, digits=4)
blue_linear <- round((summary(lm(mean_rt~(d_setsize), RT_plot %>% filter(plot_id==2))))$r.squared, digits=3)
blue_log <- round((summary(lm(mean_rt~(d_setsize_log), RT_plot %>% filter(plot_id==2))))$r.squared, digits=3)


#######################
## RT PLOT ##
#############


ggplot(RT_plot, aes(d_setsize, mean_rt,color=as.factor(plot_id))) + 
  stat_smooth(method="lm", formula=y~log(x), se=FALSE, linetype=1) +
  geom_point(data=RT_plot, aes(d_setsize, mean_rt, shape=as.factor(d_id), size=as.factor(d_id),color=as.factor(d_id))) +
  geom_errorbar(aes(ymin=mean_rt-sem, ymax=mean_rt+sem, color=as.factor(d_id)), width=.3) +
  scale_color_manual(values=c('#FF0000', '#FFA500', '#0000FF'), labels = c("Target-only", sim1, sim2)) +
  scale_shape_manual(values=c("0"=17, "1"=18, "2"=19), labels = c("Target-only", sim1, sim2)) +
  scale_size_manual(values=c(4,5,4), guide=FALSE) +
  xlab("\n Set size") +
  ylab("Reaction Time (ms) \n") +
  coord_cartesian(ylim=c(400,650))+
  # scale_y_continuous(breaks=number_ticks(20)) +
  scale_y_continuous(breaks=seq(400,650,10),
                     labels = every_nth(seq(400,650,10), 5, inverse=TRUE)) +
  theme_bw() + 
  theme(panel.border = element_rect(linetype = 'solid', colour = 'grey', size=1.2),
        text = element_text(size=14)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none",
        axis.text=element_text(size=14)) 




