### Setup

setwd('C:/Users/Gavin/Documents/GitHub/eye-movements-efficient-search')

library(tidyverse)
library(stringr)
library(ggplot2)
library(eyelinkR)
library(ez)
library(plotrix)


### Functions to beautify plots

number_ticks <- function(n) {function(limits) pretty(limits, n)}

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

### Import data
sim1 <- "High-similarity"
sim2 <- "Low-similarity"
eyeleave_val <- .15

all_df <- read_data("expt1b_data.csv", "confirm")
all_data <- all_df[[1]]
good_data <- clean_data(all_data, .9, eyeleave_val)
good_data_eye <- clean_data(all_data, .9, .25)
good_subs <- unique(good_data$sub_id)
good_subs_eye <- unique(good_data_eye$sub_id)

n_subs <- 18

## subject 10 and 18 were excluded as there were conditions in which they did not make eye movements
## subject 26 was excluded to make the total number of subjects == 18
## as stated in pre-reg
excluded_subs <- c(10, 18, 26)

excluded_subs_conditions <- good_data_eye %>% 
  filter(saccade_latency > 50) %>%
  filter(condition == 1) %>%
  filter(sub_id == 10) %>%
  group_by(d_id, d_setsize) %>%
  summarize(i = mean(condition))

accuracy <- all_data %>%
  group_by(d_id, condition) %>%
  summarize(mean = mean(hit))

#######################
## EYE LEAVE ##
###############

eyeleave <- good_data_eye %>% 
  filter(condition==1) %>%
  # filter(sub_id!=10, sub_id!=18, sub_id!=26) %>%
  filter(!is.element(sub_id, excluded_subs)) %>%
  group_by(d_id, d_setsize) %>%
  summarise(eyeleave = 1- mean(eye_leave))



###########################
## EYE MOVEMENT ANALYSES ##
###########################

## saccade and fixations data
# good_data_eye <- good_data_eye %>% 
  # filter(saccade_latency > 50) 
fix_data <- all_df[[3]] %>%
  filter(is.element(sub_id, good_subs_eye)) %>%
  group_by(sub_id,trial) %>%
  mutate(fix_num = seq_along(sub_id), fixations = sum(hit)) 

good_fix_data <- all_df[[3]] %>%
  filter(is.element(sub_id, good_subs_eye)) %>%
  filter(saccade_latency > 50)

good_fix_data$eccentricity <- lapply(good_fix_data$circle, function(x){
  if(x==1){
    return(4.2)
  } else if(x==2){
    return(7.4)
  } else{
    return(14.3)
  }
})
good_fix_data$eccentricity <- as.numeric(good_fix_data$eccentricity)


####### Total fixations ########

total_fixations <- good_data_eye %>%
  filter(condition==1) %>%
  filter(is.element(sub_id, good_subs_eye)) %>%
  filter(!is.element(sub_id, excluded_subs)) %>%
  filter(fixations > 1) %>%
  group_by(sub_id, d_id, d_setsize) %>%
  summarise(fixations = mean(fixations)) 

fix_plot <- total_fixations%>%
  group_by(d_id, d_setsize) %>%
  summarise(mean_fix = mean(fixations), sem = sd(fixations)/sqrt(n_subs))

ggplot(fix_plot, aes(d_setsize, mean_fix,linetype=as.factor(d_id))) + 
  geom_line() +
  geom_point(data=fix_plot, aes(d_setsize, mean_fix, shape=as.factor(d_id), size=as.factor(d_id),linetype=as.factor(d_id))) +
  geom_errorbar(aes(ymin=mean_fix-sem, ymax=mean_fix+sem, linetype=as.factor(d_id)), width=.3, linetype=1) +
  scale_linetype_manual(values=c("dotted", "solid", 'dashed')) +
  # scale_color_manual(values=c('#FF0000', '#FFA500', '#0000FF'), labels = c("Target-only", sim1, sim2)) +
  scale_shape_manual(values=c("0"=17, "1"=19, "2"=18), labels = c("Target-only", sim1, sim2)) +
  scale_size_manual(values=c(3.5,3.5,4.5), guide=FALSE) +
  # scale_y_continuous(limits=c(2.25,3.5)) +
  xlab("Set size") +
  ylab("Mean number of fixations \n") +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(1.5,3.5))+
  scale_y_continuous(breaks=seq(1.5,3.5,0.1),
                     labels = every_nth(seq(1.5,3.5,0.1), 5, inverse=TRUE)) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank()) 

total_fixations$d_id <- as.factor(total_fixations$d_id)
total_fixations$d_setsize <- as.factor(total_fixations$d_setsize)

ezANOVA(total_fixations %>% filter(d_id!=0) ,
        dv = fixations,
        wid = sub_id,
        within = .(d_id, d_setsize))


######## LANDING LOCATION OF INITIAL SACCADE ########
distance <- good_data_eye %>%
  filter(condition==1, !is.na(saccade_latency)) %>%
  filter(is.element(sub_id, good_subs_eye)) %>%
  filter(!is.element(sub_id, excluded_subs)) %>%
  group_by(sub_id, d_id, d_setsize) %>%
  summarise(distance = mean(distance, na.rm=TRUE)) 

distance_plot <- distance%>%
  filter(!is.na(distance)) %>%
  group_by(d_id, d_setsize) %>%
  summarise(mean_distance = mean(distance), sem = sd(distance)/sqrt(n_subs))


ggplot(distance_plot, aes(d_setsize, mean_distance,linetype=as.factor(d_id))) + 
  geom_line() +
  geom_point(data=distance_plot, aes(d_setsize, mean_distance, shape=as.factor(d_id), size=as.factor(d_id),linetype=as.factor(d_id))) +
  geom_errorbar(aes(ymin=mean_distance-sem, ymax=mean_distance+sem, linetype=as.factor(d_id)), width=.3, linetype=1) +
  scale_linetype_manual(values=c("dotted", "solid", "dashed")) +
  # scale_color_manual(values=c('#FF0000', '#FFA500', '#0000FF'), labels = c("Target-only", sim1, sim2), name="Lure type") +
  # scale_color_manual(values=c('#00CED1', '#FFA500', '#0000FF'), labels = c("Target-only", sim1, sim2)) +
  scale_shape_manual(values=c("0"=17, "1"=19, "2"=18), labels = c("Target-only", sim1, sim2)) +
  scale_size_manual(values=c(3.5,3.5,4.5), guide=FALSE) +
  # scale_linetype_manual(values=c(4,1,2,3), name="Set size") +
  # scale_y_continuous(limits=c(1.5, 6)) +
  xlab("Set size") +
  ylab("Distance between initial saccade \n and target (deg) \n") +
  # ggtitle("With no-saccade trials")
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(1,6)) +
  scale_y_continuous(breaks=seq(1,6, 0.5),
                     labels = every_nth(seq(1, 6, 0.5), 2, inverse=TRUE)) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  theme(legend.position="none")


distance$d_id <- as.factor(distance$d_id)
distance$d_setsize <- as.factor(distance$d_setsize)

ezANOVA(distance %>% filter(d_id!=0),
        dv = distance,
        wid = sub_id,
        within = .(d_id, d_setsize))


#### POLAR PLOT ####


######## ANGULAR ERROR ########

t_angle <- atan2(good_data_eye$t_xloc, good_data_eye$t_yloc)
fix_angle <- atan2(good_data_eye$fix_x, good_data_eye$fix_y)

good_data_eye$angle <- (t_angle - fix_angle) * 180/pi

## convert to -180 < x < 180 for memtoolbox
good_data_eye$angle <- sapply(good_data_eye$angle, function(x){
  if (is.na(x)){
    x
  } else if (x>180){
    x-360
  } else if (x< -180){
    x+360
  } else{
    x
  }
  
})



initial_angles <- good_data_eye %>% 
  filter(complete.cases(angle), condition ==1 & saccade_latency >50) %>%
  filter(!is.element(sub_id, excluded_subs))

initial_angles_polar <- good_data_eye %>%
  filter(complete.cases(angle), condition ==1 & saccade_latency >50) %>%
  filter(!is.element(sub_id, excluded_subs)) %>%
  select(angle, d_id, distance, circle, saccade_amplitude)

intial_angles_polar <- rbind(initial_angles_polar, c(0, 1, 4.2, 1, 4.2))

## High-similarity lures ##
# Near eccentricity
polar.plot((initial_angles_polar %>% filter(d_id == 1 & circle==1))$distance, 
           (initial_angles_polar %>% filter(d_id == 1 & circle == 1))$angle, 
           rp.type="s", point.symbols=3, point.col="orange", start=90, radial.lim = c(0,25))

# place the target
polar.plot(4.2, 0, rp.type="s", cex=1.2, point.symbols=24, point.col = "black", start = 90, radial.lim = c(0,25), add=TRUE)
polar.plot(4.2, 0, rp.type="s", cex=1.2, point.symbols=17, point.col = "red", start = 90, radial.lim = c(0,25), add=TRUE)


# Middle eccentricity
polar.plot((initial_angles_polar %>% filter(d_id == 1 & circle==2))$saccade_amplitude, 
           (initial_angles_polar %>% filter(d_id == 1 & circle == 2))$angle, 
           rp.type="s", point.symbols=3, point.col="orange", start=90, radial.lim = c(0,25))

# place the target
polar.plot(7.7, 0, rp.type="s", cex=1.2, point.symbols=24, point.col = "black", start = 90, radial.lim = c(0,25), add=TRUE)
polar.plot(7.7, 0, rp.type="s", cex=1.2, point.symbols=17, point.col = "red", start = 90, radial.lim = c(0,25), add=TRUE)


# Far eccentricity
polar.plot((initial_angles_polar %>% filter(d_id == 1 & circle==3))$saccade_amplitude, 
           (initial_angles_polar %>% filter(d_id == 1 & circle == 3))$angle, 
           rp.type="s", point.symbols=3, point.col="orange", start=90, radial.lim = c(0,25))
# place the target
polar.plot(14.3, 0, rp.type="s", cex=1.2, point.symbols=24, point.col = "black", start = 90, radial.lim = c(0,25), add=TRUE)
polar.plot(14.3, 0, rp.type="s", cex=1.2, point.symbols=17, point.col = "red", start = 90, radial.lim = c(0,25), add=TRUE)




### Low-similarity lures ###

# Near eccentricity
polar.plot((initial_angles_polar %>% filter(d_id == 2 & circle == 1))$saccade_amplitude, 
           (initial_angles_polar %>% filter(d_id == 2 & circle == 1))$angle, 
           rp.type="s", point.symbols = 4, point.col="blue", start=90, radial.lim = c(0,25))
polar.plot(4.2, 0, rp.type="s", cex=1.2, point.symbols=24, point.col = "black", start = 90, radial.lim = c(0,25), add=TRUE)
polar.plot(4.2, 0, rp.type="s", cex=1.2, point.symbols=17, point.col = "red", start = 90, radial.lim = c(0,25), add=TRUE)

# Middle eccentricity
polar.plot((initial_angles_polar %>% filter(d_id == 2 & circle == 2))$saccade_amplitude, 
           (initial_angles_polar %>% filter(d_id == 2 & circle == 2))$angle, 
           rp.type="s", point.symbols = 4, point.col="blue", start=90, radial.lim = c(0,25))
polar.plot(7.7, 0, rp.type="s", cex=1.2, point.symbols=24, point.col = "black", start = 90, radial.lim = c(0,25), add=TRUE)
polar.plot(7.7, 0, rp.type="s", cex=1.2, point.symbols=17, point.col = "red", start = 90, radial.lim = c(0,25), add=TRUE)

# Far eccentricity
polar.plot((initial_angles_polar %>% filter(d_id == 2 & circle == 3))$saccade_amplitude, 
           (initial_angles_polar %>% filter(d_id == 2 & circle == 3))$angle, 
           rp.type="s", point.symbols = 4, point.col="blue", start=90, radial.lim = c(0,25))
polar.plot(14.3, 0, rp.type="s", cex=1.2,  point.symbols=24, point.col = "black", start = 90, radial.lim = c(0,25), add=TRUE)
polar.plot(14.3, 0, rp.type="s", cex=1.2,  point.symbols=17, point.col = "red", start = 90, radial.lim = c(0,25), add=TRUE)


######## MEMFIT GRAPHS ########

## Red
g_red <- c(0.176, 0.209, 0.579, 0.074, 0.087, 0.117)
g_blue <- c(0.152, 0.212, 0.447, 0.051, 0.064, 0.088)
eccentricity <- c("Near","Middle","Far","Near","Middle","Far")
lure <- c(1,1,1,2,2,2)
memfit <- data.frame(g_red, g_blue, lure, eccentricity)

ggplot(data=memfit, aes(as.factor(eccentricity), g_red, fill=as.factor(lure))) +
  aes(x=fct_inorder(eccentricity)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Eccentricity") +
  ylab("Probability of guess saccade") +
  scale_fill_manual(values=c("#FFA500","#0000FF"), labels=c("High-similarity", "Low-similarity")) +
  coord_cartesian(ylim=c(0,0.6)) +
  scale_y_continuous(breaks=seq(0,0.6, 0.1),
                     labels = every_nth(seq(0, 0.6, 0.1), 2, inverse=TRUE)) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  theme(legend.title=element_blank(), panel.background = element_blank())


  

ggplot(data=memfit, aes(as.factor(eccentricity), g_blue, fill=as.factor(lure))) +
  aes(x=fct_inorder(eccentricity))+
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits=c(0,0.6)) +
  xlab("Eccentricity") +
  ylab("Probability of guess saccade") +
  scale_fill_manual(values=c("#0000FF","#FFA500"), labels=c("High-similarity", "Low-similarity")) +
  coord_cartesian(ylim=c(0,0.6)) +
  scale_y_continuous(breaks=seq(0,0.6, 0.1),
                     labels = every_nth(seq(0, 0.6, 0.1), 2, inverse=TRUE)) +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  theme(legend.title=element_blank(), panel.background = element_blank())

