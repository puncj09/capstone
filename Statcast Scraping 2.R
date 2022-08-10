### All project code ###

### Scraping Statcast, Data Analysis, Machine Learning, Data Visualizations ###

# Load necessary packages
library(baseballr)
library(tidyverse)
library(ggplot2)
library(gt)
library(PRROC)
library(ipred)
library(rpart)
library(caret)
library(reshape2)

# set wd
setwd('C:/Users/punco/OneDrive/Desktop/DS 785 Assignments')

# initialize data frame for live pitch tracking (way up here so I don't write over it)
all_live_pitches <- data.frame()

# Scrape Statcast w/ existing code (2021) provided by Robert Frey's YouTube video
# Source: https://github.com/robert-frey/YouTube/tree/master/Obtain%20Full%20Year%20Statcast%20Data%20with%20Some%20Simple%20Steps

#load Statcast data week by week, since it can only load 10 days at a time or 40,000 observations
#scrape_statcast_savant scrapes data from Savant given the game dates and the player types
date328407 = baseballr::scrape_statcast_savant(start_date = '2019-03-28',
                                               end_date = '2019-04-07', player_type = 'batter')

date408414 = baseballr::scrape_statcast_savant(start_date = '2019-04-08',
                                               end_date = '2019-04-14', player_type = 'batter')

date415421 = baseballr::scrape_statcast_savant(start_date = '2019-04-15',
                                               end_date = '2019-04-21', player_type = 'batter')

date422428 = baseballr::scrape_statcast_savant(start_date = '2019-04-22',
                                               end_date = '2019-04-28', player_type = 'batter')

date429505 = baseballr::scrape_statcast_savant(start_date = '2019-04-29',
                                               end_date = '2019-05-05', player_type = 'batter')

date506512 = baseballr::scrape_statcast_savant(start_date = '2019-05-06',
                                               end_date = '2019-05-12', player_type = 'batter')

date513519 = baseballr::scrape_statcast_savant(start_date = '2019-05-13',
                                               end_date = '2019-05-19', player_type = 'batter')

date520526 = baseballr::scrape_statcast_savant(start_date = '2019-05-20',
                                               end_date = '2019-05-26', player_type = 'batter')

date527602 = baseballr::scrape_statcast_savant(start_date = '2019-05-27',
                                               end_date = '2019-06-02', player_type = 'batter')

date603609 = baseballr::scrape_statcast_savant(start_date = '2019-06-03',
                                               end_date = '2019-06-09', player_type = 'batter')

date610616 = baseballr::scrape_statcast_savant(start_date = '2019-06-10',
                                               end_date = '2019-06-16', player_type = 'batter')

date617623 = baseballr::scrape_statcast_savant(start_date = '2019-06-17',
                                               end_date = '2019-06-23', player_type = 'batter')

date624630 = baseballr::scrape_statcast_savant(start_date = '2019-06-24',
                                               end_date = '2019-06-30', player_type = 'batter')

date701707 = baseballr::scrape_statcast_savant(start_date = '2019-07-01',
                                               end_date = '2019-07-07', player_type = 'batter')

date708714 = baseballr::scrape_statcast_savant(start_date = '2019-07-08',
                                               end_date = '2019-07-14', player_type = 'batter')

date715721 = baseballr::scrape_statcast_savant(start_date = '2019-07-15',
                                               end_date = '2019-07-21', player_type = 'batter')

date722728 = baseballr::scrape_statcast_savant(start_date = '2019-07-22',
                                               end_date = '2019-07-28', player_type = 'batter')

date729804 = baseballr::scrape_statcast_savant(start_date = '2019-07-29',
                                               end_date = '2019-08-04', player_type = 'batter')

date805811 = baseballr::scrape_statcast_savant(start_date = '2019-08-05',
                                               end_date = '2019-08-11', player_type = 'batter')

date812818 = baseballr::scrape_statcast_savant(start_date = '2019-08-12',
                                               end_date = '2019-08-18', player_type = 'batter')

date819825 = baseballr::scrape_statcast_savant(start_date = '2019-08-19',
                                               end_date = '2019-08-25', player_type = 'batter')

date826901 = baseballr::scrape_statcast_savant(start_date = '2019-08-26',
                                               end_date = '2019-09-01', player_type = 'batter')

date902908 = baseballr::scrape_statcast_savant(start_date = '2019-09-02',
                                               end_date = '2019-09-08', player_type = 'batter')

date909915 = baseballr::scrape_statcast_savant(start_date = '2019-09-09',
                                               end_date = '2019-09-15', player_type = 'batter')

date916922 = baseballr::scrape_statcast_savant(start_date = '2019-09-16',
                                               end_date = '2019-09-22', player_type = 'batter')

date923929 = baseballr::scrape_statcast_savant(start_date = '2019-09-23',
                                               end_date = '2019-09-29', player_type = 'batter')

#combine all data into one data frame (729,793 rows)
SavantData19 = rbind(date328407, date408414, date415421, date422428, date429505,
                     date506512, date513519, date520526, date527602, date603609,
                     date610616, date617623, date624630, date701707, date708714,
                     date715721, date722728, date729804, date805811, date812818,
                     date819825, date826901, date902908, date909915, date916922,
                     date923929)

#write file to a csv on your machine (csv is a comma separated value excel file)
write.csv(SavantData19,"SavantHittingData19.csv", row.names = F)

#read in the csv file that you just created to the dataframe
SavantData19 = read.csv("SavantHittingData19.csv", stringsAsFactors = F)

# End of source's code

# scrape 2019 hitter statistics
hitter_stats_2019 <- mlb_stats(stat_type = 'season', stat_group = 'hitting', season = 2019, player_pool = "All")
hitter_stats_2019 <- tibble(hitter_stats_2019)

# Scrape 2019 pitcher statistics
pitcher_stats_2019 <- mlb_stats(stat_type = 'season', stat_group = 'pitching', season = 2019, player_pool = "All")
pitcher_stats_2019 <- tibble(pitcher_stats_2019)

# Average exit velocity by hitter
ev_df <- SavantData19 %>% drop_na(launch_speed) %>% dplyr::group_by(batter, player_name) %>% dplyr::summarise(avg_EV = mean(launch_speed)) %>% 
  dplyr::arrange(desc(avg_EV))

# Launch angle by hitter
angle_df <- SavantData19 %>% drop_na(launch_angle) %>% dplyr::group_by(batter, player_name) %>% dplyr::summarise(avg_angle = mean(launch_angle)) %>% 
  dplyr::arrange(desc(avg_angle))

# Add pitcher name to pitcher stats data
SavantData19 <- left_join(SavantData19, pitcher_stats_2019 %>% dplyr::select(player_id, player_full_name), by=c("pitcher"="player_id"))

# EV allowed -- pitcher
ev_allowed <- SavantData19 %>% drop_na(launch_speed) %>% dplyr::group_by(pitcher, player_full_name) %>% dplyr::summarise(avg_EV_allowed = mean(launch_speed)) %>% 
  dplyr::arrange(desc(avg_EV_allowed))

# Launch angle allowed -- pitcher
angle_allowed <- SavantData19 %>% drop_na(launch_angle) %>% dplyr::group_by(pitcher, player_full_name) %>% dplyr::summarise(avg_angle_allowed = mean(launch_angle)) %>% 
  dplyr::arrange(desc(avg_angle_allowed))

# average Release speed -- pitcher
avg_velocity <- SavantData19 %>% drop_na(release_speed) %>% dplyr::group_by(pitcher, player_full_name) %>% dplyr::summarise(avg_velo = mean(release_speed)) %>% 
  dplyr::arrange(desc(avg_velo))

# Merge all hitter stats
hitters_df_all <- merge(ev_df, angle_df, by = c("batter", "player_name"))
hitters_df_all <- left_join(hitters_df_all, hitter_stats_2019 %>% dplyr::select(player_id, plate_appearances), by=c("batter"="player_id"))

# Merge all pitcher stats
#put all data frames into list
pitcher_df_list <- list(ev_allowed, angle_allowed, avg_velocity)

#merge all data frames in list
pitchers_df_all <- pitcher_df_list %>% reduce(full_join, by=c('pitcher', 'player_full_name'))
pitchers_df_all <- left_join(pitchers_df_all, pitcher_stats_2019 %>% dplyr::select(player_id, innings_pitched), by=c("pitcher"="player_id"))

########################

#### WEATHER DATA ####
weather_payload <- get_game_info_sup_petti() %>% dplyr::filter(substr(game_date, 1,4)==2021 & game_type=="R") %>% dplyr::select(game_pk, venue_name,
                                                                                                                                venue_id, temperature,
                                                                                                                                other_weather, wind)
weather_data <- tibble(weather_payload)
# Wind Speed and Direction
weather_data <- weather_data %>% mutate(WindSpeed = as.numeric(sub(" .*", "", wind)),
                                        WindDirection = sub(".*, ", "", wind))

# Wind Category (Clear, cloudy, rain, dome)
weather_data <- weather_data %>% mutate(WeatherCategory = case_when(other_weather %in% c("Sunny", "Clear") ~ "Clear",
                                                                 other_weather %in% c("Cloudy", "Partly Cloudy", "Overcast") ~ "Clouds",
                                                                 other_weather %in% c("Drizzle", "Rain", "Snow") ~ "Precip",
                                                                 TRUE ~ "Dome/Roof"),
                                        DirectionCategory = case_when(WindDirection %in% c("Out To CF", "Out To RF", "Out To LF") ~ "Out",
                                                                      WindDirection %in% c("In From CF", "In From RF", "In From LF") ~ "In",
                                                                      WindDirection %in% c("None", "Calm") ~ "No wind",
                                                                      WindDirection %in% c("L To R", "R To L") ~ "Crosswind",
                                                                      TRUE ~ "Other"))

# couple quick weather plots
ggplot(weather_data, aes(WindSpeed)) + geom_histogram() + facet_wrap(~venue_name, ncol= 6) + theme_minimal()
ggplot(weather_data, aes(DirectionCategory)) + geom_bar() #+ facet_wrap(~venue_name, ncol= 6) + theme_minimal()


## DATA ANALYSIS ##
# Read in 2021 Kaggle pitch by pitch data
pitches <- read.csv("pitches2021.csv")

# Merge pitches and weather data
pitches <- left_join(pitches, weather_data %>% dplyr::select(game_pk, venue_name, temperature, WeatherCategory, WindSpeed, DirectionCategory), by="game_pk")

# Only want batted balls
pitches2 <- pitches %>% dplyr::filter(events %in% c("single", "double", "triple", "force_out", "grounded_into_double_play", "home_run", 
                                                    "sac_fly_double_play", "fielders_choice_out", "field_out", "field_error", "fielders_choice",
                                                    "triple_play", "sac_fly"))

# Home Run binary variable
pitches2 <- pitches2 %>% dplyr::mutate(home_run = ifelse(events=='home_run',1,0))

# remove unnecessary columns from pitches data
pitches3 <- pitches2 %>% dplyr::select(batter, pitcher, events, stand, p_throws, count, outs_when_up, inning, home_run, venue_name, temperature,
                                       WeatherCategory, WindSpeed, DirectionCategory)

# Merge 2019 hitter stats with 2021 pitches
pitches4 <- left_join(pitches3, hitters_df_all, by = "batter")

# Merge 2019 pitcher stats with 2021 pitches
pitches5 <- left_join(pitches4, pitchers_df_all, by = "pitcher")

# rename batter and pitcher name columns
pitches5 <- pitches5 %>% dplyr::rename(BatterName = player_name, PitcherName=player_full_name)

# REMOVE NAs
pitches5 <- pitches5[!is.na(pitches5$innings_pitched),]
pitches5 <- pitches5[!is.na(pitches5$plate_appearances),]

#########

## Machine Learning Models ##

# log loss function
LogLoss <- function(pred, res){
  (-1/length(pred)) * sum (res * log(pred) + (1-res)*log(1-pred))
}

# logistic regression
set.seed(3)

# remove id and other non-model features
pitches6 = pitches5 %>% dplyr::select(-c(batter, pitcher, events, inning, BatterName, plate_appearances, PitcherName, innings_pitched)) 

# unlist "list" variables
pitches7 <- pitches6 %>% unnest(avg_EV) %>% unnest(avg_angle) %>% unnest(avg_EV_allowed) %>% unnest(avg_angle_allowed) %>% unnest(avg_velo)

# Remove rows with any NAs
hr_data <- hr_data[complete.cases(hr_data),]

# 10 fold CV
set.seed(3)
n = dim(hr_data)[1]
nfolds = 10
groups = rep(1:nfolds, n)
cvgroups = sample(groups, n)

# initialize prediction vector
CVpredictions = numeric(length = n)

for(ii in 1:nfolds) {
  groupii = (cvgroups == ii)
  trainset = hr_data[!groupii,]
  testset = hr_data[groupii,]
  
  # Logistic regression model
  fit <- glm(home_run ~ stand + p_throws + avg_EV + avg_angle + avg_EV_allowed + avg_angle_allowed + avg_velo + count + outs_when_up,
             data = trainset, family = 'binomial')
  predicted = predict(fit, newdata = testset, type = "response")   # predict for test set
  
  CVpredictions[groupii] = predicted
}

# calculate log loss - logistic regression
LogLoss(CVpredictions, hr_data$home_run)

# final model to test on new predictions
final_model_fit <- glm(home_run ~ stand + p_throws + avg_EV + avg_angle + avg_EV_allowed + avg_angle_allowed + avg_velo + count + outs_when_up,
                       data = hr_data, family = 'binomial')

hr_data$hr_prob <- CVpredictions     
log_model_data <- hr_data %>% dplyr::select(home_run, hr_prob) # data for model assessment

# class outcome distribution
classes_tbl <- tibble(Outcome = c("No Home Run", "Home Run"), Proportion = as.numeric(prop.table(table(hr_data$home_run))))
classes_gt <- gt(classes_tbl) %>% tab_header(title = "Table 1. Home Run Class Imbalance", subtitle = "proportion of batted balls that were home runs (2021 season)")  %>% 
  fmt_number(columns = "Proportion", decimals = 3) %>% cols_align(align="center")
classes_gt

# histogram of predicted probs - Logistic Regression
ggplot(log_model_data, aes(x=hr_prob)) +
  geom_histogram(data=subset(log_model_data,home_run==0), aes(fill=factor(home_run)),alpha=0.5) +
  geom_histogram(data=subset(log_model_data,home_run==1), aes(fill=factor(home_run)),alpha=0.5) + theme_minimal() + 
  labs(title = "Figure 1. Predicted Probabilities of Hitting a Home Run - Logistic Regression", x = "Predicted Probability", y = "Count") + 
  xlim(c(0, .25)) +
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), text = element_text(size=20), 
        legend.position = c(.75, .5), legend.background = element_rect(fill="light grey", size=2, linetype="solid", color = "light grey"),
        legend.title = element_text(size=15, face="bold")) + scale_fill_manual(name="Home Run", values = c("red","green"), labels=c("No", "Yes"))

# decision thresholds
thresholds <- seq(0, 1, by=.01)

# precision and recall lists
prec_list = numeric()
rec_list = numeric()
F_score_list = numeric()

for(i in thresholds) {
  # re-classify observations based on threshold
  # confusion matrix
  conf_mat <- as.matrix(table(factor(testing$bayes_prob > i, levels=c(F, T)), testing$home_run))
  
  # precision
  precision = conf_mat[4] / (conf_mat[4] + conf_mat[2])
  # recall
  recall = conf_mat[4] / (conf_mat[4] + conf_mat[3])
  # F score
  F_score = 2 / ((1/recall) + (1/precision))
  
  # append to lists
  prec_list = c(prec_list, precision)
  rec_list = c(rec_list, recall)
  F_score_list = c(F_score_list, F_score)
}

# optimal threshold logistic regression
opt_ind = which.max(F_score_list)

# confusion matrix at optimal threshold (GT package)
as.matrix(table(factor(testing$bayes_prob > thresholds[opt_ind], levels=c(F, T)), testing$home_run))
log_tbl = tibble(Predicted = c("Non HR", "HR"), "Non HR"=c(15190,2853), HR=c(692, 230))
gt(log_tbl) %>% tab_spanner(label="Actual Outcome", columns=c("Non HR", "HR")) %>% 
  tab_header(title = "Table 3. Confusion Matrix", subtitle="Naive Bayes Model with p=.07 Threshold") %>% cols_align(align="center") %>% 
  fmt_number(columns=2:3, use_seps = TRUE, decimals = 0)

# gg plot Precision-Recall Curve Logistic Regression
# used package to find AUC for PR curve
hr <- log_model_data$hr_prob[log_model_data$home_run==1]
no_hr <- log_model_data$hr_prob[log_model_data$home_run==0]
pr <- pr.curve(scores.class0 = hr, scores.class1 = no_hr, curve = T) #.0739
plot(pr)

# create data frame 
plot_df <- data.frame(p=prec_list, r=rec_list, f=F_score_list)
# plot PR curve with AUC pulled from above plot
ggplot(plot_df, aes(r, p)) + geom_point(size=3, color = "#454545") + 
  labs(title = "Figure 2. Precision-Recall Curve", x = "Recall", y = "Precision", subtitle = "AUC = .0739; optimal threshold = .07") + theme_minimal() + 
  scale_y_continuous(breaks=seq(0,1,.1)) + scale_x_continuous(breaks=seq(0,1,.1)) + 
  geom_point(aes(x=r[opt_ind], y=p[opt_ind]), pch=18, size=8, color='#ff0000') +
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
        text = element_text(size=17))

### Naive Bayes ###
hr_data <- pitches7[!is.na(pitches7$avg_EV_allowed),]

# combine pitcher-batter matchup
bayes_data <- hr_data %>% dplyr::select(-c(venue_name, temperature, WindCategory, WindSpeed, WindDirection))

# one hot encoding for categorical predictors
bayes_data_encoded <- bayes_data %>% mutate(ID = seq(1, nrow(bayes_data), 1)) # add ID variable for melt
bayes_data_encoded <- bayes_data_encoded %>% dplyr::select(stand, p_throws, count, outs_when_up, ID) # only categorical variables
bayes_data_encoded <- dcast(data = melt(bayes_data_encoded, id.vars = "ID"), ID ~ variable + value, length) # one-hot encode
bayes_data_final <- cbind(bayes_data_encoded, bayes_data %>% dplyr::select(-c(stand, p_throws, count, outs_when_up)))

# correlation
cor(bayes_data_final[,-c(1,21)])

#split data into training and test data sets
indxTrain <- createDataPartition(y = bayes_data$home_run,p = 0.75,list = FALSE)
training <- bayes_data[indxTrain,]
testing <- bayes_data[-indxTrain,]
x <- training[,-5] # no ID or response variable
y <- training$home_run

model = train(x,factor(y),'nb',trControl=trainControl(method='cv',number=10))

bayes_pred <- predict(model, newdata=testing, type="prob")
testing$bayes_prob <- bayes_pred[,2]

LogLoss(testing$bayes_prob, testing$home_run)

# histogram of predicted probs - Naive Bayes
ggplot(testing, aes(x=bayes_prob)) +
  geom_histogram(data=subset(testing,home_run==0), aes(fill=factor(home_run)),alpha=0.5) +
  geom_histogram(data=subset(testing,home_run==1), aes(fill=factor(home_run)),alpha=0.5) + theme_minimal() + 
  labs(title = "Figure 3. Predicted Probabilities of Hitting a Home Run - Naive Bayes", x = "Predicted Probability", y = "Count") + 
  xlim(c(0, .25)) +
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), text = element_text(size=20), 
        legend.position = c(.75, .5), legend.background = element_rect(fill="light grey", size=2, linetype="solid", color = "light grey"),
        legend.title = element_text(size=15, face="bold")) + scale_fill_manual(name="Home Run", values = c("red","green"), labels=c("No", "Yes"))

# gg plot Precision-Recall Curve naive bayes
# used package to find AUC for PR curve
hr <- testing$bayes_prob[testing$home_run==1]
no_hr <- testing$bayes_prob[testing$home_run==0]
pr <- pr.curve(scores.class0 = hr, scores.class1 = no_hr, curve = T) #.0703
plot(pr)

# create data frame 
plot_df <- data.frame(p=prec_list, r=rec_list, f=F_score_list)
# plot PR curve with AUC pulled from above plot
ggplot(plot_df, aes(r, p)) + geom_point(size=3, color = "#454545") + 
  labs(title = "Figure 4. Precision-Recall Curve", x = "Recall", y = "Precision", subtitle = "AUC = .0703; optimal threshold = .07") + theme_minimal() + 
  scale_y_continuous(breaks=seq(0,1,.1)) + scale_x_continuous(breaks=seq(0,1,.1)) + 
  geom_point(aes(x=r[opt_ind], y=p[opt_ind]), pch=18, size=8, color='#ff0000') +
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
        text = element_text(size=17))

# log loss table
log_loss_tbl = tibble(Model = c("Logistic Regression", "Naive Bayes"), "Log Loss"=c(.1914,.1922))
gt(log_loss_tbl) %>% 
  tab_header(title = "Table 4. Log Loss") %>% cols_align(align="center")

# Logistic Regression Model Coefficients Interpretation table
# Logit to probability function source: ("https://sebastiansauer.github.io/Rcode/logit2prob.R")
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(odds)
}
logit2prob(coef(final_model_fit))

coef_tbl = tibble(Variable = c("BatHand Right", "Exit Velo", "Launch Angle", "Launch Angle Allowed", "Count 0-2", "Count 2-0", "Count 3-1"),
                      Coefficient=c(.145,.132,.0416,.0261,-.4245,.3916,.4495),
                      Odds=c(1.16, 1.14, 1.04, 1.03, .65, 1.48, 1.57)) %>% arrange(desc(Odds))
gt(coef_tbl) %>% 
  tab_header(title = "Table 5. Logistic Regression Significant Coefficients", subtitle="with corresponding odds change") %>% cols_align(align="center")


# LIVE HOME RUN PROBABILITY

# Get weather data from game
get_weather_data <- function(game_pk) {
  weather = get_game_info_mlb(game_pk=game_pk) %>% dplyr::select(game_pk, game_date, venue_name, temperature, other_weather, wind)
  return(tibble(weather))
}

# update statcast data by searching game date (would be today's date) and home team
get_live_probs <- function(date, home_team) {
  # get game packs for date
  game_packs <- baseballr::get_game_pks_mlb(date=date, level_ids = 1)
  # if there are live games, choose 1 game
  one_game_pack <- game_packs %>% filter(status.detailedState == "In Progress", teams.home.team.name==home_team) %>% pull(game_pk)
  
  # Get statcast pbp data for that game 
  one_game <- baseballr::get_pbp_mlb(game_pk = one_game_pack)
  
  #get_weather_data(one_game)
  
  return(tibble(one_game))
}

# filter live statcast game data
filter_statcast_game <- function(tbl_obj) {
  filtered <- tbl_obj %>% dplyr::select(about.inning, result.awayScore, result.homeScore, matchup.batter.id, matchup.batter.fullName, 
                                        matchup.pitcher.fullName, matchup.pitcher.id, count.strikes.start, count.balls.start,
                                       matchup.batSide.code, matchup.pitchHand.code, count.outs.start, home_team, away_team, batting_team,
                                       fielding_team, result.event, atBatIndex, pitchNumber)
  # convert team names to formatted strings following filename format
  filtered <- filtered %>% dplyr::mutate(home_team = gsub(" ", "-", tolower(home_team)),
                                         away_team = gsub(" ", "-", tolower(away_team)))
  
  # Rename columns
  filtered <- filtered %>% dplyr::rename(stand = matchup.batSide.code, p_throws=matchup.pitchHand.code, batterID=matchup.batter.id,
                                         pitcherID=matchup.pitcher.id, outs_when_up=count.outs.start) %>% 
    mutate(count=paste(count.balls.start, count.strikes.start, sep="_")) %>% dplyr::select(-c(count.balls.start, count.strikes.start))
  return(filtered)
}

# IMPUTE MISSING DATA (rookies, players with insufficient data, etc.) with MEDIAN STATS

# GET MEDIAN STATISTICS
# Average exit velocity by hitter
median_ev <- SavantData19 %>% drop_na(launch_speed) %>% dplyr::summarise(median_EV = median(launch_speed))

# Launch angle by hitter
median_angle <- SavantData19 %>% drop_na(launch_angle) %>% dplyr::summarise(median_angle = median(launch_angle))

# EV allowed -- pitcher
median_ev_allowed <- median_ev

# Launch angle allowed -- pitcher
median_angle_allowed <- median_angle

# average Release speed -- pitcher
median_velocity <- SavantData19 %>% drop_na(release_speed) %>% dplyr::summarise(median_velo = median(release_speed))

# merge data data
merge_stats <- function(live_data) {
  # filter
  filtered <- filter_statcast_game(live_data)
  # merge hitter data
  merge1 <- left_join(filtered, hitters_df_all %>% dplyr::select(batter, avg_EV, avg_angle), by=c("batterID"="batter"))
  # merge pitcher data
  merge2 <- left_join(merge1, pitchers_df_all %>% dplyr::select(pitcher, avg_EV_allowed, avg_angle_allowed, avg_velo), by=c("pitcherID"="pitcher"))
  
  return(merge2)
}

# predict pitch
predict_pitch <- function(live_game_data, live=TRUE) {
  
  # merge
  if(live==TRUE){
    play <- merge_stats(live_game_data)[1,] # only top record if live
  }
  
  else {
    play <- merge_stats(live_game_data) # all game records if not live
    # remove invalid counts
    play <- play %>% filter(!count %in% c("4_0", "4_1", "4_2", "0_3", "1_3", "2_3", "3_3"))
  }
  
  # impute missing values
  # Replace NAs
  play$avg_EV[is.na(play$avg_EV)] <- as.numeric(median_ev)
  play$avg_angle[is.na(play$avg_angle)] <- as.numeric(median_angle)
  play$avg_EV_allowed[is.na(play$avg_EV_allowed)] <- as.numeric(median_ev_allowed)
  play$avg_angle_allowed[is.na(play$avg_angle_allowed)] <- as.numeric(median_angle_allowed)
  play$avg_velo[is.na(play$avg_velo)] <- as.numeric(median_velocity)
  
  new_pred <- predict(final_model_fit, newdata=play, type = "response")
  # data frame with useful info and prediction
  pred_df <- data.frame(play, new_pred)
  return(pred_df)
}

# run this code for live games **only works if there are live games (set detailedState=="Final" in line 436 if no live games)
live_game_data <- get_live_probs("2022-08-03", "Minnesota Twins")
live_pitch <- predict_pitch(live_game_data)
all_live_pitches <- rbind(all_live_pitches, live_pitch)
####

# Retroactive testing
random_dates <- c("2022-04-25", "2022-04-29", "2022-05-16", "2022-07-01", "2022-07-17")

# get game packs from random date
game_pks_random_date <- baseballr::get_game_pks_mlb(date=random_dates[4], level_ids = 1) %>% pull(game_pk)

# Pick random game from random date
random_game <- sample(game_pks_random_date, 1)

# Get pbp data from that game
pbp_random <- tibble(baseballr::get_pbp_mlb(game_pk = random_game) %>% filter(type=="pitch"))

# predict probs for random game
probs_random <- predict_pitch(pbp_random, live=FALSE) 

# remove duplicates
probs_random <- probs_random %>% distinct() %>% arrange(atBatIndex, pitchNumber) %>% mutate(pitchIndex = row_number())

# plot game hr probability
# title
game_title <- paste0(probs_random$batting_team, " vs. ", probs_random$fielding_team, " HR Probability by Pitch")[1]

# label index for conditionally annotated points
probs_random <- probs_random %>% mutate(labelIndex = ifelse(new_pred < .01, 1, 0))
# at-bat description
probs_random <- probs_random %>% mutate(ab_desc = paste0(matchup.batter.fullName, " ", result.event, " on a ", count, " count"))

# plots random game HR probability by pitch
ggplot(probs_random, aes(pitchIndex, new_pred)) + geom_line(color = "#454545", size=2) + theme_minimal() + 
  labs(title=game_title, x="Pitch Number", y="HR Probability", subtitle = random_dates[4]) + 
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), text = element_text(size=17)) +
  geom_text(aes(pitchIndex, new_pred, label=ab_desc), data = probs_random[probs_random$labelIndex==1,],
            nudge_x = -25, nudge_y = .005, fontface="bold", color="#EF4444", size=5)
