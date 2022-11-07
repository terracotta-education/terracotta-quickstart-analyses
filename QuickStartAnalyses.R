## ----------------------------------
## QUICKSTARTANALYSES.R
## ----------------------------------
# 
# This R script provides examples of how one might analyze data that are
# exported from an experiment in Terracotta.  The experiment that produced
# these data is described in Terracotta's "Quick Start Guide," available at
# https://terracotta.education/help-center/quick-start-guide
#
# Detailed documentation about the Terracotta data export is available at
# https://terracotta.education/help-center/data-dictionary
#
# This is a simulated experiment with only 5 imaginary participants, and 
# only 4 of these participants have outcome scores.  We want to emphasize
# that these data are not real, and the results should not be interpreted
# as research findings.  The purpose of this script is only to illustrate 
# how analyses might be carried out on data exported from Terracotta.
#
# November 7, 2022

# Clean-up
rm(list=ls())

# Required Libraries
require(tidyverse)
require(jsonlite)
require(rstatix)

## ----------------------------------
## IMPORT THE TERRACOTTA EXPORT
## ----------------------------------

# Make a temporary directory for eventually unzipping the data
# This will help us avoid dealing with local paths
tempdir <- tempdir()

# For this script, we will download the data from a GitHub repository on the
# web.  This may also be appropriate if you are storing your data on OSF or 
# some other institutional data repository.  Data exports from Terracotta are 
# de-identified, and if using consent, data from students who did not provide
# consent are excluded.
dataurl <- 'https://github.com/terracotta-education/terracotta-quickstart-analyses/raw/main/Terracotta%20Quick%20Start%20Guide%20Experiment%20Export.zip'
tempfile <- tempfile(fileext = ".zip")
download.file(dataurl, tempfile)
unzip(tempfile, exdir = tempdir, junkpaths = TRUE)
# NOTE: If you have the data on your local machine in the working directory,
# you can read and unzip the data export zip file directly, like this:
# datapath <- 'Terracotta Quick Start Export.zip' # path to the zip export
# unzip(datapath, exdir = tempdir)

# Read each of the CSV files from the temporary directory into the environment
experiment <- read.csv(file.path(tempdir,"experiment.csv"),stringsAsFactors = FALSE)
item_responses <- read.csv(file.path(tempdir,"item_responses.csv"),stringsAsFactors = FALSE)
items <- read.csv(file.path(tempdir,"items.csv"),stringsAsFactors = FALSE)
outcomes <- read.csv(file.path(tempdir,"outcomes.csv"),stringsAsFactors = FALSE)
participant_treatment <- read.csv(file.path(tempdir,"participant_treatment.csv"),stringsAsFactors = FALSE)
participants <- read.csv(file.path(tempdir,"participants.csv"),stringsAsFactors = FALSE)
response_options <- read.csv(file.path(tempdir,"response_options.csv"),stringsAsFactors = FALSE)
submissions <- read.csv(file.path(tempdir,"submissions.csv"),stringsAsFactors = FALSE)
# Read the events.json file 
events <- read_json(file.path(tempdir,"events.json"))

# Clean up the temporary file and directory, optional
# file.remove(tempfile)
# unlink(tempdir, recursive = T)
rm(tempdir, tempfile, dataurl)

## ----------------------------------
## DESCRIPTIVE ANALYSES
## ----------------------------------

# NUMBER OF PARTICIPANTS
# How many participants are included in the export?
length(unique(participants$participant_id))
# How many participants made submissions to each assignment?
assignment_names <- participant_treatment %>% select(assignment_id,assignment_name) %>% unique()
merge(submissions, assignment_names, by="assignment_id") %>% group_by(assignment_name) %>% summarize(n = length(unique(participant_id)))
# How many participants have scores for each outcome?
outcomes %>% group_by(outcome_name) %>% summarize(n = length(unique(participant_id)))
# NOTE: In this experiment, there are 5 consenting students who each
# submitted assignments, however, only 4 of them have outcome scores.

# DURATION OF TIME ON ASSIGNMENTS
# The time spent on assignments can be estimated as the time duration between
# starting an assignment and submitting that assignment.  The time of events
# are recorded in the events.json file, which we've imported as a list.
# These data are formatted according to the IMS Global (1EdTech) Caliper 
# standard, described at http://www.imsglobal.org/activity/caliper
# When dealing with the events, I find that it's easiest to loop through each
# event, and to extract the information you want from each event.
# First, initiate an empty data frame that will hold the relevant information
events_df <- data.frame(participant_id=integer(),
                        object=character(),
                        action=character(),
                        eventTime=character(), # we'll convert it to a datetime later
                        assignment_id=integer(),
                        exposure_id=integer(),
                        treatment_id=integer())
# Next, loop through the events, adding new rows to the empty data frame
for (i in 1:length(events)) {
  object <- events[[i]]$data[[1]]$object$type
  # Make sure we're measuring details from only "Assessment" events.  Other
  # event types, such as those corresponding to the Media profile, will have
  # a different structure, with different fields
  if (object == "Assessment") { 
    participant_id <- events[[i]]$data[[1]]$membership$member$extensions$terracotta_participant_id
    object <- events[[i]]$data[[1]]$object$type
    action <- events[[i]]$data[[1]]$action
    eventTime <- events[[i]]$data[[1]]$eventTime
    assignment_id <- events[[i]]$data[[1]]$generated$assignable$extensions$terracotta_assignment_id
    exposure_id <- events[[i]]$data[[1]]$generated$assignable$extensions$terracotta_exposure_id
    treatment_id <- events[[i]]$data[[1]]$generated$assignable$extensions$terracotta_treatment_id
    newrow <- data.frame(participant_id,object,action,eventTime,assignment_id,exposure_id,treatment_id)
    events_df <- rbind(events_df,newrow) 
  }
}
# Clean-up
rm(participant_id,object,action,eventTime,assignment_id,exposure_id,treatment_id,newrow)
# Turn the eventTime character string into an actual datetime, as promised
events_df <- events_df %>% mutate(eventTime = as.POSIXct(strptime(eventTime, "%Y-%m-%dT%H:%M:%S")))
# Now that we have the event data in a more manageable format, we need to 
# go through the list of events, and extract the latest start (or restart)
# prior to every submission event, for every participant.
# Start by making a container
durations <- data.frame(participant_id=integer(),
                        assignment_id=integer(),
                        treatment_id=integer(),
                        start_time=as.POSIXct(character()),
                        submit_time=as.POSIXct(character()),
                        duration=numeric())
# For every participant 
for (p in unique(events_df$participant_id)) {
  # For every assignment
  for (a in unique(events_df$assignment_id)) {
    these_events <- events_df %>% filter(participant_id == p, assignment_id == a) %>% arrange(eventTime)
    start_time <- NA
    submit_time <- NA
    # Loop through events
    for (e in 1:nrow(these_events)) {
      if (these_events$action[e] == "Started" || these_events$action[e] == "Restarted") {
        submit_time <- NA
        start_time <- these_events$eventTime[e]
      }
      if (these_events$action[e] == "Submitted" && !(is.na(start_time))) {
        submit_time <- these_events$eventTime[e]
        duration <- as.numeric(difftime(submit_time,start_time),units = "secs") ## DURATIONS IN SECONDS
        treatment_id <- these_events$treatment_id[e]
        newrow <- data.frame(participant_id=p,assignment_id=a,treatment_id,start_time,submit_time,duration)
        durations <- rbind(durations,newrow)
        start_time <- NA
        submit_time <- NA
      }
    }
  }
}
# Summarize durations
condition_names <- participant_treatment %>% select(treatment_id,assignment_name,condition_name)
merge(durations,condition_names) %>% group_by(assignment_name,condition_name) %>% summarize(mean_duration = mean(duration,na.rm=TRUE))

## ----------------------------------------
## COMPARING DIFFERENCES BETWEEN CONDITIONS
## ----------------------------------------
# This was a within-subject design, where each participant was exposed to 
# each conditions, just at different times.  The time when a participant 
# experiences a condition is called an "exposure set."  The details of 
# how different assignments are mapped onto different conditions, for different
# participants, at different times, is in the file called 
# "participant_treatment."  For convenience, this file also contains the score
# a participant received for each treatment assignment.

# Show the scores on the assignments
participant_treatment %>% 
  ggplot(aes(x=assignment_name, y=final_score, colour=condition_name)) +
  geom_point(position=position_jitterdodge())
# Calculate the average scores on the assignments
participant_treatment %>% 
  group_by(assignment_name,condition_name) %>% summarize(mean = mean(final_score,na.rm=TRUE),
                                                         sd = sd(final_score,na.rm=TRUE))
# Conduct a repeated measures ANOVA
aov.result <- anova_test(
  data = participant_treatment, dv = final_score, wid = participant_id,
  within = condition_name
)
get_anova_table(aov.result)

# In the steps above, we're examining the effect of condition on assignment
# scores.  Additionally, if outcome scores have been added to the experiment,
# we can examine the effect of condition on study outcomes.  These are in the
# file "outcomes."

# Show the scores on the outcomes
outcomes %>% ggplot(aes(x=outcome_name, y=outcome_score, colour=condition_name)) +
  geom_point(position=position_jitterdodge())
# Calculate the average scores on the outcomes
outcomes %>% 
  group_by(outcome_name,condition_name) %>% summarize(mean = mean(outcome_score,na.rm=TRUE),
                                                      sd = sd(outcome_score,na.rm=TRUE))
# Conduct a repeated measures ANOVA
aov.result <- anova_test(
  data = outcomes, dv = outcome_score, wid = participant_id,
  within = condition_name
)
get_anova_table(aov.result)

# A note about statistics: ANOVAs are among the most conventional ways to
# perform statistical analysis.  However, they have assumptions that are 
# rarely true of educational data.  For example, scores on tests and assignments
# are often negatively skewed and bounded between a minimum and a maximum score.
# Please use caution when conducting statistical tests.