## script to monitor a folder (and sub folders) for changes to files
## and to send a text message alert if file changes do not meet 
## the specified criteria.

## Use Rstudio addin for cronR (mac) or taskscheduleR (windows) to schedule running this script

## Use twilio (web service with adequate free trial mode)
## to send text messages from R

library(twilio)
library(fs) ## for getting file information
library(lubridate) ## for working with dates

## Give Twilio account SID and token
Sys.setenv(TWILIO_SID = "get from twilio account")
Sys.setenv(TWILIO_TOKEN = "get from twilio account")

## Givetore relevant phone numbers
owen_phone_number <- "+41xxxxxxxxx"
yves_phone_number <- "+41xxxxxxxxx"
twilios_phone_number <- "+41xxxxxxxxx"

## get file info
file_info <- dir_info(path="/Users/owenpetchey/Desktop/monitor_this_folder", recurse = TRUE)

## now comes code specific to alerting if time since last change exceeds a threshold:
## alert if time since last chance exceeds threshold
max_time_since_last_change <- 60 ## minutes

## get most recent modification
time_of_latest_change <- max(file_info$change_time)

## time in minutes since last change
time_since_last <- as.duration(time_of_latest_change %--% Sys.time())/dminutes(1)

## print for the schedule log
print(time_since_last)

## Check if alert should be sent, send one if so
if((Sys.time() - time_of_latest_change) > max_time_since_last_change) {
  alert_message <- paste0("From Oxygen recording computer: no file change during previous ",
                          time_since_last, "minutes.")
  tw_send_message(from = twilios_phone_number, to = yves_phone_number, 
                body = alert_message)
}