---
title: "Epilepsy Bullying Internalising up to 16"
output: html_document
date: "2025-04-27"
---
Rename and recode existing variables from the NCDS dataset (0 to 16 years).

Please note that the code creates new recoded variables based on a recoding_rules list and deletes original variables. This part of the code can be optioned out if original variables need to be retained.

```{r echo=FALSE, message=FALSE, warning=FALSE}
library(haven)
library(dplyr)
library(tidyverse)
library(knitr)
library(VIM)
library(ggplot2)
library(naniar)
library(mice)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(glmnet)
library(psych)
library(MatchIt)
library(cobalt)
library(lme4)
library(lmerTest)
library(performance)
library(usdm)
library(boot)

###########################################################
#
# Load data
#
###########################################################

setwd("/Users/emmablundell/Library/Mobile Documents/com~apple~CloudDocs/DClinPsy/Thesis/NCDS Datasets for R")

ncds_0to16 <- read_spss("ncds0123.sav") #NCDS1 survey data

###########################################################
#
# Create Renaming and Recoding Rules for Variables
#
###########################################################

# Create centralised recoding rules
recoding_rules <- list(

  # Sex Variable
  sex_birth = list(
    variable = "n622",  # Original variable name
    recode_map = c('1' = 0, '2' = 1, '-1' = NA_real_),  # Recode map for numeric values
    factor_levels = c(0, 1),  # Not necessary for numeric, but keeping the levels here for reference
    factor_labels = c("Male", "Female"),  # Not necessary for numeric, but keeping labels for reference
    value_meaning = "0 = Male, 1 = Female",  # Explanation of values
    description = "Sex of the child at birth"  # Description of the variable
  ),

  # Epilepsy Status at Age 7
  epilepsy_status_7 = list(
    variable = "n1842",  # Original variable name
    recode_map = c(
        '1' = 0,   # No convulsion disorder -> 0
        '2' = 0,   # Indefinite convulsion disorder -> 0
        '3' = 0,   # Faints -> 0
        '4' = 0,   # Hysterical attack -> 0
        '5' = 1,   # Yes (epilepsy) -> 1
        '6' = 1,   # Yes (unsubstantiated epilepsy) -> 1
        '7' = 0,   # Febrile convulsions - probably -> 0
        '8' = 0,   # Febrile convulsions - possibly -> 0
        '9' = 0,   # Not diagnosed with epilepsy -> 0
        '10' = 0,  # Febrile convulsions -> 0
        '11' = 0,  # Breath-holding -> 0
        '12' = 0,  # Blank-spells -> 0
        '-1' = 0,  # Not epilepsy study -> "0"
        '-2' = NA_real_  # No NCDS1, NCDS2 -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No Epilepsy", "Epilepsy"),
    description = "Epilepsy status as reated by medical professional at age 7"
  ),

  # Epilepsy severity (nb separate to epilepsy substudy) at Age 7
  epilepsy_severity_7 = list(
    variable = "n415",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '2' = 0,          # None -> 0
        '3' = 1,          # Yes, no handicap -> 1
        '4' = 1,          # Yes, slight handicap -> 1
        '5' = 2,          # Yes, moderate handicap -> 2
        '6' = 2,          # Yes, severe handicap -> 2
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("No epilepsy at 7", "none or mild handicap from epilepsy at 7", "moderate or severe handicap from epilepsy at 7"),
    description = "Epilepsy severity rated by medical professional at age 7"
  ),

  # Asthma status at Age 7
  asthma_status_7 = list(
    variable = "n1850",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '2' = 1,          # Yes
        '3' = 0,          # No
        '-1' = NA_real_,   # Missing -> NA (missing value)
        '-2' = NA_real_  # Missing -> no parental
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No asthma at 7", "Asthma at 7"),
    description = "Asthma rated by medical professional at age 7"
  ),

   # Asthma severity at Age 7
  asthma_severity_7 = list(
    variable = "n1816",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Asthma, no detail -> NA (missing value)
        '8' = 0,          # Not asthmatic -> 0
        '2' = 1,          # Mild, one attack -> 1
        '3' = 1,          # Mild -> 1
        '4' = 1,          # Mild, worse in past -> 1
        '5' = 2,          # Moderate
        '6' = 2,          # Moderate, worse in past -> 2
        '7' = 2,          # Severe -> 2
        '-1' = NA_real_,   # Missing -> NA (missing value)
        '-2' = NA_real_   # No parental -> NA
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("No asthma at 7", "mild asthma at 7", "moderate or severe asthma at 7"),
    description = "Asthma severity rated by medical professional at age 7"
  ),

   # Cerebral Palsy at Age 7
  cerebral_palsy_7 = list(
    variable = "n367",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Other -> missing NA
        '3' = 0,   # No -> 0
        '4' = 1,   # Spastic 4 limbs -> 1
        '5' = 1,   # Hemiplegia -> 1
        '6' = 1,   # Monoplegia - upper -> 1
        '7' = 1,   # Monoplegia - lower -> 1
        '8' = 1,   # Spastic - upper -> 1
        '9' = 1,   # Spastic - lower -> 1
        '10' = 1,  # Athetosis -> 1
        '11' = 1,  # Athetosis alone -> 1
        '-1' = NA_real_  # Missing -> NA
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No Cerebral Palsy", "Cerebral Palsy"),
    description = "Cerebral Palsy status as rated by medical professional at age 7"
  ),

   # Tics or Habit Spasms at Age 7
  tics_7 = list(
    variable = "n368",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '2' = 1,          # Yes -> 1
        '3' = 0,          # No -> 0
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No tics at 7", "Tics at 7"),
    description = "Tics or habit spasms as rated my medical professional at age 7"
  ),

  # 'Intellectual disability at Age 7
  Intellectual_disability_7 = list(
    variable = "n403",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '2' = 0,          # None -> 0
        '3' = 1,          # Yes, no handicap -> 1
        '4' = 1,          # Yes, slight handicap -> 1
        '5' = 1,          # Yes, moderate handicap -> 1
        '6' = 1,          # Yes, severe handicap -> 1
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No intellectual disability", "Intellectual disability"),
    description = "'Mental retardation' rated by medical professional at age 7"
  ),

  # Difficulty getting to sleep at Age 7
  Difficulty_getting_to_sleep_7 = list(
    variable = "n127",  # Original variable name
    recode_map = c(
        `1` = NA_real_,   # Don't know -> NA (missing value)
        '2' = 1,          # Yes -> 1
        '3' = 0,          # No -> 0
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No difficulty getting to sleep at 7", "Difficulty getting to sleep at 7"),
    description = "Difficulty getting to sleep in last 3 months rated by parent at age 7"
  ),

  # Sleepwalking at Age 7
  Sleepwalking_7 = list(
    variable = "n128",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '2' = 1,          # Yes -> 1
        '3' = 0,          # No -> 0
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No sleepwalking at 7", "Sleepwalking at 7"),
    description = "Sleepwalking in last 3 months rated by parent at age 7"
  ),

  # Bad dreams or night terrors at Age 7
  Night_terrors_7 = list(
    variable = "n126",  # Original variable name
    recode_map = c(
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '2' = 1,          # Yes -> 1
        '3' = 0,          # No -> 0
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No night terrors at 7", "Night terrors at 7"),
    description = "Bad dreams or night terrors in last 3 months rated by parent at age 7"
  ),

  # Sleepwalking at Age 11
  Sleepwalking_11 = list(
    variable = "n1446",  # Original variable name
    recode_map = c(
        '1' = 1,         # Yes -> 1
        '2' = 0,         # No -> 0
        '3' = NA_real_,  # Don't know -> NA (missing value)
        '9' = 0,         # Inapplicable -> 0
        '-1' = NA_real_  # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No sleepwalking at 11", "Sleepwalking at 11"),
    description = "Sleepwalking in last 3 months rated by parent at age 11"
  ),

  # Bad dreams or night terrors at Age 11
  Night_terrors_11 = list(
    variable = "n1445",  # Original variable name
    recode_map = c(
        '1' = 1,          # Yes -> 1
        '2' = 0,          # No -> 0
        '3' = NA_real_,   # Don't know -> NA (missing value)
        '9' = 0,          # Inapplicable -> 0
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1),
    factor_labels = c("No night terrors at 11", "Night terrors at 11"),
    description = "Bad dreams or night terrors in last 3 months rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 1. difficulty settling
  Rutter_A_1_7 = list(
    variable = "n133",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never has difficulty settling", "has difficulty settling sometimes", "has     difficulty settling frequently"),
    description = "Rutter A Item 1 - Difficulty settling as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 2. prefers doing things alone
  Rutter_A_2_7 = list(
    variable = "n134",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never prefers to do things alone", "prefers to do things alone sometimes", "prefers to do things alone frequently"),
    description = "Rutter A Item 2 - Prefers doing things alone as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 3. bullied by other kids
  Rutter_A_3_7 = list(
    variable = "n135",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never bullied by other kids", "bullied by other kids sometimes", "bullied by other kids frequently"),
    description = "Rutter A Item 3 - Bullied by other kids as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 4. generally destructive
  Rutter_A_4_7 = list(
    variable = "n136",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never generally destructive", "is sometimes generally destructive", "is frequently generally destructive"),
    description = "Rutter A Item 4 - Destroys own or others belongings as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 5. miserable or tearful
  Rutter_A_5_7 = list(
    variable = "n137",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never miserable or tearful", "sometimes miserable or tearful", "frequently miserable or tearful"),
    description = "Rutter A Item 5 - Miserable or tearful as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 6. squirmy or fidgety
  Rutter_A_6_7 = list(
    variable = "n138",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never squirmy or fidgety", "sometimes squirmy or fidgety", "frequently squirmy or fidgety"),
    description = "Rutter A Item 6 - Squirmy or fidgety as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 7. Worries about many things
  Rutter_A_7_7 = list(
    variable = "n139",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never worries", "sometimes worries", "frequently worries"),
    description = "Rutter A Item 7 - Worries about many things as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 8. irritable
  Rutter_A_8_7 = list(
    variable = "n140",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never irritable", "sometimes irritable", "frequently irritable"),
    description = "Rutter A Item 8 - Irritable as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 9. sucks thumb
  Rutter_A_9_7 = list(
    variable = "n141",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never sucks thumb", "sometimes sucks thumb", "frequently sucks thumb"),
    description = "Rutter A Item 9 - Sucks thumb as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 10. upset by new situations
  Rutter_A_10_7 = list(
    variable = "n142",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never upset by new situations", "sometimes upset by new situations", "frequently upset by new       situations"),
    description = "Rutter A Item 10 - Upset by new situations as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 11. twitches
  Rutter_A_11_7 = list(
    variable = "n143",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never twitches", "sometimes twitches", "frequently twitches"),
    description = "Rutter A Item 11 - Twitches and mannerisms as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 12. fights with other children
  Rutter_A_12_7 = list(
    variable = "n144",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never fights with other children", "sometimes fights with other children", "frequently fights       with other children"),
    description = "Rutter A Item 12 - Fights with other children as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 13. bites nails
  Rutter_A_13_7 = list(
    variable = "n145",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never bites nails", "sometimes bites nails", "frequently bites nails"),
    description = "Rutter A Item 13 - Bites nails as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 14. disobedient
  Rutter_A_14_7 = list(
    variable = "n146",  # Original variable name
    recode_map = c(
        '2' = 2,          # Frequently -> 2
        '3' = 1,          # Sometimes -> 1
        '4' = 0,          # Never -> 0
        '1' = NA_real_,   # Don't know -> NA (missing value)
        '-1' = NA_real_   # Missing -> NA (missing value)
    ),
    factor_levels = c(0, 1, 2),
    factor_labels = c("Never disobedient", "sometimes disobedient", "frequently disobedient"),
    description = "Rutter A Item 14 - Disobedient as rated by parent at age 7"
  ),

  # Rutter scale A (parent-report) - 1. Difficulty settling at age 11
  Rutter_A_1_11 = list(
  variable = "n1447",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never has difficulty settling", "has difficulty settling sometimes", "has difficulty   settling     frequently"),
  description = "Rutter A Item 1 - Difficulty settling as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 2. Prefers doing things alone at age 11
  Rutter_A_2_11 = list(
  variable = "n1448",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never prefers to do things alone", "prefers to do things alone sometimes", "prefers    to do        things alone frequently"),
  description = "Rutter A Item 2 - Prefers doing things alone as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 3. Bullied by other kids at age 11
  Rutter_A_3_11 = list(
  variable = "n1449",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never bullied by other kids", "bullied by other kids sometimes", "bullied by other     kids         frequently"),
  description = "Rutter A Item 3 - Bullied by other kids as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 4. Generally destructive at age 11
  Rutter_A_4_11 = list(
  variable = "n1450",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never generally destructive", "is sometimes generally destructive", "is frequently     generally    destructive"),
  description = "Rutter A Item 4 - Destroys own or others belongings as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 5. Miserable or tearful at age 11
  Rutter_A_5_11 = list(
  variable = "n1451",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never miserable or tearful", "sometimes miserable or tearful", "frequently miserable   or           tearful"),
  description = "Rutter A Item 5 - Miserable or tearful as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 6. Squirmy or fidgety at age 11
  Rutter_A_6_11 = list(
  variable = "n1452",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never squirmy or fidgety", "sometimes squirmy or fidgety", "frequently squirmy or      fidgety"),
  description = "Rutter A Item 6 - Squirmy or fidgety as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 7. Worries about many things at age 11
  Rutter_A_7_11 = list(
  variable = "n1453",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never worries", "sometimes worries", "frequently worries"),
  description = "Rutter A Item 7 - Worries about many things as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 8. Irritable at age 11
  Rutter_A_8_11 = list(
  variable = "n1454",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never irritable", "sometimes irritable", "frequently irritable"),
  description = "Rutter A Item 8 - Irritable as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 9. Sucks thumb at age 11
  Rutter_A_9_11 = list(
  variable = "n1455",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never sucks thumb", "sometimes sucks thumb", "frequently sucks thumb"),
  description = "Rutter A Item 9 - Sucks thumb as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 10. Upset by new situations at age 11
  Rutter_A_10_11 = list(
  variable = "n1456",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never upset by new situations", "sometimes upset by new situations", "frequently       upset by     new situations"),
  description = "Rutter A Item 10 - Upset by new situations as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 11. Twitches and mannerisms at age 11
  Rutter_A_11_11 = list(
  variable = "n1457",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never twitches", "sometimes twitches", "frequently twitches"),
  description = "Rutter A Item 11 - Twitches and mannerisms as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 12. Fights with other children at age 11
  Rutter_A_12_11 = list(
  variable = "n1458",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never fights with other children", "sometimes fights with other children",             "frequently   fights with other children"),
  description = "Rutter A Item 12 - Fights with other children as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 13. Bites nails at age 11
  Rutter_A_13_11 = list(
  variable = "n1459",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never bites nails", "sometimes bites nails", "frequently bites nails"),
  description = "Rutter A Item 13 - Bites nails as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 14. Disobedient at age 11
  Rutter_A_14_11 = list(
  variable = "n1460",  # Variable name for age 11
  recode_map = c(
    '3' = 2,          # Frequently -> 2
    '2' = 1,          # Sometimes -> 1
    '1' = 0,          # Never -> 0
    '9' = 0,          # Inapplicable -> 0
    '4' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # Missing -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never disobedient", "sometimes disobedient", "frequently disobedient"),
  description = "Rutter A Item 14 - Disobedient as rated by parent at age 11"
  ),

  # Rutter scale A (parent-report) - 1. Cannot settle to do things at age 16
  Rutter_A_1_16 = list(
  variable = "n2530",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never cannot settle", "somewhat cannot settle", "certainly cannot settle"),
  description = "Rutter A - Cannot settle to do things as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 2. Rather solitary at age 16
  Rutter_A_2_16 = list(
  variable = "n2523",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never rather solitary", "somewhat rather solitary", "certainly rather solitary"),
  description = "Rutter A - Rather solitary as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 3. Not liked by others at age 16
  Rutter_A_3_16 = list(
  variable = "n2521",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never not liked by others", "somewhat not liked by others", "certainly not liked by others"),
  description = "Rutter A - Not much liked by others as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 4. Often destroys belongings at age 16
  Rutter_A_4_16 = list(
  variable = "n2519",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never destroys belongings", "somewhat destroys belongings", "certainly destroys belongings"),
  description = "Rutter A - Often destroys belongings as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 5. Miserable or unhappy at age 16
  Rutter_A_5_16 = list(
  variable = "n2525",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never miserable", "somewhat miserable or unhappy", "certainly miserable or unhappy"),
  description = "Rutter A - Appears miserable or unhappy as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 6. Squirmy or fidgety at age 16
  Rutter_A_6_16 = list(
  variable = "n2518",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never squirmy or fidgety", "somewhat squirmy or fidgety", "certainly squirmy or      fidgety"),
  description = "Rutter A - Squirmy or fidgety as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 7. Often worried at age 16
  Rutter_A_7_16 = list(
  variable = "n2522",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never worries", "somewhat worries", "certainly worries"),
  description = "Rutter A - Often worried as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 8. Irritable, quick to anger at age 16
  Rutter_A_8_16 = list(
  variable = "n2524",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never irritable", "somewhat irritable", "certainly irritable"),
  description = "Rutter A - Irritable or quick to anger as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 9. Sucks thumb at age 16
  Rutter_A_9_16 = list(
  variable = "n2527",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never sucks thumb", "somewhat sucks thumb", "certainly sucks thumb"),
  description = "Rutter A - Sucks thumb as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 10. Fearful/afraid of new things at age 16
  Rutter_A_10_16 = list(
  variable = "n2531",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never upset by new things", "somewhat upset by new things", "certainy upset by new things"),
  description = "Rutter A - Upset by new things as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 11. Twitches and mannerisms at age 11
  Rutter_A_11_16 = list(
  variable = "n2526",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never twitches", "somewhat twitches", "certainly twitches"),
  description = "Rutter A - Twitches and mannerisms as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 12. Fights with others at age 16
  Rutter_A_12_16 = list(
  variable = "n2520",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never fights with others", "somewhat fights with others", "frequently fights with others"),
  description = "Rutter A - Fights with others as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 13. Bites nails at age 16
  Rutter_A_13_16 = list(
  variable = "n2528",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never bites nails", "somewhat bites nails", "certainly bites nails"),
  description = "Rutter A - Bites nails as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 14. Disobedient at age 16
  Rutter_A_14_16 = list(
  variable = "n2529",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never disobedient", "somewhat disobedient", "certainly disobedient"),
  description = "Rutter A - Disobedient as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 15. Restless at age 16
  Rutter_A_15_16 = list(
  variable = "n2517",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never restless", "somewhat restless", "certainly restless"),
  description = "Rutter A - Restless as rated by parent at age 16"
  ),

  # Rutter scale A (parent-report) - 16. Fussy or overparticular at age 16
  Rutter_A_16_16 = list(
  variable = "n2532",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never fussy", "somewhat fussy", "certainly fussy"),
  description = "Rutter A - Fussy or overparticular as rated by parent at age 16"
  ),

   # Rutter scale A (parent-report) - 17. Lies at age 16
  Rutter_A_17_16 = list(
  variable = "n2533",  # Variable name for age 16
   recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never lies", "somewhat lies", "certainly lies"),
  description = "Rutter A - Lies as rated by parent at age 16"
  ),

   # Rutter scale A (parent-report) - 18. Bullies others at age 16
  Rutter_A_18_16 = list(
  variable = "n2534",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never bullies", "somewhat bullies", "certainly bullies"),
  description = "Rutter A - Bullies others as rated by parent at age 16"
  ),

   # Rutter scale B (parent-report) - 18. Bullies others at age 16
  Rutter_B_1_16 = list(
  variable = "n2321",  # Variable name for age 16
    recode_map = c(
    '3' = 2,          # Certainly applies -> 2
    '2' = 1,          # Applies somewhat -> 1
    '1' = 0,          # Doesn't apply -> 0
    '-1' = NA_real_   # No questionnaire -> NA (missing value)
  ),
  factor_levels = c(0, 1, 2),
  factor_labels = c("Never bullies", "somewhat bullies", "certainly bullies"),
  description = "Rutter A - Bullies others as rated by teacher at age 16"
  ),

  # Father's occupation (for SES variable)
  Paternal_social_class_birth = list(
  variable = "n236",  # Original variable name
  recode_map = c(
    '2' = 1,          # Class I (non manual) -> 1
    '3' = 1,          # Class II (non manual) -> 1
    '4' = 1,          # Class III (non manual) -> 1
    '5' = 0,          # Class III (manual) -> 0
    '6' = 0,          # Class IV (manual) -> 0
    '7' = 0,          # Class V (manual) -> 0
    '1' = NA_real_,   # Unemployed/sick -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("paternal occupation = manual", "paternal occupation = non manual"),
  description = "Paternal social class - occupation at child's birth"
  ),

# Mother's education (for SES variable)
  Mother_education_birth = list(
  variable = "n537",  # Original variable name
  recode_map = c(
    '1' = 0,          # Did not stay - 25+ -> 0
    '2' = 0,          # Did not stay - 24- -> 0
    '3' = 1,          # Did stay - 25+ -> 1
    '4' = 1,          # Did stay - 24- -> 1
    '5' = 0,          # Did not stay -> 0
    '6' = 1,          # Did stay -> 1
    '7' = NA_real_,   # 25+ min age 14 years -> NA (missing value)
    '8' = NA_real_,   # 24- min age 15 years -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("mother not educated past age 16", "mother educated past age 16"),
  description = "Maternal social class - maternal education at child's birth"
  ),

  # Divorce, separation or desertion as rated by health visitor at age 7
  Parent_separation_7 = list(
  variable = "n321",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("no parental divorce, separation or desertion at age 7", "parental divorce, separation or desertion at age 7"),
  description = "Parental divorce, separation, or desertion as rated by health visitor at age 7 "
  ),

  # Death of mother as rated by health visitor at age 7 (for ACEs variable)
  Death_mother_7 = list(
  variable = "n320",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("no death of mother by age 7", "death of mother by age 7"),
  description = "Death of mother as rated by health visitor at age 7 "
  ),

  # Death of father as rated by health visitor at age 7 (for ACEs variable)
  Death_father_7 = list(
  variable = "n319",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("no death of father by age 7", "death of father by age 7"),
  description = "Death of father as rated by health visitor at age 7 "
  ),

  # Parental alcohol use as rated by health visitor at age 7 (for ACEs variable)
  Parent_alcoholism_7 = list(
  variable = "n325",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("no parental alcoholism at age 7", "parental alcoholism at age 7"),
  description = "Parental alcoholism as rated by health visitor at age 7 "
  ),

# Financial difficulties as rated by health visitor at age 7 (for ACEs variable)
  financial_difficulties_7 = list(
  variable = "n315",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("no financial difficulties at age 7", "financial difficulties at age 7"),
  description = "Financial difficulties as rated by health visitor at age 7 "
  ),

  # Family contact with mental health worker at age 7 (for ACEs variable)
  Family_mental_illness_7 = list(
  variable = "n310",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("No family contact with mental health worker at age 7", "Family contact with mental health worker at age 7"),
  description = "Family contact with mental health worker as rated by health visitor at age 7 "
  ),

  # Family contact with probation officer as rated by health visitor at age 7 (for ACEs variable)
  Family_probation_officer_7 = list(
  variable = "n307",  # Original variable name
  recode_map = c(
    '2' = 1,          # Yes -> 1
    '3' = 0,          # No -> 0
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("no family contact with probation officer by age 7", "Family contact with probation officer by age 7"),
  description = "Family contact with probation officer as rated by health visitor at age 7 "
  ),

  # Mother's marital status at birth
  Mother_marital_status_birth = list(
    variable = "n545",
    recode_map = c(
      '1' = 0,  # Separated, divorced, widowed -> 0
      '2' = 0,  # Stable union -> 0
      '3' = 1,  # Twice married -> 1
      '4' = 1,  # Married -> 1
      '5' = 0,  # Unmarried -> 0
      '-1' = NA_real_  # Missing values -> NA
    ),
    factor_levels = c(0, 1),
    factor_labels = c("Unmarried, separated, divorced, or widowed", "Married or twice married"),
    description = "Mother's marital status at birth"
  ),

  # Child ever been in local authority care by age 7 (for ACEs variable)
  Ever_in_care_7 = list(
  variable = "n132",  # Original variable name
  recode_map = c(
    '2' = 1,          # Now in care -> 1
    '3' = 1,          # Care, past -> 1
    '4' = 0,          # Never in care -> 0
    '5' = 1,          # Now in voluntary care -> 1
    '6' = 1,          # Voluntary care, past -> 1
    '7' = 1,          # Now in care abroad -> 1
    '8' = 1,          # In care, quest -> 1
    '1' = NA_real_,   # Don't know -> NA (missing value)
    '-1' = NA_real_   # NA -> NA (missing value)
  ),
  factor_levels = c(0, 1),
  factor_labels = c("Never in care by age 7", "In care now/previously by age 7"),
  description = "Whether child has been in local authority care by age 7 as rated by parent"
  ),

# Signs of physical neglect as rated by teacher at age 7 (for ACEs variable)
  Physical_neglect_7 = list(
  variable = "n462",  # Original variable name
  recode_map = c(
    '1' = 0,          # Attractive -> 0
    '2' = 0,          # Unattractive -> 0
    '3' = 1,          # Looks underfed -> 1
    '4' = 0,          # Abnormal feature -> 0
    '5' = 1,          # Scruffy and dirty -> 0
    '-1' = NA_real_   # NA -> NA (missing value)
  ),

  factor_levels = c(0, 1),
  factor_labels = c("No signs of physical neglect at age 7", "Signs of physical neglect at age 7"),
  description = "Signs of physical neglect (underfed and/or scruffy and dirty) as rated by teacher at age 7 "
  )
)

###########################################################
#
# Apply Renaming and Recoding Rules
#
###########################################################

ncds_renamed <- ncds_0to16

for (rule_name in names(recoding_rules)) {
  rule <- recoding_rules[[rule_name]]

  # Apply the recoding based on whether the variable is numeric or not
 ncds_renamed <- ncds_renamed %>%
  mutate(
    !!rule_name := {
      var_data <- get(rule$variable) %>% zap_labels()  # Remove labels if present
      recoded_var <- recode(var_data, !!!rule$recode_map)  # Apply recoding
      # Ensure the result is numeric
      as.numeric(recoded_var)
    }
  ) %>%
    # Uncomment the following line if you want to remove the original variable after recoding
    select(-!!sym(rule$variable))  # Remove the original variable after recoding
}

###########################################################
#
# Create Metadata Table for Summary
#
###########################################################

# Create Metadata Table
recoding_metadata <- tibble(
  # Create the "Variable" column: Names of the recoding rules (new variable names).
  Variable = names(recoding_rules),

  # Original_Variable column: Extract the original variable name from each rule.
  Original_Variable = sapply(recoding_rules, function(r) r$variable),

  # Old_Values column: Concatenate the original codes before recoding (from recode_map).
  Old_Values = sapply(recoding_rules, function(r) paste(
    names(r$recode_map),  # Use the names of the recode_map as old values.
    collapse = ifelse(knitr::is_html_output(), "<br>", " \\\\")  # Line breaks for HTML or LaTeX.
  )),

  # New_Values column: Map old values to new ones with labels, if applicable.
  New_Values_and_Labels = sapply(recoding_rules, function(r) {
    paste(
      # Map over each key-value pair in recode_map.
      mapply(
        function(code, value) {
          # Check for factor levels and labels in the rule.
          label <- if (!is.null(r$factor_labels) && !is.null(r$factor_levels)) {
            # Match the current value to factor levels to find the corresponding label.
            label_idx <- match(as.numeric(value), as.numeric(r$factor_levels))
            # If a match is found, append the label in parentheses.
            if (!is.na(label_idx)) paste0(" (", r$factor_labels[label_idx], ")") else ""
          } else {
            ""  # If no labels are found, leave empty.
          }
          # Combine the original code with the new value and its label (if present).
          paste0(code, " = ", ifelse(is.na(value), "NA", value), label)
        },
        names(r$recode_map),  # Original codes (keys of recode_map).
        r$recode_map          # New values (values of recode_map).
      ),
      collapse = ifelse(knitr::is_html_output(), "<br>", " \\\\")  # Line breaks for output format.
    )
  }),

  # Description column: Extract the description of each variable from the recoding rules.
  Description = sapply(recoding_rules, function(r) r$description)
)

# Generate the table
knitr::kable(
  recoding_metadata,
  caption = "Summary of Recoded and Renamed Variables",
  col.names = c("New variable", "Original Variable", "Old Values", "New Values and Labels", "Description"),
  booktabs = TRUE,
  escape = FALSE
)

# Any Chronic Health Condition at 7
ncds_renamed <- ncds_renamed %>%
  mutate(
    any_chronic_health_7 = ifelse(
      n350 == 2 | n425 == 2 | asthma_status_7 == 1 | n364 == 2 | n417 %in% c(3, 4, 5, 6),
      1, # At least one chronic condition present
      0  # No chronic conditions present
    )
  )

# BSAG Syndrome scores
# Rename the variables first
ncds_renamed <- ncds_renamed %>%
  rename(
    BSAG_Total_7 = n455,
    BSAG_Total_11 = n1008,
    BSAG_Withdrawal_7 = n434,
    BSAG_Withdrawal_11 = n977,
    BSAG_Unforthcomingness_7 = n432,
    BSAG_Unforthcomingness_11 = n974,
    BSAG_Depression_7 = n436,
    BSAG_Depression_11 = n980,
    BSAG_Writing_Off_7 = n442,
    BSAG_Writing_Off_11 = n989,
    BSAG_Misc_7 = n452,
    BSAG_Misc_11 = n1004
  )

# Convert BSAG_Total_7 and BSAG_Total_11 to numeric
ncds_renamed <- ncds_renamed %>%
  mutate(
    BSAG_Total_7 = as.numeric(BSAG_Total_7),  # Convert to numeric
    BSAG_Total_11 = as.numeric(BSAG_Total_11),  # Convert to numeric
    BSAG_Withdrawal_7 = as.numeric(BSAG_Withdrawal_7),
    BSAG_Withdrawal_11 = as.numeric(BSAG_Withdrawal_11),
    BSAG_Unforthcomingness_7 = as.numeric(BSAG_Unforthcomingness_7),
    BSAG_Unforthcomingness_11 = as.numeric(BSAG_Unforthcomingness_11),
    BSAG_Depression_7 = as.numeric(BSAG_Depression_7),
    BSAG_Depression_11 = as.numeric(BSAG_Depression_11),
    BSAG_Writing_Off_7 = as.numeric(BSAG_Writing_Off_7),
    BSAG_Writing_Off_11 = as.numeric(BSAG_Writing_Off_11),
    BSAG_Misc_7 = as.numeric(BSAG_Misc_7),
    BSAG_Misc_11 = as.numeric(BSAG_Misc_7)
  )

# Condition severity
ncds_renamed <- ncds_renamed %>%
  mutate(
    condition_severity = case_when(
      epilepsy_severity_7 == 2 ~ 2,  # Priority for severity 2
      epilepsy_severity_7 == 1 ~ 1,  # Priority for severity 1
      epilepsy_severity_7 == 0 ~ 0,  # Both are 0, severity stays 0
      TRUE ~ NA_real_                                        # Default to NA for missing data
    )
  )

# Neurological conditions
ncds_renamed <- ncds_renamed %>%
  mutate(
    neurological_conditions_7 = case_when(
      cerebral_palsy_7 == 1 | tics_7 == 1 ~ 1,  # Either condition is 1
      cerebral_palsy_7 == 0 & tics_7 == 0 ~ 0,  # All conditions are 0
      TRUE ~ NA_real_                          # Default to NA for missing data
    )
  )

# Recode number of persons per room at birth
ncds_renamed <- ncds_renamed %>%
  mutate(
    num_persons_room_birth = ifelse(n512 == -1, NA_real_, n512)
  )

# Recode number of family moves
ncds_renamed <- ncds_renamed %>%
  mutate(
    family_moves_7 = ifelse(n95 == -1, NA_real_, n95)
  )

# Recode number of children under 21 in household at age 7
ncds_renamed <- ncds_renamed %>%
  mutate(
    num_children_household_7 = case_when(
      n99 == 0 ~ NA_real_,     # If 0 children, treated as missing value
      n99 == -1 ~ NA_real_,    # If -1, treated as missing value
      TRUE ~ as.numeric(n99)   # Keep all other values as is, converting to numeric if necessary
    )
  )

# Create Gestational Period variable
ncds_renamed <- ncds_renamed %>%
  mutate(
    gestational_period = case_when(
      n497 < 154 ~ NA_real_,    # Out of valid range (invalid data)
      n497 > 344 ~ NA_real_,    # Out of valid range (invalid data)
      n497 >= 154 & n497 <= 258 ~ 1,  # Pre-term (Gestational period < 259 days)
      n497 >= 259 & n497 <= 344 ~ 0,  # Full-term (Gestational period ≥ 259 days)
      TRUE ~ NA_real_                      # Any other value (fallback to NA)
    )
  )

# Recode region of birth variable
ncds_renamed <- ncds_renamed %>%
  mutate(
    n0region = case_when(
      n0region %in% c(-2, -1) ~ NA_character_,  # Convert -2 and -1 to NA
      TRUE ~ as.character(n0region)  # Retain other values as characters
    ),
    n0region = factor(n0region,
                      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
                      labels = c("North", "North West", "E & W.Riding",
                                 "North Midlands", "Midlands", "East",
                                 "South East", "South", "South West",
                                 "Wales", "Scotland"))
  )

# Create number of hospital admissions by age 7 variable (count of '2' across specified variables)
ncds_renamed <- ncds_renamed %>%
  mutate(
    hospital_admissions_7 = ifelse(
      rowSums(select(., n254, n248, n246, n247, n251, n253, n250, n252, n249) == 2, na.rm = TRUE) > 0,
      1,
      0
    )
  )

# Create cognitive ability score variable (average score based on four cognitive tests at age 7)
ncds_renamed <- ncds_renamed %>%
  mutate(
    # Creating the cognitive ability summary score by standardizing each test score first
    cognitive_ability_score_7 = rowMeans(
      select(., n90, n457, n1840, n92) %>%
        mutate(
          n90_standardized = (n90 - mean(n90, na.rm = TRUE)) / sd(n90, na.rm = TRUE),
          n457_standardized = (n457 - mean(n457, na.rm = TRUE)) / sd(n457, na.rm = TRUE),
          n1840_standardized = (n1840 - mean(n1840, na.rm = TRUE)) / sd(n1840, na.rm = TRUE),
          n92_standardized = (n92 - mean(n92, na.rm = TRUE)) / sd(n92, na.rm = TRUE)
        ),
      na.rm = TRUE
    )
  )


# Classify Loss or Separation from Parent (based on Death of Father, Death of Mother, or Parent Separation)
ncds_renamed$loss_or_separation_parent_7 <- as.numeric(
  ncds_renamed$Death_father_7 == 1 |
  ncds_renamed$Death_mother_7 == 1 |
  ncds_renamed$Parent_separation_7 == 1
)

```


Missing Data Analysis prior to imputation.

```{r echo=FALSE, message=FALSE, warning=FALSE}

###########################################################
#
# Missing Data Analysis (Tables for CWE)
#
###########################################################

# Filter dataset for children with epilepsy
ncds_epilepsy <- ncds_renamed %>%
  filter(epilepsy_status_7 == 1)  # Use | for "OR" logic

# Step 1: Select relevant variables for imputation
imputed_vars <- ncds_epilepsy %>%
  select(
    Rutter_A_1_7, Rutter_A_2_7, Rutter_A_3_7, Rutter_A_4_7, Rutter_A_5_7,
    Rutter_A_6_7, Rutter_A_7_7, Rutter_A_8_7, Rutter_A_9_7, Rutter_A_10_7,
    Rutter_A_11_7, Rutter_A_12_7, Rutter_A_13_7, Rutter_A_14_7,
    Rutter_A_1_11, Rutter_A_2_11, Rutter_A_3_11, Rutter_A_4_11, Rutter_A_5_11,
    Rutter_A_6_11, Rutter_A_7_11, Rutter_A_8_11, Rutter_A_9_11, Rutter_A_10_11,
    Rutter_A_11_11, Rutter_A_12_11, Rutter_A_13_11, Rutter_A_14_11, Rutter_A_1_16,
    Rutter_A_2_16, Rutter_A_3_16, Rutter_A_4_16, Rutter_A_5_16,
    Rutter_A_6_16, Rutter_A_7_16, Rutter_A_8_16, Rutter_A_9_16, Rutter_A_10_16,
    Rutter_A_11_16, Rutter_A_12_16, Rutter_A_13_16, Rutter_A_14_16, Rutter_A_15_16,
    Rutter_A_16_16, Rutter_A_17_16, Rutter_A_18_16, Rutter_B_1_16, BSAG_Total_7,
    BSAG_Total_11, Family_mental_illness_7, loss_or_separation_parent_7,
    Mother_education_birth, financial_difficulties_7, family_moves_7,
    Paternal_social_class_birth, n0region, num_children_household_7,
    num_persons_room_birth, Ever_in_care_7, Physical_neglect_7, gestational_period,
    cognitive_ability_score_7, hospital_admissions_7,condition_severity
  )

# Summarize missing data percentage and count
missing_summary <- colSums(is.na(imputed_vars)) / nrow(imputed_vars) * 100
non_missing_summary <- colSums(!is.na(imputed_vars))

# Combine summaries into a single table
missing_data_table <- data.frame(
  Variable = names(missing_summary),
  Missing_Percentage = missing_summary,
  Non_Missing_Count = non_missing_summary
) %>%
  arrange(desc(Missing_Percentage))  # Sort by missing percentage

# Display the table in RMarkdown
kable(missing_data_table,
      col.names = c("Variable", "Missing Percentage (%)", "Non-Missing Count"),
      caption = "Summary of Missing and Non-Missing Data for Children with Epilepsy",
      row.names = FALSE)

# Step 4: Prepare data for heatmap visualization
missing_data_matrix <- is.na(imputed_vars)  # Create logical matrix for missingness

# Melt the logical matrix for ggplot
melted_missing_data <- melt(missing_data_matrix)
colnames(melted_missing_data) <- c("Observation", "Variable", "Missing")

# Order variables by missing percentage
missing_counts <- colSums(is.na(imputed_vars))
melted_missing_data$Variable <- factor(melted_missing_data$Variable,
                                       levels = names(sort(missing_counts, decreasing = TRUE)))

# Step 5: Create heatmap
ggplot(melted_missing_data, aes(x = Variable, y = Observation, fill = Missing)) +
  geom_tile() +
  scale_fill_manual(values = c("white", "red"), labels = c("Non-missing", "Missing")) +
  theme_minimal() +
  labs(title = "Missing Data Heatmap for Children with Epilepsy",
       x = "Variable",
       y = "Observation (Participant)",
       fill = "Data Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(),  # Remove participant ID labels from y-axis
        axis.ticks.y = element_blank())  # Remove ticks from y-axis

```

Little's MCAR Test.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# Select the relevant variables for imputation and missingness analysis
imputed_vars <- ncds_renamed %>%
  select(
    Rutter_A_1_7, Rutter_A_2_7, Rutter_A_3_7, Rutter_A_4_7, Rutter_A_5_7,
    Rutter_A_6_7, Rutter_A_7_7, Rutter_A_8_7, Rutter_A_9_7, Rutter_A_10_7,
    Rutter_A_11_7, Rutter_A_12_7, Rutter_A_13_7, Rutter_A_14_7,
    Rutter_A_1_11, Rutter_A_2_11, Rutter_A_3_11, Rutter_A_4_11, Rutter_A_5_11,
    Rutter_A_6_11, Rutter_A_7_11, Rutter_A_8_11, Rutter_A_9_11, Rutter_A_10_11,
    Rutter_A_11_11, Rutter_A_12_11, Rutter_A_13_11, Rutter_A_14_11, Rutter_A_1_16,
    Rutter_A_2_16, Rutter_A_3_16, Rutter_A_4_16, Rutter_A_5_16,
    Rutter_A_6_16, Rutter_A_7_16, Rutter_A_8_16, Rutter_A_9_16, Rutter_A_10_16,
    Rutter_A_11_16, Rutter_A_12_16, Rutter_A_13_16, Rutter_A_14_16, Rutter_A_15_16,
    Rutter_A_16_16, Rutter_A_17_16, Rutter_A_18_16, Rutter_B_1_16, BSAG_Total_7,
    BSAG_Total_11, , Family_mental_illness_7, loss_or_separation_parent_7,
    Mother_education_birth, financial_difficulties_7, family_moves_7,
    Paternal_social_class_birth, n0region, num_children_household_7,
    num_persons_room_birth, Ever_in_care_7, Physical_neglect_7, gestational_period,
    cognitive_ability_score_7, hospital_admissions_7,
    condition_severity
  )

# Create a Binary Matrix for Missingness (0 for non-missing, 1 for missing)
missing_matrix <- is.na(imputed_vars) * 1

# Calculate the Covariance Matrix of Missingness Patterns
cov_missing <- cov(missing_matrix)

# Number of Variables and Observations
p <- ncol(imputed_vars)
n <- nrow(imputed_vars)

# Compute Chi-Square Statistic for MCAR (Approximated Test)
chi_square_stat <- n * sum(diag(cov_missing)^2)
degrees_freedom <- (p * (p - 1)) / 2
p_value <- pchisq(chi_square_stat, df = degrees_freedom, lower.tail = FALSE)

# Display Little's MCAR Test Results
cat("Little's MCAR Test Results:\n")
cat("Chi-square statistic:", round(chi_square_stat, 2), "\n")
cat("Degrees of freedom:", degrees_freedom, "\n")
cat("P-value:", formatC(p_value, format = "e", digits = 2), "\n")

# Count how many participants meet the filtering criteria
n_participants_in_imputation <- ncds_renamed %>%
  filter(
    (epilepsy_status_7 == 1 & !is.na(sex_birth)) |
    ((epilepsy_status_7 != 1 | is.na(epilepsy_status_7)) &
     !is.na(sex_birth) &
     !is.na(neurological_conditions_7) &
     !is.na(any_chronic_health_7) &
     !is.na(Intellectual_disability_7) &
     !is.na(epilepsy_severity_7))
  ) %>%
  nrow()

# Print the number
cat("Number of participants included in the imputation dataset:", n_participants_in_imputation, "\n")



```

Multiple Imptuation using MICE with 50 chained equations and composite variables created again.

```{r echo=FALSE, message=FALSE, warning=FALSE}

#############################################################################

# Multiple Imputation

#############################################################################

# # Filter dataset with different conditions for children with and without epilepsy
# ncds_filtered_complete_data <- ncds_renamed %>%
#   filter(
#    # For children with epilepsy (epilepsy_status_7 == 1): Only the first three conditions apply
#     (epilepsy_status_7 == 1 &
#      !is.na(sex_birth)
#      ) |
# 
#     # For children without epilepsy or missing epilepsy status:
#     # Apply the broader set of conditions
#     ((epilepsy_status_7 != 1 | is.na(epilepsy_status_7)) &
#      !is.na(sex_birth) &
#      !is.na(neurological_conditions_7) &
#      !is.na(any_chronic_health_7) &
#      !is.na(Intellectual_disability_7) &
#      !is.na(epilepsy_severity_7))
#   )
# 
# # Variables to exclude from imputation
# exclude_vars <- c(
#   "ncdsid",  # Identifier
#   "epilepsy_status_7",
#   "n0region"
# )
# 
# # Step 1: Define imputation methods for the variables
# imputation_methods <- c(
#   "sex_birth" = "logreg",                       # Binary variable
#   "Rutter_A_1_7" = "pmm",                       # Continuous variable
#   "Rutter_A_2_7" = "pmm",                       # Continuous variable
#   "Rutter_A_3_7" = "pmm",                       # Continuous variable
#   "Rutter_A_4_7" = "pmm",                       # Continuous variable
#   "Rutter_A_5_7" = "pmm",                       # Continuous variable
#   "Rutter_A_6_7" = "pmm",                       # Continuous variable
#   "Rutter_A_7_7" = "pmm",                       # Continuous variable
#   "Rutter_A_8_7" = "pmm",                       # Continuous variable
#   "Rutter_A_9_7" = "pmm",                       # Continuous variable
#   "Rutter_A_10_7" = "pmm",                      # Continuous variable
#   "Rutter_A_11_7" = "pmm",                      # Continuous variable
#   "Rutter_A_12_7" = "pmm",                      # Continuous variable
#   "Rutter_A_13_7" = "pmm",                      # Continuous variable
#   "Rutter_A_14_7" = "pmm",                      # Continuous variable
#   "Rutter_A_1_11" = "pmm",                      # Continuous variable
#   "Rutter_A_2_11" = "pmm",                      # Continuous variable
#   "Rutter_A_3_11" = "pmm",                      # Continuous variable
#   "Rutter_A_4_11" = "pmm",                      # Continuous variable
#   "Rutter_A_5_11" = "pmm",                      # Continuous variable
#   "Rutter_A_6_11" = "pmm",                      # Continuous variable
#   "Rutter_A_7_11" = "pmm",                      # Continuous variable
#   "Rutter_A_8_11" = "pmm",                      # Continuous variable
#   "Rutter_A_9_11" = "pmm",                      # Continuous variable
#   "Rutter_A_10_11" = "pmm",                     # Continuous variable
#   "Rutter_A_11_11" = "pmm",                     # Continuous variable
#   "Rutter_A_12_11" = "pmm",                     # Continuous variable
#   "Rutter_A_13_11" = "pmm",                     # Continuous variable
#   "Rutter_A_14_11" = "pmm",                     # Continuous variable
#   "Rutter_A_1_16" = "pmm",                      # Continuous variable
#   "Rutter_A_2_16" = "pmm",                      # Continuous variable
#   "Rutter_A_3_16" = "pmm",                      # Continuous variable
#   "Rutter_A_4_16" = "pmm",                      # Continuous variable
#   "Rutter_A_5_16" = "pmm",                      # Continuous variable
#   "Rutter_A_6_16" = "pmm",                      # Continuous variable
#   "Rutter_A_7_16" = "pmm",                      # Continuous variable
#   "Rutter_A_8_16" = "pmm",                      # Continuous variable
#   "Rutter_A_9_16" = "pmm",                      # Continuous variable
#   "Rutter_A_10_16" = "pmm",                     # Continuous variable
#   "Rutter_A_11_16" = "pmm",                     # Continuous variable
#   "Rutter_A_12_16" = "pmm",                     # Continuous variable
#   "Rutter_A_13_16" = "pmm",                     # Continuous variable
#   "Rutter_A_14_16" = "pmm",                     # Continuous variable
#   "Rutter_A_15_16" = "pmm",                     # Continuous variable
#   "Rutter_A_16_16" = "pmm",                     # Continuous variable
#   "Rutter_A_17_16" = "pmm",                     # Continuous variable
#   "Rutter_A_18_16" = "pmm",                     # Continuous variable
#   "Rutter_B_1_16" = "pmm",                      # Continuous variable
#   "BSAG_Total_7" = "pmm",                       # Continuous variable
#   "BSAG_Total_11" = "pmm",                      # Continuous variable
#   "Family_mental_illness_7" = "logreg",         # Binary variable
#   "loss_or_separation_parent_7" = "logreg",     # Binary variable
#   "Family_probation_officer_7" = "logreg",      # Binary variable
#   "Parent_alcoholism_7" = "logreg",             # Binary variable
#   "financial_difficulties_7" = "logreg",        # Binary variable
#   "Mother_education_birth" = "logreg",          # Binary variable
#   "Paternal_social_class_birth" = "logreg",     # Binary variable
#   "Ever_in_care_7" = "logreg",                  # Binary variable
#   "Physical_neglect_7" = "logreg",              # Binary variable
#   "num_children_household_7" = "pmm",           # Continuous variable
#   "num_persons_room_birth" = "pmm",             # Continuous variable
#   "family_moves_7" = "pmm",                     # Continuous variable
#   "gestational_period" = "logreg",              # Binary variable
#   "cognitive_ability_score_7" = "pmm",          # Continuous variable
#   "hospital_admissions_7" = "pmm",              # Continuous variable
#   "neurological_conditions_7" = "logreg",       # Binary variable
#   "any_chronic_health_7" = "logreg",            # Binary variable
#   "epilepsy_severity_7" = "pmm",                # Continuous variable
#   "Intellectual_disability_7" = "logreg"        # Binary variable
# )
# 
# # Step 3: Subset the dataset to include only variables to be imputed
# imputation_vars <- setdiff(names(imputation_methods), exclude_vars)
# ncds_for_imputation <- ncds_filtered_complete_data[, imputation_vars]
# 
# # Step 4: Perform the imputation
# imputed_data <- mice(ncds_for_imputation, method = imputation_methods[imputation_vars], m = 50, seed = 123, printFlag = FALSE)
# 
# # Step 5: Complete the imputation (choose the first imputed dataset)
# completed_data <- complete(imputed_data, 1)
# 
# # Step 6: Combine back with the excluded variables
# imputed_dataset <- cbind(
#   ncds_filtered_complete_data[, exclude_vars, drop = FALSE],
#   completed_data
# )
# 
# #############################################################################
# 
# # Export imputed dataset
# 
# #############################################################################
# 
# # Export the completed dataset to a CSV to avoid re-imputing every time
# write.csv(imputed_dataset, paste0("bullying_16_imputed_data_", Sys.Date(), ".csv"), row.names = FALSE)

```

The imputed data "imputed_data.csv" was loaded to prevent running imputation again. This was used to create a PSM sample for analysis.

```{r echo=FALSE, message=FALSE, warning=FALSE}

##############################################################################

# Load imputed datasets to avoid running imputation again

##############################################################################

# Load the imputed dataset
final_imputed_dataset <- read.csv("bullying_16_imputed_data_2025-01-28.csv")
nrow(final_imputed_dataset)

#############################################################################

# Create Composite Variables within imputed dataset

#############################################################################

# SES score
final_imputed_dataset$SES_score <- rowSums(
  final_imputed_dataset[, c("Paternal_social_class_birth", "Mother_education_birth")],
  na.rm = TRUE
  )

# Check pairwise correlations for SES variables
ses_vars <- final_imputed_dataset[, c("Paternal_social_class_birth", "Mother_education_birth")]

# Check pairwise correlations using tetrachoric correlation for binary variables
ses_correlations <- psych::tetrachoric(ses_vars)

# Print correlation matrix
ses_correlations$rho

# Compute Cronbach's alpha for SES variable
psych::alpha(ses_vars)

# Internalising at 7
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    internalising_7 = Rutter_A_5_7 + Rutter_A_7_7 + Rutter_A_10_7
  )

# Internalising at 11
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    internalising_11 = Rutter_A_5_11 + Rutter_A_7_11 + Rutter_A_10_11
  )

# Internalising at 16
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    internalising_16 = Rutter_A_5_16 + Rutter_A_7_16 + Rutter_A_10_16
  )

# Externalising at 7
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    externalising_7 = Rutter_A_4_7 + Rutter_A_8_7 + Rutter_A_14_7
  )

# Externalising at 11
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    externalising_11 = Rutter_A_4_11 + Rutter_A_8_7 + Rutter_A_14_11
  )

# Externalising at 16
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    externalising_16 = Rutter_A_4_16 + Rutter_A_8_16 + Rutter_A_14_16
  )

# Bullied at 7
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    bullied_7 = Rutter_A_3_7
  )

# Bullied at 11
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    bullied_11 = Rutter_A_3_11
  )

# Recode bullied at 7 into binary
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    bullied_binary_7 = ifelse(bullied_7 > 0, 1, 0)
  )

# Recode bullied at 11 into binary
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    bullied_binary_11 = ifelse(bullied_11 > 0, 1, 0)
  )

# Create bullied_by_11 variable
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    bullied_by_11 = case_when(
      bullied_binary_7 == 0 & bullied_binary_11 == 0 ~ 0,
      bullied_binary_7 == 1 & bullied_binary_11 == 1 ~ 2,
      TRUE ~ 1
    )
  )

# Create loss or separation binary variable
final_imputed_dataset$parent_separation_7 <- ifelse(
  final_imputed_dataset$loss_or_separation_parent_7 == 1 | final_imputed_dataset$Ever_in_care_7 == 1,
  1,
  0
)

############################################################################

# Cronbach's Alpha

#############################################################################

# Assess Cronbach's Alpha for Internalising and Externalising
# Internalising at 7
internalising_7_items_m <- final_imputed_dataset %>%
  select(Rutter_A_5_7, Rutter_A_7_7, Rutter_A_10_7)
alpha(internalising_7_items_m)

# Internalising at 11
internalising_11_items_m <- final_imputed_dataset %>%
  select(Rutter_A_5_11, Rutter_A_7_11, Rutter_A_10_11)
alpha(internalising_11_items_m)

# Internalising at 16
internalising_16_items_m <- final_imputed_dataset %>%
  select(Rutter_A_5_16, Rutter_A_7_16, Rutter_A_10_16)
alpha(internalising_16_items_m)

# Externalising at 7
externalising_7_items_m <- final_imputed_dataset %>%
  select(Rutter_A_4_7, Rutter_A_8_7, Rutter_A_14_7)
alpha(externalising_7_items_m)

# Externalising at 11
externalising_11_items_m <- final_imputed_dataset %>%
  select(Rutter_A_4_11, Rutter_A_8_11, Rutter_A_14_11)
alpha(externalising_11_items_m)

# Externalising at 16
externalising_16_items_m <- final_imputed_dataset %>%
  select(Rutter_A_4_16, Rutter_A_8_16, Rutter_A_14_16)
alpha(externalising_16_items_m)

# Point-biserial correlation
cor_bullied_internalising <- cor(final_imputed_dataset$bullied_binary_7, final_imputed_dataset$internalising_7, method = "pearson")

# Alternatively, if you expect a non-linear relationship, use Spearman correlation
cor_bullied_internalising_spearman <- cor(final_imputed_dataset$bullied_binary_7, final_imputed_dataset$internalising_7, method = "spearman")

# Print results
cat("Point-biserial Correlation Internalising 7 (Pearson):", cor_bullied_internalising, "\n")
cat("Spearman Correlation Internalising 7:", cor_bullied_internalising_spearman, "\n")

# Point-biserial correlation
cor_bullied_externalising <- cor(final_imputed_dataset$bullied_binary_7, final_imputed_dataset$externalising_7, method = "pearson")

# Alternatively, if you expect a non-linear relationship, use Spearman correlation
cor_bullied_externalising_spearman <- cor(final_imputed_dataset$bullied_binary_7, final_imputed_dataset$externalising_7, method = "spearman")

# Print results
cat("Point-biserial Correlation Externalising 7 (Pearson):", cor_bullied_externalising, "\n")
cat("Spearman Correlation Externalising 7:", cor_bullied_externalising_spearman, "\n")

# Check pairwise correlations for SES variables
ses_vars_matched <- final_imputed_dataset[, c("Paternal_social_class_birth", "Mother_education_birth")]

# Check pairwise correlations using tetrachoric correlation for binary variables
ses_correlations_matched <- psych::tetrachoric(ses_vars_matched)

# Print correlation matrix
ses_correlations_matched$rho

# Compute Cronbach's alpha for SES variable
psych::alpha(ses_vars_matched)

############################################################################

# Propensity Score Matching for main analyses

#############################################################################

# Extract NCDSID for epilepsy cases, including non-epilepsy cases with missing epilepsy status
epilepsy_ids <- final_imputed_dataset$ncdsid[final_imputed_dataset$epilepsy_status_7 == 1]

# Create the binary variable (set missing epilepsy status as 0)
final_imputed_dataset$epilepsy_binary <- ifelse(
  final_imputed_dataset$ncdsid %in% epilepsy_ids, 1,  # Assign 1 for epilepsy cases
  0  # Assign 0 for all others, including missing data
)
# Convert to a factor with levels 0 and 1
final_imputed_dataset$epilepsy_binary <- factor(final_imputed_dataset$epilepsy_binary, levels = c(0, 1))

# Convert factors to numeric (0 and 1)
final_imputed_dataset$epilepsy_binary <- as.numeric(final_imputed_dataset$epilepsy_binary) - 1

# Filter for controls: non-epilepsy participants with no chronic health conditions or neurological issues
neurotypical_control_group <- final_imputed_dataset[
  final_imputed_dataset$epilepsy_binary == 0 &
  final_imputed_dataset$epilepsy_severity == 0 &
  final_imputed_dataset$neurological_conditions_7 == 0 &
  final_imputed_dataset$Intellectual_disability_7 == 0 &
  final_imputed_dataset$any_chronic_health_7 == 0,
]

# Filter for sensitivity controls: non-epilepsy participants
sensitivity_control_group <- final_imputed_dataset[
  final_imputed_dataset$epilepsy_binary == 0 &
  final_imputed_dataset$epilepsy_severity == 0 &
  final_imputed_dataset$neurological_conditions_7 == 0 &
  final_imputed_dataset$Intellectual_disability_7 == 0,
]

# Combine epilepsy group with control group for matching
epilepsy_group <- final_imputed_dataset[final_imputed_dataset$epilepsy_binary == 1, ]
analysis_dataset <- rbind(epilepsy_group, neurotypical_control_group)

# Combine epilepsy group with sensitivity group for matching
epilepsy_group <- final_imputed_dataset[final_imputed_dataset$epilepsy_binary == 1, ]
sensitivity_analysis_dataset <- rbind(epilepsy_group, sensitivity_control_group)

# Run propensity score matching for main control group (1:10 nearest neighbor matching)
ps_model <- matchit(
  epilepsy_binary ~ sex_birth + gestational_period + SES_score,
  data = analysis_dataset,
  method = "nearest",
  ratio = 10,
  caliper = 0.1
)

# Extract matched dataset
matched_dataset <- match.data(ps_model)

# Check final sample sizes after matching
table(matched_dataset$epilepsy_binary)

# Save matched dataset
write.csv(matched_dataset, "matched_primary_epilepsy_65_1to10.csv", row.names = FALSE)

# Run propensity score matching for sensitivity control group (1:10 nearest neighbor matching)
ps_sensitivity_model <- matchit(
  epilepsy_binary ~ sex_birth + gestational_period + SES_score,
  data = sensitivity_analysis_dataset,
  method = "nearest",
  ratio = 10,
  caliper = 0.1
)

# Extract matched dataset
matched_sensitivity_dataset <- match.data(ps_sensitivity_model)

# Check final sample sizes after matching
table(matched_sensitivity_dataset$epilepsy_binary)

# Save matched dataset
write.csv(matched_sensitivity_dataset, "matched_sensitivity_epilepsy_65_1to10.csv", row.names = FALSE)

# Optional: Save epilepsy group separately for reporting
epilepsy_group <- matched_dataset[matched_dataset$epilepsy_binary == 1, ]
write.csv(epilepsy_group, "epilepsy_group_primary.csv", row.names = FALSE)

##############################################################################
# Check integrity of PSM matching (main analysis control group)
##############################################################################

# Read in the matched datasets for epilepsy and control groups
matched_dataset <- read.csv("matched_primary_epilepsy_65_1to10.csv")
epilepsy_group <- read.csv("epilepsy_group_primary.csv")
sensitivity_dataset <- read.csv("matched_sensitivity_epilepsy_65_1to10.csv")

# Load necessary libraries
library(tableone)

# Assess balance before matching
balance_before <- bal.tab(
  epilepsy_binary ~ sex_birth + gestational_period + SES_score + internalising_7 + internalising_11 + internalising_16 + externalising_7 + externalising_11 + externalising_16 + bullied_7 + bullied_by_11 + neurological_conditions_7 + any_chronic_health_7,  # Include additional covariates for balance check
  data = analysis_dataset
)

# Print balance summary before matching
print(balance_before)

# Assess balance after matching (using the MatchIt object)
balance_after <- bal.tab(
  ps_model, # MatchIt object
  un = TRUE # Include unadjusted (pre-matching) results for comparison
)

# Print balance summary after matching
print(balance_after)

# Visualization: Love plot for standardized mean differences
love.plot(
  balance_after,
  threshold = 0.1, # Highlights covariates with SMD > 0.1
  abs = TRUE, # Show absolute SMD values for easier interpretation
  var.order = "unadjusted" # Order by unadjusted SMD
)

# # --- Descriptive statistics for the groups ---
# 
# # Descriptive statistics for the matched dataset (those who were matched by PSM)
# matched_dataset <- match.data(ps_model)
# 
# # Descriptive statistics for matched dataset by exposure (epilepsy_binary)
# matched_desc <- CreateTableOne(
#   vars = c("sex_birth", "gestational_period", "SES_score", "internalising_7", "internalising_11", "internalising_16",
#            "externalising_7", "externalising_11", "externalising_16", "bullied_7", "bullied_by_11", "neurological_conditions_7", "any_chronic_health_7"),
#   strata = "epilepsy_binary",  # Group by epilepsy status (exposure)
#   data = matched_dataset
# )
# 
# # Print the descriptive statistics for matched dataset
# print(matched_desc)
# 
# # Descriptive statistics for the original dataset, grouped by exposure (epilepsy_binary)
# # To compare, you can also generate summary statistics for the original dataset before matching
# original_desc <- CreateTableOne(
#   vars = c("sex_birth", "gestational_period", "SES_score", "internalising_7", "internalising_11", "internalising_16",
#            "externalising_7", "externalising_11", "externalising_16", "bullied_7", "bullied_by_11", "neurological_conditions_7", "any_chronic_health_7"),
#   strata = "epilepsy_binary",  # Group by epilepsy status (exposure)
#   data = analysis_dataset  # The original dataset before matching
# )
# 
# # Print the descriptive statistics for the original dataset
# print(original_desc)

##############################################################################
# Check integrity of PSM matching (sensitivity analysis control group)
##############################################################################

# Assess balance before matching
balance_before <- bal.tab(
  epilepsy_binary ~ sex_birth + gestational_period + SES_score + internalising_7 + internalising_11 + internalising_16 + externalising_7 + externalising_11 + externalising_16 + bullied_7 + bullied_by_11 + neurological_conditions_7 + any_chronic_health_7,    # Include additional covariates for balance check
  data = sensitivity_analysis_dataset
)

# Print balance summary before matching
print(balance_before)

# Assess balance after matching
balance_after_sensitivity <- bal.tab(
  ps_sensitivity_model, # MatchIt object
  un = TRUE # Include unadjusted (pre-matching) results for comparison
)

# Print balance summary after matching
print(balance_after_sensitivity)

# Visualization: Love plot for standardized mean differences
love.plot(
  balance_after_sensitivity,
  threshold = 0.1, # Highlights covariates with SMD > 0.1
  abs = TRUE, # Show absolute SMD values for easier interpretation
  var.order = "unadjusted" # Order by unadjusted SMD
)

# --- Descriptive statistics for the groups in sensitivity analysis ---

# Descriptive statistics for the matched dataset (sensitivity analysis)
matched_sensitivity_dataset <- match.data(ps_sensitivity_model)

# Descriptive statistics for matched sensitivity analysis dataset by exposure (epilepsy_binary)
matched_desc_sensitivity <- CreateTableOne(
  vars = c("sex_birth", "gestational_period", "SES_score", "internalising_7", "internalising_11", "internalising_16",
           "externalising_7", "externalising_11", "externalising_16", "bullied_7", "bullied_by_11", "neurological_conditions_7", "any_chronic_health_7"),
  strata = "epilepsy_binary",  # Group by epilepsy status (exposure)
  data = matched_sensitivity_dataset
)

# Print the descriptive statistics for matched sensitivity dataset
print(matched_desc_sensitivity)

# Descriptive statistics for the original sensitivity analysis dataset (before matching)
original_desc_sensitivity <- CreateTableOne(
  vars = c("sex_birth", "gestational_period", "SES_score", "internalising_7", "internalising_11", "internalising_16",
           "externalising_7", "externalising_11", "externalising_16", "bullied_7", "bullied_by_11", "neurological_conditions_7", "any_chronic_health_7"),
  strata = "epilepsy_binary",  # Group by epilepsy status (exposure)
  data = sensitivity_analysis_dataset  # The original dataset before matching
)

# Print the descriptive statistics for the original sensitivity dataset
print(original_desc_sensitivity)

```

Descriptive Statistics Tables

```{r echo=FALSE, message=FALSE, warning=FALSE}

###########################################################
#
# Descriptive Statistics Tables - Table 1 (CWE)
#
###########################################################

###########################################################
# Table 1 and Supplemental Table 1: Baseline Characteristics
###########################################################

library(tableone)

# Print table caption
cat("\nTable 1 - Descriptives Comparing Full Sample and Matched Sample\n\n")

# Define variables, including bullying categorization
vars <- c("sex_birth", "SES_score", "gestational_period",
          "internalising_7", "internalising_11", "internalising_16",
          "externalising_7", "externalising_11", "externalising_16",
          "bullied_7_cat", "bullied_11_cat" ,"neurological_conditions_7", "any_chronic_health_7")

# Define categorical variables, including bullying categories
factorVars <- c("sex_birth", "SES_score", "gestational_period", "bullied_7_cat", "bullied_11_cat","neurological_conditions_7", "any_chronic_health_7")

# Reclassify bullying victimisation
final_imputed_dataset <- final_imputed_dataset %>%
  mutate(
    # Classify bullying victimisation at age 7 (Never = 0, Occasionally = 1, Frequently = 2)
    bullied_7_cat = case_when(
      bullied_7 == 0 ~ "Never",
      bullied_7 == 1 ~ "Occasionally",
      bullied_7 == 2 ~ "Frequently",
      TRUE ~ NA_character_),

    # Classify bullying victimisation at age 11 (Never = 0, Occasionally = 1, Frequently = 2)
    bullied_11_cat = case_when(
      bullied_by_11 == 0 ~ "Never",
      bullied_by_11 == 1 ~ "Occasionally",
      bullied_by_11 == 2 ~ "Frequently",
      TRUE ~ NA_character_)
  )

# Reclassify bullying victimisation
matched_dataset <- matched_dataset %>%
  mutate(
    # Classify bullying victimisation at age 7 (Never = 0, Occasionally = 1, Frequently = 2)
    bullied_7_cat = case_when(
      bullied_7 == 0 ~ "Never",
      bullied_7 == 1 ~ "Occasionally",
      bullied_7 == 2 ~ "Frequently",
      TRUE ~ NA_character_),

    # Classify bullying victimisation at age 11 (Never = 0, Occasionally = 1, Frequently = 2)
    bullied_11_cat = case_when(
      bullied_by_11 == 0 ~ "Never",
      bullied_by_11 == 1 ~ "Occasionally",
      bullied_by_11 == 2 ~ "Frequently",
      TRUE ~ NA_character_)
  )

# Create Table 1 for full sample (including bullying categories)
table1_full <- CreateTableOne(vars = vars,
                              strata = "epilepsy_binary",
                              data = final_imputed_dataset,
                              factorVars = factorVars,
                              test = TRUE)

# Create Table 1 for matched sample (including bullying categories)
table1_matched <- CreateTableOne(vars = vars,
                                 strata = "epilepsy_binary",
                                 data = matched_dataset,
                                 factorVars = factorVars,
                                 test = TRUE)

# Print both tables with standardised mean differences and captions
print(table1_full, showAllLevels = TRUE, smd = TRUE, caption = "Table 1 - Baseline Characteristics in Full NCDS Sample")
print(table1_matched, showAllLevels = TRUE, smd = TRUE, caption = "Table 1 - Baseline Characteristics in Propensity Score Matched Sample")

###########################################################
# Create Supplemental Table 1 for sensitivity dataset
###########################################################

# Print table caption
cat("\nSupplemental Table 1 - Descriptives for Sensitivity Dataset\n\n")

# Reclassify bullying victimisation
sensitivity_dataset <- sensitivity_dataset %>%
  mutate(
    # Classify bullying victimisation at age 7 (Never = 0, Occasionally = 1, Frequently = 2)
    bullied_7_cat = case_when(
      bullied_7 == 0 ~ "Never",
      bullied_7 == 1 ~ "Occasionally",
      bullied_7 == 2 ~ "Frequently",
      TRUE ~ NA_character_),

    # Classify bullying victimisation at age 11 (Never = 0, Occasionally = 1, Frequently = 2)
    bullied_11_cat = case_when(
      bullied_by_11 == 0 ~ "Never",
      bullied_by_11 == 1 ~ "Occasionally",
      bullied_by_11 == 2 ~ "Frequently",
      TRUE ~ NA_character_)
  )

suptable1 <- CreateTableOne(vars = vars,
                            strata = "epilepsy_binary",
                            data = sensitivity_dataset,
                            factorVars = factorVars,
                            test = TRUE)

# Print Supplemental Table 1 with standardised mean differences and caption
print(suptable1, showAllLevels = TRUE, smd = TRUE, caption = "Supplemental Table One - Baseline Characteristics of Sensitivity Dataset")

###########################################################
# Supplemental Table 4 - Bullying victimisation scores
###########################################################

library(dplyr)

sum(matched_dataset$ncdsid %in% ncds_renamed$ncdsid)

epilepsy_ids_m <- ncds_renamed %>%
  filter(epilepsy_status_7 == 1) %>%
  select(ncdsid) %>%
  mutate(epilepsy_ids_original = 1)

matched_dataset <- matched_dataset %>%
  left_join(epilepsy_ids_m, by = "ncdsid") %>%
  mutate(epilepsy_ids_original = ifelse(is.na(epilepsy_ids_original), 0, epilepsy_ids_original))

# Find participants in matched_dataset with non-missing Rutter_A_3_7 and Rutter_A_3_11 in final_imputed_dataset
ids_with_rutter_7 <- ncds_renamed %>%
  filter(ncdsid %in% matched_dataset$ncdsid & !is.na(Rutter_A_3_7)) %>%
  pull(ncdsid)

ids_with_rutter_11 <- ncds_renamed %>%
  filter(ncdsid %in% matched_dataset$ncdsid & !is.na(Rutter_A_3_11)) %>%
  pull(ncdsid)

# Subset matched_dataset for those with valid Rutter data
matched_7 <- matched_dataset %>% filter(ncdsid %in% ids_with_rutter_7)
matched_11 <- matched_dataset %>% filter(ncdsid %in% ids_with_rutter_11)

# Report N for each group
cat("\nNumber of participants with available bullying data at age 7:", nrow(matched_7), "\n")
cat("Number of participants with available bullying data at age 11:", nrow(matched_11), "\n")

# Create tertiles again (in case original matched_dataset was modified)
matched_7 <- matched_7 %>%
  mutate(
    int7_tertile = ntile(internalising_7, 3),
    ext7_tertile = ntile(externalising_7, 3),
    int11_tertile = ntile(internalising_11, 3),
    ext11_tertile = ntile(externalising_11, 3)
  )

matched_11 <- matched_11 %>%
  mutate(
    int7_tertile = ntile(internalising_7, 3),
    ext7_tertile = ntile(externalising_7, 3),
    int11_tertile = ntile(internalising_11, 3),
    ext11_tertile = ntile(externalising_11, 3)
  )

grouping_vars <- c("epilepsy_ids_original", "sex_birth", "SES_score", "gestational_period",
                   "int7_tertile", "ext7_tertile", "int11_tertile", "ext11_tertile")


# Print table caption
cat("\nSupplemental Table 4 - Distribution of Bullying Victimisation Scores\n\n")

# Function to compute and print mean and SD for each group
calculate_and_print <- function(data, group_var, bullying_var) {
  result <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      mean_bullying = mean(!!sym(bullying_var), na.rm = TRUE),
      sd_bullying = sd(!!sym(bullying_var), na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n", group_var, "-", bullying_var, "\n")
  print(result)
}

# Compute and print stats for age 7 and 11 bullying
for (var in grouping_vars) {
  calculate_and_print(matched_7, var, "bullied_7")
  calculate_and_print(matched_11, var, "bullied_by_11")
}

###########################################################
# Supplemental Table 5 - Missingness Bullying scores
###########################################################
# Load required library
library(dplyr)
library(tidyr)

# Join in missingness indicators from ncds_renamed
missing_flags <- ncds_renamed %>%
  select(ncdsid, Rutter_A_3_7, Rutter_A_3_11) %>%
  mutate(
    missing_rutter7 = ifelse(is.na(Rutter_A_3_7), "Missing", "Available"),
    missing_rutter11 = ifelse(is.na(Rutter_A_3_11), "Missing", "Available")
  ) %>%
  select(ncdsid, missing_rutter7, missing_rutter11)

# Merge with matched_dataset
matched_with_missing_flags <- matched_dataset %>%
  left_join(missing_flags, by = "ncdsid") %>%
  mutate(
    missing_rutter7 = factor(missing_rutter7, levels = c("Available", "Missing")),
    missing_rutter11 = factor(missing_rutter11, levels = c("Available", "Missing")),
    int7_tertile = ntile(internalising_7, 3),
    int11_tertile = ntile(internalising_11, 3),
    ext7_tertile = ntile(externalising_7, 3),
    ext11_tertile = ntile(externalising_11, 3)
  )

# Variables to stratify by
vars_to_check <- c("epilepsy_binary", "SES_score", "sex_birth", "gestational_period", "int7_tertile", "ext7_tertile")

cat("\n--- Missingness of Bullying Victimisation at Age 7 ---\n")

for (var in vars_to_check) {
  tab <- matched_with_missing_flags %>%
    group_by(!!sym(var), missing_rutter7) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(!!sym(var)) %>%
    mutate(percent = round(n / sum(n) * 100, 1)) %>%
    ungroup()

  cat("\nVariable:", var, "\n")
  print(tab)
}

cat("\nTotal N with Available bullying score at age 7:", sum(matched_with_missing_flags$missing_rutter7 == "Available"), "\n")
cat("Total N with Missing bullying score at age 7:", sum(matched_with_missing_flags$missing_rutter7 == "Missing"), "\n")

vars_to_check_11 <- c(vars_to_check, "int11_tertile", "ext11_tertile")

cat("\n--- Missingness of Bullying Victimisation at Age 11 ---\n")

for (var in vars_to_check_11) {
  tab <- matched_with_missing_flags %>%
    group_by(!!sym(var), missing_rutter11) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(!!sym(var)) %>%
    mutate(percent = round(n / sum(n) * 100, 1)) %>%
    ungroup()

  cat("\nVariable:", var, "\n")
  print(tab)
}

cat("\nTotal N with Available bullying score at age 11:", sum(matched_with_missing_flags$missing_rutter11 == "Available"), "\n")
cat("Total N with Missing bullying score at age 11:", sum(matched_with_missing_flags$missing_rutter11 == "Missing"), "\n")

###########################################################
# Supplemental Table 6 - Missingness Rutter scores
###########################################################

# Step 1: Define missingness flags
missing_flags <- ncds_renamed %>%
  select(ncdsid,
         Rutter_A_5_7, Rutter_A_7_7, Rutter_A_10_7,
         Rutter_A_4_7, Rutter_A_8_7, Rutter_A_14_7,
         Rutter_A_5_11, Rutter_A_7_11, Rutter_A_10_11,
         Rutter_A_4_11, Rutter_A_8_11, Rutter_A_14_11,
         Rutter_A_5_16, Rutter_A_7_16, Rutter_A_10_16,
         Rutter_A_4_16, Rutter_A_8_16, Rutter_A_14_16) %>%
  mutate(
    missing_internalising_7 = if_any(c(Rutter_A_5_7, Rutter_A_7_7, Rutter_A_10_7), is.na),
    missing_externalising_7 = if_any(c(Rutter_A_4_7, Rutter_A_8_7, Rutter_A_14_7), is.na),
    missing_internalising_11 = if_any(c(Rutter_A_5_11, Rutter_A_7_11, Rutter_A_10_11), is.na),
    missing_externalising_11 = if_any(c(Rutter_A_4_11, Rutter_A_8_11, Rutter_A_14_11), is.na),
    missing_internalising_16 = if_any(c(Rutter_A_5_16, Rutter_A_7_16, Rutter_A_10_16), is.na),
    missing_externalising_16 = if_any(c(Rutter_A_4_16, Rutter_A_8_16, Rutter_A_14_16), is.na)
  ) %>%
  mutate(across(starts_with("missing_"), ~ ifelse(.x, "Missing", "Available"))) %>%
  select(ncdsid, starts_with("missing_"))

# Step 2: Merge with matched dataset
full_data <- matched_dataset %>%
  left_join(missing_flags, by = "ncdsid")

# Step 3: Variables to stratify by
vars_to_check <- c("epilepsy_binary", "SES_score", "sex_birth", "gestational_period", "bullied_7_cat", "bullied_11_cat")

# Step 4: Function to print simple summary tables
print_missing_summary <- function(data, missing_var, vars_to_check) {
  cat("\n--- Missingness for", missing_var, "---\n")
  for (var in vars_to_check) {
    summary_tab <- data %>%
      group_by(!!sym(var), !!sym(missing_var)) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(!!sym(var)) %>%
      mutate(percent = round(n / sum(n) * 100, 1)) %>%
      arrange(!!sym(var), desc(!!sym(missing_var)))

    cat("\nVariable:", var, "\n")
    print(summary_tab)
  }
}

# 🔹 Step 4.1: Print total Ns for each missing variable
print_missing_totals <- function(data, missing_var) {
  total_available <- sum(data[[missing_var]] == "Available", na.rm = TRUE)
  total_missing <- sum(data[[missing_var]] == "Missing", na.rm = TRUE)

  cat("\nTotal N with Available data for", missing_var, ":", total_available, "\n")
  cat("Total N with Missing data for", missing_var, ":", total_missing, "\n")
}

# Step 5: Apply function for all relevant composite scores
missing_vars <- c(
  "missing_internalising_7", "missing_externalising_7",
  "missing_internalising_11", "missing_externalising_11",
  "missing_internalising_16", "missing_externalising_16"
)

for (miss_var in missing_vars) {
  print_missing_summary(full_data, miss_var, vars_to_check)
  print_missing_totals(full_data, miss_var)
}


```

Analyses

```{r echo=FALSE, message=FALSE, warning=FALSE}

############################################################################

# A: Main Analyses Age 7

#############################################################################

# Linear model for epilepsy predicting internalising at age 7
model_epilepsy_internalising_7 <- lm(internalising_7 ~ epilepsy_binary,
                     data = matched_dataset)

cat("\n# Linear model for epilepsy predicting internalising at age 7\n")
summary(model_epilepsy_internalising_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_7)

# Linear model for epilepsy predicting internalising at age 7
model_epilepsy_internalisingb_7 <- lm(internalising_7 ~ epilepsy_binary + bullied_7,
                     data = matched_dataset)

cat("\n# Linear model for epilepsy predicting internalising at age 7 controlling for bullying victimisation\n")
summary(model_epilepsy_internalisingb_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalisingb_7)

# Linear model for epilepsy predicting externalising at age 7
model_epilepsy_externalising_7 <- lm(externalising_7 ~ epilepsy_binary,
                     data = matched_dataset)

cat("\n# Linear model for epilepsy predicting externalising at age 7\n")
summary(model_epilepsy_externalising_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_7)

# Linear model for epilepsy predicting externalising at age 7
model_epilepsy_externalisingb_7 <- lm(externalising_7 ~ epilepsy_binary + bullied_7,
                     data = matched_dataset)

cat("\n# Linear model for epilepsy predicting externalising at age 7 controlling for bullying victimisation\n")
summary(model_epilepsy_externalisingb_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalisingb_7)

# Linear model for bullying victimisation at age 7 predicting internalising at age 7
model_bullied_7_internalising_7 <- lm(internalising_7 ~ bullied_7,
                      data = matched_dataset)

cat("\n# Linear model for bullying victimisation predicting internalising at age 7\n")
summary(model_bullied_7_internalising_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_internalising_7)

# Linear model for bullying victimisation at age 7 predicting externalising at age 7
model_bullied_7_externalising_7 <- lm(externalising_7 ~ bullied_7,
                      data = matched_dataset)

cat("\n# Linear model for bullying victimisation predicting internalising at age 7\n")
summary(model_bullied_7_externalising_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_externalising_7)

# Linear model for epilepsy predicting bullying victimisation at age 7
model_epilepsy_bullied_7 <- lm(bullied_7 ~ epilepsy_binary,
                     data = matched_dataset)

cat("\n# Linear model for epilepsy predicting bullying victimisation at age 7\n")
summary(model_epilepsy_bullied_7)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_bullied_7)

############################################################################

# B: Main Analyses Age 11

#############################################################################

# Linear model for epilepsy predicting internalising at age 11
model_epilepsy_internalising_11 <- lm(internalising_11 ~ epilepsy_binary,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting internalising at age 11\n")
summary(model_epilepsy_internalising_11)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_11)

# Linear model for epilepsy predicting externalising at age 11
model_epilepsy_externalising_11 <- lm(externalising_11 ~ epilepsy_binary,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting externalising at age 11\n")
summary(model_epilepsy_externalising_11)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_11)

# Linear model for epilepsy predicting externalising at age 11
model_epilepsy_externalising_11b <- lm(externalising_11 ~ epilepsy_binary + bullied_7,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting externalising at age 11 controlling for bullying victimisation\n")
summary(model_epilepsy_externalising_11b)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_11b)

# Linear model for bullying victimisation at age 7 predicting externalising at age 11
model_bullied_7_externalising_11 <- lm(externalising_11 ~ bullied_7,
                      data = matched_dataset)

cat("\n# Linear model for bullying victimisation predicting externalising at age 11\n")
summary(model_bullied_7_externalising_11)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_externalising_11)

# Linear model for epilepsy predicting bullying victimisation at age 11
model_epilepsy_bullied_11 <- lm(bullied_11 ~ epilepsy_binary,
                     data = matched_dataset)

cat("\n# Linear model for epilepsy predicting bullying victimisation at age 11\n")
summary(model_epilepsy_bullied_11)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_bullied_11)

############################################################################

# C: Main Analyses Age 16

#############################################################################

# Linear model for epilepsy predicting internalising at age 16
model_epilepsy_internalising_16 <- lm(internalising_16 ~ epilepsy_binary,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting internalising at age 16\n")
summary(model_epilepsy_internalising_16)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_16)

# Linear model for epilepsy predicting internalising at age 16
model_epilepsy_internalising_16b <- lm(internalising_16 ~ epilepsy_binary + bullied_by_11,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting internalising at age 16 controlling for bullying victimisation\n")
summary(model_epilepsy_internalising_16b)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_16b)

# Linear model for epilepsy predicting externalising at age 16
model_epilepsy_externalising_16 <- lm(externalising_16 ~ epilepsy_binary,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting externalising at age 16\n")
summary(model_epilepsy_externalising_16)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_16)

# Linear model for epilepsy predicting externalising at age 16
model_epilepsy_externalising_16b <- lm(externalising_16 ~ epilepsy_binary + bullied_by_11,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting externalising at age 16 controlling for bullying victimisation\n")
summary(model_epilepsy_externalising_16b)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_16b)

# Linear model for bullying victimisation at age 7 predicting internalising at age 16
model_bullied_7_internalising_16 <- lm(internalising_16 ~ bullied_by_11,
                      data = matched_dataset)

cat("\n# Linear model for bullying victimisation predicting internalising at age 16\n")
summary(model_bullied_7_internalising_16)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_internalising_16)

# Linear model for bullying victimisation at age 11 predicting externalising at age 11
model_bullied_by_11 <- lm(bullied_by_11 ~ epilepsy_binary,
                      data = matched_dataset)

cat("\n# Linear model for epilepsy predicting bullying victimisation at age 11\n")
summary(model_bullied_by_11)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_by_11)

############################################################################

# Mediation Analysis Age 7

#############################################################################

# Now detach dplyr temporarily (if not needed for model)
detach("package:dplyr", unload = TRUE)

# Run mediator model (linear regression, since the mediator is now continuous)
mediator_model_7 <- lm(bullied_7 ~ epilepsy_binary, data = matched_dataset)

# Run outcome model (linear regression)
outcome_model_7 <- lm(internalising_7 ~ bullied_7 + epilepsy_binary, data = matched_dataset)

# Run the mediation analysis
library(mediation)
mediation_results_7 <- mediate(mediator_model_7, outcome_model_7,
                               treat = "epilepsy_binary",
                               mediator = "bullied_7",
                               boot = TRUE, sims = 5000)

# Summarize the mediation analysis results
cat("\n# Mediation model for bullying victimisation at age 7 mediating association between epilepsy and internalising at age 7\n")
summary(mediation_results_7)

############################################################################

# Mediation Analysis Age 16

#############################################################################

# Run mediator model (linear regression, since the mediator is now continuous)
mediator_model_16 <- lm(bullied_by_11 ~ epilepsy_binary,
                         data = matched_dataset)

# Run outcome model (linear regression)
outcome_model_16 <- lm(internalising_16 ~ bullied_by_11 + epilepsy_binary,
                        data = matched_dataset)

# Run the mediation analysis
mediation_results_16 <- mediate(mediator_model_16, outcome_model_16,
                             treat = "epilepsy_binary",
                             mediator = "bullied_by_11",
                             boot = TRUE, sims = 5000)

# Summarize the mediation analysis results
cat("\n# Mediation model for bullying victimisation by age 11 mediating association between epilepsy and internalising at age 16\n")
summary(mediation_results_16)


```

Sensitivity Analyses

```{r echo=FALSE, message=FALSE, warning=FALSE}

###########################################################################

# A: Main Analyses Age 7 - Sensitivity

############################################################################

# Linear model for epilepsy predicting internalising at age 7
model_epilepsy_internalising_7s <- lm(internalising_7 ~ epilepsy_binary,
                     data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting internalising at age 7\n")
summary(model_epilepsy_internalising_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_7s)

# Linear model for epilepsy predicting internalising at age 7
model_epilepsy_internalisingb_7s <- lm(internalising_7 ~ epilepsy_binary + bullied_7,
                     data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting internalising at age 7 controlling for bullying victimisation\n")
summary(model_epilepsy_internalisingb_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalisingb_7s)

# Linear model for epilepsy predicting externalising at age 7
model_epilepsy_externalising_7s <- lm(externalising_7 ~ epilepsy_binary,
                     data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting externalising at age 7\n")
summary(model_epilepsy_externalising_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_7s)

# Linear model for epilepsy predicting externalising at age 7
model_epilepsy_externalisingb_7s <- lm(externalising_7 ~ epilepsy_binary + bullied_7,
                     data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting externalising at age 7 controlling for bullying victimisation\n")
summary(model_epilepsy_externalisingb_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalisingb_7s)

# Linear model for bullying victimisation at age 7 predicting internalising at age 7
model_bullied_7_internalising_7s <- lm(internalising_7 ~ bullied_7,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for bullying victimisation predicting internalising at age 7\n")
summary(model_bullied_7_internalising_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_internalising_7s)

# Linear model for bullying victimisation at age 7 predicting externalising at age 7
model_bullied_7_externalising_7s <- lm(externalising_7 ~ bullied_7,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for bullying victimisation predicting externalising at age 7\n")
summary(model_bullied_7_externalising_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_externalising_7s)

# Linear model for epilepsy predicting bullying victimisation at age 7
model_epilepsy_bullied_7s <- lm(bullied_7 ~ epilepsy_binary,
                     data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting bullying victimisation at age 7\n")
summary(model_epilepsy_bullied_7s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_bullied_7s)

############################################################################

# B: Main Analyses Age 11

#############################################################################

# Linear model for epilepsy predicting internalising at age 11
model_epilepsy_internalising_11s <- lm(internalising_11 ~ epilepsy_binary,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting internalising at age 11\n")
summary(model_epilepsy_internalising_11s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_11s)

# Linear model for epilepsy predicting externalising at age 11
model_epilepsy_externalising_11s <- lm(externalising_11 ~ epilepsy_binary,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting externalising at age 11\n")
summary(model_epilepsy_externalising_11s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_11s)

# Linear model for epilepsy predicting externalising at age 11
model_epilepsy_externalising_11bs <- lm(externalising_11 ~ epilepsy_binary + bullied_7,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting externalising at age 11 controlling for bullying victimisation\n")
summary(model_epilepsy_externalising_11bs)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_11bs)

# Linear model for bullying victimisation at age 7 predicting externalising at age 11
model_bullied_7_externalising_11s <- lm(externalising_11 ~ bullied_7,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for bullying victimisation predicting externalising at age 11\n")
summary(model_bullied_7_externalising_11s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_externalising_11s)

# Linear model for epilepsy predicting bullying victimisation at age 11
model_epilepsy_bullied_11s <- lm(bullied_11 ~ epilepsy_binary,
                     data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting bullying victimisation at age 11\n")
summary(model_epilepsy_bullied_11s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_bullied_11s)

############################################################################

# C: Main Analyses Age 16

#############################################################################

# Linear model for epilepsy predicting internalising at age 16
model_epilepsy_internalising_16s <- lm(internalising_16 ~ epilepsy_binary,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting internalising at age 16\n")
summary(model_epilepsy_internalising_16s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_16s)

# Linear model for epilepsy predicting internalising at age 16
model_epilepsy_internalising_16bs <- lm(internalising_16 ~ epilepsy_binary + bullied_by_11,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting internalising at age 16 controlling for bullying victimisation\n")
summary(model_epilepsy_internalising_16bs)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_internalising_16bs)

# Linear model for epilepsy predicting externalising at age 16
model_epilepsy_externalising_16s <- lm(externalising_16 ~ epilepsy_binary,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting externalising at age 16\n")
summary(model_epilepsy_externalising_16s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_16s)

# Linear model for epilepsy predicting externalising at age 16
model_epilepsy_externalising_16bs <- lm(externalising_16 ~ epilepsy_binary + bullied_by_11,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting externalising at age 16 controlling for bullying victimisation\n")
summary(model_epilepsy_externalising_16bs)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_epilepsy_externalising_16bs)

# Linear model for bullying victimisation at age 7 predicting internalising at age 16
model_bullied_7_internalising_16s <- lm(internalising_16 ~ bullied_by_11,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for bullying victimisation predicting internalising at age 16\n")
summary(model_bullied_7_internalising_16s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_7_internalising_16s)

# Linear model for epilepsy predicting bullying victimisation by age 11
model_bullied_by_11s <- lm(bullied_by_11 ~ epilepsy_binary,
                      data = sensitivity_dataset)

cat("\n# Sensitivity Analysis - Linear model for epilepsy predicting bullying victimisation by age 11\n")
summary(model_bullied_by_11s)

# Adding confidence intervals
cat("\n# Confidence Intervals for the model coefficients\n")
confint(model_bullied_by_11s)

############################################################################

# Mediation Analysis Age 7

#############################################################################

# Run mediator model (linear regression, since the mediator is now continuous)
mediator_model_7s <- lm(bullied_7 ~ epilepsy_binary,
                         data = sensitivity_dataset)

# Run outcome model (linear regression)
outcome_model_7s <- lm(internalising_7 ~ bullied_7 + epilepsy_binary,
                        data = sensitivity_dataset)

# Run the mediation analysis
library(mediation)
mediation_results_7s <- mediate(mediator_model_7s, outcome_model_7s,
                             treat = "epilepsy_binary",
                             mediator = "bullied_7",
                             boot = TRUE, sims = 5000)

# Summarize the mediation analysis results
cat("\n# Sensitivity Analysis - Mediation model for bullying victimisation at age 7 mediating association between epilepsy and internalising at age 7\n")
summary(mediation_results_7s)

############################################################################

# Mediation Analysis Age 16

#############################################################################

# Run mediator model (linear regression, since the mediator is now continuous)
mediator_model_16s <- lm(bullied_by_11 ~ epilepsy_binary,
                         data = sensitivity_dataset)

# Run outcome model (linear regression)
outcome_model_16s <- lm(internalising_16 ~ bullied_by_11 + epilepsy_binary,
                        data = sensitivity_dataset)

# Run the mediation analysis
mediation_results_16s <- mediate(mediator_model_16s, outcome_model_16s,
                             treat = "epilepsy_binary",
                             mediator = "bullied_by_11",
                             boot = TRUE, sims = 5000)

# Summarize the mediation analysis results
cat("\n# Sensitivity Analysis - Mediation model for bullying victimisation at age 7 mediating association between epilepsy and internalising at age 16\n")
summary(mediation_results_16s)


