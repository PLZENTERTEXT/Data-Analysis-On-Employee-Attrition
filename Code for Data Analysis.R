# CHANG SHIAU HUEI

# ================================================================


# 2.0 IMPORT DATA
# 2.1 Import dataset
raw_data = read.csv("C:\\Users\\shiau\\OneDrive - Asia Pacific University\\DEGREE\\Y2\\PFDA\\ASSIGNMENT\\employee_attrition.csv", header=TRUE)
View(raw_data)

data <- raw_data

# 2.2 Import packages
# To check exactly what packages have I used throughout the code
## install.packages("NCmisc")
library(NCmisc)
list.functions.in.file("C:\\Users\\shiau\\OneDrive - Asia Pacific University\\DEGREE\\Y2\\PFDA\\ASSIGNMENT\\CHANG SHIAU HUEI_TP060322.R", alphabetic = TRUE)

# For data cleaning & data exploration
## install.packages("janitor")
library(janitor)

# For data exploration, data manipulation & data transformation
library(dplyr)

# For data visualization 
## install.packages("ggplot2")
library(ggplot2)

## install.packages("plotrix")
library(plotrix)

## install.packages("plotly")
library(plotly)

## install.packages("viridis")
library("viridis")

# For making a correlation matrix
## install.packages("ggcorrplot")
library(ggcorrplot)

# ================================================================


# 3.0 DATA CLEANING

# 3.1 - Remove Redundant Columns
data$gender_short = NULL

# 3.2 - Check for Duplicate Records
get_dupes(data)

# 3.3 - Check for Empty Columns or Rows (Excluding "Not Applicable")
colSums(data == "")

### VERY IMPORTANT
### Before doing 3.4, 4.0 - PRE-PROCESSING DATA is done first


# ================================================================


# 4.0 PRE-PROCESSING DATA

# 4.1 - Change Column Names
# 4.1.1 - Clean Column Names
colnames(data) # View column headings
data <- clean_names(data, case="all_caps")

# 4.1.2 Change Column Names
colnames(data)[2] <- "RECORD_DATE"
colnames(data)[3] <- "BIRTH_DATE"
colnames(data)[4] <- "ORI_HIRE_DATE"
colnames(data)[5] <- "TERMINATION_DATE"
colnames(data)[8] <- "CITY"
colnames(data)[9] <- "DEPARTMENT"
colnames(data)[11] <- "STORE"
colnames(data)[12] <- "GENDER"
colnames(data)[13] <- "TERMINATION_REASON"
colnames(data)[14] <- "TERMINATION_TYPE"
colnames(data)[15] <- "RECORD_YEAR"
colnames(data)


# 4.2 - Correcting Data Values
data = data %>% 
  mutate(CITY = ifelse(CITY=="New Westminister","New Westminster", CITY)) 

data = data %>% 
  mutate(TERMINATION_REASON = ifelse(TERMINATION_REASON=="Resignaton","Resignation", TERMINATION_REASON)) 

data = data %>% 
  mutate(JOB_TITLE = ifelse(JOB_TITLE=="CHief Information Officer","Chief Information Officer", JOB_TITLE))

data = data %>% 
  mutate(JOB_TITLE = ifelse(JOB_TITLE=="Accounts Receiveable","Accounts Receivable", JOB_TITLE)) 


# 4.3 - Change Datatypes
# 4.3.1 - Check Datatypes
str(data)

# 4.3.2 - Character to POS Datatype
data$RECORD_DATE <- strptime(data$RECORD_DATE,format="%m/%d/%Y %H:%M")
str(data$RECORD_DATE)

# 4.3.3 - Character to Date Datatype
for(i in 3:5) {
  data[,i] <- as.Date(data[,i], "%m/%d/%Y")
}
str(data[3:5])

# 4.3.4 - Character to Factor Datatype
data$EMPLOYEE_ID <- factor(data$EMPLOYEE_ID)
for(i in 8:14) {
  data[,i] <- factor(data[,i])
}
data$STATUS <- factor(data$STATUS)
data$BUSINESS_UNIT <- factor(data$BUSINESS_UNIT)
str(data)


# ================================================================


# 3.0 DATA CLEANING (PART 2)

# Save full data into another variable
data_full <- data

# 3.4 - Only taking the latest record of employees
data = data %>%
  group_by(EMPLOYEE_ID) %>%
  dplyr::arrange(desc(RECORD_DATE),.by_group = TRUE) %>%
  distinct(EMPLOYEE_ID,.keep_all = TRUE)
## desc() -> Latest records come first

## Making sure it is a data frame to avoid data being 4 different classes
## grouped_df" "tbl_df"     "tbl"        "data.frame"
data <- data.frame(data)


# ================================================================


# 5.0 DATA EXPLORATION

# 5.1 - VIEWING DATASET
# 5.1.1 -Viewing the “data_full” Data Set
## View the first few values of the data set
glimpse(data)

## How the data is stored (Chosen by compiler)
class(data)

## Summary of the data
summary(data)

# 5.1.2 -Viewing the “data_full” Data Set
## View the first few values of the data set
glimpse(data_full)

## How the data is stored (Chosen by compiler)
class(data_full)

## Summary of the data
summary(data_full)

# -----------------------------------------------------------------------------------

# 5.2 - "EMPLOYEE_ID" COLUMN
## Unique data
unique(data$EMPLOYEE_ID)

## Counting the amount of distinct values
nlevels(data$EMPLOYEE_ID)

## Determining the datatype
class(data$EMPLOYEE_ID)

### NOT ALL LATEST RECORDS -----------------------------------------------------
## Data summary
summary(data_full$EMPLOYEE_ID)

## List of the frequency of each values
exp_emp_id <- tabyl(data_full, EMPLOYEE_ID) %>% 
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_emp_id

## The summary of the frequency of each values
summary(exp_emp_id$n)

## Create a table that displays the frequency of the number of occurrence for each employee ID
exp_emp_id <- tabyl(data_full, EMPLOYEE_ID) %>% 
  dplyr::arrange( n ) %>% # Arrange "n" in order
  split( .[,"n"] ) %>%    # Splits "n" into different dimensions
  purrr::map_df(., janitor::adorn_totals) %>%
  # :: is to get this 1 function from a certain package
  # . is to get all variable
  filter(EMPLOYEE_ID == "Total")

## Appending the frequency number column to the data since the min & max frequency is known
Times_Repeated = factor(c(1:10))
exp_emp_id = cbind(exp_emp_id, Times_Repeated)

## Deleting unecessary columns
exp_emp_id["EMPLOYEE_ID"] = NULL

## Reorder column
exp1_emp_id <- exp_emp_id[, c(3,1,2)]

## TABLE: Frequency of the number of occurrence for EMPLOYEE_ID
exp1_emp_id %>%
    adorn_totals() %>%
    adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)

## CHART: Frequency of Repeated EMPLOYEE_ID
ggplot(exp_emp_id, aes(x=Times_Repeated, y=n)) +
  geom_bar(stat="identity", fill=magma(10)) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of the Number of Occurrence of EMPLOYEE_ID")

### ALL LATEST RECORDS ---------------------------------------------------------
## The summary of the frequency of each values --> We know that it is unique
summary(data$EMPLOYEE_ID)

# ----------------------------------------------------------------------------------------------------

# 5.3 - "RECORD_DATE" COLUMN
## Determining the datatype
class(data$RECORD_DATE)

### NOT ALL LATEST RECORDS -----------------------------------------------------
## Data summary
summary(data_full$RECORD_DATE)

## Unique data
unique(data_full$RECORD_DATE)

## List of the frequency of each date -> Most on 31 December of each year
exp_recorddate <- tabyl(data_full, RECORD_DATE) %>% 
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_recorddate

## The summary of frequency value for a date
summary(exp_recorddate$n)

## Extracting just the month value of the dates
exp_recorddate_month = format(as.Date(data_full$RECORD_DATE, format="%Y/%m/%d"),"%m")

## Counting the amount of distinct months -> Records are done in all 12 months
n_distinct(exp_recorddate_month)

## Arranging the months and their frequency
exp_recorddate_month <- tabyl(exp_recorddate_month) %>%
  dplyr::arrange( n ) %>%
  dplyr::arrange(exp_recorddate_month)
names(exp_recorddate_month)[1] <- "Month"

## TABLE: Frequency of RECORD_DATE by Month (Including All Records)
exp1_recorddate <- exp_recorddate_month %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_recorddate

## CHART: Frequency of RECORD_DATE by Month (Including All Records) -> For employees that are not terminated yet,
##        their records are updated every December
exp_recorddate_vis <- exp_recorddate_month %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
ggplot(exp_recorddate_vis, aes(x=Month, y=n)) + 
  geom_bar(stat="identity", 
           fill=viridis(n_distinct(exp_recorddate_month))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of RECORD_DATE by Month (Including All Records)")

### ALL LATEST RECORDS ---------------------------------------------------------
## Data summary
summary(data$RECORD_DATE)

## Unique data
unique(data$RECORD_DATE)

## List of the frequency of each date
exp_recorddate <- tabyl(data, RECORD_DATE) %>% 
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_recorddate

## The summary of frequency value for a date
summary(exp_recorddate$n)

## Extracting just the month value of the dates
exp_recorddate_month = format(as.Date(data$RECORD_DATE, format="%Y/%m/%d"),"%m")

## Counting the amount of distinct months -> Records are done in all 12 months
n_distinct(exp_recorddate_month)

## Arranging the months and their frequency
exp_recorddate_month <- tabyl(exp_recorddate_month) %>%
  dplyr::arrange( n ) %>%
  dplyr::arrange(exp_recorddate_month)
names(exp_recorddate_month)[1] <- "Month"

## TABLE: Frequency of RECORD_DATE by Month (Including Only Latest Records)
exp2_recorddate <- exp_recorddate_month %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp2_recorddate

## CHART: Frequency of RECORD_DATE by Month (Including Only Latest Records)
exp_recorddate_vis2 <- exp_recorddate_month %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
ggplot(exp_recorddate_vis2, aes(x=Month, y=n)) + 
  geom_bar(stat="identity", 
           fill=viridis(n_distinct(exp_recorddate_month))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of RECORD_DATE by Month (Including Only Latest Records)")

# -----------------------------------------------------------------------------------

# 5.4 - "BIRTH_DATE" COLUMN
## Data summary
summary(data$BIRTH_DATE)

## Unique data
unique(data$BIRTH_DATE)

## Counting the amount of distinct values
n_distinct(data$BIRTH_DATE)

## Determining the datatype
class(data$BIRTH_DATE)

## List of the frequency of each date
exp_birthdate <- tabyl(data, BIRTH_DATE) %>% 
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_birthdate

## The summary of frequency value for a date
summary(exp_birthdate$n)

## Extracting just the year value of the dates
exp_birthdate_year = format(as.Date(data$BIRTH_DATE, format="%Y/%m/%d"),"%Y")

## Counting the amount of distinct years
n_distinct(exp_birthdate_year)

## Arranging the years and their frequency
exp_birthdate_year <- tabyl(exp_birthdate_year) %>% 
  dplyr::arrange( n ) %>%
  dplyr::arrange(exp_birthdate_year)
names(exp_birthdate_year)[1] <- "Year"

## TABLE: Frequency of BIRTH_DATE by Year
exp1_birthdate <- exp_birthdate_year %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_birthdate

## CHART: Frequency of BIRTH_DATE by Year
exp_birthdate_vis <- exp_birthdate_year
ggplot(exp_birthdate_vis, aes(x=n, y=Year, group=1)) +
  # group=1 is to confirm that you are doing only 1 observation
  geom_bar(stat="identity",
           fill=viridis(n_distinct(exp_birthdate_year))) +
  geom_path(stat="identity", color="red", size=1) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of BIRTH_DATE by Year")

# MOST: 1988 - 149 (2.37%)

# == GENERATIONS == 
# Traditionalists -> Born in 1945 and below
# AGE >= 51       -> Boomers (Born in 1946 - 1964)
# AGE 39 - 50     -> Gen X (Born in 1965 - 1976)
# AGE 20 - 38     -> Millennials / Gen Y (Born in 1977 - 1995)
# AGE <= 19       -> Gen Z (Born in 1996 - 2015)

# -----------------------------------------------------------------------------------

# 5.5 - "ORI_HIRE_DATE" COLUMN
## Data summary
summary(data$ORI_HIRE_DATE)

## Unique data
unique(data$ORI_HIRE_DATE)

## Counting the amount of distinct values
n_distinct(data$ORI_HIRE_DATE)

## Determining the datatype
class(data$ORI_HIRE_DATE)

## List of the frequency of each date
exp_orihiredate <- tabyl(data, ORI_HIRE_DATE) %>% 
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_orihiredate

## The summary of frequency value for a date
summary(exp_orihiredate$n)

## Extracting just the year value of the dates
exp_orihiredate_year = format(as.Date(data$ORI_HIRE_DATE, format="%Y/%m/%d"),"%Y")

## Counting the amount of distinct years
n_distinct(exp_orihiredate_year)

## Arranging the years and their frequency
exp_orihiredate_year <- tabyl(exp_orihiredate_year) %>% 
  dplyr::arrange( n ) %>%
  dplyr::arrange(exp_orihiredate_year)
names(exp_orihiredate_year)[1] <- "Year"

## TABLE: Frequency of ORI_HIRE_DATE by Year
exp1_orihiredate <- exp_orihiredate_year %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_orihiredate

## CHART: Frequency of ORI_HIRE_DATE by Year
exp_orihiredate_vis <- exp_orihiredate_year
ggplot(exp_orihiredate_vis, aes(x=n, y=Year)) + 
  geom_bar(stat="identity", 
           fill=viridis(n_distinct(exp_orihiredate_year))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of ORI_HIRE_DATE by Year")

# MOST: 1998 - 341 (5.43%)

# -----------------------------------------------------------------------------------

# 5.6 - "TERMINATION_DATE" COLUMN
## To extract TERMINATED employees from the data
termdate <- filter(data, STATUS=="TERMINATED")

## Data summary
summary(termdate$TERMINATION_DATE)

## Unique data
unique(termdate$TERMINATION_DATE)

## Counting the amount of distinct values
n_distinct(termdate$TERMINATION_DATE)

## Determining the datatype
class(termdate$TERMINATION_DATE)

## List of the frequency of each date
exp_termdate <- tabyl(termdate, TERMINATION_DATE) %>% 
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
tail(exp_termdate,10)

## The summary of frequency value for a date
summary(exp_termdate$n)

## Extracting just the year value of the dates
exp_termdate_year = format(as.Date(termdate$TERMINATION_DATE, format="%Y/%m/%d"),"%Y")

## Counting the amount of distinct years
n_distinct(exp_termdate_year)

## Arranging the years and their frequency
exp_termdate_year <- tabyl(exp_termdate_year) %>% 
  dplyr::arrange( n ) %>%
  dplyr::arrange(exp_termdate_year)
names(exp_termdate_year)[1] <- "Year"

## TABLE: Frequency of TERMINATION_DATE by Year
exp1_termdate <- exp_termdate_year %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_termdate

## CHART: Frequency of TERMINATION_DATE by Year
exp_termdate_vis <- exp_termdate_year
ggplot(exp_termdate_vis, aes(y=n, x=Year)) + 
  geom_bar(stat="identity", 
           fill=viridis(n_distinct(exp_termdate_year))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of TERMINATION_DATE by Year")

# MOST: 2014 - 252 (17.03%)

# -----------------------------------------------------------------------------------

# 5.7 - "AGE" COLUMN
## Data summary
summary(data$AGE)

## Unique data
unique(data$AGE)

## Counting the amount of distinct values
n_distinct(data$AGE)

## Determining the datatype
class(data$AGE)

## TABLE: Frequency of AGE
exp_age <- tabyl(data, AGE) %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_age

## CHART: Frequency and Density of AGE
ggplotly(ggplot(data, aes(x=AGE)) + 
   geom_histogram(colour="white", aes(y=..density.., fill=..count..), binwidth=1) +
   scale_fill_gradient("Count", low="hotpink", high="turquoise") +
   geom_density(alpha=.1, fill="black") +
   ggtitle("Frequency and Density of AGE"))

# MOST:
#   AGE 65 - 591 (9.40%)
#   AGE 60 - 401 (6.38%)

# -----------------------------------------------------------------------------------

# 5.8 - "LENGTH_OF_SERVICE" COLUMN
## Data summary
summary(data$LENGTH_OF_SERVICE)

## Unique data
unique(data$LENGTH_OF_SERVICE)

## Determining the datatype
class(data$LENGTH_OF_SERVICE)

## TABLE: Frequency of LENGTH_OF_SERVICE
exp_len_service <- tabyl(data, LENGTH_OF_SERVICE) %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp_len_service

## CHART: Frequency and Density of LENGTH_OF_SERVICE 
ggplotly(ggplot(data, aes(x=LENGTH_OF_SERVICE)) + 
   geom_histogram(colour="white", aes(y=..density.., fill=..count..), binwidth=1) +
   scale_fill_gradient("Count", low="hotpink", high="turquoise") +
   geom_density(alpha=.1, fill="black") +
   ggtitle("Frequency and Density of LENGTH_OF_SERVICE"))

# MOST:
#   13 YEARS - 700 (11.14%)
#    8 YEARS - 195 (6.29%)

# -----------------------------------------------------------------------------------

# 5.9 - "CITY" COLUMN
## Data summary
summary(data$CITY)

## Unique data
unique(data$CITY)

## Counting the amount of distinct values
nlevels(data$CITY)

## Determining the datatype
class(data$CITY)

## List of the frequency of each values
exp_city <- tabyl(data, CITY)

## TABLE: Frequency of CITY
exp1_city <- exp_city %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_city

## CHART: Frequency of CITY
ggplot(exp_city, aes(x=n, y=CITY)) +
  geom_bar(stat="identity", fill=magma(nlevels(data$CITY))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of CITY")

# MOST: Vancouver - 1392 (22.15%)

# -----------------------------------------------------------------------------------

# 5.10 - "DEPARTMENT" COLUMN
## Data summary
summary(data$DEPARTMENT)

## Unique data
unique(data$DEPARTMENT)

## Counting the amount of distinct values
nlevels(data$DEPARTMENT)

## Determining the datatype
class(data$DEPARTMENT)

## List of the frequency of each values
exp_dept <- tabyl(data, DEPARTMENT)

## TABLE: Frequency of DEPARTMENT
exp1_dept <- exp_dept %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_dept

## CHART: Frequency of DEPARTMENT
ggplot(exp_dept, aes(x=n, y=DEPARTMENT)) +
  geom_bar(stat="identity", fill=magma(nlevels(data$DEPARTMENT))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of DEPARTMENT")

# MOST:
#   Meats - 1252 (19.92%)
#   Customer Service - 1190 (18.94%)

# -----------------------------------------------------------------------------------

# 5.11 - "JOB_TITLE" COLUMN
## Data summary
summary(data$JOB_TITLE)

## Unique data
unique(data$JOB_TITLE)

## Counting the amount of distinct values
nlevels(data$JOB_TITLE)

## Determining the datatype
class(data$JOB_TITLE)

## List of the frequency of each values
exp_job <- tabyl(data, JOB_TITLE)

## List of the frequency of each values (Director)
select(exp_job, c(JOB_TITLE,n)) %>% 
  dplyr::filter(grepl('Director', JOB_TITLE)) %>%
  adorn_totals()

## List of the frequency of each values (Exec Assistant)
select(exp_job, c(JOB_TITLE,n)) %>% 
  dplyr::filter(grepl('Exec Assistant', JOB_TITLE)) %>%
  adorn_totals()

## TABLE: Frequency of JOB_TITLE
exp1_job <- exp_job %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_job

## CHART: Frequency of JOB_TITLE
ggplot(exp_job, aes(x=n, y=JOB_TITLE)) +
  geom_bar(stat="identity", fill=magma(nlevels(data$JOB_TITLE))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of JOB_TITLE")

# MOST:
#   Meat Cutter - 1218 (19.38%)
#   Cashier - 1158 (18.43%)

# -----------------------------------------------------------------------------------

# 5.12 - "STORE" COLUMN
## Data summary
summary(data$STORE)

## Unique data
unique(data$STORE)

## Counting the amount of distinct values
nlevels(data$STORE)

## Determining the datatype
class(data$JOB_TITLE)

## List of the frequency of each values
exp_store <- tabyl(data, STORE)

## TABLE: Frequency of STORE
exp1_store <- exp_store %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_store

## CHART: Frequency of STORE
ggplot(exp_store, aes(x=n, y=STORE)) +
  geom_bar(stat="identity", fill=magma(nlevels(data$STORE))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of STORE")

# MOST:
#   Store 46 - 522 (8.31%)
#   Store 18 - 481 (7.65%)

# -----------------------------------------------------------------------------------

# 5.13 - "GENDER" COLUMN
## Data summary
summary(data$GENDER)

## Unique data
unique(data$GENDER)

## Counting the amount of distinct values
nlevels(data$GENDER)

## Determining the datatype
class(data$GENDER)

## List of the frequency of each values
exp_gender <- tabyl(data, GENDER)

## TABLE: Frequency of GENDER
exp1_gender <- exp_gender %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp1_gender

## CHART: Frequency of GENDER
exp_gender_vis <- exp_gender %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
pie(exp_gender_vis$n, 
    label=paste0(exp_gender_vis$GENDER, 
                 "\nFrequency = ", exp_gender_vis$n, 
                 "\nPercent = ", exp_gender_vis$percent), 
    radius=1, 
    main="Frequency of GENDER", 
    col=c("pink", "light blue"))

# -----------------------------------------------------------------------------------

# 5.14 - "TERMINATION_REASON" COLUMN
## Data summary
summary(data$TERMINATION_REASON)

## Unique data
unique(data$TERMINATION_REASON)

## Counting the amount of distinct values
nlevels(data$TERMINATION_REASON)

## Determining the datatype
class(data$TERMINATION_REASON)

## List of the frequency of each values
exp_termreason <- tabyl(data, TERMINATION_REASON)

## TABLE: Frequency of TERMINATION_REASON
exp1_termreason <- exp_termreason %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_termreason

## CHART: Frequency of TERMINATION_REASON
ggplot(exp_termreason, aes(x=TERMINATION_REASON, y=n)) +
  geom_bar(stat="identity", fill=viridis(nlevels(data$TERMINATION_REASON))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of TERMINATION_REASON")

## TABLE: Frequency of TERMINATION_REASON (Excluding NA)
exp2_termreason <- filter(exp_termreason, TERMINATION_REASON!="Not Applicable") %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp2_termreason

## CHART: Frequency of TERMINATION_REASON (Excluding NA)
ggplot(filter(exp_termreason, TERMINATION_REASON!="Not Applicable"), 
              aes(x=TERMINATION_REASON, y=n)) +
  geom_bar(stat="identity", fill=cm.colors(3)) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of TERMINATION_REASON (Excluding NA)")

# -----------------------------------------------------------------------------------

# 5.15 - "TERMINATION_TYPE" COLUMN
## Data summary
summary(data$TERMINATION_TYPE)

## Unique data
unique(data$TERMINATION_TYPE)

## Counting the amount of distinct values
nlevels(data$TERMINATION_TYPE)

## Determining the datatype
class(data$TERMINATION_TYPE)

## List of the frequency of each values
exp_termtype <- tabyl(data, TERMINATION_TYPE)

## TABLE: Frequency of TERMINATION_TYPE
exp1_termtype <- exp_termtype %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp1_termtype

## CHART: Frequency of TERMINATION_TYPE
ggplot(exp_termtype, aes(x=TERMINATION_TYPE, y=n)) +
  geom_bar(stat="identity", fill=magma(nlevels(data$TERMINATION_TYPE))) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of TERMINATION_TYPE")

## TABLE: Frequency of TERMINATION_TYPE (Excluding NA)
exp2_termtype <- filter(exp_termtype, TERMINATION_TYPE!="Not Applicable") %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
exp2_termtype

## CHART: Frequency of TERMINATION_TYPE (Excluding NA)
ggplot(filter(exp_termtype, TERMINATION_TYPE!="Not Applicable"), 
       aes(x=TERMINATION_TYPE, y=n)) +
  geom_bar(stat="identity", fill=cm.colors(2)) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of TERMINATION_TYPE (Excluding NA)")

# -----------------------------------------------------------------------------------

# 5.16 - "RECORD_YEAR" COLUMN
## Unique data
unique(data$RECORD_YEAR)

## Counting the amount of distinct values
n_distinct(data$RECORD_YEAR)

## Determining the datatype
class(data$RECORD_YEAR)

### NOT ALL LATEST RECORDS -----------------------------------------------------
## Data summary
summary(data_full$RECORD_YEAR)

## List of the frequency of each year
exp_record_year <- tabyl(data_full, RECORD_YEAR)

## TABLE: Frequency of RECORD_YEAR
exp1_record_year <- exp_record_year %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp1_record_year

## CHART: Frequency of RECORD_YEAR
exp_record_year_vis <- ggplot(exp_record_year, aes(x=RECORD_YEAR, y=n)) +
  geom_line(col="red", size=1) +
  ggtitle("Frequency of RECORD_YEAR (Including All Records)") +
  scale_x_continuous(breaks=unique(data_full$RECORD_YEAR))
ggplotly(exp_record_year_vis)

### ALL LATEST RECORDS ---------------------------------------------------------
## Data summary
summary(data$RECORD_YEAR)

## List of the frequency of each year
exp_record_year <- tabyl(data, RECORD_YEAR)

## TABLE: Frequency of RECORD_YEAR
exp2_record_year <- exp_record_year %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp2_record_year

## CHART: Frequency of RECORD_YEAR
exp_record_year_vis2 <- exp_record_year
ggplot(exp_record_year_vis2, aes(x=RECORD_YEAR, y=n)) +
  geom_bar(stat="identity",
           fill=heat.colors(n_distinct(exp_record_year))) +
  geom_line(col="red", size=1) +
  geom_text(aes(label=n)) +
  ggtitle("Frequency of RECORD_YEAR Overview (Including Only Latest Records)") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR))

# -----------------------------------------------------------------------------------

# 5.17 - "STATUS" COLUMN
## Data summary
summary(data$STATUS)

## Unique data
unique(data$STATUS)

## Counting the amount of distinct values
nlevels(data$STATUS)

## Determining the datatype
class(data$STATUS)

## List of the frequency of each values
exp_status <- tabyl(data, STATUS)

## TABLE: Frequency of STATUS
exp1_status <- exp_status %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp1_status

## CHART: Frequency of STATUS
exp_status_vis <- exp_status %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
pie(exp_status_vis$n, 
    label=paste0(exp_status_vis$STATUS, 
                 "\nFrequency = ", exp_status_vis$n, 
                 "\nPercent = ", exp_status_vis$percent), 
    radius=1, 
    main="Frequency of STATUS", 
    col=c("turquoise", "lightcoral"))

# -----------------------------------------------------------------------------------

# 5.18 - "BUSINESS_UNIT" COLUMN
## Data summary
summary(data$BUSINESS_UNIT)

## Unique data
unique(data$BUSINESS_UNIT)

## Counting the amount of distinct values
nlevels(data$BUSINESS_UNIT)

## Determining the datatype
class(data$BUSINESS_UNIT)

## List of the frequency of each values
exp_business_unit <- tabyl(data, BUSINESS_UNIT)

## TABLE: Frequency of BUSINESS_UNIT
exp1_business_unit <- exp_business_unit %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE)
exp1_business_unit

## CHART: Frequency of BUSINESS_UNIT
exp_business_unit_vis <- exp_business_unit %>%
  adorn_pct_formatting(digits=2, affix_sign=TRUE, rounding="half to even", percent)
pie(exp_business_unit_vis$n, 
    label=paste0(exp_business_unit_vis$BUSINESS_UNIT, 
                 "\nFrequency = ", exp_business_unit_vis$n, 
                 "\nPercent = ", exp_business_unit_vis$percent), 
    radius=1, 
    main="Frequency of BUSINESS_UNIT", 
    col=cm.colors(2))


# ================================================================


# 6.0 QUESTION & ANALYSIS

# 6.1 - Question 1: What is the Company’s Layout?
# 6.1.1 - Question 1.1 - Which Store is the Head Office?
# Analysis 6.1.1.1 - Check if certain stores are focused on certain departments
ggplot(data, aes(x=STORE, y=DEPARTMENT)) + 
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend') +
  labs(title="The Co-variation between STORE and DEPARTMENT", x="STORE", y="DEPARTMENT")
## guides(color = 'legend') - Tells ggplot to display it as legend instead of a separate color bar

# Analysis 6.1.1.2 - Comparing Store 35 with BUSINESS_UNIT data
subset(exp_business_unit, BUSINESS_UNIT=="HEADOFFICE", select=c(BUSINESS_UNIT,n))
subset(exp_store, STORE=="35", select=c(STORE,n))

# Analysis 6.1.1.3 - Departments that the Head Office consists of
store35_HO = nrow(filter(data, STORE=="35" & DEPARTMENT!=("Meats") & DEPARTMENT!=("Dairy")))
store35_HO

# Analysis 6.1.1.4 - Employees that work in Head Office and Stores Departments of Store 35
store35_ST = nrow(filter(data, STORE=="35" & (DEPARTMENT==("Meats") | DEPARTMENT==("Dairy"))))
store35_ST

pie3D(c(store35_HO,store35_ST), 
      labels=c(paste0("HEADOFFICE = ",store35_HO),paste0("STORES = ",store35_ST)), 
      col=cm.colors(2),
      main="Number of Employees that Work in Head Office and Stores Departments of Store 35")

# Analysis 6.1.1.5 - Location of the Head Office (Store 35)
unique(subset(data, STORE=="35", select=CITY))


# 6.1.2 - Question 1.2 - How are the Stores distributed geographically by City?
# Analysis 6.1.2.1 - Relationship between Store and City
data %>% select(STORE, CITY) %>%
  ggplot(aes(x=STORE, y=CITY, colour=CITY)) + 
  geom_point() + 
  labs(colour="CITY", x="Store", y="City", title="Relationship between Store and City")
# New Westminster - 2 (Store 20 & 21)
# Victoria        - 2 (Store 37 & 46)
# Vancouver       - 6 (Store 35, 41, 42, 43, 44, 45)
# Numbering of the store names corresponds to the alphabetical order of the city

# Analysis 6.1.2.2 - Active and Inactive Stores
active_store <- data_full %>%
  select(RECORD_YEAR, STORE, STATUS) %>%
  filter(STATUS=="ACTIVE" & RECORD_YEAR==2015)
ggplotly(ggplot(active_store, aes(x=RECORD_YEAR, fill=STATUS)) +
  geom_bar(fill="limegreen") +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Employees", title="Active and Inactive Stores") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) +
  facet_wrap(. ~ STORE))


# 6.1.3 - Question 1.3 - Does the Age and Length of Service Correlates in this Company?
# Analysis 6.1.3.1 - Correlation between Age and Length of Service
age_los <- data %>% select(AGE, LENGTH_OF_SERVICE)
cor_matrix <- round(cor(age_los), 1)
cor_matrix
ggcorrplot(cor_matrix) + labs(title="Correlation between Age and Length of Service")

# Analysis 6.1.3.2 - Relationship between Age and Length of Service
ggplotly(data %>% select(AGE, LENGTH_OF_SERVICE) %>%
   ggplot(aes(x=AGE, y=LENGTH_OF_SERVICE, fill=AGE)) + 
   geom_boxplot(aes(group=AGE)) + 
   scale_fill_gradient("AGE", low="hotpink", high="turquoise") +
   labs(x="Age", y="Length of Service", title="Relationship between Age and Length of Service") +
   scale_x_continuous(breaks=unique(data$AGE)))

# AGE 60 & 65, some employees served for 8 and 13 years respectively => Below average

# -----------------------------------------------------------------------------------

# 6.2 - Question 2: What Year or Month are Most Employees Getting Terminated?
# Analysis 6.2.1 - Number of employees that are terminated by year -> Most: 2014
data %>% filter(STATUS == "TERMINATED") %>%
  select(RECORD_YEAR,TERMINATION_REASON) %>%
  ggplot(aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Terminated Employees", title="Number of Employees that are Terminated by Year") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR))


# Analysis 6.2.2 - Number of Laid Off employees by year and month -> Most: December, 2014
data %>% filter(TERMINATION_REASON == "Layoff") %>%
  mutate(RECORD_MONTH = format(as.Date(RECORD_DATE, format="%Y/%m/%d"),"%m")) %>%
  select(RECORD_YEAR, RECORD_MONTH,TERMINATION_REASON) %>%
  ggplot(aes(x=RECORD_MONTH, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Month", y="Number of Laid Off Employees", title="Number of Laid Off employees by Year and Month") +
  facet_grid(. ~ RECORD_YEAR)


# Analysis 6.2.3 - Number of Resigned Employees by Year -> Most: 2012
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, TERMINATION_REASON) %>%
  ggplot(aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Resigned Employees", title="Number of Resigned Employees by Year") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) + 
  scale_fill_manual(values = "limegreen")

  # Analysis 6.2.3.1 - Number of Resigned employees by Month in Every Year -> Most: Near the end of year (Except 2012)
resig_year <- data %>% filter(TERMINATION_REASON == "Resignation") %>%
  mutate(RECORD_MONTH = format(as.Date(RECORD_DATE, format="%Y/%m/%d"),"%m")) %>%
  select(RECORD_YEAR, RECORD_MONTH,TERMINATION_REASON)
ggplotly(ggplot(resig_year, aes(x=RECORD_MONTH, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Month", y="Number of Resigned Employees", title="Number of Resigned Employees by Month in Every Year") +
  scale_fill_manual(values = "limegreen") +
  facet_wrap(. ~ RECORD_YEAR))

# Analysis 6.2.3.2 - Number of Resigned employees by Month -> Most: October
ggplot(resig_year, aes(x=RECORD_MONTH, fill=RECORD_YEAR)) +
  geom_bar(fill="springgreen") +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Month", y="Number of Resigned Employees", title="Number of Resigned Employees by Month")


# Analysis 6.2.4 - Number of Retired Employees by Year -> Most: 2006-2010
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, TERMINATION_REASON) %>%
  ggplot(aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Retired Employees", title="Number of Retired Employees by Year") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) + 
  scale_fill_manual(values = "dodgerblue")

# Analysis 6.2.4.1 - Number of Retired employees by Month in Every Year -> Most: Near the end of year
retire_year <- data %>% filter(TERMINATION_REASON == "Retirement") %>%
  mutate(RECORD_MONTH = format(as.Date(RECORD_DATE, format="%Y/%m/%d"),"%m")) %>%
  select(RECORD_YEAR, RECORD_MONTH,TERMINATION_REASON)
ggplotly(ggplot(retire_year, aes(x=RECORD_MONTH, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Month", y="Number of Retired Employees", title="Number of Retired Employees by Month in Every Year") +
  scale_fill_manual(values = "dodgerblue") +
  facet_wrap(. ~ RECORD_YEAR))

# Analysis 6.2.4.2 - Number of Retired employees by Month -> Most: December
ggplot(retire_year, aes(x=RECORD_MONTH, fill=RECORD_YEAR)) +
  geom_bar(fill="deepskyblue") +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Month", y="Number of Retired Employees", title="Number of Retired Employees by Month")

# -----------------------------------------------------------------------------------

# 6.3 - Question 3: What is the Nature of Layoff?
# Analysis 6.3.1 - Relationship between Age and Laid Off Employees -> Most: AGE 64, 22
ggplotly(data %>% filter(TERMINATION_REASON == "Layoff") %>%
   select(RECORD_YEAR, AGE) %>%
   ggplot(aes(x=AGE)) +
   geom_area(aes(y = ..count..), stat ="bin", binwidth=1, fill="tomato") +
   labs(colour="YEAR", x="Age", y="Number of Laid Off employees", title="Relationship between Age and Laid Off Employees") +
   scale_x_continuous(breaks=unique(data$AGE)))
## Use labs(colour=) when colour= is chosen in aes()
## Use labs(fill=) when fill= is chosen in aes()

# Analysis 6.3.1.1 - Relationship between Age and Laid Off Employees by Year -> Young (2014), Senior (2015)
ggplotly(data %>% filter(TERMINATION_REASON == "Layoff") %>%
   select(RECORD_YEAR, AGE) %>%
   ggplot(aes(x=AGE, colour=factor(RECORD_YEAR))) +
   geom_freqpoly(size=1, binwidth=1) +
   labs(colour="YEAR", x="Age", y="Number of Laid Off Employees", title="Relationship between Age and Laid Off Employees by Year") +
   scale_x_continuous(breaks=unique(data$AGE)))

# Analysis 6.3.1.2 - Correlation between Age and Length of Service for Laid Off Employees
age_los <- data %>% filter(TERMINATION_REASON=="Layoff") %>% select(AGE, LENGTH_OF_SERVICE)
cor_matrix <- round(cor(age_los), 1)
cor_matrix
ggcorrplot(cor_matrix) +
  labs(title="Correlation between Age and Length of Service for Laid Off Employees")

data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, AGE, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=AGE, y=LENGTH_OF_SERVICE, colour=factor(RECORD_YEAR))) + 
  geom_point() + 
  labs(colour="YEAR", x="Age", y="Length of Service", title="Relationship between Age and Length of Service for Laid Off Employees") +
  scale_x_continuous(breaks=unique(data$AGE)) +
  stat_smooth(method=lm)

# Analysis 6.3.1.3 - Relationship between Length of Service and Laid Off Employees -> Most: 2, 7, 25
ggplotly(data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=LENGTH_OF_SERVICE)) +
  geom_area(aes(y = ..count..), stat ="bin", binwidth=1, fill="tomato") +
  labs(colour="YEAR", x="Length of Service", y="Number of Laid Off Employees", title="Relationship between Length of Service and Laid Off Employees") +
  scale_x_continuous(breaks=unique(data$LENGTH_OF_SERVICE)))

# Analysis 6.3.1.4 - Relationship between Length of Service and Laid Off Employees by Year -> Young (2014), Senior (2015)
ggplotly(data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=LENGTH_OF_SERVICE, colour=factor(RECORD_YEAR))) +
  geom_freqpoly(size=1, binwidth=1) +
  labs(colour="YEAR", x="Length of Service", y="Number of Laid Off Employees", title="Relationship between Length of Service and Laid Off Employees by Year") +
  scale_x_continuous(breaks=unique(data$LENGTH_OF_SERVICE)))


# Analysis 6.3.2 - Relationship between City and Laid Off Employees -> Most: Fort Nelson (2014), White Rock (2015)
data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, CITY) %>%
  ggplot(aes(x=CITY, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="City", y="Number of Laid Off Employees", title="Relationship between City and Laid Off Employees by Year") +
  coord_flip()

# Analysis 6.3.2.1 - Stores in Vancouver and Victoria that have Laid Off Employees -> None in S35
data %>% filter(TERMINATION_REASON == "Layoff", (CITY=="Vancouver" | CITY=="Victoria")) %>%
  select(STORE, CITY) %>%
  ggplot(aes(x=CITY, fill=STORE)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="STORE", x="City", y="Number of Laid Off Employees", title="Stores in Vancouver and Victoria that has Laid Off Employees") + 
  scale_fill_manual(values = c("lawngreen","turquoise1"))

# Analysis 6.3.2.2 - Relationship between Store and Laid Off Employees -> Most: Fort Nelson - S11 (2014), White Rock - S39 (2015)
data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, STORE) %>%
  ggplot(aes(x=STORE, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Store", y="Number of Laid Off Employees", title="Relationship between Store and Laid Off Employees by Year")


# Analysis 6.3.3 - Relationship between Department and Laid Off Employees -> Most: Customer Service
data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, DEPARTMENT) %>%
  ggplot(aes(x=DEPARTMENT, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Department", y="Number of Laid Off Employees", title="Relationship between Department and Laid Off Employees by Year")


# Analysis 6.3.4 - Relationship between Job and Laid Off Employees -> Most: Cashier
data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, JOB_TITLE) %>%
  ggplot(aes(x=JOB_TITLE, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Job", y="Number of Laid Off Employees", title="Relationship between Job and Laid Off Employees by Year") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))


# Analysis 6.3.5 - Relationship between Gender and Laid Off Employees -> Most: Female
data %>% filter(TERMINATION_REASON == "Layoff") %>%
  select(RECORD_YEAR, GENDER) %>%
  ggplot(aes(x=GENDER, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Gender", y="Number of Laid Off Employees", title="Relationship between Gender and Laid Off Employees by Year")


# Analysis 6.3.6 - Determining the Layoff Criteria of Employees by comparing with the current attributes of Active Employees
data_full %>% select(RECORD_YEAR, STATUS, TERMINATION_REASON) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015)) %>%
  ggplot(aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Employees", title="Relationship between Active Employees and Terminated Employees") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR))

# Analysis 6.3.6.1 - Age of both Active and Terminated Employees -> Fire younger ages
age_at <- data_full %>% 
  select(RECORD_YEAR, TERMINATION_REASON, AGE) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015))
ggplotly(ggplot(age_at, aes(x=AGE, fill=TERMINATION_REASON)) + 
  geom_density(alpha=.5) +
  ggtitle("Density of the Age of both Active and Terminated Employees"))

# Analysis 6.3.6.2 - Length of service of both Active and Terminated Employees -> Fire unexperienced
los_at <- data_full %>% 
  select(RECORD_YEAR, TERMINATION_REASON, LENGTH_OF_SERVICE) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015))
ggplotly(ggplot(los_at, aes(x=LENGTH_OF_SERVICE, fill=TERMINATION_REASON)) + 
  geom_density(alpha=.5) +
  ggtitle("Density of the Length of Service of both Active and Terminated Employees"))

# Analysis 6.3.6.3 - Cities, Stores and Business Units of both Active and Terminated employees -> No
city_at <- data_full %>% 
  select(RECORD_YEAR, CITY, TERMINATION_REASON) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015))
ggplotly(ggplot(city_at, aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Employees", title="Cities of both Active and Terminated Employees") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) +
  facet_wrap(. ~ CITY))

# Analysis 6.3.6.4 - Departments of both Active and Terminated Employees -> Only S Departments
dept_at <- data_full %>% 
  select(RECORD_YEAR, DEPARTMENT, TERMINATION_REASON) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015))
ggplotly(ggplot(dept_at, aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Employees", title="Departments of both Active and Terminated Employees") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) +
  facet_wrap(. ~ DEPARTMENT))

# Analysis 6.3.6.5 - Jobs of both Active and Terminated Employees -> Only S Jobs
job_at <- data_full %>% 
  select(RECORD_YEAR, JOB_TITLE, TERMINATION_REASON) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015))
ggplotly(ggplot(job_at, aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Employees", title="Jobs of both Active and Terminated Employees") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) +
  facet_wrap(. ~ JOB_TITLE))

# Analysis 6.3.6.6 - Gender of both Active and Terminated Employees -> No
gender_at <- data_full %>% 
  select(RECORD_YEAR, GENDER, TERMINATION_REASON) %>%
  filter(TERMINATION_REASON=="Layoff" | TERMINATION_REASON=="Not Applicable" & (RECORD_YEAR==2014 | RECORD_YEAR==2015))
ggplotly(ggplot(gender_at, aes(x=RECORD_YEAR, fill=TERMINATION_REASON)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="Year", y="Number of Employees", title="Gender of both Active and Terminated Employees") +
  scale_x_continuous(breaks=unique(data$RECORD_YEAR)) +
  facet_wrap(. ~ GENDER))

# == CRITERIA CONCLUSION == 
# Age -> YES
## Fire young
## Short years of experience
## Millennials - Lack vision, Miscommunicate, Overconfident, Arrogant
## Boomers - Low efficiency, Company downsize, Cannot adapt, Not digital literate

# Length of Service -> YES
## Short length of service
## Long length of service - Refer "Age-Boomers"

# City -> NO

# Store -> NO

# Business Unit -> YES
## Fire S
## No S35 - Decision making (Important)

# Department -> YES
## Customer Service - Low requirements, Turnover rate high
## No S35

# Job -> YES
## Cashier - Refer "Department-Customer Service"
## No S35

# Gender -> NO

# -----------------------------------------------------------------------------------

# 6.4 - Question 4: What is the Nature of Resignation?
# Analysis 6.4.1 - Relationship between Age and Resigned Employees -> Most: Age 30, 21 
ggplotly(data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, AGE) %>%
  ggplot(aes(x=AGE)) +
  geom_area(aes(y = ..count..), stat ="bin", binwidth=1, fill="chartreuse") +
  labs(colour="YEAR", x="Age", y="Number of Resigned Employees", title="Relationship between Age and Resigned Employees") +
  scale_x_continuous(breaks=unique(data$AGE)))

# Analysis 6.4.1.1 - Relationship between Age and Resigned Employees by year -> Most: 21 (2014), 30 (2011-2013, 2015)
ggplotly(data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, AGE) %>%
  ggplot(aes(x=AGE, colour=factor(RECORD_YEAR))) +
  geom_freqpoly(size=1, binwidth=1) +
  labs(colour="YEAR", x="Age", y="Number of Resigned Employees", title="Relationship between Age and Resigned Employees by Year") +
  scale_x_continuous(breaks=unique(data$AGE)))

# Analysis 6.4.1.2 - Correlation between Age and Length of Service for Resigned Employees
age_los <- data %>% filter(TERMINATION_REASON=="Resignation") %>% select(AGE, LENGTH_OF_SERVICE)
cor_matrix <- round(cor(age_los), 1)
cor_matrix
ggcorrplot(cor_matrix) +
  labs(title="Correlation between Age and Length of Service for Resigned Employees")

data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, AGE, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=AGE, y=LENGTH_OF_SERVICE, colour=factor(RECORD_YEAR))) + 
  geom_point() + 
  labs(colour="YEAR", x="Age", y="Length of Service", title="Relationship between Age and Length of Service for Resigned Employees") +
  scale_x_continuous(breaks=unique(data$AGE)) +
  stat_smooth(method=lm)

# Analysis 6.4.1.3 - Relationship between Length of Service and Resigned Employees -> Most: 1, 5 (Young <5, 30Y/O >= 5)
ggplotly(data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=LENGTH_OF_SERVICE)) +
  geom_area(aes(y = ..count..), stat ="bin", binwidth=1, fill="chartreuse") +
  labs(colour="YEAR", x="Length of Service", y="Number of Resigned Employees", title="Relationship between Length of Service and Resigned Employees") +
  scale_x_continuous(breaks=unique(data$LENGTH_OF_SERVICE)))

# Analysis 6.4.1.4 - Relationship between Length of Service and Resigned Employees by year -> Young (2011, 2012, 2014), 30Y/O (2011-2013, 2015)
ggplotly(data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=LENGTH_OF_SERVICE, colour=factor(RECORD_YEAR))) +
  geom_freqpoly(size=1, binwidth=1) +
  labs(colour="YEAR", x="Length of Service", y="Number of Resigned Employees", title="Relationship between Length of Service and Resigned Employees by Year") +
  scale_x_continuous(breaks=unique(data$LENGTH_OF_SERVICE)))

# == AGE & LENGTH_OF_SERVICE CONCLUSION == 
# Age 30, 21
## Young <5, 30Y/O >= 5
## Millennials - Job Hopping, Least Engaged Generation, Salary, No reason to stay
## Age 30 - Marriage


# Analysis 6.4.2 - Relationship between City and Resigned Employees -> Most: Vancouver, Victoria
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(CITY) %>%
  ggplot(aes(x=CITY, fill=CITY)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="City", y="Number of Resigned Employees", title="Relationship between City and Resigned Employees") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))
## n.dodge = 2 -> Overcome the over-plotting problem by dodging the labels vertically

# Analysis 6.4.2.1 - Relationship between City and Resigned Employees by Year -> Most: 2012
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, CITY) %>%
  ggplot(aes(x=CITY, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="City", y="Number of Resigned Employees", title="Relationship between City and Resigned Employees by Year") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))

# Analysis 6.4.2.2 - Stores in Vancouver and Victoria that has Resigned Employees -> Most: S44 (Vancouver), S46 (Victoria)
data %>% filter(TERMINATION_REASON == "Resignation", (CITY=="Vancouver" | CITY=="Victoria")) %>%
  select(STORE, CITY) %>%
  ggplot(aes(x=CITY, fill=STORE)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="STORE", x="City", y="Number of Resigned Employees", title="Stores in Vancouver and Victoria that has Resigned Employees")

# Analysis 6.4.2.3 - Relationship between Store and Resigned Employees -> Most: 2012 (S46-Victoria), 2014 (S44-Vancouver) 
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, STORE) %>%
  ggplot(aes(x=STORE, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Store", y="Number of Resigned Employees", title="Relationship between Store and Resigned Employees by Year")

# == CITY & STORE CONCLUSION == 
# 2012 (S46-Victoria), 2014 (S44-Vancouver)
# S35 - 1 resign, S44 - 28 resign
## Company focusing on the wellbeing in the Head Office, other S neglected


# Analysis 6.4.3 - Relationship between Department and Resigned Employees -> Most: Customer Service
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, DEPARTMENT) %>%
  ggplot(aes(x=DEPARTMENT, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Department", y="Number of Resigned Employees", title="Relationship between Department and Resigned Employees by Year")


# Analysis 6.4.4 - Relationship between Job and Resigned Employees -> Most: Cashier
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, JOB_TITLE) %>%
  ggplot(aes(x=JOB_TITLE, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Job", y="Number of Resigned Employees", title="Relationship between Job and Resigned Employees by Year")

# == DEPARTMENT & JOB CONCLUSION == 
# Customer Service
# Cashier
## Stress, Job hopping, Boring job


# Analysis 6.4.5 - Relationship between Gender and Resigned Employees -> Most: Female (+2015)
data %>% filter(TERMINATION_REASON == "Resignation") %>%
  select(RECORD_YEAR, GENDER) %>%
  ggplot(aes(x=GENDER, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Gender", y="Number of Resigned Employees", title="Relationship between Gender and Resigned Employees by Year")

# -----------------------------------------------------------------------------------

# 6.5 - Question 5: What is the Nature of Retirement?
# Analysis 6.5.1 - Relationship between Age and Retired Employees -> Most: 60, 65
ggplotly(data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, AGE) %>%
  ggplot(aes(x=AGE)) +
  geom_area(aes(y = ..count..), stat ="bin", binwidth=1, fill="dodgerblue") +
  labs(colour="YEAR", x="Age", y="Number of Retired Employees", title="Relationship between Age and Retired Employees") +
  scale_x_continuous(breaks=unique(data$AGE)))

# Analysis 6.5.1.1 - Relationship between Age and Retired Employees by year -> Most: AGE 65 (2007), AGE 60 (2006)
ggplotly(data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, AGE) %>%
  ggplot(aes(x=AGE, colour=factor(RECORD_YEAR))) +
  geom_freqpoly(size=1, binwidth=1) +
  labs(colour="YEAR", x="Age", y="Number of Retired Employees", title="Relationship between Age and Retired Employees by Year") +
  scale_x_continuous(breaks=unique(data$AGE)))

# Analysis 6.5.1.2 - Correlation between Age and Length of Service for Retired Employees
age_los <- data %>% filter(TERMINATION_REASON=="Retirement") %>% select(AGE, LENGTH_OF_SERVICE)
cor_matrix <- round(cor(age_los), 1)
cor_matrix
ggcorrplot(cor_matrix) +
  labs(title="Correlation between Age and Length of Service for Retired Employees")

ggplotly(data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, AGE, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=AGE, y=LENGTH_OF_SERVICE, colour=factor(RECORD_YEAR))) + 
  geom_point() + 
  labs(colour="YEAR", x="Age", y="Length of Service", title="Relationship between Age and Length of Service for Retired Employees") +
  scale_x_continuous(breaks=unique(data$AGE)) +
  stat_smooth(method=lm))


# Analysis 6.5.2 - Relationship between Length of Service and Retired Employees -> Most: 13
ggplotly(data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, LENGTH_OF_SERVICE) %>%
  ggplot(aes(x=LENGTH_OF_SERVICE)) +
  geom_area(aes(y = ..count..), stat ="bin", binwidth=1, fill="chartreuse") +
  labs(colour="YEAR", x="Length of Service", y="Number of Retired Employees", title="Relationship between Length of Service and Retired Employees") +
  scale_x_continuous(breaks=unique(data$LENGTH_OF_SERVICE)))

# == AGE CONCLUSION == 
# Age 60, 65
## 65 - Average retirement age


# Analysis 6.5.3 - Relationship between City and Retired Employees -> Most: Vancouver, Victoria
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(CITY) %>%
  ggplot(aes(x=CITY, fill=CITY)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(x="City", y="Number of Retired Employees", title="Relationship between City and Retired Employees") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))
## n.dodge = 2 -> Overcome the over-plotting problem by dodging the labels vertically

# Analysis 6.5.3.1 - Relationship between City and Retired Employees by Year -> Most: Vancouver (2009), Victoria (2007-2009)
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, CITY) %>%
  ggplot(aes(x=CITY, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="City", y="Number of Retired Employees", title="Relationship between City and Retired Employees by Year") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))

# Analysis 6.5.3.2 - Stores in Vancouver and Victoria that has Retired Employees -> Most: S35 (Vancouver), S37 (Victoria)
data %>% filter(TERMINATION_REASON == "Retirement", (CITY=="Vancouver" | CITY=="Victoria")) %>%
  select(STORE, CITY) %>%
  ggplot(aes(x=CITY, fill=STORE)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="STORE", x="City", y="Number of Retired Employees", title="Stores in Vancouver and Victoria that has Retired Employees")

# Analysis 6.5.3.3 - Relationship between Store and Retired Employees -> Most: 2009 (S35-Vancouver), 2007 & 2008 (S37-Victoria)
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, STORE) %>%
  ggplot(aes(x=STORE, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Store", y="Number of Retired Employees", title="Relationship between Store and Retired Employees by Year")

# == STORE CONCLUSION == 
# S35 (HO)
## Sufficient pay, Happy


# Analysis 6.5.4 - Relationship between Department and Retired Employees -> Most: Meats, Produce
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, DEPARTMENT) %>%
  ggplot(aes(x=DEPARTMENT, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Department", y="Number of Retired Employees", title="Relationship between Department and Retired Employees by Year") +
  scale_x_discrete(guide=guide_axis(n.dodge = 2))


# Analysis 6.5.5 - Relationship between Job and Retired Employees -> Most: Meat Cutter, Produce Clerk
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, JOB_TITLE) %>%
  ggplot(aes(x=JOB_TITLE, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Job", y="Number of Retired Employees", title="Relationship between Job and Retired Employees by Year") +
  theme(axis.text.x=element_text(angle=-45, hjust=0))

# == JOB CONCLUSION == 
# Meat Cutter, Produce Clerk
## Sufficient pay, Happy


# Analysis 6.5.6 - Relationship between Gender and Retired Employees -> Most: Female (+ 2012-2015)
data %>% filter(TERMINATION_REASON == "Retirement") %>%
  select(RECORD_YEAR, GENDER) %>%
  ggplot(aes(x=GENDER, fill=factor(RECORD_YEAR))) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat="count", position=position_stack(vjust = 0.5)) +
  labs(fill="YEAR", x="Gender", y="Number of Retired Employees", title="Relationship between Gender and Retired Employees by Year")

# ================================================================

# 7.0 EXTRA FEATURES

# 7.1 - list.functions.in.file()
# 7.2 - get_dupes()
# 7.3 - colSums()
# 7.4 - clean_names()
# 7.5 - glimpse()
# 7.6, 7.7, 7.8 - tabyl(), map_df(), adorn_totals()
# 7.9, 7.10, 7.11, 7.12 - magma(), viridis(), heat.colors(), cm.colors()
# 7.13 - ggplotly()
# 7.14 - geom_density()
# 7.15 - grepl()
# 7.16 - scale_x_continuous()
# 7.17 - cor()
# 7.18 - ggcorrplot()
# 7.19 - scale_fill_manual()
# 7.20 - geom_area()
# 7.21 - coord_flip()
# 7.22 - scale_x_discrete()

# ================================================================

# CONCLUSION
# Number of questions: 8
# Number of analysis: 58
# Number of extra features: 22
# Types of charts used: 13
#   Pie Chart
#   Line Graph
#   Path Graph
#   Point Graph
#   Bar Chart
#   Histogram
#   Frequency Polygon
#   Scatter Plot
#   Count Plot
#   Box Plot
#   Density Graph
#   Area Graph
#   Correlation matrix