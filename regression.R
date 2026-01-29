# This R file contains code to run a logistic regression to determine how likely 
# are women to publish economic papers in one of the top 10 institutions. 


# Load libraries
library(readr)
library(dplyr)

# Load the data
df <- read_csv("/Users/celina.madaschi/Desktop/Ecole polytechnique/Data Collection/repec_with_gender.csv")

# Select relevant columns, create new variables
df <- df %>%
  select(
    Title, Journal, Year, Type, author_name, short_id,
    institution, share_pct, location, repec_institution_id, gender
  ) %>%
  filter(!is.na(Year)) %>%
  mutate(
    top10 = ifelse(institution %in% c(
      "Economics Department London School of Economics (LSE)", "Paris School of Economics", "Department of Economics University College London (UCL)", "Barcelona School of Economics (BSE)", "Toulouse School of Economics (TSE)",
      "Centre for Economic Performance (CEP) London School of Economics (LSE)", "Department of Economics Oxford University", "Department of Economics Sciences économiques Sciences Po", "Centre for Economic Policy Research (CEPR)", "Department of Economics University of Warwick"
    ), 1, 0),
    female = ifelse(gender == "female", 1, 0),
    one_affiliation = ifelse(share_pct == "100", 1, 0),
    Year = as.numeric(Year),
    Type = as.factor(Type), 
    recent = ifelse(Year >= 2021, 1, 0)
  )

# Remove duplicate rows 
dup_df <- df %>%
  group_by(across(everything())) %>%
  mutate(dup_count = n()) %>%
  filter(dup_count > 1) %>%
  ungroup()

df_unique <- df %>%
  distinct()


# Estimate the model
model <- glm(
  top10 ~ female + Type + one_affiliation + recent,
  data = df_unique,
  family = binomial(link = "logit")
)

summary(model)
exp(coef(model))


