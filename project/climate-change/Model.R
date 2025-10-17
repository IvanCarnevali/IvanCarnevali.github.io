
# Libraries ####

library(tidyverse)
library(nlme)
library(lattice)
library(glmnet)
library(corrplot)
library(ggplot2)
library(strucchange)
library(curl)
library(sf)
library(eurostat)
library(giscoR)
library(magick)

# Data Manipulation ####

# Read dataset

data<-read_csv("DATA.csv")
data$GDP.in.millions<-1000000*1000*data$GDP.in.millions/data$tot.population
data <- data %>% rename(GDP.pro.capita = GDP.in.millions)

# Dealing with Employment rate
data$employment.rate <- sapply(data$employment.rate, function(x) {
  x <- as.character(x)  
  x <- gsub(",", "", x) 
  n <- nchar(x)
  if (n == 3) {
    paste0(substr(x, 1, 2), ".", substr(x, 3, 3))
  } else if (n == 2) {
    paste0(x, ".0")
  } else {
    NA
  }
})

# Creation of variable zone (north, center, south)

center <- c("ITI1", "ITI2", "ITI3", "ITI4")
north <- c("ITC1", "ITC2", "ITC3", "ITC4", "ITH1", "ITH2", "ITH3",
          "ITH4", "ITH5")
south <- c("ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITG1", "ITG2")

data$zone <- with(data, ifelse(nuts %in% center, "center", 
                        ifelse(nuts %in% south, "south",
                        ifelse(nuts %in% north, "north", NA))))
data <- data %>%
  relocate(zone, .after = region)

# CHOOSE DATASET ####
# Uncomment and run only one of the following for choosing the dataset

## No center

# data <- data[data$zone != "center", ]

## Center and north joined

# center_north<-c(center,north)
# data$zone <- with(data, ifelse(nuts %in% center_north, "center-north",
#                        ifelse(nuts %in% south, "south", NA)))

## north, center, south

# data<-data


# zone as factor
data$zone <- as.factor(data$zone)
data$nuts <- as.factor(data$nuts)
data$region <- as.factor(data$region)
data$number.of.tourists <- as.numeric(gsub("\\.", "", data$number.of.tourists))
data$employment.rate<-as.numeric(data$employment.rate)
## Dealing with NAs ####

sum(is.na(data))/14000 # 6% of observations are missing values

# Implementation of Last Observation Carried Forward (LOCF) method to impute missing values
# for variables

data <- data %>% 
  fill(everything(), .direction = "down")

sum(is.na(data))


## Exploratory analysis and varibles selection ####

###Descriptive graphs ####

nuts2 <- get_eurostat_geospatial(output_class = "sf", resolution = "20", nuts_level = "2") %>%
  filter(CNTR_CODE == "IT") %>%
  rename(nuts = NUTS_ID)

data_gdp <- data[, c("time", "nuts", "region", "zone", "GDP.pro.capita")]

map_gdp <- nuts2 %>%
  left_join(data_gdp, by = "nuts")

map_gdp$zone <- factor(map_gdp$zone, levels = c("north", "center", "south"))

zone_coords <- data.frame(
  zone = c("north", "center", "south"),
  lon = c(9.5, 12.5, 15.5),
  lat = c(45.5, 43, 41))

gdp_zone_time <- data %>%
  group_by(zone, time) %>%
  summarise(mean_gdp = round(mean(GDP.pro.capita, na.rm = TRUE),0), .groups = "drop")

zone_coords_time <- zone_coords %>%
  left_join(gdp_zone_time, by = "zone") %>%
  filter(!is.na(mean_gdp))

temp_region <- data %>%
  group_by(region, time) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = TRUE),
    zone = first(zone),
    nuts = first(nuts),
    .groups = "drop"
  )

map_temp_year <- left_join(nuts2, temp_region, by = "nuts")

p1 <- ggplot(map_gdp) +
  geom_sf(aes(fill = zone), color = "gray40", size = 0.2) +
  scale_fill_manual(values = c(
    "north" = "#84CCBD",
    "center" = "#F9D340",
    "south" = "#E86E78"
  )) +
  geom_text(
    data = zone_coords_time,
    aes(x = lon, y = lat, label = mean_gdp),
    fontface = "bold", size = 4
  ) +
  labs(title = 'GDP per Capita by Zone: {closest_state}', fill = "Zone") +
  transition_states(time, transition_length = 1, state_length = 1) +
  ease_aes('linear') +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

anim1 <- animate(p1, width = 800, height = 600, duration = 20, renderer = gifski_renderer()) 
anim_save("gdp_map.gif", anim1)

p2 <- ggplot(map_temp_year) +
  geom_sf(aes(fill = mean_temp), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Avg Temp (°C)") +
  labs(title = 'Average Temperature: {closest_state}') +
  transition_states(time, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

png_renderer <- file_renderer(
  dir = "frames",        
  prefix = "gdp_frame_")
animate(p1, nframes = 100, fps = 10, renderer = png_renderer)

png_renderer <- file_renderer(
  dir = "frames",        
  prefix = "temp_frame_")
animate(p2, nframes = 100, fps = 10, renderer = png_renderer)


### Check for multicollinearity (only numeric variables) ####
num_vars <- data %>% select(where(is.numeric)) 

short_names <- c(
  "time" = "time",
  "bovine" = "bovine",
  "cows" = "cows",
  "swine" = "swine",
  "sheep" = "sheep",
  "goats" = "goats",
  "cereals_grain" = "cereal",
  "tot.precipitation" = "precip_tot",
  "months.of.droughts" = "drought_mo",
  "total.extreme.precipitation.mm" = "precip_ext",
  "fire.weather.index" = "fire_idx",
  "frost.days" = "frost",
  "heatdays" = "heat",
  "mean.wind.speed" = "wind",
  "temperature" = "temp",
  "deaths" = "deaths",
  "fertility.rate" = "fert_rate",
  "life.expectancy" = "life_exp",
  "Crude.rate.of.net.migration" = "net_mig",
  "Total.population.change" = "pop_change",
  "tot.population" = "pop_tot",
  "GDP.pro.capita" = "gdp_pc",
  "employment.rate" = "empl_rate",
  "Pre.primary.education" = "edu_pre",
  "Primary..lower.and.upper.secondary.education" = "edu_sec",
  "First.and.second.stage.of.tertiary.education" = "edu_ter",
  "NEETS.." = "neets"
)

num_vars_short <- num_vars %>% rename(!!!setNames(names(short_names), short_names))

cor_matrix <- cor(num_vars_short, use = "complete.obs")

corrplot(cor_matrix, 
         method = "color", 
         tl.col = "black",
         type = "upper", 
         tl.cex = 0.8, 
         addCoef.col = NULL, 
         col = colorRampPalette(c("red", "white", "darkblue"))(200),
         mar = c(1,1,1,1),
         addgrid.col = "black")


# Var with high corr
threshold <- 0.7
high_corr_pairs <- which(abs(cor_matrix) > threshold & lower.tri(cor_matrix), arr.ind = TRUE)

results <- data.frame(Var1 = rownames(cor_matrix)[high_corr_pairs[,1]],
  Var2 = colnames(cor_matrix)[high_corr_pairs[,2]],
  Correlation = cor_matrix[high_corr_pairs])

var_high_corr <- results[order(abs(results$Correlation), decreasing = TRUE), ]
print(var_high_corr)

# Because of high correlation we:
# merge Pre primary education with variable up until Upper secondary education
# divide all population variables by the total population in that zone to get population rates:
# - deaths divide 
# - remove total population change as it is already accounted for by rates and migration
# - create education variables and divide
# We choose ignore high correlation between temperature and other climate variables 
# Bovine and cows, we remove cows
# Swine and cows we ignore the high correlation (maybe 1 animal variable?)
# We remove neets
# We remove total precipation as we feel that total extreme is more important
# We merge goats and sheep

# Merge Pre-primary and Upper Secondary
data$low_and_medium_education <- data$Pre.primary.education + 
  data$Primary..lower.and.upper.secondary.education

# Divide deaths by total population
data$death.rate <- data$deaths / data$tot.population

# Remove total population change
data <- data %>% select(-Total.population.change)

# Divide education variables by total population
data$number.of.tourists.rate<-data$number.of.tourists / data$tot.population
data$low_and_medium_education.rate <- data$low_and_medium_education / data$tot.population
data$high_education.rate <- data$First.and.second.stage.of.tertiary.education / data$tot.population
data <- data %>% select(-cows)

data <- data %>% select(-NEETS..)
data <- data %>% select(-tot.precipitation)
data$ovine <- data$goats + data$sheep

# Clean up any variables no longer needed
data <- data %>%
  select(-Pre.primary.education,
         -Primary..lower.and.upper.secondary.education,
         -low_and_medium_education,
         -deaths,
         -First.and.second.stage.of.tertiary.education,
         -goats,
         -sheep,
         -tot.population,
         -number.of.tourists)

data <- data %>%
  relocate(ovine, .after = bovine)
data <- data %>%
  relocate(GDP.pro.capita, .after = zone)

# Study the dependencies between GDP and each variable
numerical_vars <- names(data)[sapply(data, is.numeric)]

# Remove target variable
numerical_vars <- setdiff(numerical_vars, "GDP.pro.capita")

# P values for numerical variables
for (var in numerical_vars) {
  formula <- as.formula(paste("GDP.pro.capita ~", var))
  model <- lm(formula, data = data)
  summary_model <- summary(model)
  
  p_val <- summary_model$coefficients[2, 4]
  coef <- summary_model$coefficients[2, 1]
  r_squared <- summary_model$r.squared
  
  cat(sprintf("%s: p-value = %.4f\n", 
              var, p_val))
}

write.csv(data, "Data_final.csv")

# Model ####

## GDP ####

model <- lm(GDP.pro.capita ~.-nuts, data=data)

summary(model)

## By region ####
data_model_region <- data[,-c(2,4)] #tolto nuts and zone (rimane region)
data_mixed_region <- groupedData(GDP.pro.capita~time|region, data=data_model_region)
plot(data_mixed_region)

lmList_region<-lmList(data_mixed_region)
plot(augPred(lmList_region),layout=c(7,3))
intervals(lmList_region)
plot(intervals(lmList_region))

data_mixed_region <- data_mixed_region %>%
  mutate(across(where(is.numeric) & !any_of("time"), scale))

model_region_rand<-lme(fixed=GDP.pro.capita~time + bovine + ovine + swine +
                         cereals_grain + months.of.droughts + total.extreme.precipitation.mm +
                         fire.weather.index + frost.days + heatdays + mean.wind.speed +
                         temperature + fertility.rate + life.expectancy +
                         Crude.rate.of.net.migration + employment.rate + death.rate + 
                         number.of.tourists.rate + low_and_medium_education.rate +
                         high_education.rate,random=~time|region, 
                            data=data_model_region, method = "ML")
summary(model_region_rand)
ranef(model_region_rand)

## By zone ####
data_model_zone <- data[,-c(2,3)] # tolto nuts and region (rimane zone)
data_mixed_zone <- groupedData(GDP.pro.capita~time|zone, data=data_model_zone)
plot(data_mixed_zone)

lmList_zone<-lmList(data_mixed_zone)
plot(augPred(lmList_zone))
intervals(lmList_zone)
plot(intervals(lmList_zone))

data_mixed_zone <- data_mixed_zone %>%
  mutate(across(where(is.numeric) & !any_of("time"), scale))

model_zone_rand<-lme(fixed=GDP.pro.capita~time + bovine + ovine + swine +
                       cereals_grain + months.of.droughts + total.extreme.precipitation.mm +
                       fire.weather.index + frost.days + heatdays + mean.wind.speed +
                       temperature + fertility.rate + life.expectancy +
                       Crude.rate.of.net.migration + employment.rate + death.rate + 
                       number.of.tourists.rate + low_and_medium_education.rate +
                       high_education.rate,random=~time|zone, data=data_model_zone, method = "ML")
summary(model_zone_rand)

ranef(model_zone_rand)

### testing the random slope ####

mod0zone<-update(model_zone_rand,random=~1|zone)

loglikratio<-anova(mod0zone,model_zone_rand)$L.Ratio[2]
p.value_zone<-0.5*(1-pchisq(loglikratio,0)) + 0.5*(1-pchisq(loglikratio,1))
p.value_zone

# Testing for breakpoints in temperature - Paris Agreement ####

# Compute average temperature per year and zone
zone_temp <- data %>%
  group_by(zone, time) %>%
  summarise(mean_temp = mean(temperature, na.rm = TRUE), .groups = "drop")

# Compute national average temperature per year
italy_temp <- data %>%
  group_by(time) %>%
  summarise(mean_temp = mean(temperature, na.rm = TRUE)) %>%
  mutate(zone = "Italia")

# Combine both datasets
temp_plot_data <- bind_rows(zone_temp, italy_temp)
temp_plot_data$zone <- factor(temp_plot_data$zone,
                              levels = c("center", "north", "south", "Italia"))

# Plot
ggplot(temp_plot_data, aes(x = as.numeric(time),
                           y = mean_temp,
                           color = zone,
                           linetype = zone)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("center" = "#1f77b4",
                                "north" = "darkgreen",
                                "south" = "#d62728",
                                "Italia" = "black")) +
  scale_linetype_manual(values = c("center" = "solid",
                                   "north" = "solid",
                                   "south" = "solid",
                                   "Italia" = "dotdash")) +
  labs(title = "",
       x = "Year",
       y = "Average Temperature (°C)",
       color = "Zone",
       linetype = "Zone") +
  theme_minimal()

# Extract only the Italy time series
italy_ts <- temp_plot_data %>%
  filter(zone == "Italia") %>%
  arrange(time)

# Convert to time series object
temp_ts <- ts(italy_ts$mean_temp, start = min(italy_ts$time), frequency = 1)

# Breakpoint analysis
bp_model <- breakpoints(temp_ts ~ 1)

# Summary of breakpoints
summary(bp_model)

# Plot with breakpoints corresponding to the minimum BIC
plot(temp_ts, type = "l", col = "black", lwd = 2,
     ylab = "Mean temperature", xlab = "Year",
     main = "Breakpoints in national average temperature")
lines(fitted(bp_model), col = "blue", lwd = 2)

# Extract optimal number of breakpoints according to BIC
optimal_breaks <- breakpoints(temp_ts ~ 1, breaks = which.min(BIC(bp_model)))

# Add vertical lines at BIC-optimal breakpoints
abline(v = time(temp_ts)[optimal_breaks$breakpoints], col = "red", lwd = 2, lty = 2)

# Add legend
legend("topleft", legend = c("Observed", "Estimated", "Breakpoint"),
       col = c("black", "blue", "red"), lwd = 2, lty = c(1, 1, 2))
