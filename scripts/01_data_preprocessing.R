##############################################################################
#               Script 01 - preprocess ESS9 variables
#                           Irena Zimovska 
#                             16.07.2024
#############################################################################

# 1. Transform inspected variables 
# ------------------------------------------------------------------------------
# Here the transformations appear that we need for variables included in the regressions
# gender
# level of education 
# age 
# so called "trust" variables
# income 
# and so on


# -----------------------------------------------------------------------------
# education level transformation

ess9 %>% 
  select(eisced) %>% 
  filter(eisced == "") %>% # checks how many "" empty rows are
  count()

ess9 %>%
  select(eisced) %>%
  filter(is.na(eisced)) %>% # checks how many NA rows are
  count()

# Impute "" values with NA
ess9 <- ess9 %>%
  mutate(eisced = na_if(eisced, ""))

# transform var into factor 
ess9$eisced <- as.factor(ess9$eisced)
ess9 <- ess9 %>%
  mutate(eisced = fct_collapse(eisced,
                               "Lower secondary or less" = c("ES-ISCED I , less than lower secondary", 
                                                             "ES-ISCED II, lower secondary"),
                               "Upper secondary education" = c("ES-ISCED IIIa, upper tier upper secondary", 
                                                               "ES-ISCED IIIb, lower tier upper secondary"),
                               "Advanced vocational, sub-degree" = "ES-ISCED IV, advanced vocational, sub-degree",
                               "Tertiary education" = c("ES-ISCED V1, lower tertiary education, BA level", 
                                                        "ES-ISCED V2, higher tertiary education, >= MA level"),
                               "Other" = "Other"
  ))

summary(ess9$eisced)

# -----------------------------------------------------------------------------
# age transformation

# impution with median NA for age 
summary(ess9$agea)
ess9$agea[is.na(ess9$agea)] <- median(ess9$agea, na.rm = TRUE)

ess9 <- ess9 %>% mutate(age_group = cut(agea,
                               breaks = c(15, 24, 34, 44, 54, 64, 74, 90), 
                               labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-90"),
                               include.lowest = TRUE))
ess9$age_group <- as.factor(ess9$age_group)
# -----------------------------------------------------------------------------
# Gender 
ess9$gndr <- as.factor(ess9$gndr)
summary(ess9$gndr) 

# -----------------------------------------------------------------------------
# Income & trust variables
ess9 <- ess9 %>% mutate(inc_unfair = recode(netifr, 
                                            "Low, extremely unfair" = 4,
                                            "Low, very unfair" = 3,
                                            "Low, somewhat unfair" = 2,
                                            "Low, slightly unfair" = 1,
                                            "Fair" = 0,	
                                            "High, slightly unfair" = -1,	
                                            "High, somewhat unfair" = -2,
                                            "High, very unfair" = -3,
                                            "High, extremely unfair" = -4),
                        d_inc_unfair = ifelse(inc_unfair > 0, 1, 0),
                        top_inc_unfair = recode(topinfr, 
                                                "Low, extremely unfair" = -4,
                                                "Low, very unfair" = -3,
                                                "Low, somewhat unfair" = -2,
                                                "Low, slightly unfair" = -1,
                                                "Fair" = 0,	
                                                "High, slightly unfair" = 1,
                                                "High, somewhat unfair" = 2,
                                                "High, very unfair" = 3,
                                                "High, extremely unfair" = 4),
                        d_top_inc_unfair = ifelse(top_inc_unfair > 0, 1, 0),
                        bttm_inc_unfair = recode(btminfr, 
                                                 "Low, extremely unfair" = 4,
                                                 "Low, very unfair" = 3,
                                                 "Low, somewhat unfair" = 2,
                                                 "Low, slightly unfair" = 1,
                                                 "Fair" = 0,	
                                                 "High, slightly unfair" = -1,	
                                                 "High, somewhat unfair" = -2,
                                                 "High, very unfair" = -3,
                                                 "High, extremely unfair" = -4),
                        d_bttm_inc_unfair = ifelse(bttm_inc_unfair > 0, 1, 0),
                        job_self_fair = as.numeric(recode(as.character(ifrjob),
                                                          "Does not apply at all" = "0", 
                                                          "Applies completely" = "10")),
                        job_self_unfair = -(job_self_fair - 10),
                        d_job_self_unfair = ifelse(job_self_unfair>5, 1, 0),
                        job_other_fair = as.numeric(recode(as.character(evfrjob),
                                                           "Does not apply at all" = "0", 
                                                           "Applies completely" = "10")),
                        job_other_unfair = -(job_other_fair - 10),
                        d_job_other_unfair = ifelse(job_other_unfair>5, 1, 0)
) 


ess9 <- ess9 %>% mutate(trust_prl = recode(trstprl,
                                           "No trust at all" = 0,
                                           "1" = 1,
                                           "2" = 2,
                                           "3" = 3,
                                           "4" = 4,
                                           "5" = 5,
                                           "6" = 6,
                                           "7" = 7,
                                           "8" = 8,
                                           "9" = 9,
                                           "Complete trust" = 10),
                        trust_lgl = recode(trstlgl,
                                           "No trust at all" = 0,
                                           "1" = 1,
                                           "2" = 2,
                                           "3" = 3,
                                           "4" = 4,
                                           "5" = 5,
                                           "6" = 6,
                                           "7" = 7,
                                           "8" = 8,
                                           "9" = 9,
                                           "Complete trust" = 10),
                        trust_plc = recode(trstplc,
                                           "No trust at all" = 0,
                                           "1" = 1,
                                           "2" = 2,
                                           "3" = 3,
                                           "4" = 4,
                                           "5" = 5,
                                           "6" = 6,
                                           "7" = 7,
                                           "8" = 8,
                                           "9" = 9,
                                           "Complete trust" = 10),
                        trust_plt = recode(trstplt,
                                           "No trust at all" = 0,
                                           "1" = 1,
                                           "2" = 2,
                                           "3" = 3,
                                           "4" = 4,
                                           "5" = 5,
                                           "6" = 6,
                                           "7" = 7,
                                           "8" = 8,
                                           "9" = 9,
                                           "Complete trust" = 10),
                        trust_prt = recode(trstprt,
                                           "No trust at all" = 0,
                                           "1" = 1,
                                           "2" = 2,
                                           "3" = 3,
                                           "4" = 4,
                                           "5" = 5,
                                           "6" = 6,
                                           "7" = 7,
                                           "8" = 8,
                                           "9" = 9,
                                           "Complete trust" = 10),
                        trust_ep = recode(trstep,
                                          "No trust at all" = 0,
                                          "1" = 1,
                                          "2" = 2,
                                          "3" = 3,
                                          "4" = 4,
                                          "5" = 5,
                                          "6" = 6,
                                          "7" = 7,
                                          "8" = 8,
                                          "9" = 9,
                                          "Complete trust" = 10),
                        trust_un = recode(trstun,
                                          "No trust at all" = 0,
                                          "1" = 1,
                                          "2" = 2,
                                          "3" = 3,
                                          "4" = 4,
                                          "5" = 5,
                                          "6" = 6,
                                          "7" = 7,
                                          "8" = 8,
                                          "9" = 9,
                                          "Complete trust" = 10),
                        trust_pols = (trust_plt + trust_prl)/2,
                        sat_democracy = recode(stfdem,
                                               "Extremely dissatisfied" = 0,
                                               "1" = 1,
                                               "2" = 2,
                                               "3" = 3,
                                               "4" = 4,
                                               "5" = 5,
                                               "6" = 6,
                                               "7" = 7,
                                               "8" = 8,
                                               "9" = 9,
                                               "Extremely satisfied" = 10)
)




# income, female, age, union, unemployed, citizen, religiosity, urban, turnout
ess9 <- ess9 %>% mutate(female = ifelse(gndr=="Female", 1, 0),
                        age = as.numeric(as.character(agea)),
                        region = as.character(region),
                        income = recode(hinctnta, 
                                        "J - 1st decile" = 1L,
                                        "R - 2nd decile" = 2L, 
                                        "C - 3rd decile" = 3L, 
                                        "M - 4th decile" = 4L,
                                        "F - 5th decile" = 5L,
                                        "S - 6th decile" = 6L,
                                        "K - 7th decile" = 7L,
                                        "P - 8th decile" = 8L,
                                        "D - 9th decile" = 9L,
                                        "H - 10th decile" = 10L),
                        union = ifelse(mbtru %in% c("Yes, currently"), 1, 0),
                        citizen = ifelse( ctzcntr == "Yes", 1, 0),
                        religion = as.numeric(recode(as.character(rlgdgr),
                                                     "Not at all religious" = "0", 
                                                     "Very religious" = "10")), 
                        urban = ifelse(domicil %in% c("A big city",
                                                      "Suburbs or outskirts of big city"), 1, 0),
                        unemployed = ifelse(mnactic %in% c("Unemployed, looking for job",
                                                           "Unemployed, not looking for job"), 1, 0),
                        voted = recode(vote, "Yes" = 1 , "No" = 0))

# -------------------------------------------------------------------------
# II. Data exploration - verifications, WOE, summary tables

# -------------------------------------------------------------------------
# Empirical distribution of RFPOPI 
# -------------------------------------------------------------------------

library(gridExtra)

# Plot for populism_ide
plot1 <- ggplot(ess9, aes(x = populism_ide)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, fill = "black", color = "black", alpha = 0.4) +  # Histogram
  geom_density(color = "blue", fill = "blue", alpha = 0.3) +  # Density plot
  scale_x_continuous(breaks = seq(0, 10, by = 0.5)) +  # Set x-axis ticks every 0.5 units
  labs(title = "Empirical Distribution of RFPOPI - ideology", x = "Populist ideology", y = "Density") +
  theme_minimal()

# Plot for populism_rhet
plot2 <- ggplot(ess9, aes(x = populism_rhet)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, fill = "black", color = "black", alpha = 0.4) +  # Histogram
  geom_density(color = "red", fill = "red", alpha = 0.3) +  # Density plot
  scale_x_continuous(breaks = seq(0, 10, by = 0.5)) +  # Set x-axis ticks every 0.5 units
  labs(title = "Empirical Distribution of RFPOPI - rhetorics", x = "Populist rhetorics", y = "Density") +
  theme_minimal()
 
# Arrange the plots in one row
grid.arrange(plot1, plot2, nrow = 2)

# -------------------------------------------------------------------------
# Who votes for populists having high level of trust in politicians/parliament?
# -------------------------------------------------------------------------

library(table1)

table1::label(ess9$eisced) <- "Education level"
table1::label(ess9$age_group) <- "Age"
table1::label(ess9$gndr) <- "Gender"

# Respondents sample analysis
high_trust_ctzns <- ess9 %>% filter(
  schafer == 1 & trust_plt >= 7 & trust_prl >= 7
)

# Make summary stats tables
t1 <- table1::table1(~gndr + age_group + eisced + income + unemployed + religion | schafer, data = ess9 %>% 
                                     filter(!is.na(schafer) &
  schafer == 1 & trust_plt >= 7 & trust_prl >= 7
), overall = NULL)

t2 <- table1::table1(~gndr + age_group + eisced + income + unemployed + religion | ineq, data = ess9 %>% 
                                     filter(!is.na(ineq) &
  ineq == 1 & trust_plt >= 7 & trust_prl >= 7
), overall = NULL)

t3 <- table1::table1(~gndr + age_group + eisced + income + unemployed + religion | populism, data = ess9 %>% 
                                     filter(!is.na(populism) &
  populism == 1 & trust_plt >= 7 & trust_prl >= 7
), overall = NULL)

t4 <- table1::table1(~gndr + age_group + eisced + income + unemployed + religion | rfpopi_ide, data = ess9 %>% filter(
  rfpopi_ide == 1 & trust_plt >= 7 & trust_prl >= 7
), overall = NULL)

t5 <- table1::table1(~gndr + age_group + eisced + income + unemployed + religion | rfpopi_rhet, data = ess9 %>% filter(
  rfpopi_rhet == 1 & trust_plt >= 7 & trust_prl >= 7
), overall = NULL)

# -------------------------------------------------------------------------
# How trust variables are related to classifications? 
# -------------------------------------------------------------------------

# Function to calculate WOE for each factor level
calculate_woe <- function(data, feature, target) {
  # Create a contingency table
  freq_table <- table(data[[feature]], data[[target]])
  
  # Convert table to data frame
  freq_df <- as.data.frame.matrix(freq_table)
  colnames(freq_df) <- c("Good", "Bad")
  
  # Calculate the total number of Good and Bad
  total_good <- sum(freq_df$Good)
  total_bad <- sum(freq_df$Bad)
  
  # Calculate the proportion of Good and Bad for each factor level
  freq_df$Good_Dist <- freq_df$Good / total_good
  freq_df$Bad_Dist <- freq_df$Bad / total_bad
  
  # Calculate WOE for each factor level
  freq_df$WOE <- log(freq_df$Good_Dist / freq_df$Bad_Dist)
  
  return(freq_df)
}

plot_woe_distribution <- function(trust_var, populism_var) {
  woe_values <- calculate_woe(ess9, trust_var, populism_var)
  
  # Filter out rows where WOE is Inf or -Inf
  woe_values <- woe_values[is.finite(woe_values$WOE), ]
  
  # Create a plot for WOE
  woe_values$Level <- factor(rownames(woe_values), levels = unique(rownames(woe_values)))
  
  plt <- ggplot(woe_values, aes(x = Level, y = WOE, group = 1)) +  
    geom_bar(stat = "identity", fill = "#53a6c5", alpha = 0.7, color = 'gray') +  # Bars
    geom_text(aes(label = round(WOE, 2)),                 # Text labels with WOE values
              vjust = -0.8,                               # Adjust vertical position (above the bar)
              color = "black", size = 3.5) +  
    geom_point(color = "black", size = 2) +               # Scatter points
    geom_line(color = "#3154ce", size = 0.5, linetype = 'dashed') +  # Dashed line
    labs(x = trust_var, y = "WOE") +                      # Axis labels
    ggtitle(paste(trust_var, 'vs', populism_var)) + 
    expand_limits(y = max(woe_values$WOE) * 1.2) +  # Extend y-axis limits
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
          plot.title = element_text(family = "Helvetica",  
                                    size = 14))  # Custom title style
  return(plt)
}

populism_vars <- c('populism', 'schafer', 'ineq', 'populism_ide', 'populism_rhet')
trust_vars <- c("trust_prt", "trust_prl", "trust_plc",
                "trust_plt", "trust_lgl", "trust_un", "trust_ep")

# Loop through each trust variable
for (var_t in trust_vars) {
  plot_list <- list()  # Create an empty list to store plots for each trust variable
  
  for (var in populism_vars) {
    # Generate the plot and append to the list
    plot_list[[var]] <- plot_woe_distribution(var_t, var)
  }
  
  # Arrange the plots in a grid for the current trust variable
  combined_plot <- grid.arrange(grobs = plot_list, ncol = 2, top = paste("WOE distribution for", var_t))
  
  # Save the combined plot to a file with specified dimensions
  ggsave(filename = paste0("woe_distribution_", var_t, ".png"), plot = combined_plot, width = 10, height = 12, dpi = 300)
}









