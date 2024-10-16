##############################################################################
#               Script 05 - estimate regression for Inequality classification
#                           Irena Zimovska 
#                             16.07.2024
#############################################################################

# --------------------------------------------------------------------------
ess9_ineq <- ess9 %>% 
  filter(citizen != 0) %>%
  filter(ESS9_party_name != 'Obedineni patrioti - NFSB, Ataka i VMRO') %>% 
  filter(ESS9_party_name != '') %>% 
  filter(!is.na(ineq) & ineq != '')

y_vars <- c('ineq', 
            'rand_1_ineq0.1', 
            'rand_1_ineq0.05', 
            'rand_0_ineq0.1', 
            "rand_0_ineq0.05"
)

# ---------------------------------------------------------------------------
# REGRESSSIONS AND RESULTS OF RANDOMIZATION
# ----------------------------------------------------------------------------
# Regressions with countries - Fixed Effects 
# ----------------------------------------------------------------------------


for (y_var in y_vars){ 
  regression_list <- list()
  
  # Loop through each trust variable
  for (trust_var in trust_vars) {
    formula <- paste(y_var, " ~ inc_unfair + top_inc_unfair + bttm_inc_unfair + 
                     female + age + income + union + unemployed + 
                     religion + urban + sat_democracy +", trust_var, 
                     "+ cntry"
    )
    
    # Print the formula for verification (optional)
    cat("Formula for", trust_var, ":", formula, "\n")
    
    # Create the regression model
    regression_name <- paste(y_var, "logit_", gsub("\\.", "_", trust_var), sep = "")
    regression_list[[regression_name]] <- glm(formula,
                                              data = ess9_ineq,
                                              family = binomial(link = "logit"))
  }
  
  r_1 <- robustify(regression_list[[1]])
  r_2 <- robustify(regression_list[[2]])
  r_3 <- robustify(regression_list[[3]])
  r_4 <- robustify(regression_list[[4]])
  r_5 <- robustify(regression_list[[5]])
  r_6 <- robustify(regression_list[[6]])
  r_7 <- robustify(regression_list[[7]])
  
  stargazer(r_1, r_2, r_3, r_4, r_5, r_6, r_7, type = "html", out = paste0(y_var, '_trust_cntry.html'))
  
}


for (y_var in y_vars){ 
  regression_list <- list()
  
  # Loop through each trust variable
  for (trust_var in trust_vars) {
    formula <- paste(y_var, " ~ inc_unfair + top_inc_unfair + bttm_inc_unfair + 
                     female + age + income + union + unemployed + 
                     religion + urban + sat_democracy +", trust_var, 
                     "+ year"
    )
    
    # Print the formula for verification (optional)
    cat("Formula for", trust_var, ":", formula, "\n")
    
    # Create the regression model
    regression_name <- paste(y_var, "logit_", gsub("\\.", "_", trust_var), sep = "")
    regression_list[[regression_name]] <- glm(formula,
                                              data = ess9_ineq,
                                              family = binomial(link = "logit"))
  }
  
  r_1 <- robustify(regression_list[[1]])
  r_2 <- robustify(regression_list[[2]])
  r_3 <- robustify(regression_list[[3]])
  r_4 <- robustify(regression_list[[4]])
  r_5 <- robustify(regression_list[[5]])
  r_6 <- robustify(regression_list[[6]])
  r_7 <- robustify(regression_list[[7]])
  
  stargazer(r_1, r_2, r_3, r_4, r_5, r_6, r_7, type = "html", out = paste0(y_var, '_trust_year.html'))
  
}

# ----------------------------------------------------------------------------
# Regressions without countries 
# ----------------------------------------------------------------------------

for (y_var in y_vars){ 
  regression_list <- list()
  
  # Loop through each trust variable
  for (trust_var in trust_vars) {
    formula <- paste(y_var, " ~ inc_unfair + top_inc_unfair + bttm_inc_unfair + 
                     female + age + income + union + unemployed + 
                     religion + urban + sat_democracy +", trust_var
    )
    
    # Print the formula for verification (optional)
    cat("Formula for", trust_var, ":", formula, "\n")
    
    # Create the regression model
    regression_name <- paste(y_var, "logit_", gsub("\\.", "_", trust_var), sep = "")
    regression_list[[regression_name]] <- glm(formula,
                                              data = ess9_ineq,
                                              family = binomial(link = "logit"))
  }
  
  r_1 <- robustify(regression_list[[1]])
  r_2 <- robustify(regression_list[[2]])
  r_3 <- robustify(regression_list[[3]])
  r_4 <- robustify(regression_list[[4]])
  r_5 <- robustify(regression_list[[5]])
  r_6 <- robustify(regression_list[[6]])
  r_7 <- robustify(regression_list[[7]])
  
  stargazer(r_1, r_2, r_3, r_4, r_5, r_6, r_7, type = "html", out = paste0(y_var, '_trust.html'))
  
}
