

# -----------------------------------------------------------------------------
ggplot(rfpopi, aes(x = populism_rhet, y = schafer)) +
  geom_point(color = "red", alpha = 0.5, size = 3) +
  labs(
    title = "RFPOPI classification vs Schafer [POPULISM_RHET]",
    x = "RFPOPI - populism_rhet",
    y = "Schafer"
  ) +
  theme_minimal()


ggplot(rfpopi, aes(x = populism_ide, y = schafer)) +
  geom_point(color = "blue", alpha = 0.5, size = 3) +
  labs(
    title = "RFPOPI classification vs Schafer [POPULISM_IDE]",
    x = "RFPOPI - populism_ide",
    y = "Schafer"
  ) +
  theme_minimal()


ggplot(rfpopi, aes(x = populism_rhet, y = ineq)) +
  geom_point(color = "red", alpha = 0.5, size = 3) +
  labs(
    title = "RFPOPI classification vs Inequality [POPULISM_RHET]",
    x = "RFPOPI - populism_rhet",
    y = "Inequality"
  ) +
  theme_minimal()


ggplot(rfpopi, aes(x = populism_ide, y = ineq)) +
  geom_point(color = "blue", alpha = 0.5, size = 3) +
  labs(
    title = "RFPOPI classification vs Inequality [POPULISM_IDE]",
    x = "RFPOPI - populism_ide",
    y = "Inequality"
  ) +
  theme_minimal()


# -------------------------------------------------------------------------
# Violin Plots 

# Most basic violin chart

ggplot(rfpopi, aes(x = factor(schafer), y = populism_ide, fill = factor(schafer))) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Adding transparency to the violins
  geom_jitter(aes(color = factor(schafer)), width = 0.2, size = 2.5, alpha = 0.6) +  # Jitter points for better visibility
  coord_flip() +  # Flip coordinates
  labs(
    title = "Classifications RFPOPI vs Schafer [POPULISM_IDE]",
    x = "Schafer (0/1)",
    y = "Populism Ideology"
  ) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "#f27598")) +  # Custom fill colors for violin plots
  scale_color_manual(values = c("0" = "#122694", "1" = "#e91651")) +  # Custom colors for jitter points
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "none"  # Hide legend
  )


ggplot(rfpopi, aes(x = factor(schafer), y = populism_rhet, fill = factor(schafer))) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Adding transparency to the violins
  geom_jitter(aes(color = factor(schafer)), width = 0.2, size = 2.5, alpha = 0.6) +  # Jitter points for better visibility
  coord_flip() +  # Flip coordinates
  labs(
    title = "Classifications RFPOPI vs Schafer [POPULISM_RHET]",
    x = "Schafer (0/1)",
    y = "Populism Rhetorics"
  ) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "#f27598")) +  # Custom fill colors for violin plots
  scale_color_manual(values = c("0" = "#122694", "1" = "#e91651")) +  # Custom colors for jitter points
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "none"  # Hide legend
  )



ggplot(rfpopi, aes(x = factor(ineq), y = populism_ide, fill = factor(ineq))) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Adding transparency to the violins
  geom_jitter(aes(color = factor(ineq)), width = 0.2, size = 2.5, alpha = 0.6) +  # Jitter points for better visibility
  coord_flip() +  # Flip coordinates
  labs(
    title = "Classifications RFPOPI vs Inequality [POPULISM_IDE]",
    x = "Inequality (0/1)",
    y = "Populism Ideology"
  ) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "#f27598")) +  # Custom fill colors for violin plots
  scale_color_manual(values = c("0" = "#122694", "1" = "#e91651")) +  # Custom colors for jitter points
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "none"  # Hide legend
  )


ggplot(rfpopi, aes(x = factor(ineq), y = populism_rhet, fill = factor(ineq))) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Adding transparency to the violins
  geom_jitter(aes(color = factor(ineq)), width = 0.2, size = 2.5, alpha = 0.6) +  # Jitter points for better visibility
  coord_flip() +  # Flip coordinates
  labs(
    title = "Classifications RFPOPI vs Inequality [POPULISM_RHET]",
    x = "Inequality (0/1)",
    y = "Populism Rhetorics"
  ) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "#f27598")) +  # Custom fill colors for violin plots
  scale_color_manual(values = c("0" = "#122694", "1" = "#e91651")) +  # Custom colors for jitter points
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "none"  # Hide legend
  )


#----------------------------------------------------------------------------

y_vars <- c('rfpopi_ide', 
                'rand_1_rfpopi_ide0.1', 
                'rand_1_rfpopi_ide0.05', 
                'rand_0_rfpopi_ide0.1', 
                'rand_0_rfpopi_ide0.05', 
                'rfpopi_rhet', 
                'rand_1_rfpopi_rhet0.1',  
                'rand_1_rfpopi_rhet0.05', 
                'rand_0_rfpopi_rhet0.1', 
                'rand_0_rfpopi_rhet0.05')


trust_vars <- c("trust_prt", "trust_prl", "trust_plc",
                "trust_plt", "trust_lgl", "trust_un", "trust_ep")

ess9_rfpopi <- ess9 %>% 
  filter(citizen != 0) %>%
  filter(ESS9_party_name != 'Obedineni patrioti - NFSB, Ataka i VMRO') %>% 
  filter(ESS9_party_name != '') %>% 
  filter(!is.na(rfpopi_ide) & rfpopi_ide != '')


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
                                              data = ess9_rfpopi,
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
                                              data = ess9_rfpopi,
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

# -----------------------------------------------------------------------------
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
                                              data = ess9_rfpopi,
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


#write.csv(ess9, file = "data/ess9_trasnformed_features.csv", row.names = FALSE)
