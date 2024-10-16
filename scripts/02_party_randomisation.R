##############################################################################
#               Script 02 - randomise party classifications
#                           Irena Zimovska 
#                             16.07.2024
#############################################################################
# Import randomisation functions
source('randomisation_functions.R') 

# 1. 'Populist' Database
# ---------------------------------------------------------------------
populism <- ess9 %>% filter(!is.na(populism) & populism != '') %>% 
  filter(ESS9_party_name != '')

populism  <- populism %>% 
  dplyr::select(
    cntry, ESS9_party_name, populism
  ) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(ESS9_party_name != '') %>% 
  filter(ESS9_party_name != 'Obedineni patrioti - NFSB, Ataka i VMRO')
# ---------------------------------------------------------------------

# RANDOMIZATION 

set.seed(453)
rand_10_pop <- randomize_binary_column_1(populism, 'populism', 0.1)

# 5 % 
set.seed(467)
rand_5_pop <- randomize_binary_column_1(populism, 'populism', 0.05)

# ZERA NA JEDYNKI 
# 10 % 
set.seed(12)
rand_10_pop_0 <- randomize_binary_column_0(populism, 'populism', 0.1)

# 5 % 
set.seed(163)
rand_5_pop_0 <- randomize_binary_column_0(populism, 'populism', 0.05)


# Join randomized column to ESS9 
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_pop[[2]])[, c('ESS9_party_name','rand_1_populism0.1', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_pop[[2]])[, c('ESS9_party_name','rand_1_populism0.05', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_pop_0[[2]])[, c('ESS9_party_name','rand_0_populism0.1', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_pop_0[[2]])[, c('ESS9_party_name','rand_0_populism0.05','cntry')],
  by = c('cntry', 'ESS9_party_name'))


# 2. 'Schafer' Classification
# ---------------------------------------------------------------------
schafer <- ess9 %>% filter(!is.na(schafer) & schafer != '')
schafer  <- schafer %>% 
  dplyr::select(
    cntry, ESS9_party_name, schafer
  ) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(ESS9_party_name != '') %>% 
  filter(ESS9_party_name != 'Obedineni patrioti - NFSB, Ataka i VMRO')
# ----------------------------------------------------------------------------

set.seed(111)
rand_5_sch <- randomize_binary_column_1(schafer, 'schafer', 0.05)

set.seed(222)
rand_10_sch <- randomize_binary_column_1(schafer, 'schafer', 0.1)

set.seed(122)
rand_5_sch_0 <- randomize_binary_column_0(schafer, 'schafer', 0.05)

set.seed(254)
rand_10_sch_0 <- randomize_binary_column_0(schafer, 'schafer', 0.1)

# ----------------------------------------------------------------------------
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_sch[[2]])[, c('ESS9_party_name','rand_1_schafer0.05', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_sch[[2]])[, c('ESS9_party_name','rand_1_schafer0.1', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_sch_0[[2]])[, c('ESS9_party_name','rand_0_schafer0.05', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_sch_0[[2]])[, c('ESS9_party_name','rand_0_schafer0.1', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

# 3. 'Inequality' Classification
# ----------------------------------------------------------------------------
ineq <- ess9 %>% filter(!is.na(ineq) & ineq != '')
ineq <- ineq %>% 
  dplyr::select(
    cntry, ESS9_party_name, ineq
  ) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(ESS9_party_name != '') %>% 
  filter(ESS9_party_name != 'Obedineni patrioti - NFSB, Ataka i VMRO')
# ---------------------------------------------------------------------

set.seed(432)
rand_5_ineq <- randomize_binary_column_1(ineq, 'ineq', 0.05)

set.seed(22)
rand_10_ineq <- randomize_binary_column_1(ineq, 'ineq', 0.1)

set.seed(176)
rand_5_ineq_0 <- randomize_binary_column_0(ineq, 'ineq', 0.05)

set.seed(367)
rand_10_ineq_0 <- randomize_binary_column_0(ineq, 'ineq', 0.1)

# ----------------------------------------------------------------------------

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_ineq[[2]])[, c('ESS9_party_name','rand_1_ineq0.05', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_ineq[[2]])[, c('ESS9_party_name','rand_1_ineq0.1', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_ineq_0[[2]])[, c('ESS9_party_name','rand_0_ineq0.05','cntry')],
  by = c('cntry', 'ESS9_party_name'))

ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_ineq_0[[2]])[, c('ESS9_party_name','rand_0_ineq0.1', 'cntry')],
  by = c('cntry', 'ESS9_party_name'))



############################# IDEOLOGY ########################################
# JEDYNKI NA ZERA 
# 10 % 
set.seed(222)
rand_10_ide <- randomize_binary_column_1(rfpopi, 'rfpopi_ide', 0.1)
# Join randomized column to ESS9 
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_ide[[2]])[, c('ESS9_party_name','rand_1_rfpopi_ide0.1', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

# 5 % 
set.seed(444)
rand_5_ide <- randomize_binary_column_1(rfpopi, 'rfpopi_ide', 0.05)
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_ide[[2]])[, c('ESS9_party_name','rand_1_rfpopi_ide0.05', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

# ZERA NA JEDYNKI 
# 10 % 
set.seed(1223)
rand_10_ide_0 <- randomize_binary_column_0(rfpopi, 'rfpopi_ide', 0.1)
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_ide_0[[2]])[, c('ESS9_party_name','rand_0_rfpopi_ide0.1','cntry')],
                           by = c('ESS9_party_name', 'cntry'))

# 5 % 
set.seed(1653)
rand_5_ide_0 <- randomize_binary_column_0(rfpopi, 'rfpopi_ide', 0.05)
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_ide_0[[2]])[, c('ESS9_party_name','rand_0_rfpopi_ide0.05', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

############################### RHETORICS #####################################

# JEDYNKI NA ZERA 
# 10 % 
set.seed(222)
rand_10_rhet <- randomize_binary_column_1(rfpopi, 'rfpopi_rhet', 0.1)
# Join randomized column to ESS9 
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_rhet[[2]])[, c('ESS9_party_name','rand_1_rfpopi_rhet0.1', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

# 5 % 
set.seed(444)
rand_5_rhet <- randomize_binary_column_1(rfpopi, 'rfpopi_rhet', 0.05)
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_rhet[[2]])[, c('ESS9_party_name','rand_1_rfpopi_rhet0.05', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

# ZERA NA JEDYNKI 
# 10 % 
set.seed(1223)
rand_10_rhet_0 <- randomize_binary_column_0(rfpopi, 'rfpopi_rhet', 0.1)
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_10_rhet_0[[2]])[, c('ESS9_party_name','rand_0_rfpopi_rhet0.1', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

# 5 % 
set.seed(1653)
rand_5_rhet_0 <- randomize_binary_column_0(rfpopi, 'rfpopi_rhet', 0.05)
ess9 <- ess9 %>% left_join(
  as.data.frame(rand_5_rhet_0[[2]])[, c('ESS9_party_name','rand_0_rfpopi_rhet0.05', 'cntry')],
                           by = c('ESS9_party_name', 'cntry'))

