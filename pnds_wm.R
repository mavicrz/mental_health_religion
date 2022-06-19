# PNDS - Woman------------------------------------------------------------------
# Author: Maria Cruz
# Date: 27/05/2022
# Description: This code treat the PNDS from 2006 about women's health

# 0. Settings-------------------------------------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('haven','tidyverse', 'stringr', 'tibble','readstata13', 'purrr'),
                 install = T)

# Input
pnds_women <- 'input/pnds/2006/pnds_mulheres.dta'

pnds_medication <- 'input/pnds/2006/pnds_medicamentos.dta'

# 1. Read data -----------------------------------------------------------------
file_women <- readstata13::read.dta13(pnds_women, generate.factors=T, 
                                      nonint.factors = T, convert.factors = T) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-rec_type)

file_medication <- readstata13::read.dta13(pnds_medication, generate.factors=T, 
                                           nonint.factors = T, convert.factors = T) %>% 
  tibble::as_tibble() %>%
  dplyr::select(mulher_id,	domicilio_id,  m391_diag_1,	m391_diag_2,	m391_diag_3,
                m391_diag_4, m391_diag_5,	m391_diag_6) %>% 
  purrr::set_names(c('wm_id',	'hs_id',	'wm_diag_hipertensao',
                                  'wm_diag_diabetes',	'wm_diag_bronquite_asma',
                                  'wm_diag_depress_ans_inso',	'wm_diag_anemia',
                                  'wm_diag_artrite'))

# 2. Panel ---------------------------------------------------------------------
panel_women <- file_women %>% 
  dplyr::select(mulher_id,	domicilio_id,	cm003_estr,	cm004_macr,	cm008_situ,
                cm023_data_entr,	m102_idad,	m103_onde,	m105_ler,	m106_escu,
                m107_assi,	m108_reli,	m109_reli,	m110_serv,	m111_cor,
                m112_conv,	m113_titu,	m114_filh,	m200_atua,	m202_quis,
                m223_veze,	m224_nunc,	m228_idad,	m227_perd,	m373_sati,
                m374_usar,	m375_gost,	m380_nenh,	m501_casa,	m617_raza,
                m629_mari,	m63001b_dst,	m63002b_outr,	m63003b_part,
                m63004b_cans,	m63005b_nao) %>% 
  purrr::set_names(c('wm_id',	'hs_id',	'reg_sub_macro',	'reg_macro',
                                  'reg_situ','date_interv',	'wm_age',	'wm_12yo',
                                  'hab_read',	'hab_listen', 'hab_watch',
                                  'wm_reli_fam',	'wm_reli',	'wm_reli_freq',
                                  'wm_race',	'hlth_plan',	'hlth_plan_titu',
                                  'hlth_plan_ch', 'wm_preg',	'wm_preg_wanted',
                                  'wm_preg_times', 'wm_why_never_preg',
                                  'wm_age_first_preg', 'wm_lost_bb',
                                  'meth_satisfied',	'meth_use',	'meth_like',
                                  'meht_dont',	'wm_relation',	'meth_dont_want',
                                  'mn_number_ch', 'mn_sex_dst',	'mn_sex_other_wm',
                                  'mn_sex_birth',	'mn_sex_tired',
                                  'mn_sex_dont_want')) %>% 
  dplyr::left_join(file_medication, by = c('wm_id','hs_id')) %>% 
  dplyr::mutate(across(everything(.), ~case_when(. == 95  ~ 'Sem resposta',
                                                 . == 98 ~ 'Não sabe',
                                                 . == 99  ~ 'Não quis responder',
                                                 . == 96 ~ 'Outro',
                                                 T ~ as.character(.)))) %>% 
  dplyr::mutate(across(starts_with('wm_diag_'), ~case_when(. == 2 ~ 0,
                                                       . == 1 ~ 1,
                                                       T ~ 0)))

save(panel_women, file = 'output/panel_women.RData')
