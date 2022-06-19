# Analysis PNDS-----------------------------------------------------------------
# Author: Maria Cruz
# Date: 18/06/2022
# Description: This code analysis the data from PNDS from 1986, 1996 and 2006

# 0. Settings-------------------------------------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse', 'tibble', 'ggplot', 'wesanderson'),
                 install = T)
# Input
load('output/panel_women.RData')

# 1. Descriptive analysis ------------------------------------------------------
# 1.1. PNDS 2006 ---------------------------------------------------------------

# Porcentagem de diagnósticos por religião
perc_dis_reli <- panel_women %>%
  dplyr::filter(wm_reli != 'Sem resposta', wm_reli != 'Não sabe') %>% 
  dplyr::mutate(wm_reli =  case_when( wm_reli == 'Afro-brasileira' |
                                        wm_reli == 'Espírita' ~ 'Outra',
                                      wm_reli == 'Evangélica pentecostal' |
                                        wm_reli == 'Evangélica tradicional' ~
                                        'Evangélica',
                                      T ~ wm_reli)) %>% 
  dplyr::select(wm_reli, starts_with('wm_diag_')) %>% 
  dplyr::group_by(wm_reli) %>%
  dplyr::mutate(across(starts_with('wm_diag_'), ~ as.double(.))) %>% 
  dplyr::summarise_all(.funs = sum) %>%
  tidyr::pivot_longer(cols = starts_with('wm_diag_'),
                      names_to = 'cd',
                      values_to = 'sum') %>%
  dplyr::group_by(wm_reli) %>% 
  dplyr::mutate(Doença = factor(cd, levels = c('wm_diag_hipertensao',
                                  'wm_diag_diabetes',	'wm_diag_bronquite_asma',
                                  'wm_diag_depress_ans_inso',	'wm_diag_anemia',
                                  'wm_diag_artrite'),
                                labels=c('Hipertensão e doenças cardiovasculares',
                                         'Diabetes', 'Bronquite e asma',
                                         'Depressão, ansiedade e insônia',
                                         'Anemia',  'Artrite e reumatismo'
                                         )),
                perc = sum/sum(sum),
                label = signif(perc*100, digits = 3)) %>% 
  ggplot(aes(x = wm_reli, y = perc*100, fill = Doença),plot.margin=grid::unit(c(0,2,0,0),"mm")) +
  geom_bar(position="stack", stat="identity")+
  geom_text(position = position_stack(vjust = .5), aes(y = label, label = paste(format(label, nsmall = 1), "%")))+
  scale_fill_brewer(type = 'qual')+ylab('%') + xlab('') + theme_minimal()



# 2. -----------------------------------------------------
# 3. -----------------------------------------------------
# 4. -----------------------------------------------------
# 5. -----------------------------------------------------