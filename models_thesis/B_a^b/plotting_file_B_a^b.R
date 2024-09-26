#  File for plotting 

library(ggplot2)

#500 TF_a 500 Chp_a 500 Chp_i B_a^b 
file_B_Ab_500 <- read.delim('TF_a_500/B_a^b_results_500.E.out', col.names = 
                             c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                               'E_2','Chp_a','Chp_i','Pho','S','useless'))
file_B_Ab_500$useless <- NULL

ggplot(file_B_Ab_500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle( 'B_a^b - TF on activator 500 - STOP control') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


setwd('TF_a_500')
ggsave(filename = 'B_a^b_stop_500.png', plot = last_plot())

setwd('../')

file_B_Ab_50 <- read.delim('TF_a_50/B_a^b_results_50.E.out', 
                          col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p'
                                        ,'TF_a','TF_i','E_1','E_2','Chp_a','Chp_i','Pho','S','useless'))
file_B_Ab_50$useless <- NULL

ggplot(file_B_Ab_50) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle( 'B_a^b - TF on activator 50 - STOP control') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


setwd('TF_a_50')
ggsave(filename = 'B_a^b_stop_50.png', plot = last_plot())
