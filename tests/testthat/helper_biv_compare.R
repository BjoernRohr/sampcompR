
local_test_data_biv1 <- function( remove_nas="all",tableargs=NULL) {

  
  black<<-card[card$black==1,]
  north<<-card[card$south==0,]
  white<<-card[card$black==0,]
  south<<-card[card$south==1,]
  
  
  set.seed(1)
  
  ### analyze data ###
  biv_data<-sampcompR::biv_compare(dfs = c("north","black"),
                                   benchmarks = c("south","white"),
                                   variables = c("age","educ","wage","IQ"),
                                   data=T,
                                   weight = c("weight",NA),
                                   id = c("id",NA),
                                   weight_bench = c("weight",NA),
                                   id_bench = c("id",NA), 
                                   remove_nas = remove_nas)
  
  do.call(biv_compare_table,
          c(list(biv_compare_object=biv_data),tableargs))
}








local_test_data_biv2 <- function(env = parent.frame(),tableargs=NULL) {
  
  card<-sampcompR::card
  
  black<<-card[card$black==1,]
  north<<-card[card$south==0,]
  white<<-card[card$black==0,]
  south<<-card[card$south==1,]
  
  
  set.seed(1)
  
  ### analyze data ###
  biv_data<-sampcompR::biv_compare(dfs = c("north","black"),
                                   benchmarks = c("south","white"),
                                   variables = c("age","educ","wage","IQ"),
                                   data=T,
                                   weight = NULL,
                                   id = NULL,
                                   weight_bench = NULL,
                                   id_bench = NULL,
                                   p_adjust = "bonferroni", 
                                   remove_nas = "pairwise")
  
  do.call(biv_compare_table,
          c(list(biv_compare_object=biv_data),tableargs))
}










