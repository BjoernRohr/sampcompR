
local_test_data_uni1 <- function(env = parent.frame(), summet="avg1",
                                 nboots=20, funct="rel_mean") {
  
  data("card")
  
  black<<-card[card$black==1,]
  north<<-card[card$south==0,]
  white<<-card[card$black==0,]
  south<<-card[card$south==1,]
  
  set.seed(1)
  
  ### analyze data ###
  uni_data<-sampcompR::uni_compare(dfs = c("north","black"),
                                   benchmarks = c("south","white"),
                                   nboots = nboots,
                                   variables = c("age","educ"),
                                   funct = funct,
                                   data=T,
                                   summetric = summet,
                                   weight = c("weight",NA),
                                   id = c("id",NA),
                                   weight_bench = c("weight",NA),
                                   id_bench = c("id",NA),
                                   percentile_ci = F)
  
  uni_data
}



# uni_compare_table(uni_data)
# uni_compare_table(uni_data2)
# uni_compare_table(uni_data3)


local_test_data_uni2 <- function(env = parent.frame()) {
  
  data("card")
  
  black<<-card[card$black==1,]
  north<<-card[card$south==0,]
  white<<-card[card$black==0,]
  south<<-card[card$south==1,]
  
  
  
  
  ### analyze data ###
  set.seed(1);
  uni_data<-uni_compare(dfs = c("north","black"),
                        benchmarks = c("south","white"),
                        nboots = 20,
                        variables = c("age","educ"),
                        funct = "d_mean",
                        data=T,
                        summetric = "rmse2",
                        weight = NULL,
                        id = NULL,
                        weight_bench = NULL,
                        id_bench = NULL,
                        percentile_ci = F)
  
  uni_data
}


local_test_data_uni3 <- function(env = parent.frame()) {
  
  
  card1<<-card
  card2<<-card
  
  ### analyze data ###
  sampcompR::R_indicator(dfs= c("card1","card2"),
                        response_identificators = c("south","black"),
                        variables=c("age","educ","fatheduc","motheduc","IQ","wage"),
                        id=c("id",NA),
                        weight=c("weight",NA))
  
}



