
local_test_data_uni1 <- function(env = parent.frame()) {
  
  list.of.packages <- c("wooldridge")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  black<<-wooldridge::card[wooldridge::card$black==1,]
  north<<-wooldridge::card[wooldridge::card$south==0,]
  white<<-wooldridge::card[wooldridge::card$black==0,]
  south<<-wooldridge::card[wooldridge::card$south==1,]
  
  set.seed(1)
  
  ### analyze data ###
  uni_data<-uni_compare(dfs = c("north","black"),
                        benchmarks = c("south","white"),
                        nboots = 20,
                        variables = c("age","educ"),
                        funct = "rel_mean",
                        data=T,
                        summetric = "avg",
                        weight = c("weight",NA),
                        id = c("id",NA),
                        weight_bench = c("weight",NA),
                        id_bench = c("id",NA))
  
  uni_data
}








local_test_data_uni2 <- function(env = parent.frame()) {
  
  list.of.packages <- c("wooldridge")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  black<<-wooldridge::card[wooldridge::card$black==1,]
  north<<-wooldridge::card[wooldridge::card$south==0,]
  white<<-wooldridge::card[wooldridge::card$black==0,]
  south<<-wooldridge::card[wooldridge::card$south==1,]
  
  
  set.seed(1)
  
  ### analyze data ###
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
                        id_bench = NULL)
  
  uni_data
}


#uni_compare_table(local_test_data2())


