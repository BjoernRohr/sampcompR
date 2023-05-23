### unweighted results of multi_compare ###
local_test_multi_compare1_1 <- function(env = parent.frame()){
  
  ## Get Data for comparison
  card<-wooldridge::card
  
  south <- card[card$south==1,]
  north <- card[card$south==0,]
  
  ## use the function to plot the data
  invisible(multi_data1 <- sampcompR::multi_compare(df = north, 
                                          bench = south,
                                          independent = c("age","fatheduc","motheduc","IQ"),
                                          dependent = c("educ","wage"),
                                          method = "ols",silence_summary = T)) 
  invisible(multi_data1)
}


### weighted results of multi_compare ###
local_test_multi_compare1_2 <- function(env = parent.frame()){
  
  ## Get Data for comparison
  card<-wooldridge::card
  
  south <- card[card$south==1,]
  north <- card[card$south==0,]
  
  ## use the function to plot the data
  invisible(multi_data1 <- sampcompR::multi_compare(df = north, 
                                          bench = south,
                                          independent = c("age","fatheduc","motheduc","IQ"),
                                          dependent = c("educ","wage"),
                                          method = "ols",
                                          weight = "weight",
                                          id="id",
                                          weight_bench = "weight",
                                          id_bench = "id",silence_summary = T))
  invisible(multi_data1)
}




### unweighted results of multi_compare2 ###
local_test_multi_compare2_1 <- function(env = parent.frame()){
  
  ## Get Data for comparison
  card<-wooldridge::card
  
  
  card$married[card$married!=1]<-0
  table(card$married)
  
  south <- card[card$south==1,]
  north <- card[card$south==0,]
  
  formula_list<-list(as.formula(educ ~ age + fatheduc + motheduc + IQ),
                     as.formula(wage ~ age + fatheduc + motheduc + IQ))
  names(formula_list)<-c("educ", "wage")
  
  ## use the function to plot the data 
  invisible(multi_data1<-sampcompR::multi_compare2(df = north, benchmark= south,
                                         formula_list=formula_list,
                                         family = gaussian(link = "identity"),
                                         silence_summary = T))
  multi_data1
}





### unweighted results of multi_compare2 ###
local_test_multi_compare2_2 <- function(env = parent.frame()){
  
  ## Get Data for comparison
  card<-wooldridge::card
  
  
  card$married[card$married!=1]<-0
  table(card$married)
  
  south <- card[card$south==1,]
  north <- card[card$south==0,]
  
  formula_list<-list(as.formula(educ ~ age + fatheduc + motheduc + IQ),
                     as.formula(wage ~ age + fatheduc + motheduc + IQ))
  names(formula_list)<-c("educ", "wage")
  
  ## use the function to plot the data 
  invisible(multi_data1<-sampcompR::multi_compare2(df = north, benchmark= south,
                                         formula_list=formula_list,
                                         family = gaussian(link = "identity"),
                                         weight = "weight",
                                         id="id",
                                         weight_bench = "weight",
                                         id_bench = "id",
                                         silence_summary = T))
  multi_data1
}


normal_glm<-function(env = parent.frame()){
  
  
  ## Get Data for comparison
  card<-wooldridge::card
  
  north <- card[card$south==0,]
  
  model<-glm(educ ~ age + fatheduc + motheduc + IQ, data=north, family = gaussian(link = "identity"))
  round(as.vector(summary(model)$coefficients[,1])[-1],digits = 3)
}



test<-local_test_multi_compare2_2()
