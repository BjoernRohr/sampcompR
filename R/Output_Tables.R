####################################################################
### Tables for Univariate, Bivariate and Multivariate Comparison ###
####################################################################






################################
### Univariate Output Tables ###
################################

#' Create an Output-Table of a uni_compare_object
#'
#' Returns a table based on the information of an \code{uni_compare_object} 
#' which can be outputted as html or LaTex Table, for example with the help of 
#' the \link[stargazer]{stargazer} function.
#'
#' @param uni_compare_object A object returned by 
#' \code{\link[sampcompR]{uni_compare}}.
#' @param conf_adjustment A logical parameter determining if adjusted 
#' confidence intervals should be returned.
#' @param df_names A character vector to relabel the data frames of comparison.
#' @param ndigits The number of digits for rounding in table.
#' @param varlabels A character vector to relabel the Variables in the table.
#' @param ci_line If TRUE, confidence intervals will be displayed in a separate 
#' line, otherwise, they are shown in the same line instead.
#'
#' @return A table containing information on the univariate comparison based on 
#' the \code{\link[sampcompR]{uni_compare}} function.
#' 
# #' @importFrom reshape2 melt 
# #' @importFrom data.table setDT
#' 
#' 
#' @examples
#' 
#' ## Get Data for comparison
#' require(wooldridge)
#' card<-wooldridge::card
#' 
#' south <- card[card$south==1,]
#' north <- card[card$south==0,]
#' black <- card[card$black==1,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' univar_data<-sampcompR::uni_compare(dfs = c("north","white"),
#'                                     benchmarks = c("south","black"),
#'                                     variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                     funct = "abs_rel_mean",
#'                                     nboots=0,
#'                                     summetric="rmse2",
#'                                     data=TRUE)
#'
#' table<-sampcompR::uni_compare_table(univar_data)
#' noquote(table)
#' 
#' @export
#'
#'
uni_compare_table<-function(uni_compare_object, 
                             conf_adjustment=F,
                            df_names=NULL,
                            varlabels=NULL,
                            ci_line=T,
                            ndigits=3){
  
  i<-unique(uni_compare_object$data$sample)
  
  if(is.null(varlabels)) {
    if(is.null(uni_compare_object$varlabels)) varlabels<-uni_compare_object$variables
    if(is.null(uni_compare_object$varlabels)==F) varlabels<-uni_compare_object$varlabels}
  
  if(is.null(df_names)) {df_names<-uni_compare_object$name_dfs}
  
  
  table_list<-purrr::map(i,~subfunc_uni_compare_table(uni_compare_object=uni_compare_object,
                                               i=.,
                                               conf_adjustment=conf_adjustment,
                                               ci_line=ci_line,
                                               ndigits=ndigits))
  
  base<-table_list[[1]]
  for (j in 2:length(i)){
    levels<-uni_compare_object$variables
    if(isTRUE(ci_line)) {
      base<-merge(base,table_list[[j]], by=c("Variables","name"), all=T)
      colnames(base)<- c("Variables","name",df_names[1:j])
      
      }
    if(isFALSE(ci_line)){   
      base<-merge(base,table_list[[j]], by=c("Variables"), all=T) 
      colnames(base)<- c("Variables",df_names[1:j])}
    
  } 
  

  
  if(isTRUE(ci_line)) {
  base<-base|> dplyr::mutate(Variables=factor(Variables,levels= levels,labels=varlabels)) |> 
    dplyr::arrange(Variables,name)}

  if(isFALSE(ci_line)) {
  base<-base|> dplyr::mutate(Variables=factor(Variables,levels= levels,labels=varlabels)) |> 
    dplyr::arrange(Variables)}
  
  
  if(isTRUE(ci_line)) {
    base<-base|> 
    #dplyr::rename_with(~paste0(df_names),starts_with("value")) |> 
    dplyr::mutate(Variables = as.character(Variables)) |> 
    dplyr::select(-"name")}
  if(isFALSE(ci_line)) {
    base<-base|> 
    #dplyr::rename_with(~paste0(df_names),starts_with("value")) |> 
    dplyr::mutate(Variables = as.character(Variables)) }
  
  
  if(isTRUE(ci_line)) base$Variables[seq(2, nrow(base), 2)]<-""
  
 
  ### add summetric ###
  if(is.null(uni_compare_object$summet)==F){
    if(uni_compare_object$summet=="rmse1"| uni_compare_object$summet=="rmse2"){
      base<-base::rbind(base,c("RMSE",format(round(unique(uni_compare_object$data$rmse), digits=ndigits), nsmall=ndigits)))
    }
    
    if(uni_compare_object$summet=="mse1"| uni_compare_object$summet=="mse2"){
      base<-base::rbind(base,c("MSE",format(round(unique(uni_compare_object$data$mse), digits=ndigits), nsmall=ndigits)))
    }
    
    if(uni_compare_object$summet=="avg1"| uni_compare_object$summet=="avg2"){
      base<-rbind(base,c("Average Error",format(round(unique(uni_compare_object$data$avg), digits=ndigits), nsmall=ndigits)))
    }
    
    if(uni_compare_object$summet=="R"){
      base<-rbind(base,c("R-Indicator",format(round(unique(uni_compare_object$data$R_indicator), digits=ndigits), nsmall=ndigits)))
    }
    
  }
  
  ### Add Error Ranking
  if(uni_compare_object$summet=="rmse1"| uni_compare_object$summet=="rmse2"){
    base<-rbind(base,c("RANK",paste(rank(round(unique(uni_compare_object$data$rmse), digits=ndigits)))))
  }
  
  if(uni_compare_object$summet=="mse1"| uni_compare_object$summet=="mse2"){
    base<-rbind(base,c("RANK",paste(rank(round(unique(uni_compare_object$data$mse), digits=ndigits)))))
  }
  
  if(uni_compare_object$summet=="avg1"| uni_compare_object$summet=="avg2"){
    base<-rbind(base,c("RANK",paste(rank(round(unique(uni_compare_object$data$avg), digits=ndigits)))))
  }
  
  if(uni_compare_object$summet=="R"){
    base<-rbind(base,c("RANK",paste(rank(round(unique(uni_compare_object$data$R_indicator), digits=ndigits)))))
  }
  
  
  #add N ###
  or_data<-uni_compare_object$data
  for (i in 1:length(unique(or_data$sample))){
    
    if (i==1) {
      if(min(or_data$n_df[or_data$sample==i])!= max(or_data$n_df[or_data$sample==i])){
        n<- paste(min(or_data$n_df[or_data$sample==i]),"-",max(or_data$n_df[or_data$sample==i]))
      }
      
      if(min(or_data$n_df[or_data$sample==i])== max(or_data$n_df[or_data$sample==i])){
        n<- paste(min(or_data$n_df[or_data$sample==i]))
      }
    }
    
    if (i>1) {
      if(min(or_data$n_df[or_data$sample==i])!= max(or_data$n_df[or_data$sample==i])){
        n<-c(n, paste(min(or_data$n_df[or_data$sample==i]),"-",max(or_data$n_df[or_data$sample==i])))
      }
      
      if(min(or_data$n_df[or_data$sample==i])== max(or_data$n_df[or_data$sample==i])){
        n<-c(n, paste(min(or_data$n_df[or_data$sample==i])))
      }
    }
  }
  
  base<-rbind(base,c("N",n))
  
  
  
  as.matrix(base)
  
}

subfunc_uni_compare_table<-function(uni_compare_object, conf_adjustment=F,names=NULL,i=1, ci_line=T,ndigits=3){
  
  
  if(isFALSE(conf_adjustment)) uni_compare_object <- uni_compare_object$data |> 
      dplyr::select(c("varnames","sample","t_vec","ci_lower","ci_upper","n_df",
               "n_bench")) |> 
      tibble::tibble() |> 
      dplyr::filter(sample==i)
  
  if(isTRUE(conf_adjustment)) uni_compare_object <- uni_compare_object$data |> 
      dplyr::select(c("varnames","sample","t_vec","n_df",
               "n_bench","ci_lower_adjusted","ci_upper_adjusted")) |> 
      dplyr::mutate(ci_lower = ci_lower_adjusted,
             ci_upper = ci_upper_adjusted) |> 
      dplyr::select(-ci_lower_adjusted,-ci_upper_adjusted)|> 
      tibble::tibble() |> 
      dplyr::filter(sample==i)
  
  very_low_neg<-paste0("<-",format(0,nsmall=ndigits))
  very_low_pos<-paste0(">",format(0,nsmall=ndigits))
  
  uni_compare_object<- uni_compare_object|> 
    dplyr::transmute(Variables=varnames,
              t_vec=as.character(format(round(uni_compare_object$t_vec,ndigits), nsmall=ndigits)),
              ci_lower=uni_compare_object$ci_lower,
              ci_upper=uni_compare_object$ci_upper,
              ci_lower=ifelse(ci_lower!=0 & round(ci_lower,ndigits)==0,
                              ifelse(ci_lower<0,very_low_neg,very_low_pos),
                              format(round(ci_lower,ndigits), nsmall=ndigits)),
              ci_upper=ifelse(ci_upper!=0 & round(ci_upper,ndigits)==0,
                              ifelse(ci_upper<0,very_low_neg,very_low_pos),
                              format(round(ci_upper,ndigits), nsmall=ndigits)),
              cis = paste0("(",ci_lower,", ",ci_upper,")")) |> 
    dplyr::select(-ci_lower,-ci_upper)
  
  if(isFALSE(ci_line)){
    uni_compare_object<- uni_compare_object |> 
      dplyr::transmute(Variables=Variables,
                       value= paste(t_vec,cis))
  }
  
  if(isTRUE(ci_line)) {
  
    uni_compare_object<- uni_compare_object |> 
      tidyr::pivot_longer(-c(Variables)) |> 
      dplyr::mutate(name=factor(name, levels= c("t_vec","cis")))}
  
  
  uni_compare_object
  
}


uni_compare_table2<-function(uni_compare_object, conf_adjustment=F,names=NULL){
  
  
  or_data<-uni_compare_object$data[,c("varnames","sample","t_vec","ci_lower","ci_upper","n_df","n_bench")]
  if (isTRUE(conf_adjustment) & "ci_lower_adjusted" %in% colnames(uni_compare_object$data)){
    or_data<-uni_compare_object$data[,c("varnames","sample","t_vec","ci_lower_adjusted","ci_upper_adjusted","n_df","n_bench")]
    colnames(or_data)<-c("varnames","sample","t_vec","ci_lower","ci_upper","n_df","n_bench")}
  #data<-or_data[or_data$sample==1,]
  data2<-NULL
  
  for (i in 1:length(unique(or_data$sample))){
    
    
    data_new<-or_data[or_data$sample==i,]
    data_new$t_vec<-as.character(round(data_new$t_vec, digits = 3))
    #data_new$t_vec[data_new$ci_lower<=0 & data_new$ci_upper>=0]<- paste("<b>",data_new$t_vec[data_new$ci_lower<=0 & data_new$ci_upper>=0], "</b>", sep="")
    
    data_new$ci_lower[round(as.numeric(data_new$ci_lower), digits=3)==0 & data_new$ci_lower>0]<- ">0"
    data_new$ci_lower[round(as.numeric(data_new$ci_lower[data_new$ci_lower!=">0"]), digits=3)==0 & data_new$ci_lower[data_new$ci_lower!=">0"]<0]<- "<0"
    data_new$ci_lower[data_new$ci_lower!= ">0" & data_new$ci_lower!= "<0"]<- round(as.numeric(data_new$ci_lower[data_new$ci_lower!= ">0" & data_new$ci_lower!= "<0"]), digits=3)
    data_new$ci_upper[round(as.numeric(data_new$ci_upper), digits=3)==0 & data_new$ci_upper>0]<- ">0"
    data_new$ci_upper[round(as.numeric(data_new$ci_upper[data_new$ci_upper!=">0"]), digits=3)==0 & data_new$ci_upper[data_new$ci_upper!=">0"]<0]<- "<0"
    data_new$ci_upper[data_new$ci_upper!= ">0" & data_new$ci_upper!= "<0"]<- round(as.numeric(data_new$ci_upper[data_new$ci_upper!= ">0" & data_new$ci_upper!= "<0"]), digits=3)
    data_new$ci<-paste("(", data_new$ci_lower,", ",data_new$ci_upper,")", sep = "")
    
    data_new<-data_new[,c("varnames","t_vec","ci")]
    # data_new$t_vec<-as.character(data_new$t_vec)
    data_new<- reshape2::melt(data.table::setDT(data_new), id.vars = c("varnames"), variable.name = "measure" )
    colnames(data_new)<-c("varnames",paste("measure",i,sep=""),paste("value",i,sep=""))
    
    if(i==1){
      data<-data_new
      
      missing_varnames<-unique(or_data$varnames)[!unique(or_data$varnames) %in% unique(data$varnames)]
      if(length(missing_varnames)>0) data2<-data.frame(varnames=missing_varnames,measure=NA, value=NA)
      if(length(missing_varnames)>0) colnames(data2)<-c("varnames",paste("measure",i,sep=""),paste("value",i,sep=""))
      if(length(missing_varnames)>0) data<-rbind(data,data2)
      
    }
    
    if(i>=2){
      
      data<-merge(data,data_new,by.x=c("varnames",paste("measure",1,sep="")),by.y=c("varnames",paste("measure",i,sep="")), all=T)
      data<-data[rowSums(is.na(data)) != (ncol(data)-1), ]
      
    }
    
    
  }
  if(is.null(uni_compare_object$variables)){
  data<-data[order(factor(data$varnames, levels = unique(or_data$varnames))),]}
  if(!is.null(uni_compare_object$variables))
  data<-data[order(factor(data$varnames, levels = uni_compare_object$variables)),]

  
  if(data$measure1[1]=="ci"){
    ### make it the right order
    reorder_indices <- c()
    for (i in 1:(nrow(data)/2)) {
      reorder_indices <- c(reorder_indices, (2*i), (2*i-1))}
    
    
    data <- data[reorder_indices, ]}
  
  data<-data[is.na(data$measure1)==F,]
  data$varnames[seq(2, nrow(data), 2)]<-""
  data$measure1<-NULL
  colnames(data)<-c("variables", unique(uni_compare_object$data$name_dfs))
  data<-as.matrix(data)
  
  ### add summetric ###
  if(is.null(uni_compare_object$summet)==F){
    if(uni_compare_object$summet=="rmse"| uni_compare_object$summet=="rmse2"){
      data<-rbind(data,c("RMSE",round(unique(uni_compare_object$data$rmse), digits=3)))
    }
    
    if(uni_compare_object$summet=="mse"| uni_compare_object$summet=="mse2"){
      data<-rbind(data,c("MSE",round(unique(uni_compare_object$data$mse), digits=3)))
    }
    
    if(uni_compare_object$summet=="avg"){
      data<-rbind(data,c("Average Error",round(unique(uni_compare_object$data$avg), digits=3)))
    }
    
    if(uni_compare_object$summet=="R"){
      data<-rbind(data,c("R-Indicator",round(unique(uni_compare_object$data$R_indicator), digits=3)))
    }
    
  }
  
  ### Add Error Ranking
  data<-rbind(data,c("RANK",paste(order(round(unique(uni_compare_object$data$rmse), digits=3)))))
  
  #add N ###
  
  for (i in 1:length(unique(or_data$sample))){
    
    if (i==1) {
      if(min(or_data$n_df[or_data$sample==i])!= max(or_data$n_df[or_data$sample==i])){
        n<- paste(min(or_data$n_df[or_data$sample==i]),"-",max(or_data$n_df[or_data$sample==i]))
      }
      
      if(min(or_data$n_df[or_data$sample==i])== max(or_data$n_df[or_data$sample==i])){
        n<- paste(min(or_data$n_df[or_data$sample==i]))
      }
    }
    
    if (i>1) {
      if(min(or_data$n_df[or_data$sample==i])!= max(or_data$n_df[or_data$sample==i])){
        n<-c(n, paste(min(or_data$n_df[or_data$sample==i]),"-",max(or_data$n_df[or_data$sample==i])))
      }
      
      if(min(or_data$n_df[or_data$sample==i])== max(or_data$n_df[or_data$sample==i])){
        n<-c(n, paste(min(or_data$n_df[or_data$sample==i])))
      }
    }
  }
  
  data<-rbind(data,c("N",n))
  rownames(data)<-NULL
  
  if(is.null(names)==F) colnames(data)<-c("variables",names)
  if(is.null(names)==T) colnames(data)[1]<-c("variables")
  
  return(data)
}









### Bivariate output table ###

#' Returns a table based on the information of a \code{biv_compare_object} which 
#' can be outputted as html or LaTex Table, for example with the help of the 
#' \link[stargazer]{stargazer} function.
#' 
#' @param biv_compare_object A object returned by the 
#' \code{\link[sampcompR]{biv_compare}} function.
#' @param type A character string, to choose what matrix should be printed. 
#' * If "dfs", a correlation matrix of all variables of comparison in the chosen 
#' dataframe will be returned.
#' * If "benchmarks", a correlation matrix of all variables of comparison in 
#' the chosen benchmark will be returned.
#' * if "diff", a matrix indicating the difference between the chosen 
#' dataframe and benchmark will be returned.
#' @param comparison_number A number indicating the data of which data frame, 
#' benchmark or comparison should be displayed.
#' The maximum length is equal to the length of the \code{dfs vector} that is 
#' used to generate the \code{biv_compare_object}.
#' @param ndigits Number of digits shown in the table.
#'
#' @examples
#' 
#' ## Get Data for comparison
#' require(wooldridge)
#' card<-wooldridge::card
#' 
#' south <- card[card$south==1,]
#' north <- card[card$south==0,]
#' black <- card[card$black==1,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("south","black"),
#'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                    data=TRUE)
#'                         
#' table<-sampcompR::biv_compare_table(bivar_data, type="diff", comparison_number=1)
#' noquote(table)
#' 
#'
#' @export
#' 
#' @return A correlation matrix, or difference matrix based on information of a \code{biv_compare_object}.
biv_compare_table<-function(biv_compare_object, type="diff", comparison_number=1,ndigits=2){
  
  get_stars_biv<-function(biv_compare_object, type="diff", comparison_number=1){
    
    if(type=="diff") {
      r_data<-biv_compare_object[[(comparison_number+1)]]$r_diff_matrix
      p_values<-biv_compare_object[[(comparison_number+1)]]$r_diff_p_matrix
    }
    
    if(type=="dfs") {
      r_data<-biv_compare_object[[(comparison_number+1)]]$pearsons_matrix_df
      p_values<-biv_compare_object[[(comparison_number+1)]]$p_matrix_df
    }
    
    if(type=="benchmarks") {
      r_data<-biv_compare_object[[(comparison_number+1)]]$pearsons_r_bench
      p_values<-biv_compare_object[[(comparison_number+1)]]$p_matrix_bench
    }
    
    r_data[is.na(p_values)==F]<-format(round(r_data[is.na(p_values)==F],digits = ndigits),nsmall=ndigits)
    r_data[is.na(p_values)]<-""
    r_data[p_values<0.001 & is.na(r_data)==F & is.na(p_values)==F]<-
      paste0(r_data[p_values<0.001 & is.na(r_data)==F & is.na(p_values)==F],"***")
    r_data[p_values<0.01 & p_values>=0.001 & is.na(r_data)==F & is.na(p_values)==F]<-
      paste0(r_data[p_values<0.01 & p_values>=0.001  & is.na(r_data)==F & is.na(p_values)==F],"** ")
    r_data[p_values<0.05 & p_values>=0.01 & is.na(r_data)==F & is.na(p_values)==F]<-
      paste0(r_data[p_values<0.05 & p_values>=0.01  & is.na(r_data)==F & is.na(p_values)==F],"*  ")
    r_data[p_values>=0.05 & is.na(r_data)==F & is.na(p_values)==F]<-
      paste0(r_data[p_values>=0.05 & is.na(r_data)==F & is.na(p_values)==F],"   ")
    
    return(r_data)
  }
  
  outpt<-get_stars_biv(biv_compare_object=biv_compare_object,type=type,comparison_number=comparison_number)
  return(outpt)
  
}


# biv_compare_table<-function(df,bench, varlabels=NULL, weight=NULL, ID=NULL, all=NULL){
#   
#   weighted_cor<-function(x, weight, ID){
#     
# 
#     x<-lapply(x, as.numeric)
#     x<-as.data.frame(x)
#     
#     
#     df_weighted<- svydesign(id      = ~ID,
#                             strata  = NULL,
#                             weights = ~weight,
#                             nest    = FALSE,
#                             data    = x)
#     
#     insertform<-paste("~",colnames(x[1]))
#     for (i in 2:(length(x))) {
#       insertform<-paste(insertform," + ",colnames(x[i]), collapse = "")
#     }
#     
#     
#     matrix<-svycor(formula=as.formula(insertform), design = df_weighted, na.rm = TRUE)
#     matrix$cors[matrix$cors==1]<-NA
#     
#     return(matrix$cors)
#   }
#   
#   if (is.null(weight)==FALSE & is.null(ID)==FALSE) {
#     df2<-df
#     df[,c(weight,ID)]<-NULL
#   }
#   cor_matrix_df<-rcorr(as.matrix(df[,1:ncol(df)]))
#   if (is.null(weight)==FALSE & is.null(ID)==FALSE) {
#     cor_matrix_df$r<- weighted_cor(df, df2[,weight] , ID=df2[,ID])
#     cor_matrix_df$r[is.na(cor_matrix_df$r)]<-1
#   }
#   #  fischer_cor_df<- fisherz(cor_matrix_df$r)
#   #  fischer_cor_micro<- fisherz(bench$r)
#   
#   
#   fischer_z_test<-psych::paired.r(cor_matrix_df$r,bench$r,n=cor_matrix_df$n, n2=bench$n)
#   
#   
#   cor_matrix_df[[4]]<-fischer_z_test$z
#   cor_matrix_df[[5]]<-fischer_z_test$p
#   
#   for (i in 1:5){
#     
#     cor_matrix_df[[i]] <- round(cor_matrix_df[[i]],3)
#   }
#   
#   
#   cor_matrix_df[[3]][is.na(cor_matrix_df[[3]])]<-0
#   cor_matrix_df[[5]][is.na(cor_matrix_df[[5]])]<-0
#   
#   bench$r <- round(bench$r,3)
#   
#   #bench$r[bench$r==Inf]<-0
#   
#   cor_matrix_df[[6]]<-cor_matrix_df[[1]]
#   cor_matrix_df[[7]]<-cor_matrix_df[[5]]
#   
#   ### p_adjust ###
#   cor_matrix_df[[3]][upper.tri(cor_matrix_df[[3]], diag =TRUE)]<-NA
#   cor_matrix_df[[3]]<-matrix(stats::p.adjust(p = cor_matrix_df[[3]], method = "bonferroni"), nrow(cor_matrix_df[[3]]),ncol(cor_matrix_df[[3]]))
#   
#   cor_matrix_df[[5]][upper.tri(cor_matrix_df[[5]], diag =TRUE)]<-NA
#   cor_matrix_df[[5]]<-matrix(stats::p.adjust(p = cor_matrix_df[[5]], method = "bonferroni"), nrow(cor_matrix_df[[5]]),ncol(cor_matrix_df[[5]]))
#   
#   
#   
#   cor_matrix_df[[6]][cor_matrix_df[[3]]>0.05 & is.na(cor_matrix_df[[3]])==F]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]>0.05 & is.na(cor_matrix_df[[3]])==F], "   ", sep = "")
#   cor_matrix_df[[6]][cor_matrix_df[[3]]<0.05 & cor_matrix_df[[3]]>=0.01 & is.na(cor_matrix_df[[3]])==F]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]<0.05 & cor_matrix_df[[3]]>=0.01 & is.na(cor_matrix_df[[3]])==F], "*  ", sep = "")
#   cor_matrix_df[[6]][cor_matrix_df[[3]]<0.01 & cor_matrix_df[[3]]>=0.001 & is.na(cor_matrix_df[[3]])==F]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]<0.01 & cor_matrix_df[[3]]>=0.001 & is.na(cor_matrix_df[[3]])==F], "** ", sep = "")
#   cor_matrix_df[[6]][cor_matrix_df[[3]]<0.001 & is.na(cor_matrix_df[[3]])==F]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]<0.001 & is.na(cor_matrix_df[[3]])==F], "***", sep = "")
#   
#   cor_matrix_df[[7]][cor_matrix_df[[5]]>0.05 & is.na(cor_matrix_df[[5]])==F]<-paste("'",round((bench$r[cor_matrix_df[[5]]>0.05 & is.na(cor_matrix_df[[5]])==F] - cor_matrix_df[[1]][cor_matrix_df[[5]]>0.05 & is.na(cor_matrix_df[[5]])==F]), 3) , "   ", sep = "")
#   cor_matrix_df[[7]][cor_matrix_df[[5]]<0.05 & cor_matrix_df[[5]]>=0.01 & is.na(cor_matrix_df[[5]])==F]<-paste("'",round((bench$r[cor_matrix_df[[5]]<0.05 & cor_matrix_df[[5]]>=0.01 & is.na(cor_matrix_df[[5]])==F]- cor_matrix_df[[1]][cor_matrix_df[[5]]<0.05 & cor_matrix_df[[5]]>=0.01 & is.na(cor_matrix_df[[5]])==F]),3), "*  ", sep = "")
#   cor_matrix_df[[7]][cor_matrix_df[[5]]<0.01 & cor_matrix_df[[5]]>=0.001 & is.na(cor_matrix_df[[5]])==F]<-paste("'",round((bench$r[cor_matrix_df[[5]]<0.01 & cor_matrix_df[[5]]>=0.001 & is.na(cor_matrix_df[[5]])==F] - cor_matrix_df[[1]][cor_matrix_df[[5]]<0.01 & cor_matrix_df[[5]]>=0.001 & is.na(cor_matrix_df[[5]])==F] ), 3), "** ", sep = "")
#   cor_matrix_df[[7]][cor_matrix_df[[5]]<0.001 & is.na(cor_matrix_df[[5]])==F]<-paste("'",round((bench$r[cor_matrix_df[[5]]<0.001 & is.na(cor_matrix_df[[5]])==F] - cor_matrix_df[[1]][cor_matrix_df[[5]]<0.001 & is.na(cor_matrix_df[[5]])==F] ) ,3), "***", sep = "")
#   
#   
#   
#   for (i in 1:7){
#     cor_matrix_df[[i]][upper.tri(cor_matrix_df[[i]], diag =TRUE)]<-""
#   }
#   
#   names(cor_matrix_df)<- c("Persons_r", "n", "pearsons_p-values","Fishers_Z", "Fishers_p-values",
#                            "pearsons_r_star","pearsons_r_diff_star")
#   
#   
#   shortlist<-list(cor_matrix_df[[6]],cor_matrix_df[[7]])
#   
#   #if (is.null(varlabels)==F) colnames(shortlist[[1]])<-varlabels
#   if (is.null(varlabels)==F) rownames(shortlist[[1]])<-varlabels
#   #if (is.null(varlabels)==F) colnames(shortlist[[2]])<-varlabels
#   if (is.null(varlabels)==F) rownames(shortlist[[2]])<-varlabels
#   
#   if (is.null(all)==F) return(cor_matrix_df)
#   return(shortlist)
# }



#' Create an Output-Table of a multi_compare_object
#'
#' Returns a table based on the information of a \code{multi_compare_object} 
#' which can be outputted as HTML or LaTex Table, for example with the help of 
#' the \link[stargazer]{stargazer} function.
#'
#' @param multi_compare_objects One or more object that were returned by 
#' \code{\link[sampcompR]{multi_compare}}.
#' @param type A character string, to determine the type of regression table. 
#' * If "dfs" a regression table based on the data frame(s) is returned. 
#' * If "benchmarks" a regression table based on the benchmark(s) is returned. 
#' * If "diff" a table indicating the difference between the df(s) and the 
#' benchmark(s) is returned.
#' @param names A character vector to rename the data frames of comparison.
#' @param ndigits The Number of digits that is shown in the table.
#' @param envir The environment, where the \code{multi_core_objects} can be 
#' found.
#' 
#' 
#'@return A table containing information on the multivariate comparison based on 
#'the \code{\link[sampcompR]{multi_compare}} function.
#'
#'
#' @examples
#' 
#' ## Get Data for comparison
#' require(wooldridge)
#' card<-wooldridge::card
#' 
#' south <- card[card$south==1,]
#' north <- card[card$south==0,]
#' black <- card[card$black==1,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data
#' multi_data1 <- sampcompR::multi_compare(df = north, 
#'                                          bench = south,
#'                                          independent = c("age","fatheduc","motheduc","IQ"),
#'                                          dependent = c("educ","wage"),
#'                                          family = "ols") 
#'                                      
#' multi_data2 <- sampcompR::multi_compare(df = black, 
#'                                          bench = white,
#'                                          independent = c("age","fatheduc","motheduc","IQ"),
#'                                          dependent = c("educ","wage"),
#'                                          family = "ols") 
#'                                      
#' table<-multi_compare_table(c("multi_data1","multi_data2"),type="diff")
#'
#' noquote(table)
#'
#' @export
#'
#'
### Multivariate output table ###

multi_compare_table<-function(multi_compare_objects,type="diff",names=NULL,ndigits=3,
                              envir=parent.frame()){
  
  get_stars<-function(multi_compare_object, name=NULL, type="diff"){
    
    if (is.null(name)){
      if(type=="diff"| type=="dfs") name<-multi_compare_object$names_df_benchmark[[1]]
      if(type=="benchmarks") name<-multi_compare_object$names_df_benchmark[[2]]} 
    
    if (type=="diff"){
      coef<-multi_compare_object$coefs_difference_star
      se<-multi_compare_object$coefs_diff_se}
    
    if (type=="dfs"){
      coef<-multi_compare_object$coefs_data1_star
      se<-multi_compare_object$coefs1_se}
    
    if (type=="benchmarks"){
      coef<-multi_compare_object$coefs_data2_star
      se<-multi_compare_object$coefs2_se}
    
    
    row1<-matrix( data= c(name,rep("", (2*length(multi_compare_object$independent)-1))),ncol = 1)
    colnames(row1)<-"data_frames"
    
    
    rest<-matrix(coef,nrow = length(multi_compare_object$independent))
    
    ### round rest and add stars again ##
    star<-matrix("   ",ncol=ncol(coef),nrow = nrow(coef))
    star[grepl("*  ", coef, fixed = TRUE) & !grepl("** ", coef, fixed = TRUE) & !grepl("***", coef, fixed = TRUE)]<-"*  "
    star[grepl("** ", coef, fixed = TRUE) & !grepl("***", coef, fixed = TRUE)]<- "** "
    star[grepl("***", coef, fixed = TRUE)]<-"***"
    
    positive<-matrix("",ncol=ncol(coef),nrow = nrow(coef))
    positive[readr::parse_number(rest)>0]<-" "
    
    
    rest_rounded<-format(round(as.matrix(readr::parse_number(rest)),digits=ndigits),nsmall=ndigits)
    rest_rounded[readr::parse_number(rest)>0 & rest_rounded==0]<-">0   "
    rest_rounded[readr::parse_number(rest)<0 & rest_rounded==0]<-"<0   "
    
    rest<-matrix(paste(positive,rest_rounded,star, sep=""),nrow = nrow(rest))
    
    rest<-cbind(rest,seq(from =1, to=(nrow(rest)*2), by=2))
    se<-cbind(matrix(paste("(",round(se,digits=ndigits),")",sep = ""),nrow= nrow(se)), seq(from =2, to=(nrow(rest)*2), by=2))

    colnames(rest)<-c(multi_compare_object$dependent, "help")
    
    
    rest<-rbind(rest,se)
    rest<-as.data.frame(rest)
    rest<-as.matrix(rest[order(as.numeric(rest$help)),1:(ncol(rest)-1)])
    
    ### get a row with variable names ###
    variables <- as.matrix(rep(c(multi_compare_object$independent), each = 2))
    variables[c(FALSE, TRUE)] <- ""
    colnames(variables)<-"variables"
    
    
    
    rownames(rest)<-NULL
    
    final_matrix<-as.data.frame(cbind(row1,variables,rest))
    
    
    return(final_matrix)
  }
  
  if(is.null(names)) table_list<-mapply(get_stars, lapply(multi_compare_objects,get, envir = envir), type=type, SIMPLIFY = F)
  if(is.null(names)==F) table_list<-mapply(get_stars, lapply(multi_compare_objects,get, envir = envir), names, type=type ,SIMPLIFY = F)

  as.matrix(dplyr::bind_rows(table_list),nrow = nrow(get(multi_compare_objects[1])$independent)*length(multi_compare_objects),envir = envir)
  
}










descript_table_sub<-function(df, variables, varlabels=NULL, weight=NULL,strata=NULL,id=NULL, value="mean", digits=NULL){
  
    design <- survey::svydesign(ids = id, weights = weight, strata = strata, data=df)
    
    if (value=="mean"){
      output<-sapply(variables,
                     function(variable,design){survey::svymean(stats::reformulate(variable),design,na.rm=T)}, 
                     design, simplify = T)
      
      output<-matrix(round(output,digits=digits),ncol=1)
    }
    
    if (value=="percent"){
      output<-sapply(variables,
                     function(variable,design){survey::svymean(stats::reformulate(variable),design,na.rm=T)}, 
                     design, simplify = T)
      
      output<-paste(round((output*100), digits = digits),"%", sep="")
      
      output<-matrix(output,ncol=1)
    }
    
    if (value=="total"){
      output<-sapply(variables,
                     function(variable,design){survey::svytotal(stats::reformulate(variable),design,na.rm=T)}, 
                     design, simplify = T)
      
      output<-matrix(round(output,digits = digits),ncol=1)
      
    }
    
    if (value=="total_percent"){
      output1<-sapply(variables,
                     function(variable,design){survey::svytotal(stats::reformulate(variable),design,na.rm=T)}, 
                     design, simplify = T)
      
      output2<-sapply(variables,
                     function(variable,design){survey::svymean(stats::reformulate(variable),design,na.rm=T)}, 
                     design, simplify = T)
      
      output <- paste(round(output1,digits = digits)," (",round((output2*100),digits = digits),"%)",sep="" )
      output<-matrix(output,ncol=1)
      
      
    }
   
    output<-rbind(output,nrow(df))
  
    if(is.null(varlabels)) rownames(output)<- c(variables,"N") else rownames(output)<- c(varlabels,"N")
  
    output
}


#' Get a Descriptive Table for Every Data Frame
#'
#' Get a Descriptive Table for every Data Frame, to easy document your Data
#' 
#' @param dfs A character vector, containing the names of the data frames.
#' @param variables A character vector containing the variables in the data frame 
#' that should be described.
#' @param varlabels A character vector containing the Labels for every variable in variables.
#' @param weight A character vector, containing either the name of a weight in the 
#' respective data frame, or NA, if no weighting should be performed for this data frame.
#' @param id A character vector, containing either the name of a id in the 
#' respective data frame, or NA, if every row is unique for this data frame.
#' @param strata A character vector, containing either the name of a strata in the 
#' respective data frame, or NA, if no strata  should be used when weighting this 
#' data frame.
#' @param value A character vector indicating what descriptive value should be displayed
#' for the data frame. It can either be "mean", "percent", "total", or "total_percent".
#' @param digits A numeric value indicating the number of digits that the Descriptive table 
#' should be rounded to.
#' 
#' @returns Returns a matrix of Descriptive information. Output depends on value.
#' 
#' 
#'
#'@export
descriptive_table<-function(dfs,variables,varlabels=NULL, weight=NULL,
                            strata=NULL,id=NULL, value="mean", digits=3){
  
  table<-NULL
  
  if(length(value)==1) value<-c(rep(value,length(dfs)))
  
  for (i in 1:length(dfs)){
    
    ### look if weight_var is there ###
    if(is.null(weight)) weights<-get(dfs[i])[,weight[i]]
    if(is.null(strata)) stratas<-NULL
    if(is.null(id)) ids<-c(1:nrow(get(dfs[i])))
    
    if(is.null(weight)==F) {
      if(is.na(weight[i])==F) weights<-get(dfs[i])[,weight[i]]/(sum(get(dfs[i])[,weight[i]])/nrow(get(dfs[i])))
      if(is.na(weight[i])==T) weights<-rep(1,nrow(get(dfs[i])))}
    if(is.null(strata)==F) {
      if(is.na(strata[i])==F) stratas<-get(dfs[i])[,strata[i]]
      if(is.na(strata[i])==T) stratas<-NULL}
    if(is.null(id)==F) {
      if(is.na(id[i])==F) ids<-get(dfs[i])[,id[i]]
      if(is.na(id[i])==T) ids<-c(1:nrow(get(dfs[i])))}
    
    #dataframe1$df_weights/(sum(dataframe1$df_weights)/nrow(dataframe1))
    weights<-weights/(sum(weights)/nrow(get(dfs[i])))
    
    
    help<-descript_table_sub(df=get(dfs[i]), variables=variables, varlabels = varlabels,
                            weight=weights,strata=stratas,id=ids, value=value[i],digits=digits)
    
   
    
    
    if(is.null(table)){
      table<-help
    }
    else table<-cbind(table,help)
  }
  
  
  colnames(table)<-dfs
  return(table)
}





