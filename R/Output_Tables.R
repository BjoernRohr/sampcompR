####################################################################
### Tables for Univariate, Bivariate and Multivariate Comparison ###
####################################################################






################################
### Univariate Output Tables ###
################################

#' Create an Output-Table of a uni_compare_object
#'
#' Returns a table based on the information of an \code{uni_compare_object} 
#' which can be outputted as HTML or LaTex Table, for example with the help of 
#' the \link[stargazer]{stargazer} function.
#'
#' @param uni_compare_object A object returned by 
#' \code{\link[sampcompR]{uni_compare}}.
#' @param conf_adjustment A logical parameter determining if adjusted 
#' confidence intervals should be returned.
#' @param df_names A character vector to relabel the data frames of comparison.
#' @param ndigits The number of digits to round the numbers in table.
#' @param varlabels A character vector to relabel the variables in the table.
#' @param ci_line If \code{TRUE}, confidence intervals will be displayed in a 
#' separate line, otherwise, they are shown in the same line instead.
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
#' 
#' data("card")
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' univar_data<-sampcompR::uni_compare(dfs = c("north","white"),
#'                                     benchmarks = c("card","card"),
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
                             conf_adjustment=FALSE,
                            df_names=NULL,
                            varlabels=NULL,
                            ci_line=TRUE,
                            ndigits=3){
  
  i<-unique(uni_compare_object$data$sample)
  
  if(is.null(varlabels)) {
    if(is.null(uni_compare_object$varlabels)) varlabels<-uni_compare_object$variables
    if(is.null(uni_compare_object$varlabels)==FALSE) varlabels<-uni_compare_object$varlabels}
  
  if(is.null(df_names)) {df_names<-uni_compare_object$name_dfs}
  
  
  table_list<-purrr::map(i,~subfunc_uni_compare_table(uni_compare_object=uni_compare_object,
                                               i=.,
                                               conf_adjustment=conf_adjustment,
                                               ci_line=ci_line,
                                               ndigits=ndigits))
  
  base<-table_list[[1]]
  levels<-uni_compare_object$variables
  
  if(length(i)==1){
    if(isTRUE(ci_line)) {
      colnames(base)<- c("Variables","name",df_names[1])}
    if(isFALSE(ci_line)){
      colnames(base)<- c("Variables",df_names[1])}
  }
  
  if(length(i)>=2){
  for (j in 2:length(i)){
    
    if(isTRUE(ci_line)) {
      base<-merge(base,table_list[[j]], by=c("Variables","name"), all=TRUE)
      colnames(base)<- c("Variables","name",df_names[1:j])
      
      }
    if(isFALSE(ci_line)){   
      base<-merge(base,table_list[[j]], by=c("Variables"), all=TRUE) 
      colnames(base)<- c("Variables",df_names[1:j])}
    
  }} 


  
  if(isTRUE(ci_line)) {
  base<-base|> dplyr::mutate(Variables=factor(base$Variables,levels= levels,labels=varlabels))  
  base<-base|>dplyr::arrange(base$Variables,base$name)}

  if(isFALSE(ci_line)) {
  base<-base|> dplyr::mutate(Variables=factor(base$Variables,levels= levels,labels=varlabels)) 
  base<-base|> dplyr::arrange(base$Variables)}
  
  
  if(isTRUE(ci_line)) {
    base<-base|> 
    #dplyr::rename_with(~paste0(df_names),starts_with("value")) |> 
    dplyr::mutate(Variables = as.character(base$Variables)) |> 
    dplyr::select(-"name")}
  if(isFALSE(ci_line)) {
    base<-base|> 
    #dplyr::rename_with(~paste0(df_names),starts_with("value")) |> 
    dplyr::mutate(Variables = as.character(base$Variables)) }
  
  
  if(isTRUE(ci_line)) base$Variables[seq(2, nrow(base), 2)]<-""
  
 
  ### add summetric ###
  if(is.null(uni_compare_object$summet)==FALSE){
    if(uni_compare_object$summet=="rmse1"| uni_compare_object$summet=="rmse2"){
      
      rmse<- uni_compare_object$data |>
        dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
        dplyr::group_by("name_dfs") |> 
        dplyr::summarise(unique(rmse))
      rmse<-as.numeric(as.data.frame(rmse)[,2])
      base<-base::rbind(base,c("RMSE",format(round(rmse, digits=ndigits), nsmall=ndigits)))
    }
    
    if(uni_compare_object$summet=="mse1"| uni_compare_object$summet=="mse2"){
      
      mse<- uni_compare_object$data |>
        dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
        dplyr::group_by("name_dfs") |> 
        dplyr::summarise(unique(mse))
      mse<-as.numeric(as.data.frame(mse)[,2])
      base<-base::rbind(base,c("MSE",format(round(mse, digits=ndigits), nsmall=ndigits)))
    }
    
    if(uni_compare_object$summet=="avg1"| uni_compare_object$summet=="avg2"){
      
      avg<- uni_compare_object$data |>
        dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |> 
        dplyr::group_by("name_dfs") |> 
        dplyr::summarise(unique(avg))
      avg<-as.numeric(as.data.frame(avg)[,2])
      base<-rbind(base,c("Average Error",format(round(avg, digits=ndigits), nsmall=ndigits)))
    }
    
    if(uni_compare_object$summet=="R"){
      R_indicator<- uni_compare_object$data |>
        dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
        dplyr::group_by("name_dfs") |> 
        dplyr::summarise(unique(R_indicator))
      R_indicator<-as.numeric(as.data.frame(R_indicator)[,2])
      base<-rbind(base,c("R-Indicator",format(round(R_indicator, digits=ndigits), nsmall=ndigits)))
    }
    
  }
  
  ### Add Error Ranking
  if(uni_compare_object$summet=="rmse1"| uni_compare_object$summet=="rmse2"){
    
    rmse<- uni_compare_object$data |>
      dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
      dplyr::group_by("name_dfs") |> 
      dplyr::summarise(unique(rmse))
    rmse<-as.numeric(as.data.frame(rmse)[,2])
    base<-rbind(base,c("RANK",paste(rank(round(rmse, digits=ndigits)))))
  }
  
  if(uni_compare_object$summet=="mse1"| uni_compare_object$summet=="mse2"){
    
    mse<- uni_compare_object$data |>
      dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
      dplyr::group_by("name_dfs") |> 
      dplyr::summarise(unique(mse))
    mse<-as.numeric(as.data.frame(mse)[,2])
    base<-rbind(base,c("RANK",paste(rank(round(mse, digits=ndigits)))))
  }
  
  if(uni_compare_object$summet=="avg1"| uni_compare_object$summet=="avg2"){
    
    avg<- uni_compare_object$data |>
      dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
      dplyr::group_by("name_dfs") |> 
      dplyr::summarise(unique(avg))
    avg<-as.numeric(as.data.frame(avg)[,2])
    base<-rbind(base,c("RANK",paste(rank(round(avg, digits=ndigits)))))
  }
  
  if(uni_compare_object$summet=="R"){
    
    R_indicator<- uni_compare_object$data |>
      dplyr::mutate(name_dfs=factor("name_dfs",levels=uni_compare_object$name_dfs)) |>
      dplyr::group_by("name_dfs") |> 
      dplyr::summarise(unique(R_indicator))
    R_indicator<-as.numeric(as.data.frame(R_indicator)[,2])
    base<-rbind(base,c("RANK",paste(rank(round(R_indicator, digits=ndigits)))))
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

subfunc_uni_compare_table<-function(uni_compare_object, conf_adjustment=FALSE,names=NULL,i=1, ci_line=TRUE,ndigits=3){
  
  
  if(isFALSE(conf_adjustment)) uni_compare_object <- uni_compare_object$data |> 
      dplyr::select(c("varnames","sample","t_vec","ci_lower","ci_upper","n_df",
               "n_bench")) |> 
      tibble::tibble() |> 
      dplyr::filter(sample==i)
  
  if(isTRUE(conf_adjustment)) uni_compare_object <- uni_compare_object$data |> 
      dplyr::select(c("varnames","sample","t_vec","n_df",
               "n_bench","ci_lower_adjusted","ci_upper_adjusted")) |> 
      dplyr::mutate(ci_lower = uni_compare_object$data$ci_lower_adjusted,
             ci_upper = uni_compare_object$data$ci_upper_adjusted) |> 
      dplyr::select(-"ci_lower_adjusted",-"ci_upper_adjusted")|> 
      tibble::tibble() |> 
      dplyr::filter(sample==i)
  
  very_low_neg<-paste0("<-",format(0,nsmall=ndigits))
  very_low_pos<-paste0(">",format(0,nsmall=ndigits))
  
  uni_compare_object<- uni_compare_object|> 
    dplyr::transmute(Variables=uni_compare_object$varnames,
              t_vec=as.character(format(round(uni_compare_object$t_vec,ndigits), nsmall=ndigits)),
              ci_lower=uni_compare_object$ci_lower,
              ci_upper=uni_compare_object$ci_upper,
              ci_lower=ifelse(uni_compare_object$ci_lower!=0 & round(uni_compare_object$ci_lower,ndigits)==0,
                              ifelse(uni_compare_object$ci_lower<0,very_low_neg,very_low_pos),
                              format(round(uni_compare_object$ci_lower,ndigits), nsmall=ndigits)),
              ci_upper=ifelse(uni_compare_object$ci_upper!=0 & round(uni_compare_object$ci_upper,ndigits)==0,
                              ifelse(uni_compare_object$ci_upper<0,very_low_neg,very_low_pos),
                              format(round(uni_compare_object$ci_upper,ndigits), nsmall=ndigits)))
  
  uni_compare_object<- uni_compare_object|>
    dplyr::mutate(cis = paste0("(",uni_compare_object$ci_lower,", ",uni_compare_object$ci_upper,")")) |> 
    dplyr::select(-"ci_lower",-"ci_upper")
  
  if(isFALSE(ci_line)){
    uni_compare_object<- uni_compare_object |> 
      dplyr::transmute(Variables=uni_compare_object$Variables,
                       value= paste(uni_compare_object$t_vec,uni_compare_object$cis))
  }
  
  if(isTRUE(ci_line)) {
  
    uni_compare_object<- uni_compare_object |> 
      tidyr::pivot_longer(-"Variables") 
    
    uni_compare_object<- uni_compare_object |>
      dplyr::mutate(name=factor(uni_compare_object$name, levels= c("t_vec","cis")))}
  
  
  uni_compare_object
  
}


uni_compare_table2<-function(uni_compare_object, conf_adjustment=FALSE,names=NULL){
  
  
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
      
      data<-merge(data,data_new,by.x=c("varnames",paste("measure",1,sep="")),by.y=c("varnames",paste("measure",i,sep="")), all=TRUE)
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
  
  data<-data[is.na(data$measure1)==FALSE,]
  data$varnames[seq(2, nrow(data), 2)]<-""
  data$measure1<-NULL
  colnames(data)<-c("variables", unique(uni_compare_object$data$name_dfs))
  data<-as.matrix(data)
  
  ### add summetric ###
  if(is.null(uni_compare_object$summet)==FALSE){
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
  
  if(is.null(names)==FALSE) colnames(data)<-c("variables",names)
  if(is.null(names)==TRUE) colnames(data)[1]<-c("variables")
  
  return(data)
}









### Bivariate output table ###

#' Returns a table based on the information of a \code{biv_compare_object} which 
#' can be outputted as HTML or LaTex Table, for example with the help of the 
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
#' 
#' data("card")
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("card","card"),
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
    
    r_data[is.na(p_values)==FALSE]<-format(round(r_data[is.na(p_values)==FALSE],digits = ndigits),nsmall=ndigits)
    r_data[is.na(p_values)]<-""
    r_data[p_values<0.001 & is.na(r_data)==FALSE & is.na(p_values)==FALSE]<-
      paste0(r_data[p_values<0.001 & is.na(r_data)==FALSE & is.na(p_values)==FALSE],"***")
    r_data[p_values<0.01 & p_values>=0.001 & is.na(r_data)==FALSE & is.na(p_values)==FALSE]<-
      paste0(r_data[p_values<0.01 & p_values>=0.001  & is.na(r_data)==FALSE & is.na(p_values)==FALSE],"** ")
    r_data[p_values<0.05 & p_values>=0.01 & is.na(r_data)==FALSE & is.na(p_values)==FALSE]<-
      paste0(r_data[p_values<0.05 & p_values>=0.01  & is.na(r_data)==FALSE & is.na(p_values)==FALSE],"*  ")
    r_data[p_values>=0.05 & is.na(r_data)==FALSE & is.na(p_values)==FALSE]<-
      paste0(r_data[p_values>=0.05 & is.na(r_data)==FALSE & is.na(p_values)==FALSE],"   ")
    
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
#   cor_matrix_df[[6]][cor_matrix_df[[3]]>0.05 & is.na(cor_matrix_df[[3]])==FALSE]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]>0.05 & is.na(cor_matrix_df[[3]])==FALSE], "   ", sep = "")
#   cor_matrix_df[[6]][cor_matrix_df[[3]]<0.05 & cor_matrix_df[[3]]>=0.01 & is.na(cor_matrix_df[[3]])==FALSE]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]<0.05 & cor_matrix_df[[3]]>=0.01 & is.na(cor_matrix_df[[3]])==FALSE], "*  ", sep = "")
#   cor_matrix_df[[6]][cor_matrix_df[[3]]<0.01 & cor_matrix_df[[3]]>=0.001 & is.na(cor_matrix_df[[3]])==FALSE]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]<0.01 & cor_matrix_df[[3]]>=0.001 & is.na(cor_matrix_df[[3]])==FALSE], "** ", sep = "")
#   cor_matrix_df[[6]][cor_matrix_df[[3]]<0.001 & is.na(cor_matrix_df[[3]])==FALSE]<-paste("'",cor_matrix_df[[1]][cor_matrix_df[[3]]<0.001 & is.na(cor_matrix_df[[3]])==FALSE], "***", sep = "")
#   
#   cor_matrix_df[[7]][cor_matrix_df[[5]]>0.05 & is.na(cor_matrix_df[[5]])==FALSE]<-paste("'",round((bench$r[cor_matrix_df[[5]]>0.05 & is.na(cor_matrix_df[[5]])==FALSE] - cor_matrix_df[[1]][cor_matrix_df[[5]]>0.05 & is.na(cor_matrix_df[[5]])==FALSE]), 3) , "   ", sep = "")
#   cor_matrix_df[[7]][cor_matrix_df[[5]]<0.05 & cor_matrix_df[[5]]>=0.01 & is.na(cor_matrix_df[[5]])==FALSE]<-paste("'",round((bench$r[cor_matrix_df[[5]]<0.05 & cor_matrix_df[[5]]>=0.01 & is.na(cor_matrix_df[[5]])==FALSE]- cor_matrix_df[[1]][cor_matrix_df[[5]]<0.05 & cor_matrix_df[[5]]>=0.01 & is.na(cor_matrix_df[[5]])==FALSE]),3), "*  ", sep = "")
#   cor_matrix_df[[7]][cor_matrix_df[[5]]<0.01 & cor_matrix_df[[5]]>=0.001 & is.na(cor_matrix_df[[5]])==FALSE]<-paste("'",round((bench$r[cor_matrix_df[[5]]<0.01 & cor_matrix_df[[5]]>=0.001 & is.na(cor_matrix_df[[5]])==FALSE] - cor_matrix_df[[1]][cor_matrix_df[[5]]<0.01 & cor_matrix_df[[5]]>=0.001 & is.na(cor_matrix_df[[5]])==FALSE] ), 3), "** ", sep = "")
#   cor_matrix_df[[7]][cor_matrix_df[[5]]<0.001 & is.na(cor_matrix_df[[5]])==FALSE]<-paste("'",round((bench$r[cor_matrix_df[[5]]<0.001 & is.na(cor_matrix_df[[5]])==FALSE] - cor_matrix_df[[1]][cor_matrix_df[[5]]<0.001 & is.na(cor_matrix_df[[5]])==FALSE] ) ,3), "***", sep = "")
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
#   #if (is.null(varlabels)==FALSE) colnames(shortlist[[1]])<-varlabels
#   if (is.null(varlabels)==FALSE) rownames(shortlist[[1]])<-varlabels
#   #if (is.null(varlabels)==FALSE) colnames(shortlist[[2]])<-varlabels
#   if (is.null(varlabels)==FALSE) rownames(shortlist[[2]])<-varlabels
#   
#   if (is.null(all)==FALSE) return(cor_matrix_df)
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
#' 
#' data("card")
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data
#' multi_data1 <- sampcompR::multi_compare(df = north, 
#'                                          bench = card,
#'                                          independent = c("age","fatheduc","motheduc","IQ"),
#'                                          dependent = c("educ","wage"),
#'                                          family = "ols") 
#'                                      
#' multi_data2 <- sampcompR::multi_compare(df = white, 
#'                                          bench = card,
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
  
  if(is.null(names)) table_list<-mapply(get_stars, lapply(multi_compare_objects,get, envir = envir), type=type, SIMPLIFY = FALSE)
  if(is.null(names)==FALSE) table_list<-mapply(get_stars, lapply(multi_compare_objects,get, envir = envir), names, type=type ,SIMPLIFY = FALSE)

  as.matrix(dplyr::bind_rows(table_list),nrow = nrow(get(multi_compare_objects[1])$independent)*length(multi_compare_objects),envir = envir)
  
}










descript_table_sub<-function(df, variables, varlabels=NULL, weight=NULL,strata=NULL,id=NULL, value="mean", digits=NULL){
  
    design <- survey::svydesign(ids = id, weights = weight, strata = strata, data=df)
    
    if (value=="mean"){
      output<-sapply(variables,
                     function(variable,design){survey::svymean(stats::reformulate(variable),design,na.rm=TRUE)}, 
                     design, simplify = TRUE)
      
      output<-matrix(round(output,digits=digits),ncol=1)
    }
    
    if (value=="percent"){
      output<-sapply(variables,
                     function(variable,design){survey::svymean(stats::reformulate(variable),design,na.rm=TRUE)}, 
                     design, simplify = TRUE)
      
      output<-paste(round((output*100), digits = digits),"%", sep="")
      
      output<-matrix(output,ncol=1)
    }
    
    if (value=="total"){
      output<-sapply(variables,
                     function(variable,design){survey::svytotal(stats::reformulate(variable),design,na.rm=TRUE)}, 
                     design, simplify = TRUE)
      
      output<-matrix(round(output,digits = digits),ncol=1)
      
    }
    
    if (value=="total_percent"){
      output1<-sapply(variables,
                     function(variable,design){survey::svytotal(stats::reformulate(variable),design,na.rm=TRUE)}, 
                     design, simplify = TRUE)
      
      output2<-sapply(variables,
                     function(variable,design){survey::svymean(stats::reformulate(variable),design,na.rm=TRUE)}, 
                     design, simplify = TRUE)
      
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
    
    if(is.null(weight)==FALSE) {
      if(is.na(weight[i])==FALSE) weights<-get(dfs[i])[,weight[i]]/(sum(get(dfs[i])[,weight[i]])/nrow(get(dfs[i])))
      if(is.na(weight[i])==TRUE) weights<-rep(1,nrow(get(dfs[i])))}
    if(is.null(strata)==FALSE) {
      if(is.na(strata[i])==FALSE) stratas<-get(dfs[i])[,strata[i]]
      if(is.na(strata[i])==TRUE) stratas<-NULL}
    if(is.null(id)==FALSE) {
      if(is.na(id[i])==FALSE) ids<-get(dfs[i])[,id[i]]
      if(is.na(id[i])==TRUE) ids<-c(1:nrow(get(dfs[i])))}
    
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


###################################
### Bias per Variable bivariate ###
###################################

#' Returns a table based on the information of a \code{biv_compare_object} that 
#' indicates the proportion of biased variables. It can be outputted as HTML or 
#' LaTex Table, for example with the help of the \link[stargazer]{stargazer} 
#' function.
#' 
#' @param biv_compare_object A object returned by the 
#' \code{\link[sampcompR]{biv_compare}} function.
#' @param ndigits Number of digits that is shown in the table.
#' @param varlabels A character vector containing labels for the variables.
#' @param label_df A character vector containing labels for the data frames.
#'
#' @return A matrix, that indicates the proportion of bias for every individual
#' variable. This is given separately for every comparison, as well as averaged
#' over comparisons. 
#' 
#' @examples
#' 
#' data("card")
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("card","card"),
#'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                    data=TRUE)
#' 
#' table<-sampcompR::biv_per_variable(bivar_data)
#' noquote(table)
#' 
#' @export

biv_per_variable<-function(biv_compare_object, ndigits = 1,varlabels=NULL,label_df=NULL){
  
  main_object<-biv_compare_object$comparison_dataframe |> 
    dplyr::filter(is.na(biv_compare_object$comparison_dataframe$value)==F) |> 
    dplyr::mutate(different=dplyr::case_when(value=="Same"~0,
                                             value=="X"~NA,
                                             value!="Same" & value!="X"~1))
  
  
  if(is.null(biv_compare_object$varlabels)){
    variables<-biv_compare_object$variables}
  
  if(is.null(biv_compare_object$varlabels)==F){
    variables<-biv_compare_object$varlabels}
  
  
  bias_per_variable_sub<-function(main_object,variable,sample=NULL){
    
    if(is.null(sample)){
      different<-main_object$different[main_object$x==variable| 
                                         main_object$y==variable]}
    
    if(is.null(sample)==F){
      different<-main_object$different[(main_object$x==variable|
                                          main_object$y==variable) &
                                         main_object$samp==sample]
    }
    
    mean(different,na.rm = T)
  }

  
  
    samples<-unique(main_object$samp)
    bias<-list()
    for (i in 1:length(samples)){
      bias[[i]]<-purrr::map_dbl(variables,~bias_per_variable_sub(main_object,.,
                                                          sample = samples[i]))}
    bias<-purrr::map(bias,function(bias,variables)
      {names(bias)<-variables
      bias},
    variables=variables)
    bias<-matrix(unlist(bias),ncol=length(bias))
    bias<-cbind(bias,purrr::map_dbl(variables,
                             ~bias_per_variable_sub(main_object,.)))
    #bias<-cbind(bias,map_dbl(1:length(variables),~mean(bias[.x,],na.rm=T)))
    bias<-format(round(x=bias*100,digits = ndigits),nsmall = ndigits)
    bias[bias!=" NaN"]<-paste0(bias[bias!=" NaN"],"%")
    bias[bias==" NaN"]<-"-"
    
    if(is.null(varlabels)==FALSE){
      l_varlabels<-ifelse(length(variables)<=length(varlabels),length(variables),length(varlabels))
      variables<-as.character(variables)
      variables[1:l_varlabels]<-varlabels[1:l_varlabels]
    }
     
    bias<-cbind(as.character(variables),bias)
    if(is.null(biv_compare_object$plots_label)==FALSE) {
      if(is.null(label_df)==FALSE){
        plots_label<- biv_compare_object$plots_label
        l_label_df<-ifelse(length(plots_label)<=length(label_df),length(plots_label),length(label_df))
        biv_compare_object$plots_label[1:l_label_df]<-label_df[1:l_label_df]
      }
      
      colnames(bias)<-c("Variables",unique(biv_compare_object$plots_label),"Average Bias")}
    
    if(is.null(biv_compare_object$plots_label)){
      if(is.null(label_df)==FALSE){
        dfs<-biv_compare_object$dfs
        l_label_df<-ifelse(length(dfs)<=length(label_df),length(dfs),length(label_df))
        biv_compare_object$dfs[1:l_label_df]<-label_df[1:l_label_df]
      }
      colnames(bias)<-c("Variables",unique(biv_compare_object$dfs),"Average Bias")}
    return(bias) 
  
}







######################################
### Bias per Variable multivariate ###
######################################

#' Returns a table based on the information of a \code{multi_compare_object} that 
#' indicates the proportion of biased variables. It can be outputted as HTML or 
#' LaTex Table, for example with the help of the \link[stargazer]{stargazer} 
#' function.
#' 
#' @param multi_compare_objects A object returned by the 
#' \code{\link[sampcompR]{multi_compare}} function. Object can either be 
#' inserted as single object or as a character string containing the names of
#' multiple objects.
#' @param ndigits Number of digits that is shown in the table.
#' @param lables_coefs A character vector containing labels for the 
#' coefficients.
#' @param lables_models A character vector containing labels for the 
#' models.
#' @param label_df A character vector containing labels for the data frames.
#' @param type The \code{type} of table, can either be \code{"coefs"}, 
#' \code{"models"}, or \code{"complete"}. When coefs is chosen, the average bias 
#' of the coefficients is outputted, when models is chosen, the average bias 
#' for the models is outputted, and when complete is chosen, both are outputted.
#'
#' @return A matrix, that indicates the proportion of bias for every individual
#' coefficient or model for multivariate comparisons. This is given separately 
#' for every comparison, as well as averaged over comparisons. 
#' 
#' @examples
#' 
#' data("card")
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data
#' multi_data1 <- sampcompR::multi_compare(df = north, 
#'                                         bench = card,
#'                                         independent = c("age","fatheduc","motheduc","IQ"),
#'                                         dependent = c("educ","wage"),
#'                                         family = "ols") 
#'
#' multi_data2 <- sampcompR::multi_compare(df = white, 
#'                                         bench = card,
#'                                         independent = c("age","fatheduc","motheduc","IQ"),
#'                                         dependent = c("educ","wage"),
#'                                         family = "ols") 
#' 
#' table<-sampcompR::multi_per_variable(multi_compare_objects = c("multi_data1","multi_data2"))
#' noquote(table)
#' 
#' @export

multi_per_variable<-function(multi_compare_objects,
                             type = "coefs",
                             label_df = NULL,
                             lables_coefs = NULL,
                             lables_models = NULL,
                             ndigits=1){
  
multi_per_variable_single<-function(multi_compare_objects,
                             type = "coefs",
                             label_df = NULL,
                             lables_coefs = NULL,
                             lables_models = NULL,
                             ndigits=1){
  ### Function tocalculate differences matrices 
  multi_same_func<-function(multi_compare_object,p_adjust=T){
    
    
    multi_compare_object<-get(multi_compare_object)
    
    p_adjust=multi_compare_object$p_adjust
    
    if (p_adjust==FALSE){
      sample_diff<-multi_compare_object$P_coefs_difference
      p_df<-multi_compare_object$P_coefs1
      p_benchmark<-multi_compare_object$P_coefs2
    }
    
    if (p_adjust==TRUE|is.character(p_adjust)){
      sample_diff<-multi_compare_object$p_diff_adjusted
      p_df<-multi_compare_object$P_coefs1
      p_benchmark<-multi_compare_object$P_coefs2
    }
    
    b_df<-multi_compare_object$coefs_data1
    b_benchmark<-multi_compare_object$coefs_data2
    
    independent<-multi_compare_object$independent
    dependent<-multi_compare_object$dependent
    breaks<-c("Same", "Small Diff","Large Diff")
    p_value<-0.05
    
    comp_matrix<-sample_diff
    colnames(comp_matrix)<-dependent
    comp_matrix[sample_diff > p_value & !is.na(sample_diff)] <- 0
    comp_matrix[sample_diff < p_value & (p_df > p_value & p_benchmark > p_value) & !is.na(sample_diff)] <- 0
    comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) & !is.na(sample_diff)] <- 1
    comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) &
                  (abs(b_df) > 2*abs(b_benchmark) |  2*abs(b_df) < abs(b_benchmark)) &
                  ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0)) & !is.na(sample_diff)] <- 1
    comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) &
                  ((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) & !is.na(sample_diff)] <- 1
    
    comp_matrix
  }
  
  ### runction tocalculate differences matrices
  if(inherits(multi_compare_objects,"character")){
  bias_table <- purrr::map(multi_compare_objects,~multi_same_func(multi_compare_object=.x,
                                                      p_adjust=p_adjust))}
  
  if(inherits(multi_compare_objects,"list")){
    multi_compare_object<-deparse(substitute(multi_compare_objects))
    
    bias_table <- purrr::map(multi_compare_object,~multi_same_func(multi_compare_object=.x,
                                                                    p_adjust=p_adjust))
    }
  
  
  ### get row and collumn label lists
  rows<-purrr::map(bias_table,~rownames(.x))
  cols<-purrr::map(bias_table,~colnames(.x))
  
  
  #turn all matrices in dataframes, add a sample indicator and combine them
  bias_table2<-purrr::map(bias_table,~reshape2::melt(.x))
  bias_table2<-purrr::map(1:length(bias_table2),~dplyr::mutate(bias_table2[[.x]],samp=.x))
  bias_df <- do.call("rbind", bias_table2)
  
  if(type=="df") return(bias_df)
  
  ## bias per variable function ##
  bias_per_var_table_multi<-function(bias_list,rows=NULL,cols=NULL,type="coefs",ndigits=1){
    
    if(type=="coefs"){
      out<-purrr::map_dbl(rows,~mean(bias_list[.x,],na.rm=T))*100
      out<-format(round(out,ndigits),nsmall=ndigits)
      out<-paste0(out,"%")
      names(out)<-rows
      
    }
    
    if(type=="models"){
      out<-purrr::map_dbl(cols,~mean(bias_list[,.x],na.rm=T))*100
      out<-format(round(out,ndigits),nsmall=ndigits)
      out<-paste0(out,"%")
      names(out)<-cols
    }
    return(out)
  }
  ### get bias per variable for models and coefs
  if(type=="coefs") by_samp_list<-purrr::map2(bias_table,rows,~bias_per_var_table_multi(.x,rows=.y,type="coefs", ndigits=ndigits))
  if(type=="models") by_samp_list<-purrr::map2(bias_table,cols,~bias_per_var_table_multi(.x, cols=.y, type="models", ndigits=ndigits))
  
  
  ### calculate average bias for coefs and models
  avg_bias_func<-function(bias_df,variable,type="coefs",ndigits=1){
    if(type=="coefs") out<-mean(bias_df[bias_df$Var1 %in% variable,]$value,na.rm=T)*100
    if(type=="models")out<-mean(bias_df[bias_df$Var2 %in% variable,]$value,na.rm=T)*100
    
    out<-format(round(out,ndigits),nsmall=ndigits)
    out<-paste0(out,"%")
    out
  }
  
  # get all rows and collumns
  rows_unique<-unique(unlist(rows))
  cols_unique<-unique(unlist(cols))
  
  # use the average_bias function
  if(type=="coefs") {
    avg_bias<-purrr::map_chr(rows_unique,~avg_bias_func(bias_df,.x,ndigits=ndigits))
    names(avg_bias)<-rows_unique
  }
  if(type=="models") {
    avg_bias<-purrr::map_chr(cols_unique,~avg_bias_func(bias_df,.x,type="models",
                                                 ndigits=ndigits))
    names(avg_bias)<-cols_unique
  }
  if(type=="complete"){}
  
  
  # put the average bias in the previous list
  by_samp_list[[length(by_samp_list)+1]]<-avg_bias
  
  # turn it into a matrix
  by_samp_list<-by_samp_list %>% 
    purrr::transpose() 
  
  by_samp_list<-do.call("rbind", by_samp_list)
  
  by_samp_list[by_samp_list %in% "NULL"]<-""
  
  if(inherits(multi_compare_objects,"character")) out<-as.matrix(by_samp_list,ncol=(length(multi_compare_objects)+1))
  if(inherits(multi_compare_objects,"list")) out<-as.matrix(by_samp_list,ncol=(1+1))
  
  
  ### Label the matrix 
  if(type=="coefs"){
    if(is.null(lables_coefs)==TRUE) rownames(out)<-rows_unique
    if(is.null(lables_coefs)==FALSE){
      l_coefs<-ifelse(length(rows_unique)<=length(lables_coefs),length(rows_unique),length(lables_coefs))
      coefs<-as.character(rows_unique)
      coefs[1:l_coefs]<-lables_coefs[1:l_coefs]
      rownames(out)<-coefs}}
  
  if(type=="models"){
    if(is.null(lables_models)==TRUE) rownames(out)<-cols_unique
    if(is.null(lables_models)==FALSE){
      l_models<-ifelse(length(cols_unique)<=length(lables_models),length(cols_unique),length(lables_models))
      models<-as.character(cols_unique)
      models[1:l_models]<-lables_models[1:l_models]
      rownames(out)<-models}}
  
  if(inherits(multi_compare_objects,"character")){
  if(is.null(label_df)==TRUE) colnames(out)<-c(multi_compare_objects,"Average Bias")
  if(is.null(label_df)==FALSE){
    l_df<-ifelse(length(multi_compare_objects)<=length(label_df),length(multi_compare_objects),length(label_df))
    lab_df<-as.character(multi_compare_objects)
    lab_df[1:l_df]<-label_df[1:l_df]
    
    colnames(out)<-c(lab_df,"Average Bias")}}
  
  if(inherits(multi_compare_objects,"list")){
    
    if(is.null(label_df)==TRUE) colnames(out)<-c(multi_compare_object,"Average Bias")
    if(is.null(label_df)==FALSE){
      l_df<-ifelse(length(multi_compare_object)<=length(label_df),length(multi_compare_object),length(label_df))
      lab_df<-as.character(multi_compare_object)
      lab_df[1:l_df]<-label_df[1:l_df]
      
      colnames(out)<-c(lab_df,"Average Bias")}}
  
  out
}

multi_df<-multi_per_variable_single(multi_compare_objects = 
                                      multi_compare_objects,
                                    type = "df",
                                    label_df = label_df,
                                    lables_coefs = lables_coefs,
                                    ndigits = ndigits)

multi_coefs<-multi_per_variable_single(multi_compare_objects = 
                                         multi_compare_objects,
                          type = "coefs",
                          label_df = label_df,
                          lables_coefs = lables_coefs,
                          ndigits = ndigits)

multi_models<-multi_per_variable_single(multi_compare_objects = 
                                          multi_compare_objects,
                                       type = "models",
                                       lables_models = lables_models,
                                       ndigits = ndigits)

if(type== "coefs") out<-multi_coefs
if(type== "models") out<-multi_models
if(type== "df") out<- multi_df

if(type=="complete"){
  
  by_coefficients<-matrix(c(rep("",ncol(multi_coefs))),ncol=ncol(multi_coefs))
  rownames(by_coefficients)<-"By Coefficients"
  by_models<-matrix(c(rep("",ncol(multi_models))),ncol=ncol(multi_models))
  rownames(by_models)<-"By Models"
  
  out<-rbind(by_coefficients,multi_coefs,by_models,multi_models)
  
}

out
}










###########################################
### Average Bias per Variable bivariate ###
###########################################

#' Returns a table based on the information of a \code{biv_compare_object} that 
#' indicates the Average Absolute Bias (AARB) in Pearson's r or the Average Absolute 
#' Relative Bias (AARB) in Pearson's r for every data frame It can be outputted as HTML or 
#' LaTex Table, for example with the help of the \link[stargazer]{stargazer} 
#' function.
#' 
#' @param biv_compare_object A object returned by the 
#' \code{\link[sampcompR]{biv_compare}} function.
#' @param ndigits Number of digits that is shown in the table.
#' @param varlabels A character vector containing labels for the variables.
#' @param label_df A character vector containing labels for the data frames.
#' @param type A character string, which is \code{"AAB"} if the Average Absolute 
#' Bias per variable should be displayed in the table, or "AARB" if the Average Absolute 
#' Relative Bias per Variable should be displayed in the table.
#' @param final_col A character string, indicating if the last column of the table
#' should display an average bias per variable of over all data frames (\code{"average"}),
#' or the difference between the first and the average bias of the first and the last
#' data frame (\code{"difference"}).
#' @return A matrix, that shows the Average Absolute Bias (AAB) or the Average 
#' Absolute Relative Bias (AARB) for every individual variable. 
#' This is given separately for every comparison data frame, as well as averaged
#' over comparisons, or as the difference between the first and the last comparison. 
#' 
#' @examples
#' 
#' data("card")
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("card","card"),
#'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                    data=TRUE)
#' 
#' table1<-sampcompR::biv_bias_per_variable(bivar_data,type="rel_diff",
#'                                          final_col="average",ndigits=2)
#' noquote(table1)
#' 
#' table2<-sampcompR::biv_bias_per_variable(bivar_data,type = "diff",
#'                                          final_col="difference",ndigits=2)
#' noquote(table2)
#' 
#' @importFrom rlang :=
#' @export
biv_bias_per_variable<-function(biv_compare_object, type="rel_diff",
                                final_col="difference",ndigits=3,
                                varlabels=NULL,
                                label_df=NULL){
  samp_name<-NULL
  x<-NULL
  y<-NULL
  new_diff<-NULL
  
  vars<-as.character(biv_compare_object$varlabels)
  
  if(type=="rel_diff") biv_compare_object[[1]]$new_diff<-abs(as.numeric(biv_compare_object[[1]]$abs_rel_difference_r))
  if(type=="diff") biv_compare_object[[1]]$new_diff<-abs(as.numeric(biv_compare_object[[1]]$difference_r))
  
  output<-purrr::map_df(vars,~biv_bias_per_var_sub(biv_compare_object,.x))
  output2<-purrr::map_dbl(vars,~biv_bias_per_var_sub2(biv_compare_object,.x))
  
  
  if (final_col=="difference") output$"Bias Difference"<-output[,ncol(output)]-output[,1]
  if (final_col=="average") output$"Average Bias"<-output2
  output<-format(round(output,digits=ndigits),nsmall=ndigits)
  output<-cbind(vars,output)
  rownames(output)<-NULL
  colnames(output)[1]<-"Variables"
  
  ### add General AARB and Rank ###
  output3<-biv_compare_object[[1]] %>% 
    dplyr::mutate(samp_name=factor(samp_name,levels=unique(samp_name))) %>%
    dplyr::group_by(samp_name) %>%
    dplyr::summarise(mean=mean(new_diff, na.rm=T)) %>% 
    dplyr::select(samp_name,mean)
  
  if(final_col=="difference") output4<-output3$mean[nrow(output3)]-output3$mean[1]
  
  if(final_col=="average") {
    output4<-biv_compare_object[[1]] %>% 
      dplyr::summarise(mean=mean(new_diff, na.rm=T))
    output4<-output4$mean
  }
  

  
  output3$mean<-format(round(output3$mean,digits=ndigits),nsmall=ndigits)
  output4<-format(round(output4,digits=ndigits),nsmall=ndigits)
  
  ### Change Labels of variables 
  
  if(is.null(varlabels)==F){
    if(length(varlabels)<length(output[,1])){
      output[1:length(varlabels),1]<-varlabels
    }
    if(length(varlabels)>length(output[,1])){
      output[,1]<-varlabels[1:length(output[,1])]
    }
    if(length(varlabels)==length(output[,1])){
      output[,1]<-varlabels
    }
  }
  
  ### Change Names of Dataframes 
  
  if(is.null(label_df)==F){
    col_length<-length(colnames(output)[2:(ncol(output)-1)])
    if(length(label_df)<col_length){
      colnames(output)[2:length(label_df)]<-label_df
    }
    if(length(label_df)>col_length){
      colnames(output)[2:(col_length+1)]<-label_df[1:col_length]
    }
    if(length(label_df)==col_length){
      colnames(output)[2:(col_length+1)]<-label_df
    }
  }
  
  output<-rbind(output,c("Total Average Bias ",output3$mean,output4))
  output<-rbind(output,c("RANK",paste(rank(as.numeric(output3$mean))),"-"))
  
  output
}



biv_bias_per_var_sub<-function(biv_compare_object,var){
  
  samp_name<-NULL
  x<-NULL
  y<-NULL
  new_diff<-NULL
  
  dep_option<-options()$dplyr.summarise.inform
  options(dplyr.summarise.inform = FALSE)
  out<-biv_compare_object[[1]] %>% 
    dplyr::mutate(samp_name=factor(samp_name,levels=unique(samp_name)),
                  !!var:=ifelse(x==var|y==var,1,0)) %>%
    dplyr::group_by(samp_name,dplyr::across(dplyr::all_of(var))) %>%
    dplyr::summarise(mean=mean(new_diff, na.rm=T))
  
  out<-suppressWarnings(dplyr::filter(out,dplyr::across(dplyr::all_of(var))==1) %>% 
                            dplyr::select(samp_name,mean))
 
  
  options(dplyr.summarise.inform = dep_option)
  out<-data.frame(mean=out$mean)
  rownames(out)<-unique(biv_compare_object[[1]]$samp_name)
  out<-as.data.frame(t(out))
  out
}

biv_bias_per_var_sub2<-function(biv_compare_object,var){
  
  samp_name<-NULL
  x<-NULL
  y<-NULL
  new_diff<-NULL
  
  out<-biv_compare_object[[1]] %>% 
    dplyr::mutate(samp_name=factor(samp_name,levels=unique(samp_name)),
                  !!var:=ifelse(x==var|y==var,1,0)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(var))) %>%
    dplyr::summarise(mean=mean(new_diff, na.rm=T))
  
  out<-suppressWarnings(dplyr::filter(out,dplyr::across(dplyr::all_of(var))==1) %>% 
                          dplyr::select(mean))
  
  out$mean
  
}











#####################
### Missing Table ###
#####################

#' Returns a Table indicating the number and proportion of NA values for a selected
#' set of variables.
#'
#' @param dfs A character vector with names of data frames for which the 
#' missings per variable should be displayed.
#' @param variables A character vector of variable names for which the missings 
#' should be displyed.
#' @param df_names Either Null or a character vector of names, to relabel the 
#' data frames in the table with.
#' @param varlabels Either Null, or a character vector of variable names, to 
#' relabel the variables in the table with.
#' @return Returns a Table indicating the number and proportion of NA values for 
#' a selected set of variables. This can be used to get an overview of the data, 
#' detect errors after data rangeling, or find items in a survey, with especially,
#' high item nonresponse.
#' 
#' @examples
#' ## Get Data for comparison
#'
#' data("card")
#'
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#'
#' variables<- c("age","educ","fatheduc","motheduc","wage","IQ")
#' varlabels<-c("Age","Education","Father's Education",
#'              "Mother's Education","Wage","IQ")
#'
#' missing_tab<-sampcompR::missing_table(dfs = c("north","white"),
#'                                       variables=variables,
#'                                       df_names = c("North","White"),
#'                                       varlabels=varlabels)
#' 
#' missing_tab
#' 
#' 
#' 
#' @export
#'
missing_table <- function(dfs, variables, df_names=NULL, varlabels = NULL) {
  
  # Convert dfs into a list
  df_list <- purrr::map(dfs, get)
  
  # Apply the 'missing_table_sub' function to each dataframe and store the results
  result_list <- purrr::map2(.x = df_list, .y = dfs, missing_table_sub, variables = variables)
  
  # Initialize the 'results' dataframe with the first result
  results <- result_list[[1]]
  
  # Merge the results of other dataframes using a loop
  if(length(result_list)>=2){
    for (i in 2:length(result_list)) {
      results <- dplyr::full_join(results, result_list[[i]], by = "Variables")
    }}
  
  # Remove the 'Variables' column from the final result
  results <- results %>% dplyr::select(-"Variables")
  
  # Add a row at the end with the number of rows (N) for each dataframe
  results <- results %>%
    rbind(c(purrr::map(df_list, nrow)))
  
  # Set row names to 'variables', 'Max', and 'N'
  rownames(results) <- c(variables, "Max", "N")
  
  # If 'varlabels' is provided, set row names to 'varlabels' instead
  if (!is.null(varlabels)) {
    rownames(results) <- c(varlabels, "Max", "N")
  }
  
  # If 'df_names' is provided, set column names to 'df_names'
  if (!is.null(df_names)) {
    colnames(results) <- df_names
  }
  
  # Replace NA values with "NA"
  results[is.na(results)] <- "NA"
  
  results
}



# Define a function called missing_table_sub
missing_table_sub <- function(dataframe, df_name, variables) {
  
  # Filter the 'variables' vector to include only those that are column names in 'dataframe'
  variables <- variables[variables %in% colnames(dataframe)]
  
  # Calculate the count of missing values for selected variables in 'dataframe'
  out <- dataframe %>%
    dplyr::select(variables) %>%
    dplyr::summarise(dplyr::across(variables, ~sum(is.na(.)))) %>% 
    t() %>% 
    as.data.frame()
  
  # Add a row at the end containing the maximum missing value count
  out <- rbind(out, max(out$V1))
  
  V1<-NULL
  # Modify the result dataframe to include percentages and rename columns
  out <- out %>%
    dplyr::mutate(V1 = paste0(V1, " (", round(V1 / (nrow(dataframe)) * 100, digits = 2), "%)"),
           variables = c(variables, "Max")) %>%
    stats::setNames(c(df_name, "Variables"))
  
  return(out)
}