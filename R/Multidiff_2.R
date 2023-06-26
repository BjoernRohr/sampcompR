
###################################################
### 1 Function to reduce df for seperate models ###
###################################################

reduce_df_glm <- function (df, formula_list, rm_na="pairwise", 
                           weight_var = NULL, id = NULL, strata=NULL){
  
  df_list<-list()
  
  ### get all needed variables ###
  var_list<-lapply(formula_list, all.vars)
  unique_vars <- unique(unlist(lapply(formula_list, all.vars)))
  
  
  if (is.null(weight_var)==F) weighting<-c(weight_var,id)
  if (is.null(strata)==F) weighting<-c(weighting,strata)
  
  
  if (is.null(weight_var)) df<-subset(df, select=c(unique_vars))
  if (is.null(weight_var)==F) df<-subset(df, select=c(unique_vars,weighting))
  #reduce df to dependent or independent variables
  if(rm_na=="listwise") df<-stats::na.omit(df)
  
  #reduce df to the model
  for (i in 1:length(formula_list)){
    if (is.null(weight_var)) dataframe<-subset(df,select = c(var_list[[i]]))
    if (is.null(weight_var)==F) dataframe<-subset(df,select = c(var_list[[i]], weighting))
    if (is.null(weight_var)==F) names(dataframe)[names(dataframe)==weight_var]<-"df_weights"
    if (is.null(weight_var)==F & is.null(id)==F) names(dataframe)[names(dataframe)==id]<-"id_df"
    if (is.null(weight_var)==F & is.null(strata)==F) names(dataframe)[names(dataframe)==strata]<-"strata_df"
    if (rm_na=="pairwise") dataframe<-stats::na.omit(dataframe)
    df_list[[i]]<-dataframe
    }
  
  return(df_list)
}

######################################################
### 1.5 function to combine both data frames to one ###
######################################################

combine_dfs_glm<-function(df,benchmark,formula_list,id=NULL,id_bench=NULL,weight=NULL,weight_bench=NULL,strata=NULL,strata_bench=NULL){
  
  
  comb_df<-list()
  for (i in 1:length(formula_list)){
    dataframe1<-as.data.frame(df[[i]])
    dataframe1$sample_ident<-0
    dataframe2<-as.data.frame(benchmark[[i]])
    dataframe2$sample_ident<-1
    
    ### normalize the weights if they are present ###
    if (is.null(weight)==F) dataframe1$df_weights<- dataframe1$df_weights/(sum(dataframe1$df_weights)/nrow(dataframe1))
    if (is.null(weight_bench)==F) dataframe2$df_weights<- dataframe2$df_weights/(sum(dataframe2$df_weights)/nrow(dataframe2))

    if ((is.null(weight)==F | is.null(weight_bench)==F) & is.null(id)) dataframe1$id_df<-id_df<-1:nrow(dataframe1)
    if (is.null(weight_bench)==F & is.null(weight)) dataframe1$df_weights<-1

    #comb_data<-rbind(dataframe1,dataframe2)
    if ((is.null(weight)==F | is.null(weight_bench)==F) & is.null(id_bench)) dataframe2$id_df<-id_benchmark<-1:nrow(dataframe2)
    if (is.null(weight)==F & is.null(weight_bench)) dataframe2$df_weights<-1

    comb_data<-rbind(dataframe1,dataframe2)
    if(is.null(weight)==F | is.null(weight_bench)==F) comb_data$id_df[comb_data$sample_ident==1]<-comb_data$id_df[comb_data$sample_ident==1]+max(comb_data$id_df[comb_data$sample_ident==0])
    comb_df[[i]]<-comb_data
  }
  comb_df
  }



######################################
### subfunction to prepare formula ###
######################################

modify_glm_formula <- function(formula) {
  
  
  dependent<-as.character(formula)[[2]]
  independent<-as.character(formula)[[3]]

  
  updated_formula<-paste(dependent,"~ (",independent,")","* sample_ident")
  
  updated_formula<-stats::as.formula(updated_formula)
  return(updated_formula)
}


########################################################
### Subfunc, to make a design list, for weighted glm ###
########################################################

### build survey design for every model ###
weighted_design_glm<-function(data_list,formula_list, weight_var, id, strata=NULL, nest=FALSE, type="ineract"){
  design_list<-list()
  for (i in 1:length (formula_list)){
    if(type=="interact"){
      design_list[[i]]<-survey::svydesign(id = ~get(id),
                                          strata = strata,
                                          weights = ~get(weight_var),
                                          nest = nest,
                                          data = data_list[[i]])}
    
    if(type=="df1"){
      design_list[[i]]<-survey::svydesign(id = ~get(id),
                                          strata = strata,
                                          weights = ~get(weight_var),
                                          nest = nest,
                                          data = data_list[[i]][data_list[[i]]$sample_ident==0,])}
    
    if(type=="bench"){
      design_list[[i]]<-survey::svydesign(id = ~get(id),
                                          strata = strata,
                                          weights = ~get(weight_var),
                                          nest = nest,
                                          data = data_list[[i]][data_list[[i]]$sample_ident==1,])}
  }
  
  return(design_list)
  
}



##############################
### subfunction to run_glm ###
##############################

run_glm<-function(df_comb,formula_list, design_list=NULL, family=stats::gaussian(link = "identity"), type="interact", ...){
  ### get formular for glm regression ###
  
  glm_list<-list()
  for (i in 1:length (formula_list)){
    
    comb_data<-df_comb[[i]]
    
    
    if(type=="interact"){
      
      form<- modify_glm_formula(formula_list[[i]])
      }
    
    
    if (type=="df1" | type=="bench"){
      form<-formula_list[[i]]
    }
    
    
    ### run glm or weighted_glm ###
      if(type=="interact"){
        if(is.null(design_list)) ols_next<- stats::glm(stats::as.formula(form), data = comb_data, family = family, ...)
        
        if(is.null(design_list)==F) ols_next<- survey::svyglm(formula = stats::as.formula(form),design = design_list[[i]], family = family, ...)}
      
      if(type=="df1"){
        if(is.null(design_list)) ols_next<- stats::glm(stats::as.formula(form), data = comb_data[comb_data$sample_ident==0,], family = family, ...)
        
        if(is.null(design_list)==F) ols_next<- survey::svyglm(formula = stats::as.formula(form),design = design_list[[i]], family = family, ...)}
      
      if(type=="bench"){
        if(is.null(design_list)) ols_next<- stats::glm(stats::as.formula(form), data = comb_data[comb_data$sample_ident==1,], family = family, ...)
        
        if(is.null(design_list)==F) ols_next<- survey::svyglm(formula = stats::as.formula(form),design = design_list[[i]], family = family, ...)}
    
    

    
    glm_list[[i]]<-ols_next
    glm_list[[i+length(formula_list)]]<-summary(ols_next)
  }
  
  
  names(glm_list)[1:length(formula_list)]<-paste(names(formula_list),"_model", sep = "")
  names(glm_list)[(length(formula_list)+1):(2*length(formula_list))]<-paste(names(formula_list),"_mod_summ", sep = "")
  
  glm_list
}

###################################################################
### Function to extract interactions for the interaction models ###
###################################################################

extract_interaction_results<-function(glm_model, robust_se=F){
  
  
  if(robust_se == F) model_summary <- summary(glm_model)
  if(robust_se == T) model_summary<- lmtest::coeftest(glm_model, vcov = sandwich::vcovHC(glm_model, type="HC1"))
  
  # Extract the interaction coefficients, standard errors, and p-values
  if(robust_se == F){
    interaction_coeffs <- stats::coef(glm_model)
    interaction_se <- model_summary$coefficients[, "Std. Error"]
    interaction_pvalues <- model_summary$coefficients[, 4]
  }
  
  if(robust_se == T){
    interaction_coeffs <- model_summary[, "Estimate"]
    interaction_se <- model_summary[, "Std. Error"]
    interaction_pvalues <- model_summary[, 4]
  }
  
  # Create a data frame to store the results
  results <- data.frame(Coefficients = interaction_coeffs,
                        Std_Error = interaction_se,
                        P_Value = interaction_pvalues)
  #return(results)
  # Filter only the interaction terms
  results <- results[grep(":sample_ident", rownames(results)), ]
  
  # Print the results
  as.matrix(results)
}


##############################################
### function to prepare the output dataset ###
##############################################



final_glm_list<-function(glm_list, formula_list ,weight_var=NULL,robust_se=F, p_adjust=NULL){
  
  output_list<-list()
  output_list[[1]]<-glm_list
  
  
   for (i in 1:length(formula_list)) {
      
      bmatrix1_help<- as.matrix(stats::coef(glm_list[[2]][[i]])[-1])
      bmatrix2_help<- as.matrix(stats::coef(glm_list[[3]][[i]])[-1])
      bmatrix_diff_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=F)[,1])
      

      colnames(bmatrix1_help)<-names(formula_list)[i]
      colnames(bmatrix2_help)<-names(formula_list)[i]
      colnames(bmatrix_diff_help)<-names(formula_list)[i]
      rownames(bmatrix_diff_help)<-rownames(bmatrix1_help)
      
      
      if (robust_se==F | isTRUE(weight_var)){
        pmatrix_diff_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=F)[,3])
        pmatrix1_help<- as.matrix(summary(glm_list[[2]][[i]])$coefficients[, 4][-1])
        pmatrix2_help<- as.matrix(summary(glm_list[[3]][[i]])$coefficients[, 4][-1])
        se_matrix_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=F)[,2])
        se_matrix1_help<- as.matrix(summary(glm_list[[2]][[i]])$coefficients[, "Std. Error"][-1])
        se_matrix2_help<- as.matrix(summary(glm_list[[3]][[i]])$coefficients[, "Std. Error"][-1])
        
        colnames(pmatrix_diff_help)<-names(formula_list)[i]
        rownames(pmatrix_diff_help)<-rownames(pmatrix1_help)
        
        colnames(pmatrix1_help)<-names(formula_list)[i]
        colnames(pmatrix2_help)<-names(formula_list)[i]
        
        colnames(se_matrix_help)<-names(formula_list)[i]
        rownames(se_matrix_help)<-rownames(se_matrix1_help)
        
        colnames(se_matrix1_help)<-names(formula_list)[i]
        colnames(se_matrix2_help)<-names(formula_list)[i]
        
      }
      
      if (robust_se==T & is.null(weight_var)){
        pmatrix_diff_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=T)[,3])
        pmatrix1_help<- as.matrix(lmtest::coeftest(glm_list[[2]][[i]], vcov = sandwich::vcovHC(glm_list[[2]][[1]], type="HC1"))[, 4][-1])
        pmatrix2_help<- as.matrix(lmtest::coeftest(glm_list[[3]][[i]], vcov = sandwich::vcovHC(glm_list[[3]][[1]], type="HC1"))[, 4][-1])
        se_matrix_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=T)[,2])
        se_matrix1_help<- as.matrix(lmtest::coeftest(glm_list[[2]][[i]], vcov = sandwich::vcovHC(glm_list[[2]][[1]], type="HC1"))[, "Std. Error"][-1])
        se_matrix2_help<- as.matrix(lmtest::coeftest(glm_list[[3]][[i]], vcov = sandwich::vcovHC(glm_list[[3]][[1]], type="HC1"))[, "Std. Error"][-1])
      
        colnames(pmatrix_diff_help)<-names(formula_list)[i]
        rownames(pmatrix_diff_help)<-rownames(pmatrix1_help)
        
        colnames(pmatrix1_help)<-names(formula_list)[i]
        colnames(pmatrix2_help)<-names(formula_list)[i]
        
        colnames(se_matrix_help)<-names(formula_list)[i]
        rownames(se_matrix_help)<-rownames(se_matrix1_help)
        
        colnames(se_matrix1_help)<-names(formula_list)[i]
        colnames(se_matrix2_help)<-names(formula_list)[i]
        
        }
      
      
      ### bind the data together 
      
      if (i==1){
        bmatrix1<- bmatrix1_help
        bmatrix2 <- bmatrix2_help
        bmatrix_diff <- bmatrix_diff_help
        
        pmatrix1 <- pmatrix1_help
        pmatrix2 <- pmatrix2_help
        pmatrix_diff <-pmatrix_diff_help

        se_matrix1 <- se_matrix1_help
        se_matrix2 <- se_matrix2_help
        se_matrix <- se_matrix_help
        
      }
      
      if (i>1){
        bmatrix1<- merge(bmatrix1, bmatrix1_help, by=0,all=T,sort = F)
        rownames(bmatrix1)<-bmatrix1$Row.names
        bmatrix1$Row.names<-NULL
        bmatrix1 <- as.matrix(bmatrix1)
        
        bmatrix2 <- merge(bmatrix2, bmatrix2_help, by=0,all=T,sort = F)
        rownames(bmatrix2)<-bmatrix2$Row.names
        bmatrix2$Row.names<-NULL
        bmatrix2 <- as.matrix(bmatrix2)

        bmatrix_diff <- merge(bmatrix_diff, bmatrix_diff_help, by=0,all=T,sort = F)
        rownames(bmatrix_diff)<-bmatrix_diff$Row.names
        bmatrix_diff$Row.names<-NULL
        bmatrix_diff <- as.matrix(bmatrix_diff)
        
        
        pmatrix1 <- merge(pmatrix1, pmatrix1_help, by=0,all=T,sort = F)
        rownames(pmatrix1)<-pmatrix1$Row.names
        pmatrix1$Row.names<-NULL
        pmatrix1 <- as.matrix(pmatrix1)
        
        pmatrix2 <- merge(pmatrix2, pmatrix2_help, by=0,all=T,sort = F)
        rownames(pmatrix2)<-pmatrix2$Row.names
        pmatrix2$Row.names<-NULL
        pmatrix2 <- as.matrix(pmatrix2)
        
        pmatrix_diff <- merge(pmatrix_diff, pmatrix_diff_help, by=0,all=T,sort = F)
        rownames(pmatrix_diff)<-pmatrix_diff$Row.names
        pmatrix_diff$Row.names<-NULL
        pmatrix_diff <- as.matrix(pmatrix_diff)
        
        
        se_matrix1 <- merge(se_matrix1, se_matrix1_help, by=0,all=T,sort = F)
        rownames(se_matrix1)<-se_matrix1$Row.names
        se_matrix1$Row.names<-NULL
        se_matrix1 <- as.matrix(se_matrix1)
        
        se_matrix2 <- merge(se_matrix2, se_matrix2_help, by=0,all=T,sort = F)
        rownames(se_matrix2)<-se_matrix2$Row.names
        se_matrix2$Row.names<-NULL
        se_matrix2 <- as.matrix(se_matrix2)
        
        se_matrix <- merge(se_matrix, se_matrix_help, by=0,all=T,sort = F)
        rownames(se_matrix)<-se_matrix$Row.names
        se_matrix$Row.names<-NULL
        se_matrix <- as.matrix(se_matrix)
        
      }

   }
  


  #boferoni correction
  ### maybe use p.adjust instead ###

  if (is.null(p_adjust)==F) adjust_method<-p_adjust
  if (is.null(p_adjust)) adjust_method<-"bonferroni"
  
  p1_adjusted<- matrix(stats::p.adjust(p = pmatrix1, method = adjust_method),
                       ncol = ncol(pmatrix1))
  rownames(p1_adjusted)<-rownames(pmatrix1)
  p2_adjusted<- matrix(stats::p.adjust(p = pmatrix2, method = adjust_method),
                       ncol = ncol(pmatrix1))
  rownames(p2_adjusted)<-rownames(pmatrix2)
  p_diff_adjusted<- matrix(stats::p.adjust (p = pmatrix_diff, method = adjust_method),
                           ncol = ncol(pmatrix1))
  rownames(p_diff_adjusted)<-rownames(pmatrix1)
  
  
  if (is.null(p_adjust)==F){
    #p1_used<- p1_adjusted
    #p2_used<- p2_adjusted
    p1_used<- pmatrix1
    p2_used<- pmatrix2
    p_diff_used<- p_diff_adjusted
  } else {
    p1_used<- pmatrix1
    p2_used<- pmatrix2
    p_diff_used<- pmatrix_diff
    
  }
 
  
  bmatrix1<- formatC( bmatrix1, format = "e", digits = 2)
  bmatrix2<- formatC( bmatrix2, format = "e", digits = 2)
  bmatrix_diff<- formatC( bmatrix_diff, format = "e", digits = 2)
  
  bmatrix1_2<- suppressWarnings(matrix(as.numeric(formatC( bmatrix1, format = "e", digits = 6)), ncol = ncol(bmatrix1)))
  rownames(bmatrix1_2)<-rownames(bmatrix1)
  bmatrix2_2<- suppressWarnings(matrix(as.numeric(formatC( bmatrix2, format = "e", digits = 6)), ncol = ncol(bmatrix2)))
  rownames(bmatrix2_2)<-rownames(bmatrix2)
  bmatrix_diff_2<- suppressWarnings(matrix(as.numeric(formatC( bmatrix_diff, format = "e", digits = 6)), ncol = ncol(bmatrix_diff)))
  rownames(bmatrix_diff_2)<-rownames(bmatrix1)
  
  
  # build star matrices for print
  #bmatrix1_star
  bmatrix1_star<-matrix(ncol = ncol(bmatrix1),nrow = nrow(bmatrix1))
  bmatrix1_star[p1_used>=0.05 & is.na(p1_used)==F]<-paste(bmatrix1[p1_used>=0.05 & is.na(p1_used)==F], "   ", sep = "")
  bmatrix1_star[p1_used<0.05 & p1_used>=0.01 & is.na(p1_used)==F]<-paste(bmatrix1[p1_used<0.05 & p1_used>=0.01 & is.na(p1_used)==F], "*  ", sep = "")
  bmatrix1_star[p1_used<0.01 & p1_used>=0.001 & is.na(p1_used)==F]<-paste(bmatrix1[p1_used<0.01 & p1_used>=0.001 & is.na(p1_used)==F], "** ", sep = "")
  bmatrix1_star[p1_used<0.001 & is.na(p1_used)==F]<-paste(bmatrix1[p1_used<0.001 & is.na(p1_used)==F], "***", sep = "")
  bmatrix1_star[bmatrix1_2>0 & is.na(bmatrix1_2)==F]<-paste(" ",bmatrix1_star[bmatrix1_2>0 & is.na(bmatrix1_2)==F],sep = "")
  rownames(bmatrix1_star)<-rownames(bmatrix1)
  
  #bmatrix2_star
  bmatrix2_star<-matrix(ncol = ncol(bmatrix2),nrow = nrow(bmatrix2))
  bmatrix2_star[p2_used>=0.05 & is.na(p2_used)==F]<-paste(bmatrix2[p2_used>=0.05 & is.na(p2_used)==F], "   ", sep = "")
  bmatrix2_star[p2_used<0.05 & p2_used>=0.01 & is.na(p2_used)==F]<-paste(bmatrix2[p2_used<0.05 & p2_used>=0.01 & is.na(p2_used)==F], "*  ", sep = "")
  bmatrix2_star[p2_used<0.01 & p2_used>=0.001 & is.na(p2_used)==F]<-paste(bmatrix2[p2_used<0.01 & p2_used>=0.001 & is.na(p2_used)==F], "** ", sep = "")
  bmatrix2_star[p2_used<0.001 & is.na(p2_used)==F]<-paste(bmatrix2[p2_used<0.001 & is.na(p2_used)==F], "***", sep = "")
  bmatrix2_star[bmatrix2_2>0 & is.na(bmatrix2_2)==F]<-paste(" ",bmatrix2_star[bmatrix2_2>0 & is.na(bmatrix2_2)==F],sep = "")
  rownames(bmatrix2_star)<-rownames(bmatrix2)
  
  #bmatrix_diff_star
  bmatrix_diff_star<-matrix(ncol = ncol(bmatrix_diff),nrow = nrow(bmatrix_diff))
  bmatrix_diff_star[p_diff_used>=0.05 & is.na(p_diff_used)==F]<-paste(bmatrix_diff[p_diff_used>=0.05 & is.na(p_diff_used)==F], "   ", sep = "")
  bmatrix_diff_star[p_diff_used<0.05 & p_diff_used>=0.01 & is.na(p_diff_used)==F]<-paste(bmatrix_diff[p_diff_used<0.05 & p_diff_used>=0.01 & is.na(p_diff_used)==F], "*  ", sep = "")
  bmatrix_diff_star[p_diff_used<0.01 & p_diff_used>=0.001 & is.na(p_diff_used)==F]<-paste(bmatrix_diff[p_diff_used<0.01 & p_diff_used>=0.001 & is.na(p_diff_used)==F], "** ", sep = "")
  bmatrix_diff_star[p_diff_used<0.001 & is.na(p_diff_used)==F]<-paste(bmatrix_diff[p_diff_used<0.001 & is.na(p_diff_used)==F], "***", sep = "")
  bmatrix_diff_star[bmatrix_diff_2>0 & is.na(bmatrix_diff_2)==F]<-paste(" ",bmatrix_diff_star[bmatrix_diff_2>0 & is.na(bmatrix_diff_2)==F],sep = "")
  rownames(bmatrix_diff_star)<-rownames(bmatrix2)
  
  
  
  ### add matrices
  
  output_list[[2]]<-bmatrix1_2
  output_list[[3]]<-bmatrix2_2
  output_list[[4]]<-bmatrix_diff_2
  
  output_list[[5]]<-pmatrix1
  output_list[[6]]<-pmatrix2
  output_list[[7]]<-pmatrix_diff
  
  output_list[[8]]<-p1_adjusted
  output_list[[9]]<-p2_adjusted
  output_list[[10]]<-p_diff_adjusted
  
  output_list[[11]]<- noquote(bmatrix1_star)
  output_list[[12]]<- noquote(bmatrix2_star)
  output_list[[13]]<- noquote(bmatrix_diff_star)
  
  output_list[[14]]<- se_matrix1
  output_list[[15]]<- se_matrix2
  output_list[[16]]<- se_matrix
  
  if(is.null(p_adjust)) output_list[[17]]<-length(pmatrix_diff[pmatrix_diff>=0.05])/length(pmatrix_diff)
  if(is.null(p_adjust)) output_list[[18]]<-length(pmatrix_diff[pmatrix_diff<0.05])/length(pmatrix_diff)
  if(is.null(p_adjust)==F)  output_list[[17]]<-length(p_diff_adjusted[p_diff_adjusted>=0.05])/length(p_diff_adjusted)
  if(is.null(p_adjust)==F)  output_list[[18]]<-length(p_diff_adjusted[p_diff_adjusted<0.05])/length(p_diff_adjusted)
  
  names(output_list)<-c("models_interaction",
                        "coefs_data1", "coefs_data2","coefs_difference",
                        "P_coefs1","P_coefs2","P_coefs_difference",
                        "p1_adjusted","p2_adjusted","p_diff_adjusted",
                        "coefs_data1_star", "coefs_data2_star","coefs_difference_star",
                        "coefs1_se", "coefs2_se","coefs_diff_se",
                        "percent_similar","percent_diff")
  
  for (i in 2:16){
    #rownames(output_list[[i]])<-independent
    colnames(output_list[[i]])<-names(formula_list)
  }
  output_list
}




### Documentation of the multi_compare2 ###

#' Compares Samples using any glm or svyglm.
#' @description
#' \code{multi_compare2} compares samples using regression models based on glm other 
#' than \link{multi_compare},it is able to compare all types of \code{\link[stats]{glm}} 
#' or \code{\link[survey]{svyglm}} models.
#' 
#'
#' @param df,benchmark A Data frame containing the sample or benchmark to 
#' compare, or a character string containing the name of the sample or 
#' benchmark. All independent and dependent variables must be inside both data 
#' frames.
#' @param formula_list A list of named formulas. Every formula in the list will 
#' be given to \code{\link[stats]{glm}} or \code{\link[survey]{svyglm}}.
#' @param family A family input, that can be given to \code{\link[stats]{glm}} or 
#' \code{\link[survey]{svyglm}}
#' @param rm_na A character to determine how to handle missing values.  For this two
#' options are  supportet. If \code{rm_na = "pairwise"} NAs will be removed
#' seperately for every model. Only cases containing NA on one of the variables used
#' in the respective model will be removed (all independent variables but only
#' the respective dependent variable). If \code{rm_na = "listwise"} all cases containing NA
#' on one of the dependent or independent variables are removed.
#' @param out_output_list A logical value. If \code{out_output_list} = TRUE, a list will be
#' returned, containing the seperate interaction models calculated with the glm function (
    #' or svyglm in case of weighting, as well as a summary object for every model.
#' Standard errors and p-values of this models are always calculated without robustness methods.
#' @param out_df IF True, the used data frames will also be part of the output list
#' @param print_p IF TRUE, in addition to the difference in Average Discrete Change (ADC),
#' p-values will be printed.
#' @param print_se IF TRUE, additionally Standard Errors will be printed.
#' @param weight,weight_bench A character vector containing the name of the weight
#' variable in the respective data frame. IF provided the data frame will be weighted
#' using the \code{survey package}. Also id must be provided.
#' @param id,id_bench A character vector containing the name of the id variable in the respectiv
#' data frame. Only needed for weighting.
#' @param strata,strata_bench A character vector containing the name of the strata variable
#' in the respective data frame. It is used in the \code{\link[survey]{svydesign}}
#' function for weighting.
#' @param nest,nest_bench A logical Vector used in the \code{\link[survey]{svydesign}}
#' function for the respective data frame.
#' @param robust_se A logical value If TRUE instead of normal standard errors,  
#' heteroscedasticity-consistent standard errors will be used in the analysis for 
#' calculation the sandwitch package and lmtest packages are used.
#' @param p_adjust A logical input or character string indicating a adjustment method usable in the 
#' \code{method} parameter of \code{\link[stats]{p.adjust}}. If set to TRUE the Bonferroni adjusted 
#' p-values are used in inference.
#' @param names_df_benchmark A vector containing first the name of df and benchmark.
#' @param silence_summary A Logical value, to indicate if the 
#' printed summary should not be printed instead.
#' @param ... Potential other paramaters that can be given to \code{\link[stats]{glm}} or 
#' 
#' \code{\link[survey]{svyglm}}.
#'
#' @return A table is pinted showing the difference between samples for each model
#' , as well as an indicator, if they differ sinificantly from each other. It is
#' generated using the chosen \code{method}. If\code{out_output_list} = TRUE, also a list
#' with additional informations will be retruned that can be used in some aditional
#' packages of this function to reprint the summary or to visualize the results.
#'
#' @examples
#' 
#' ## Get Data for comparison
#' card<-wooldridge::card
#' 
#' south <- card[card$south==1,]
#' north <- card[card$south==0,]
#' 
#' formula_list<-list(as.formula(educ ~ age + fatheduc + motheduc + IQ),
#'                    as.formula(wage ~ age + fatheduc + motheduc + age*IQ))
#' names(formula_list)<-c("Education", "Wage")
#' 
#' ## use the function to plot the data 
#' multi_data1<-sampcompR::multi_compare2(df = north, benchmark= south,
#'                                        formula_list=formula_list)
#'  
#'  #Visualize the difference                                       
#'  sampcompR::plot_multi_compare("multi_data1")
#'                         
#'
#' @export
# #' @importFrom survey svydesign
#' 


multi_compare2<-function(df,benchmark,formula_list,rm_na="pairwise", out_output_list=T,
                         out_df=T, print_p=F, print_se=F, weight=NULL, id=NULL,
                         strata=NULL, nest=FALSE, weight_bench=NULL, id_bench=NULL,
                         strata_bench=NULL, nest_bench=FALSE, robust_se=F, p_adjust=NULL, 
                         names_df_benchmark=NULL, 
                         family=stats::gaussian(link = "identity"),
                         silence_summary=F,...){
  
  ### 1 reduce both data frames ###
  if(inherits(df,"data.frame")){  
    old_df<-df
    name_old_df<-deparse(substitute(df))}
  
  if(inherits(benchmark,"data.frame")){  
    old_benchmark<-benchmark
    name_old_benchmark<-deparse(substitute(benchmark))}
  
  if(inherits(df,"data.frame")==F){
    if(is.character(df)){
      old_df<-get(df)
      name_old_df<-df
      df<-get(df)
    }
    else stop(paste("df", " must be a data frame or a character string with the name of a dataframe",
                    sep = "", collapse = NULL))
  }
  
  if(inherits(benchmark,"data.frame")==F){
    if(is.character(benchmark)){
      old_benchmark<-get(benchmark)
      name_old_benchmark<-benchmark
      benchmark<-get(benchmark)
    }
    else stop(paste("benchmark", " must be a data frame or a character string with the name of a dataframe",
                    sep = "", collapse = NULL))
  }
  
  
  
  if (is.null(weight)) df<-reduce_df_glm(df, formula_list, rm_na = rm_na)
  if (is.null(weight_bench)) benchmark<-reduce_df_glm(benchmark, formula_list, rm_na = rm_na)
  if (is.null(weight)==F) df<-reduce_df_glm(df, formula_list,  weight_var = weight, id = id, rm_na = rm_na)
  if (is.null(weight_bench)==F) benchmark<-reduce_df_glm(benchmark, formula_list,  weight_var = weight_bench, id = id_bench, rm_na = rm_na)
  
  
  df_comb<-combine_dfs_glm(df,benchmark,formula_list,id=id,id_bench=id_bench,
                       weight=weight,weight_bench=weight_bench,
                       strata=strata,strata_bench=strata_bench)
  

  
  if (is.null(weight)==F | is.null(weight_bench)==F) {
    design_list<-list()
    design_list[[1]] <- weighted_design_glm(df_comb,formula_list,weight_var="df_weights", id="id_df", strata=NULL, nest=F, type="interact")
    design_list[[2]] <- weighted_design_glm(df_comb,formula_list,weight_var="df_weights", id="id_df", strata=NULL, nest=F, type="df1")
    design_list[[3]] <- weighted_design_glm(df_comb,formula_list,weight_var="df_weights", id="id_df", strata=NULL, nest=F, type="bench")
    
  } else {design_list = list(NULL,NULL,NULL)}
  

  
  ### 2 get a list with ols results for both data frames ###

    glm_list<-list()
    glm_list[[1]]<-run_glm(df_comb = df_comb,formula_list, design_list =  design_list[[1]], type="interact", family = family, ...)
    glm_list[[2]]<-run_glm(df_comb = df_comb,formula_list, design_list =  design_list[[2]], type="df1", family = family, ...)
    glm_list[[3]]<-run_glm(df_comb = df_comb,formula_list, design_list =  design_list[[3]], type="bench", family = family, ...)
    

  if (is.null(weight)==F | is.null(weight_bench)==F) weight_var<-T
  else weight_var<-NULL
  
  ### 3 build a output list ###
  output<-final_glm_list(glm_list = glm_list, formula_list = formula_list, 
                         weight_var=weight_var,
                         robust_se = robust_se, p_adjust = p_adjust)
  
  
  ### add dependent and independent to list
  
  independent_vars<-function(formula){
    var_string <- sub(".*~", "", deparse(formula))
    var_string<-strsplit(var_string, "\\+")
    as.vector(sapply(var_string,function(string){gsub(" ", "", string)}))
  }
  
  dependent_vars<-function(formula_list){
    
    dependent_vars_sub<-function(formula){
      dependent<-sub("\\~.*", "", deparse(formula))
      gsub(" ", "", dependent) 
    }
    dependent<-unlist(lapply(formula_list,dependent_vars_sub))
    names(dependent)<-NULL
    
    index_duplicates(dependent)
  }
  
  index_duplicates<-function(vector){
    helper<-vector
    for (i in 1:length(unique(vector))){
      if((table(vector)[i]>1)==T) names<-names(table(vector)[i])
      if((table(vector)[i]>1)==F) names<-NULL
      helper[vector %in% names]<-paste(helper[vector %in% names],"_model",seq(1:table(vector)[i]>1),sep="")
    }
    names(helper)<-NULL
    helper
  }  

  output[[19]]<- index_duplicates(names(formula_list)) #dependent
  names(output)[19]<-"dependent"
  output[[20]]<- unique(unlist(lapply(formula_list,independent_vars)))
  names(output)[20]<-"independent"
  
  ### add names of the data frames
  if (is.null(names_df_benchmark)==F) output[[21]]<-names_df_benchmark
  else output[[21]]<-c(name_old_df,name_old_benchmark)
  names(output)[21]<-"names_df_benchmark"
  
  ### p_adjustment ###
  if(is.null(p_adjust)==T){
    output[[22]]<-FALSE
  }
  if(is.null(p_adjust)==F){
    if (is.character(p_adjust)==T){
      output[[22]]<-p_adjust
    }
    if (is.character(p_adjust)==F){
      output[[22]]<-"bonferroni"
    }
    
  }
  names(output)[22]<-"p_adjust"
  
  ###edit the output ###
  ### put out used data frames ###
  if(isTRUE(out_df)) {
    output[[23]]<-list()
    names(output)[23]<-"dataframes"
    output[[23]][[1]]<-old_df
    output[[23]][[2]]<-old_benchmark
    if (is.null(names_df_benchmark)==F) names(output[[23]])<-names_df_benchmark
    else names(output[[23]])<-c(name_old_df,name_old_benchmark)}
  

  if(silence_summary==F){
  cat("\n")
  cat("Difference in coeficients between samples \n \n")
  
  print(output$coefs_difference_star)
  cat("\n")
  cat(paste("Overall difference between ", output[[21]][1], " & ", output[[21]][2], ": " , (round(output$percent_diff, digits = 3)*100),"% of coeficients are significant different", sep="",collapse = NULL))
  cat("\n")
  cat("(*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) \n ")
  }
  
  
  
  if(isTRUE(print_p)) {
    if (is.null(p_adjust)==F){
      cat("\n")
      cat("P-Values for every coeficient & model \n \n")
      print(output$p_diff_adjusted)}
    else {
      cat("\n")
      cat("P-Values for every coeficient & model \n \n")
      print(output$P_coefs_difference)}
  }
  
  if(isTRUE(print_se)) {
    cat("\n")
    cat("Standard-Errors for the difference in coeficients \n \n")
    print(output$coefs_diff_se)}
  
  
  if (out_output_list==TRUE) output<-output
  if (out_output_list==F) output<-output$coefs_difference_stars
  
  output
}






