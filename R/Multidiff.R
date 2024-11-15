###########################################################################
###
###		Subject:	A Function to Calculate and compare linear regression
###		Date: 		February 2022
###		Author: 	Bjoern Rohr
### 	Version:  	1.00
###
###		Bugfix:   	/
###
###########################################################################

### needed packages ###

### calculate a logit regression ###



##################################################
### 1 Function to reduce df to seperate models ###
##################################################

reduce_df_glm <- function (df,dependent=NULL,independent=NULL,formula_list=NULL, rm_na="pairwise", 
                            weight_var = NULL, id = NULL, strata=NULL,
                            adjustment_vars=NULL){

  df_list<-list()


  
  
  dependent<-dependent[dependent %in% colnames(df)]

  variables<-c(dependent,independent)
  if (is.null(weight_var)==FALSE) weighting<-c(weight_var,id)
  if (is.null(strata)==FALSE) weighting<-c(weighting,strata)
  #adjustment_vars<-adjustment_vars[!(adjustment_vars %in% variables)]

  if (is.null(weight_var)& is.null(adjustment_vars)==FALSE) df<-subset(df, select=c(variables,adjustment_vars))
  if (is.null(weight_var)& is.null(adjustment_vars)==TRUE) df<-subset(df, select=c(variables))
  
  if (is.null(weight_var)==FALSE & is.null(adjustment_vars)==FALSE) df<-subset(df, select=c(variables,weighting,adjustment_vars))
  if (is.null(weight_var)==FALSE & is.null(adjustment_vars)==TRUE) df<-subset(df, select=c(variables,weighting))
  #reduce df to dependent or independent variables
  if(rm_na=="listwise") df<-stats::na.omit(df)

  #reduce df to the model
  if(is.null(adjustment_vars)==TRUE){
  for (i in 1:length(dependent)){
    if (is.null(weight_var)) dataframe<-subset(df,select = c(dependent[i],independent))
    if (is.null(weight_var)==FALSE) dataframe<-subset(df,select = c(dependent[i],independent, weighting))
    if (is.null(weight_var)==FALSE) names(dataframe)[names(dataframe)==weight_var]<-"df_weights"
    if (is.null(weight_var)==FALSE & is.null(id)==FALSE) names(dataframe)[names(dataframe)==id]<-"id_df"
    if (is.null(weight_var)==FALSE & is.null(strata)==FALSE) names(dataframe)[names(dataframe)==strata]<-"strata_df"
    #if (rm_na=="pairwise") dataframe<-stats::na.omit(dataframe)
    df_list[[i]]<-dataframe}}
  
  if(is.null(adjustment_vars)==FALSE){
  for (i in 1:length(dependent)){
    adjustment_vars_i<-adjustment_vars[!(adjustment_vars %in% independent) &!(adjustment_vars %in% dependent[i])]
    if (is.null(weight_var)) dataframe<-subset(df,select = c(dependent[i],independent,adjustment_vars_i))
    if (is.null(weight_var)==FALSE) dataframe<-subset(df,select = c(dependent[i],independent, weighting,adjustment_vars_i))
    if (is.null(weight_var)==FALSE) names(dataframe)[names(dataframe)==weight_var]<-"df_weights"
    if (is.null(weight_var)==FALSE & is.null(id)==FALSE) names(dataframe)[names(dataframe)==id]<-"id_df"
    if (is.null(weight_var)==FALSE & is.null(strata)==FALSE) names(dataframe)[names(dataframe)==strata]<-"strata_df"
    #if (rm_na=="pairwise") dataframe<-stats::na.omit(dataframe)
    df_list[[i]]<-dataframe}
  
  }
  
  return(df_list)
}

#######################################################
### 1.5 function to combine both data frames to one ###
#######################################################

combine_dfs<-function(df,benchmark,dependent,independent,
                      id=NULL,id_bench=NULL,weight=NULL,
                      weight_bench=NULL,strata=NULL,strata_bench=NULL,
                      adjustment_vars=NULL){

  comb_df<-list()
  for (i in 1:length(dependent)){
    dataframe1<-as.data.frame(df[[i]])
    dataframe1$sample_ident<-0
    
    dataframe2<-as.data.frame(benchmark[[i]])
    dataframe2$sample_ident<-1
    
    ### normalize the weights if they are present ###
    if (is.null(weight)==FALSE) dataframe1$df_weights<- dataframe1$df_weights/(sum(dataframe1$df_weights)/nrow(dataframe1))
    if (is.null(weight_bench)==FALSE) dataframe2$df_weights<- dataframe2$df_weights/(sum(dataframe2$df_weights)/nrow(dataframe2))
    
    
    if ((is.null(weight)==FALSE | is.null(weight_bench)==FALSE) & is.null(id)) dataframe1$id_df<-id_df<-1:nrow(dataframe1)
    if (is.null(weight_bench)==FALSE & is.null(weight)) dataframe1$df_weights<-1
    if ((is.null(weight)==FALSE | is.null(weight_bench)==FALSE) & is.null(strata)==FALSE) dataframe1$strata<-dataframe1[,strata]
    if ((is.null(weight)==FALSE | is.null(weight_bench)==FALSE) & is.null(strata_bench)==FALSE & is.null(strata)) {
      dataframe1$strata_df<-1
    }
    
    if ((is.null(weight)==FALSE | is.null(weight_bench)==FALSE) & is.null(id_bench)) dataframe2$id_df<-id_benchmark<-1:nrow(dataframe2)
    if (is.null(weight)==FALSE & is.null(weight_bench)) dataframe2$df_weights<-1
    if ((is.null(weight)==FALSE | is.null(weight_bench)==FALSE) & is.null(strata_bench)==FALSE) {
      dataframe2$strata_df<-as.numeric(dataframe2$strata_df)+max(as.numeric(dataframe1$strata_df))
    }
    if ((is.null(weight)==FALSE | is.null(weight_bench)==FALSE) & is.null(strata_bench) & is.null(strata)==FALSE) {
      dataframe2$strata_df<-1+as.numeric(max(dataframe1$strata_df))
    }
      
    
    if(is.null(adjustment_vars)==FALSE){
      for (r in 1:length(adjustment_vars[!(adjustment_vars%in%dependent[i]) & !(adjustment_vars%in%independent)])){
        dataframe2[adjustment_vars[!(adjustment_vars%in%dependent[i]) & !(adjustment_vars%in%independent)][r]]<-NA
      }
    }
    
    comb_data<-rbind(dataframe1,dataframe2)
    if(is.null(weight)==FALSE | is.null(weight_bench)==FALSE) comb_data$id_df[comb_data$sample_ident==1]<-comb_data$id_df[comb_data$sample_ident==1]+max(comb_data$id_df[comb_data$sample_ident==0])
    comb_df[[i]]<-comb_data
  }

  comb_df
}

#######################################
### 2 Subfunctions for weighting ###
#######################################


### build survey design for every model ###
weighted_design_glm <-function(data_list,dependent, weight_var, 
                              id, strata=NULL, nest=FALSE, type="ineract", 
                              adjustment_vars=NULL, raking_targets=NULL,
                              post_targets=NULL,
                              adj_weight_design=NULL,nboots=0, 
                              boot_all=FALSE){
  design_list<-list()
  for (i in 1:length (dependent)){
    if(type=="interact"){
      
      if(is.null(adj_weight_design)==FALSE){
        
      raked_weight<-stats::weights(adj_weight_design[[i]])
      weight<-data_list[[i]][,weight_var]
      weight[1:length(raked_weight)]<-raked_weight
      data_list[[i]][,weight_var]<-weight
      }
      
      
      weight_var<-data_list[[i]]$df_weights
      new_id<-data_list[[i]]$id_df
      if(is.null(data_list[[i]]$strata_df)==FALSE) strata<-data_list[[i]]$strata_df
      if(is.null(data_list[[i]]$strata_df))strata<-NULL
      
      
    design_list[[i]]<-survey::svydesign(id = new_id,
                                        strata = strata,
                                        weights = weight_var,
                                        nest = nest,
                                        data = data_list[[i]])}

    if(type=="df1"){
      
      if(is.null(id)) new_id<-c(1:nrow(data_list[[i]][data_list[[i]]$sample_ident==0,]))
      if(is.null(id)==FALSE) new_id<-data_list[[i]][data_list[[i]]$sample_ident==0,]$id_df
      if(is.null(weight_var)) weight_var<-rep(1,nrow(data_list[[i]][data_list[[i]]$sample_ident==0,]))
      if(is.null(weight_var)==FALSE) weight_var<-data_list[[i]][data_list[[i]]$sample_ident==0,]$df_weights
      if(is.null(data_list[[i]][data_list[[i]]$sample_ident==0,]$strata_df)==FALSE) strata<-data_list[[i]][data_list[[i]]$sample_ident==0,]$strata_df
      if(is.null(data_list[[i]][data_list[[i]]$sample_ident==0,]$strata_df)) strata<-NULL
     
      
      design_list[[i]]<-survey::svydesign(id = new_id,
                                          strata = strata,
                                          weights = weight_var,
                                          nest = nest,
                                          data = data_list[[i]][data_list[[i]]$sample_ident==0,])
       
      
      
      
      if(nboots>0) {
        design_list[[i]] <- svrep::as_bootstrap_design(design_list[[i]],
                                                                       type = "Rao-Wu-Yue-Beaumont",
                                                                       replicates = nboots)} 
      if(is.null(raking_targets)==FALSE){
        

        rake_margins<-purrr::map(paste0("~",adjustment_vars),stats::as.formula)
        
        design_list[[i]] <- survey::rake(design = design_list[[i]],
                                 population.margins = raking_targets,
                                 sample.margins = rake_margins,
                                 control = list(maxit =1000, epsilon = 1, verbose=FALSE))
        
        
      }
      
      if(is.null(post_targets)==FALSE){
      design_list[[i]] <- survey::postStratify(design= design_list[[i]], 
                                                 strata=stats::reformulate(adjustment_vars), 
                                                 population=post_targets,
                                                 partial = FALSE)
      
      
      }}

    if(type=="bench"){
      if(is.null(id)==FALSE){
        
      if(is.null(strata)==FALSE) strata<-data_list[[i]][data_list[[i]]$sample_ident==1,]$strata_df
      if(is.null(strata)) strata<-NULL
        
      design_list[[i]]<-survey::svydesign(id = ~get(id),
                                          strata = strata,
                                          weights = ~get(weight_var),
                                          nest = nest,
                                          data = data_list[[i]][data_list[[i]]$sample_ident==1,])}
      if(is.null(id)){
        
         design_list[[i]]<-survey::svydesign(id = c(1:nrow(data_list[[i]][data_list[[i]]$sample_ident==1,])),
                                             strata = strata,
                                             weights = c(rep(1,nrow(data_list[[i]][data_list[[i]]$sample_ident==1,]))),
                                             nest = nest,
                                             data = data_list[[i]][data_list[[i]]$sample_ident==1,])}}
  
  
  if(type=="rake_df1"){
    ids<-c(1:nrow(data_list[[i]][data_list[[i]]$sample_ident==0,]))
      
    design_list[[i]]<-survey::svydesign(id = 1:nrow(data_list[[i]][data_list[[i]]$sample_ident==0,]),
                                        strata = NULL,
                                        weights = NULL,
                                        nest = nest,
                                        data = data_list[[i]][data_list[[i]]$sample_ident==0,])
    
    if(nboots>0) {design_list[[i]] <- svrep::as_bootstrap_design(design_list[[i]],
                                                                 type = "Rao-Wu-Yue-Beaumont",
                                                                 replicates = nboots)} 
    
    if(is.null(raking_targets)==FALSE){
      
      
      rake_margins<-purrr::map(paste0("~",adjustment_vars),stats::as.formula)
      
      design_list[[i]] <- survey::rake(design = design_list[[i]],
                               population.margins = raking_targets,
                               sample.margins = rake_margins,
                               control = list(maxit =1000, epsilon = 1, verbose=FALSE))
    }
    
    if(is.null(post_targets)==FALSE){
      design_list[[i]] <- survey::postStratify(design= design_list[[i]], 
                                               strata=stats::reformulate(adjustment_vars), 
                                               population=post_targets,
                                               partial = FALSE)
      }
    
  }
    
    
  
  if(type=="rake_interact"){
    
    if(is.null(adj_weight_design)==FALSE){
    raked_weight<-stats::weights(adj_weight_design[[i]])
    weight<-rep(1,nrow(data_list[[i]]))
    weight[1:length(raked_weight)]<-raked_weight
    
  design_list[[i]]<-survey::svydesign(id = 1:nrow(data_list[[i]]),
                                      strata = NULL,
                                      weights = weight,
                                      nest = nest,
                                      data = data_list[[i]])}}
    
  }

  return(design_list)
  

}


### weight every model ###
weighted_glm <- function(formula, design=NULL, family=stats::gaussian(link = "identity"), 
                              adjustment_vars=NULL,
                              raking_targets=NULL,
                              replicates=NULL) {
  
  # if(is.null(design_list_df)==FALSE & is.null(adjustment_vars)==FALSE){
  #   
  #   adjustment_vars<-purrr::map(paste0("~",adjustment_vars),stats::as.formula)
  #   #return(raking_targets)
  #   survey_design_raked <- rake(design = design_list_df, 
  #                               population.margins = raking_targets, 
  #                               sample.margins = adjustment_vars, 
  #                               control = list(maxit =1000, epsilon = 5, verbose=FALSE))
  #   
  # design$prob<-(design$prob/sum(design$prob))*length(design$prob)
  # survey_design_raked$prob<-(survey_design_raked$prob/sum(survey_design_raked$prob))*length(survey_design_raked$prob)
  # design$prob[1:length(survey_design_raked$prob)]<-survey_design_raked$prob
  # }

  if(is.null(replicates)){
    model<-suppressWarnings(survey::svyglm(formula = stats::as.formula(formula),
                                           design = design, 
                                           family = family))
  }
  
  if(is.null(replicates)==FALSE){
    model<-suppressWarnings(survey::svyglm(formula = stats::as.formula(formula),
                                           design = design, 
                                           family = family,
                                           return.replicates=TRUE))
  }
  

  
  return(model)
}



#############################################################
### 3 run GLM model and get Average Marginal Effects ###
#############################################################

run_glm<-function(df_comb=NULL,dependent,independent, design_list=NULL, 
                  type="interact",
                  adjustment_vars=NULL,
                  raking_targets=NULL,
                  design_list_df=NULL,
                  replicates=NULL,
                  family=stats::gaussian(link = "identity"),
                  formula_list=NULL){
  ### get formular for glm regression ###
  glm_list<-list()
  for (i in 1:length (dependent)){

    if(is.null(df_comb)==FALSE) comb_data<-df_comb[[i]]


    if(type=="interact"){
    indep<-paste(independent[1:(length(independent)-1)],"* sample_ident","+",
                 sep = " ", collapse = " ")
    form<-paste(dependent[i],"~", indep, independent[length(independent)],"* sample_ident", collapse = " ")}


    if (type=="df1" | type=="bench"){
    indep<-paste(independent[1:(length(independent)-1)],"+",
                 sep = " ", collapse = " ")
    form<-paste(dependent[i],"~", indep, independent[length(independent)], collapse = " ")}
    

    ### run glm or weighted_glm ###
    if(is.null(formula_list)){
        if(type=="interact"){
        if(is.null(design_list)) glm_next<- stats::glm(stats::as.formula(form), data = comb_data, family = family)
        if(is.null(design_list)==FALSE) glm_next<- weighted_glm (design=design_list[[i]], formula= form,replicates=replicates,family = family)}

      if(type=="df1"){
        if(is.null(design_list)) glm_next<- stats::glm(stats::as.formula(form), data = comb_data[comb_data$sample_ident==0,], family = family)
        if(is.null(design_list)==FALSE) glm_next<- weighted_glm (design=design_list[[i]], formula= form,replicates = replicates,family = family)}
    
      if(type=="bench"){
        if(is.null(design_list)) glm_next<- stats::glm(stats::as.formula(form), data = comb_data[comb_data$sample_ident==1,], family = family)
        if(is.null(design_list)==FALSE) glm_next<- weighted_glm (design=design_list[[i]], formula= form,replicates= replicates,family = family)}
    }
    
    if(is.null(formula_list)==FALSE){
      if(type=="interact"){
        form<-stats::update(formula_list,. ~ . * sample_ident)
        if(is.null(design_list)) glm_next<- stats::glm(stats::as.formula(form), data = comb_data, family = family)
        if(is.null(design_list)==FALSE) glm_next<- weighted_glm (design=design_list[[i]], formula= form,replicates=replicates,family = family)}
      
      if(type=="df1"){
        form<-formula_list
        if(is.null(design_list)) glm_next<- stats::glm(stats::as.formula(form), data = comb_data[comb_data$sample_ident==0,], family = family)
        if(is.null(design_list)==FALSE) glm_next<- weighted_glm (design=design_list[[i]], formula= form,replicates = replicates,family = family)}
      
      if(type=="bench"){
        form<-formula_list
        if(is.null(design_list)) glm_next<- stats::glm(stats::as.formula(form), data = comb_data[comb_data$sample_ident==1,], family = family)
        if(is.null(design_list)==FALSE) glm_next<- weighted_glm (design=design_list[[i]], formula= form,replicates= replicates,family = family)}
    }

    glm_list[[i]]<-glm_next
    glm_list[[i+length(dependent)]]<-summary(glm_next)
  }
  names(glm_list)[1:length(dependent)]<-paste(dependent,"_model", sep = "")
  names(glm_list)[(length(dependent)+1):(2*length(dependent))]<-paste(dependent,"_mod_summ", sep = "")

  glm_list
}

###############################
### 4 Calculate the outputs ###
###############################


final_glm_list<-function(glm_list, dependent=NULL,independent=NULL,formula_list=NULL,
                         weight_var=NULL,robust_se=FALSE, 
                         p_adjust=NULL, nboots=0, df=NULL,benchmark=NULL,
                         id=NULL,id_bench=NULL, weight = NULL,weight_bench = NULL,
                         strata = NULL, strata_bench = NULL, rm_na = "pairwise", 
                         family = stats::gaussian(link = "identity"), 
                         parallel=FALSE, out_models=FALSE,
                         adjustment_vars=NULL,raking_targets=NULL,
                         post_targets=NULL,boot_all=FALSE,
                         percentile_ci=TRUE){

  output_list<-list()
  if(out_models==TRUE) output_list[[1]]<-glm_list
  
  if(is.null(formula_list)) forms<- purrr::map(dependent,~stats::as.formula(paste(.,"~",paste(independent, collapse = " + "))))
  if(is.null(formula_list)==FALSE) forms<-formula_list
  
  if(is.null(names(forms))) names(forms)<-purrr::map(forms,~as.character(.)[2])
  names(forms)<-number_occurrences(names(forms))
  
  for (i in 1:length(forms)) {
    bmatrix1_help<- as.matrix(stats::coef(glm_list[[2]][[i]])[-1])
    bmatrix2_help<- as.matrix(stats::coef(glm_list[[3]][[i]])[-1])
    bmatrix_diff_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=FALSE)[,1])
    
    
    colnames(bmatrix1_help)<-names(forms)[i]
    colnames(bmatrix2_help)<-names(forms)[i]
    colnames(bmatrix_diff_help)<-names(forms)[i]
    rownames(bmatrix_diff_help)<-rownames(bmatrix1_help)
    
    
    if (robust_se==FALSE | isTRUE(weight_var)){
      pmatrix_diff_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=FALSE)[,3])
      pmatrix1_help<- as.matrix(summary(glm_list[[2]][[i]])$coefficients[, 4][-1])
      pmatrix2_help<- as.matrix(summary(glm_list[[3]][[i]])$coefficients[, 4][-1])
      se_matrix_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=FALSE)[,2])
      se_matrix1_help<- as.matrix(summary(glm_list[[2]][[i]])$coefficients[, "Std. Error"][-1])
      se_matrix2_help<- as.matrix(summary(glm_list[[3]][[i]])$coefficients[, "Std. Error"][-1])
      
      colnames(pmatrix_diff_help)<-names(forms)[i]
      rownames(pmatrix_diff_help)<-rownames(pmatrix1_help)
      
      colnames(pmatrix1_help)<-names(forms)[i]
      colnames(pmatrix2_help)<-names(forms)[i]
      
      colnames(se_matrix_help)<-names(forms)[i]
      rownames(se_matrix_help)<-rownames(se_matrix1_help)
      
      colnames(se_matrix1_help)<-names(forms)[i]
      colnames(se_matrix2_help)<-names(forms)[i]
      
    }
    
    if (robust_se==TRUE & is.null(weight_var)){
      pmatrix_diff_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=TRUE)[,3])
      pmatrix1_help<- as.matrix(lmtest::coeftest(glm_list[[2]][[i]], vcov = sandwich::vcovHC(glm_list[[2]][[1]], type="HC1"))[, 4][-1])
      pmatrix2_help<- as.matrix(lmtest::coeftest(glm_list[[3]][[i]], vcov = sandwich::vcovHC(glm_list[[3]][[1]], type="HC1"))[, 4][-1])
      se_matrix_help<- as.matrix(extract_interaction_results(glm_list[[1]][[i]],robust_se=TRUE)[,2])
      se_matrix1_help<- as.matrix(lmtest::coeftest(glm_list[[2]][[i]], vcov = sandwich::vcovHC(glm_list[[2]][[1]], type="HC1"))[, "Std. Error"][-1])
      se_matrix2_help<- as.matrix(lmtest::coeftest(glm_list[[3]][[i]], vcov = sandwich::vcovHC(glm_list[[3]][[1]], type="HC1"))[, "Std. Error"][-1])
      
      colnames(pmatrix_diff_help)<-names(forms)[i]
      rownames(pmatrix_diff_help)<-rownames(pmatrix1_help)
      
      colnames(pmatrix1_help)<-names(forms)[i]
      colnames(pmatrix2_help)<-names(forms)[i]
      
      colnames(se_matrix_help)<-names(forms)[i]
      rownames(se_matrix_help)<-rownames(se_matrix1_help)
      
      colnames(se_matrix1_help)<-names(forms)[i]
      colnames(se_matrix2_help)<-names(forms)[i]
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
      bmatrix1<- merge(bmatrix1, bmatrix1_help, by=0,all=TRUE,sort = FALSE)
      rownames(bmatrix1)<-bmatrix1$Row.names
      bmatrix1$Row.names<-NULL
      bmatrix1 <- as.matrix(bmatrix1)
      
      bmatrix2 <- merge(bmatrix2, bmatrix2_help, by=0,all=TRUE,sort = FALSE)
      rownames(bmatrix2)<-bmatrix2$Row.names
      bmatrix2$Row.names<-NULL
      bmatrix2 <- as.matrix(bmatrix2)
      
      bmatrix_diff <- merge(bmatrix_diff, bmatrix_diff_help, by=0,all=TRUE,sort = FALSE)
      rownames(bmatrix_diff)<-bmatrix_diff$Row.names
      bmatrix_diff$Row.names<-NULL
      bmatrix_diff <- as.matrix(bmatrix_diff)
      
      
      pmatrix1 <- merge(pmatrix1, pmatrix1_help, by=0,all=TRUE,sort = FALSE)
      rownames(pmatrix1)<-pmatrix1$Row.names
      pmatrix1$Row.names<-NULL
      pmatrix1 <- as.matrix(pmatrix1)
      
      pmatrix2 <- merge(pmatrix2, pmatrix2_help, by=0,all=TRUE,sort = FALSE)
      rownames(pmatrix2)<-pmatrix2$Row.names
      pmatrix2$Row.names<-NULL
      pmatrix2 <- as.matrix(pmatrix2)
      
      pmatrix_diff <- merge(pmatrix_diff, pmatrix_diff_help, by=0,all=TRUE,sort = FALSE)
      rownames(pmatrix_diff)<-pmatrix_diff$Row.names
      pmatrix_diff$Row.names<-NULL
      pmatrix_diff <- as.matrix(pmatrix_diff)
      
      
      se_matrix1 <- merge(se_matrix1, se_matrix1_help, by=0,all=TRUE,sort = FALSE)
      rownames(se_matrix1)<-se_matrix1$Row.names
      se_matrix1$Row.names<-NULL
      se_matrix1 <- as.matrix(se_matrix1)
      
      se_matrix2 <- merge(se_matrix2, se_matrix2_help, by=0,all=TRUE,sort = FALSE)
      rownames(se_matrix2)<-se_matrix2$Row.names
      se_matrix2$Row.names<-NULL
      se_matrix2 <- as.matrix(se_matrix2)
      
      se_matrix <- merge(se_matrix, se_matrix_help, by=0,all=TRUE,sort = FALSE)
      rownames(se_matrix)<-se_matrix$Row.names
      se_matrix$Row.names<-NULL
      se_matrix <- as.matrix(se_matrix)
      
    }
    
  }
  
  ### if nboots >0 use bootsrap to get the p_values for df and interaction
  
  if (nboots!=0 & nboots <=1) {
    stop("nboots must be 0 (for analytic p_values) or >1 for bootstrap p_values")}
  
  if(nboots>1) {
    
    p_se_list<-multi_boot(df=df,benchmark=benchmark,dependent,independent,
                          formula_list=formula_list,
                          id = id,
                          id_bench = id_bench,weight = weight,weight_bench = weight_bench,
                          strata = strata, strata_bench = strata_bench, rm_na = rm_na, 
                          family = family, nboots = nboots, parallel = parallel, 
                          adjustment_vars = adjustment_vars, raking_targets = raking_targets,
                          post_targets=post_targets,
                          ref=bmatrix2,boot_all=boot_all,
                          coef1=bmatrix1,
                          coef1_bench=bmatrix2,
                          percentile_ci=percentile_ci)
    
    pmatrix1<-p_se_list[[1]]
    pmatrix_diff<-p_se_list[[2]]
    se_matrix1<-p_se_list[[3]]
    se_matrix<-p_se_list[[4]]
  }

  #boferoni correction
  ### maybe use p.adjust instead ###

  if (is.null(p_adjust)==FALSE) adjust_method<-p_adjust
  if (is.null(p_adjust)) adjust_method<-"bonferroni"

  p1_adjusted <- pmatrix1
  p2_adjusted <- pmatrix2
  p_diff_adjusted <- pmatrix_diff
  
  for (i in 1:nrow(pmatrix1)){
  p1_adjusted[i,]<- matrix(stats::p.adjust(p = pmatrix1[i,], method = adjust_method,
                                           n=ncol(pmatrix1)),
                       ncol = ncol(pmatrix1))
  p2_adjusted[i,]<- matrix(stats::p.adjust(p = pmatrix2[i,], method = adjust_method,
                                           n=ncol(pmatrix1)),
                       ncol = ncol(pmatrix1))
  p_diff_adjusted[i,]<- matrix(stats::p.adjust (p = pmatrix_diff[i,], method = adjust_method,
                                                n=ncol(pmatrix1)),
                           ncol = ncol(pmatrix1))}

  if (is.null(p_adjust)==FALSE){
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
  bmatrix2_2<- suppressWarnings(matrix(as.numeric(formatC( bmatrix2, format = "e", digits = 6)), ncol = ncol(bmatrix2)))
  bmatrix_diff_2<- suppressWarnings(matrix(as.numeric(formatC( bmatrix_diff, format = "e", digits = 6)), ncol = ncol(bmatrix_diff)))

  # build star matrices for print
  #bmatrix1_star
  bmatrix1_star<-bmatrix1
  
  bmatrix1_star[p1_used>=0.05 & is.na(p1_used)==FALSE]<-paste(bmatrix1[p1_used>=0.05 & is.na(p1_used)==FALSE], "   ", sep = "")
  bmatrix1_star[p1_used<0.05 & p1_used>=0.01 & is.na(p1_used)==FALSE]<-paste(bmatrix1[p1_used<0.05 & p1_used>=0.01 & is.na(p1_used)==FALSE], "*  ", sep = "")
  bmatrix1_star[p1_used<0.01 & p1_used>=0.001 & is.na(p1_used)==FALSE]<-paste(bmatrix1[p1_used<0.01 & p1_used>=0.001 & is.na(p1_used)==FALSE], "** ", sep = "")
  bmatrix1_star[p1_used<0.001 & is.na(p1_used)==FALSE]<-paste(bmatrix1[p1_used<0.001 & is.na(p1_used)==FALSE], "***", sep = "")
  bmatrix1_star[bmatrix1_2>0 & is.na(p1_used)==FALSE]<-paste("",bmatrix1_star[bmatrix1_2>0 & is.na(p1_used)==FALSE],sep = "")

  #bmatrix2_star
  bmatrix2_star<-bmatrix2
  
  bmatrix2_star[p2_used>=0.05 & is.na(p2_used)==FALSE]<-paste(bmatrix2[p2_used>=0.05 & is.na(p2_used)==FALSE & is.na(p2_used)==FALSE], "   ", sep = "")
  bmatrix2_star[p2_used<0.05 & p2_used>=0.01 & is.na(p2_used)==FALSE]<-paste(bmatrix2[p2_used<0.05 & p2_used>=0.01 & is.na(p2_used)==FALSE], "*  ", sep = "")
  bmatrix2_star[p2_used<0.01 & p2_used>=0.001 & is.na(p2_used)==FALSE]<-paste(bmatrix2[p2_used<0.01 & p2_used>=0.001 & is.na(p2_used)==FALSE], "** ", sep = "")
  bmatrix2_star[p2_used<0.001 & is.na(p2_used)==FALSE]<-paste(bmatrix2[p2_used<0.001 & is.na(p2_used)==FALSE], "***", sep = "")
  bmatrix2_star[bmatrix2_2>0 & is.na(p2_used)==FALSE]<-paste("",bmatrix2_star[bmatrix2_2>0 & is.na(p2_used)==FALSE],sep = "")

  #bmatrix_diff_star
  bmatrix_diff_star<-bmatrix_diff
  
  bmatrix_diff_star[p_diff_used>=0.05 & is.na(p_diff_used)==FALSE]<-paste(bmatrix_diff[p_diff_used>=0.05 & is.na(p_diff_used)==FALSE], "   ", sep = "")
  bmatrix_diff_star[p_diff_used<0.05 & p_diff_used>=0.01 & is.na(p_diff_used)==FALSE]<-paste(bmatrix_diff[p_diff_used<0.05 & p_diff_used>=0.01 & is.na(p_diff_used)==FALSE], "*  ", sep = "")
  bmatrix_diff_star[p_diff_used<0.01 & p_diff_used>=0.001 & is.na(p_diff_used)==FALSE]<-paste(bmatrix_diff[p_diff_used<0.01 & p_diff_used>=0.001 & is.na(p_diff_used)==FALSE], "** ", sep = "")
  bmatrix_diff_star[p_diff_used<0.001 & is.na(p_diff_used)==FALSE]<-paste(bmatrix_diff[p_diff_used<0.001 & is.na(p_diff_used)==FALSE], "***", sep = "")
  bmatrix_diff_star[bmatrix_diff_2>0 & is.na(p_diff_used)==FALSE]<-paste("",bmatrix_diff_star[bmatrix_diff_2>0 & is.na(p_diff_used)==FALSE],sep = "")



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
  if(is.null(p_adjust)==FALSE)  output_list[[17]]<-length(p_diff_adjusted[p_diff_adjusted>=0.05])/length(p_diff_adjusted)
  if(is.null(p_adjust)==FALSE)  output_list[[18]]<-length(p_diff_adjusted[p_diff_adjusted<0.05])/length(p_diff_adjusted)

  names(output_list)<-c("models_interaction",
                        "coefs_data1", "coefs_data2","coefs_difference",
                        "P_coefs1","P_coefs2","P_coefs_difference",
                        "p1_adjusted","p2_adjusted","p_diff_adjusted",
                        "coefs_data1_star", "coefs_data2_star","coefs_difference_star",
                        "coefs1_se", "coefs2_se","coefs_diff_se",
                        "percent_similar","percent_diff")
  colnames(bmatrix1)
  
  for (i in 2:16){
    rownames(output_list[[i]])<-rownames(bmatrix1)
    colnames(output_list[[i]])<-colnames(bmatrix1)
  }
  output_list
}



#######################################################
### final: combine all functions, to compare models ###
#######################################################

### Documentation of the multi_glm_compare ###

# #' Compares set of respondents using GLM-models fit in two models.
# #' 
# #' \code{multi_glm_compare} This Function compares the coefficients of one GLM-Regression-Model
# #' in one set of respondents with the coefficients of an identical model in 
# #' another set of respondents. First, both sets of respondents
# #' will be attached to each other in one data frame and the model will be 
# #' calculated on the combined set of respondents.
# #' A Dummy will be used as an interaction term, to look for differences in the set of respondents.
# #' One model will be calclated for every dependent variable provided, using 
# #' every independet
# #' variable in the models.
# #' 
# #' @param df,benchmark Data frames containing the sets of respondents to compare. 
# #' All independent and
# #' dependent variables must be inside both data frames.
# #' @param dependent A list of strings containing the dependent variables (y) for comparison.
# #' One model will be computed for every dependent variable (y) provided.
# #' @param independent A list of strings containing the independent variables (x) for comparison.
# #' Every independent variable will be used in every model to estimate the dependent variable (y)
# #' @param rm_na To compute the chow test missings must be removed.  For this two
# #' options are currently supportet. If \code{rm_na = "pairwise"} NAs will be removed
# #' seperately for every model. Only cases containing NA on one of the variables used
# #' in the respective model will be removed (all independent variables but only
# #' the respective dependent variable). If \code{rm_na = "listwise"} all cases containing NA
# #' on one of the dependent or independent variables are removed.
# #' @param out_glmlist A logical value. If \code{out_glmlist} = TRUE, a list will be
# #' returned, containing the seperate interaction models calculated with the glm function (
# #' or svyglm in case of weighting, as well as a summary object for every model.
# #' Standard errors and p-values of this models are always calculated without robustness methods.
# #' @param out_df If TRUE, the used data frames will also be part of the output list
# #' @param print_p If TRUE, in addition to the difference in Average Discrete Change (ADC),
# #' p-values will be printed.
# #' @param print_se If TRUE, additionally Standard Errors will be printed.
# #' @param weight,weight_bench A character vector containing the name of the weight
# #' variable in the respective data frame. If provided the data frame will be weighted
# #' using the \code{survey package}. Also id must be provided.
# #' @param id,id_bench A character vector containing the name of the id variable in the respectiv
# #' data frame. Only needed for weighting.
# #' @param strata,strata_bench A character vector containing the name of the strata variable
# #' in the respective data frame. It is used in the \code{\link[survey]{svydesign}}
# #' function for weighting.
# #' @param nest,nest_bench A logical Vector used in the \code{\link[survey]{svydesign}}
# #' function for the respective data frame.
# #' @param robust_se=FALSE A logical value If TRUE instead of normal standard errors,  heteroscedasticity-consistent
# #' standard errors will be used in the analysis for calculation the sandwitch package and lmtest packages are used.
# #' @param p_adjust A logical value If TRUE the bonferroni adjusted p-values are used in inference
# #' statistic.
# #' @param names_df_benchmark A vector containing first the name of df and the benchmark.
# #' 
# #' 
# #' @return A table is pinted showing the difference in \code{Coeficients)}
# #' between sets of respondents for each model, as well as an indicator, if 
# #' they differ sinificantly from each other. If\code{out_glmlist} = TRUE, also 
# #' a list with additional informations will be retruned that can be used in 
# #' some aditional packages of this function to reprint the summary or to 
# #' visualize the results.
# #' 
# #' 
# #' 
# #' 
# #' @importFrom survey svydesign
# #' 
# #' 
# #' 


multi_glm_compare<-function(df,benchmark,independent=NULL,dependent=NULL,formula_list=NULL,rm_na="pairwise", out_glmlist=TRUE,
                            out_df=FALSE, out_models=FALSE, print_p=FALSE, print_se=FALSE, weight=NULL, id=NULL,
                            strata=NULL, nest=FALSE, weight_bench=NULL, id_bench=NULL,
                            strata_bench=NULL, nest_bench=FALSE, robust_se=FALSE, p_adjust=NULL, 
                            names_df_benchmark=NULL, family=stats::gaussian(link = "identity"), silence_summary=FALSE, 
                            nboots=0, parallel=FALSE, adjustment_vars=NULL,
                            raking_targets=NULL, post_targets=NULL,boot_all=FALSE,
                            percentile_ci=TRUE){

  ### 1 reduce both data frames ###
  ### 1 reduce both data frames ###
  if(inherits(df,"data.frame")){  
    old_df<-df
    name_old_df<-deparse(substitute(df))}
  
  if(inherits(benchmark,"data.frame")){  
    old_benchmark<-benchmark
    name_old_benchmark<-deparse(substitute(benchmark))}
  
  if(inherits(df,"data.frame")==FALSE){
    if(is.character(df)){
      old_df<-get(df)
      name_old_df<-df
      df<-get(df)
    }
    else stop(paste("df", " must be a data frame or a character string with the name of a dataframe",
                    sep = "", collapse = NULL))
  }
  
  if(inherits(benchmark,"data.frame")==FALSE){
    if(is.character(benchmark)){
      old_benchmark<-get(benchmark)
      name_old_benchmark<-benchmark
      benchmark<-get(benchmark)
    }
    else stop(paste("benchmark", " must be a data frame or a character string with the name of a dataframe",
                    sep = "", collapse = NULL))
  }

  dependent<-dependent_checker(df=df,dependent = dependent, dfname = name_old_df)
  dependent<-dependent_checker(df=benchmark,dependent = dependent, dfname = name_old_benchmark)


  df_old<-df
  benchmark_old<-benchmark
  
  if(is.null(formula_list)){
  if (is.null(weight)) df<-reduce_df_glm(df, dependent, independent, rm_na = rm_na, adjustment_vars = adjustment_vars)
  if (is.null(weight_bench)) benchmark<-reduce_df_glm(benchmark, dependent, independent, rm_na = rm_na,strata)
  if (is.null(weight)==FALSE) df<-reduce_df_glm(df, dependent, independent,  weight_var = weight, id = id, strata=strata, rm_na = rm_na, adjustment_vars = adjustment_vars)
  if (is.null(weight_bench)==FALSE) benchmark<-reduce_df_glm(benchmark, dependent, independent,  weight_var = weight_bench, id = id_bench, strata=strata_bench, rm_na = rm_na)
  }
  
  if(is.null(formula_list)==FALSE){
      var_list<-purrr::map(formula_list,all.vars)
      dep_indep<-function(vars,type="dependent"){
        
        if(type=="dependent"){
          out<-vars[1]
        }else{
          out<-vars[2:length(vars)]
        }
        
        out
      }
      dependent<-purrr::map(var_list,~dep_indep(.,"dependent"))
      independent<-purrr::map(var_list,~dep_indep(.,"independent"))
      
      
    
    
    if (is.null(weight)) df<-purrr::map2(.x=dependent,.y=independent,~reduce_df_glm(df, .x,.y, rm_na = rm_na, adjustment_vars = adjustment_vars)[[1]])
    if (is.null(weight_bench)) benchmark<-purrr::map2(.x=dependent,.y=independent,~reduce_df_glm(benchmark, .x,.y, rm_na = rm_na,strata)[[1]])
    if (is.null(weight)==FALSE) df<-purrr::map2(.x=dependent,.y=independent,~reduce_df_glm(df, .x,.y,  weight_var = weight, id = id, strata=strata, rm_na = rm_na, adjustment_vars = adjustment_vars)[[1]])
    if (is.null(weight_bench)==FALSE) benchmark<-purrr::map2(.x=dependent,.y=independent,~reduce_df_glm(benchmark, .x,.y,  weight_var = weight_bench, id = id_bench, strata=strata_bench, rm_na = rm_na)[[1]])
 
  }
  
  if(is.null(formula_list)){
  df_comb<-combine_dfs(df,benchmark,dependent,independent,id=id,id_bench=id_bench,
                       weight=weight,weight_bench=weight_bench,
                       strata=strata,strata_bench=strata_bench, 
                       adjustment_vars = adjustment_vars)}
  
  if(is.null(formula_list)==FALSE){
    df_comb<-purrr::map(.x=1:length(dependent),
                        ~combine_dfs(df[.x],benchmark[.x],dependent[[.x]],independent[[.x]],id=id,id_bench=id_bench,
                         weight=weight,weight_bench=weight_bench,
                         strata=strata,strata_bench=strata_bench, 
                         adjustment_vars = adjustment_vars)[[1]])}
  

  
  
  

  # df_comb2<-combine_dfs(benchmark,df,dependent,independent,id=id_bench,id_bench=id,
  #                       weight=weight_bench,weight_bench=weight,
  #                       strata=strata_bench,strata_bench=strata)

  # calculate survey deisgns if weighted

  if (is.null(weight)==FALSE) {
    design_list<-list()
    if(is.null(raking_targets)==TRUE & is.null(post_targets)==TRUE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="df1")
    if(is.null(raking_targets)==FALSE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="df1",
                                                                           adjustment_vars = adjustment_vars, raking_targets=raking_targets)
    if(is.null(post_targets)==FALSE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="df1",
                                                                           adjustment_vars = adjustment_vars, post_targets=post_targets)
    if(is.null(raking_targets)==TRUE & is.null(post_targets)==TRUE) design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="interact")
    if(is.null(raking_targets)==FALSE| is.null(post_targets)==FALSE) design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="interact", 
                                                                        adj_weight_design = design_list[[2]])
    } else {design_list = list(NULL,NULL)}
    
  if(is.null(weight_bench)==FALSE){  
  design_list[[3]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata_bench, nest=FALSE, type="bench")
  design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata_bench, nest=FALSE, type="interact")
  } 
  else {
    design_list[[3]] <- weighted_design_glm(df_comb,dependent,weight_var=NULL, id=NULL, strata=NULL, nest=FALSE, type="bench")}
  
  if (is.null(weight)==TRUE & (is.null(raking_targets)==FALSE | is.null(post_targets)==FALSE)) {
    
    if(is.null(raking_targets)==FALSE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="rake_df1",
                                            adjustment_vars = adjustment_vars, raking_targets=raking_targets)
    if(is.null(post_targets)==FALSE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="rake_df1",
                                                                           adjustment_vars = adjustment_vars, post_targets=post_targets)
    
    
    design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=strata, nest=FALSE, type="rake_interact", 
                                            adj_weight_design = design_list[[2]])
  }


  

  ### 2 get a list with glm results for both data frames ###
  if(is.null(formula_list)){
  
  glm_list<-list()
   glm_list[[1]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[1]], type="interact",design_list_df =design_list[[2]],family=family)
   glm_list[[2]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[2]], type="df1",family=family)
   glm_list[[3]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[3]], type="bench",family=family)
    }
  
  if(is.null(formula_list)==FALSE){
    
      glm_list<-list()
      glm_list[[1]]<-purrr::map(.x=1:length(dependent),~run_glm(df_comb = df_comb[.x],dependent[[.x]],independent[[.x]], design_list =  design_list[[1]][.x], type="interact",design_list_df =design_list[[2]][.x],,family=family, formula_list=formula_list[[.x]])[[1]])
      glm_list[[2]]<-purrr::map(.x=1:length(dependent),~run_glm(df_comb = df_comb[.x],dependent[[.x]],independent[[.x]], design_list =  design_list[[2]][.x], type="df1",,family=family,formula_list=formula_list[[.x]])[[1]])
      glm_list[[3]]<-purrr::map(.x=1:length(dependent),~run_glm(df_comb = df_comb[.x],dependent[[.x]],independent[[.x]], design_list =  design_list[[3]][.x], type="bench",,family=family,formula_list=formula_list[[.x]])[[1]])
    }



  if (is.null(weight)==FALSE | is.null(weight_bench)==FALSE) weight_var<-TRUE
  else weight_var<-NULL

  ### 3 build a output list ###
  output<-final_glm_list(glm_list, dependent = dependent,
                         independent = independent, 
                         formula_list=formula_list,
                         weight_var=weight_var,
                         robust_se = robust_se, p_adjust = p_adjust,
                         df=df_old,benchmark=benchmark_old,
                         id=id,id_bench=id_bench, weight = weight,
                         weight_bench = weight_bench,
                         strata = strata, strata_bench = strata_bench, rm_na = rm_na, 
                         family=family,nboots=nboots, 
                         parallel = parallel, out_models=out_models,
                         adjustment_vars=adjustment_vars, raking_targets=raking_targets,
                         post_targets=post_targets,boot_all=boot_all,
                         percentile_ci=percentile_ci)
  

  
  
  ### add dependent and independent to list
  output[[19]]<-number_occurrences(unlist(dependent))
  names(output)[19]<-"dependent"
  output[[20]]<-unique(unlist(independent))
  names(output)[20]<-"independent"

  ### add names of the data frames
  if (is.null(names_df_benchmark)==FALSE) output[[21]]<-names_df_benchmark
  else output[[21]]<-c(name_old_df,name_old_benchmark)
  names(output)[21]<-"names_df_benchmark"

  ### p_adjustment ###
  if(is.null(p_adjust)==TRUE){
    output[[22]]<-FALSE
  }
  if(is.null(p_adjust)==FALSE){
    if (is.character(p_adjust)==TRUE){
      output[[22]]<-p_adjust
    }
    if (is.character(p_adjust)==FALSE){
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
    if (is.null(names_df_benchmark)==FALSE) names(output[[23]])<-names_df_benchmark
    else names(output[[23]])<-c(name_old_df,name_old_benchmark)}

if(silence_summary==FALSE){
  cat("\n")
  cat("Difference in coeficients between sets of respondents \n \n")

  print(output$coefs_difference_star)
  cat("\n")
  cat(paste("Overall difference between ", output[[21]][1], " & ", output[[21]][2], ": " , (round(output$percent_diff, digits = 3)*100),"% of coeficients are significant different", sep="",collapse = NULL))
  cat("\n")
  cat("(*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test robust standard errors are used) \n ")
}
  
  if(isTRUE(print_p)) {
    if (is.null(p_adjust)==FALSE){
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


  if (out_glmlist==TRUE) output<-output
  if (out_glmlist==FALSE) output<-output$coefs_difference_stars

  output
}


###########################
### summary_glm_compare ###
###########################

# #' A summary of a multi_ame_compare list-object
# #'
# #' \code{summary_glm_compare} prints the printed output of the \code{\link[sampcompR]{multi_glm_compare}}
# #' function of this package
# #'
# #' @param glm_comp_object A list returned by the \code{\link[sampcompR]{multi_glm_compare}} function.
# #' @param print_p IF TRUE, in addition to the difference in Average Discrete Change (ADC),
# #' p-values will be printed.
# #' @param print_se IF TRUE, additionally Standard Errors will be printed.
# #'
# #' @return Prints several infomation of the inputed \code{multi_glm_compare} list-object.
# #'

summary_glm_compare<-function (glm_comp_object, print_p=FALSE, print_se=FALSE){

  cat("\n")
  cat("Difference in Average Discrete Change (ADC) between samples \n \n")

  df<- glm_comp_object$names_df_benchmark[1]
  benchmark<- glm_comp_object$names_df_benchmark[2]

  print(glm_comp_object$coefs_difference_star)
  cat("\n")
  cat(paste("Overall similarity between ", df, " & ", benchmark, ": ", (round(glm_comp_object$percent_diff, digits = 3)*100),"% of coeficients are significant different", sep="",collapse = NULL))
  cat("\n")
  cat("(*p<0.05 ; **p<0.005 ; ***p<0.001;  for t-test Robust Standard Errors are used) \n ")

  if(isTRUE(print_p)) {
    cat("\n")
    cat("P-Values for every coeficient & model \n \n")
    print(glm_comp_object$P_coefs_difference)}

  if(isTRUE(print_se)) {
    cat("\n")
    cat("Standard-Errors for the difference in ADC \n \n")
    print(glm_comp_object$coefs_diff_se)}
}



### Documentation of the multi_compare ###

#' Compares data frames using different regression methods.
#'
#' \code{multi_compare} compares data frames using regression models based on 
#' differing methods. All \code{\link[stats]{glm}} Models can be compared. 
#'
#' @param df,benchmark A data frame containing the set of respondents or 
#' benchmark set of respondents to compare, or a character string containing the 
#' name of the set of respondents or benchmark set of respondents. All independent 
#' and dependent variables must be inside both data frames.
#' @param dependent A list of strings containing the dependent variables (y) for 
#' comparison. One model will be computed for every dependent variable (y) 
#' provided. When a \code{formula_list} is provided, \code{dependent} will be 
#' ignored.
#' @param independent A list of strings containing the independent variables (x) 
#' for comparison. Every independent variable will be used in every model to 
#' estimate the dependent variable (y). When a \code{formula_list} is provided, 
#' \code{independent} will be ignored.
#' @param family A family input, that can be given to \code{\link[stats]{glm}} or 
#' \code{\link[survey]{svyglm}}. Additionally, if "ols" is given, 
#' \code{gaussian(link = "identity")}, and if "logit" is given, 
#' \code{binomial(link = "logit")} is used.
#' @param rm_na A character to determine how to handle missing values.  For this two
#' options are  supported. If \code{rm_na = "pairwise"} NAs will be removed
#' separately for every model. Only cases containing NA on one of the variables used
#' in the respective model will be removed (all independent variables but only
#' the respective dependent variable). If \code{rm_na = "listwise"} all cases containing NA
#' on one of the dependent or independent variables are removed.
#' @param out_output_list A logical value. If \code{out_output_list = TRUE}, a 
#' list will be returned, containing the separate interaction models calculated 
#' with the \code{\link[stats]{glm}} function or \code{\link[survey]{svyglm}} 
#' in case of weighting, as well as a summary object for every model.
#' Standard errors and p-values of these models are always calculated without 
#' robustness methods.
#' @param out_df If \code{TRUE}, the used data frames will also be part of the output list.
#' @param print_p If \code{TRUE}, in addition to the difference in Average Discrete Change (ADC),
#' p-values will be printed.
#' @param print_se If \code{TRUE}, additionally standard errors will be printed.
#' @param weight,weight_bench A character vector containing the name of the weight
#' variable in the respective data frame. If provided the data frame will be weighted
#' using the \code{\link[survey]{svydesign}} function. Also \code{id} must be provided.
#' @param id,id_bench A character vector containing the name of the id variable in the respectiv
#' data frame. Only needed for weighting.
#' @param strata,strata_bench A character vector containing the name of the strata variable
#' in the respective data frame. It is used in the \code{\link[survey]{svydesign}}
#' function for weighting.
#' @param nest,nest_bench A logical vector that is used in the \code{\link[survey]{svydesign}}
#' function for the respective data frame.
#' @param robust_se A logical value If \code{TRUE} instead of normal standard errors,  
#' heteroscedasticity-consistent standard errors will be used in the analysis to 
#' calculate them the \code{\link[sandwich]{vcovHC}} and \code{\link[lmtest]{coeftest}} 
#' packages are used.
#' @param p_adjust A logical input or character string indicating an adjustment 
#' method usable in the \code{method} parameter of \code{\link[stats]{p.adjust}}. 
#' If set to TRUE the Bonferroni adjusted p-values are used in inference.
#' @param silence_summary A logical value, to indicate if the 
#' printed summary should not be printed instead.
#' @param nboots A numeric value indicating the number of bootstrap replications. 
#' If nboots = 0 no bootstrapping will be performed. Else \code{nboots} must 
#' be >2. Note, that bootstrapping can be very computationaly heavy and can 
#' therefore take a while.
#' @param parallel If \code{TRUE}, all detected cores will be used in bootstrapping.
#' @param names_df_benchmark A vector containing first the name of \code{df} and benchmark.
#' @param out_models If True, GLM model objects will be part of the returned object.
#' @param adjustment_vars Variables used to adjust the survey when using raking 
#' or post-stratification.
#' @param raking_targets A List of raking targets that can be given to the rake 
#' function of \code{\link[survey]{rake}}, to rake the \code{df}.
#' @param post_targets A List of post_stratification targets that can be given to the rake 
#' function of \code{\link[survey]{postStratify}}, to post_stratificatify the \code{df}.
#' @param formula_list A list of formulas to use in the regression models. If 
#' given, \code{dependent} and \code{independent} parameters will be ignored.
#' @param boot_all If TURE, both, dfs and benchmarks will be bootstrapped. Otherwise 
#' the benchmark estimate is assumed to be constant.
#' @param percentile_ci If TURE, cofidence intervals will be calculated using the percentile method.
#' If False, they will be calculated using the normal method.
#' 
#'
#' @return A table is printed showing the difference between the set of respondents
#' for each model, as well as an indicator, if they differ significantly from each 
#' other. It is generated using the chosen \code{method}. 
#' If\code{out_output_list} = TRUE, also a list with additional information will 
#' be returned that can be used in some additional packages of this function to 
#' reprint the summary or to visualize the results.
#'
#' @examples
#' 
#' #Example 1
#' ## Make a comparison specifiying dependent and independent variables.
#' 
#' ## Get Data for comparison
#' require(wooldridge)
#' card<-wooldridge::card
#' 
#' north <- card[card$south==0,]
#' 
#' 
#' ## use the function to plot the data 
#' multi_data1<-sampcompR::multi_compare(df = north, 
#'                                      bench = card,
#'                                      independent = c("age","fatheduc","motheduc","IQ"),
#'                                      dependent = c("educ","wage"),
#'                                      family="ols") 
#'                         
#' plot_multi_compare("multi_data1")
#' 
#' #Example 2
#' ## Make a comparison with a formula_list
#' card<-wooldridge::card
#'
#' north <- card[card$south==0,]
#'
#' form_list<-list(formula(educ~age+fatheduc+motheduc+IQ),
#'                 formula(wage~age+fatheduc+motheduc+IQ))
#'
## use the function to plot the data
#'
#' multi_data2 <- sampcompR::multi_compare(df = north, 
#'                                         bench = card,
#'                                         formula_list = form_list,
#'                                         family="ols")
#' 
#'
#' plot_multi_compare("multi_data2")
#'
#' @export
#' 


multi_compare <- function(df,benchmark,independent=NULL,dependent=NULL, formula_list=NULL, 
                          family="ols",rm_na="pairwise",out_output_list=TRUE,
                          out_df=FALSE, out_models=FALSE, print_p=FALSE, print_se=FALSE, weight=NULL, id=NULL,
                          strata=NULL, nest=FALSE, weight_bench=NULL, id_bench=NULL,strata_bench=NULL,
                          nest_bench=FALSE, robust_se=FALSE, p_adjust=NULL, names_df_benchmark=NULL, 
                          silence_summary=FALSE, nboots=0, boot_all=FALSE, parallel = FALSE, adjustment_vars=NULL,
                          raking_targets=NULL, post_targets=NULL,percentile_ci=TRUE){


  if (is.null(names_df_benchmark)) names_df_benchmark<-c (deparse(substitute(df)), deparse(substitute(benchmark)))
  else names_df_benchmark<-names_df_benchmark
  
   
   
  
    
  if(inherits(family,"family")){
    output<-multi_glm_compare(df=df,benchmark=benchmark,independent=independent,
                              dependent=dependent,formula_list=formula_list,
                              rm_na=rm_na, out_glmlist=out_output_list,
                              out_df=out_df, out_models = out_models,print_p=print_p, print_se=print_se,
                              weight=weight, id=id, strata=strata,
                              nest=nest, weight_bench=weight_bench, id_bench=id_bench,
                              strata_bench=strata_bench, nest_bench=nest_bench, robust_se=robust_se,
                              p_adjust=p_adjust, names_df_benchmark=names_df_benchmark,
                              silence_summary=silence_summary, nboots = nboots, 
                              parallel = parallel,adjustment_vars=adjustment_vars,
                              raking_targets=raking_targets, post_targets = post_targets,
                              family=family, boot_all=boot_all,
                              percentile_ci=percentile_ci)}

  if(inherits(family,"character")){
    if(family=="ols"){
      output<-multi_glm_compare(df=df,benchmark=benchmark,independent=independent,
                                dependent=dependent,formula_list=formula_list,
                                rm_na=rm_na, out_glmlist=out_output_list,
                                out_df=out_df, out_models = out_models,print_p=print_p, print_se=print_se,
                                weight=weight, id=id, strata=strata,
                                nest=nest, weight_bench=weight_bench, id_bench=id_bench,
                                strata_bench=strata_bench, nest_bench=nest_bench, robust_se=robust_se,
                                p_adjust=p_adjust, names_df_benchmark=names_df_benchmark,
                                silence_summary=silence_summary, nboots = nboots, 
                                parallel = parallel,adjustment_vars=adjustment_vars,
                                raking_targets=raking_targets, post_targets = post_targets,
                                family=stats::gaussian(link = "identity"),boot_all=boot_all,
                                percentile_ci=percentile_ci)}}

  if(inherits(family,"character")){
    if(family=="logit"){
      output<-multi_glm_compare(df=df,benchmark=benchmark,independent=independent,
                                dependent=dependent,formula_list=formula_list,
                                rm_na=rm_na, out_glmlist=out_output_list,
                                out_df=out_df, out_models = out_models,print_p=print_p, print_se=print_se,
                                weight=weight, id=id, strata=strata,
                                nest=nest, weight_bench=weight_bench, id_bench=id_bench,
                                strata_bench=strata_bench, nest_bench=nest_bench, robust_se=robust_se,
                                p_adjust=p_adjust, names_df_benchmark=names_df_benchmark,
                                silence_summary=silence_summary, nboots = nboots, 
                                parallel = parallel,adjustment_vars=adjustment_vars,
                                raking_targets=raking_targets, post_targets = post_targets,
                                family = stats::binomial(link = "logit"),boot_all=boot_all,
                                percentile_ci=percentile_ci)}}
  
  output
}






###########################################################################################
### multi_reg_plotter: A Function to visualize the results of multi_reg_compare objects ###
###########################################################################################

### Documentation of the multi_reg_plotter_old ###

# #' Visualize multi_reg_compare-objects
# #'
# #' \code{multi_reg_plotter_old} visualizes objects generated my the multi_reg_compare function
# #'
# #' @param multi_reg_object A output of the multi_reg_compare function
# #' @param df_lab,benchmark_lab A Name for the compared data frames used in the title.
# #' If plot_title is not provided.
# #' @param plot_title A title of the Plot.
# #' @param p_value A probability used as a minimum p-value for some variants
# #' @param breaks Legend labels. If variant == "one", 3 are needed, in variant "two"
# #' and "three", 4 breaks are needed
# #' @param matrix IF True a matrix will be outputted, instead of a heatmap, containing the same information
# #' @param colors A vector of strings, containing the colors for the plot. Depending on the
# #' variant 3 or 4 are needed, equivalent to the breaks
# #' @param variant Variant can be either "one", "two" or "three".
# #' If "one" the plot will show if the variables in the regression models are
# #' significantly different from each other (Diff). If they are, it will also show if
# #' they differ in strength or direction as well (Strong Diff).
# #' If "two" the plot will show if variables in the regression models differ significantly
# #' from each other (Strong Diff) If not it will show if they still differ in direction
# #' (Diff in dir) or if one is significant while the other is not (Diff in sig).
# #' If "three" the plot will show if variables in the regression models differ from
# #' each other on various aspects. If one is significant, while the other is not (Diff in sig),
# #' if they differ in direction "Diff in sig" of if they differ in strength. If variables meet the criteria
# #' for multiple categories they will classified in the following prefering order:
# #' Diff in Sig > Diff in dir or Diff in strength.
# #'
# #' @return A heatmatrix, visualising the difference of models of differing data frames, compared with the
# #' multi_reg_compare function
# #'
# #' @import  plot.matrix

# multi_reg_plotter_old<-function(multi_reg_object, df_lab=NULL, benchmark_lab=NULL, plot_title=NULL,
#                                 p_value=0.05, breaks=NULL,matrix=FALSE, colors=NULL, variant="one", p_adjust=NULL,
#                                 note=TRUE,mar=NULL, key=list(side=4), ...){
# 
#   ### Build title ###
#   df_title <- multi_reg_object[[21]][1]
#   benchmark_title <- multi_reg_object[[21]][2]
#   plot_title <- ifelse(is.null(plot_title), paste("Compare ", df_title, " & ", benchmark_title, sep = "", collapse = NULL), plot_title)
# 
# 
#   ### Look for Labels
#   df_lab <- ifelse(is.null(df_lab), "dependent variables", df_lab)
#   benchmark_lab <- ifelse(is.null(benchmark_lab), "independent variables", benchmark_lab)
# 
#   ### get the relevant data out of the list ###
#   if (is.null(p_adjust)){
#   sample_diff<-multi_reg_object$P_coefs_difference
#   p_df<-multi_reg_object$P_coefs1
#   p_benchmark<-multi_reg_object$P_coefs2
#   }
# 
#   if (is.null(p_adjust)==FALSE){
#     sample_diff<-multi_reg_object$p_diff_adjusted
#     p_df<-multi_reg_object$p1_adjusted
#     p_benchmark<-multi_reg_object$p2_adjusted
#   }
# 
#   b_df<-multi_reg_object$coefs_data1
#   b_benchmark<-multi_reg_object$coefs_data2
# 
#   independent<-multi_reg_object$independent
#   dependent<-multi_reg_object$dependent
# 
# 
#   ### prepare breaks ###
#   if(is.null(breaks)){
#     if(variant=="one") breaks <- c("Same", "Different", "High Difference")
#     if(variant=="two") breaks <- c("Same", "Diff in Direrction","Diff in Significance", "Significant Difference")
#     if(variant=="three") breaks <- c("Same", "Diff in Strength", "Diff in Direrction","Diff in Significance")
#     if(variant=="four") breaks<- c("Same", "Diff in Significance")
#     if(variant=="five") breaks<- c("Same", "Diff in Direction")
#     if(variant=="six") breaks<- c("Same", "Diff in Stength")
#   }
# 
# 
#   ### different variants of plots
# 
#   ### only signif ###
#   if (variant=="one"){
#   comp_matrix<-sample_diff
#   comp_matrix[sample_diff > p_value] <- breaks[1]
#   comp_matrix[sample_diff < p_value & (p_df > p_value & p_benchmark > p_value)] <- breaks[1]
#   comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value)] <-  breaks[2]
#   comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) &
#                 (abs(b_df) > 2*abs(b_benchmark) |  2*abs(b_df) < abs(b_benchmark)) &
#                 ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0))] <- breaks[3]
#   comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) &
#                 ((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0))] <- breaks[3]
#   }
# 
#   ### also insig difference ###
#   if(variant=="two"){
#     comp_matrix<-sample_diff
#     comp_matrix[sample_diff > p_value] <- breaks[1]
#     comp_matrix[sample_diff < p_value & (p_df > p_value & p_benchmark > p_value)] <- breaks[1]
#     comp_matrix[sample_diff > p_value & ((p_df < p_value & p_benchmark > p_value) |
#                                      (p_df > p_value & p_benchmark < p_value))] <- breaks[3] # Differenz not sig and one model sig, the other not
#     comp_matrix[sample_diff > p_value & (p_df < p_value | p_benchmark < p_value) &
#                   ((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) &
#                   comp_matrix!= breaks[3]] <-  breaks[2] #Differenz not sig and one model double the other
# 
#     comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) & comp_matrix!= breaks[3]] <- breaks[4] # Models Significant Different
#   }
# 
#   ### only difference in estimation ###
#   if(variant=="three"){
#     comp_matrix<-sample_diff
#     comp_matrix[(p_df < p_value & p_benchmark > p_value) | (p_df > p_value & p_benchmark < p_value)] <- breaks[4] # Differenz not sig and one model sig, the other not
#     comp_matrix[((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) & comp_matrix!=breaks[4] ] <- breaks[3] #Differenz not sig and one model double the other
# 
#     comp_matrix[(p_df < p_value | p_benchmark < p_value) & (abs(b_df) > 2*abs(b_benchmark) |  2*abs(b_df) < abs(b_benchmark)) &
#                   ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0)) & comp_matrix!= breaks[4]] <- breaks[2] # Models Significant Different
#     comp_matrix[comp_matrix!= breaks[2] & comp_matrix!= breaks[3] & comp_matrix!= breaks[4]] <- breaks[1]
#   }
# 
#   ### only difference in significance ###
#   if(variant=="four"){
#     comp_matrix<-sample_diff
#     comp_matrix[(p_df < p_value & p_benchmark > p_value) | (p_df > p_value & p_benchmark < p_value)]<- breaks[2]
#     comp_matrix[comp_matrix!= breaks[2]] <- breaks[1]
#   }
# 
#   ### only difference in significance ###
#   if(variant=="five"){
#     comp_matrix<-sample_diff
#     comp_matrix[((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0))]<-breaks[2]
#     comp_matrix[comp_matrix!= breaks[2]] <- breaks[1]
#   }
# 
#   ### only difference in significance ###
#   if(variant=="six"){
#     comp_matrix<-sample_diff
#     comp_matrix[(p_df < p_value | p_benchmark < p_value) & (abs(b_df) > 2*abs(b_benchmark) |  2*abs(b_df) < abs(b_benchmark)) & ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0))] <- breaks[2]
#     comp_matrix[comp_matrix!= breaks[2]] <- breaks[1]
#   }
# 
# 
# 
# 
# 
# 
#   if (variant=="one"){
#     if (matrix == FALSE & is.null(colors)) colors <- c("green", "yellow", "red")
#     #if (matrix == FALSE & is.null(breaks)) breaks <- c("Same", "Different", "High Difference")
#     note_text<- "Note: Same (green) means that the coeficients are not significant different. \nDifferent (yellow) means, at least one is significant >0 or <0 and both are significant different from each other. \nHigh Difference (red) means all conditions for Difference are true and the coeficients differ in direction \nor one is double the value of the other. \nLevel of Significance is p < 0.05."
#     if (is.null (mar)) {mar= c(12.5, 6, 2, 6)}
#     if (is.null (mar)==FALSE){mar=mar}
#     }
# 
#   if (variant=="two"){
#     if (matrix == FALSE & is.null(colors)) colors <- c("green", "yellow","orange", "red")
#     #if (matrix == FALSE & is.null(breaks)) breaks <- c("Same", "Diff in direrction","Diff in sig", "Strong Diff")
#     note_text<-"Note: Same (green) means no difference of any type. \nDiff in direction (yellow) means no significant difference between coefs, but they differ in direction. \nDiff in Significance (orange) means no significant difference but one is significant < or > 0 while the other is not. \nSignificant Difference (red) means, both coeficients differ significant from each other. \nLevel of Significance is p < 0.05"
#     if (is.null (mar)) {mar= c(12.5, 6, 2, 7.5)}
#     if (is.null (mar)==FALSE){mar=mar}
#     }
# 
#   if (variant=="three"){
#     if (matrix == FALSE & is.null(colors)) colors <- c("green", "yellow","orange", "red")
#     #if (matrix == FALSE & is.null(breaks)) breaks <- c("Same", "Diff in strength", "Diff in Direrction","Diff in significance")
#     note_text<-"Note: Same (green) means there is no meaningful difference of any type. \nDiff in Strength (yellow) means that one coef is > double the value of the other. \nDiff in Direction (orange) means that one coef is positive while the other is negative \nDiff in Significance (red) means that one is significant < or > 0 while the other is not. \nLevel of Significance is p < 0.05"
#     if (is.null (mar)) {mar= c(12.5, 6, 2, 5.5)}
#     if (is.null (mar)==FALSE){mar=mar}
#     }
# 
#   if (variant=="four"){
#     if (matrix == FALSE & is.null(colors)) colors <- c("green", "red")
#     #if (matrix == FALSE & is.null(breaks)) breaks <- c("Same", Diff in Significance")
#     note_text<-"Note: Same (green) means there is no difference in significance. \nDiff in Significance (red) means that one coefcient is significant < or > 0 while the other is not. \nLevel of Significance is p < 0.05"
#     if (is.null (mar)) {mar= c(11.5, 6, 2, 6.5)}
#     if (is.null (mar)==FALSE){mar=mar}
#     }
# 
#   if (variant=="five"){
#     if (matrix == FALSE & is.null(colors)) colors <- c("green", "red")
#     #if (matrix == FALSE & is.null(breaks)) breaks <- c("Same", Diff in Direction")
#     note_text<-"Note: Same (green) means there is no difference in direction. \nDiff in Direction (red) means that one coeficient is <0 while the other is >0."
#     if (is.null (mar)) {mar= c(10.5, 6, 2, 5.5)}
#     if (is.null (mar)==FALSE){mar=mar}
#     }
# 
#   if (variant=="six"){
#     if (matrix == FALSE & is.null(colors)) colors <- c("green", "red")
#     #if (matrix == FALSE & is.null(breaks)) breaks <- c("Same", Diff in Strength")
#     note_text<-"Note: Same (green) means there is no difference in strength. \nDiff in Strength (red) means that one coeficient has double the value of the other."
#     if (is.null (mar)) {mar= c(10.5, 6, 2, 5.5)}
#     if (is.null (mar)==FALSE){mar=mar}
#     }
# 
# 
#   opar <- graphics::par(no.readonly=TRUE)      # make a copy of current settings
# 
#   if (matrix == FALSE & variant =="one") graphics::par(mar = mar,las=2,...,cex.axis=0.75)
#   if (matrix == FALSE & variant =="two") graphics::par(mar = mar ,las=2, ..., cex.axis=0.75) # bottom,left,top,right
#   if (matrix == FALSE & variant =="three") graphics::par(mar = mar,las=2, ..., cex.axis=0.75) # bottom,left,top,right
#   if (matrix == FALSE & variant =="four") graphics::par(mar = mar ,las=2, ..., cex.axis=0.75) # bottom,left,top,right
#   if (matrix == FALSE & variant =="five") graphics::par(mar = mar ,las=2, ..., cex.axis=0.75) # bottom,left,top,right
#   if (matrix == FALSE & variant =="six") graphics::par(mar = mar ,las=2, ..., cex.axis=0.75) # bottom,left,top,right
#   if (matrix == FALSE) comparison_plot <- plot(comp_matrix, col = colors, breaks = breaks, main = plot_title, label_y="", label_x="", key=key)
#   if (matrix == FALSE & variant =="one" & note == TRUE) graphics::mtext(note_text, side = 1, line = (mar[1]-1), cex = 0.8, adj=0, las=0)
#   if (matrix == FALSE & variant =="two" & note == TRUE) graphics::mtext(note_text, side = 1, line = (mar[1]-1), cex = 0.8, adj=0, las=0)
#   if (matrix == FALSE & variant =="three" & note == TRUE) graphics::mtext(note_text, side = 1, line = (mar[1]-1), cex = 0.8, adj=0, las=0)
#   if (matrix == FALSE & variant =="four" & note == TRUE) graphics::mtext(note_text, side = 1, line = (mar[1]-1.5), cex = 0.8, adj=0, las=0)
#   if (matrix == FALSE & variant =="five" & note == TRUE) graphics::mtext(note_text, side = 1, line = (mar[1]-1.5), cex = 0.8, adj=0, las=0)
#   if (matrix == FALSE & variant =="six" & note == TRUE) graphics::mtext(note_text, side = 1, line = (mar[1]-1.5), cex = 0.8, adj=0, las=0)
# 
#   par(opar) #reset to original par
#   return(comparison_plot)
# }








###########################################################################################
### multi_reg_plotter: A Function to visualize the results of multi_reg_compare objects ###
###########################################################################################

### Documentation of the multi_reg_plotter ###

# #' Visualize multi_reg_compare-objects
# #'
# #' @description
# #' \code{multi_reg_plotter} visualizes objects generated by the \code{multi_reg_compare} function
# #'
# #' @param multi_reg_object A output of the multi_reg_compare function.
# #' @param df_lab,benchmark_lab A Name for the compared data frames used in the title.
# #' If plot_title is not provided.
# #' @param plot_title A string containing the title of the vizualization.
# #' @param p_value A number between zero and one, used as p-value in sigificance analyses
# #' @param breaks A vector, containing a number of strings, to rename the categories in the legend.
# #' It's possible length depends on the \code{variant}.
# #' @param colors A vector of colors, usable in ggplot, for every break. It's possible length depends on the \code{variant}.
# #' @param plot_data A Logical value. If TRUE, instead of a plot a data frame will be returned, that is used for the plot.
# #' @param variant Variant can be either "one", "two", "three","four","five", or "six".
# #' \itemize{
# #' \item{'variant = one"'} {The plot will show whether the coefficients in the regression models are
# #' significantly different from each other (Diff). When they are, it will also show if
# #' they differ in strength (one is twice the size of the other) or direction as well (Large Diff).}
# #'
# #' \item{'variant = "two"'} {The plot will show whether coefficients in the regression models differ significantly
# #' from each other (Large Diff). If not it will show whether they still differ in direction
# #' (Diff in Direction) or whether one is significant while the other is not (Diff in Significance).}
# #'
# #' \item{'variant = "three"'} {The plot will show whether coefficients in the regression models differ from
# #' each other on various aspects. Whether one is significant, while the other is not (Diff in Significance),
# #' whether they differ in direction (Diff in Direrction) or whether one is double the size of the other (Diff in Strength).
# #' When variables meet the criteria for multiple categories they will classified in the latest fitting category.}
# #'
# #' \item{'variant = "four"'} {The plot will show if the coefficient in the df is significant,
# #' while the coefficient is not significant in the benchmark or the other way around  (Diff in Significance).}
# #'
# #' \item{'variant = "five"'} {The plot will show if the coefficient in the df is positive,
# #' while the coefficient in the benchmark is negative or the other way around  (Diff in Direction).}
# #'
# #' \item{'variant = "six"'} {The plot will show if the coefficient in the df is double the size of the
# #' coefficient in the benchmark or the other way around  (Diff in Strength).}}
# #'
# #' @param note A Logical value. If TRUE, a note will be displayed under the plot describing the \code{variant}
# #' @param grid A string, that can either be "none" or a color, for the edges of every tile. If "none", no grid will be displayed.
# #' @param diff_perc A logical value. If TRUE, the Percent of the differing Categories, decided by the variant, will be displayed
# #' in the corner of the plot.
# #' @param diff_perc_size A Number du decide the size of the text in \code{diff_perc}
# #' @param perc_diff_transparance A Number between zero and one, to decide the background transparancy of \code{diff_perc}
# #' @param diff_perc_position A character string, to choose the position of \code{diff_perc} Can either be "top_right"(default),
# #' "bottom_right","bottom_left", or "top_left"
# #'
# #'
# #'
# #' @return Returns a heatmatrix-like plot created with ggplot, to vizualize the multivariate differences. On the y-axis
# #' the indepent variables are displayed, while on the x-axis the independent variables are displayed. Depending on the
# #' variant, the displayed tile colors must be interpreted differently. For more information on interpretation look at
# #' \code{variant}.
# #'
# #' @export
# 
# multi_reg_plotter<-function(multi_reg_object, df_lab=NULL, benchmark_lab=NULL, plot_title=NULL,
#                             p_value=0.05, breaks=NULL,plot_data=FALSE, colors=NULL, variant="one", p_adjust=NULL,
#                             note=TRUE, grid="white", diff_perc=FALSE, diff_perc_size=4.5,
#                             perc_diff_transparance=0, diff_perc_position= "top_right", label_x=NULL, label_y=NULL,missings_x=TRUE){
# 
#   ### Build title ###
#   df_title <- multi_reg_object[[21]][1]
#   benchmark_title <- multi_reg_object[[21]][2]
#   plot_title <- ifelse(is.null(plot_title), paste("Compare ", df_title, " & ", benchmark_title, sep = "", collapse = NULL), plot_title)
# 
# 
#   ### Look for Labels
#   df_lab <- ifelse(is.null(df_lab), "dependent variables", df_lab)
#   benchmark_lab <- ifelse(is.null(benchmark_lab), "independent variables", benchmark_lab)
# 
#   ### get the relevant data out of the list ###
#   if (is.null(p_adjust)){
#     sample_diff<-multi_reg_object$P_coefs_difference
#     p_df<-multi_reg_object$P_coefs1
#     p_benchmark<-multi_reg_object$P_coefs2
#   }
# 
#   if (is.null(p_adjust)==FALSE){
#     sample_diff<-multi_reg_object$p_diff_adjusted
#     p_df<-multi_reg_object$P_coefs1
#     p_benchmark<-multi_reg_object$P_coefs2
#   }
# 
#   b_df<-multi_reg_object$coefs_data1
#   b_benchmark<-multi_reg_object$coefs_data2
# 
#   independent<-multi_reg_object$independent
#   dependent<-multi_reg_object$dependent
# 
# 
#   ### prepare breaks ###
#   if(is.null(breaks)){
#     if(variant=="one") breaks <- c("Same", "Small Difference", "Large Difference")
#     if(variant=="two") breaks <- c("Same", "Diff in Direrction","Diff in Significance", "Significant Difference")
#     if(variant=="three") breaks <- c("Same", "Diff in Strength", "Diff in Direrction","Diff in Significance")
#     if(variant=="four") breaks<- c("Same", "Diff in Significance")
#     if(variant=="five") breaks<- c("Same", "Diff in Direction")
#     if(variant=="six") breaks<- c("Same", "Diff in Stength")
#   }
# 
# 
#   ### different variants of plots
# 
#   ### only signif ###
#   if (variant=="one"){
#     comp_matrix<-sample_diff
#     colnames(comp_matrix)<-dependent
#     comp_matrix[sample_diff > p_value & !is.na(sample_diff)] <- breaks[1]
#     comp_matrix[sample_diff < p_value & (p_df > p_value & p_benchmark > p_value) & !is.na(sample_diff)] <- breaks[1]
#     comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) & !is.na(sample_diff)] <-  breaks[2]
#     comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) &
#                   (abs(b_df) > 2*abs(b_benchmark) |  2*abs(b_df) < abs(b_benchmark)) &
#                   ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0)) & !is.na(sample_diff)] <- breaks[3]
#     comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) &
#                   ((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) & !is.na(sample_diff)] <- breaks[3]
#   }
# 
# 
#   ### also insig difference ###
#   if(variant=="two"){
#     comp_matrix<-sample_diff
#     colnames(comp_matrix)<-dependent
#     comp_matrix[sample_diff > p_value] <- breaks[1]
#     comp_matrix[sample_diff < p_value & (p_df > p_value & p_benchmark > p_value) & !is.na(sample_diff)] <- breaks[1]
#     comp_matrix[sample_diff > p_value & ((p_df < p_value & p_benchmark > p_value) |
#                                      (p_df > p_value & p_benchmark < p_value)) & !is.na(sample_diff)] <- breaks[3] # Differenz not sig and one model sig, the other not
#     comp_matrix[sample_diff > p_value & (p_df < p_value | p_benchmark < p_value) &
#                   ((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) &
#                   comp_matrix!= breaks[3] & !is.na(sample_diff)] <-  breaks[2] #Differenz not sig and one model double the other
# 
#     comp_matrix[sample_diff < p_value & (p_df < p_value | p_benchmark < p_value) & comp_matrix!= breaks[3] & !is.na(sample_diff)] <- breaks[4] # Models Significant Different
#   }
# 
#   ### only difference in estimation ###
#   if(variant=="three"){
#     comp_matrix<-sample_diff
#     colnames(comp_matrix)<-dependent
#     comp_matrix[(p_df < p_value & p_benchmark > p_value) | (p_df > p_value & p_benchmark < p_value) & !is.na(sample_diff)] <- breaks[4] # Differenz not sig and one model sig, the other not
#     comp_matrix[((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) & comp_matrix!=breaks[4] & !is.na(sample_diff)] <- breaks[3] #Differenz not sig and one model double the other
# 
#     comp_matrix[(p_df < p_value | p_benchmark < p_value) & (abs(b_df) > 2*abs(b_benchmark) |  2*abs(b_df) < abs(b_benchmark)) &
#                   ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0)) & comp_matrix!= breaks[4] & !is.na(sample_diff)] <- breaks[2] # Models Significant Different
#     comp_matrix[comp_matrix!= breaks[2] & comp_matrix!= breaks[3] & comp_matrix!= breaks[4] & !is.na(sample_diff)] <- breaks[1]
#   }
# 
#   ### only difference in significance ###
#   if(variant=="four"){
#     comp_matrix<-sample_diff
#     colnames(comp_matrix)<-dependent
#     comp_matrix[(p_df < p_value & p_benchmark > p_value) | (p_df > p_value & p_benchmark < p_value) & !is.na(sample_diff)]<- breaks[2]
#     comp_matrix[comp_matrix!= breaks[2] & !is.na(sample_diff)] <- breaks[1]
#   }
# 
#   ### only difference in significance ###
#   if(variant=="five"){
#     comp_matrix<-sample_diff
#     colnames(comp_matrix)<-dependent
#     comp_matrix[((b_df < 0 & b_benchmark > 0) | (b_df > 0 & b_benchmark < 0)) & !is.na(sample_diff)]<-breaks[2]
#     comp_matrix[comp_matrix!= breaks[2] & !is.na(sample_diff)] <- breaks[1]
#   }
# 
#   ### only difference in significance ###
#   if(variant=="six"){
#     comp_matrix<-sample_diff
#     colnames(comp_matrix)<-dependent
#     comp_matrix[(p_df < p_value | p_benchmark < p_value) & (abs(b_df) > 2*abs(b_benchmark) |
#                                                   2*abs(b_df) < abs(b_benchmark)) &
#                   ((b_df < 0 & b_benchmark < 0) | (b_df > 0 & b_benchmark > 0)) & !is.na(sample_diff)] <- breaks[2]
#     comp_matrix[comp_matrix!= breaks[2] & !is.na(sample_diff)] <- breaks[1]
#   }
# 
# 
# 
# 
# 
# 
#   if (variant=="one"){
#     if (is.null(colors)) colors <- c("green", "yellow", "red")
#     note_text<- "Note: Same (green) means that the coeficients are not significant different. \nDifferent (yellow) means, at least one is significant >0 or <0 and both are significant different from each other. \nHigh Difference (red) means all conditions for Difference are true and the coeficients differ in direction \nor one is double the value of the other. \nLevel of Significance is p < 0.05."
#   }
# 
#   if (variant=="two"){
#     if (is.null(colors)) colors <- c("green", "yellow","orange", "red")
#     note_text<-"Note: Same (green) means no difference of any type. \nDiff in direction (yellow) means no significant difference between coefs, but they differ in direction. \nDiff in Significance (orange) means no significant difference but one is significant < or > 0 while the other is not. \nSignificant Difference (red) means, both coeficients differ significant from each other. \nLevel of Significance is p < 0.05"
#   }
# 
#   if (variant=="three"){
#     if (is.null(colors)) colors <- c("green", "yellow","orange", "red")
#     note_text<-"Note: Same (green) means there is no meaningful difference of any type. \nDiff in Strength (yellow) means that one coef is > double the value of the other. \nDiff in Direction (orange) means that one coef is positive while the other is negative \nDiff in Significance (red) means that one is significant < or > 0 while the other is not. \nLevel of Significance is p < 0.05"
#   }
# 
#   if (variant=="four"){
#     if (is.null(colors)) colors <- c("green", "red")
#     note_text<-"Note: Same (green) means there is no difference in significance. \nDiff in Significance (red) means that one coefcient is significant < or > 0 while the other is not. \nLevel of Significance is p < 0.05"
#   }
# 
#   if (variant=="five"){
#     if (is.null(colors)) colors <- c("green", "red")
#     note_text<-"Note: Same (green) means there is no difference in direction. \nDiff in Direction (red) means that one coeficient is <0 while the other is >0."
#   }
# 
#   if (variant=="six"){
#     if (is.null(colors)) colors <- c("green", "red")
#     note_text<-"Note: Same (green) means there is no difference in strength. \nDiff in Strength (red) means that one coeficient has double the value of the other."
#   }
# 
# 
# 
#   ### Get breaks in % ###
# 
#   if(diff_perc==TRUE) {
#     percental_difference_b1<-length(comp_matrix[comp_matrix == breaks[1] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
#     percental_difference_b2<-length(comp_matrix[comp_matrix == breaks[2] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
#     if (length(breaks)>2) percental_difference_b3<-length(comp_matrix[comp_matrix == breaks[3] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
#     if (length(breaks)>3) percental_difference_b4<-length(comp_matrix[comp_matrix == breaks[4] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
# 
#     diff_summary<-paste("Different Correlations in % : \n",breaks[1]," :",(round((percental_difference_b1), digits = 3)*100),"% \n",
#                         breaks[2]," :",(round(percental_difference_b2, digits = 3)*100),"%")
#     if (length(breaks)>2) diff_summary<-paste (diff_summary, "\n",breaks[3], " :", (round(percental_difference_b3, digits = 3)*100),"%")
#     if (length(breaks)>3) diff_summary<-paste (diff_summary, "\n",breaks[4], " :", (round(percental_difference_b4, digits = 3)*100),"%")
#   }
# 
# 
# 
#   ###########################
#   # prepare data for ggplot
#   ###########################
# 
#   comp_matrix_df<-reshape2::melt(comp_matrix)
#   colnames(comp_matrix_df) <- c("x", "y", "value")
#   comp_matrix_df$value[is.na(comp_matrix_df$value)]<-"X"
# 
# 
#   if (grid!="white"){ # create a matrix for NA, to exclude from grid
#     na_matrix<-comp_matrix_df[is.na(comp_matrix_df$value),]
#   }
# 
#   ##############################
#   ###     Label variables    ###
#   ##############################
# 
#   if (is.null(label_x)) label_x<- unique(comp_matrix_df$x)
#   if (is.null(label_y)) label_y<- unique(comp_matrix_df$y)
# 
#   #############################
#   # Plot Matrix with ggplot2
#   #############################
# 
# 
#   comparison_plot<-
#     ggplot2::ggplot(comp_matrix_df, ggplot2::aes(x = comp_matrix_df$y, y = comp_matrix_df$x, fill = factor(comp_matrix_df$value, levels = breaks))) +
#     {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1)}+
#     {if (grid == "none") ggplot2::geom_tile()}+
#     {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_matrix, colour = "white", lwd=1,linetype=1)}+
#     {if(missings_x==TRUE) ggplot2::geom_point(data=subset(comp_matrix_df, comp_matrix_df$value=="X"),shape=4, size=5, show.legend = FALSE)}+
#     ggplot2::coord_fixed()+
#     ggplot2::scale_fill_manual(values=colors, name="", na.translate = FALSE, drop=FALSE)+
#     ggplot2::scale_y_discrete(name="", limits = rev(levels(comp_matrix_df$x)), labels= label_x, breaks=unique(comp_matrix_df$x), drop=FALSE)+
#     ggplot2::scale_x_discrete(name="", limits = levels(comp_matrix_df$y), labels= label_y, breaks=unique(comp_matrix_df$y), drop=FALSE)+
#     ggplot2::theme_classic()+
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle =90, vjust = 0.33, hjust=1),
#                    axis.text.y = ggplot2::element_text(vjust = 0.33, hjust=1),
#                    axis.title.x= ggplot2::element_blank(),
#                    axis.title.y= ggplot2::element_blank(),
#                    plot.caption=ggplot2::element_text(hjust = 0))+
#     ggplot2::ggtitle(plot_title)
# 
#   if(note==TRUE) comparison_plot<-comparison_plot + ggplot2::labs(caption = note_text)
# 
# 
#   if (diff_perc==TRUE) {
#     if (diff_perc_position== "top_left") {
#     comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = -Inf, y = Inf, hjust = 0, vjust = 1, label = diff_summary$label),
#                                                              fill = ggplot2::alpha("white", perc_diff_transparance), color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
#     if (diff_perc_position== "top_right") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = Inf, y = Inf, hjust = 1, vjust = 1, label = diff_summary$label),
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
#     if (diff_perc_position== "bottom_left") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = -Inf, y = -Inf, hjust = 0, vjust = 0, label = diff_summary$label),
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
#     if (diff_perc_position== "bottom_right") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = Inf, y = -Inf, hjust = 1, vjust = 0, label = diff_summary$label),
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
# 
#     }
# 
# 
# 
#   if (plot_data==FALSE) return (comparison_plot)
#   if (plot_data==TRUE) return (comp_matrix_df)
# }
















# #' Plot Multiple multi_compare_objects
# #'
# #' @description
# #' \code{plot_multi_compare} plots multipe \code{multi_compare_objects} together
# #'
# #' @param multi_compare_objects A character vector containing the names of one or more \code{multi_compare_objects}.
# #' Every object will be displayed seperately in \code{facet_warp} of \code{ggplot}.
# #' @param plots_label A character vector of the same lengths as \code{multi_compare_objects}, to name the different objects
# #' in facet_warp of ggplot.
# #' @param plot_title A string containing the title of the vizualization
# #' @param p_value A number between zero and one, used as p-value in sigificance analyses
# #' @param breaks A vector, containing a number of strings, to rename the categories in the legend.
# #' It's possible length depends on the \code{variant}.
# #' @param colors A vector of colors, usable in ggplot, for every break. It's possible length depends on the \code{variant}.
# #' @param plot_data A Logical value. If TRUE, instead of a plot a data frame will be returned, that is used for the plot.
# #' @param variant Variant can be either "one", "two", "three","four","five", or "six".
# #' \itemize{
# #' \item{'variant = "one"'}{The plot will show whether the coefficients in the regression models are
# #' significantly different from each other (Diff). When they are, it will also show if
# #' they differ in strength (one is twice the size of the other) or direction as well (Large Diff).}
# #'
# #' \item{'variant = "two"'} {The plot will show whether coefficients in the regression models differ significantly
# #' from each other (Large Diff). If not it will show whether they still differ in direction
# #' (Diff in Direction) or whether one is significant while the other is not (Diff in Significance).}
# #'
# #' \item{'variant = "three"'} {The plot will show whether coefficients in the regression models differ from
# #' each other on various aspects. Whether one is significant, while the other is not (Diff in Significance),
# #' whether they differ in direction (Diff in Direrction) or whether one is double the size of the other (Diff in Strength).
# #' When variables meet the criteria for multiple categories they will classified in the latest fitting category.}
# #'
# #' \item{'variant = "four"'} {The plot will show if the coefficient in the df is significant,
# #' while the coefficient is not significant in the benchmark or the other way around  (Diff in Significance).}
# #'
# #' \item{'variant = "five"'} {The plot will show if the coefficient in the df is positive,
# #' while the coefficient in the benchmark is negative or the other way around  (Diff in Direction).}
# #'
# #' \item{'variant = "six"'} {The plot will show if the coefficient in the df is double the size of the
# #' coefficient in the benchmark or the other way around  (Diff in Strength).}}
# #'
# #' @param note A Logical value. If TRUE, a note will be displayed under the plot describing the \code{variant}
# #' @param grid A string, that can either be "none" or a color, for the edges of every tile. If "none", no grid will be displayed.
# #' @param diff_perc A logical value. If TRUE, the Percent of the differing Categories, decided by the variant, will be displayed
# #' in the corner of the plot.
# #' @param diff_perc_size A Number du decide the size of the text in \code{diff_perc}
# #' @param perc_diff_transparance A Number between zero and one, to decide the background transparancy of \code{diff_perc}
# #' @param diff_perc_position A character string, to choose the position of \code{diff_perc} Can either be "top_right"(default),
# #' "bottom_right","bottom_left", or "top_left"
# #' @param gradient A Logical Value. If True, the transperancy of the tiles depends of the coefficient
# #' @param sum_weights_indep,sum_weights_dep A vector of weights for every dependent or independent variable. Must be NULL,
# #' or the same lengths as the independent variables or independent variables.
# #' @param p_adjust If TRUE results based on ajusted p-values will be used. Adjustment methods depend on 
# #' the method used to generate the \code{multi_compare_objects}.
# #' @param label_x,label_y A character string or vector of character strings containing a label for
# #' the x-axis and y-axis.
# #' @param missings_x If TRUE, missing pairs in the plot will be marked with an X.
# #' 
# #' 
# #'@return Returns a a heatmatrix-like plot created with ggplot, to vizualize the multivariate differences. If multiple objects are used, they
# #'will be displayed seperatly with ggplot's facet_wrap function. On the y-axis the indepent variables are displayed,
# #'while on the x-axis the independent variables are displayed. Depending on the variant, the displayed tile colors
# #'must be interpreted differently. For more information on interpretation look at \code{variant}.
# #'
# #'
# #' @examples
# #' 
# #' ## Get Data for comparison
# #' card<-wooldridge::card
# #' 
# #' south <- card[card$south==1,]
# #' north <- card[card$south==0,]
# #' black <- card[card$black==1,]
# #' white <- card[card$black==0,]
# #' 
# #' ## use the function to plot the data
# #' multi_data1 <- sampcompR::multi_compare(df = north, 
# #'                                         bench = south,
# #'                                         independent = c("age","fatheduc","motheduc","IQ"),
# #'                                         dependent = c("educ","wage"),
# #'                                         method = "ols") 
# #'                                      
# #' multi_data2 <- sampcompR::multi_compare(df = black, 
# #'                                         bench = white,
# #'                                         independent = c("age","fatheduc","motheduc","IQ"),
# #'                                         dependent = c("educ","wage"),
# #'                                         method = "ols") 
# #'                                      
# #' plot_multi_compare(c("multi_data1","multi_data2"))
# #'
# #'
# #' @export
# 
# plot_multi_compare<-function(multi_compare_objects,plots_label=NULL, plot_title=NULL,
#                          p_value=0.05, breaks=NULL,plot_data=FALSE, colors=NULL, variant="one", p_adjust=NULL,
#                          note=FALSE, grid="white", diff_perc=TRUE, diff_perc_size=4.5,
#                          perc_diff_transparance=0, diff_perc_position= "top_right", gradient=FALSE,
#                          sum_weights_indep=NULL,sum_weights_dep=NULL, label_x=NULL, label_y=NULL,
#                          missings_x=TRUE){
# 
# 
# 
#   ######################
#   ### prepare breaks ###
#   ######################
# 
#   if(is.null(breaks)){
#     if(variant=="one") breaks <- c("Same", "Small Diff", "Large Diff")
#     if(variant=="two") breaks <- c("Same", "Diff in Direrction","Diff in Significance", "Significant Difference")
#     if(variant=="three") breaks <- c("Same", "Diff in Strength", "Diff in Direrction","Diff in Significance")
#     if(variant=="four") breaks<- c("Same", "Diff in Significance")
#     if(variant=="five") breaks<- c("Same", "Diff in Direction")
#     if(variant=="six") breaks<- c("Same", "Diff in Stength")
#   }
# 
#   ###############################
#   ### prepare colors and note ###
#   ###############################
# 
#   if (variant=="one"){
#     if (is.null(colors)) colors <- c("green", "yellow", "red")
#     note_text<- "Note: Same (green) means that the coeficients are not significant different. \nDifferent (yellow) means, at least one is significant >0 or <0 and both are significant different from each other. \nHigh Difference (red) means all conditions for Difference are true and the coeficients differ in direction \nor one is double the value of the other. \nLevel of Significance is p < 0.05."
#   }
# 
#   if (variant=="two"){
#     if (is.null(colors)) colors <- c("green", "yellow","orange", "red")
#     note_text<-"Note: Same (green) means no difference of any type. \nDiff in direction (yellow) means no significant difference between coefs, but they differ in direction. \nDiff in Significance (orange) means no significant difference but one is significant < or > 0 while the other is not. \nSignificant Difference (red) means, both coeficients differ significant from each other. \nLevel of Significance is p < 0.05"
#   }
# 
#   if (variant=="three"){
#     if (is.null(colors)) colors <- c("green", "yellow","orange", "red")
#     note_text<-"Note: Same (green) means there is no meaningful difference of any type. \nDiff in Strength (yellow) means that one coef is > double the value of the other. \nDiff in Direction (orange) means that one coef is positive while the other is negative \nDiff in Significance (red) means that one is significant < or > 0 while the other is not. \nLevel of Significance is p < 0.05"
#   }
# 
#   if (variant=="four"){
#     if (is.null(colors)) colors <- c("green", "red")
#     note_text<-"Note: Same (green) means there is no difference in significance. \nDiff in Significance (red) means that one coefcient is significant < or > 0 while the other is not. \nLevel of Significance is p < 0.05"
#   }
# 
#   if (variant=="five"){
#     if (is.null(colors)) colors <- c("green", "red")
#     note_text<-"Note: Same (green) means there is no difference in direction. \nDiff in Direction (red) means that one coeficient is <0 while the other is >0."
#   }
# 
#   if (variant=="six"){
#     if (is.null(colors)) colors <- c("green", "red")
#     note_text<-"Note: Same (green) means there is no difference in strength. \nDiff in Strength (red) means that one coeficient has double the value of the other."
#   }
# 
#   #########################
#   ### prepare plot data ###
#   #########################
# 
# 
#   plot_df<-NULL
#   summary_df<-data.frame("samp"=NA,"label"=NA)
# 
#   for (i in 1:length(multi_compare_objects)){
# 
#     curr_df<-get(multi_compare_objects[i])
# 
# 
# 
#     help<-multi_reg_plotter(multi_reg_object=curr_df, plot_title=plot_title,
#                             p_value=p_value, breaks=breaks, colors=colors, variant=variant, p_adjust=p_adjust,
#                             note=note, diff_perc=diff_perc, diff_perc_size=diff_perc_size,
#                             plot_data=TRUE, missings_x=missings_x)
# 
# 
# 
#     ####################
#     ### add gradient ###
#     ####################
# 
#     gradient_df<-reshape2::melt(curr_df[[4]])
#     help$gradient<- gradient_df[,3]
# 
#     ########################
#     ### Add name of plot ###
#     ########################
# 
#     if (is.null(plots_label)) help$samp<-multi_compare_objects[i]
#     if (is.null(plots_label)==FALSE) help$samp<-plots_label[i]
# 
#     ##########################
#     ### add plots together ###
#     ##########################
# 
#     if (is.null(plot_df)==FALSE) plot_df<-rbind(plot_df,help)
#     if(is.null(plot_df)) plot_df=help
#     }
# 
#   ##########################
#   ### add X for missings ###
#   ##########################
# 
#   plot_df<-empty_finder2(plot_df)
# 
# 
#   ##########################################
#   ### Calculate percentage of difference ###
#   ##########################################
# 
#   if(diff_perc==TRUE) {
#   #  percental_difference_b1<-length(help$value[help$value == breaks[1] & is.na(help$value)==FALSE ])/ length(help$value[is.na(help$value)==FALSE])
#   #  percental_difference_b2<-length(help$value[help$value == breaks[2] & is.na(help$value)==FALSE ])/ length(help$value[is.na(help$value)==FALSE])
#   #  if (length(breaks)>2) percental_difference_b3<-length(help$value[help$value == breaks[3] & is.na(help$value)==FALSE ])/ length(help$value[is.na(help$value)==FALSE])
#   #  if (length(breaks)>3) percental_difference_b4<-length(help$value[help$value == breaks[4] & is.na(help$value)==FALSE ])/ length(help$value[is.na(help$value)==FALSE])
# 
#   #  diff_summary<-paste(breaks[1]," :",(round((percental_difference_b1), digits = 3)*100),"% \n",
#   #                      breaks[2]," :",(round(percental_difference_b2, digits = 3)*100),"%")
#   #  if (length(breaks)>2) diff_summary<-paste (diff_summary, "\n",breaks[3], " :", (round(percental_difference_b3, digits = 3)*100),"%")
#   #  if (length(breaks)>3) diff_summary<-paste (diff_summary, "\n",breaks[4], " :", (round(percental_difference_b4, digits = 3)*100),"%")
#   #}
# 
#     summary_df<-difference_summary2(results_object=plot_df,breaks=breaks,sum_weights_indep=sum_weights_indep,sum_weights_dep=sum_weights_dep)}
# 
#   #if (is.null(plots_label)) summary_df[i,]<- c(multi_compare_objects[i], diff_summary)
#   #if (is.null(plots_label)==FALSE) summary_df[i,]<- c(plots_label[i], diff_summary)
# 
# 
# 
#   ##############################
#   ###     Label variables    ###
#   ##############################
# 
#   if (is.null(label_x)) label_x<- unique(plot_df$x)
#   if (is.null(label_y)) label_y<- unique(plot_df$y)
# 
# 
#   #######################################
#   ### reorder plots to original order ###
#   #######################################
# 
#   if (is.null(plots_label)) plot_df$samp <- factor(plot_df$samp, levels = multi_compare_objects)
#   if (is.null(plots_label)==FALSE) plot_df$samp <- factor(plot_df$samp, levels = plots_label)
# 
# 
#   ############
#   ### plot ###
#   ############
# 
#   comparison_plot<-
#     ggplot2::ggplot(data=plot_df, ggplot2::aes(x = plot_df[,"y"], y = plot_df[,"x"], fill = factor(plot_df[,"value"], levels = breaks))) +
#     {if (gradient==TRUE) ggplot2::aes(alpha= as.numeric(gradient))}+
#     ggplot2::geom_tile(colour= grid, lwd =1,linetype=1)+
#     {if(nrow(plot_df[plot_df$value=="X",])>0 & missings_x==TRUE) ggplot2::geom_point(data=plot_df[plot_df$value=="X",],
#                                                                   x=plot_df[plot_df$value=="X",]$y,
#                                                                   y=plot_df[plot_df$value=="X",]$x,
#                                                                   fill=factor(plot_df[plot_df$value=="X",]$value, levels = breaks),
#                                                                   shape=4, show.legend = FALSE)}+
#     #ggplot2::geom_point(data=subset(plot_df,value=="X"),shape=4, show.legend = FALSE)+
#     ggplot2::coord_fixed()+
#     ggplot2::scale_fill_manual(values= colors, name="", na.translate = FALSE)+
#     ggplot2::scale_y_discrete(name="", limits = rev(levels(plot_df$x)), labels= label_x, breaks=unique(plot_df$x))+
#     ggplot2::scale_x_discrete(name="", limits = levels(plot_df$y), labels= label_y, breaks=unique(plot_df$y))+
#     ggplot2::theme_classic()+
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.33, hjust=1),
#                    axis.text.y = ggplot2::element_text(vjust = 0.33, hjust=1),
#                    axis.title.x= ggplot2::element_blank(),
#                    axis.title.y= ggplot2::element_blank(),
#                    plot.caption=ggplot2::element_text(hjust = 0))+
#     ggplot2::ggtitle(plot_title)+
#     ggplot2::guides(alpha="none")+
#     ggplot2::facet_wrap(~ factor(samp))
# 
# 
#   if(note==TRUE) comparison_plot<-comparison_plot + ggplot2::labs(caption = note_text)
# 
# 
#   #if (diff_perc==TRUE) {
#   #  comparison_plot <- comparison_plot + ggplot2::geom_label(x=Inf, y=Inf,
#   #                                                           ggplot2::aes(label = label,  hjust = 1, vjust = 1), data=summary_df,
#   #                                                           fill = ggplot2::alpha("white", perc_diff_transparance), color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
# 
#   if (diff_perc==TRUE) {
#     if (diff_perc_position== "top_left") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = -Inf, y = Inf, hjust = 0, vjust = 1), data=summary_df, 
#                                                                label=summary_df$label,
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), 
#                                                                color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
#     if (diff_perc_position== "top_right") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = Inf, y = Inf, hjust = 1, vjust = 1),  data=summary_df, 
#                                                                label=summary_df$label,
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), 
#                                                                color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
#     if (diff_perc_position== "bottom_left") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = -Inf, y = -Inf, hjust = 0, vjust = 0), data=summary_df, 
#                                                                label=summary_df$label,
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), 
#                                                                color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
#     if (diff_perc_position== "bottom_right") {
#       comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = Inf, y = -Inf, hjust = 1, vjust = 0), data=summary_df, 
#                                                                label=summary_df$label,
#                                                                fill = ggplot2::alpha("white", perc_diff_transparance), 
#                                                                color = ggplot2::alpha("black", 1), size= diff_perc_size)}
# 
# 
#   }
# 
#   if (plot_data==TRUE) return(plot_df)
#   if (plot_data==FALSE) return(comparison_plot)
# 
# }




empty_finder2<-function(df){

  varnames1<-as.character(unique(df$x))
  varnames2<-as.character(unique(df$y))
  sampnames<-as.character(unique(df$samp))

  for (i in 1:length (varnames1)){

    v1<-varnames1[i]
    for (j in 1:length (varnames2)){
      v2<-varnames2[j]
      for (k in 1:length (sampnames)) {
        v3<-sampnames[k]
        if ((length(df$value[df[,1]==varnames1[i] & df[,2]==varnames2[j] & df[,5]==sampnames[k]])==0) &
            (any((df[,1]==varnames1[i] & df[,2]==varnames2[j] & df[,5]!=sampnames[k])))) df<-rbind(df, c(varnames1[i],varnames2[j],"X",NA,sampnames[k]))
        if ((length(df$value[df[,1]==varnames1[i] & df[,2]==varnames2[j] & df[,5]==sampnames[k]])==0) &
            (any((df[,1]==varnames1[i] & df[,2]==varnames2[j] & is.na(df[,3]) & df[,5]!=sampnames[k])))==FALSE) df<-rbind(df, c(varnames1[i],varnames2[j],"X",NA,sampnames[k]))

      }

    }

  }


  return (df)
}



# 
# 
# difference_summary2<-function(results_object,breaks,sum_weights_indep=NULL,sum_weights_dep=NULL){
# 
#   ### prepare needed variables ###
#   varnames1<-as.character(unique(results_object$y))
#   varnames2<-as.character(unique(results_object$x))
#   samps<-as.character(unique(results_object$samp))
#   results_object$sum_weight<-NA
#   summary_df<-data.frame("samp"=NA,"label"=NA)
# 
#   ### check for sum_weights_indep ###
#   if (is.null(sum_weights_indep)) {
#     sum_weights_indep<-matrix(data=1, nrow=length(samps), ncol=length(varnames2))
#   }
# 
#   ### check for sum_weights_dep ###
#   if (is.null(sum_weights_dep)) {
#     sum_weights_dep<-matrix(data=1, nrow=length(samps), ncol=length(varnames1))
#   }
# 
#   for (i in 1:length(samps)){
# 
# 
#     help_matrix<-matrix(NA, nrow=length(varnames2), ncol=length(varnames1))
#     colnames(help_matrix)<-varnames1
#     rownames(help_matrix)<-varnames2
# 
#     ### build a weight matrix ###
#     for (f in 1:length(varnames1)){
#       for (g in 1:length(varnames2)){
#         help_matrix[g,f]<-sum_weights_dep[i,][f]*sum_weights_indep[i,][g]
#       }
#     }
# 
#     ### turn weight matrix to df ###
#     help_matrix_df<-reshape2::melt(help_matrix)
#     help_matrix_df$samp<-samps[i]
#     #colnames(help_matrix_df)<-c("x","y","sum_weight","samp")
#     #return(help_matrix_df)
#     #return(help_matrix_df)
# 
#     ### add help_matrix to results_object ###
#     results_object$sum_weight[results_object$samp==samps[i]]<-help_matrix_df$value
#     results_object$sum_weight[is.na(results_object$value)]<-NA
# 
# 
# 
#     ### build a summary for every sample ###
# 
#     percental_difference_b1<-sum(results_object$sum_weight[results_object$value == breaks[1] & is.na(results_object$value)==FALSE
#                                                            & results_object$samp==samps[i] & results_object$value != "X"])/
#       sum(results_object$sum_weight[is.na(results_object$value)==FALSE & results_object$samp==samps[i] & results_object$value != "X"])
#     percental_difference_b2<-sum(results_object$sum_weight[results_object$value == breaks[2] & is.na(results_object$value)==FALSE
#                                                            & results_object$samp==samps[i] & results_object$value != "X"])/
#       sum(results_object$sum_weight[is.na(results_object$value)==FALSE & results_object$samp==samps[i] & results_object$value != "X"])
#     if (length(breaks)>2) {
#       percental_difference_b3<-sum(results_object$sum_weight[results_object$value == breaks[3] & is.na(results_object$value)==FALSE
#                                                              & results_object$samp==samps[i] & results_object$value != "X"])/
#         sum(results_object$sum_weight[is.na(results_object$value)==FALSE & results_object$samp==samps[i] & results_object$value != "X"])}
# 
#     diff_summary<-paste(breaks[1]," :",(round((percental_difference_b1), digits = 3)*100),"% \n",
#                         breaks[2]," :",(round(percental_difference_b2, digits = 3)*100),"%")
#     if (length(breaks)>2) diff_summary<-paste (diff_summary, "\n",breaks[3], " :", (round(percental_difference_b3, digits = 3)*100),"%")
# 
#     summary_df[i,]<- c(samps[i], diff_summary)
#   }
# 
#   return(summary_df)
# }







dependent_checker<-function(dependent, df, dfname){

  ### check if dependent variable is in df ###
  dependent2<-dependent[dependent %in% colnames(df)]

  if (length(dependent)>length(dependent2)){
    missing<- dependent[!dependent %in% dependent2]
    #inside <- labelchecker(target_df, source_df, variables = variables)

    ### Put Missing variables in string together for a warning message ###
    missingvar <- paste(missing[1])

    if(length(missing)>=2){
    for (i in 2:length(missing)) {
      missingvar <- paste(missingvar, "|", missing[i], sep = " ")
    }}


    if (length(missingvar)>0) {
        warning(paste(
          dfname, "does not contain all dependent variables",
          "\n   Only variables included",dfname, "are used",
          "\n   Missing variables are:", missingvar))
    }
    
    dependent<-dependent2
  }

return(dependent)


}



# multireg_merge<-function(multi_reg_object1, multi_reg_object2, p_adjust=TRUE){
# 
#   for (i in 2:16) {
# 
#     if (i<=7 | i>13){
#       multi_reg_object1[[i]]<-cbind(multi_reg_object1[[i]],multi_reg_object2[[i]])
#     }
# 
#     if (i>=8 & i<11){
#       multi_reg_object1[[i]]<- matrix(stats::p.adjust(p = multi_reg_object1[[i-3]], method = "bonferroni"),
#                                       ncol = ncol(multi_reg_object1[[i-3]]))
#       colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
#       rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
#     }
#     if (i>=11 & i<14) {
#       if (p_adjust==TRUE ){
#         help<- formatC(multi_reg_object1[[i-9]], format = "e", digits = 2)
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]>0.05]<-
#           paste(help[multi_reg_object1[[i-3]]>0.05], "   ", sep = "")
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.05 & multi_reg_object1[[i-3]]>=0.01]<-
#           paste(help[multi_reg_object1[[i-3]]<0.05 & multi_reg_object1[[i-3]]>=0.01], "*  ", sep = "")
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.01 & multi_reg_object1[[i-3]]>=0.001]<-
#           paste(help[multi_reg_object1[[i-3]]<0.01 & multi_reg_object1[[i-3]]>=0.001], "** ", sep = "")
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.001]<-
#           paste(help[multi_reg_object1[[i-3]]<0.001], "***", sep = "")
#         multi_reg_object1[[i]][help>0]<-paste(" ",multi_reg_object1[[i]][help>0],sep = "")
# 
#         multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-3]]), nrow = nrow(multi_reg_object1[[i-3]])))
#         colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
#         rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
#       }
#       if (p_adjust==FALSE) {
#         help<- formatC(multi_reg_object1[[i-9]], format = "e", digits = 2)
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]>0.05]<-
#           paste(help[multi_reg_object1[[i-6]]>0.05], "   ", sep = "")
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01]<-
#           paste(help[multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01], "*  ", sep = "")
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001]<-
#           paste(help[multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001], "** ", sep = "")
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.001]<-
#           paste(help[multi_reg_object1[[i-6]]<0.001], "***", sep = "")
#         multi_reg_object1[[i]][help>0]<-paste(" ",multi_reg_object1[[i]][help>0],sep = "")
# 
#         multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-3]]), nrow = nrow(multi_reg_object1[[i-3]])))
#         colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
#         rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
#       }
#     }
#   }
# 
#   if(p_adjust==FALSE) multi_reg_object1[[17]]<-length(multi_reg_object1[[7]][multi_reg_object1[[7]]>0.05])/length(multi_reg_object1[[7]])
#   if(p_adjust==FALSE) multi_reg_object1[[18]]<-length(multi_reg_object1[[7]][multi_reg_object1[[7]]<0.05])/length(multi_reg_object1[[7]])
#   if(p_adjust==TRUE)  multi_reg_object1[[17]]<-length(multi_reg_object1[[10]][multi_reg_object1[[10]]>0.05])/length(multi_reg_object1[[10]])
#   if(p_adjust==TRUE)  multi_reg_object1[[18]]<-length(multi_reg_object1[[10]][multi_reg_object1[[10]]<0.05])/length(multi_reg_object1[[10]])
# 
#   multi_reg_object1[[19]]<-c(multi_reg_object1[[19]],multi_reg_object2[[19]])
# 
#   multi_reg_object1
# }





#' Combine multi_compare_objects
#'
#' @description
#' \code{multi_compare_merge} combines two \code{multi_compare_objects} to plot them together.
#'
#' @param multi_reg_object1,multi_reg_object2 Multireg objects that should be combined.
#' @param p_adjust A logical input or character string indicating an adjustment 
#' method that isusable in the \code{method} parameter of 
#' \code{\link[stats]{p.adjust}}. If set to TRUE the Bonferroni adjusted 
#' p-values are used in inference.
#'
#' @return A combined \code{multi_reg_object} that can be used in plot functions to 
#' create a visualization.
#' 
#' @examples
#' 
#' ## Get Data for comparison
#' card<-wooldridge::card
#' 
#' north <- card[card$south==0,]
#' white <- card[card$black==0,]
#' 
#' ## use the function to plot the data
#' multi_data1 <- sampcompR::multi_compare(df = north, 
#'                                         bench = card,
#'                                         independent = c("age","fatheduc","motheduc","IQ"),
#'                                         dependent = c("educ"),
#'                                         family = "ols") 
#'                                      
#' multi_data2 <- sampcompR::multi_compare(df = white, 
#'                                         bench = card,
#'                                         independent = c("age","fatheduc","motheduc","IQ"),
#'                                         dependent = c("wage"),
#'                                         family = "ols") 
#'  ### merge two objects ###                                       
#'  merged_object<-multi_compare_merge(multi_data1,multi_data2)
#'  
#'  ### Plot the merged object ###
#'  plot_multi_compare("merged_object")                                       
#' 
#' @export
multi_compare_merge <- function(multi_reg_object1, multi_reg_object2, p_adjust=FALSE){
  
  if(!is.null(multi_reg_object1[[1]])| !is.null(multi_reg_object2[[1]])) {
    multi_reg_object1[[1]]<-c(multi_reg_object1[[1]],multi_reg_object2[[1]])}
  
  for (i in 2:16) {
    
    if (i<=7 | i>13){
      data1<-as.data.frame(multi_reg_object1[[i]])
      data2<-as.data.frame(multi_reg_object2[[i]])
      data2[rownames(data1)[!rownames(data1) %in% rownames(data2)],]<-NA
      data1[rownames(data2)[!rownames(data2) %in% rownames(data1)],]<-NA
      
      #data1$varnames<-multi_reg_object1[[20]]
      #data2$varnames<-multi_reg_object2[[20]]
      
      
      merged<-merge(data1,data2, by = 0,all=TRUE,sort=FALSE)
      rownames(merged)<-merged$Row.names
      merged$Row.names<-NULL
      
      
      multi_reg_object1[[i]]<-as.matrix(merged)
      #rownames(multi_reg_object1[[i]])<-multi_reg_object2[[20]]
      #colnames(multi_reg_object1[[i]])<-c(multi_reg_object1$dependent,multi_reg_object2$dependent)
      
      #multi_reg_object1[[i]]<-cbind(multi_reg_object1[[i]],multi_reg_object2[[i]])
    }
    
    if (i>=8 & i<11){
      data1<-as.data.frame(multi_reg_object1[[i]])
      data2<-as.data.frame(multi_reg_object2[[i]])
      data2[rownames(data1)[!rownames(data1) %in% rownames(data2)],]<-NA
      data1[rownames(data2)[!rownames(data2) %in% rownames(data1)],]<-NA
      
      
      #data1$varnames<-multi_reg_object1[[20]]
      #data2$varnames<-multi_reg_object2[[20]]
      
      merged<-merge(data1,data2, by = 0,all=TRUE,sort=FALSE)
      rownames(merged)<-merged$Row.names
      merged$Row.names<-NULL
      
      multi_reg_object1[[i]]<-as.matrix(merged)
      #rownames(multi_reg_object1[[i]])<-multi_reg_object2[[20]]
      #colnames(multi_reg_object1[[i]])<-c(multi_reg_object1$dependent,multi_reg_object2$dependent)
      
      if (p_adjust==TRUE & is.character(p_adjust)==TRUE) p_method<-p_adjust
      else p_method<-"bonferroni"
      
      for (j in 1:nrow(multi_reg_object1[[i]])){
      multi_reg_object1[[i]][j,]<- matrix(stats::p.adjust(p = multi_reg_object1[[i-3]][j,], method = p_method,
                                                          n= ncol(multi_reg_object1[[i]])),
                                      ncol = ncol(multi_reg_object1[[i-3]]))}
      
      colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
      rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
      
    }
    if (i>=11 & i<13) {
      
      help<- formatC(multi_reg_object1[[i-9]], format = "e", digits = 2)
      
      
      multi_reg_object1[[i]][multi_reg_object1[[i-6]]>=0.05 & is.na(multi_reg_object1[[i-6]])==FALSE]<-
        paste(help[multi_reg_object1[[i-6]]>=0.05 & is.na(multi_reg_object1[[i-6]])==FALSE], "   ", sep = "")
      
      multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01 & is.na(multi_reg_object1[[i-6]])==FALSE]<-
        paste(help[multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01 & is.na(multi_reg_object1[[i-6]])==FALSE], "*  ", sep = "")
      
      multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001 & is.na(multi_reg_object1[[i-6]])==FALSE]<-
        paste(help[multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001 & is.na(multi_reg_object1[[i-6]])==FALSE], "** ", sep = "")
      
      multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.001 & is.na(multi_reg_object1[[i-6]])==FALSE]<-
        paste(help[multi_reg_object1[[i-6]]<0.001 & is.na(multi_reg_object1[[i-6]])==FALSE], "***", sep = "")
      multi_reg_object1[[i]][help>0]<-paste(" ",multi_reg_object1[[i]][help>0],sep = "")
      
      multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-6]]), nrow = nrow(multi_reg_object1[[i-6]])))
      colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-6]])
      rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-6]])
      
      
      
      
    }
    
    if (i==13){
      
      if (p_adjust==TRUE | is.character(p_adjust)==TRUE){
        help<- formatC(multi_reg_object1[[4]], format = "e", digits = 2)
        
        
        multi_reg_object1[[i]][multi_reg_object1[[i-3]]>=0.05 & is.na(multi_reg_object1[[i-3]])==FALSE]<-
          paste(help[multi_reg_object1[[i-3]]>=0.05 & is.na(multi_reg_object1[[i-3]])==FALSE], "   ", sep = "")
        
        multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.05 & multi_reg_object1[[i-3]]>=0.01 & is.na(multi_reg_object1[[i-3]])==FALSE]<-
          paste(help[multi_reg_object1[[i-3]]<0.05 & multi_reg_object1[[i-3]]>=0.01 & is.na(multi_reg_object1[[i-3]])==FALSE], "*  ", sep = "")
        
        multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.01 & multi_reg_object1[[i-3]]>=0.001 & is.na(multi_reg_object1[[i-3]])==FALSE]<-
          paste(help[multi_reg_object1[[i-3]]<0.01 & multi_reg_object1[[i-3]]>=0.001 & is.na(multi_reg_object1[[i-3]])==FALSE], "** ", sep = "")
        
        multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.001 & is.na(multi_reg_object1[[i-3]])==FALSE]<-
          paste(help[multi_reg_object1[[i-3]]<0.001 & is.na(multi_reg_object1[[i-3]])==FALSE], "***", sep = "")
        multi_reg_object1[[i]][help>0]<-paste(" ",multi_reg_object1[[i]][help>0],sep = "")
        
        multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-3]]), nrow = nrow(multi_reg_object1[[i-3]])))
        colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
        rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
      }
      
      if (p_adjust==FALSE) {
        help<- formatC(multi_reg_object1[[4]], format = "e", digits = 2)
        #help[help==" NA"]<-NA
        #multi_reg_object1[[i]]<-c(rep(NA,28))
        
        multi_reg_object1[[i]][multi_reg_object1[[i-6]]>=0.05 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE]<-
          paste(help[multi_reg_object1[[i-6]]>=0.05 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE], "   ", sep = "")
        
        multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE]<-
          paste(help[multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE], "*  ", sep = "")
        
        multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE]<-
          paste(help[multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE], "** ", sep = "")
        
        multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.001 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE]<-
          paste(help[multi_reg_object1[[i-6]]<0.001 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE] , "***", sep = "")
        
        multi_reg_object1[[i]][help>0 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE]<-paste(" ",multi_reg_object1[[i]][help>0 & !is.na(help) & is.na(multi_reg_object1[[i-6]])==FALSE],sep = "")
        
        
        multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-3]]), nrow = nrow(multi_reg_object1[[i-3]])))
        colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
        rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
      }
    }
    
  }
  
  if(p_adjust==FALSE) multi_reg_object1[[17]]<-length(multi_reg_object1[[7]][multi_reg_object1[[7]]>=0.05 & !is.na(multi_reg_object1[[7]])])/
      length(multi_reg_object1[[7]][!is.na(multi_reg_object1[[7]])])
  if(p_adjust==FALSE) multi_reg_object1[[18]]<-length(multi_reg_object1[[7]][multi_reg_object1[[7]]<0.05 & !is.na(multi_reg_object1[[7]])])/
      length(multi_reg_object1[[7]][!is.na(multi_reg_object1[[7]])])
  if(p_adjust==TRUE)  multi_reg_object1[[17]]<-length(multi_reg_object1[[10]][multi_reg_object1[[10]]>=0.05 & !is.na(multi_reg_object1[[7]])])/
      length(multi_reg_object1[[10]][!is.na(multi_reg_object1[[7]])])
  if(p_adjust==TRUE)  multi_reg_object1[[18]]<-length(multi_reg_object1[[10]][multi_reg_object1[[10]]<0.05 & !is.na(multi_reg_object1[[7]])])/
      length(multi_reg_object1[[10]][!is.na(multi_reg_object1[[7]])])
  
  multi_reg_object1[[19]]<-c(multi_reg_object1[[19]],multi_reg_object2[[19]])
  multi_reg_object1[[20]]<-unique(c(multi_reg_object1[[20]],multi_reg_object2[[20]]))
  
  
  
  multi_reg_object1
}


# multi_compare_merge_old <- function(multi_reg_object1, multi_reg_object2, p_adjust=FALSE){
# 
#   multi_reg_object1[[1]]<-c(multi_reg_object1[[1]],multi_reg_object2[[1]])
# return(multi_reg_object1)
#   for (i in 2:16) {
# 
#     if (i<=7 | i>13){
#       data1<-as.data.frame(multi_reg_object1[[i]])
#       data2<-as.data.frame(multi_reg_object2[[i]])
#       data2[rownames(data1)[!rownames(data1) %in% rownames(data2)],]<-NA
#       data1[rownames(data2)[!rownames(data2) %in% rownames(data1)],]<-NA
#       
#       data1$varnames<-multi_reg_object1[[20]]
#       data2$varnames<-multi_reg_object2[[20]]
# 
# 
#       merged<-dplyr::inner_join(data1,data2, by = "varnames")
#       merged$varnames<-NULL
# 
#       multi_reg_object1[[i]]<-as.matrix(merged)
#       rownames(multi_reg_object1[[i]])<-multi_reg_object2[[20]]
#       colnames(multi_reg_object1[[i]])<-c(multi_reg_object1$dependent,multi_reg_object2$dependent)
# 
#       #multi_reg_object1[[i]]<-cbind(multi_reg_object1[[i]],multi_reg_object2[[i]])
# 
#     }
# 
#     if (i>=8 & i<11){
#       data1<-as.data.frame(multi_reg_object1[[i]])
#       data2<-as.data.frame(multi_reg_object2[[i]])
#       data2[rownames(data1)[!rownames(data1) %in% rownames(data2)],]<-NA
#       data1[rownames(data2)[!rownames(data2) %in% rownames(data1)],]<-NA
# 
# 
#       data1$varnames<-multi_reg_object1[[20]]
#       data2$varnames<-multi_reg_object2[[20]]
# 
#       merged<-dplyr::inner_join(data1,data2, by = "varnames")
#       merged$varnames<-NULL
# 
#       multi_reg_object1[[i]]<-as.matrix(merged)
#       rownames(multi_reg_object1[[i]])<-multi_reg_object2[[20]]
#       colnames(multi_reg_object1[[i]])<-c(multi_reg_object1$dependent,multi_reg_object2$dependent)
# 
#       if(p_adjust==TRUE)p_method<-p_adjust
#       else p_method<-"bonferroni"
# 
#       multi_reg_object1[[i]]<- matrix(stats::p.adjust(p = multi_reg_object1[[i-3]], method = p_method),
#                                       ncol = ncol(multi_reg_object1[[i-3]]))
#       colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
#       rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
# 
#     }
#     if (i>=11 & i<13) {
# 
#         help<- formatC(multi_reg_object1[[i-9]], format = "e", digits = 2)
# 
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]>=0.05]<-
#           paste(help[multi_reg_object1[[i-6]]>=0.05], "   ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01]<-
#           paste(help[multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01], "*  ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001]<-
#           paste(help[multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001], "** ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.001]<-
#           paste(help[multi_reg_object1[[i-6]]<0.001], "***", sep = "")
#         multi_reg_object1[[i]][help>0]<-paste(" ",multi_reg_object1[[i]][help>0],sep = "")
# 
#         multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-6]]), nrow = nrow(multi_reg_object1[[i-6]])))
#         colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-6]])
#         rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-6]])
# 
# 
# 
# 
#     }
# 
#     if (i==13){
# 
#       if (p_adjust!=FALSE){
#         help<- formatC(multi_reg_object1[[4]], format = "e", digits = 2)
# 
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]>=0.05]<-
#           paste(help[multi_reg_object1[[i-3]]>=0.05], "   ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.05 & multi_reg_object1[[i-3]]>=0.01]<-
#           paste(help[multi_reg_object1[[i-3]]<0.05 & multi_reg_object1[[i-3]]>=0.01], "*  ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.01 & multi_reg_object1[[i-3]]>=0.001]<-
#           paste(help[multi_reg_object1[[i-3]]<0.01 & multi_reg_object1[[i-3]]>=0.001], "** ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-3]]<0.001]<-
#           paste(help[multi_reg_object1[[i-3]]<0.001], "***", sep = "")
#         multi_reg_object1[[i]][help>0]<-paste(" ",multi_reg_object1[[i]][help>0],sep = "")
# 
#         multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-3]]), nrow = nrow(multi_reg_object1[[i-3]])))
#         colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
#         rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
#       }
# 
#       if (p_adjust==FALSE) {
#         help<- formatC(multi_reg_object1[[4]], format = "e", digits = 2)
#         #help[help==" NA"]<-NA
#         #multi_reg_object1[[i]]<-c(rep(NA,28))
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]>=0.05 & !is.na(help)]<-
#           paste(help[multi_reg_object1[[i-6]]>=0.05 & !is.na(help)], "   ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01 & !is.na(help)]<-
#           paste(help[multi_reg_object1[[i-6]]<0.05 & multi_reg_object1[[i-6]]>=0.01 & !is.na(help)], "*  ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001 & !is.na(help)]<-
#           paste(help[multi_reg_object1[[i-6]]<0.01 & multi_reg_object1[[i-6]]>=0.001 & !is.na(help)], "** ", sep = "")
# 
#         multi_reg_object1[[i]][multi_reg_object1[[i-6]]<0.001 & !is.na(help)]<-
#           paste(help[multi_reg_object1[[i-6]]<0.001 & !is.na(help)] , "***", sep = "")
# 
#         multi_reg_object1[[i]][help>0 & !is.na(help)]<-paste(" ",multi_reg_object1[[i]][help>0 & !is.na(help)],sep = "")
# 
# 
#         multi_reg_object1[[i]]<-noquote(matrix(multi_reg_object1[[i]], ncol = ncol(multi_reg_object1[[i-3]]), nrow = nrow(multi_reg_object1[[i-3]])))
#         colnames(multi_reg_object1[[i]])<-colnames(multi_reg_object1[[i-3]])
#         rownames(multi_reg_object1[[i]])<-rownames(multi_reg_object1[[i-3]])
#       }
#     }
# 
#   }
# 
#   if(p_adjust==FALSE) multi_reg_object1[[17]]<-length(multi_reg_object1[[7]][multi_reg_object1[[7]]>=0.05 & !is.na(multi_reg_object1[[7]])])/
#       length(multi_reg_object1[[7]][!is.na(multi_reg_object1[[7]])])
#   if(p_adjust==FALSE) multi_reg_object1[[18]]<-length(multi_reg_object1[[7]][multi_reg_object1[[7]]<0.05 & !is.na(multi_reg_object1[[7]])])/
#       length(multi_reg_object1[[7]][!is.na(multi_reg_object1[[7]])])
#   if(p_adjust==TRUE)  multi_reg_object1[[17]]<-length(multi_reg_object1[[10]][multi_reg_object1[[10]]>=0.05 & !is.na(multi_reg_object1[[7]])])/
#       length(multi_reg_object1[[10]][!is.na(multi_reg_object1[[7]])])
#   if(p_adjust==TRUE)  multi_reg_object1[[18]]<-length(multi_reg_object1[[10]][multi_reg_object1[[10]]<0.05 & !is.na(multi_reg_object1[[7]])])/
#       length(multi_reg_object1[[10]][!is.na(multi_reg_object1[[7]])])
# 
#   multi_reg_object1[[19]]<-c(multi_reg_object1[[19]],multi_reg_object2[[19]])
# 
#   multi_reg_object1
# }


multi_boot_sub<-function(df,nboots=2000,benchmark,dependent,independent,
                         formula_list=NULL,
                         ids = NULL, id_bench = NULL,
                         weight_df = NULL,weight_bench = NULL,
                         stratas = NULL, strata_bench = NULL, 
                         rm_na = "pairwise",
                         family=stats::gaussian(link = "identity"), 
                         adjustment_vars=NULL,
                         raking_targets=NULL,
                         post_targets=NULL,
                         boot_all=FALSE){
  
  ### Prepare datasets
  if(is.null(formula_list)){
    if (is.null(weight_df)) df<-reduce_df_glm(df, dependent, independent, rm_na = rm_na, adjustment_vars=adjustment_vars)
    if (is.null(weight_df)==FALSE) df<-reduce_df_glm(df, dependent, independent,  weight_var = weight_df, id = ids, rm_na = rm_na,adjustment_vars=adjustment_vars)}
  
  if(is.null(formula_list)==FALSE){
    var_list<-purrr::map(formula_list,all.vars)
    dep_indep<-function(vars,type="dependent"){
      
      if(type=="dependent"){
        out<-vars[1]
      }else{
        out<-vars[2:length(vars)]
      }
      
      out
    }
    dependent<-purrr::map(var_list,~dep_indep(.,"dependent"))
    independent<-purrr::map(var_list,~dep_indep(.,"independent"))
    
    if (is.null(weight_df)) df<-purrr::map2(.x=dependent,.y=independent,~reduce_df_glm(df, .x,.y, rm_na = rm_na, adjustment_vars = adjustment_vars)[[1]])
    if (is.null(weight_df)==FALSE) df<-purrr::map2(.x=dependent,.y=independent,~reduce_df_glm(df, .x,.y,  weight_var = weight_df, id = ids, strata=stratas, rm_na = rm_na, adjustment_vars = adjustment_vars)[[1]])
    
  }
  
  df<-purrr::map(df,~dplyr::mutate(.x,sample_ident=0))
  
  ### Prepare weigthed dfs with and without raking 
  if (is.null(weight_df)==FALSE) {
    design_list<-list()
    
    if(is.null(raking_targets)==TRUE &
       is.null(post_targets)==TRUE) design_list <- weighted_design_glm(df,dependent,weight_var="df_weights", 
                                                                   id="id_df", strata=NULL, 
                                                                   nest=FALSE, type="df1",
                                                                   nboots=nboots)
    
    if(is.null(raking_targets)==FALSE) design_list <- weighted_design_glm(df,dependent,weight_var="df_weights", 
                                                                   id="id_df", strata=NULL, 
                                                                   nest=FALSE, type="df1", 
                                                                   adjustment_vars = adjustment_vars, 
                                                                   raking_targets = raking_targets,
                                                                   nboots=nboots)
    
    if(is.null(post_targets)==FALSE) design_list <- weighted_design_glm(df,dependent,weight_var="df_weights", 
                                                                   id="id_df", strata=NULL, 
                                                                   nest=FALSE, type="df1", 
                                                                   adjustment_vars = adjustment_vars, 
                                                                   post_targets = post_targets,
                                                                   nboots=nboots)

  } else {
    if(is.null(post_targets)==TRUE & 
       is.null(raking_targets)==TRUE) design_list <- weighted_design_glm(df,dependent,weight_var=NULL, 
                                                                    id=NULL, strata=NULL, 
                                                                    nest=FALSE, type="df1",
                                                                    nboots=nboots)
    
    if(is.null(raking_targets)==FALSE) design_list <- weighted_design_glm(df,dependent,weight_var=NULL, 
                                             id=NULL, strata=NULL, 
                                             nest=FALSE, type="df1", 
                                             adjustment_vars = adjustment_vars, 
                                             raking_targets = raking_targets,
                                             nboots=nboots)
    if(is.null(post_targets)==FALSE) design_list <- weighted_design_glm(df,dependent,weight_var=NULL, 
                                                                      id=NULL, strata=NULL, 
                                                                      nest=FALSE, type="df1", 
                                                                      adjustment_vars = adjustment_vars, 
                                                                      post_targets = post_targets,
                                                                      nboots=nboots)}
  
  
  # ### prepare unweighted dfs for raking
  # if(is.null(weight_df)==TRUE  & is.null(adjustment_vars)==FALSE){
  #   ### fix for a random set of ids ###
  #   design_list <- weighted_design_glm(df,dependent,weight_var=NULL, id=NULL, strata=strata, nest=FALSE, type="rake_df1", 
  #                                      adjustment_vars = adjustment_vars,raking_targets = raking_targets,nboots=nboots)
  #   }
  
  
  ### 2 get a list with glm results for both data frames ###
  if(is.null(formula_list)){
  
    glm_list<-list()
    #glm_list[[1]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[1]], type="interact",design_list_df =design_list[[2]])
    glm_list<-run_glm(df_comb = NULL,dependent,independent, design_list =  design_list, type="df1",replicates = TRUE,family = family)
    }
  
if(is.null(formula_list)==FALSE){
    #glm_list<-list()
  
    glm_list<-purrr::map(.x=1:length(dependent),
                         ~run_glm(df_comb = NULL,dependent[[.x]],
                                  independent[[.x]], 
                                  design_list =  design_list[.x], 
                                  type="df1",replicates = TRUE,family=family,
                                  formula_list = formula_list[[.x]]))
  
}
 
  
  get_b_function<-function(glm_list,i,forms=FALSE,max_indep=FALSE){

    if(forms==FALSE){
      #out<-glm_list[[2]][[i]][[1]][-1]
      
      out<-t(glm_list[[i]]$replicates)
      out<-out[2:nrow(out),]
      
    }
    
    if(forms==TRUE){
      out<-t(glm_list[[i]][[1]]$replicates)
      out<-out[2:nrow(out),]
      while(nrow(out)<max_indep){
        out<-rbind(out,NA)
      }
    }
    
    out
  }
  
  ### dataframe coefficients
  
  if(is.null(formula_list)){
  out<-sapply(1:length(dependent),get_b_function, glm_list=glm_list,
              forms=FALSE)
  }
  
  if(is.null(formula_list)==FALSE){
    independent<-purrr::map(formula_list,~attr(terms(.), "term.labels"))
    uni_indep<- unique(unlist(independent))
    max_indep<- length(uni_indep)
    out<-sapply(1:length(unlist(dependent)),get_b_function, glm_list=glm_list,
                forms=TRUE,max_indep=max_indep)
  }
  
  # ### interaction coefficients ###
  # out<-cbind(out,sapply(1:length(dependent),get_b_function, glm_list=glm_list,
  #                       independent = independent,type="interact"))
  
  out
  
  
  
}


  
  
  
  
  
  
  
  
# multi_boot_sub<-function(df,i=NULL,benchmark,dependent,independent,ids = NULL,
#                          id_bench = NULL,weight_df = NULL,weight_bench = NULL,
#                          stratas = NULL, strata_bench = NULL, rm_na = "pairwise",
#                          method = "ols", bootstrap=FALSE, adjustment_vars=NULL,
#                          raking_targets=NULL){
#   
#   if (bootstrap==TRUE) df<-df[i,]
#   
#   
#   if (is.null(weight_df)) df<-reduce_df_glm(df, dependent, independent, rm_na = rm_na, adjustment_vars=adjustment_vars)
#   if (is.null(weight_bench)) benchmark<-reduce_df_glm(benchmark, dependent, independent, rm_na = rm_na)
#   if (is.null(weight_df)==FALSE) df<-reduce_df_glm(df, dependent, independent,  weight_var = weight_df, id = ids, rm_na = rm_na,adjustment_vars=adjustment_vars)
#   if (is.null(weight_bench)==FALSE) benchmark<-reduce_df_glm(benchmark, dependent, independent,  weight_var = weight_bench, id = id_bench, rm_na = rm_na)
#   
#   
#   
#   df_comb<-combine_dfs(df,benchmark,dependent,independent,id=ids,id_bench=id_bench,
#                        weight=weight_df,weight_bench=weight_bench,
#                        strata=stratas,strata_bench=strata_bench, 
#                        adjustment_vars = adjustment_vars)
#   
#   
#   # calculate survey deisgns if weighted
#   
#   if (is.null(weight_df)==FALSE) {
#     design_list<-list()
#     
#     if(is.null(adjustment_vars)==TRUE) design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=NULL, nest=FALSE, type="interact")
#     if(is.null(adjustment_vars)==TRUE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=NULL, nest=FALSE, type="df1")
#     
#     if(is.null(adjustment_vars)==FALSE) design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=NULL, nest=FALSE, type="df1", 
#                                                                         adjustment_vars = adjustment_vars, raking_targets = raking_targets)
#     if(is.null(adjustment_vars)==FALSE) design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var="df_weights", id="id_df", strata=NULL, nest=FALSE, type="interact",
#                                                                         raked_design = design_list[[2]])
#     
#   } else {design_list = list(NULL,NULL,NULL)}
#   
#   if(is.null(weight_df)==TRUE  & is.null(adjustment_vars)==FALSE){
#     ### fix for a random set of ids ###
#     design_list[[2]] <- weighted_design_glm(df_comb,dependent,weight_var=NULL, id=NULL, strata=strata, nest=FALSE, type="rake_df1", 
#                                             adjustment_vars = adjustment_vars,raking_targets = raking_targets)
#     design_list[[1]] <- weighted_design_glm(df_comb,dependent,weight_var=NULL, id=NULL, strata=strata, nest=FALSE, type="rake_interact", 
#                                             raked_design = design_list[[2]])
#     
#     
#     
#   }
#   
#   # if (is.null(weight_df)==FALSE | is.null(weight_bench)==FALSE) {
#   #   design_list2<-list()
#   #   design_list2[[1]] <- weighted_design_glm(df_comb2,dependent,weight_var="df_weights", id="id_df", strata=NULL, nest=FALSE, type="interact")
#   #   design_list2[[2]] <- weighted_design_glm(df_comb2,dependent,weight_var="df_weights", id="id_df", strata=NULL, nest=FALSE, type="df1")
#   # } else {design_list2 = list(NULL,NULL,NULL)}
#   
#   
#   ### 2 get a list with glm results for both data frames ###
#   if (method=="ols") {
#     glm_list<-list()
#     glm_list[[1]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[1]], type="interact",design_list_df =design_list[[2]])
#     glm_list[[2]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[2]], type="df1")
# 
#     }
#   
#   if (method=="logit"){
#     glm_list<-list()
#     glm_list[[1]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[1]], method = "logit", type="interact",design_list_df =design_list[[2]])
#     glm_list[[2]]<-run_glm(df_comb = df_comb,dependent,independent, design_list =  design_list[[2]], method = "logit", type="df1")
# 
#     }
#   ### get the coefficients out of the output list
#   
#  ### function ###
#   get_b_function<-function(glm_list,i,independent, type="interact"){
#     
#     if(type=="interact"){
#       out<-glm_list[[1]][[i]][[1]][(length(independent)+3):(2*length(independent)+2)]
#     }
#     
#     if(type=="df"){
#       out<-glm_list[[2]][[i]][[1]][-1]
#     }
#     
#     out
#   }
#   
#   ### dataframe coefficients
#   
#   out<-sapply(1:length(dependent),get_b_function, glm_list=glm_list,
#               independent = independent,type="df")
#   
#   ### interaction coefficients ###
#   out<-cbind(out,sapply(1:length(dependent),get_b_function, glm_list=glm_list,
#                         independent = independent,type="interact"))
#   
#   out
# }

multi_boot<-function(df,benchmark,dependent,independent,formula_list=NULL,
                     id = NULL,
                     id_bench = NULL,weight = NULL,weight_bench = NULL,
                     strata = NULL, strata_bench = NULL, rm_na = "pairwise",
                     family=stats::gaussian(link = "identity"), nboots=1000, 
                     parallel=FALSE, ref=NULL,
                     adjustment_vars=NULL,
                     raking_targets=NULL, 
                     post_targets=NULL,boot_all=boot_all,
                     coef1=NULL,
                     coef1_bench=NULL,
                     percentile_ci=TRUE){
  
  if (parallel==TRUE) para<-"snow"
  if (parallel==FALSE) para<-"no"
  
  # boot_out<-boot(df,statistic = multi_boot_sub, R = nboots, benchmark = benchmark, 
  #                dependent=dependent,independent = independent, ids = id, 
  #                stratas=strata, weight_df = weight, id_bench= id_bench,
  #                weight_bench=weight_bench,strata_bench = strata_bench, 
  #                rm_na = "pairwise", bootstrap=TRUE, 
  #                ncpus = (parallel::detectCores()-1), parallel = para, 
  #                adjustment_vars=adjustment_vars, 
  #                raking_targets=raking_targets)
  
  boot_out<- multi_boot_sub(df=df,benchmark = benchmark,dependent=dependent,
                             independent = independent, formula_list=formula_list,ids = id, 
                             stratas=strata, weight_df = weight, id_bench= id_bench,
                             weight_bench=weight_bench,strata_bench = strata_bench,
                             rm_na = "pairwise",family=family,
                             adjustment_vars=adjustment_vars,
                             raking_targets=raking_targets,
                             post_targets=post_targets,
                             nboots=nboots)
  
  if(isTRUE(boot_all)){
    boot_out_bench<- multi_boot_sub(df=benchmark,benchmark = df,dependent=dependent,
                              independent = independent, formula_list=formula_list,ids = id_bench, 
                              stratas=strata_bench, weight_df = weight_bench, id_bench= id,
                              weight_bench=weight,strata_bench = strata,
                              rm_na = "pairwise",family=family,
                              adjustment_vars=NULL,
                              raking_targets=NULL,
                              post_targets=NULL,
                              nboots=nboots)
    
  }
  
  p_se_list<-list()
  if(is.null(formula_list)){
    
  p_se_list[[1]]<-boot_pvalues_multi_main(boot_out,dependent = dependent, 
                                          independent = independent,
                                          type="df_p",
                                          coef1=coef1,
                                          coef1_bench=coef1_bench,
                                          percentile_ci=percentile_ci)

  if(isFALSE(boot_all)){p_se_list[[2]]<-boot_pvalues_multi_main(boot_out,dependent = dependent, 
                                          independent = independent,
                                          type="interaction_p",
                                          ref=ref,
                                          boot_all = boot_all,
                                          coef1=coef1,
                                          coef1_bench=coef1_bench,
                                          percentile_ci=percentile_ci)}
  
  if(isTRUE(boot_all)){p_se_list[[2]]<-boot_pvalues_multi_main(boot_out,dependent = dependent, 
                                                                independent = independent,
                                                                type="interaction_p",
                                                                ref=boot_out_bench,
                                                               boot_all = boot_all,
                                                               coef1=coef1,
                                                               coef1_bench=coef1_bench,
                                                               percentile_ci=percentile_ci)}
  
  p_se_list[[3]]<-boot_pvalues_multi_main(boot_out,dependent = dependent, 
                                          independent = independent,
                                          type="df_se",
                                          coef1=coef1,
                                          coef1_bench=coef1_bench,
                                          percentile_ci=percentile_ci)

  if(isFALSE(boot_all)) {p_se_list[[4]]<-boot_pvalues_multi_main(boot_out,dependent = dependent, 
                                                              independent = independent,
                                                              type="interaction_se",
                                                              ref=ref,
                                                              boot_all = boot_all,
                                                              coef1=coef1,
                                                              coef1_bench=coef1_bench,
                                                              percentile_ci=percentile_ci)}
  
  if(isTRUE(boot_all)) {p_se_list[[4]]<-boot_pvalues_multi_main(boot_out,dependent = dependent, 
                                                              independent = independent,
                                                              type="interaction_se",
                                                              ref=boot_out_bench,
                                                              boot_all = boot_all,
                                                              coef1=coef1,
                                                              coef1_bench=coef1_bench,
                                                              percentile_ci=percentile_ci)}
  }
  

  if(is.null(formula_list)==FALSE){
    independent<-purrr::map(formula_list,~attr(terms(.), "term.labels"))
    uni_indep<- unique(unlist(independent))
    max_indep<- length(uni_indep)
    
    p_se_list[[1]]<-purrr::map(.x=1:length(dependent),~boot_pvalues_multi_main(data.frame(boot_out[,.x]),dependent = dependent[[.x]], 
                                                                               independent = independent[[.x]],
                                                                               type="df_p",max_indep=max_indep))
    
    if(isFALSE(boot_all)) {p_se_list[[2]]<-purrr::map(.x=1:length(dependent),~boot_pvalues_multi_main(data.frame(boot_out[,.x]),dependent = dependent[[.x]], 
                                                                                                      independent = independent[[.x]],
                                                                                                      type="interaction_p",
                                                                                                      ref=data.frame(ref[,.x]),
                                                                                                      max_indep=max_indep,
                                                                                                      boot_all = boot_all,
                                                                                                      coef1=coef1,
                                                                                                      coef1_bench=coef1_bench,
                                                                                                      percentile_ci=percentile_ci))}
    
    if(isTRUE(boot_all)) {p_se_list[[2]]<-purrr::map(.x=1:length(dependent),~boot_pvalues_multi_main(data.frame(boot_out[,.x]),dependent = dependent[[.x]], 
                                                                                                      independent = independent[[.x]],
                                                                                                      type="interaction_p",
                                                                                                      ref=data.frame(boot_out_bench[,.x]),
                                                                                                      max_indep=max_indep,
                                                                                                     boot_all = boot_all,
                                                                                                     coef1=coef1,
                                                                                                     coef1_bench=coef1_bench,
                                                                                                     percentile_ci=percentile_ci))}
    
    p_se_list[[3]]<-purrr::map(.x=1:length(dependent),~boot_pvalues_multi_main(data.frame(boot_out[,.x]),dependent = dependent[[.x]], 
                                            independent = independent[[.x]],
                                            type="df_se",max_indep=max_indep))
    
    if(isFALSE(boot_all)) {p_se_list[[4]]<-purrr::map(.x=1:length(dependent),~boot_pvalues_multi_main(data.frame(boot_out[,.x]),dependent = dependent[[.x]], 
                                                                                                      independent = independent[[.x]],
                                                                                                      type="interaction_se",
                                                                                                      ref=data.frame(ref[,.x]),
                                                                                                      max_indep=max_indep,
                                                                                                      boot_all = boot_all,
                                                                                                      coef1=coef1,
                                                                                                      coef1_bench=coef1_bench,
                                                                                                      percentile_ci=percentile_ci))}
    
    if(isTRUE(boot_all)) {p_se_list[[4]]<-purrr::map(.x=1:length(dependent),~boot_pvalues_multi_main(data.frame(boot_out[,.x]),dependent = dependent[[.x]], 
                                                                                                      independent = independent[[.x]],
                                                                                                      type="interaction_se",
                                                                                                      ref=data.frame(boot_out_bench[,.x]),
                                                                                                      max_indep=max_indep,
                                                                                                     boot_all = boot_all,
                                                                                                     coef1=coef1,
                                                                                                     coef1_bench=coef1_bench,
                                                                                                     percentile_ci=percentile_ci))}
    

    
    renamer<-function(col,dep,indep,uni_indep) {
      colnames(col)<-dep
      rownames(col)<-c(indep,uni_indep[!(uni_indep%in%indep)])
      return(col)}
    
    
    p_se_list<-purrr::map(p_se_list,~purrr::map(.x=.x,~data.frame(c(.x[,1],rep(NA,(max_indep-nrow(.x)))))))
    p_se_list<-purrr::map(p_se_list,~purrr::map2(.x=.,.y=1:length(independent),
                                                 ~renamer(.x,dependent[[.y]],independent[[.y]],uni_indep = uni_indep)))
    
    for (i in 2:length(p_se_list[[1]])){
      p_se_list[[1]][[1]]<-cbind(p_se_list[[1]][[1]],p_se_list[[1]][[i]])
      p_se_list[[2]][[1]]<-cbind(p_se_list[[2]][[1]],p_se_list[[2]][[i]])
      p_se_list[[3]][[1]]<-cbind(p_se_list[[3]][[1]],p_se_list[[3]][[i]])
      p_se_list[[4]][[1]]<-cbind(p_se_list[[4]][[1]],p_se_list[[4]][[i]])
    }
    p_se_list[[1]]<-as.matrix(p_se_list[[1]][[1]])
    p_se_list[[2]]<-as.matrix(p_se_list[[2]][[1]])
    p_se_list[[3]]<-as.matrix(p_se_list[[3]][[1]])
    p_se_list[[4]]<-as.matrix(p_se_list[[4]][[1]])
  }
  
  # p_se_list[[2]]<-boot_pvalues_multi(boot_out,dependent = dependent, 
  #                                 independent = independent,
  #                                 type="interact")
  # 
  # p_se_list[[3]]<-boot_pvalues_multi(boot_out,dependent = dependent, 
  #                                 independent = independent,
  #                                 type="se_df")
  # 
  # p_se_list[[4]]<-boot_pvalues_multi(boot_out,dependent = dependent, 
  #                                 independent = independent,
  #                                 type="se_interact")
  
  names(p_se_list[1])<-"p_df"
  names(p_se_list[2])<-"p_interaction"
  names(p_se_list[3])<-"se_df"
  names(p_se_list[4])<-"se_interaction"
  
  p_se_list
  
}

boot_pvalues_multi_main<-function(boot_object,dependent, independent, type="df_p", 
                                  ref=NULL,max_indep=NULL,boot_all=FALSE,
                                  coef1=NULL,
                                  coef1_bench=NULL,
                                  percentile_ci=TRUE){
  
  sub_boot_pvalues_multi_row<-function(boot_object,start,ncoef,col,
                                       ref=NULL,type="p",warning=FALSE,
                                       dependent=NULL,independent=NULL,
                                       max_indep=NULL,boot_all=FALSE,
                                       coef1=NULL,
                                       coef1_bench=NULL,
                                       percentile_ci=TRUE){
    
    coefs<-seq(start,nrow(boot_object),ncoef)
    vec<-c(unlist(boot_object[coefs,col]))
    
    
    
    if(is.null(ref)==FALSE){ 
      if(isFALSE(boot_all)) ref_coef<-ref[ncoef,col]
      if(isTRUE(boot_all)) ref_coef<-c(unlist(ref[coefs,col]))}
    if(is.null(ref)) ref_coef<-0
    coef1<-coef1[ncoef,col]
    coef1_bench<-coef1_bench[ncoef,col]
    
   
    if(type=="p" & warning==TRUE) out<-boot_pvalues3_multi(vec,ref=ref_coef,
                                                        col_numb = col,
                                                        row_numb = ncoef,
                                                        row = independent,
                                                        col = dependent,
                                                        max_indep=max_indep,
                                                        coef1=coef1,
                                                        coef1_bench=coef1_bench,
                                                        percentile_ci=percentile_ci)
    
    
    if(type=="p" & warning==FALSE) out<-boot_pvalues3_multi(vec,ref=ref_coef,
                                                            coef1=coef1,
                                                            coef1_bench=coef1_bench,
                                                            percentile_ci=percentile_ci)

    
    
    if(type=="se") out<-subfunc_multi_se(vec,ref=ref_coef)
    out
  }
  
  sub_boot_pvalues_multi_col<-function(boot_object,ncoef,col,ref=NULL,type="p",
                                       warning=FALSE,dependent=NULL,independent=NULL,
                                       max_indep=NULL,boot_all=FALSE,
                                       coef1=NULL,
                                       coef1_bench=NULL,
                                       percentile_ci=TRUE){
    
    purrr::map_dbl(.x=c(1:ncoef),~sub_boot_pvalues_multi_row(boot_object,.x,ncoef,col=col,ref=ref,
                                                      type=type,
                                                      warning=warning,
                                                      dependent=dependent,
                                                      independent=independent,
                                                      max_indep=max_indep,
                                                      boot_all=boot_all,
                                                      coef1=coef1,
                                                      coef1_bench=coef1_bench,
                                                      percentile_ci=percentile_ci))
  }
    
  l_dep<-length(dependent)
  l_indep<-length(independent)
  
  if(type=="df_p"){
    out<-purrr::map(.x=c(1:l_dep),~sub_boot_pvalues_multi_col(boot_object,l_indep,.x,
                                                       warning = TRUE,
                                                       dependent=dependent,
                                                       independent=independent,
                                                       max_indep=max_indep,
                                                       boot_all=boot_all,
                                                       coef1=coef1,
                                                       coef1_bench=coef1_bench,
                                                       percentile_ci=percentile_ci))
  }
  
  if(type=="interaction_p"){
    out<-purrr::map(.x=c(1:l_dep),~sub_boot_pvalues_multi_col(boot_object,l_indep,.x,ref=ref,
                                                              boot_all=boot_all,
                                                              coef1=coef1,
                                                              coef1_bench=coef1_bench,
                                                              percentile_ci=percentile_ci))
  
    }
  
  if(type=="df_se"){
    out<-purrr::map(.x=c(1:l_dep),~sub_boot_pvalues_multi_col(boot_object,l_indep,.x,type="se",
                                                              boot_all=boot_all,
                                                              coef1=coef1,
                                                              coef1_bench=coef1_bench,
                                                              percentile_ci=percentile_ci))
  }
  
  if(type=="interaction_se"){
    out<-purrr::map(.x=c(1:l_dep),~sub_boot_pvalues_multi_col(boot_object,l_indep,.x,ref=ref,type="se",
                                                              boot_all=boot_all,
                                                              coef1=coef1,
                                                              coef1_bench=coef1_bench,
                                                              percentile_ci=percentile_ci))
  }
  
  matrix(unlist(out),ncol=l_dep)
    
  
  
}

boot_pvalues3_multi<-function(boot_object,reference=0,ref=NULL,
                              row=NULL,col=NULL,
                              col_numb=NULL,row_numb=NULL,
                              bench=FALSE,max_indep=NULL,
                              coef1=NULL,
                              coef1_bench=NULL,
                              percentile_ci=TRUE){
  
  #boot_object[boot_object %in% "NaN"]<-NA
  #if(!is.null(r)) r[r%in%"NaN"]<-NA
  in_interval <-TRUE
  alpha<-0
  
  if(is.null(max_indep)==FALSE) {model_less<-length(row)-max_indep
  ### check if boot_object contains na ###
  
  if(sum(is.na(boot_object)) > 0 & is.null(col_numb)==FALSE & model_less==0){
    
    warning(paste(sum(is.na(boot_object)), "of the", length(boot_object),
                  "bootstraps contain zero combined cases for the variable pair of:",
                  row[row_numb],"and",col[col_numb],".\n"))
  }}
  
  #if(bench==TRUE & sum(is.na(boot_object))==length(boot_object)) return("NaN")
  if(sum(is.na(boot_object))==length(boot_object)) return(NA)
  if(is.null(ref)==FALSE) {if(length(ref)==1){if(is.na(ref)) return(NA)}}
  if(is.null(ref)==FALSE) {if(length(ref)==1){if(all(is.na(ref))) return(NA)}}
  

  
  ### get p_values up to 0.00001
  while(in_interval){
    alpha <- alpha + 0.001
    if(is.null(ref)& percentile_ci==FALSE) cis<-c(stats::quantile(boot_object, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(boot_object, probs=(alpha/2),na.rm=TRUE))
    if(is.null(ref)==TRUE & percentile_ci==FALSE){
      SE=stats::sd(boot_object,na.rm=T)
      if(alpha=="1") return(1)
      cis<-c(coef1 + stats::qnorm(1-alpha/2) * SE,
             coef1 - stats::qnorm(1-alpha/2) * SE)}
    
    if(is.null(ref)==FALSE){
      if(percentile_ci==TRUE){
      diff<- boot_object - ref
      cis<-c(stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(diff, probs=(alpha/2),na.rm=TRUE))}
      
    if(percentile_ci==FALSE){
      SE=stats::sd(boot_object-ref,na.rm=T)
      if(alpha=="1") return(1)
      cis<-c(coef1-coef1_bench + stats::qnorm(1-alpha/2) * SE,
             coef1-coef1_bench - stats::qnorm(1-alpha/2) * SE)}
      # lower_ci<- ((ref) - (mean(boot_object,na.rm=TRUE)) - (ref))- (stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object)))))
      # upper_ci<- ((ref) - (mean(boot_object,na.rm=TRUE)) - (ref))+ (stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object)))))
      # cis<-c(lower_ci,upper_ci)
    }
    
    in_interval<- (reference > cis[1] & reference < cis[2])|(reference < cis[1] & reference > cis[2])
  }
  
  alpha<-alpha-0.001
  in_interval<-TRUE
  
  while(in_interval){
    alpha <- alpha + 0.00001
    if(is.null(ref)==TRUE & percentile_ci==TRUE) cis<-c(stats::quantile(boot_object, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(boot_object, probs=(alpha/2),na.rm=TRUE))
    if(is.null(ref)==TRUE & percentile_ci==FALSE){
      SE=stats::sd(boot_object,na.rm=T)
      if(alpha=="1") return(1)
      cis<-c(coef1 + stats::qnorm(1-alpha/2) * SE,
             coef1 - stats::qnorm(1-alpha/2) * SE)}
    
    if(is.null(ref)==FALSE){
      if(percentile_ci==TRUE){
      diff<- boot_object - ref
      cis<-c(stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(diff, probs=(alpha/2),na.rm=TRUE))}
      
      if(percentile_ci==FALSE){
        SE=stats::sd(boot_object-ref,na.rm=T)
        if(alpha=="1") return(1)
        cis<-c(coef1-coef1_bench + stats::qnorm(1-alpha/2) * SE,
               coef1-coef1_bench - stats::qnorm(1-alpha/2) * SE)}
      #lower_ci<- (ref) - (mean(boot_object) - (ref))- (stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object)))))
      #upper_ci<- (ref) - (mean(boot_object) - (ref))+ (stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object)))))
      #cis<-c(lower_ci,upper_ci)
    }
    in_interval<- (reference > cis[1] & reference < cis[2])|(reference < cis[1] & reference > cis[2])
  }
  alpha
}

# boot_pvalues_multi<-function(boot_object,dependent, independent, type="df"){
#   
#   subfunc_boot_pvalues_multi<-function(boot_object,i){
#     
#     if(is.na(as.vector(boot_object$t0)[i])==FALSE){
#       alpha<-boot.pval::boot.pval(boot_object, type="perc",theta_null=0,index = i)}
#     
#     else {alpha<-c(NA)}
#     
#     alpha
#     
#   }
#   
#   if(type=="df" | type=="se_df") {i<-1:(length(dependent)*length(independent))}
#   if(type=="interact"| type=="se_interact") {i<-(length(dependent)*length(independent)+1):(2*(length(dependent)*length(independent)))}
#   
#   if(type=="df"|type=="interact"){
#   ps<-sapply(i,subfunc_boot_pvalues_multi, boot_object=boot_object)
#   ps<-matrix(ps,ncol=length(dependent),byrow = FALSE)
#   colnames(ps)<-dependent
#   rownames(ps)<-independent
#   return(ps)}
#   
#   if(type=="se_df"| type=="se_interact"){
#     se<-sapply(i,subfunc_multi_se, boot_object=boot_object)
#     se<-matrix(se,ncol=length(dependent),byrow = FALSE)
#     colnames(se)<-dependent
#     rownames(se)<-independent
#   return(se)}
# }


subfunc_multi_se<-function(boot_object,ref=NULL){
  boot_object[abs(boot_object)>1000*stats::quantile(abs(boot_object),0.99,na.rm=TRUE)]<-NA
  if(is.null(ref)) se<-stats::sd(boot_object,na.rm = TRUE)
  if(is.null(ref)==FALSE) se<-stats::sd((boot_object-ref),na.rm = TRUE)
  se
}




extract_interaction_results<-function(glm_model, robust_se=FALSE){
  
  
  if(robust_se == FALSE) model_summary <- summary(glm_model)
  if(robust_se == TRUE) model_summary<- lmtest::coeftest(glm_model, vcov = sandwich::vcovHC(glm_model, type="HC1"))
  
  # Extract the interaction coefficients, standard errors, and p-values
  if(robust_se == FALSE){
    interaction_coeffs <- stats::coef(glm_model)
    interaction_se <- model_summary$coefficients[, "Std. Error"]
    interaction_pvalues <- model_summary$coefficients[, 4]
  }
  
  
  if(robust_se == TRUE){
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
  results <- results[c(grep(":sample_ident", rownames(results)),grep("sample_ident:", rownames(results))), ]
  
  # Print the results
  as.matrix(results)
}


# Create a function to number subsequent occurrences
number_occurrences <- function(x) {
  # Create an empty list to store counts
  counts <- list()
  
  # Initialize the output vector
  output <- character(length(x))
  
  for (i in seq_along(x)) {
    val <- x[i]
    
    if (!val %in% names(counts)) {
      # First occurrence
      counts[[val]] <- 1
      output[i] <- val
    } else {
      # Subsequent occurrences
      counts[[val]] <- counts[[val]] + 1
      output[i] <- paste0(val, counts[[val]])
    }
  }
  
  return(output)
}