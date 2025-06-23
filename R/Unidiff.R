###########################################################################
###
### 		Subject:	R-script zur Pforr und Dannwolf Funktion
### 		Date: 		May 2023
### 		Author: 	Bjoern Rohr
### 	Version:  	1.00
###
### 		Bugfix:   	/
###
###########################################################################


#################################
### The function for the Plot ###
#################################

### Documentation of the diff_plotter_function ###

#' Compare data frames and Plot Differences
#'
#' Returns data or a plot showing the difference of two or more
#' data frames The differences are calculated on the base of
#' differing metrics, chosen in the funct argument. All used data frames must
#' contain at least one column named equal in all data frames, that has equal
#' values. 
#' @param dfs A character vector containing the names of data frames to compare against the benchmarks. 
#' @param benchmarks A character vector containing the names of benchmarks to compare the data frames against.
#' The vector must either be the same length as \code{dfs}, or length 1. If it has length 1 every
#' df will be compared against the same benchmark. Benchmarks can either be the name of data frames, 
#' the name of a list of tables, or a named vector of means. The tables in the list need to be named as the respective variables
#' in the data frame of comparison. When they are a named vector of means, the means need to be named as the respective variables
#' in the dfs.
#' @param variables A character vector containing the names of the variables for the comparison. If NULL,
#' all variables named similarly in both the \code{dfs} and the benchmarks will be compared. Variables missing
#' in one of the data frames or the benchmarks will be neglected for this comparison.
#' @param nboots  The number of bootstraps used to calculate standard errors. Must either be >2 or 0.
#' If >2 bootstrapping is used to calculate standard errors with \code{nboots} iterations. If 0, SE
#' is calculated analytically. We do not recommend using \code{nboots} =0 because this method is not
#' yet suitable for every \code{funct} used and every method. Depending on the size of the data and the
#' number of bootstraps, \code{uni_compare} can take a while.
#' @param funct A character string, indicating the function to calculate the 
#' difference between the data frames.
#'   
#' Predefined functions are:
#' 
#' * \code{"d_mean"}, \code{"ad_mean"} A function to calculate the (absolute) difference in mean of
#' the variables in \code{dfs} and benchmarks with the same name. Only applicable for
#' metric variables.
#'
#' * \code{"d_prop"}, \code{"ad_prop"} A function to calculate the (absolute) difference in proportions of
#' the variables in \code{dfs} and benchmarks with the same name. Only applicable for dummy
#' variables.
#'
#' * \code{"rel_mean"}, \code{"abs_rel_mean"} A function to calculate the (absolute) 
#' relative difference in mean of the variables in \code{dfs} and benchmarks with the same name.
#' #' For more information on the formula for difference and analytic variance, see Felderer 
#' et al. (2019). Only applicable for metric variables.
#'
#' * \code{"rel_prop"}, \code{"abs_rel_prop"} A function to calculate the (absolute) 
#' relative difference in proportions of the variables in \code{dfs} and benchmarks with 
#' the same name. It is calculated similar to the relative difference in mean 
#' (see Felderer et al., 2019), however the default label for the plot is different. 
#' Only applicable for dummy variables.
#'
# #' * \code{"ad_median"} A function to calculate the (absolute) relative difference in median of
# #' the variables in \code{dfs} and benchmarks with the same name.
#'
#'
#' @param data If TRUE, a uni_compare_object is returned, containing results of the comparison.
#' @param legendlabels A character string or vector of strings containing a label for the
#' legend.
#' @param legendtitle A character string containing the title of the legend.
#' @param colors A vector of colors, that is used in the plot for the
#' different comparisons.
#' @param shapes A vector of shapes applicable in [ggplot2::ggplot2()], that is 
#' used in the plot for the different comparisons.
#' @param summetric If \code{"avg1"}, \code{"mse1"}, \code{"rmse1"}, or \code{"R"} 
#' the respective measure is calculated for the biases of each survey. The values 
#' \code{"mse1"} and \code{"rmse1"} lead to similar results as in \code{"mse2"} and \code{"rmse2"}, 
#' with slightly different visualization in the plot. If \code{summetric = NULL}, no summetric 
#' will be displayed in the Plot. When \code{"R"} is chosen, also \code{response_identificator} 
#' is needed.
#' @param label_x,label_y A character string or vector of character strings containing a label for
#' the x-axis and y-axis.
#' @param plot_title A character string containing the title of the plot.
#' @param varlabels A character string or vector of character strings containing the new names of
#' variables, also used in plot.
#' @param name_dfs,name_benchmarks A character string or vector of character strings containing the
#' new names of the \code{dfs} and \code{benchmarks}, that is also used in plot.
#' @param summet_size A number to determine the size of the displayed \code{summetric} in the plot.
#' @param silence If \code{silence = FALSE} a warning will be displayed, if variables a
#' re excluded from either the data frame or benchmark, for not existing in both.
#' @param conf_level A numeric value between zero and one to determine the confidence level of the confidence
#' interval.
#' @param conf_adjustment If \code{conf_adjustment = TRUE} the confidence level of the confidence interval will be
#' adjusted with a Bonferroni adjustment, to account for the problem of multiple comparisons.
#' @param weight,weight_bench A character vector determining variables to weight the \code{dfs} or
#' \code{benchmarks}. They have to be part of the respective data frame. If only one character is provided,
#' the same variable is used to weigh every \code{df} or \code{benchmark}. If a 
#' weight variable is provided also an \code{id} variable is needed.For 
#' weighting, the \code{survey} package is used.
#' @param id,id_bench A character vector determining \code{id} variables used 
#' to weigh the \code{dfs} or \code{benchmarks} with the help of the 
#' \code{survey} package. They have to be part of the respective data frame. If 
#' only one character is provided, the same variable is used to weigh every 
#' \code{df} or \code{benchmark}.
#' @param strata,strata_bench A character vector determining strata variables 
#' used to weigh the \code{dfs} or \code{benchmarks} with the help of the 
#' \code{survey} package.They have to be part of the respective data frame. 
#' If only one character is provided, the same variable is used to weight every 
#' \code{df} or \code{benchmark}.
# #' @param R_variables A character vector with the names of variables that should be used in the model 
# #' to calculate the R indicator
#' @param type Define the type of comparison. Can either be \code{"comparison"} or \code{"nonresponse"}.
#' @param ndigits The number of digits to round the numbers in the plot.
#' @param adjustment_vars Variables used to adjust the survey when using raking 
#' or post stratification.
#' @param raking_targets A list of raking targets that can be given to the rake 
#' function of \code{\link[survey]{rake}}, to rake the \code{dfs}.
#' @param post_targets A list of post-stratification targets that can be given to the 
#' \code{\link[survey]{postStratify}} function, to post-stratify the \code{dfs}.
#' @param adjustment_weighting A character vector indicating if adjustment 
#' weighting should be used. It can either be \code{"raking"} or \code{"post_start"}.
#' @param boot_all If TURE, both, dfs and benchmarks will be bootstrapped. Otherwise 
#' the benchmark estimate is assumed to be constant.
#' @param percentile_ci If TURE, cofidence intervals will be calculated using the percentile method.
#' If False, they will be calculated using the normal method.
#' @param parallel Can be either \code{FALSE} or a number of cores that should 
#' be used in the function. If it is \code{FALSE}, only one core will be used and 
#' otherwise the given number of cores will be used.
#' @param n_bench A list of vectors containing the number of cases for every variable in the benchmark.
#' This is only needed, if the benchmark is given as a vector. The list should be as long as the number of dataframes
#' 
#' 
#' @return A plot based on [ggplot2::ggplot2()] (or data frame if data==TRUE)
#' which shows the difference between two or more data frames on predetermined variables,
#' named identical in both data frames.
#' 
#' @references 
#' Felderer, B., Kirchner, A., & Kreuter, FALSE. (2019). The Effect of Survey Mode on Data 
#' Quality: Disentangling Nonresponse and Measurement Error Bias. Journal of Official 
#' Statistics, 35(1), 93–115. https://doi.org/10.2478/jos-2019-0005
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom boot boot
#' @examples
#' 
#' ## Get Data for comparison
#' 
#' data("card")
#' 
#' north<-card[card$south==0,]
#' white<-card[card$black==0,]
#' 
#' ## use the function to plot the data 
#' univar_comp<-sampcompR::uni_compare(dfs = c("north","white"),
#'                                     benchmarks = c("card","card"),
#'                                     variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                     funct = "abs_rel_mean",
#'                                     nboots=200,
#'                                     summetric="rmse2",
#'                                     data=FALSE)
#'
#'  univar_comp
#'  

### The diff_plotter_function
uni_compare <- function(dfs, benchmarks, variables=NULL, nboots = 2000,
                        n_bench=NULL, boot_all=FALSE,funct = "rel_mean",
                        data = TRUE, type="comparison",legendlabels = NULL, 
                        legendtitle = NULL, colors = NULL, shapes = NULL,
                        summetric = "rmse2", label_x = NULL, label_y = NULL, 
                        plot_title = NULL, varlabels = NULL,
                        name_dfs=NULL, name_benchmarks=NULL,
                        summet_size=4, silence=TRUE, conf_level=0.95, 
                        conf_adjustment=NULL,percentile_ci =TRUE,
                        weight =NULL, id=NULL, strata=NULL, 
                        weight_bench=NULL,id_bench=NULL, strata_bench=NULL,
                        adjustment_weighting="raking", 
                        adjustment_vars=NULL,raking_targets=NULL,
                        post_targets=NULL,ndigits=3,parallel=FALSE) {
  
  ##################################
  ### Errors if inputs are wrong ###
  ##################################
  
  ### Not enough Data frames ###
  if (is.null(dfs) | is.null(benchmarks)) stop("no data for compairson provided")
  
  
  ### benchmarks is longer than 1 but shorter than dfs ###
  if (length(benchmarks)>1 & length(benchmarks)!=length(dfs)) stop("benchmarks must either be length 1 or the same length as dfs")
  
  ### Inputs are not a Data frame ###
  for (i in 1:length(dfs)){
    
    if (is.function(get(dfs[i]))) stop(paste("dfs must not be named the same as a existing function"))
    
    if (is.data.frame(get(dfs[i])) == FALSE) stop(paste(dfs[i]," must be a character naming a data frame",
                                                        sep = "", collapse = NULL))
    
    ### check if benchmarks have similar variables ###
    if (length(benchmarks)==length(dfs)){
      if ((any(names(get(dfs[i])) %in% names(get(benchmarks[i]))))==FALSE) stop(dfs[i], " has no common variable with ", benchmarks[i],".")}
    if ((length(benchmarks)==length(dfs))==FALSE){
      if ((any(names(get(dfs[i])) %in% names(get(benchmarks[1])))==FALSE)) stop(dfs[i], " has no common variable with ",benchmarks[1],".")}
  }
  
  
  for (i in 1:length(benchmarks)){
    
    if (is.function(get(benchmarks[i]))) stop(paste("benchmarks must not be named the same as a existing function"))
    
    if (inherits(get(benchmarks[i]),"data.frame") == FALSE &
        inherits(get(benchmarks[i]),"list")==FALSE &
        is_named_vector(get(benchmarks[i]))==FALSE) stop(paste(benchmarks[i], " must be a data frame, a named numeric vector, or a list",
                                                               sep = "", collapse = NULL))
    if (inherits(get(benchmarks[i]),"list") & 
        is.null(weight_bench)==FALSE) stop(paste(benchmarks[i]), " if benchmark is a list of tables, weighting needs to be permored, when generating the list.")
  }
  
  ### Check if funct is either a function, character, vector of functions or vector of characters.
  if(length(funct)==1){
    if (is.function(funct)==FALSE){
      if ((funct%in% c("d_mean","ad_mean","d_prop","ad_prop","prop_modecat","abs_prop_modecat",
                       "avg_prop_diff","avg_abs_prop_diff","rel_mean","rel_prop","ad_median",
                       "ad_mode","abs_rel_mean","abs_rel_prop"))==FALSE) {
        stop("funct must either be a function applicable as statistic in the boot package, or
             a character vector indicating one of the predefined functions.")
      }
    }
  }
  
  if(length(funct)>1){
    if(is.null(variables)) stop("if funct>1, funct has to be of the same length as variables.")
    if(length(funct)!= length(variables)) stop("if funct>1, funct has to be of the same length as variables.")
    for (i in 1:length(funct)) {
      if (is.function(funct)==FALSE){
        if ((funct%in% c("d_mean","ad_mean","d_prop","ad_prop","prop_modecat","abs_prop_modecat",
                         "avg_prop_diff","avg_abs_prop_diff","rel_mean","rel_prop","ad_median",
                         "ad_mode","abs_rel_mean","abs_rel_prop"))==FALSE) {
          stop("funct must either be a function applicable as statistic in the boot package, or
             a character vector indicating one of the predefined functions.")
        }
      }
    }
  }
  
  
  
  ### Check if characterinputs are characters ###
  
  if (is.null(label_x) == FALSE) {
    if (is.character(label_x) == FALSE) stop("label_x must be a character.")
  }
  if (is.null(label_y) == FALSE) {
    if (is.character(label_y) == FALSE) stop("label_y must be a character.")
  }
  if (is.null(varlabels) == FALSE) {
    if (is.character(varlabels) == FALSE) stop("varlabels must be a character.")
  }
  if (is.null(plot_title) == FALSE) {
    if (is.character(plot_title) == FALSE) stop("plot_title must be a character.")
  }
  if (is.null(colors) == FALSE) {
    if (is.character(colors) == FALSE) stop("colors must be a character.")
  }
  if (is.null(legendlabels) == FALSE) {
    if (is.character(legendlabels) == FALSE) stop("legendlabels must be a character.")
  }
  if (is.null(legendtitle) == FALSE) {
    if (is.character(legendtitle) == FALSE) stop("legendtitle must be a character.")
  }
  if (is.null(name_dfs) == FALSE) {
    if (is.character(name_dfs) == FALSE) stop("name_dfs must be a character.")
  }
  if (is.null(name_benchmarks ) == FALSE) {
    if (is.character(name_benchmarks ) == FALSE) stop("name_benchmarks must be a character.")
  }
  if (is.null(summet_size) == FALSE) {
    if (is.numeric(summet_size) == FALSE) stop("summet_size must be a number that is >0.")
  }
  
  # ### check for ci-type ###
  # if((ci_type %in% c("perc","norm"))==FALSE) stop('ci_type must either be "perc" or "norm".')
  
  ### Check if data frame is logical
  if (is.logical(data) == FALSE) stop("data must be of type logical")
  if (is.logical(silence) == FALSE) stop("silence must be of type logical")
  
  ### Check if data frame is logical
  if (is.numeric(nboots) == FALSE) stop("nboots must be of type numeric")
  if (nboots < 0 | nboots == 1) stop("nboots must be 0(for standard SE) or >1 for bootstrap SE")
  
  ### Check is summetric is right ###
  if (is.null(summetric)== FALSE) {
    if(summetric!= "rmse1" & summetric!= "rmse2" &
       summetric!= "mse1" & summetric!= "mse2" &
       summetric!="avg1" & summetric!="avg2" &
       summetric!="R") stop("summetric must be either avg,avg2, rmse1, rmse2, mse1, mse2 or R")}
  
  ### confidence level out of bound ###
  if (conf_level>1 | conf_level<0) stop("conf_level must be <1 and >0")
  
  
  ### check for weight var ###
  if(is.null(weight)==FALSE) if(is.null(id)) stop("if a weight var is provided for the data frame also a id is needed")
  if(is.null(weight_bench)==FALSE) if(is.null(id_bench)) stop("if a weight var is provided for the benchmark also id_bench is needed")
  
  
  
  response_identificator<-NULL
  R_variables<-NULL 
  
  ##############################
  ### Get Benchmarks and DFS ###
  ##############################
  
  
  ### get benchmark if only one benchmark is provided ###
  if(length(benchmarks)==1) benchmarks<-c(rep(benchmarks,length(dfs)))
  
  ### Get Names of data frameS ###
  
  if (is.null(name_dfs)==FALSE) names<-name_dfs else names=NULL
  name_dfs<-dfs
  if (is.null(names)==FALSE) name_dfs[1:(length(names))] <- names
  
  
  
  
  if (is.null(name_benchmarks)==FALSE) names<-name_benchmarks else names=NULL
  name_benchmarks<-benchmarks
  
  if (is.null(names)==FALSE) name_benchmarks[1:(length(names))] <- names
  
  
  
  ##########################
  ### save dfs in a list ###
  ##########################
  
  df_list<-list()
  
  for (i in 1:length(dfs)){
    df_list[[i]]<-get(dfs[i])

  }
  
  
  ### prepare weights ###
  
  if(is.null(id)==FALSE) if(length(id)>=1 & length(id)<length(dfs)) id<-rep(id,length(dfs))
  if(is.null(weight)==FALSE) if(length(weight)>=1 & length(weight)<length(dfs)) weight<-rep(weight,length(dfs))
  if(is.null(strata)==FALSE) if(length(strata)>=1 & length(strata)<length(dfs)) strata<-rep(strata,length(dfs))
  
  if(is.null(id_bench)==FALSE) if(length(id_bench)>=1 & length(id_bench)<length(dfs)) id_bench<-rep(id_bench,length(dfs))
  if(is.null(weight_bench)==FALSE) if(length(weight_bench)>=1 & length(weight_bench)<length(dfs)) weight_bench<-rep(weight_bench,length(dfs))
  if(is.null(strata_bench)==FALSE) if(length(strata_bench)>=1 & length(strata_bench)<length(dfs)) strata_bench<-rep(strata_bench,length(dfs))
  
  
  #################################
  ### save benchmarks in a list ###
  #################################
  
  bench_list<-list()
  for (i in 1:length(benchmarks)){
    bench_list[[i]]<-get(benchmarks[i])
    
  }
  
  
  ###################################
  ### equalize data to benchmarks ###
  ###################################
  
  ### Equalize Data to Benchmark
  
  for (i in 1:length(dfs)){
    if(is_named_vector(bench_list[[i]])==F){
      
      df_list[[i]]<- dataequalizer(target_df= bench_list[[i]] ,source_df = df_list[[i]],
                                   variables = variables, silence = silence)
      
      bench_list[[i]]<- dataequalizer(target_df = df_list[[i]], source_df = bench_list[[i]],
                                      variables = variables, silence = silence)}
    if(is_named_vector(bench_list[[i]])){
      vars<-variables[variables%in%names(bench_list[[i]])]
      vars<-vars[vars%in%names(df_list[[i]])]
      bench_list[[i]]<- bench_list[[i]][vars]
      df_list[[i]]<-df_list[[i]][vars]
    }
    
    if(is.null(weight)==FALSE){
      if (is.na(weight[i])==FALSE) df_list[[i]][,weight[i]]<-get(dfs[i])[,weight[i]]
    }
    if(is.null(id)==FALSE){
      if (is.na(id[i])==FALSE) df_list[[i]][,id[i]]<-get(dfs[i])[,id[i]]
    }
    if(is.null(strata)==FALSE){
      if (is.na(strata[i])==FALSE) df_list[[i]][,strata[i]]<-get(dfs[i])[,strata[i]]
    }
    if(is.null(adjustment_vars)==FALSE)df_list[[i]]<-cbind(df_list[[i]],get(dfs[i])[,adjustment_vars[[i]]])
    
    
    if(is.null(id_bench)==FALSE){
      if (is.na(id_bench[i])==FALSE) bench_list[[i]][,id_bench[i]]<-get(benchmarks[i])[,id_bench[i]]
    }
    if(is.null(weight_bench)==FALSE){
      if (is.na(weight_bench[i])==FALSE) bench_list[[i]][,weight_bench[i]]<-get(benchmarks[i])[,weight_bench[i]]
    }
    if(is.null(strata_bench)==FALSE){
      if (is.na(strata_bench[i])==FALSE) bench_list[[i]][,strata_bench[i]]<-get(benchmarks[i])[,strata_bench[i]]
    }
    
    
    
  }
  
  
  #####################################################
  ### Get  Functions for every variable in data frames ###
  #####################################################
  
  #############################
  ### Choosing the Function ###
  #############################
  
  ### get func_name
  if (is.character(funct)){
    if (length(funct)==1) func_name<-funct
    else func_name<-"Difference"}
  
  func<-NA
  if (is.character(funct)){
    for (i in 1:length(funct)) {
      if (funct[i] == "rel_mean") func[i] <- "REL_MEAN"
      if (funct[i] == "rel_prop") func[i] <- "REL_MEAN"
      if (funct[i] == "abs_rel_mean") func[i] <- "ABS_REL_MEAN"
      if (funct[i] == "abs_rel_prop") func[i] <- "ABS_REL_MEAN"
      if (funct[i] == "ad_mean") func[i] <- "ABS_PROP_DIFF"
      if (funct[i] == "d_mean") func[i] <- "PROP_DIFF"
      if (funct[i] == "ad_median") func[i] <- "AD_MED"
      if (funct[i] == "ad_mode") func[i] <- "AD_MODE"
      if (funct[i] == "ks") func[i] <- "KS"
      if (funct[i] == "prop_modecat") func[i] <- "PERC_MODECOUNT"
      if (funct[i] == "abs_prop_modecat") func[i] <- "ABS_PERC_MODECOUNT"
      if (funct[i] == "d_prop") func[i]<- "PROP_DIFF"
      if (funct[i] == "ad_prop") func[i]<- "ABS_PROP_DIFF"
      if (funct[i] == "avg_prop_diff") func[i] <- "MEAN_PERC_DIST"
      if (funct[i] == "avg_abs_prop_diff") func[i] <- "Mean_ABS_PERC_DIST"
      
    }}
  
  
  #######################
  ### calculate alpha ###
  #######################
  
  alpha<-1-conf_level
  
  #########################
  ### Calculate Results ###
  #########################
  if(parallel!=FALSE){future::plan(future::multisession,workers=parallel)}
  
  
  results<-furrr::future_map_dfr(.x=c(1:length(dfs)),
                                 ~subfunc_diffplotter(x = df_list[[.x]], y = bench_list[[.x]],
                                                      samp = .x, nboots = nboots, func = funct[1],
                                                      variables = variables,
                                                      func_name = func_name, alpha=alpha,
                                                      conf_adjustment=conf_adjustment,
                                                      ids=id[.x], weights = weight[.x], 
                                                      strata = strata[.x],
                                                      ids_bench = id_bench[.x], weights_bench = weight_bench[.x],
                                                      strata_bench = strata_bench[.x],
                                                      adjustment_weighting=adjustment_weighting,
                                                      adjustment_vars=adjustment_vars[[.x]],
                                                      raking_targets=raking_targets[[.x]],
                                                      post_targets=post_targets[[.x]],
                                                      boot_all=boot_all,
                                                      percentile_ci =percentile_ci,
                                                      parallel=parallel, max_samp=length(dfs),
                                                      n_bench=n_bench[[.x]]),
                                 .options = furrr::furrr_options(seed = NULL))
  
  #browser()
  # for (i in 1:length(dfs)){
  #   if (ncol(df_list[[i]])>0) {
  #   if (i==1) {
  # results<-subfunc_diffplotter(x = df_list[[i]], y = bench_list[[i]],
  #                              samp = i, nboots = nboots, func = funct[1],
  #                              variables = variables,
  #                              func_name = func_name, alpha=alpha,
  #                              conf_adjustment=conf_adjustment,
  #                              ids=id[i], weights = weight[i], strata = strata[i],
  #                              ids_bench = id_bench[i], weights_bench = weight_bench[i],
  #                              strata_bench = strata_bench[i],
  #                              adjustment_weighting=adjustment_weighting,
  #                              adjustment_vars=adjustment_vars[[i]],
  #                              raking_targets=raking_targets[[i]],
  #                              post_targets=post_targets[[i]],
  #                              boot_all=boot_all,
  #                              percentile_ci =percentile_ci,
  #                              parallel=parallel)
  #     
  #     message(paste("survey",i,"of",length(dfs),"is compared"))
  #   
  #   #return(result)
  #   } 
  # 
  #     
  #   if (i!=1){
  #     
  #     results<- rbind(results,subfunc_diffplotter(x = df_list[[i]], y = bench_list[[i]],
  #                                                 samp = i, nboots = nboots, func = funct[1],
  #                                                 variables = variables,
  #                                                 func_name = func_name, alpha=alpha, 
  #                                                 conf_adjustment=conf_adjustment,
  #                                                 ids=id[i], weights = weight[i],strata = strata[i],
  #                                                 ids_bench = id_bench[i], weights_bench = weight_bench[i],
  #                                                 strata_bench = strata_bench[i],
  #                                                 adjustment_weighting=adjustment_weighting,
  #                                                 adjustment_vars=adjustment_vars[[i]],
  #                                                 raking_targets=raking_targets[[i]],
  #                                                 post_targets=post_targets[[i]],
  #                                                 boot_all=boot_all,percentile_ci =percentile_ci,
  #                                                 parallel=parallel))
  #   
  #     message(paste("survey",i,"of",length(dfs),"is compared"))
  #   }}
  # 
  #   if (ncol(df_list[[i]])==0) stop(paste(name_dfs[i],"does not share a common variable with the benchmark or the variables parameter"),
  #                                      sep =" ")
  # }
  
  
  ############################
  ### Add df_names to Data ###
  ############################
  
  for (i in 1:length(name_dfs)){
    results$name_dfs[results$sample==i]<-name_dfs[i]
  }
  
  for (i in 1:length(name_benchmarks)){
    results$name_benchmarks[results$sample==i]<-name_benchmarks[i]
  }
  
  
  ################################
  ### Add function names to df ###
  ################################
  
  if (length(funct)>1 ){
    
    for (i in 1:length(dfs)){
      for (j in 1:nrow(results[results$sample])){
        results$funct[results$sample==i][j]<-funct[j]
      }}}
  
  if(length(funct)==1) results$funct<-funct
  
  ############################################################################################
  ### Calculate a SumMETRIC to DATA, that Makes the whole data frame compairable with another ###
  ### RMSE & MSE                                                                           ###
  ############################################################################################
  
  results$mse<-NA
  results$rmse<-NA
  
  for (i in 1:length(dfs)){
    
    bias<-results$t_vec[results$sample==i]
    
    results$mse[results$sample==i]<-sum(bias*bias)/length(bias)
    results$rmse[results$sample==i]<-sqrt(sum(bias*bias)/length(bias))
    results$avg[results$sample==i]<-sum(abs(bias))/length(bias)
  }
  
  
  
  #############################
  ### Calculate R indicator ###
  #############################
  
  ### get R_vars ###
  
  if(is.null(R_variables)) R_variables<-colnames(df_list[[i]])
  
  
  for (i in 1:length(dfs)){
    if (is.null(response_identificator)==FALSE){if(is.na(response_identificator[i])==FALSE) 
      
      R_indicator<-R_indicator_func(get(dfs[i]),response_identificator=response_identificator,
                                    variables=R_variables,
                                    weight = weight[i],id=id[i],strata=strata[i])
    }
    if(is.null(response_identificator)) R_indicator<-NA
    if(is.null(response_identificator)==FALSE) {if(is.na(response_identificator[i])==FALSE) R_indicator<-NA}
    
    
    results$R_indicator[results$sample==i]<-R_indicator
    
  }
  
  ############################################################################
  ### add results and everything else together to create an results object ###
  ############################################################################
  
  results<-final_data2(data = results, name_dfs=name_dfs, name_benchmarks=name_benchmarks, summetric=summetric, colors=colors,
                       shapes=shapes, legendlabels=legendlabels, legendtitle=legendtitle , label_x=label_x, label_y=label_y,
                       summet_size=summet_size, plot_title=plot_title, funct=funct,type = type, variables = variables,
                       varlabels=varlabels,ndigits=ndigits)
  
  if (isTRUE(data)) return(results)
  
  #####################
  ### Edit varnames ###
  #####################
  if (is.null(varlabels)) varlabels<-unique(results$data$varnames)
  if (length(varlabels) >= length(unique(results$data$varnames))){varlabels<-varlabels[1:length(unique(results$data$varnames))]}
  if (length(varlabels) < length(unique(results$data$varnames))) varlabels<-c(varlabels,unique(results$data$varnames)[(length(varlabels)+1):length(unique(results$data$varnames))])
  
  ################
  ### Plotting ###
  ################
  
  Plot <- ggplot2::ggplot(data = results$data, ggplot2::aes(x = results$data$t_vec, y = factor(results$data$varnames), col = factor(results$data$sample), shape = factor(results$data$sample), group = factor(results$data$sample))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1), stat = "identity", size = 3) +
    {if (isTRUE(conf_adjustment)==FALSE) ggplot2::geom_errorbar(data = results$data, ggplot2::aes( xmin = results$data$ci_lower, xmax = results$data$ci_upper, width = 0.2), position = ggplot2::position_dodge(width = 1))} +
    {if (isTRUE(conf_adjustment)) ggplot2::geom_errorbar(data = results$data, ggplot2::aes( xmin = results$data$ci_lower_adjusted, xmax = results$data$ci_upper_adjusted, width = 0.2), position = ggplot2::position_dodge(width = 1))} +
    ggplot2::scale_y_discrete(limits = rev(unique(results$data$varnames)),labels= varlabels, breaks=unique(results$data$varnames)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_color_manual(
      values = results$colors, name = results$legendtitle,
      labels = results$legendlabels
    ) + ### Handle Color and Legend
    ggplot2::scale_shape_manual(
      values = results$shapes,
      name = results$legendtitle, labels = results$legendlabels
    ) +
    ggplot2::xlab(results$label_x) +
    ggplot2::ylab(results$label_y)
  
  if (is.null(results$label_summetric) == FALSE) {
    Plot <- Plot + ggplot2::geom_label(ggplot2::aes(x = Inf, y = Inf, hjust = 1, vjust = 1, label = results$label_summetric),
                                       fill = ggplot2::alpha("white", 0.02), color = ggplot2::alpha("black", 0.1), size=results$summet_size
    )
  }
  if (is.null(results$plot_title) == FALSE) Plot <- Plot + ggplot2::ggtitle(results$plot_title)
  
  return(Plot)
}



################################
### Subfunction to bootstrap ###
################################


subfunc_diffplotter <- function(x, y, samp = 1, nboots = nboots, func = func, variables,
                                func_name="none", ci_type="perc", alpha=0.05, conf_adjustment=NULL,
                                ids=ids, weights=weights,strata=strata, 
                                ids_bench = ids_bench,weights_bench = weights_bench,
                                strata_bench = strata_bench, 
                                adjustment_weighting="raking", 
                                adjustment_vars=NULL,raking_targets=NULL,
                                post_targets=NULL,boot_all=FALSE,percentile_ci =TRUE,
                                parallel=FALSE,max_samp,n_bench=NULL) {
  
  #if (length(y)<=4) return(y)
  ### Check if x and y are factors and edit them to make them fit for further analyses
  x<-unfactor(df=x,func=func[1],weight=weights,strata=strata,id=ids)
  if(is_named_vector(y)==FALSE){
    y<-unfactor(df=y,func=func[1],weight=weights_bench,strata=strata_bench,
                id=ids_bench)}

  
  ##########################################################
  ### loop to bootstrap for every Variable in data frame ###
  ##########################################################
  
  if(is.null(variables)==FALSE)variables<-colnames(x)[colnames(x)%in% variables]
  if(is.null(variables))variables<-colnames(x)
  
  if(is.null(ids)==FALSE) {if(is.na(ids)==FALSE) variables<-variables[!variables %in% ids]}
  if(is.null(weights)==FALSE) {if(is.na(weights)==FALSE) variables<-variables[!variables %in% weights]}
  if(is.null(strata)==FALSE) {if(is.na(strata)==FALSE) variables<-variables[!variables %in% strata]}
  
  if(is.null(ids_bench)==FALSE) {if(is.na(ids_bench)==FALSE) variables<-variables[!variables %in% ids_bench]}
  if(is.null(weights_bench)==FALSE) {if(is.na(weights_bench)==FALSE) variables<-variables[!variables %in% weights_bench]}
  if(is.null(strata_bench)==FALSE) {if(is.na(strata_bench)==FALSE) variables<-variables[!variables %in% strata_bench]}
  
  
  svy_boot<-boot_svy_mean(data = x,variables = variables,nboots = nboots,
                          id=ids, weight = weights, strata = strata,
                          func=func,
                          adjustment_weighting=adjustment_weighting,
                          adjustment_vars = adjustment_vars,
                          raking_targets = raking_targets,
                          post_targets=post_targets,parallel=parallel)
  
  
  #boot <- boot(data = as.data.frame(x), y = as.data.frame(y), statistic = get(func[1]), R = nboots, ncpus = parallel::detectCores(), parallel = "multicore")
  
  if(boot_all==FALSE){
    if(is_named_vector(y)==FALSE){
      means_bench<-mean_bench_func(data = y,variables = variables,
                                   id = ids_bench,weight = weights_bench,
                                   strata = strata_bench,
                                   func=func,nboots=nboots)}
    
    if(is_named_vector(y)) {means_bench<-y}
  }
  
  if(boot_all==TRUE){
    if(is_named_vector(y)){stop("boot_all can not be TRUE, as benchmark is a named vector")}
    means_bench<-boot_svy_mean(data = y,variables = variables,nboots = nboots,
                               id=ids_bench, weight = weights_bench, 
                               strata = strata_bench,
                               func=func,parallel=parallel)
  }
  ### Transform data to a data frame ###
  
  
  
  alpha_adjusted<-alpha/length(x)
  
  if(nboots==0){
    
    svy_boot2<-svy_boot[[length(svy_boot)]]
    if(is_named_vector(y)==FALSE){means_bench2<-means_bench[[length(means_bench)]]}
    if(is_named_vector(y)){means_bench2<-means_bench}
    
    abs<-FALSE
    
    if (func_name %in% c("abs_rel_mean", "abs_rel_prop",
                         "ad_mean","ad_median","ad_prop")){abs=TRUE}
    
    lower_ci<- se_mean_diff(svy_boot2,means_bench2, 
                            conf_level=(1-alpha),value = "lower_ci", abs=abs, method=func_name,
                            variables = variables)
    upper_ci<- se_mean_diff(svy_boot2,means_bench2, 
                            conf_level=(1-alpha),value = "upper_ci", abs=abs, method=func_name,
                            variables = variables)
    se_vect<- se_mean_diff(svy_boot2,means_bench2, 
                           conf_level=(1-alpha),value = "SE", abs=abs, method=func_name,
                           variables = variables)
    lower_ci_adjusted<-se_mean_diff(svy_boot2,means_bench2,  
                                    conf_level=(1-alpha_adjusted),value = "lower_ci", abs=abs, method=func_name,
                                    variables = variables)
    upper_ci_adjusted<-se_mean_diff(svy_boot2,means_bench2,  
                                    conf_level=(1-alpha_adjusted),value = "upper_ci", abs=abs, method=func_name,
                                    variables = variables)
    
  }
  
  svy_boot<-svy_boot[1:(length(svy_boot)-1)]
  if(is_named_vector(y)==FALSE){means_bench<-means_bench[1:(length(means_bench)-1)]}
  
  t_vec<-measure_function(svy_boot,means_bench,func = func,out = "diff",alpha=alpha)

  
  
  if(nboots>0){
    
    se_vect<-measure_function(svy_boot,means_bench,func = func,out = "SE",alpha=alpha, boot_all = boot_all,percentile_ci = percentile_ci)
    lower_ci<-measure_function(svy_boot,means_bench,func = func,out = "lower_ci",alpha=alpha, boot_all = boot_all, percentile_ci =percentile_ci)
    upper_ci<-measure_function(svy_boot,means_bench,func = func,out = "upper_ci",alpha=alpha, boot_all = boot_all, percentile_ci =percentile_ci)
    lower_ci_adjusted<-measure_function(svy_boot,means_bench,func = func,out = "lower_ci",alpha=alpha_adjusted, boot_all = boot_all, percentile_ci =percentile_ci)
    upper_ci_adjusted<-measure_function(svy_boot,means_bench,func = func,out = "upper_ci",alpha=alpha_adjusted, boot_all = boot_all, percentile_ci =percentile_ci)  
  }

  
  
  ########################
  ### weitere schritte ###
  ########################
  
  
  data <- as.data.frame(t_vec)
  rownames(data)<-1:nrow(data)
  data$se_vec <- se_vect
  names <- variables #rownames(data) ### to align values in plot
  data$varnames <- names
  data$ci_lower<-lower_ci
  data$ci_upper<-upper_ci
  data$ci_level<- 1-alpha

  
  if (is.null(conf_adjustment)==FALSE){
    
    data$ci_lower_adjusted<-lower_ci_adjusted
    data$ci_upper_adjusted<-upper_ci_adjusted
    data$ci_level_adjusted<- 1-alpha_adjusted
  }
  
  n_df_func<-function(df,variable){
    
    length(stats::na.omit(df[,variable]))
    
  }
  
  
  data$n_df<-as.vector(sapply(variables,n_df_func,df=x))
  if(is_named_vector(y)==F) {data$n_bench<-as.vector(sapply(variables,n_df_func,df=y))}
  if(is.null(n_bench)) data$n_bench<-NA
  if(is.null(n_bench)==F) data$n_bench<-n_bench
  
  data$estimate_bench<-purrr::map(1:length(means_bench), ~means_bench[[.x]][[1]])
  data$estimate_df<-purrr::map(1:length(means_bench), ~svy_boot[[.x]][[1]])
  
  if (is.null(conf_adjustment)){
    names(data) <- c("t_vec", "se_vec", "varnames","ci_lower","ci_upper","ci_level","n_df","n_bench","estimate_bench","estimate_df")}
  
  if (is.null(conf_adjustment)==FALSE){
    names(data) <- c("t_vec", "se_vec", "varnames","ci_lower","ci_upper","ci_level", "ci_lower_adjusted",
                     "ci_upper_adjusted","adjusted_ci_level","n_df","n_bench","estimate_bench","estimate_df")}
  
  
  data$se_vec <- as.numeric(data$se_vec)
  
  data$sample <- samp
  
  message(paste("survey",samp,"of",max_samp,"is compared"))
  
  
  return(data)
}


se_mean_diff<-function(df1,df2, conf_level =0.95, value="lower_ci", abs=FALSE, method="d_mean",
                       id=NULL,weight=NULL,strata=NULL,
                       id_bench=NULL,weight_bench=NULL,
                       strata_bench=NULL, 
                       variables){

  if(inherits(df1,"data.frame")){
    design_df<-get_survey_design(df1, id=id,weight=weight,strata=strata)}
  
  if(inherits(df2,"data.frame")){
    design_bench<-get_survey_design(df2, id=id_bench,weight=weight_bench,
                                    strata=strata_bench)}
  
  if(inherits(df1,"data.frame")==FALSE){
    design_df<-df1}
  if(inherits(df1,"data.frame")==FALSE){
    design_bench<-df2
  }
  
  
  ### ci function for single variables ###
  se_mean_diff_var<-function(variable, design_df, design_bench,
                             conf_level =0.95,value="lower_ci", abs=FALSE, 
                             method="d_mean"){
    
    ### prepare relevant values for weighted and unweighted 
    
    n_df<- length(stats::na.omit(design_df$variables[,variable]))
    #n_bench<- length(stats::na.omit(design_bench$variables[,variable]))
    
    variance_df<- survey::svyvar(stats::reformulate(variable),design_df, na.rm=TRUE)
    #variance_bench<- survey::svyvar(stats::reformulate(variable),design_bench, na.rm=TRUE)
    
    mean_df<- survey::svymean(stats::reformulate(variable),design_df, na.rm=TRUE)
    if(is_named_vector(design_bench)==F){
      mean_bench<- survey::svymean(stats::reformulate(variable),design_bench, na.rm=TRUE)}
    if(is_named_vector(design_bench)){
      mean_bench<-design_bench[variable]
    }
    
    table_df<- survey::svytable(stats::reformulate(variable),design_df)
    if(is_named_vector(design_bench)==F){
      table_bench<- survey::svytable(stats::reformulate(variable),design_bench)
      mode<-names(table_bench[which.max(table_bench)])}
    
    alpha<-1-conf_level
    
    
    if (method=="d_mean"|method=="ad_mean"|
        method=="d_prop"|method=="ad_prop") {
      #SE<- sqrt(stats::var(var1)/length(var1)+stats::var(var2)/length(var2))
      SE<- sqrt(variance_df/n_df)
      
      if(abs==FALSE){
        upper<- mean_df - mean_bench + stats::qnorm(1-alpha/2) * SE
        lower<- mean_df - mean_bench - stats::qnorm(1-alpha/2) * SE
      }
      
      if (abs==TRUE){
        upper<- abs(mean_df - mean_bench) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(mean_df - mean_bench) - stats::qnorm(1-alpha/2) * SE
      }
    }
    
    if (method=="d_median"|method=="ad_median") {
      #SE<- sqrt(stats::var(var1)/length(var1)+stats::var(var2)/length(var2))
      SE<- sqrt(variance_df/n_df)
      median_df<-as.numeric(survey::svyquantile(stats::reformulate(variable),design_df, quantiles=0.5,na.rm=TRUE)[[variable]][1])
      median_bench<-as.numeric(survey::svyquantile(stats::reformulate(variable),design_bench, quantiles=0.5,na.rm=TRUE)[[variable]][1])
      
      
      if(abs==FALSE){
        upper<- median_df - median_bench + stats::qnorm(1-alpha/2) * SE
        lower<- median_df - median_bench - stats::qnorm(1-alpha/2) * SE
      }
      
      if (abs==TRUE){
        upper<- abs(median_df - median_bench) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(median_df - median_bench) - stats::qnorm(1-alpha/2) * SE
      }
    }
    
    if (method=="mode_prop") {
      
      p1<-(table_df[mode])/(sum(table_df))
      p2<-(table_bench[mode])/(sum(table_bench))
      #p1 <- table(var1[var1==Mode(var2)])/length(var1)
      #p2 <- table(var2[var2==Mode(var2)])/length(var2)
      
      
      #SE<- sqrt(p1*(1-p1)/length(var1)+p2*(1-p2)/length(var2))
      SE<- sqrt(p1*(1-p1)/n_df)
      
      if(abs==FALSE){
        upper<- p1-p2 + stats::qnorm(1-alpha/2) * SE
        lower<- p1-p2 - stats::qnorm(1-alpha/2) * SE}
      
      
      if (abs==TRUE){
        upper<- abs(p1-p2) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(p1-p2) - stats::qnorm(1-alpha/2) * SE}
      
    }
    
    if (method=="avg_abs_prop_diff") {
      
      a<-prop.table(survey::svytable(stats::reformulate(variable),design_df))
      b<-prop.table(survey::svytable(stats::reformulate(variable),design_bench))
      
      avg_abs_prop_diff<-mean(abs(as.vector(a-b)))
      
      
      
      #SE<- sqrt(p1*(1-p1)/n_df)
      SE<- sqrt(mean((a-avg_abs_prop_diff)^2)/n_df)
      
      
      if (abs==TRUE){
        upper<- abs(avg_abs_prop_diff) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(avg_abs_prop_diff) - stats::qnorm(1-alpha/2) * SE}
      
    }
    
    
    
    
    
    if (method=="rel_mean"|method=="abs_rel_mean"| 
        method=="rel_prop" | method=="abs_rel_prop") {
      
      
      #SE <- sqrt((stats::var(var1)/length(var1) + stats::var(var2)/length(var2)) / (mean(var1)-mean(var2))^2)
      var_rel <- (1/(mean_bench^2))*(variance_df)
      SE<-sqrt(var_rel)/sqrt(n_df)
      rel_diff_mean<- (mean_df - mean_bench) / (mean_bench)
      
      if(abs==FALSE){
        upper<- rel_diff_mean + stats::qnorm(1-alpha/2) * SE
        lower<- rel_diff_mean - stats::qnorm(1-alpha/2) * SE}
      
      
      if (abs==TRUE){
        upper<- abs(rel_diff_mean) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(rel_diff_mean) - stats::qnorm(1-alpha/2) * SE}
      
    }
    
    ### return ###
    if (value=="lower_ci") return(lower)
    if (value=="upper_ci") return(upper)
    if (value=="SE") return(SE)
    
  }
  
  ### ci function for whole data frame ###
  sapply(X=variables,FUN=se_mean_diff_var, design_df=design_df, design_bench=design_bench,
         value = value, abs=abs, method=method)
  
}


get_survey_design<-function(df, id=NULL,weight=NULL,strata=NULL){
  
  if (is.null(id)==FALSE) {
    if(is.na(id)==FALSE){
      id_new<-df[,id]
    }
    if(is.na(id)){id_new<-c(1:nrow(df))}
  }
  
  if (is.null(weight)==FALSE) {
    if(is.na(weight)==FALSE){
      weight_new<-df[,weight]
      df[,weight]<-NULL
    }
    if(is.na(weight)){weight_new<-rep(1,nrow(df))}
  }
  
  if (is.null(strata)==FALSE) {
    if(is.na(strata)==FALSE){
      strata_new<-df[,strata]
      df[,strata]<-NULL
    }
    if(is.na(strata)){strata_new<-NULL}
  }
  
  
  if(is.null(weight)) weight_new<-rep(1,nrow(df))
  if(is.null(id)) id_new<-c(1:nrow(df))
  if(is.null(strata)) strata_new<-NULL
  
  
  
  
  design <- survey::svydesign(
    data = df,
    id = id_new, 
    weights = weight_new,
    strata = strata_new
  )
  
  return(design)
}



final_data2<-function(data, name_dfs, name_benchmarks, summetric=NULL, colors=NULL,
                      shapes=NULL, legendlabels=NULL, legendtitle=NULL , label_x=NULL, label_y=NULL,
                      summet_size=NULL, plot_title=NULL,funct=NULL,type=NULL,
                      variables=NULL,varlabels=NULL,ndigits=3){
  
  
  ###########################
  ### save data as a list ###
  ###########################
  
  data_list<-list()
  
  
  #######################
  ### get a summetric ###
  #######################
  
  if (is.null(summetric) == FALSE) label_summet<-
      calculate_summetric(data=data, summetric = summetric,
                          name_dfs = name_dfs, name_benchmarks = name_benchmarks,
                          funct = funct,ndigits=ndigits)
  
  if (is.null(summetric) == TRUE) label_summet=NULL
  
  
  #####################
  ### Decide colors ###
  #####################
  
  color<-c("blue","red","purple","green","yellow", "brown","orange2", "cyan2",
           "springgreen3", "beige", "bisque4", "aquamarine", "chocolate",
           "darkmagenta", "pink", "darksalmon", "gold", "cornflowerblue", "cyan4",
           "deeppink")
  
  if (is.null(colors) == FALSE) {
    color[1:(length(colors))] <- colors
  }
  
  colors <- color
  
  #####################
  ### Decide shapes ###
  #####################
  
  shape<-c(16,15,17, 18,19,21,22,23,24,25,1,2,0,5,6,7,8,9,10,11,12,13,14)
  
  if (is.null(shapes) == FALSE) {
    shape[1:(length(shapes))] <- shapes
  }
  
  shapes <- shape
  
  ############################
  ### label Legend & title ###
  ############################
  
  def_leglabels<-NULL
  
  for (i in 1:length(name_dfs)){
    
    label<- paste(name_dfs[i], " vs. ", name_benchmarks[i])
    
    if (is.null(def_leglabels)==FALSE) def_leglabels<-c(def_leglabels,label)
    if (is.null(def_leglabels)==TRUE) def_leglabels<-label
  }
  
  if (is.null(legendlabels) == FALSE) {
    def_leglabels[1:(length(legendlabels))] <- legendlabels
  }
  
  legendlabels <- def_leglabels
  
  legendtitle <- if (is.null(legendtitle)) legendtitle <- "Data frames" else legendtitle<-legendtitle
  
  ### label AXIS ###
  ### label X-Axis
  if (is.null(label_x)) (if (is.character(funct)){
    if(type=="comparison"){
      if (funct=="d_mean") label_x <- "Bias: Difference in Mean"
      if (funct=="ad_mean") label_x <- "Bias: Absolute Difference in Mean"
      if (funct=="d_prop") label_x <- "Bias: Difference in Proportions"
      if (funct=="ad_prop") label_x <- "Bias: Absolute Difference in Proportions"
      if (funct=="rel_mean") label_x <- "Bias: Relative Difference in Mean"
      if (funct=="abs_rel_mean") label_x <- "Bias: Absolute Relative Difference in Mean"
      if (funct=="rel_prop") label_x <- "Bias: Relative Difference in Proportions"
      if (funct=="abs_rel_prop") label_x <- "Bias: Absolute Relative Difference in Proportions"
      if (funct=="ad_median") label_x <- "Bias: Absolute Relative Difference in Median"
    }
    if(type=="nonresponse"){
      if (funct=="d_mean") label_x <- "Nonresponse Bias:\n Difference in Mean"
      if (funct=="ad_mean") label_x <- "Nonresponse Bias:\n Absolute Difference in Mean"
      if (funct=="d_prop") label_x <- "Nonresponse Bias:\n Difference in Proportions"
      if (funct=="ad_prop") label_x <- "Nonresponse Bias:\n Absolute Difference in Proportions"
      if (funct=="rel_mean") label_x <- "Nonresponse Bias:\n Relative Difference in Mean"
      if (funct=="abs_rel_mean") label_x <- "Nonresponse Bias:\n Absolute Relative Difference in Mean"
      if (funct=="rel_prop") label_x <- "Nonresponse Bias:\n Relative Difference in Proportions"
      if (funct=="abs_rel_prop") label_x <- "Nonresponse Bias:\n Absolute Relative Difference in Proportions"
      if (funct=="ad_median") label_x <- "Nonresponse Bias:\n Absolute Relative Difference in Median"
    }
    
  } 
  else label_x <- "Difference-Metric")
  
  ### label Y-Axis
  if (is.null(label_y)) label_y <- "Variables"
  
  
  #######################
  ### add all to list ###
  #######################
  
  data_list[[1]] <- data
  data_list[[2]] <- label_summet
  data_list[[3]] <- colors
  data_list[[4]] <- shapes
  data_list[[5]] <- legendlabels
  data_list[[6]] <- legendtitle
  data_list[[7]] <- label_x
  data_list[[8]] <- label_y
  data_list[[9]] <- as.character(funct)
  data_list[[10]]<- summetric
  data_list[[11]] <- summet_size
  data_list[[12]] <- plot_title
  data_list[[13]]<- name_dfs
  data_list[[14]]<- name_benchmarks
  if(!is.null(variables)){
    data_list[[15]]<- variables 
  }
  if(is.null(variables)){
    data_list[15]<- list(NULL)
  }
  if(!is.null(varlabels)){
    data_list[[16]]<- varlabels
  }
  if(is.null(varlabels)){
    data_list[16]<- list(NULL)
  }
  
  
  names(data_list)<-c("data","label_summetric","colors","shapes","legendlabels",
                      "legendtitle","label_x","label_y","measure","summet","summet_size",
                      "plot_title","name_dfs","name_benchmarks","variables","varlabels")
  
  return(data_list)
}


### function to calculate bootstrap means for the dataframe ###
boot_svy_mean<-function(data,variables, nboots=2000, 
                        id=NULL, weight=NULL, strata=NULL,
                        func,
                        adjustment_weighting="raking", 
                        adjustment_vars=NULL,raking_targets=NULL,
                        post_targets=NULL,parallel=FALSE){
  
  if(is.null(id)) data$id<-1:nrow(data)
  if(is.null(id)==FALSE) {if(is.na(id)) data$id<-1:nrow(data)}
  if(is.null(id)==FALSE) {if (is.na(id)==FALSE) data$id<-data[,id]}
  
  if(is.null(weight)) data$weight<-1
  if(is.null(weight)==FALSE) {if(is.na(weight)) data$weight<-1}
  if(is.null(weight)==FALSE) {if (is.na(weight)==FALSE) data$weight<-data[,weight]}
  
  if(is.null(strata)==FALSE) {if(is.na(strata)==FALSE) strata<-data[,strata]}
  
  
  
  
  
  data_design <- survey::svydesign(data = data, ids = ~ id,weights = ~weight, 
                                   strata=strata)
  
  if(nboots>0){
    data_design <- svrep::as_bootstrap_design(data_design,
                                              type = "Rao-Wu-Yue-Beaumont",
                                              replicates = nboots)}
  
  
  if(is.null(adjustment_vars)==FALSE & is.null(raking_targets)==FALSE & adjustment_weighting=="raking"){
    
    adjustment_vars<-purrr::map(paste0("~",adjustment_vars),stats::as.formula)
    #return(raking_targets)
    data_design <- survey::rake(design = data_design, 
                                population.margins = raking_targets, 
                                sample.margins = adjustment_vars, 
                                control = list(maxit =1000, epsilon = 5, verbose=FALSE))
    
    
  }
  
  if(is.null(adjustment_vars)==FALSE & is.null(post_targets)==FALSE & adjustment_weighting=="post_strat"){
    
    
    data_design <- survey::postStratify(design= data_design, 
                                        strata=stats::reformulate(adjustment_vars), 
                                        population=post_targets,
                                        partial = FALSE)
    
    
  }
  
  
  
  
  
  final_boot_svy_mean<-function(variable,design,func){
    if(func!="ad_median") results<-survey::svymean(x = stats::reformulate(variable), 
                                                   design = design ,na.rm=TRUE,return.replicates=TRUE)
    if(func=="ad_median") results<-survey::svyquantile(x = stats::reformulate(variable), 
                                                       quantile=c(0.5),
                                                       design = design ,na.rm=TRUE,return.replicates=TRUE, 
                                                       interval.type="quantile", ci=FALSE)
    
    return(results)
  }
  
  #if(parallel!=FALSE){future::plan(future::multisession,workers=parallel)}
  
  #if(is.null(adjustment_vars)) out<-lapply(X=variables,FUN=final_boot_svy_mean,design=data_design, func=func)
  if(is.null(adjustment_vars)) out<-furrr::future_map(variables,~final_boot_svy_mean(.x,design = data_design,func=func))
  
  # if(is.null(adjustment_vars)==FALSE & is.null(raking_targets)==FALSE & adjustment_weighting=="raking"){
  #   out<-lapply(X=variables,FUN=final_boot_svy_mean,design=data_design, func=func) 
  # }
  if(is.null(adjustment_vars)==FALSE & is.null(raking_targets)==FALSE & adjustment_weighting=="raking"){
    out<-furrr::future_map(variables,~final_boot_svy_mean(.x,design = data_design,func=func))}
  
  # if(is.null(adjustment_vars)==FALSE & is.null(post_targets)==FALSE & adjustment_weighting=="post_strat"){
  #   out<-lapply(X=variables,FUN=final_boot_svy_mean,design=data_design, func=func) 
  # }
  if(is.null(adjustment_vars)==FALSE & is.null(post_targets)==FALSE & adjustment_weighting=="post_strat"){
    out<-furrr::future_map(variables,~final_boot_svy_mean(.x,design = data_design,func=func))}
  
  out[[length(out)+1]]<-data_design
  out
}


### function to calculate survey means for the benchmark ###
mean_bench_func<-function(data,variables, 
                          id=NULL, weight=NULL, strata=NULL,
                          func=func, nboots){
  
  if(is.null(id)) data$id<-c(1:nrow(data))
  if(is.null(id)==FALSE) {if(is.na(id)) data$id<-1:nrow(data)}
  if(is.null(id)==FALSE) {if (is.na(id)==FALSE) data$id<-data[,id]}
  
  if(is.null(weight)) data$weight<-1
  if(is.null(weight)==FALSE) {if(is.na(weight)) data$weight<-1}
  if(is.null(weight)==FALSE) {if (is.na(weight)==FALSE) data$weight<-data[,weight]}
  
  if(is.null(strata)==FALSE) {if(is.na(strata)==FALSE) strata<-data[,strata]}
  
  
  data_design <- survey::svydesign(data = data, ids = ~ id,weights = ~weight, 
                                   strata=strata)
  
  
  final_boot_svy_mean<-function(variable,design,func){
    if(func!="ad_median") results<-survey::svymean(x = stats::reformulate(variable), 
                                                   design = design ,na.rm=TRUE,return.replicates=TRUE)
    if(func=="ad_median") results<-survey::svyquantile(x = stats::reformulate(variable), 
                                                       quantile=c(0.5),
                                                       design = design ,na.rm=TRUE,return.replicates=TRUE, ci=FALSE)
    return(results)
  }
  
  out<-lapply(X=variables,FUN=final_boot_svy_mean,design=data_design,func=func)
  out[[length(out)+1]]<-data_design
  out
}





measure_function<-function(svyboot_object,mean_bench_object,func="abs_rel_mean",
                           out="diff",alpha=0.05, percentile_ci= TRUE,
                           boot_all=FALSE){
  
  if(func == "abs_rel_mean" | func =="abs_rel_prop"){
    subfunc_abs_rel_mean<-function(svyboot_object_part,mean_bench_object_part,
                                   out="diff", alpha=0.05){
      
      
      if(is_named_vector(mean_bench_object_part)==FALSE){
        diff<-abs((svyboot_object_part[[1]][1]-mean_bench_object_part[[1]])/mean_bench_object_part[[1]])
      }
      if(is_named_vector(mean_bench_object_part)){
        diff<-abs((svyboot_object_part[[1]][1]-mean_bench_object_part)/mean_bench_object_part)
      }
      if (out=="diff") return(diff)
      
      if(boot_all==FALSE & percentile_ci==FALSE){
        var_rel<-abs((1/mean_bench_object_part[[1]]^2)*stats::var(svyboot_object_part[[2]]))
        SE<-sqrt(var_rel)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE}
      
      if(boot_all==FALSE & percentile_ci==TRUE){
        if(is_named_vector(mean_bench_object_part)==FALSE){
          diff<- abs((svyboot_object_part[[2]] - mean_bench_object_part[[1]])/mean_bench_object_part[[1]])}
        if(is_named_vector(mean_bench_object_part)==FALSE){
          diff<- abs((svyboot_object_part[[2]] - mean_bench_object_part)/mean_bench_object_part)}
        
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==TRUE){
        diff<- abs((svyboot_object_part[[2]] - mean_bench_object_part[[2]])/mean_bench_object_part[[2]])
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==FALSE){
        diff2<- abs((svyboot_object_part[[2]] - mean_bench_object_part[[2]])/mean_bench_object_part[[2]])
        SE<- stats::sd(diff2)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE
        
      }
      
      if (out=="SE") return(SE)
      if (out=="lower_ci") return(lower_ci)
      if (out=="upper_ci") return(upper_ci)
      
      
    }
    results<-mapply(subfunc_abs_rel_mean,svyboot_object,mean_bench_object,
                    MoreArgs=list(out=out, alpha=alpha))
  }
  
  if(func == "rel_mean" | func =="rel_prop"){
    subfunc_rel_mean<-function(svyboot_object_part,mean_bench_object_part,
                               out="diff", alpha=0.05){
      
      diff<-(svyboot_object_part[[1]][1]-mean_bench_object_part[[1]])/mean_bench_object_part[[1]]
      if (out=="diff") return(diff)
      
      if(boot_all==FALSE & percentile_ci==FALSE){
        var_rel<-(1/mean_bench_object_part[[1]]^2)*stats::var(svyboot_object_part[[2]])
        SE<-sqrt(var_rel)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE}
      
      if(boot_all==FALSE & percentile_ci==TRUE){
        diff<- (svyboot_object_part[[2]] - mean_bench_object_part[[1]])/mean_bench_object_part[[1]]
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==TRUE){
        diff<- (svyboot_object_part[[2]] - mean_bench_object_part[[2]])/mean_bench_object_part[[2]]
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==FALSE){
        diff2<- (svyboot_object_part[[2]] - mean_bench_object_part[[2]])/mean_bench_object_part[[2]]
        SE<- stats::sd(diff2)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE
      }
      
      if (out=="SE") return(SE)
      if (out=="lower_ci") return(lower_ci)
      if (out=="upper_ci") return(upper_ci)
      
      
    }
    results<-mapply(subfunc_rel_mean,svyboot_object,mean_bench_object,
                    MoreArgs=list(out=out, alpha=alpha))
  }
  
  if(func == "ad_mean" | func =="ad_prop"){
    subfunc_ad_mean<-function(svyboot_object_part,mean_bench_object_part,
                              out="diff", alpha=0.05){
      
      
      diff<-abs(svyboot_object_part[[1]][1]-mean_bench_object_part[[1]])
      if (out=="diff") return(diff)
      
      if(boot_all==FALSE & percentile_ci==FALSE){
        var<-stats::var(svyboot_object_part[[2]])
        SE<-sqrt(var)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE}
      
      if(boot_all==FALSE & percentile_ci==TRUE){
        diff<- abs(svyboot_object_part[[2]] - mean_bench_object_part[[1]])
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==TRUE){
        diff<- abs(svyboot_object_part[[2]] - mean_bench_object_part[[2]])
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==FALSE){
        diff2<- abs(svyboot_object_part[[2]] - mean_bench_object_part[[2]])
        SE<- stats::sd(diff2)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE
      }
      
      if (out=="SE") return(SE)
      if (out=="lower_ci") return(lower_ci)
      if (out=="upper_ci") return(upper_ci)
      
    }
    results<-mapply(subfunc_ad_mean,svyboot_object,mean_bench_object,
                    MoreArgs=list(out=out, alpha=alpha))
  }
  
  if(func == "d_mean" | func =="d_prop"){
    subfunc_d_mean<-function(svyboot_object_part,mean_bench_object_part,
                             out="diff", alpha=0.05){
      
      
      diff<-svyboot_object_part[[1]][1]-mean_bench_object_part[[1]]
      if (out=="diff") return(diff)
      
      if(boot_all==FALSE & percentile_ci==FALSE){
        var<-stats::var(svyboot_object_part[[2]])
        SE<-sqrt(var)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE}
      
      if(boot_all==FALSE & percentile_ci==TRUE){
        diff<- svyboot_object_part[[2]] - mean_bench_object_part[[1]]
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==TRUE){
        diff<- svyboot_object_part[[2]] - mean_bench_object_part[[2]]
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==FALSE){
        diff2<- svyboot_object_part[[2]] - mean_bench_object_part[[2]]
        SE<- stats::sd(diff2)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE
      }
      
      if (out=="SE") return(SE)
      if (out=="lower_ci") return(lower_ci)
      if (out=="upper_ci") return(upper_ci)
      
      
    }
    
    results<-mapply(subfunc_d_mean,svyboot_object,mean_bench_object,
                    MoreArgs=list(out=out, alpha=alpha))}
  
  if(func == "ad_median"){
    subfunc_ad_median<-function(svyboot_object_part,mean_bench_object_part,
                                out="diff", alpha=0.05){
      
      
      diff<-abs(svyboot_object_part[[1]][1]-mean_bench_object_part[[1]])
      if (out=="diff") return(diff)
      
      if(boot_all==FALSE & percentile_ci==FALSE){
        var<-stats::var(svyboot_object_part[[2]])
        SE<-sqrt(var)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE}
      
      if(boot_all==FALSE & percentile_ci==TRUE){
        diff<- abs(svyboot_object_part[[2]] - mean_bench_object_part[[1]])
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==TRUE){
        diff<- abs(svyboot_object_part[[2]] - mean_bench_object_part[[2]])
        upper_ci<-stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE)
        lower_ci<-stats::quantile(diff, probs=(alpha/2),na.rm=TRUE)
        SE<- stats::sd(diff)
      }
      
      if(boot_all==TRUE & percentile_ci==FALSE){
        diff2<- abs(svyboot_object_part[[2]] - mean_bench_object_part[[2]])
        SE<- stats::sd(diff2)
        upper_ci<- diff + stats::qnorm(1-alpha/2) * SE
        lower_ci<- diff - stats::qnorm(1-alpha/2) * SE
      }
      
      if (out=="SE") return(SE)
      if (out=="lower_ci") return(lower_ci)
      if (out=="upper_ci") return(upper_ci)
      
    }
    
    results<-mapply(subfunc_ad_median,svyboot_object,mean_bench_object,
                    MoreArgs=list(out=out, alpha=alpha))
  }
  
  
  results
  
}










unfactor<-function(df, func, weight, strata,id){
  
  df2<-NULL
  
  if(is.null(id)==FALSE) if(is.na(id)==FALSE){
    id_var<-df[,id]
    df2<-as.data.frame(df[,-which(colnames(df) %in% c(id))])
  } 
  
  if(is.null(weight)==FALSE) if(is.na(weight)==FALSE){
    weight_var<-df[,weight]
    df2<-as.data.frame(df[,-which(colnames(df) %in% c(weight))]) 
  } 
  
  
  if(is.null(strata)==FALSE) if(is.na(strata)==FALSE){ 
    strata_var<-df[,strata]
    df2<-as.data.frame(df[,-which(colnames(df) %in% c(strata))])
  }
  
  if(is.null(df2))df2<-df

  ### check, if df variables are factors ###
  if(func=="REL_MEAN"| func=="ABS_REL_MEAN"| 
     func=="ABS_PROP_DIFF"| func=="PROP_DIFF"|
     func=="d_prop"|func=="d_mean"|
     func=="ad_prop"|func=="ad_mean"|
     func=="abs_rel_mean"|func=="rel_mean"|
     func=="abs_rel_prop"|func=="rel_prop"){
    
    for (i in 1:ncol(df2)){
      if(is.factor(df2[,i])){
        if(length(levels(df2[,i]))==2){
          if(all(levels(df2[,i])== c("0","1"))) df2[,i]<-as.numeric(as.character(df2[,i]))
          else(stop(paste(colnames(df)[i],"must be coded as 0 and 1")))
        }
        if(length(levels(df2[,i]))>2) stop(paste(colnames(df2)[i],"must be numeric, or a factor coded as 0 or 1, for the chosen function"))
      }
    }
  }
  
  if(is.null(id)==FALSE) if(is.na(id)==FALSE){
    df2[,id]<-id_var
  } 
  
  if(is.null(weight)==FALSE) if(is.na(weight)==FALSE){
    df2[,weight]<-weight_var
  } 
  
  
  if(is.null(strata)==FALSE) if(is.na(strata)==FALSE){ 
    df2[,strata]<-strata_var
  }
  
  
  df2
}


calculate_summetric<-function(data, summetric=NULL, funct, name_dfs,name_benchmarks, ndigits=3){
  
  
  for (i in 1:max(data$sample)){
    
    bias<-data$t_vec[data$sample==i]
    
    data$mse[data$sample==i]<-sum(bias*bias)/length(bias)
    data$rmse[data$sample==i]<-sqrt(sum(bias*bias)/length(bias))
    data$avg[data$sample==i]<-sum(abs(bias))/length(bias)
  }
  
  
  #if (is.null(summetric) == FALSE) {
  if (summetric == "rmse1") {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1){
        labelrmse <- paste("RMSE:\n ", name_dfs[i], " & ", name_benchmarks[i], "\n", "  ",
                           format(round(unique(data$rmse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                           "\n",
                           sep = "", collapse = NULL)}
      if (i>1){
        labelrmse <- paste(name_dfs[i], " & ", name_benchmarks[i], "\n", "  ",
                           format(round(unique(data$rmse[data$sample==i]), digits = ndigits),nsmall=ndigits), 
                           "\n",
                           sep = "", collapse = NULL)}
      
      
      if (i==1) label_summet<-labelrmse
      if (i>1) label_summet<-paste(label_summet, labelrmse)
    }}
  
  
  if (summetric == "mse1") {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelmse <- paste("MSE:\n ", name_dfs[i], " & ", name_benchmarks[i], "\n", "  ",
                          format(round(unique(data$mse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      if (i>1) {
        labelmse <- paste(name_dfs[i], " & ", name_benchmarks[i], "\n", "  ",
                          format(round(unique(data$mse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelmse
      if (i>1) label_summet<-paste(label_summet, labelmse)
    }}
  
  
  if (summetric == "rmse2") {
    
    for (i in 1:length(name_dfs)){
      
      if(i==1){
        labelrmse <- paste("RMSE:\n ", name_dfs[i], "   ",
                           format(round(unique(data$rmse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                           "\n",
                           sep = "", collapse = NULL)}
      if (i>1) {
        labelrmse <- paste(name_dfs[i], "   ",
                           format(round(unique(data$rmse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                           "\n",
                           sep = "", collapse = NULL)}
      
      
      if (i==1) label_summet<-labelrmse
      if (i>1) label_summet<-paste(label_summet, labelrmse)
      
    }}
  
  
  if (summetric == "mse2") {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelmse <- paste("MSE:\n ", name_dfs[i],"   ",
                          format(round(unique(data$mse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i>1) {
        labelmse <- paste(name_dfs[i], "   ",
                          format(round(unique(data$mse[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelmse
      if (i>1) label_summet<-paste(label_summet, labelmse)
      
    }}
  
  ### AARB Long ###
  if (summetric == "avg1" & (funct=="rel_mean" | funct=="abs_rel_mean" |
                             funct=="rel_prop" | funct=="abs_rel_prop")) {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelavg <- paste("Absolute Average\n Relative Bias:\n ", name_dfs[i],"   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i>1) {
        labelavg <- paste(name_dfs[i], "   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelavg
      if (i>1) label_summet<-paste(label_summet, labelavg)
      
    }}
  
  ### AAB Long ###
  if (summetric == "avg1" & !(funct=="rel_mean" | funct=="abs_rel_mean" |
                              funct=="rel_prop" | funct=="abs_rel_prop")) {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelavg <- paste("Absolute Average\n Bias:\n ", name_dfs[i],"   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i>1) {
        labelavg <- paste(name_dfs[i], "   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelavg
      if (i>1) label_summet<-paste(label_summet, labelavg)
      
    }}
  
  ### AARB Short ###
  if (summetric == "avg2" & (funct=="rel_mean" | funct=="abs_rel_mean" |
                             funct=="rel_prop" | funct=="abs_rel_prop")) {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelavg <- paste("AARB:\n ", name_dfs[i],"   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i>1) {
        labelavg <- paste(name_dfs[i], "   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelavg
      if (i>1) label_summet<-paste(label_summet, labelavg)
      
    }}
  
  ### AAB Short ###
  if (summetric == "avg2" & !(funct=="rel_mean" | funct=="abs_rel_mean" |
                              funct=="rel_prop" | funct=="abs_rel_prop")) {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelavg <- paste("AAB:\n ", name_dfs[i],"   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i>1) {
        labelavg <- paste(name_dfs[i], "   ",
                          format(round(unique(data$avg[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelavg
      if (i>1) label_summet<-paste(label_summet, labelavg)
      
    }}
  
  
  
  if (summetric == "R") {
    
    for (i in 1:length(name_dfs)){
      
      if (i==1) {
        labelavg <- paste("R-Indicator:\n ", name_dfs[i],"   ",
                          format(round(unique(data$R_indicator[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i>1) {
        labelavg <- paste(name_dfs[i], "   ",
                          format(round(unique(data$R_indicator[data$sample==i]), digits = ndigits),nsmall=ndigits),
                          "\n",
                          sep = "", collapse = NULL)}
      
      if (i==1) label_summet<-labelavg
      if (i>1) label_summet<-paste(label_summet, labelavg)
      
    }}
  
  #}
  
  
  
  return(label_summet)
}


is_named_vector <- function(x) 
{is.vector(x,mode = "numeric") & !is.null(names(x)) & !any(is.na(names(x)))}

