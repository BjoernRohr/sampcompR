########################################################
###                                                  ###
### 		Subject:	Uni Compare Function               ###
### 		Date: 		May 2023                           ###
### 		Author: 	Bjoern Rohr                        ###
### 	Version:  	1.00                               ###
###                                                  ###
### 		Bugfix:   	/                                ###
###                                                  ###
########################################################


#################################
### The function for the Plot ###
#################################

### Documentation of the diff_plotter_function ###

#' Compare data frames and Plot Differences
#'
#' Returns a plot or data showing the difference of two or more
#' data frames The differences are calculated on the base of
#' differing metrics, chosen in the funct argument. All used data frames must
#' contain at least one column named equal in all data frames, that has equal
#' values. For a comparison of weighted data in bootstrapping, uni_compare 2 is 
#' faster and more accurate, as it's bootstrap technique is based on the 
#' \link[survey]{svydesign} package.
#'
#' @param dfs A character vector containing the names of data frames to compare against the benchmarks. 
#' @param benchmarks A character vector containing the names of benchmarks to compare the data frames against.
#' The vector must either to be the same length as \code{dfs}, or length 1. If it has length 1 every
#' df will be compared against the same benchmark. Benchmarks can either be the name of data frames or 
#' the name of a list of tables. The tables in the list need to be named as the respective variables
#' in the data frame of comparison.
#' @param variables A character vector containing the names of the variables for the comparison. If NULL,
#' all variables named similar in both the dfs and the benchmarks will be compared. Variables missing
#' in one of the data frames or the benchmarks will be neglected for this comparison.
#' @param nboots  The Number of bootstraps used to calculate standard errors. Must either be >2 or 0.
#' If >2 bootstrapping is used to calculate standard errors with \code{nboots} iterations. If 0, SE
#' is calculated analytically. We do not recommend using \code{nboots} =0 because this method is not
#' yet suitable for every \code{funct} used and every method. Depending on the size of the data and the
#' number of bootstraps, \code{uni_compare} can take a while.
#' @param funct A function or a vector of functions to calculate the difference between the
#' data frames. If a single input is given, the same function will be used for all variables.
#' If the input is a vector, the vector has to be of the same length as \code{variables}. 
#' Then for eachvariable the indicated function will be used. The input can either be a 
#' character indicating a predefined function, or function (not yet clearly defined). 
#' Predefined functions are:
#' 
#' * \code{"d_mean"}, \code{"ad_mean"} A function to calculate the (absolute) difference in mean of
#' the variables in dfs and benchmarks with the same name. Only applicable for
#' metric variables
#'
#' * \code{"d_prop"}, \code{"ad_prop"} A function to calculate the (absolute) difference in proportions of
#' the variables in dfs and benchmarks with the same name. Only applicable for dummy
#' variables.
#'
#' * \code{"prop_modecat"}, \code{"abs_prop_modecat"} A function to calculate the (absolute) difference in
#' proportions of the variables in dfs and benchmarks with the same name. Only applicable for
#' variables with a limited number of categories.
#'
#' * \code{"avg_abs_prop_diff"} A function to calculate the average absolute difference in
#' proportions of all categories in a variables in dfs and benchmarks with the same name.
#' Only applicable for variables with the same number of categories.
#'
#' * \code{"rel_mean"}, \code{"abs_rel_mean"} A function to calculate the (absolute) 
#' relative difference in mean of the variables in dfs and benchmarks with the same name. 
#' For more information on the formula for difference and analytic variance, see Felderer 
#' et al. (2019). Only applicable for metric variables. 
#'
#' * \code{"rel_prop"}, \code{"abs_rel_prop"} A function to calculate the (absolute) 
#' relative difference in proportions of the variables in dfs and benchmarks with 
#' the same name. it is calculated similar to the relative difference in mean 
#' (see Felderer et al., 2019), however the default label for the plot is different. 
#' Only applicable for dummy variables.
#'
#' * \code{"d_median"} \code{"ad_median"} A function to calculate the (absolute) relative difference in median of
#' the variables in dfs and benchmarks with the same name.
#'
#' @param data If TRUE, a uni_compare_object is returned, containing results of the comparison.
#' @param legendlabels A character string or vector of strings containing a label for the
#' legend.
#' @param legendtitle A character string containing the title of the Legend.
#' @param colors A vector of colors used in the plot for the
#' different comparisons.
#' @param shapes A vector of shapes applicable in [ggplot2::ggplot2()] used in the plot for
#' the different comparisons.
#' @param summetric If ,\code{"avg"}, \code{"mse1"}, \code{"rmse1"}, or \code{"R"} 
#' the respective measure is calculated for the biases of each survey. The values 
#' \code{"mse1"} and \code{"rmse2"} lead to similar results as in \code{"mse1"} and \code{"rmse1"}, 
#' with slightly different visualization in the plot. If summetric = NULL, no summetric 
#' will be displayed in the Plot. When \code{"R"} is chosen, also \code{response_identificator} 
#' is needed.
#' @param label_x,label_y A character string or vector of character strings containing a label for
#' the x-axis and y-axis.
#' @param plot_title A character string containing the title of the plot.
#' @param varlabels A character string or vector of character strings containing the new names of
#' variables, also used in plot.
#' @param name_dfs,name_benchmarks A character string or vector of character strings containing the
#' new names of the data frames and benchmarks, also used in plot.
#' @param summet_size A number to determine the size of the displayed summetric in the plot.
#' @param ci_type A character string determining the type of bootstrap ci available in the
#' \code{\link[boot]{boot.ci}} function of the code{boot} package.
#' @param silence If silence = F a warning will be displayed, if variables are excluded from either
#' data frame or benchmark, for not existing in both.
#' @param conf_level A numeric value between zero and one to determine the confidence level of the confidence
#' interval.
#' @param conf_adjustment If conf_adjustment=T the confidence level of the confidence interval will be
#' adjusted with a Bonferroni adjustment, to account for the problem of multiple comparisons.
#' @param weight,weight_bench A character vector determining variables to weight the \code{dfs} or
#' \code{benchmarks}. They have to be part of the respective data frame. If only one character is provided,
#' the same variable is used to weight every df or benchmark. If a weight variable is provided also an id
#' variable is needed.For weighting, the \code{survey} package is used.
#' @param id,id_bench A character vector determining id variables used to weight the \code{dfs} or
#' \code{benchmarks} with the help of the \code{survey} package. They have to be part of the respective
#' data frame. If only one character is provided, the same variable is used to weight every df or benchmark.
#' @param strata,strata_bench A character vector determining strata variables used to weight
#' the \code{dfs} or \code{benchmarks} with the help of the \code{survey} package. They have
#' to be part of the respective data frame. If only one character is provided, the same variable
#' is used to weight every df or benchmark.
# #' @param R_variables A character vector with the names of variables that should be used in the model 
# #' to calculate the R indicator.
#' @param type Define the type of comparison. Can either be "comparison" for a comparison between two surveys,
#' "benchmark" for a comparison between a survey and a benchmark to estimate bias in the survey, 
#' or "nonrespnse", when the function is used to measure nonresponse bias.
#' @param parallel If True, all detected cors will be used to in bootstrapping.
#'
#' @return A plot based on [ggplot2::ggplot2()] (or data frame if data==TRUE)
#' which shows the difference between two or more data frames on predetermined variables,
#' named identical in both data frames.
#' 
#' @references 
#' Felderer, B., Kirchner, A., & Kreuter, F. (2019). The Effect of Survey Mode on Data 
#' Quality: Disentangling Nonresponse and Measurement Error Bias. Journal of Official 
#' Statistics, 35(1), 93–115. https://doi.org/10.2478/jos-2019-0005
#' 
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom boot boot
#' @examples
#' 
#' ## Get Data for comparison
#' card<-wooldridge::card
#' 
#' black<-wooldridge::card[wooldridge::card$black==1,]
#' north<-wooldridge::card[wooldridge::card$south==0,]
#' white<-wooldridge::card[wooldridge::card$black==0,]
#' south<-wooldridge::card[wooldridge::card$south==1,]
#' 
#' ## use the function to plot the data 
#' univar_comp<-sampcompR::uni_compare(dfs = c("north","white"),
#'                                     benchmarks = c("south","black"),
#'                                     variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                     funct = "abs_rel_mean",
#'                                     nboots=0,
#'                                     summetric="rmse2",
#'                                     data=FALSE)
#'
#'  univar_comp
#'  

### The diff_plotter_function
uni_compare <- function(dfs, benchmarks, variables=NULL, nboots = 2000, funct = "rel_mean",
                        data = FALSE, summetric = "rmse2", varlabels = NULL, type="comparison",
                        weight =NULL, id=NULL, strata=NULL, weight_bench=NULL,id_bench=NULL, 
                        strata_bench=NULL, legendlabels = NULL, legendtitle = NULL, 
                        colors = NULL, shapes = NULL, label_x = NULL, label_y = NULL, 
                        plot_title = NULL, name_dfs=NULL, name_benchmarks=NULL,
                        summet_size=4, ci_type="perc", silence=T, conf_level=0.95, 
                        conf_adjustment=NULL, parallel = F) {


  ##################################
  ### Errors if inputs are wrong ###
  ##################################

  ### Not enough Data frames ###
  if (is.null(dfs) | is.null(benchmarks)) stop("no data for compairson provided")


  ### benchmarks is longer than 1 but shorter than dfs ###
  if (length(benchmarks)>1 & length(benchmarks)!=length(dfs)) stop("benchmarks must either be length 1 or the same length as dfs")

  ### Inputs are not a Data frame ###
  for (i in 1:length(dfs)){

    if (is.data.frame(get(dfs[i])) == FALSE) stop(paste(dfs[i],"must be a character naming a data frame",
                                                        sep = "", collapse = NULL))
    
    ### check if benchmarks have similar variables ###
    if (length(benchmarks)==length(dfs)){
        if ((any(names(get(dfs[i])) %in% names(get(benchmarks[i]))))==F) stop(dfs[i], " has no common variable with ", benchmarks[i],".")}
    if ((length(benchmarks)==length(dfs))==F){
        if ((any(names(get(dfs[i])) %in% names(get(benchmarks[1])))==F)) stop(dfs[i], " has no common variable with ",benchmarks[1],".")}
  }

  for (i in 1:length(benchmarks)){

    if (inherits(get(benchmarks[i]),"data.frame") == FALSE &
        inherits(get(benchmarks[i]),"list")==FALSE) stop(paste(benchmarks[i], " must be a data frame or a list",
                                                        sep = "", collapse = NULL))
    if (inherits(get(benchmarks[i]),"list") & 
        is.null(weight_bench)==F) stop(paste(benchmarks[i]), " if benchmark is a list of tables, weighting needs to be permored, when generating the list.")
  }

  ### Check if funct is either a function, character, vector of functions or vector of characters.
  if(length(funct)==1){
    if (is.function(funct)==F){
      if ((funct%in% c("d_mean","ad_mean","d_prop","ad_prop","prop_modecat","abs_prop_modecat",
                      "avg_abs_prop_diff","rel_mean","rel_prop","d_median","ad_median",
                      "abs_rel_mean","abs_rel_prop"))==F) {
        stop("funct must either be a function applicable as statistic in the boot package, or
             a character vector indicating one of the predefined functions.")
      }
     }
    }

    if(length(funct)>1){
      if(is.null(variables)) stop("if funct>1, funct has to be of the same length as variables.")
      if(length(funct)!= length(variables)) stop("if funct>1, funct has to be of the same length as variables.")
      for (i in 1:length(funct)) {
        if (is.function(funct)==F){
          if ((funct%in% c("d_mean","ad_mean","d_prop","ad_prop","prop_modecat","abs_prop_modecat",
                           "avg_abs_prop_diff","rel_mean","rel_prop","ad_median",
                           "abs_rel_mean","abs_rel_prop"))==F) {
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

  ### check for ci-type ###
  if((ci_type %in% c("perc","norm"))==F) stop('ci_type must either be "perc" or "norm".')

  ### Check if data frame is logical
  if (is.logical(data) == FALSE) stop("data must be of type logical")
  if (is.logical(silence) == FALSE) stop("silence must be of type logical")

  ### Check if data frame is logical
  if (is.numeric(nboots) == FALSE) stop("nboots must be of type numeric")
  if (nboots < 0 | nboots == 1) stop("nboots must be 0 (for analytic SE) or >1 for bootstrap SE")

  ### Check is summetric is right ###
  if (is.null(summetric)== FALSE) {
    if(summetric!= "rmse1" & summetric!= "rmse2" &
      summetric!= "mse1" & summetric!= "mse2" &
      summetric!="avg" & summetric!="avg2" &
      summetric!="R") stop("summetric must be either avg,avg2, rmse1, rmse2, mse1, mse2 or R")}

  ### confidence level out of bound ###
  if (conf_level>1 | conf_level<0) stop("conf_level must be <1 and >0")


  ### check for weight var ###
  if(is.null(weight)==F) if(is.null(id)) stop("if a weight var is provided for the data frame also a id is needed")
  if(is.null(weight_bench)==F) if(is.null(id_bench)) stop("if a weight var is provided for the benchmark also id_bench is needed")

  
  response_identificator<-NULL
  R_variables<-NULL 

  ##############################
  ### Get Benchmarks and DFS ###
  ##############################
  
  
  ### get benchmark if only one benchmark is provided ###
  if(length(benchmarks)==1) benchmarks<-c(rep(benchmarks,length(dfs)))
  
  ### Get Names of data frameS ###
  
  if (is.null(name_dfs)==F) names<-name_dfs else names=NULL
  name_dfs<-dfs
  if (is.null(names)==F) name_dfs[1:(length(names))] <- names
  
  
  
  
  if (is.null(name_benchmarks)==F) names<-name_benchmarks else names=NULL
  name_benchmarks<-benchmarks
  
  if (is.null(names)==F) name_benchmarks[1:(length(names))] <- names
  
  
  
  ##########################
  ### save dfs in a list ###
  ##########################
  
  df_list<-list()
  
  for (i in 1:length(dfs)){
    df_list[[i]]<-get(dfs[i])
    
  }
  
  
  ### prepare weights ###
  
  if(is.null(id)==F) if(length(id)>=1 & length(id)<length(dfs)) id<-rep(id,length(dfs))
  if(is.null(weight)==F) if(length(weight)>=1 & length(weight)<length(dfs)) weight<-rep(weight,length(dfs))
  if(is.null(strata)==F) if(length(strata)>=1 & length(strata)<length(dfs)) strata<-rep(strata,length(dfs))
  
  if(is.null(id_bench)==F) if(length(id_bench)>=1 & length(id_bench)<length(dfs)) id_bench<-rep(id_bench,length(dfs))
  if(is.null(weight_bench)==F) if(length(weight_bench)>=1 & length(weight_bench)<length(dfs)) weight_bench<-rep(weight_bench,length(dfs))
  if(is.null(strata_bench)==F) if(length(strata_bench)>=1 & length(strata_bench)<length(dfs)) strata_bench<-rep(strata_bench,length(dfs))
  
  
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
  varlist<-list()
  for (i in 1:length(dfs)){
    df_list[[i]]<- dataequalizer(target_df= bench_list[[i]] ,source_df = df_list[[i]],
                                 variables = variables, silence = silence)
    
    
    bench_list[[i]]<- dataequalizer(target_df = df_list[[i]], source_df = bench_list[[i]],
                                    variables = variables, silence = silence)
    
    varlist[[i]]<-colnames(bench_list[[i]])
    
    if(is.null(weight)==F){
      if (is.na(weight[i])==F) df_list[[i]][,weight[i]]<-get(dfs[i])[,weight[i]]
    }
    if(is.null(id)==F){
      if (is.na(id[i])==F) df_list[[i]][,id[i]]<-get(dfs[i])[,id[i]]
    }
    if(is.null(strata)==F){
      if (is.na(strata[i])==F) df_list[[i]][,strata[i]]<-get(dfs[i])[,strata[i]]
    }
    
    
    if(is.null(id_bench)==F){
      if (is.na(id_bench[i])==F) bench_list[[i]][,id_bench[i]]<-get(benchmarks[i])[,id_bench[i]]
    }
    if(is.null(weight_bench)==F){
      if (is.na(weight_bench[i])==F) bench_list[[i]][,weight_bench[i]]<-get(benchmarks[i])[,weight_bench[i]]
    }
    if(is.null(strata_bench)==F){
      if (is.na(strata_bench[i])==F) bench_list[[i]][,strata_bench[i]]<-get(benchmarks[i])[,strata_bench[i]]
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
        if (funct[i] == "d_median") func[i] <- "D_MED"
        if (funct[i] == "ad_median") func[i] <- "AD_MED"
        if (funct[i] == "ks") func[i] <- "KS"
        if (funct[i] == "prop_modecat") func[i] <- "PERC_MODECOUNT"
        if (funct[i] == "abs_prop_modecat") func[i] <- "ABS_PERC_MODECOUNT"
        if (funct[i] == "d_prop") func[i]<- "PROP_DIFF"
        if (funct[i] == "ad_prop") func[i]<- "ABS_PROP_DIFF"
        if (funct[i] == "avg_abs_prop_diff") func[i] <- "MEAN_PERC_DIST"
        if (funct[i] == "avg_abs_prop_diff") func[i] <- "Mean_ABS_PERC_DIST"
        
      }}

  #if (is.character(funct) == FALSE) {
  #  func <- deparse(substitute(funct))}

  ### if a list of variables is given, a function can be declaired fore every variable ###

  if (is.null(variables)==F) {
    if (length(func)>1) func_matrix<- as.data.frame(cbind (variables, func))
    if (length(func)==1) func_matrix<- as.data.frame(cbind (variables, rep(func, length(variables))))
  }

  #####################
  ### Function list ###
  #####################

  ### Build a list for each data frame, that declaires the function unsed for each variable)


  func_list<-list()

  if (is.null(variables)==F) {
    for (i in 1:length(dfs)) {
      func_list[[i]]<-func_matrix[,2][func_matrix[,1] %in% colnames(df_list[[i]])]
    }}

  if (is.null(variables)==T) {
    for (i in 1:length(dfs)) {
      func_list[[i]]<-rep (func, ncol(df_list[[i]]))
    }}

  ### alpha ###

  alpha<-1- conf_level

  #########################
  ### Calculate Results ###
  #########################


  for (i in 1:length(dfs)){

    if (ncol(df_list[[i]])>0) {
    if (i==1) {
    results<-subfunc_diffplotter(x = df_list[[i]], y = bench_list[[i]],
                                 samp = i, nboots = nboots, func = func_list[[i]],
                                 func_name = func_name, ci_type=ci_type, alpha=alpha, 
                                 conf_adjustment=conf_adjustment, id=id[i],
                                 weight=weight[i],strata=strata[i],id_bench=id_bench[i],
                                 weight_bench=weight_bench[i],strata_bench=strata_bench[i],
                                 variables = varlist[[i]], parallel = parallel)

    } 


    if (i!=1){
    results<- rbind(results,subfunc_diffplotter(x = df_list[[i]], y = bench_list[[i]],
                                                samp = i, nboots = nboots, func = func_list[[i]],
                                                func_name = func_name, ci_type=ci_type, alpha=alpha, 
                                                conf_adjustment=conf_adjustment,id=id[i],
                                                weight=weight[i],strata=strata[i],id_bench=id_bench[i],
                                                weight_bench=weight_bench[i],strata_bench=strata_bench[i],
                                                variables = varlist[[i]], parallel = parallel))
    }}

    if (ncol(df_list[[i]])==0) stop(paste(name_dfs[i],"does not share a common variable with the benchmark or the variables parameter"),
                                       sep =" ")
  }

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
  if (is.null(response_identificator)==F){if(is.na(response_identificator[i])==F) 
    
    R_indicator<-R_indicator_func(get(dfs[i]),response_identificator=response_identificator,
                                  variables=R_variables,
                                  weight = weight[i],id=id[i],strata=strata[i])[1]
  }
  if(is.null(response_identificator)) R_indicator<-NA
  if(is.null(response_identificator)==F) {if(is.na(response_identificator[i])==F) R_indicator<-NA}

    
    results$R_indicator[results$sample==i]<-R_indicator
    
  }

   ############################################################################
   ### add results and everything else together to create an results object ###
   ############################################################################

  results<-final_data(data = results, name_dfs=name_dfs, name_benchmarks=name_benchmarks, summetric=summetric, colors=colors,
                      shapes=shapes, legendlabels=legendlabels, legendtitle=legendtitle , label_x=label_x, label_y=label_y,
                      summet_size=summet_size, plot_title=plot_title, funct=funct,type=type)

  



  #####################
  ### Edit varnames ###
  #####################
  if (is.null(varlabels)) varlabels<-unique(results$data$varnames)
  if (length(varlabels) >= length(unique(results$data$varnames))){varlabels<-varlabels[1:length(unique(results$data$varnames))]}
  if (length(varlabels) < length(unique(results$data$varnames))) varlabels<-c(varlabels,unique(results$data$varnames)[(length(varlabels)+1):length(unique(results$data$varnames))])

  
  ### rename variables according to varlabels
  variables<-(unique(results$data$varnames))
  
  for (i in 1:length(variables)){
    results$data$varnames[results$data$varnames==variables[i]]<-varlabels[i]
  }
  
  if (isTRUE(data)) return(results)
  
  ################
  ### Plotting ###
  ################

  Plot <- ggplot2::ggplot(data = results$data, ggplot2::aes(x = results$data$t_vec, y = factor(results$data$varnames), col = factor(results$data$sample), shape = factor(results$data$sample), group = factor(results$data$sample))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1), stat = "identity", size = 3) +
    {if (isTRUE(conf_adjustment)==F) ggplot2::geom_errorbar(data = results$data, ggplot2::aes( xmin = results$data$ci_lower, xmax = results$data$ci_upper, width = 0.2), position = ggplot2::position_dodge(width = 1))} +
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


##########################################
### Pregenerated Calculation Functions ###
##########################################

ABS_REL_MEAN<-function(x,y,i, 
                       id_x=NULL,weight_x=NULL,strata_x=NULL,
                       id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL){
  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  mean_by_design<-function(variable,design){
    survey::svymean(stats::reformulate(variable),design,na.rm=T)
  }
  
  a<-sapply(variables,FUN=mean_by_design, design=x_design)
  b<-sapply(variables,FUN=mean_by_design, design=y_design)
  
  c <- abs((a - b)/(b))
  return(as.vector(c))
}

REL_MEAN<-function(x,y,i, 
                    id_x=NULL,weight_x=NULL,strata_x=NULL,
                    id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL){
  
  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  mean_by_design<-function(variable,design){
    survey::svymean(stats::reformulate(variable),design,na.rm=T)
  }
  
  a<-sapply(variables,FUN=mean_by_design, design=x_design)
  b<-sapply(variables,FUN=mean_by_design, design=y_design)
  
  c <- (a - b)/(b)
  return(as.vector(c))
}



# Absolute Difference in Median
D_MED <- function(x, y, i,
                   id_x=NULL,weight_x=NULL,strata_x=NULL,
                   id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {

  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  med_by_design<-function(variable,design){
    
    out<-survey::svyquantile(stats::reformulate(variable),design, quantiles=0.5,na.rm=T)
    
    return(as.numeric(out[[variable]][1]))
  }
  
  a<-sapply(variables,FUN=med_by_design, design=x_design)
  b<-sapply(variables,FUN=med_by_design, design=y_design)
  
  c <- a - b
  return(as.numeric(c))
}

# Absolute Difference in Median
AD_MED <- function(x, y, i,
                   id_x=NULL,weight_x=NULL,strata_x=NULL,
                   id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {
  
  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  med_by_design<-function(variable,design){
    
    out<-survey::svyquantile(stats::reformulate(variable),design, quantiles=0.5,na.rm=T)
    
    return(as.numeric(out[[variable]][1]))
  }
  
  a<-sapply(variables,FUN=med_by_design, design=x_design)
  b<-sapply(variables,FUN=med_by_design, design=y_design)
  
  c <- abs(a - b)
  return(as.numeric(c))
}



# Function for KS
KS<- function(x, i ,y){

  sub_x<-x[i,]

  ks_var<-function(x,y) {
    ks<-stats::ks.test(x,y)
    return(ks$statistic)}

  mapply(ks_var, x=sub_x, y=y)


}

### Percental Difference in Mode Categorie ###
PERC_MODECOUNT <- function(x, y, i, 
                            id_x=NULL,weight_x=NULL,strata_x=NULL,
                            id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {

  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  prop_by_design<-function(variable,design,design_bench){
    
    bench_table<-survey::svytable(stats::reformulate(variable),design_bench)
    df_table<-survey::svytable(stats::reformulate(variable),design)
    mode<-names(bench_table[which.max(bench_table)])
    
    (df_table[mode])/(sum(df_table))

  }
  
  a<-sapply(variables,FUN=prop_by_design, design=x_design, design_bench=y_design)
  b<-sapply(variables,FUN=prop_by_design, design=y_design, design_bench=y_design)
  c <- a - b
  return(c)
}

### Absolute Percental Difference in Mode Categorie ###
ABS_PERC_MODECOUNT <- function(x, y, i,
                               id_x=NULL,weight_x=NULL,strata_x=NULL,
                               id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {

  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  prop_by_design<-function(variable,design,design_bench){
    
    bench_table<-survey::svytable(stats::reformulate(variable),design_bench)
    df_table<-survey::svytable(stats::reformulate(variable),design)
    mode<-names(bench_table[which.max(bench_table)])
    
    (df_table[mode])/(sum(df_table))
    
  }
  
  a<-sapply(variables,FUN=prop_by_design, design=x_design, design_bench=y_design)
  b<-sapply(variables,FUN=prop_by_design, design=y_design, design_bench=y_design)
  c <- abs(a - b)
  return(c)
}


PROP_DIFF <- function(x, i, y,
                       id_x=NULL,weight_x=NULL,strata_x=NULL,
                       id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {
  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  mean_by_design<-function(variable,design){
    survey::svymean(stats::reformulate(variable),design,na.rm=T)
  }
  
  a<-sapply(variables,FUN=mean_by_design, design=x_design)
  b<-sapply(variables,FUN=mean_by_design, design=y_design)
  
  c <- a - b
  return(as.vector(c))
}

ABS_PROP_DIFF <- function(x, i, y,
                           id_x=NULL,weight_x=NULL,strata_x=NULL,
                           id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {
  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  mean_by_design<-function(variable,design){
    survey::svymean(stats::reformulate(variable),design,na.rm=T)
  }
  
  a<-sapply(variables,FUN=mean_by_design, design=x_design)
  b<-sapply(variables,FUN=mean_by_design, design=y_design)
  
  c <- abs(a - b)
  return(as.vector(c))
}




MEAN_PERC_DIST <- function(x, y, i, 
                            id_x=NULL,weight_x=NULL,strata_x=NULL,
                            id_bench=NULL,weight_bench=NULL,strata_bench=NULL,variables=NULL) {
  
  xi<-x[i,]
  
  x_design<-get_survey_design(xi, id=id_x,weight=weight_x,strata=strata_x)
  y_design<-get_survey_design(y, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  avg_abs_prop_diff_by_design<-function(variable,design, design_bench){
    
    a<-prop.table(survey::svytable(stats::reformulate(variable),design))
    b<-prop.table(survey::svytable(stats::reformulate(variable),design_bench))
    
    mean(abs(as.vector(a-b)))
  }
  
  
  c<-sapply(variables,FUN=avg_abs_prop_diff_by_design, design=x_design,design_bench=y_design)
  
  return(c)
}





################################
### Subfunction to bootstrap ###
################################

subfunc_diffplotter <- function(x, y, samp = 1, nboots = nboots, func = func,
                                func_name="none", ci_type="perc", alpha=0.05, 
                                conf_adjustment=NULL,id=NULL,
                                weight=NULL,strata=NULL,id_bench=NULL,
                                weight_bench=NULL,strata_bench=NULL,variables=NULL,
                                parallel = F) {


  #######################################################
  ### loop to bootstrap for every Variable in data frame ###
  #######################################################
  if (parallel==T) para<-"snow"
  if (parallel==F) para<-"multicore"
  
  boot <- boot(data = x, y = y, 
               statistic = get(func[1]), R = nboots, 
               ncpus = (parallel::detectCores()-1), parallel = para,
               id_x = id,weight_x=weight,strata_x=strata,id_bench=id_bench,
               weight_bench=weight_bench,strata_bench=strata_bench,variables=variables)

  ### Make data to a data frame ###
  #t_vec <- getoutboot(bootlist, value = "t0")
  t_vec<-as.numeric(boot$t0)
  

  #########################
  ### Bootstrap CI & SE ###
  #########################
  getCI <- function(x,w,ci_type, varnames, alpha) {
    if(length(unique(x$t[,w]))==1) return(c(0,0,unique(x$t[,w]),unique(x$t[,w])))
    suppressWarnings(b1 <- boot::boot.ci(x,type = ci_type, conf = (1-alpha),index=w))
    ## extract info for all CI types
    tab <- t(sapply(b1[-(1:3)],function(x) utils::tail(c(x),2)))
    ## combine with metadata: CI method, index
    tab <- cbind(w,rownames(tab),as.data.frame(tab))
    if (ci_type=="norm") colnames(tab) <- c("index","method","lwr","upr")
    if (ci_type=="perc") colnames(tab) <- c("index","method","lwr","upr")
    tab
  }
  
  ### function to get boot.cis ###
  if (nboots>=2) {
  

  ## do it for both parameters

  if(ci_type=="norm") cis<-do.call(rbind,lapply(1:length(variables),getCI,x=boot, ci_type="norm", alpha=alpha, varnames= variables))
  if(ci_type=="perc") cis<-do.call(rbind,lapply(1:length(variables),getCI,x=boot, ci_type="perc", alpha=alpha,varnames= variables))
  lower_ci<- cis[,(ncol(cis)-1)]
  upper_ci<- cis[,(ncol(cis))]
  
  alpha_adjusted<-alpha/length(x)

  if(ci_type=="norm") cis<-do.call(rbind,lapply(1:length(variables),getCI,x=boot, ci_type="norm", alpha=alpha_adjusted, varnames= variables))
  if(ci_type=="perc") cis<-do.call(rbind,lapply(1:length(variables),getCI,x=boot, ci_type="perc", alpha=alpha_adjusted, varnames= variables))
  lower_ci_adjusted<- cis[,(ncol(cis)-1)]
  upper_ci_adjusted<- cis[,(ncol(cis))]

  se_vect<- as.numeric(sub(".*\\s", "", utils::capture.output(boot)[14:(13+length(variables))]))

  }
  
  ############################
  ### Analytical CI and Se ###
  ############################

  if (nboots == 0){

    alpha_adjusted<-alpha/length(x)

    if (func_name=="d_mean" |
        func_name=="d_prop") {

      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=F, method="d_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=F, method="d_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=F, method="d_mean",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=F, method="d_mean", 
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=F, method="d_mean", 
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }

    if (func_name== "ad_mean" |
        func_name== "ad_prop" |
        func_name== "ad_median") {

      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=T, method="d_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=T, method="d_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=T, method="d_mean",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=T, method="d_mean",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=T, method="d_mean",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }
    
    if (func_name== "d_median") {
      
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=F, method="d_median",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=F, method="d_median",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=F, method="d_median",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=F, method="d_median",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=F, method="d_median",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }
    
    if (func_name== "ad_median") {
      
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=T, method="d_median",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=T, method="d_median",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=T, method="d_median",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=T, method="d_median",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=T, method="d_median",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }

    if (func_name=="prop_modecat"){
    lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=F, method="mode_prop",
                             variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                             weight_bench = weight_bench,strata_bench = strata_bench)
    upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=F, method="mode_prop",
                             variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                             weight_bench = weight_bench,strata_bench = strata_bench)
    se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=F, method="mode_prop",
                            variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                            weight_bench = weight_bench,strata_bench = strata_bench)
    lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                     value = "lower_ci", abs=F, method="mode_prop",
                                     variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                     weight_bench = weight_bench,strata_bench = strata_bench)
    upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                     value = "upper_ci", abs=F, method="mode_prop",
                                     variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                     weight_bench = weight_bench,strata_bench = strata_bench)
    }

    if (func_name=="abs_prop_modecat"){
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=T, method="mode_prop",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=T, method="mode_prop",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=T, method="mode_prop",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=T, method="mode_prop",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=T, method="mode_prop",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }
    
    if (func_name=="avg_abs_prop_diff"){
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=T, method="avg_abs_prop_diff",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=T, method="avg_abs_prop_diff",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=T, method="avg_abs_prop_diff",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=T, method="avg_abs_prop_diff",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=T, method="avg_abs_prop_diff",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }

    if (func_name=="rel_mean"| func_name=="rel_prop"){
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), 
                               conf_level=(1-alpha),value = "lower_ci", abs=F, method="rel_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), 
                               conf_level=(1-alpha),value = "upper_ci", abs=F, method="rel_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), 
                              conf_level=(1-alpha),value = "SE", abs=F, method="rel_mean",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), 
                                       conf_level=(1-alpha_adjusted),value = "lower_ci", abs=F, method="rel_mean",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), 
                                       conf_level=(1-alpha_adjusted),value = "upper_ci", abs=F, method="rel_mean",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }

    if (func_name=="abs_rel_mean" | func_name=="abs_rel_prop"){
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=T, method="rel_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=T, method="rel_mean",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=T, method="rel_mean",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=T, method="rel_mean",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=T, method="rel_mean",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }

    if (func_name=="ks" ){
      lower_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "lower_ci", abs=F, method="ks",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "upper_ci", abs=F, method="ks",
                               variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                               weight_bench = weight_bench,strata_bench = strata_bench)
      se_vect<- se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha),value = "SE", abs=F, method="ks",
                              variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                              weight_bench = weight_bench,strata_bench = strata_bench)
      lower_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "lower_ci", abs=F, method="ks",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
      upper_ci_adjusted<-se_mean_diff(as.data.frame(x),as.data.frame(y), conf_level=(1-alpha_adjusted),
                                       value = "upper_ci", abs=F, method="ks",
                                       variables = variables, id=id,weight=weight,strata=strata,id_bench=id_bench,
                                       weight_bench = weight_bench,strata_bench = strata_bench)
    }
  }

  ########################
  ### weitere schritte ###
  ########################
  names <- c(variables) ### to align values in plot
  data <- as.data.frame(t_vec)
  data$se_vec <- se_vect
  data$varnames <- names
  data$ci_lower<-lower_ci
  data$ci_upper<-upper_ci
  data$ci_level<- 1-alpha

  if (is.null(conf_adjustment)==F){

    data$ci_lower_adjusted<-lower_ci_adjusted
    data$ci_upper_adjusted<-upper_ci_adjusted
    data$ci_level_adjusted<- 1-alpha_adjusted
    }


  n_df_func<-function(df,variable){
    
    length(stats::na.omit(df[,variable]))
    
  }
  
  data$n_df<-as.vector(sapply(variables,n_df_func,df=x))
  data$n_bench<-as.vector(sapply(variables,n_df_func,df=y))


  if (is.null(conf_adjustment)){
    names(data) <- c("t_vec", "se_vec", "varnames","ci_lower","ci_upper","ci_level","n_df","n_bench")}

  if (is.null(conf_adjustment)==F){
    names(data) <- c("t_vec", "se_vec", "varnames","ci_lower","ci_upper","ci_level", "ci_lower_adjusted",
                     "ci_upper_adjusted","adjusted_ci_level","n_df","n_bench")}



  data$se_vec <- as.numeric(data$se_vec)

  data$sample <- samp
  return(data)
}






final_data<-function(data, name_dfs, name_benchmarks, summetric=NULL, colors=NULL,
                     shapes=NULL, legendlabels=NULL, legendtitle=NULL , label_x=NULL, label_y=NULL,
                     summet_size=NULL, plot_title=NULL,funct=NULL, type="comparison"){


  ###########################
  ### save data as a list ###
  ###########################

  data_list<-list()

  #######################
  ### get a summetric ###
  #######################

  if (is.null(summetric) == F) label_summet<-
      calculate_summetric(data=data, summetric = summetric,
                          name_dfs = name_dfs, name_benchmarks = name_benchmarks,
                          funct = funct)

  if (is.null(summetric) == T) label_summet=NULL

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

    if (is.null(def_leglabels)==F) def_leglabels<-c(def_leglabels,label)
    if (is.null(def_leglabels)==T) def_leglabels<-label


  }

  if (is.null(legendlabels) == FALSE) {
    def_leglabels[1:(length(legendlabels))] <- legendlabels
  }

  legendlabels <- def_leglabels

  legendtitle <- if (is.null(legendtitle)) legendtitle <- "Data frames" else legendtitle<-legendtitle

  ### label AXIS ###
  ### label X-Axis
  if (is.null(label_x)) (if (is.character(funct)){
    if(type=="benchmark"){
    if (funct=="d_mean") label_x <- "Bias: Difference in Mean"
    if (funct=="ad_mean") label_x <- "Bias: Absolute Difference in Mean"
    if (funct=="d_prop") label_x <- "Bias: Difference in Proportions"
    if (funct=="ad_prop") label_x <- "Bias: Absolute Difference in Proportions"
    if (funct=="prop_modecat") label_x <- "Bias: Difference in Mode Category"
    if (funct=="abs_prop_modecat") label_x <- "Bias: Absolute Difference in Mode Category"
    if (funct=="avg_abs_prop_diff") label_x <- "Bias: Average Absolute Difference in All variable Categories"
    if (funct=="rel_mean") label_x <- "Bias: Relative Difference in Mean"
    if (funct=="abs_rel_mean") label_x <- "Bias: Absolute Relative Difference in Mean"
    if (funct=="rel_prop") label_x <- "Bias: Relative Difference in Proportions"
    if (funct=="abs_rel_prop") label_x <- "Bias: Absolute Relative Difference in Proportions"
    if (funct=="ad_median") label_x <- "Bias: Absolute Relative Difference in Median"
    if (funct=="KS") label_x <- "Bias: KS-Test"
    }
    if(type=="nonresponse"){
      if (funct=="d_mean") label_x <- "Nonresponse Bias:\n Difference in Mean"
      if (funct=="ad_mean") label_x <- "Nonresponse Bias:\n Absolute Difference in Mean"
      if (funct=="d_prop") label_x <- "Nonresponse Bias:\n Difference in Proportions"
      if (funct=="ad_prop") label_x <- "Nonresponse Bias:\n Absolute Difference in Proportions"
      if (funct=="prop_modecat") label_x <- "Nonresponse Bias:\n Difference in Mode Category"
      if (funct=="abs_prop_modecat") label_x <- "Nonresponse Bias:\n Absolute Difference in Mode Category"
      if (funct=="avg_abs_prop_diff") label_x <- "Nonresponse Bias:\n Average Absolute Difference in All variable Categories"
      if (funct=="rel_mean") label_x <- "Nonresponse Bias:\n Relative Difference in Mean"
      if (funct=="abs_rel_mean") label_x <- "Nonresponse Bias:\n Absolute Relative Difference in Mean"
      if (funct=="rel_prop") label_x <- "Nonresponse Bias:\n Relative Difference in Proportions"
      if (funct=="abs_rel_prop") label_x <- "Nonresponse Bias:\n Absolute Relative Difference in Proportions"
      if (funct=="ad_median") label_x <- "Nonresponse Bias:\n Absolute Relative Difference in Median"
      if (funct=="KS") label_x <- "Nonresponse Bias:\n KS-Test"
    }
    if(type=="comparison"){
      if (funct=="d_mean") label_x <- "Difference in Mean"
      if (funct=="ad_mean") label_x <- "Absolute Difference in Mean"
      if (funct=="d_prop") label_x <- "Difference in Proportions"
      if (funct=="ad_prop") label_x <- "Absolute Difference in Proportions"
      if (funct=="prop_modecat") label_x <- "Difference in Mode Category"
      if (funct=="abs_prop_modecat") label_x <- "Absolute Difference in Mode Category"
      if (funct=="avg_abs_prop_diff") label_x <- "Average Absolute Difference in All variable Categories"
      if (funct=="rel_mean") label_x <- "Relative Difference in Mean"
      if (funct=="abs_rel_mean") label_x <- "Absolute Relative Difference in Mean"
      if (funct=="rel_prop") label_x <- "Relative Difference in Proportions"
      if (funct=="abs_rel_prop") label_x <- "Absolute Relative Difference in Proportions"
      if (funct=="ad_median") label_x <- "Absolute Relative Difference in Median"
      if (funct=="KS") label_x <- "KS-Test"
    }
    
  }  else label_x <- "Difference-Metric")

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
  data_list[[10]] <- summetric
  data_list[[11]] <- summet_size
  data_list[[12]] <- type
  data_list[[13]] <- plot_title
  data_list[[14]] <- name_dfs
  data_list[[15]] <- name_benchmarks


  names(data_list)<-c("data","label_summetric","colors","shapes","legendlabels",
                      "legendtitle","label_x","label_y","measure","summet","summet_size",
                      "comparison_type","plot_title","name_dfs","name_benchmarks")



   return(data_list)



}





se_mean_diff<-function(df1,df2, conf_level =0.95, value="lower_ci", abs=F, method="d_mean",
                        id=NULL,weight=NULL,strata=NULL,id_bench=NULL,weight_bench=NULL,
                        strata_bench=NULL, variables){

  design_df<-get_survey_design(df1, id=id,weight=weight,strata=strata)

  design_bench<-get_survey_design(df2, id=id_bench,weight=weight_bench,strata=strata_bench)
  
  
  
  
  ### ci function for single variables ###
  se_mean_diff_var<-function(variable, design_df, design_bench,
                             conf_level =0.95,value="lower_ci", abs=F, method="d_mean"){

    ### prepare relevant values for weighted and unweighted 

    n_df<- length(stats::na.omit(design_df$variables[,variable]))
    n_bench<- length(stats::na.omit(design_bench$variables[,variable]))
    
    variance_df<- survey::svyvar(stats::reformulate(variable),design_df, na.rm=T)
    variance_bench<- survey::svyvar(stats::reformulate(variable),design_bench, na.rm=T)
    
    mean_df<- survey::svymean(stats::reformulate(variable),design_df, na.rm=T)
    mean_bench<- survey::svymean(stats::reformulate(variable),design_bench, na.rm=T)
    
    table_df<- survey::svytable(stats::reformulate(variable),design_df)
    table_bench<- survey::svytable(stats::reformulate(variable),design_bench)
    mode<-names(table_bench[which.max(table_bench)])
    
    alpha<-1-conf_level


    if (method=="d_mean") {
      #SE<- sqrt(stats::var(var1)/length(var1)+stats::var(var2)/length(var2))
      SE<- sqrt(variance_df/n_df)

      if(abs==F){
        upper<- mean_df - mean_bench + stats::qnorm(1-alpha/2) * SE
        lower<- mean_df - mean_bench - stats::qnorm(1-alpha/2) * SE
      }

      if (abs==T){
        upper<- abs(mean_df - mean_bench) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(mean_df - mean_bench) - stats::qnorm(1-alpha/2) * SE
      }
    }
    
    if (method=="d_median") {
      #SE<- sqrt(stats::var(var1)/length(var1)+stats::var(var2)/length(var2))
      SE<- sqrt(variance_df/n_df)
      median_df<-as.numeric(survey::svyquantile(stats::reformulate(variable),design_df, quantiles=0.5,na.rm=T)[[variable]][1])
      median_bench<-as.numeric(survey::svyquantile(stats::reformulate(variable),design_bench, quantiles=0.5,na.rm=T)[[variable]][1])
      
      
      if(abs==F){
        upper<- median_df - median_bench + stats::qnorm(1-alpha/2) * SE
        lower<- median_df - median_bench - stats::qnorm(1-alpha/2) * SE
      }
      
      if (abs==T){
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
      
      if(abs==F){
        upper<- p1-p2 + stats::qnorm(1-alpha/2) * SE
        lower<- p1-p2 - stats::qnorm(1-alpha/2) * SE}


      if (abs==T){
        upper<- abs(p1-p2) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(p1-p2) - stats::qnorm(1-alpha/2) * SE}

    }
    
    if (method=="avg_abs_prop_diff") {
      
      a<-prop.table(survey::svytable(stats::reformulate(variable),design_df))
      b<-prop.table(survey::svytable(stats::reformulate(variable),design_bench))
      
      avg_abs_prop_diff<-mean(abs(as.vector(a-b)))
      
      
      
      #SE<- sqrt(p1*(1-p1)/n_df)
      SE<- sqrt(mean((a-avg_abs_prop_diff)^2)/n_df)
      
      
      if (abs==T){
        upper<- abs(avg_abs_prop_diff) + stats::qnorm(1-alpha/2) * SE
        lower<- abs(avg_abs_prop_diff) - stats::qnorm(1-alpha/2) * SE}
      
    }
    
    
    


    if (method=="rel_mean") {


      #SE <- sqrt((stats::var(var1)/length(var1) + stats::var(var2)/length(var2)) / (mean(var1)-mean(var2))^2)
      var_rel <- (1/(mean_bench^2))*(variance_df)
      SE<-sqrt(var_rel)/sqrt(n_df)
      rel_diff_mean<- (mean_df - mean_bench) / (mean_bench)

      if(abs==F){
        upper<- rel_diff_mean + stats::qnorm(1-alpha/2) * SE
        lower<- rel_diff_mean - stats::qnorm(1-alpha/2) * SE}


      if (abs==T){
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
  
  if (is.null(id)==F) {
    if(is.na(id)==F){
      id_new<-df[,id]
    }
    if(is.na(id)){id_new<-c(1:nrow(df))}
  }
  
  if (is.null(weight)==F) {
    if(is.na(weight)==F){
      weight_new<-df[,weight]
      df[,weight]<-NULL
    }
    if(is.na(weight)){weight_new<-rep(1,nrow(df))}
  }
  
  if (is.null(strata)==F) {
    if(is.na(strata)==F){
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










#' plot univar data
#'
#' \code{plot_uni_compare} This uses ggplot2 to generate a plot based on an object
#' generated by the \code{\link[sampcompR]{uni_compare}} function.
#'
#' @param uni_compare_objects A object generated by \code{\link[sampcompR]{uni_compare}}
#' function.
#' @param name_dfs,name_benchmarks A character string or vector of character strings containing the
#' new names of the data frames and benchmarks, also used in plot.
#' @param summetric If ,\code{"avg"}, \code{"mse1"}, \code{"rmse1"}, or \code{"R"} 
#' the respective measure is calculated for the biases of each survey. The values 
#' \code{"mse"} and \code{"rmse2"} lead to similar results as in \code{"mse1"} and \code{"rmse1"}, 
#' with slightly different visualization in the plot. If summetric = NULL, no summetric 
#' will be displayed in the Plot.
#' @param colors A vector of colors used in the plot for the
#' different comparisons.
#' @param shapes A vector of shapes applicable in [ggplot2::ggplot2()] used in the plot for the different
#' comparisons.
#' @param legendlabels A character string or vector of strings containing a label for the
#' legend.
#' @param legendtitle A character string containing the title of the Legend.
#' @param label_x,label_y A character string or vector of character strings containing a label for
#' the x-axis and y-axis.
#' @param summet_size A number to determine the size of the displayed summetric in the plot.
#' @param plot_title A character string containing the title of the plot.
#' @param conf_adjustment If conf_adjustment=T the cofidence level of the confidence interval will be
#' adjusted with a bonferoni adjustment, to account for the problem of multiple comparisons.
#' @param varlabels A character string or vector of character strings containing the new names of
#' variables, also used in plot.
#'
#' @return Plot of a \code{\link[sampcompR]{uni_compare}} object using
#' [ggplot2::ggplot2()] which shows the difference between two or more data frames.
#'
#' @examples
#' 
#' ## Get Data for comparison
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
#' sampcompR::plot_uni_compare(univar_data)
#' 
#' @export
#' 


plot_uni_compare<-function(uni_compare_objects, name_dfs=NULL, name_benchmarks=NULL, summetric=NULL, colors=NULL,
                           shapes=NULL, legendlabels=NULL, legendtitle=NULL , label_x=NULL, label_y=NULL,
                           summet_size=NULL, plot_title=NULL,conf_adjustment=F, varlabels= NULL) {
  
  
  ###########################
  ### Chose data frame names ###
  ###########################
  
  if (is.null(name_dfs) == F) {
    uni_compare_objects$name_dfs[1:(length(name_dfs))] <- name_dfs
  }
  
  ###########################
  ### Chose data frame names ###
  ###########################
  
  if (is.null(name_benchmarks) == F) {
    uni_compare_objects$name_benchmarks[1:(length(name_benchmarks))] <- name_benchmarks
  }
  
  
  #######################
  ### get a summetric ###
  #######################
  
  #if (is.null(summetric) == T) label_summet=NULL
  
  if (isFALSE(summetric)) uni_compare_objects$label_summetric = NULL
  
  if (is.null(summetric) == F) uni_compare_objects$label_summetric<-
      calculate_summetric(data=uni_compare_objects$data, summetric = summetric,
                          name_dfs = uni_compare_objects$name_dfs, 
                          name_benchmarks = uni_compare_objects$name_benchmarks,
                          funct = uni_compare_objects$measure)
  
  #####################
  ### Decide colors ###
  #####################
  
  
  
  if (is.null(colors) == F) {
    uni_compare_objects$colors[1:(length(colors))] <- colors
  }
  
  
  #####################
  ### Decide shapes ###
  #####################
  
  if (is.null(shapes) == F) {
    uni_compare_objects$shape[1:(length(shapes))] <- shapes
  }
  
  ############################
  ### label Legend & title ###
  ############################
  
  if (is.null(legendlabels) == F) {
    uni_compare_objects$legendlabels[1:(length(legendlabels))] <- legendlabels
  }
  
  
  ### Legendtitle ###
  
  if (is.null(legendtitle)==F) uni_compare_objects$legendtitle <- legendtitle
  
  ### label AXIS ###
  ### label X-Axis
  if (is.null(label_x)==F) uni_compare_objects$label_x <- label_x
  
  ### label Y-Axis
  if (is.null(label_y)==F) uni_compare_objects$label_y <- label_x
  
  ### summet_size ###
  if (is.null(summet_size)==F) uni_compare_objects$summet_size <- summet_size
  
  ### summet_size ###
  if (is.null(plot_title)==F) uni_compare_objects$plot_title <- plot_title
  
  
  # ##############################
  # ###     Label variables    ###
  # ##############################
  #
  # if (is.null(varlabels)) varlabels<- unique(uni_compare_objects$data$varnames)
  
  #####################
  ### Edit varnames ###
  #####################
  if (is.null(varlabels)) varlabels<-unique(uni_compare_objects$data$varnames)
  if (length(varlabels) >= length(unique(uni_compare_objects$data$varnames))){varlabels<-varlabels[1:length(unique(uni_compare_objects$data$varnames))]}
  if (length(varlabels) < length(unique(uni_compare_objects$data$varnames))) varlabels<-c(varlabels,unique(uni_compare_objects$data$varnames)[(length(varlabels)+1):length(unique(uni_compare_objects$data$varnames))])
  
  
  #######################
  ### add all to list ###
  #######################
  
  
  Plot <- ggplot2::ggplot(data = uni_compare_objects$data, ggplot2::aes(x = uni_compare_objects$data$t_vec, y = factor(uni_compare_objects$data$varnames), col = factor(uni_compare_objects$data$sample), shape = factor(uni_compare_objects$data$sample), group = factor(uni_compare_objects$data$sample))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 1), stat = "identity", size = 3) +
    {if (isTRUE(conf_adjustment)==F) ggplot2::geom_errorbar(data = uni_compare_objects$data, ggplot2::aes( xmin = uni_compare_objects$data$ci_lower, xmax = uni_compare_objects$data$ci_upper, width = 0.2), position = ggplot2::position_dodge(width = 1))} +
    {if (isTRUE(conf_adjustment)) ggplot2::geom_errorbar(data = uni_compare_objects$data, ggplot2::aes( xmin = uni_compare_objects$data$ci_lower_adjusted, xmax = uni_compare_objects$data$ci_upper_adjusted, width = 0.2), position = ggplot2::position_dodge(width = 1))} +
    ggplot2::scale_y_discrete(limits = rev(unique(uni_compare_objects$data$varnames)), labels= varlabels, breaks=unique(uni_compare_objects$data$varnames)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_color_manual(
      values = uni_compare_objects$colors, name = uni_compare_objects$legendtitle,
      labels = uni_compare_objects$legendlabels
    ) + ### Handle Color and Legend
    ggplot2::scale_shape_manual(
      values = uni_compare_objects$shapes,
      name = uni_compare_objects$legendtitle, labels = uni_compare_objects$legendlabels
    ) +
    ggplot2::xlab(uni_compare_objects$label_x) +
    ggplot2::ylab(uni_compare_objects$label_y)+
    ggplot2::theme(axis.text.y = ggplot2::element_text( vjust =0.33, hjust=0))
  
  if (is.null(uni_compare_objects$label_summet) == FALSE) {
    Plot <- Plot + ggplot2::geom_label(ggplot2::aes(x = Inf, y = Inf, hjust = 1, vjust = 1, label = uni_compare_objects$label_summetric),
                                       fill = ggplot2::alpha("white", 0.02), color = ggplot2::alpha("black", 0.1), size=uni_compare_objects$summet_size
    )
  }
  if (is.null(uni_compare_objects$plot_title) == FALSE) Plot <- Plot + ggplot2::ggtitle(uni_compare_objects$plot_title)
  
  
  
  return(Plot)
  
  
  
}


calculate_summetric<-function(data, summetric=NULL, funct, name_dfs,name_benchmarks){
  
  
  for (i in 1:max(data$sample)){
    
    bias<-data$t_vec[data$sample==i]
    
    data$mse[data$sample==i]<-sum(bias*bias)/length(bias)
    data$rmse[data$sample==i]<-sqrt(sum(bias*bias)/length(bias))
    data$avg[data$sample==i]<-sum(abs(bias))/length(bias)
  }
  
  
  if (is.null(summetric) == FALSE) {
    if (summetric == "rmse1") {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1){
          labelrmse <- paste("RMSE:\n ", name_dfs[i], " & ", name_benchmarks[i], ":\n", "  ",
                             round(unique(data$rmse[data$sample==i]), digits = 3), "\n",
                             sep = "", collapse = NULL)}
        if (i>1){
          labelrmse <- paste(name_dfs[i], " & ", name_benchmarks[i], ":\n", "  ",
                             round(unique(data$rmse[data$sample==i]), digits = 3), "\n",
                             sep = "", collapse = NULL)}
        
        
        if (i==1) label_summet<-labelrmse
        if (i>1) label_summet<-paste(label_summet, labelrmse)
      }}
    
    
    if (summetric == "mse1") {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelmse <- paste("MSE:\n ", name_dfs[i], " & ", name_benchmarks[i], ":\n", "  ",
                            round(unique(data$mse[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        if (i>1) {
          labelmse <- paste(name_dfs[i], " & ", name_benchmarks[i], ":\n", "  ",
                            round(unique(data$mse[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelmse
        if (i>1) label_summet<-paste(label_summet, labelmse)
      }}
    
    
    if (summetric == "rmse2") {
      
      for (i in 1:length(name_dfs)){
        
        if(i==1){
          labelrmse <- paste("RMSE:\n ", name_dfs[i], ":   ",
                             round(unique(data$rmse[data$sample==i]), digits = 3), "\n",
                             sep = "", collapse = NULL)}
        if (i>1) {
          labelrmse <- paste(name_dfs[i], ":   ",
                             round(unique(data$rmse[data$sample==i]), digits = 3), "\n",
                             sep = "", collapse = NULL)}
        
        
        if (i==1) label_summet<-labelrmse
        if (i>1) label_summet<-paste(label_summet, labelrmse)
        
      }}
    
    
    if (summetric == "mse2") {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelmse <- paste("MSE:\n ", name_dfs[i],":   ",
                            round(unique(data$mse[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i>1) {
          labelmse <- paste(name_dfs[i], ":   ",
                            round(unique(data$mse[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelmse
        if (i>1) label_summet<-paste(label_summet, labelmse)
        
      }}
    
    ### AARB Long ###
    if (summetric == "avg" & (funct=="rel_mean" | funct=="abs_rel_mean" |
                              funct=="rel_prop" | funct=="abs_rel_prop")) {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelavg <- paste("Absolute Average\n Relative Bias:\n ", name_dfs[i],":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i>1) {
          labelavg <- paste(name_dfs[i], ":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelavg
        if (i>1) label_summet<-paste(label_summet, labelavg)
        
      }}
    
    ### AAB Long ###
    if (summetric == "avg" & !(funct=="rel_mean" | funct=="abs_rel_mean" |
                               funct=="rel_prop" | funct=="abs_rel_prop")) {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelavg <- paste("Absolute Average\n Bias:\n ", name_dfs[i],":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i>1) {
          labelavg <- paste(name_dfs[i], ":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelavg
        if (i>1) label_summet<-paste(label_summet, labelavg)
        
      }}
    
    ### AARB Short ###
    if (summetric == "avg2" & (funct=="rel_mean" | funct=="abs_rel_mean" |
                               funct=="rel_prop" | funct=="abs_rel_prop")) {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelavg <- paste("AARB:\n ", name_dfs[i],":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i>1) {
          labelavg <- paste(name_dfs[i], ":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelavg
        if (i>1) label_summet<-paste(label_summet, labelavg)
        
      }}
    
    ### AAB Short ###
    if (summetric == "avg2" & !(funct=="rel_mean" | funct=="abs_rel_mean" |
                                funct=="rel_prop" | funct=="abs_rel_prop")) {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelavg <- paste("AAB:\n ", name_dfs[i],":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i>1) {
          labelavg <- paste(name_dfs[i], ":   ",
                            round(unique(data$avg[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelavg
        if (i>1) label_summet<-paste(label_summet, labelavg)
        
      }}
    
    
    
    if (summetric == "R") {
      
      for (i in 1:length(name_dfs)){
        
        if (i==1) {
          labelavg <- paste("R-Indicator:\n ", name_dfs[i],":   ",
                            round(unique(data$R_indicator[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i>1) {
          labelavg <- paste(name_dfs[i], ":   ",
                            round(unique(data$R_indicator[data$sample==i]), digits = 3), "\n",
                            sep = "", collapse = NULL)}
        
        if (i==1) label_summet<-labelavg
        if (i>1) label_summet<-paste(label_summet, labelavg)
        
      }}
    
  }
  
  
  
  return(label_summet)
}



add_together<-function(uni_compare_object1,uni_compare_object2){
  
  #########################
  ### add together data ###
  #########################
  
  
  data1<-uni_compare_object1$data
  data1$object<-1
  data2<-uni_compare_object2$data
  data2$object<-2
  
  final_diffplotter_object<-uni_compare_object1
  
  final_data<-rbind(data1,data2)
  
  
  ### add existing variables ###
  
  for (i in 1:length(unique(final_data$name_dfs))){
    
    final_data$sample[final_data$name_dfs==unique(final_data$name_dfs)[i]]<-i
  }
  
  final_diffplotter_object$data<-final_data
  
  ### add together name_dfs ###
  
  final_diffplotter_object$name_dfs<-unique(final_diffplotter_object$data$name_dfs)
  
  ### add together name_dfs ###
  
  final_diffplotter_object$name_dfs<-unique(final_diffplotter_object$data$name_benchmarks)
  
  
}






### chi-square test ###

CHISQ<-function(x=x,y=y, out="p.value") {
  
  return(stats::chisq.test(table(x),p=prop.table(table(y)),correct = F)[[out]])
}


subfunc_chisq<-function(x, y, sample=1){
  
  p_list<-mapply(FUN=CHISQ,x=as.list(x),y=as.list(y), SIMPLIFY = F)
  chi_list<-mapply(FUN=CHISQ,x=as.list(x),y=as.list(y),out="statistic", SIMPLIFY = F)
  dgf_list<-mapply(FUN=CHISQ,x=as.list(x),y=as.list(y),out="parameter", SIMPLIFY = F)
  
  test_func<-function(x,p){
    if(p<=0.05 & p>0.01) result<-paste(x,"*", sep = "")
    if(p<=0.01 & p>0.001) result<-paste(x,"**", sep = "")
    if(p<=0.001) result<-paste(x,"***", sep = "")
    if(p>0.05) result <- paste(x)
    return(result)}
  
  chi_table<-mapply(round(as.numeric(chi_list), digits = 2), FUN=test_func, p=as.numeric(p_list), SIMPLIFY=T)
  
  
  
  chi_table<- paste(chi_table, " (DF ", as.numeric(dgf_list), ")", sep="")
  chi_table<-as.data.frame(chi_table)
  
  #chi_table<-rbind(as.data.frame(chi_list),as.data.frame(dgf_list),as.data.frame(p_list))
  results<-data.frame(colnames(x),chi_table)
  colnames(results)<-c("varnames",paste("Chi2","_",sample,sep = ""))
  
  
  return(results)
}

#univar_alb_w<-univar_alb_w[,colnames(univar_micro)]
#test<-subfunc_chisq(x=univar_alb_w,y=univar_micro)



chi_square_df<- function(dfs,benchmarks, name_dfs=NULL, name_benchmarks=NULL, variables=NULL){
  
  
  ##############################
  ### Get Benchmarks and DFS ###
  ##############################
  
  
  ### get benchmark if only one benchmark is provided ###
  if(length(benchmarks)==1) benchmarks<-c(rep(benchmarks,length(dfs)))
  
  ### Get Names of data frameS ###
  
  if (is.null(name_dfs)==F) names<-name_dfs else names=NULL
  name_dfs<-dfs
  if (is.null(names)==F) name_dfs[1:(length(names))] <- names
  
  
  if (is.null(name_benchmarks)==F) names<-name_benchmarks else names=NULL
  name_benchmarks<-benchmarks
  
  if (is.null(names)==F) name_benchmarks[1:(length(names))] <- names
  
  ##########################
  ### save dfs in a list ###
  ##########################
  
  df_list<-list()
  
  for (i in 1:length(dfs)){
    df_list[[i]]<-get(dfs[i])
  }
  
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
    df_list[[i]]<- dataequalizer(target_df= bench_list[[i]] ,source_df = df_list[[i]],
                                 variables = variables)
    
    bench_list[[i]]<- dataequalizer(target_df = df_list[[i]], source_df = bench_list[[i]],
                                    variables = variables)
  }
  
  for (i in 1:length(dfs)){
    df_list[[i]]<- df_list[[i]][,colnames(bench_list[[i]])]
  }
  
  #return(df_list)
  #return(bench_list)
  
  
  #########################
  ### Calculate Results ###
  #########################
  
  
  for (i in 1:length(dfs)){
    
    if (ncol(df_list[[i]])>0) {
      if (i==1) {
        results<-subfunc_chisq(x = df_list[[i]], y = bench_list[[i]],sample = i)}
      
      
      if (i!=1){
        results<- merge(results,subfunc_chisq(x = df_list[[i]], y = bench_list[[i]],sample = i), by="varnames", all=T)
      }}
    
    if (ncol(df_list[[i]])==0) stop(paste(name_dfs[i],"does not share a common variable with the benchmark or the variables parameter"),
                                    sep =" ")
  }
  
  colnames(results)<-c("variables",dfs)
  results<-as.matrix(results)
  return(results)
  
}








#' Calculate the R-Indicator
#' 
#' Calculates the R-Indicator of the (weighted) data frame.
#'
#' @param dfs A character vector containing the names of data frames 
#' to calculate the R indicator for.
#' @param response_identificators A character vector, naming response identificators 
#' for every df. response identificators should indicate if respondents are part 
#' of the set of respondents \code{(respondents = 1)} or not part of the set of
#' respondents. 
#' \code{(non-respondents = 0)}. If only one character is provided, the same 
#' variable is used in every df.
#' @param variables A character vector with the names of variables that should be 
#' used in the model to calculate the R indicator
#' @param id A character vector determining id variables used to weight the dfs 
#' with the help of the survey package. They have to be part of the respective 
#' data frame. If only one character is provided, the same variable is used to weight 
#' every df.
#' @param weight A character vector determining variables to weight the dfs. 
#' They have to be part of the respective data frame. If only one character is 
#' provided, the same variable used to weight every df. 
#' If a weight variable is provided also an id variable is needed. 
#' For weighting, the survey package is used.
#' @param strata A character vector determining strata variables used to weight 
#' the dfs with the help of the survey package. They have to be part of the 
#' respective data frame. If only one character is provided, the same variable 
#' is used to weight every df.
#' @param get_r2 If true, Pseudo R-Squared of the propensity model will be 
#' returned, based on the method of McFadden.
#' 
#' @return A list containing the R-indicator, and its standard error for every data frame.
#' 
#' @note The calculated R-indicator is based on Shlomo et al., (2012).
#' 
#' @references 
#' * Shlomo, N., Skinner, C., & Schouten, B. (2012). Estimation of an 
#' indicator of the representativeness of survey response. Journal of Statistical 
#' Planning and Inference, 142(1), 201–211. https://doi.org/10.1016/j.jspi.2011.07.008

#' 
#' @examples
#' 
#' card<-wooldridge::card
#' 
#' # For the purpose of this example, we assume that only respondents living in 
#' # the south or only white respondents have participated in the survey.
#' 
#' sampcompR::R_indicator(dfs=c("card","card"),
#'                        response_identificators = c("south","black"),
#'                        variables = c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                        weight = c("weight","weight"))
#' 
#' @export

R_indicator<-function(dfs,response_identificators,variables,
                      id=NULL,weight=NULL,strata=NULL, get_r2=F){
  
  if(length(response_identificators)>1 & length(response_identificators)<length(dfs)){
    stop(paste("response_identificators has to be of length 1, or the same length as dfs"))
  }
  if(length(id)>1 & length(id)<length(dfs)){
    stop(paste("id has to be of length 1, or the same length as dfs"))
  }
  if(length(weight)>1 & length(weight)<length(dfs)){
    stop(paste("weight has to be of length 1, or the same length as dfs"))
  }
  if(length(strata)>1 & length(strata)<length(dfs)){
    stop(paste("strata has to be of length 1, or the same length as dfs"))
  }
  
  if(is.character(dfs)==F) stop(paste('dfs must be of class "character".'))
  if(is.character(response_identificators)==F) stop(paste('response_identificators must be of class "character".'))
  if(is.character(variables)==F) stop(paste('variables must of class "character"'))
  if(is.null(id)==F) if(is.character(id)==F) stop(paste('id must be of class "character"'))
  if(is.null(weight)==F) if(is.character(weight)==F) stop(paste('weight must of class "character"'))
  if(is.null(strata)==F) if(is.character(strata)==F) stop(paste('strata must of class "character"'))
  
  
  if(is.null(response_identificators)==F){
    if(length(response_identificators)<length(dfs)) response_identificators<-c(rep(response_identificators[1],length(dfs)))
  }
  
  if(is.null(id)==F){
    if(length(id)<length(dfs)) id<-c(rep(id[1],length(dfs)))
  }
  if(is.null(weight)==F){
    if(length(weight)<length(dfs)) weight<-c(rep(weight[1],length(dfs)))
  }
  if(is.null(strata)==F){
    if(length(strata)<length(dfs)) strata<-c(rep(strata[1],length(dfs)))
  }
  

  
  
  results<-list()
  for (i in 1:length(dfs)){
    
    results[[i]]<-R_indicator_func(df=get(dfs[i]),
                                   response_identificator=response_identificators[i],
                                   variables=variables,
                                   id=id[i],
                                   weight=weight[i],strata=strata[i], get_r2 = get_r2)
    
  }
  
  names(results)<-dfs
  results
}




R_indicator_func<-function(df,response_identificator,variables,
                           id=NULL,weight=NULL,strata=NULL, get_r2=F){
  
  
  
  ### turn df into data.frame
  df<-as.data.frame(df)
  
  ### Null if na
  if(is.null(id)==F) {if(is.na(id)) id<-NULL}
  if(is.null(weight)==F) {if(is.na(weight)) weight<-NULL}
  if(is.null(strata)==F) {if(is.na(strata)) strata<-NULL}
  
  if(is.null(id)==F & is.null(weight)==F & is.null(strata)==F) df<-stats::na.omit(df[,c(variables, weight,id,strata, response_identificator)])
  if(is.null(id)==F & is.null(weight)==F & is.null(strata)==T) df<-stats::na.omit(df[,c(variables, weight,id, response_identificator)])
  if(is.null(id)==F & is.null(weight)==T & is.null(strata)==F) df<-stats::na.omit(df[,c(variables,id,strata, response_identificator)])
  if(is.null(id)==T & is.null(weight)==F & is.null(strata)==F) df<-stats::na.omit(df[,c(variables, weight,strata, response_identificator)])
  if(is.null(id)==T & is.null(weight)==T & is.null(strata)==F) df<-stats::na.omit(df[,c(variables, strata, response_identificator)])
  if(is.null(id)==F & is.null(weight)==T & is.null(strata)==T) df<-stats::na.omit(df[,c(variables, id, response_identificator)])
  if(is.null(id)==T & is.null(weight)==F & is.null(strata)==T) df<-stats::na.omit(df[,c(variables, weight, response_identificator)])
  if(is.null(id)==T & is.null(weight)==T & is.null(strata)==T) df<-stats::na.omit(df[,c(variables, response_identificator)])
  
  
  
  ### normalize weights ###
  if(is.null(weight)==F) weights<-df[,weight]/(sum(df[,weight])/nrow(df))
  if(is.null(weight)) weights <-rep(1,nrow(df))
  
  if(is.null(id)==F) id <- df[,id]
  if(is.null(id)) id <- 1:nrow(df)
  if(is.null(strata)==F) strata <- df[,strata]
  df$insample<-df[,response_identificator]
  
  #df<-df[stats::complete.cases(df[,c(variables,"insample")]),]
 
  
  df_design <- survey::svydesign(
    data = df,
    id = id,
    weights = weights,
    strata = strata
  )
  
  formula<-stats::as.formula(paste("insample ~",paste(variables, collapse = " + ")))
  
  model<-survey::svyglm(design=df_design, formula =formula,family = stats::quasibinomial("logit"))
  
  
  #Response_propensity <- stats::predict(model,type = "response")
  Response_propensity <-model$fitted.values
  
  estimated_pop_variance<- survey::svyvar(x= Response_propensity, design=df_design)
  
  estimated_pop_std_dev<-sqrt(estimated_pop_variance)
  
  r_indicator<-1-2*estimated_pop_std_dev
  
  mcfadden <- function(model){1- (model$deviance/model$null.deviance)}
  
  output<-c(r_indicator,survey::SE(estimated_pop_std_dev))
  if(get_r2==T) output<-c(output, mcfadden(model))
  if(get_r2==F) names(output)<-c("R-Indicator","SE")
  if(get_r2==T) names(output)<-c("R-Indicator","SE", "Pseudo R2")
  output
}






R_indicator_func2<-function(df,benchmark,variables,
                            id=NULL,weight=NULL,strata=NULL,
                            id_bench=NULL,weight_bench=NULL,strata_bench=NULL){
  
  
  ### Null if na
  if(is.null(id)==F) {if(is.na(id)) id<-NULL}
  if(is.null(weight)==F) {if(is.na(weight)) weight<-NULL}
  if(is.null(strata)==F) {if(is.na(strata)) strata<-NULL}
  if(is.null(id_bench)==F) {if(is.na(id_bench)) id_bench<-NULL}
  if(is.null(weight_bench)==F) {if(is.na(weight_bench)) weight_bench<-NULL}
  if(is.null(strata_bench)==F) {if(is.na(strata_bench)) strata_bench<-NULL}
  
  df_new<- dataequalizer(target_df= benchmark ,source_df = df,
                         variables = variables, silence = TRUE)
  benchmark_new<- dataequalizer(target_df= df_new  ,source_df = benchmark,
                                variables = variables, silence = TRUE)
  
  
  
  ### normalize weights ###
  if(is.null(weight)==F) df_new$weights<-df[,weight]/(sum(df[,weight])/nrow(df))
  if(is.null(weight)) df_new$weights <-1
  
  if(is.null(id)==F) df_new$id <- df[,id]
  if(is.null(id)) df_new$id <- NA
  if(is.null(strata)==F) df_new$strata <- df[,strata]
  df_new$insample<-1
  
  ### normalize weights ###
  if(is.null(weight_bench)==F) benchmark_new$weights<-benchmark[,weight_bench]/(sum(benchmark[,weight_bench])/nrow(benchmark))
  if(is.null(weight_bench)) benchmark_new$weights <-1
  
  if(is.null(id_bench)==F) benchmark_new$id <- benchmark[,id_bench]
  if(is.null(id_bench)) benchmark_new$id <- NA
  if(is.null(strata_bench)==F) benchmark_new$strata <- benchmark[,strata_bench]
  benchmark_new$insample=0
  
  
  comp_sample<-rbind(df_new,benchmark_new)
  
  if(is.null(id) & is.null(id_bench)){
    comp_sample$id[comp_sample$insample==1]<- 1:nrow(df_new)
    comp_sample$id[comp_sample$insample==0]<- (nrow(df_new)+1):((nrow(df_new))+nrow(benchmark_new))}
  
  if(is.null(id)==F & is.null(id_bench)){
    comp_sample$id[comp_sample$insample==0]<- (max(df_new$id)+1):(max(df_new$id)+nrow(benchmark_new))}
  
  if(is.null(id) & is.null(id_bench)==F){
    comp_sample$id[comp_sample$insample==1]<-(max(benchmark_new$id)+1):(max(benchmark_new$id)+nrow(df_new))}
  
  
  comp_design <- survey::svydesign(
    data = comp_sample,
    id = comp_sample$id, weights = comp_sample$weights
  )
  
  formula<-stats::as.formula(paste("insample ~",paste(variables, collapse = " + ")))
  
  suppressWarnings(model<-survey::svyglm(design=comp_design, formula =formula,family = stats::binomial("logit")))
  
  
  Response_propensity <- stats::predict(model,type = "response")

  
  estimated_pop_std_dev<-sqrt(stats::var(Response_propensity))
  
  r_indicator <- 1 - 2*estimated_pop_std_dev
  
  r_indicator
}
