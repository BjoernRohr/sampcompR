##############################################################
###                                                        ###
### 		Subject:	Compare Samples on a bivariate level     ###
### 		Date: 		May 2023                                 ###
### 		Author: 	Bjoern Rohr                              ###
### 	Version:  	1.00                                     ###
###                                                        ###
### 		Bugfix:   	/                                      ###
###                                                        ###
##############################################################

############################################
### function for only 1 benchmark and df ###
############################################

biv_comp_subfunction<-function(df, benchmark, data = TRUE, corrtype="r",plot_title=NULL, 
                               variables=NULL,ID=NULL,ID_bench=NULL,
                               weight=NULL,weight_bench=NULL, strata=NULL,strata_bench=NULL,
                               p_value=NULL, varlabels=NULL, mar = c(0,0,0,0),
                               note=TRUE,p_adjust=NULL, full=FALSE, grid="white",diff_perc=FALSE,
                               diff_perc_size=4.5,perc_diff_transparance=0 ,gradient=FALSE, 
                               breaks=breaks, colors=NULL, remove_nas="pairwise", nboots=0,
                               parallel = FALSE,adjustment_weighting="raking", 
                               adjustment_vars=NULL,raking_targets=NULL,post_targets=NULL,
                               boot_all=FALSE,percentile_ci=TRUE,number=i) {


  ### Build title
  df1_label<-deparse(substitute(df))
  bench_label<-deparse(substitute(benchmark))
  plot_title<-ifelse(is.null(plot_title),paste("Compare ", df1_label," & ",bench_label, sep = "", collapse = NULL), plot_title)



  ############################
  ### equalize data frames ###
  ############################
  
  ###############################################
  ### When benchmark is a object of dataframe ###
  ###############################################
  if (inherits(benchmark,"data.frame")) {
  df2<-dataequalizer(benchmark,df,variables=variables,silence=TRUE)
  benchmark2<-dataequalizer(df2,benchmark,variables=variables,silence=TRUE)
  

  #### add weight variables again ifany ###
  if (is.null(weight)==FALSE) df2<-df[,c(colnames(df2),weight)]
  if (is.null(ID)==FALSE) df2<-df[,c(colnames(df2),ID)]
  if (is.null(strata)==FALSE) df2<-df[,c(colnames(df2),strata)]
  if (is.null(adjustment_vars)==FALSE)df2<-df[,c(colnames(df2),adjustment_vars)]

  if (is.null(weight_bench)==FALSE) benchmark2<-benchmark[,c(colnames(benchmark2),weight_bench)]
  if (is.null(ID_bench)==FALSE) benchmark2<-benchmark[,c(colnames(benchmark2),ID_bench)]
  if (is.null(strata_bench)==FALSE) benchmark2<-benchmark[,c(colnames(benchmark2), strata_bench)]

  
  #colnames(df)<-c(as.character(1:(ncol(df))))
  ### Build correlation matrices
  if(remove_nas=="all") df2<-stats::na.omit(df2)
  df<-df2
  
  if (is.null(weight)==FALSE & is.null(ID)==FALSE) {
    df[,weight]<-NULL
    df[,ID]<-NULL
    #ID<-df2[,ID]
    #weight<-df2[,weight]
    if (is.null(strata)==FALSE) {
      df[,strata]<-NULL
      #strata<-df2[,strata]
      }
  }

  if(remove_nas=="all") benchmark2<-stats::na.omit(benchmark2)
  benchmark<-benchmark2
  if (is.null(weight_bench)==FALSE & is.null(ID_bench)==FALSE) {
    benchmark[,weight_bench]<-NULL
    benchmark[,ID_bench]<-NULL
    #ID_bench<-benchmark2[,ID_bench]
    #weight_bench<-benchmark2[,weight_bench]
    if (is.null(strata_bench)==FALSE) {
      benchmark[,strata_bench]<-NULL
      #strata_bench<-benchmark2[,strata_bench]
      }
    }
  }
  
  ###########################################
  ### When benchmark is a object of rcorr ###
  ###########################################
  
  if(inherits(benchmark,"data.frame")==FALSE){
    
    fit_cor<-function(rcorr_object,df=NULL,variables=NULL){
      
      ### check for variables in df and variables ###
      if (is.null(variables)==FALSE) vars<-variables[variables %in% colnames(df)]
      if (is.null(variables)) vars<- colnames(df)
      
      
      
      rcorr_object[[1]]<-rcorr_object[[1]][rownames(rcorr_object[[1]]) %in% vars, colnames(rcorr_object[[1]]) %in% vars]
      rcorr_object[[2]]<-rcorr_object[[2]][rownames(rcorr_object[[2]]) %in% vars, colnames(rcorr_object[[2]]) %in% vars]
      rcorr_object[[3]]<-rcorr_object[[3]][rownames(rcorr_object[[3]]) %in% vars, colnames(rcorr_object[[3]]) %in% vars]
      
      rcorr_object
      
    }
    
    benchmark<-fit_cor(benchmark,df=df,variables = variables)
    df2<-df[,colnames(benchmark[[1]])]

    
    if (is.null(weight)==FALSE) df2<-df[,c(colnames(df2),weight)]
    if (is.null(ID)==FALSE) df2<-df[,c(colnames(df2),ID)]
    if (is.null(strata)==FALSE) df2<-df[,c(colnames(df2),strata)]
    if (is.null(adjustment_vars)==FALSE) df2<-df[,c(colnames(df2),adjustment_vars)]
    
    
    if(remove_nas=="all") df2<-stats::na.omit(df2)
    df<-df2
    if (is.null(weight)==FALSE & is.null(ID)==FALSE) {
      df[,weight]<-NULL
      df[,ID]<-NULL
      #ID<-df2[,ID]
      #weight<-df2[,weight]
      if (is.null(strata)==FALSE) {
        df[,strata]<-NULL
        #strata<-df2[,strata]
        }
    }
    
  }
  
  ###########################
  ### Type of correlation ###
  ###########################

  if(corrtype=="r") corrtype<-"pearson"
  
  if (is.null(ID)==FALSE ){
    if(is.na(ID)==TRUE) if(corrtype=="rho") corrtype<-"spearman"
    if(is.na(ID)==FALSE) if(corrtype=="rho") corrtype<-"pearson"
  }
  if (is.null(ID)==TRUE){
    if(corrtype=="rho") corrtype<-"spearman"
  }
  
  
  
  ###############################################
  ### Get correlation matrix of the Benchmark ###
  ###############################################
  
  if (inherits(benchmark,"data.frame")) {
    cor_matrix_bench <- Hmisc::rcorr(as.matrix(benchmark), type = corrtype)
    
    if (is.null(weight_bench)==FALSE & is.null(ID_bench)==FALSE) 
      cor_matrix_bench<- weighted_correlation(benchmark2,weight=weight_bench,
                                               ids=ID_bench,stratas=strata_bench,
                                               variables=colnames(benchmark),remove_nas = remove_nas ,
                                               nboots = 0, parallel = parallel,
                                               corrtype=corrtype,benchmark=TRUE)
    

    gc()  }
  
  if (inherits(benchmark,"data.frame")==FALSE) cor_matrix_bench<-benchmark
  
  
  
  ####################################
  ### Get correlation matrix of df ###
  ####################################
  
  cor_matrix_df <- Hmisc::rcorr(as.matrix(df), type = corrtype)
  if ((is.null(weight)==FALSE & is.null(ID)==FALSE) |is.null(adjustment_vars)==FALSE | nboots>0){
    
    cor_matrix_df<- weighted_correlation(df2,weight=weight,ids=ID,stratas=strata,
                                         bench=benchmark2,weight_bench=weight_bench,
                                         ids_bench=ID_bench,stratas_bench=strata_bench,
                                          variables=colnames(cor_matrix_bench[[1]]), remove_nas=remove_nas,
                                          nboots = nboots, adjustment_vars=adjustment_vars,
                                          raking_targets=raking_targets, 
                                          post_targets=post_targets,
                                          adjustment_weighting=adjustment_weighting,
                                          cor_matrix_bench=cor_matrix_bench,
                                          parallel=parallel,
                                          corrtype=corrtype,benchmark=FALSE,
                                         boot_all=boot_all,
                                         percentile_ci=percentile_ci)}
  
  
  
  #return(cor_matrix_df)###
  # if (is.null(weight)==FALSE & is.null(ID)==FALSE) cor_matrix_df$r<- weighted_cor(df2, weight , ID=ID, strata = strata, return="r")
  # if (is.null(weight)==FALSE & is.null(ID)==FALSE) cor_matrix_df[[3]]<- weighted_cor(df2, weight , ID=ID, strata = strata, return="p")
  cor_matrix_df$r[cor_matrix_df$r=="NaN"]<-NA
  cor_matrix_df$r[cor_matrix_bench$r=="NaN"]<-NA
  cor_matrix_df[[3]][cor_matrix_df[[3]]=="NaN"]<-NA


  
  fischer_cor_df<- suppressWarnings(psych::fisherz(cor_matrix_df$r))
  fischer_cor_bench<- suppressWarnings(psych::fisherz(cor_matrix_bench$r))
  
  fischer_z_test<-suppressWarnings(psych::paired.r(cor_matrix_df$r,cor_matrix_bench$r,n=cor_matrix_df$n, n2=cor_matrix_bench$n))
  fischer_z_test$p[round(cor_matrix_df$r,digits=3) %in%"-1" & round(cor_matrix_bench$r,digits=3) %in% "-1"]<-1
  fischer_z_test$p[fischer_z_test$p=="NaN"]<-NA
  if(is.null(cor_matrix_df$p_diff)==FALSE) fischer_z_test$p<-cor_matrix_df$p_diff
  p_value= ifelse(is.null(p_value),0.05,p_value)

  ### implement p_adjustments if needed ###
  
  for (i in 1:nrow(fischer_z_test$p)){
  if (is.null(p_adjust)==FALSE){ 
    if (p_adjust!=FALSE) fischer_z_test$p[i,]<- matrix(stats::p.adjust(p = fischer_z_test$p[i,], 
                                                                   method = p_adjust,
                                                                   n = ncol(fischer_z_test$p)),
                                                      ncol = ncol(fischer_z_test$p))}}

  ### Compute Comparison Matrix
  comp_matrix<-fischer_cor_df
  comp_matrix[comp_matrix=="Inf"]<-NA
  comp_matrix[fischer_z_test$p=="NaN"]<-NA
  comp_matrix[fischer_z_test$p>p_value]<-breaks[1]
  comp_matrix[fischer_z_test$p<p_value & (cor_matrix_df[[3]]>=p_value & cor_matrix_bench[[3]]>=p_value)]<-breaks[1]
  comp_matrix[fischer_z_test$p<p_value & (cor_matrix_df[[3]]<p_value | cor_matrix_bench[[3]]<p_value)]<-breaks[2]
  comp_matrix[fischer_z_test$p<p_value & (cor_matrix_df[[3]]<p_value | cor_matrix_bench[[3]]<p_value) &
                (abs(cor_matrix_df$r)>2*abs(cor_matrix_bench$r) | abs(cor_matrix_bench$r)>2*abs(cor_matrix_df$r)) &
                ((cor_matrix_df$r<0 & cor_matrix_bench$r<0) | (cor_matrix_df$r>0 & cor_matrix_bench$r>0))]<-breaks[3]
  comp_matrix[fischer_z_test$p<p_value & (cor_matrix_df[[3]]<p_value | cor_matrix_bench[[3]]<p_value) &
                ((cor_matrix_df$r>0 & cor_matrix_bench$r<0) | (cor_matrix_df$r<0 & cor_matrix_bench$r>0))]<-breaks[3] # 1 sig and one positive, while the other negative
  comp_matrix<-as.matrix(comp_matrix)
  if (isFALSE(full)) comp_matrix[upper.tri(comp_matrix)]<-NA

  if (is.null(colors)==TRUE) colors=c('green','yellow','red')

  note_text<- paste("Note: ",breaks[1]," ", colors[1],") means that the Pearson's rs are not significant different. \n" ,breaks[2]," (", colors[2], ") means, at least one is significant >0 or <0 and both are
  significant different from each other. \n",breaks[3]," (", colors[3], ") means all conditions for Small Diff are true and the
  coeficients differ in direction or one is double the value of the other. \nLevel of Significance is p < 0.05.")



  ### Calculate percentage of difference ###

  if(diff_perc==TRUE) {
    percental_difference_b1<-length(comp_matrix[comp_matrix == breaks[1] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
    percental_difference_b2<-length(comp_matrix[comp_matrix == breaks[2] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
    if (length(breaks)>2) percental_difference_b3<-length(comp_matrix[comp_matrix == breaks[3] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])
    if (length(breaks)>3) percental_difference_b4<-length(comp_matrix[comp_matrix == breaks[4] & is.na(comp_matrix)==FALSE ])/ length(comp_matrix[is.na(comp_matrix)==FALSE])

    diff_summary<-paste(breaks[1]," :",(round((percental_difference_b1), digits = 3)*100),"% \n",
                        breaks[2]," :",(round(percental_difference_b2, digits = 3)*100),"%")
    if (length(breaks)>2) diff_summary<-paste (diff_summary, "\n",breaks[3], " :", (round(percental_difference_b3, digits = 3)*100),"%")
    if (length(breaks)>3) diff_summary<-paste (diff_summary, "\n",breaks[4], " :", (round(percental_difference_b4, digits = 3)*100),"%")
  }

  ###########################
  ### Get X Instead of NA ###
  ###########################
  
  comp_matrix[lower.tri(comp_matrix) & is.na(comp_matrix)]<-"X"
  #return(comp_matrix)###
  
  ###########################
  # prepare data for ggplot
  ###########################

  comp_matrix_df<-reshape2::melt(comp_matrix)
  colnames(comp_matrix_df) <- c("x", "y", "value")

  if (grid!="white"){ # create a matrix for NA, to exclude from grid
    na_matrix<-comp_matrix_df[is.na(comp_matrix_df$value),]

    comp_matrix_2<-comp_matrix
    comp_matrix_2[is.na(comp_matrix_2)]<- 99

    for (i in 1:(nrow(comp_matrix_2)-1)){
      comp_matrix_2[i+1,i]<-NA
    }

    comp_matrix_df2<-reshape2::melt(comp_matrix_2)
    colnames(comp_matrix_df2) <- c("x", "y", "value")
    edge_matrix<- comp_matrix_df2[is.na(comp_matrix_df2$value),]
  }

  ### add r and bench_r to df
  
  comp_matrix_df$corr<-reshape2::melt(cor_matrix_df$r)$value
  comp_matrix_df$corr_bench<-reshape2::melt(cor_matrix_bench$r)$value
  
  ### add p_values and bench_p_values to df
  
  comp_matrix_df$p<-reshape2::melt(cor_matrix_df$P)$value
  comp_matrix_df$bench_p<-reshape2::melt(cor_matrix_bench$P)$value
  comp_matrix_df$p_diff<-reshape2::melt(fischer_z_test$p)$value
  
  #### add difference to data frame

  difference_r<-(cor_matrix_df$r-cor_matrix_bench$r)
  difference_r<-reshape2::melt(difference_r)
  comp_matrix_df$difference_r<-difference_r$value

  ### add absolute relative difference to data frame
  comp_matrix_df$abs_rel_difference_r<-reshape2::melt(abs((cor_matrix_df$r-cor_matrix_bench$r)/cor_matrix_bench$r))$value
  
  ### change gradient ###

  alpha_matrix<-(abs(cor_matrix_df$r-cor_matrix_bench$r)/2/5)+0.8
  alpha_matrix<-reshape2::melt(alpha_matrix)
  colnames(alpha_matrix) <- c("x", "y", "gradient")
  comp_matrix_df$gradient<-alpha_matrix$gradient
  #}

  if (gradient==FALSE) alpha_matrix$value<-1

  #if (matrix==TRUE) return(comp_matrix_df)

  ##############################
  ###     Label variables    ###
  ##############################

  if (is.null(varlabels)) varlabels<- unique(comp_matrix_df$x)


  ### build bigger out matrix ###


  cor_matrix_df[[1]][upper.tri(cor_matrix_df[[1]], diag= TRUE)]<-NA
  cor_matrix_df[[2]][upper.tri(cor_matrix_df[[2]], diag= TRUE)]<-NA
  cor_matrix_df[[3]][upper.tri(cor_matrix_df[[3]], diag = TRUE)]<-NA
  cor_matrix_bench[[1]][upper.tri(cor_matrix_bench[[1]], diag= TRUE)]<-NA
  cor_matrix_bench[[2]][upper.tri(cor_matrix_bench[[2]], diag= TRUE)]<-NA
  cor_matrix_bench[[3]][upper.tri(cor_matrix_bench[[3]], diag = TRUE)]<-NA
  diff_table<-cor_matrix_df[[1]]-cor_matrix_bench[[1]]
  fischer_z_test$p[upper.tri(fischer_z_test$p,diag=TRUE)]<-NA

  comp_matrix_list<-list(comp_matrix_df,list(cor_matrix_df[[1]],cor_matrix_df[[2]],cor_matrix_df[[3]],
                                             cor_matrix_bench[[1]],cor_matrix_bench[[2]],cor_matrix_bench[[3]],
                                             diff_table,fischer_z_test$p))

  names(comp_matrix_list)<-c("comparison_dataframe", "correlation_data")
  names(comp_matrix_list[[2]])<-c("pearsons_matrix_df","n_matrix_df","p_matrix_df",
                                  "pearsons_r_bench","n_matrix_bench","p_matrix_bench",
                                  "r_diff_matrix","r_diff_p_matrix")

  #  comp_matrix_list<- cor_matrix_df[[1]]
  if (data == TRUE) return(comp_matrix_list)

  #############################
  # Plot Matrix with ggplot2
  #############################


  comparison_plot<-
    ggplot2::ggplot(comp_matrix_df, ggplot2::aes(x = comp_matrix_df$y, y = comp_matrix_df$x, fill = factor(comp_matrix_df$value, levels = breaks))) +
    {if (gradient==TRUE) ggplot2::aes(alpha= alpha_matrix$gradient)}+
    {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1)}+
    {if (grid == "none") ggplot2::geom_tile()}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_matrix, colour = "white", lwd=1,linetype=1)}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = edge_matrix, colour = grid, lwd=1,linetype=1)}+
    ggplot2::coord_fixed()+
    ggplot2::scale_fill_manual(values= colors, name="", na.translate = FALSE)+
    ggplot2::scale_y_discrete(name="", limits = rev(levels(comp_matrix_df$x)), labels= varlabels, breaks=unique(comp_matrix_df$x))+
    ggplot2::scale_x_discrete(name="", limits = levels(comp_matrix_df$y), labels= varlabels, breaks=unique(comp_matrix_df$y))+
    #{if(gradient==TRUE) ggplot2::scale_alpha_continuous(values = alpha_matrix$values, na.translate=FALSE)}+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=0.9),
                   axis.text.y = ggplot2::element_text(vjust = 1, hjust=0.9),
                   axis.title.x= ggplot2::element_blank(),
                   axis.title.y= ggplot2::element_blank(),
                   plot.margin = grid::unit(mar, "cm"),
                   plot.caption=ggplot2::element_text(hjust = 0))+
    ggplot2::ggtitle(plot_title)+
    ggplot2::guides(alpha="none")

  if(note==TRUE) comparison_plot<-comparison_plot + ggplot2::labs(caption = note_text)


  if (diff_perc==TRUE) {
    label<-diff_summary$label
    comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes(x = rep(Inf,length(label)), 
                                                                           y = rep(Inf,length(label)), 
                                                                           hjust = rep(1,length(label)), 
                                                                           vjust = rep(1,length(label))), data = diff_summary,
                                                             label=diff_summary$label,
                                                             fill = ggplot2::alpha("white", perc_diff_transparance), 
                                                             color = ggplot2::alpha("black", 1), size= diff_perc_size)}


  #if (gradient==TRUE) return(alpha_matrix)

  if (data == FALSE) return(comparison_plot)

}







#' Compare Multiple Data Frames on a Bivariate Level
#'
#' Compare multiple data frames on a bivariate level and plot them together.
#'
#' @param dfs A character vector containing the names of data frames to compare 
#' against the \code{benchmarks}.
#' @param benchmarks A character vector containing the names of benchmarks to 
#' compare the \code{dfs} against, or the names of a list. If it is a list, it 
#' has to be of the form, as the output of \link[Hmisc]{rcorr}, with a 
#' Pearson's r matrix in the first position, a n-matrix (matrix of n for every 
#' correlation) in the second position and a p-matrix in the third position. 
#' The vector must either be the same length as \code{dfs}, or length 1. If it 
#' has length one every survey will be compared against the same benchmark. 
#' @param variables A character vector that containes the names of the variables for 
#' the comparison. If it is \code{NULL}, all variables that are named similarly 
#' in both the \code{dfs} and the benchmarks will be compared. Variables missing 
#' in one of the \code{dfs} or the \code{benchmarks} will be neglected for this 
#' comparison.
#' @param plot_title A character string containing the title of the plot.
#' @param plots_label A character string or vector of character strings 
#' containing the new names of the data frames that are used in the plot.
#' @param data If \code{TRUE}, a biv_compare object is returned, containing the results 
#' of the comparison.
#' @param id_bench,id A character vector determining id variables used to weigh
#' the \code{dfs} or \code{benchmarks} with the help of the \code{survey} package. They have
#' to be part of the respective data frame. If less characters strings are provided,
#' than in \code{dfs}, the first input is used to weigh every \code{df} or 
#' \code{benchmark}, where no input is provided.
#' @param weight_bench,weight A character vector that determines variables to weigh
#' the \code{dfs} of \code{benchmarks}. They have to be part of the respective 
#' data frame. If fewer characters strings are provided, than in \code{dfs}, 
#' the first input is used to weigh every df or benchmark, where no input is 
#' provided. If a weight variable is provided also an id variable is needed. 
#' For weighting, the \code{survey} package is used.
#' @param strata,strata_bench A character vector that determines strata variables 
#' that are used to weigh the \code{dfs} or \code{benchmarks} with the help of 
#' the \code{survey} package. It has to be part of the respective data frame. 
#' If fewer characters strings are provided, than in \code{dfs}, the first input 
#' is used to weigh every df or benchmark, where no input is provided.
#' @param p_value A number between zero and one to determine the maximum significance niveau.
#' @param varlabels A character string or vector of character strings containing 
#' the new names of variables that is used in the plot.
#' @param mar A vector that determines the margins of the plot.
#' @param note If \code{note = TRUE}, a note will be displayed to describe the plot.
#' @param p_adjust Can be either \code{TRUE} or a character string indicating an 
#' adjustment method. If \code{p_adjust = TRUE} the p_values will be adjusted with the 
#' Bonferroni adjustment method, by default, to account for the problem of 
#' multiple comparisons. All adjustment methods available in 
#' \code{\link{p.adjust}} can be used here, with the same character strings.
#' @param grid A color string, that determines the color of the lines between 
#' the tiles of the heatmap.
#' @param diff_perc If \code{TRUE} a percental difference between surveys and benchmarks is
#' displayed in the plot.
#' @param diff_perc_size A number to determine the size of the displayed percental
#' difference between surveys in the plot.
#' @param perc_diff_transparance A number to determine the transparency of the displayed
#' percental difference between surveys in the plot.
#' @param gradient If \code{gradient = TRUE}, colors in the heatmap will be more 
#' or less transparent, depending on the difference in Pearson's r of the data 
#' frames of comparison.
#' @param sum_weights A vector containing information for every variable to 
#' weigh them in the displayed percental-difference calculation. It can be used 
#' if some variables are over- or underrepresented in the analysis.
#' @param order A character vector to determine in which order the variables should be
#' displayed in the plot.
#' @param breaks A vector to label the color scheme in the legend.
#' @param colors A vector to determine the colors in the plot.
#' @param corrtype A character string, indicating the type of the bivariate correlation. 
#' It can either be "r" for Pearson's r or "rho" for Spearman's "rho". At the moment,
#' rho is only applicable to unweighted data.
#' @param missings_x If \code{TRUE}, missing pairs in the plot will be marked with an X.
#' @param remove_nas A character string, that indicates how missing values should be 
#' removed, can either be \code{"all"}, to remove all cases that contain NA in any 
#' of the variables, or \code{"pairwise"}, to remove NAs separately for every variable 
#' pair when calculating Pearson's r.
#' @param ncol_facet The number of columns used in faced_wrap() for the plots.
#' @param nboots A numeric value indicating the number of bootstrap replications. 
#' If \code{nboots = 0} no bootstrapping will be performed. Else \code{nboots} 
#' must be >2. Note, that bootstrapping can be very computationally heavy and can 
#' therefore take a while.
#' @param parallel Can be either \code{FALSE} or a number of cores that should 
#' be used in the function. If it is \code{FALSE}, only one core will be used and 
#' otherwise the given number of cores will be used.
#' @param adjustment_vars Variables used to adjust the survey when using raking 
#' or post-stratification.
#' @param raking_targets A list of raking targets that can be given to the rake 
#' function of \code{\link[survey]{rake}}, to rake the \code{dfs}.
#' @param post_targets A list of post_stratification targets that can be given to 
#' the \code{\link[survey]{postStratify}} function, to post-stratify the \code{dfs}.
#' @param adjustment_weighting A character vector indicating if adjustment 
#' weighting should be used. It can either be \code{"raking"} or \code{"post_start"}.
#' @param boot_all If TURE, both, dfs and benchmarks will be bootstrapped. Otherwise 
#' the benchmark estimate is assumed to be constant.
#' @param percentile_ci If TURE, cofidence intervals will be calculated using the percentile method.
#' If False, they will be calculated using the normal method.
#'
#' @return A object generated with the help of [ggplot2::ggplot2()] visualizes
#' the differences between the data frames and benchmarks. If \code{data = TRUE} 
#' instead of the plot a list will be returned containing information of the 
#' analyses. This \code{biv_compare} object can be used in 
#' \code{plot_biv_compare} to build a plot, or in \code{biv_compare_table}, 
#' to get a table.
#'
#' @details
#' The plot shows a heatmap of a correlation matrix, where the colors are determined by
#' the similarity of the Pearson's r values in both sets of respondents. Leaving 
#' default breaks and colors,
#' * \code{Same} (green) indicates, that the Pearson's r correlation is not significant > 0 in
#' the related data frame or benchmark or the Pearson's r correlations are not significantly
#' different, between data frame and benchmark.
#' * \code{Small Diff} (yellow) indicates that the Pearson's r
#' correlation is significant > 0 in the related data frame or benchmark and the Pearson's r
#' correlations are significantly different, between data frame and benchmark.
#' * \code{Large Diff} (red) indicates, that the same conditions of yellow are fulfilled, and
#' the correlations are either in opposite directions,or one is double the size of the other.
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
#' \donttest{bivar_comp<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("card","card"),
#'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                    data=FALSE)
#' bivar_comp
#' }
#' 
#' @export

biv_compare<-function (dfs, benchmarks, variables=NULL, corrtype="r", data = TRUE,
                       id=NULL,weight=NULL, strata=NULL,  id_bench=NULL,
                       weight_bench=NULL, strata_bench=NULL, p_value=NULL,
                       p_adjust=NULL, varlabels=NULL, plot_title=NULL, plots_label=NULL,
                       diff_perc=TRUE, diff_perc_size=4.5, perc_diff_transparance=0,  
                       note=FALSE, order=NULL,  breaks=NULL, colors=NULL,  
                       mar = c(0,0,0,0), grid="white", gradient=FALSE,sum_weights= NULL ,missings_x=TRUE, 
                       remove_nas="pairwise", ncol_facet=3, nboots=0, boot_all=FALSE,
                       parallel = FALSE, adjustment_weighting="raking",
                       adjustment_vars=NULL,raking_targets=NULL,
                       post_targets=NULL,percentile_ci=TRUE){





  if(is.null(colors)==TRUE) colors=c('green','yellow','red')
  if (is.null(breaks)) breaks<-c("Same","Small Diff", "Large Diff")

  plot_list<-NULL
  summary_df<-data.frame("samp"=NA,"label"=NA)

  if (is.null(plots_label)) plots_label<-dfs[1:length(dfs)]
  if (is.null(plots_label)==FALSE) {
    if (length(plots_label)<length(dfs)) plots_label[(length(plots_label)+1):length(dfs)]<-dfs[(length(plots_label)+1):length(dfs)]
    if (length(plots_label)>length(dfs)) plots_label<-plots_label[[1:length(dfs)]]
  }
  
  ### prepare some inputs ###
  if (length(benchmarks)>=1 & length(benchmarks) < length(dfs)) benchmarks<-c(benchmarks,rep(benchmarks[1],length(dfs)-length(benchmarks)))
  
  if (length(id)>=1 & length(id) < length(dfs)) id<- c(id,rep(id[1],(length(dfs)-length(id))))
  if (length(weight)>=1 & length(weight) < length(dfs)) weight<- c(weight,rep(weight[1],(length(dfs)-length(weight))))
  if (length(strata)>=1 & length(strata) < length(dfs)) strata<- c(strata,rep(strata[1],(length(dfs)-length(strata))))
  if (length(id_bench)>=1 & length(id_bench) < length(dfs)) id_bench<- c(id_bench,rep(id_bench[1],(length(dfs)-length(id_bench))))
  if (length(weight_bench)>=1 & length(weight_bench) < length(dfs)) weight_bench<- c(weight_bench,rep(weight_bench[1],(length(dfs)-length(weight_bench))))
  if (length(strata_bench)>=1 & length(strata_bench) < length(dfs)) strata_bench<- c(strata_bench,rep(strata_bench[1],(length(dfs)-length(strata_bench))))
  
  ### if p_adjust==TRUE get bonferroni as default ###
  if(is.null(p_adjust)==FALSE) if(isTRUE(p_adjust)) if(is.character(p_adjust)==FALSE) p_adjust<-"bonferroni"
  

  ### Get Dataframes ###
  help<-list()
  
  for (i in 1:length(dfs)){

    curr_df<-get(dfs[i])
    if(is.function(curr_df)) stop(paste("dfs must not be named the same as a existing function"))
    
    curr_bench<-get(benchmarks[i])
    if(is.function(curr_bench)) stop(paste("benchmarks must not be named the same as a existing function"))
    

    if (is.null(weight)==FALSE) {
    if (is.na(weight[i])==FALSE) {
      curr_id <- id[i]
      curr_weight<- weight[i]}
      if (is.null(strata)==FALSE) {if (is.na(strata[i])==FALSE) curr_strata<- strata[i]}}
    

    if (is.null(weight_bench)==FALSE){
      if (is.na(weight_bench[i])==FALSE) {
        curr_id_bench <- id_bench[i]
        curr_weight_bench<- weight_bench[i]}
      if (is.null(strata_bench)==FALSE) {if (is.na(strata_bench[i])==FALSE) curr_strata_bench<- strata_bench[i]}}




    if (is.null(weight)==FALSE) {
      if (is.na(weight[i])) {
        curr_id <- NULL
        curr_weight<- NULL
        if (is.null(strata)==FALSE) {if(is.na(strata[i])) curr_strata<-NULL}}}

    if (is.null(weight)) {
      curr_id <- NULL
      curr_weight<- NULL}
    if (is.null(strata)) curr_strata<-NULL

    if (is.null(weight_bench)==FALSE){
      if (is.na(weight_bench[i])) {
        curr_id_bench <- NULL
        curr_weight_bench<- NULL
        if (is.null(strata_bench)==FALSE) {if(is.na(strata_bench[i])) curr_strata_bench<-NULL}}}

    if (is.null(weight_bench)){
        curr_id_bench <- NULL
        curr_weight_bench<- NULL}
    if (is.null(strata_bench)) curr_strata_bench<-NULL


    help[[i]]<-biv_comp_subfunction(df=curr_df,benchmark = curr_bench, corrtype=corrtype, variables = variables ,
                               plot_title=plot_title, ID_bench=curr_id_bench,
                               weight_bench= curr_weight_bench, ID=curr_id,weight=curr_weight,
                               strata = curr_strata, strata_bench = curr_strata_bench,
                               p_value=p_value, varlabels=varlabels, note=note,p_adjust=p_adjust, gradient=TRUE,
                               data = TRUE, breaks=breaks,remove_nas=remove_nas, nboots = nboots,
                               parallel = parallel, adjustment_weighting=adjustment_weighting,
                               adjustment_vars=adjustment_vars[[i]],
                               raking_targets=raking_targets[[i]],
                               post_targets=post_targets[[i]],boot_all=boot_all,
                               percentile_ci=percentile_ci,number=i)
    message(paste("survey",i,"of",length(dfs),"is compared"))
    
    #return(help[[i]])###
    help[[i]][[1]]$samp<-i
    if (is.null(plots_label)) help[[i]][[1]]$samp_name<-dfs[i]
    if (is.null(plots_label)==FALSE) help[[i]][[1]]$samp_name<-plots_label[i]

    
    if (is.null(plot_list)==FALSE) {
      plot_list[[1]]<-rbind(plot_list[[1]],help[[i]][[1]])
      plot_list[1+i]<-help[[i]][2]
    }

    
    if(is.null(plot_list)) plot_list <- help[[i]]
    


  }

  names(plot_list)<-c(names(plot_list[1]),paste("correlation_data_",plots_label[1:length(dfs)],sep=""))

   



  #######################################
  ### reorder plots to original order ###
  #######################################

 # if (is.null(plots_label)) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = dfs)
 # if (is.null(plots_label)==FALSE) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = plots_label)
   if (is.null(plots_label)) plots_label <- dfs
   if(length(plots_label)< length(dfs)) plots_label[(length(plots_label)+1):length(dfs)]<-
    dfs[(length(plots_label)+1):length(dfs)]


  
  labellist_biv<-function(lables,values){
    
    # output<-list()
    # for (i in 1:length(lables)){
    #   output[i]<-lables[i]
    # }
    output<-lables
    names(output)<-as.character(values)
    output
  }
  
  
  labellist<-labellist_biv(plots_label,c(1:length(plots_label)))
  
  
  ##########################
  ### add X for missings ###
  ##########################
  
  plot_list[[1]]<-empty_finder(plot_list[[1]],plots_label)
  
  ################################
  ### change color of the grid ###
  ################################
  
  if (grid!="white"){ # create a matrix for NA, to exclude from grid
    
    ### buid a df where no grid shall be set ###
    na_df<-plot_list[[1]][is.na(plot_list[[1]]$value),]
    
    ### build a df, where the diagonal is.
    plot_df2<-plot_list[[1]]
    names_var<-as.character(unique(plot_df2$x))
    names_var<-c(names_var,names_var[1])
    plot_df2$value[is.na(plot_df2$value)]<- "not_edge"
    
    
    
    for (i in 1:length(names_var)){
      #if (is.null(k)==FALSE) k=k+1
      #if (is.null(k)) k<-1
      plot_df2$value[plot_df2$x==names_var[i+1] & plot_df2$y==names_var[i]]<-NA
    }
    
    edge_df<- plot_df2[is.na(plot_df2$value),]
  }
  
  
  ##########################################
  ### Calculate percentage of difference ###
  ##########################################
  
  if(diff_perc==TRUE) {
    summary_df<-difference_summary(plot_list[[1]],breaks=breaks, sum_weights=sum_weights)
  }
  
  
  #####################################
  ### edit shape, breaks and colors ###
  #####################################
  
  breaks2<-c(breaks,"X")
  colors2<-c(colors, "white")
  
  plot_list[[1]]$shape<-NA
  plot_list[[1]]$shape[plot_list[[1]]$value=="X"]<-"X"
  

  ##############################
  ###    order variables     ###
  ##############################
  if (is.null(order) & is.null(variables)==FALSE) order<-variables[variables %in% unique(plot_list[[1]]$x)]
  if (is.null(order)==FALSE) plot_list[[1]]$x<-factor(plot_list[[1]]$x, levels =order)
  if (is.null(order)==FALSE) plot_list[[1]]$y<-factor(plot_list[[1]]$y, levels = order)

  ##############################
  ###     Label variables    ###
  ##############################
  
  variables_in<-unique(plot_list[[1]]$x)
  if (is.null(varlabels)) varlabels<- unique(plot_list[[1]]$x)
  if (length(varlabels)<length(variables_in)) varlabels<-c(varlabels,variables_in[(length(varlabels)+1):length(variables_in)])
  if(length(varlabels)==length(variables)) varlabels<-varlabels[variables %in% unique(plot_list[[1]]$x)]
  
  #return(list(variables,varlabels,unique(plot_title[[1]]$x)))
  plot_list[[1]]<-plot_list[[1]] %>%
    dplyr::mutate(x=forcats::fct_recode(plot_list[[1]]$x, !!! stats::setNames(order, varlabels)),
           y=forcats::fct_recode(plot_list[[1]]$y, !!! stats::setNames(order, varlabels)))
  
  
  
  ####################
  ### Build a Note ###
  ####################
  
  note_text<- paste("Note: ",breaks[1]," (", colors[1],") means that the Pearson's rs are not significant different. \n" ,breaks[2]," (", colors[2], ") means, at least one is significant >0 or <0 and both are
  significant different from each other. \n",breaks[3]," (", colors[3], ") means all conditions for Small Diff are true and the
  coeficients differ in direction or one is double the value of the other. \nLevel of Significance is p < 0.05.")
  
  ####################################
  ### Add information to plot_list ###
  ####################################
  
  plot_list$dfs<-dfs
  plot_list$benchmarks<-benchmarks
  plot_list$variables <-variables 
  plot_list$colors<-colors2
  plot_list$breaks <-breaks 
  plot_list$shape<-plot_list[[1]]$shape
  #plot_list$plots_label<-as.character(unique(plot_list[[1]]$samp_name))
  plot_list$plots_label<-plots_label
  
  if (is.null(plot_title)==FALSE) plot_list$plot_title<-plot_title
  if (is.null(plot_title)) plot_list$plot_title<-NA
  
  if (is.null(varlabels)==FALSE) plot_list$varlabels<-varlabels
  if (is.null(varlabels)) plot_list$varlabels<-NA
  
  if (is.null(p_value)==FALSE) plot_list$p_value <-p_value 
  if (is.null(p_value)) plot_list$p_value <-NA  
  
  if (is.null(id)==FALSE) plot_list$id  <-id  
  if (is.null(id)) plot_list$id  <-NA   
  
  if (is.null(weight)==FALSE) plot_list$weight  <-weight  
  if (is.null(weight)) plot_list$weight  <-NA   
  
  if (is.null(strata)==FALSE) plot_list$strata   <-strata    
  if (is.null(strata)) plot_list$strata   <-NA    
  
  if (is.null(id_bench)==FALSE) plot_list$id_bench  <-id_bench  
  if (is.null(id_bench)) plot_list$id_bench  <-NA   
  
  if (is.null(weight_bench)==FALSE) plot_list$weight_bench  <-weight_bench   
  if (is.null(weight_bench)) plot_list$weight_bench  <-NA   
  
  if (is.null(strata_bench)==FALSE) plot_list$strata_bench <-strata_bench  
  if (is.null(strata_bench)) plot_list$strata_bench <-NA  
  
  if (is.null(p_adjust )==FALSE) plot_list$p_adjust  <-p_adjust   
  if (is.null(p_adjust )) plot_list$p_adjust  <-NA
  
  plot_list$note_text  <-note_text
  
  
  for (i in 1:length(plot_list)){
    plot_list[i][is.na(plot_list[i])]<-list(NULL)}
  
  ###########################
  ### Return if data=TRUE ###
  ###########################
  
  if (data == TRUE) return(plot_list)
  
  ######################
  ###     Plots      ###
  ######################


  comparison_plot<-
    ggplot2::ggplot(plot_list[[1]], ggplot2::aes(x = plot_list[[1]]$y, y = plot_list[[1]]$x)) +
    {if (gradient==TRUE) ggplot2::aes(alpha= gradient)}+
    {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1,
                                            ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid == "none") ggplot2::geom_tile(ggplot2::aes( fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_df, colour = "white", lwd=1,linetype=1,
                                                              ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = edge_df, colour = grid, lwd=1,linetype=1,
                                                              ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    #ggplot2::geom_point(data= subset(plot_list[[1]], value=="X"), ggplot2::aes(x = y, y = x), show.legend = TRUE)+
    {if (missings_x==TRUE) 
      ggplot2::geom_point(show.legend = FALSE, na.rm = TRUE, ggplot2::aes(shape= factor(plot_list[[1]]$shape, levels="X", labels=c("Missing"))))}+
    ggplot2::coord_fixed()+
    ggplot2::scale_fill_manual(values= colors, name="", na.translate = FALSE)+
    ggplot2::scale_y_discrete(name="", limits = rev(levels(plot_list[[1]]$x)), labels= varlabels, breaks=unique(plot_list[[1]]$x))+
    ggplot2::scale_x_discrete(name="", limits = levels(plot_list[[1]]$y), labels= varlabels, breaks=unique(plot_list[[1]]$y))+
    ggplot2::scale_shape_manual(name="", values = c("Missing"=4))+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.33, hjust=0),
                   axis.text.y = ggplot2::element_text(vjust = 0.33, hjust=0),
                   axis.title.x= ggplot2::element_blank(),
                   axis.title.y= ggplot2::element_blank(),
                   plot.margin = grid::unit(mar, "cm"),
                   plot.caption=ggplot2::element_text(hjust = 0))+
    ggplot2::ggtitle(plot_title)+
    ggplot2::guides(alpha="none",
                    fill  = ggplot2::guide_legend(order = 1),
                    shape = ggplot2::guide_legend(order = 2))+
    ggplot2::facet_wrap(~ factor(samp,levels=unique(samp),labels = labellist), labeller = ggplot2::labeller(samp = labellist),ncol = ncol_facet)

  if(note==TRUE) comparison_plot<-comparison_plot + ggplot2::labs(caption = plot_list[[1]]$note_text)


  if (diff_perc==TRUE) {
    comparison_plot <- comparison_plot + ggplot2::geom_label(x=Inf, y=Inf,
                                                             ggplot2::aes( hjust = 1, vjust = 1), data=summary_df, 
                                                             label = summary_df$label,
                                                             fill = ggplot2::alpha("white", perc_diff_transparance),
                                                             color = ggplot2::alpha("black", 1), size= diff_perc_size, show.legend = FALSE)}




  if (data == FALSE) return (comparison_plot)

}





# #' Plot Comparison of Multiple Data Frames on a Bivariate Level
# #'
# #' Plot a object generated by \link{biv_compare} function.
# #' @param biv_data_object A object generated by \link{biv_compare} function.
# #' @param plot_title A character string containing the title of the plot.
# #' @param plots_label A character string or vector of character strings containing the
# #' new names of the data frames , also used in plot.
# #' @param p_value A number between 0 and one to determine the maximum significance niveau.
# #' @param varlabels A character string or vector of character strings containing the new
# #' names of variables, also used in plot.
# #' @param mar A vector that determines the margins of the plot.
# #' @param note If note = True, a note will be displayed to describe the Plot.
# #' @param p_adjust Can be either TRUE or a character string indicating a adjustment method.
# #' If p_adjust=TRUE the p_values will be adjusted with the Bonferroni adjustment method, by default,
# #' to account for the problem of multiple comparisons. All adjustment methods available
# #' in \code{\link{p.adjust}} can be used here, with the same character strings.
# #' @param grid Grid determines the color of the lines between the tiles of the heatmap.
# #' @param diff_perc If \code{TRUE} a percental measure of difference between dfs and benchmarks is
# #' displayed in the plot.
# #' @param diff_perc_size A number to determine the size of the displayed percental
# #' difference between surveys in the plot.
# #' @param perc_diff_transparance A number to determine the transparancy of the displayed
# #' percental difference between surveys in the plot.
# #' @param gradient If gradient = TRUE, colors in the heatmap will be more or less transparent,
# #' depending on the difference in Pearson's r of the data frames of comparison.
# #' @param sum_weights A vector containing information for every variable to weight them in
# #' the displayed percental difference calculation. It can be used if some variables are
# #' over- or underrepresented in the analysis.
# #' @param legend_show_x If true the X will be shown in the legend. At the moment, das
# #' does not yet work correctly.
# #' @param order A character vector to determine in which order the variables should be
# #' displayed in the plot.
# #' @param breaks A vector to label the color sheme in the in the legend.
# #' @param colors A vector to determine the colors in the plot.
# #' @param missings_x If TRUE, missing pairs in the plot will be marked with an X.
# #' @param ncol_facet Number of colomns used in faced_wrap() for the plots.
# #'
# #' @return A object generated with the help of [ggplot2::ggplot2()], used to visualize
# #' the differences between the data frames and benchmarks.
# #' @details The plot shows a heatmap of a correlation matrix, where the colors are determined by
# #' the similarity of the Pearson's r value in both sets of respondents. Leaving 
# #' default breaks and colors,
# #' * \code{Same} (green) indicates, that the Pearson's r correlation is not significant > 0 in
# #' the related data frame or benchmark or the Pearson's r correlations are not significant
# #' different, between data frame and benchmark.
# #' * \code{Small Diff} (yellow) indicates that the Pearson's r
# #' correlation is significant > 0 in the related data frame or benchmark and the Pearson's r
# #' correlations are significant different, between data frame and benchmark.
# #' * \code{Large Diff} (red) indicates, that the same coditions of yellow are fulfilled, and
# #' the correlations are either in opposite directions,or one is double the size of the other.
# #'
# #' @examples
# #' 
# #' ## Get Data for comparison
# #' 
# #' data("card")
# #' 
# #' south <- card[card$south==1,]
# #' north <- card[card$south==0,]
# #' black <- card[card$black==1,]
# #' white <- card[card$black==0,]
# #' 
# #' ## use the function to plot the data 
# #' \donttest{bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
# #'                                    benchmarks = c("south","black"),
# #'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
# #'                                    data=TRUE)
# #'                         
# #' sampcompR::plot_biv_compare(bivar_data)}
# #'
# #' @export
# 
# 
# plot_biv_compare<-function (biv_data_object, plot_title=NULL, plots_label=NULL,
#                             p_value=NULL, varlabels=NULL,
#                             mar = c(0,0,0,0),note=FALSE, grid="white",diff_perc=TRUE,
#                             diff_perc_size=4.5,perc_diff_transparance=0,gradient=FALSE,sum_weights= NULL,
#                             missings_x = TRUE, legend_show_x=FALSE, order=NULL, breaks=NULL,colors=NULL,
#                             ncol_facet = 3){
# 
# 
# 
#   plot_list<- biv_data_object
#   if(is.null(colors)==TRUE) colors=plot_list$colors
#   if (is.null(breaks)) breaks<-plot_list$breaks
#
# 
#   ##########################################
#   ### Calculate percentage of difference ###
#   ##########################################
# 
#   if(diff_perc==TRUE) {
# 
#     summary_df<-difference_summary(plot_list[[1]],breaks=breaks, sum_weights=sum_weights)
#   }
# 
#   ################################
#   ### change color of the grid ###
#   ################################
# 
#   if (grid!="white"){ # create a matrix for NA, to exclude from grid
# 
#     ### buid a df where no grid shall be set ###
#     na_df<-plot_list[[1]][is.na(plot_list[[1]]$value),]
# 
#     ### build a df, where the diagonal is.
#     plot_df2<-plot_list[[1]]
#     names_var<-as.character(unique(plot_df2$x))
#     names_var<-c(names_var,names_var[1])
#     plot_df2$value[is.na(plot_df2$value)]<- "not_edge"
# 
# 
# 
#     for (i in 1:length(names_var)){
#       plot_df2$value[plot_df2$x==names_var[i+1] & plot_df2$y==names_var[i]]<-NA
#     }
# 
#     edge_df<- plot_df2[is.na(plot_df2$value),]
#   }
# 
# 
# 
#   #######################################
#   ### reorder plots to original order ###
#   #######################################
# 
#   #if (is.null(plots_label)) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = unique(plot_list[[1]]$samp_name))
#   #if (is.null(plots_label)==FALSE) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = plots_label)
#   if (is.null(plots_label)) plots_label <- plot_list$plots_label
#   if(length(plots_label)< length(plot_list$plots_label)) plots_label[(length(plots_label)+1):length(plot_list$plots_label)]<-
#       plot_list$plots_label[(length(plots_label)+1):length(plot_list$plots_label)]
#   
# 
#   breaks2<-c(breaks,"X")
#   colors2<-c(colors, "white")
# 
#   plot_list[[1]]$shape<-NA
#   plot_list[[1]]$shape[plot_list[[1]]$value=="X"]<-"X"
#   
#   
#   labellist_biv<-function(lables,values){
#     output<-lables
#     names(output)<-as.character(values)
#     output
#   }
#   
#   
#   labellist<-labellist_biv(plots_label,c(1:length(plots_label)))
#   
# 
#   ##############################
#   ###    order variables     ###
#   ##############################
#   if (is.null(order)==FALSE) plot_list[[1]]$x<-factor(plot_list[[1]]$x, levels =order)
#   if (is.null(order)==FALSE) plot_list[[1]]$y<-factor(plot_list[[1]]$y, levels = order)
# 
#   ##############################
#   ###     Label variables    ###
#   ##############################
# 
#   variables_in<-unique(plot_list[[1]]$x)
#   if (is.null(varlabels)) varlabels<- unique(plot_list[[1]]$x)
#   if (length(varlabels)<length(variables_in)) varlables<-c(varlabels,variables_in[(length(varlabels)+1):length(variables_in)])
#   
#   # ########################
#   # ### edit plots_label ###
#   # ########################
#   # 
#   # if (is.null(plots_label)) plots_label<-"dfs"
#   # if (length(plots_label)<length(unique(plot_list[[1]]$samp_name))) plots_label<-c(plots_label,unique(plot_list[[1]]$samp_name)[(length(plots_label)+1:length(unique(plot_list[[1]]$samp_name)))])
#   # 
# 
#   ######################
#   ###     Plots      ###
#   ######################
# 
#   comparison_plot<-
#     ggplot2::ggplot(plot_list[[1]], ggplot2::aes(x = plot_list[[1]]$y, y = plot_list[[1]]$x)) +
#     {if (gradient==TRUE) ggplot2::aes(alpha= gradient)}+
#     {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1,
#                                             ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
#     {if (grid == "none") ggplot2::geom_tile(ggplot2::aes( fill = factor(plot_list[[1]]$value, levels = breaks)))}+
#     {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_df, colour = "white", lwd=1,linetype=1,
#                                                               ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
#     {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = edge_df, colour = grid, lwd=1,linetype=1,
#                                                               ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
#     #ggplot2::geom_point(data= subset(plot_list[[1]], value=="X"), ggplot2::aes(x = y, y = x), show.legend = TRUE)+
#     {if(missings_x==TRUE) 
#       ggplot2::geom_point(show.legend = legend_show_x, na.rm = TRUE, ggplot2::aes(shape= factor(plot_list$shape, levels="X", labels=c("Missing"))))}+
#     ggplot2::coord_fixed()+
#     ggplot2::scale_fill_manual(values= colors, name="", na.translate = FALSE)+
#     ggplot2::scale_y_discrete(name="", limits = rev(levels(plot_list[[1]]$x)), labels= varlabels, breaks=unique(plot_list[[1]]$x))+
#     ggplot2::scale_x_discrete(name="", limits = levels(plot_list[[1]]$y), labels= varlabels, breaks=unique(plot_list[[1]]$y))+
#     ggplot2::scale_shape_manual(name="", values = c("Missing"=4))+
#     ggplot2::theme_classic()+
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.33, hjust=0),
#                    axis.text.y = ggplot2::element_text(vjust = 0.33, hjust=0),
#                    axis.title.x= ggplot2::element_blank(),
#                    axis.title.y= ggplot2::element_blank(),
#                    plot.margin = grid::unit(mar, "cm"),
#                    plot.caption=ggplot2::element_text(hjust = 0))+
#     ggplot2::ggtitle(plot_title)+
#     ggplot2::guides(alpha="none",
#                     fill  = ggplot2::guide_legend(order = 1),
#                     shape = ggplot2::guide_legend(order = 2))+
#     ggplot2::facet_wrap(~ samp, labeller = ggplot2::labeller(samp = labellist),ncol = ncol_facet)
# 
#   if(note==TRUE) comparison_plot<-comparison_plot + ggplot2::labs(caption = plot_list$note_text)
# 
# 
#   if (diff_perc==TRUE) {
#     comparison_plot <- comparison_plot + ggplot2::geom_label(x=Inf, y=Inf,
#                                                              ggplot2::aes(  hjust = 1, vjust = 1), data=summary_df,
#                                                              label = summary_df$label,
#                                                              fill = ggplot2::alpha("white", perc_diff_transparance),
#                                                              color = ggplot2::alpha("black", 1), size= diff_perc_size, show.legend = FALSE)}
# 
# 
#
# 
#   
#     return (comparison_plot)
# 
# }



empty_finder <- function(df,samp_names){
  
  varnames<-as.character(unique(df$y))
  samps<-as.character(unique(df$samp))
  sampnames<-samp_names
  
  
  
  for (i in 1:length (varnames)) {
    for (j in 1:length (varnames)){
      for (k in 1:length (samps)) {
        
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df$samp==samps[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & is.na(df[,3]) & df$samp!=samps[k])))==FALSE) df<-rbind(df, c(varnames[i],varnames[j],"X",NA,NA,NA,NA,NA,NA,NA,NA,samps[k],sampnames[k]))
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df$samp==samps[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & df$samp!=samps[k])))==TRUE) df<-rbind(df, c(varnames[i],varnames[j],NA,NA,NA,NA,NA,NA,NA,NA,NA,samps[k],sampnames[k]))
      }
      
    }
    
  }
  return (df)
}


empty_finder2 <- function(df){
  
  varnames<-as.character(unique(df$y))
  sampnames<-as.character(unique(df$samp))
  length(varnames)
  
  for (i in 1:length (varnames)) {
    for (j in 1:length (varnames)){
      for (k in 1:length (sampnames)) {
        
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df["samp"]==sampnames[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & is.na(df[,3]) & df["samp"]!=sampnames[k])))==FALSE) df<-rbind(df, c(varnames[i],varnames[j],"X",NA,NA,sampnames[k]))
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df["samp"]==sampnames[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & df["samp"]!=sampnames[k])))==TRUE) df<-rbind(df, c(varnames[i],varnames[j],NA,NA,NA,sampnames[k]))
      }
      
    }
    
  }
  return (df)
}



difference_summary<-function(results_object,sum_weights=NULL,breaks){
  
  ### prepare needed variables ###
  varnames<-as.character(unique(results_object$x))
  samps<-unique(results_object$samp)
  results_object$sum_weight<-NA
  summary_df<-data.frame("samp"=NA,"label"=NA)
  
  ### check for sum_weights ###
  if (is.null(sum_weights)) {
    sum_weights<-matrix(data=1, nrow=length(samps), ncol=length(varnames))
  }
  
  for (i in 1:length(samps)){
    
    
    help_matrix<-matrix(NA, nrow=length(varnames), ncol=length(varnames))
    rownames(help_matrix)<-varnames
    colnames(help_matrix)<-varnames
    
    ### build a weight matrix ###
    for (f in 1:length(varnames)){
      for (g in 1:length(varnames)){
        help_matrix[f,g]<-sum_weights[i,][f]*sum_weights[i,][g]
      }
    }
    
    ### turn weight matrix to df ###
    help_matrix_df<-reshape2::melt(help_matrix)
    help_matrix_df$samp<-samps[i]
    
    
    
    ### add help_matrix to results_object ###
    results_object$sum_weight[results_object$samp==samps[i]]<-help_matrix_df$value
    
    
    results_object$sum_weight[is.na(results_object$value)]<-NA
    
    ### build a summary for every sample ###
    
    percental_difference_b1<-sum(results_object$sum_weight[results_object$value == breaks[1] & is.na(results_object$value)==FALSE
                                                           & results_object$samp==samps[i] & results_object$value != "X"])/
      sum(results_object$sum_weight[is.na(results_object$value)==FALSE & results_object$samp==samps[i] & results_object$value != "X"])
    
    percental_difference_b2<-sum(results_object$sum_weight[results_object$value == breaks[2] & is.na(results_object$value)==FALSE
                                                           & results_object$samp==samps[i] & results_object$value != "X"])/
      sum(results_object$sum_weight[is.na(results_object$value)==FALSE & results_object$samp==samps[i] & results_object$value != "X"])
    if (length(breaks)>2) {
      percental_difference_b3<-sum(results_object$sum_weight[results_object$value == breaks[3] & is.na(results_object$value)==FALSE
                                                             & results_object$samp==samps[i] & results_object$value != "X"])/
        sum(results_object$sum_weight[is.na(results_object$value)==FALSE & results_object$samp==samps[i] & results_object$value != "X"])}
    
    
    # Define a function to pad a string with leading spaces
    pad_with_spaces <- function(x, width=4) {
      if (nchar(x) < width) {
        paste0(rep("  ", width - nchar(x)),x)
      } else {
        x
      }
    }
    
    
    diff_summary<-paste0(breaks[1],"  ",pad_with_spaces(format((round((percental_difference_b1), digits = 3)*100),nsmall=1))," %\n",
                         breaks[2],"  ",pad_with_spaces(format((round(percental_difference_b2, digits = 3)*100),nsmall=1))," %")
    if (length(breaks)>2) diff_summary<-paste0 (diff_summary, "\n",breaks[3], "  ", pad_with_spaces(format((round(percental_difference_b3, digits = 3)*100),nsmall=1))," %")
    
    summary_df[i,]<- c(as.character(samps[i]), diff_summary)
  }
  
  return(summary_df)
}





### Some needed little functions ###

na_help_func<-function(x){
  ifelse(x==1,return(1),return(na_help_func(x-1)+x-1))
  #if(x==1) return(1)
  #if(x>=2) return(na_help_func(x-1)+x-1)
}

na_func<-function(values,vars,i){
  
  ifelse(i==0,
         return(rep(NA,(length(vars)-i))),
         return(c(values[na_help_func(i):(na_help_func(i)+(i-1))],rep(NA,length(vars)-i))))
  # if(i==0) return(rep(NA,(length(vars)-i)))
  # else(c(values[na_help_func(i):(na_help_func(i)+(i-1))],rep(NA,length(vars)-i)))
  
}

r_cor_func<-function(design,var1,var2,corrtype="pearson"){
  
  if(corrtype=="pearson"){
  variance <- suppressWarnings(survey::svyvar(stats::reformulate(var1,var2), design, na.rm = TRUE,
                             return.replicates=TRUE))}
  
  if(corrtype=="spearman"){
    
    svyrank<-function(formula, design){
      mf <- stats::model.frame(formula, stats::model.frame(design), na.action = stats::na.omit)
      y<-mf[,1]
      ii <- order(y)
      n <- length(y)
      rankhat <- numeric(n)
      w <- stats::weights(design, "sampling")
      rankhat[ii] <- stats::ave(cumsum(w[ii]) - w[ii]/2, factor(y[ii]))
      rankhat
    }
    
    #rank_desig<-svyrank(stats::reformulate(var1,var2),design)
    
    variance <- suppressWarnings(survey::svyvar(stats::reformulate(var1,var2), 
                                                design, na.rm = TRUE, 
                                                return.replicates=TRUE))
    
    
  }
  suppressWarnings(cormatrix<-stats::cov2cor(as.matrix(variance)))
  # boot_r<-map_dbl(1:nrow(variance[[2]]),~cov2cor(matrix(variance[.x,],ncol=2))[2])
  # boot_SE<-sqrt(stats::var(boot_r))
  list(cormatrix[2],variance)
}

### get a longer list of variables
varfunc<-function(vars,i){
  c(rep(vars[i],i-1))
}

varfunc2<-function(vars,i){
  c(vars[1:i])
}

### Function to calculate pairwise n
n_func<-function(design,var1,var2){
  rows<-"rows"
  
  formula<-paste(rows,  " ~ " ,var1, " + ", var2)
  
  n<-survey::svytotal(stats::as.formula(formula), design, na.rm = TRUE)[1]
  names(n)<-NULL
  n
}

### Function, to weight correlations ###

wgt_cor<-function(df, row, col, i = NULL, weight_var = NULL, stratas = NULL, ids = NULL,  
                  bench=NULL,weight_var_bench=NULL,
                  stratas_bench=NULL,ids_bench=NULL,
                  variables = NULL, remove_nas = "pairwise", nboots=0, 
                  adjustment_weighting="raking",
                  adjustment_vars=NULL, raking_targets=NULL,post_targets=NULL,
                  cor_matrix_bench=NULL,
                  parallel=FALSE,corrtype="pearson",benchmark=FALSE,
                  boot_all=FALSE,
                  percentile_ci=TRUE){
  
  

  
  ### pret dataframe for the boot function
  if(remove_nas=="all") {
    df<-stats::na.omit(df)
    if(isTRUE(boot_all)) bench<-stats::na.omit(bench)}
  df$rows<-1
  
  ### prepare weight variables ###
  ifelse(is.null(ids)==FALSE,
         ifelse(is.na(ids)==FALSE,ids<-df[,ids],c(1:nrow(df)))
         ,ids<-c(1:nrow(df)))
  
  if(isTRUE(boot_all)){
    ifelse(is.null(ids_bench)==FALSE,
           ifelse(is.na(ids_bench)==FALSE,ids_bench<-bench[,ids_bench],c(1:nrow(bench)))
           ,ids_bench<-c(1:nrow(bench)))
  }
  
  ifelse(is.null(weight_var)==FALSE,
         ifelse(is.na(weight_var)==FALSE,weight_var<-df[,weight_var],weight_var<-c(rep(1,nrow(df))))
         ,weight_var<-c(rep(1,nrow(df))))
  
  if(isTRUE(boot_all)){
    ifelse(is.null(weight_var_bench)==FALSE,
           ifelse(is.na(weight_var_bench)==FALSE,weight_var_bench<-bench[,weight_var_bench],weight_var_bench<-c(rep(1,nrow(bench))))
           ,weight_var_bench<-c(rep(1,nrow(bench))))
  }

  
  ### normalize the weight_var
  weight_var<- weight_var/(sum(weight_var)/nrow(df))
  if(isTRUE(boot_all)){weight_var_bench<- weight_var_bench/(sum(weight_var_bench)/nrow(bench))}
  
  if(is.null(stratas)==FALSE){
    ifelse(is.na(stratas)==FALSE,stratas<-df[,stratas],stratas<-NULL)
  }

  if(isTRUE(boot_all)){
    if(is.null(stratas_bench)==FALSE){
      ifelse(is.na(stratas_bench)==FALSE,stratas_bench<-bench[,stratas_bench],stratas_bench<-NULL)
    }
  }
  
  ### turn factor dummy variables into numeric variables ###
  df<-unfactor(df=df,func="d_mean",weight=NULL,strata=NULL,id=NULL)
  
  
  ### Get surveydesign ###
  df_design<- survey::svydesign(id      = ids,
                                strata  = stratas,
                                weights = weight_var,
                                nest    = FALSE,
                                data    = df)
  
  if(isTRUE(boot_all)){
    bench_design<- survey::svydesign(id      = ids_bench,
                                  strata  = stratas_bench,
                                  weights = weight_var_bench,
                                  nest    = FALSE,
                                  data    = bench)
  }
  
  #################################
  ### if no bootstrap is needed ###
  #################################
  
  if(nboots==0){
  
    if(is.null(adjustment_vars)==FALSE & adjustment_weighting=="raking"){
      
      adjustment_vars<-purrr::map(paste0("~",adjustment_vars),stats::as.formula)
      
      survey_design_raked <- survey::rake(design = df_design, 
                                          population.margins = raking_targets, 
                                          sample.margins = adjustment_vars, 
                                          control = list(maxit =1000, epsilon = 1, verbose=FALSE))
      
    }
    
    if(is.null(adjustment_vars)==FALSE & adjustment_weighting=="post_strat"){
      
      #adjustment_vars<-purrr::map(paste0("~",adjustment_vars),as.formula)
      
      survey_design_post <- survey::postStratify(design= df_design, 
                                         strata=stats::reformulate(adjustment_vars), 
                                         population=post_targets,
                                         partial = FALSE)
      
    }
    
    
    }
  
  ##############################
  ### if bootstrap is needed ###
  ##############################
  
  if(nboots>0){
    
    bootstrap_rep_design <- svrep::as_bootstrap_design(df_design,
                                                       type = "Rao-Wu-Yue-Beaumont",
                                                       replicates = nboots)
    
    if(is.null(adjustment_vars)==FALSE & adjustment_weighting=="raking"){
      
      adjustment_vars<-furrr::future_map(paste0("~",adjustment_vars),stats::as.formula)
      
      survey_design_raked <- survey::rake(design = bootstrap_rep_design, 
                                  population.margins = raking_targets, 
                                  sample.margins = adjustment_vars, 
                                  control = list(maxit =1000, epsilon = 1, verbose=FALSE))
    }
    
    if(is.null(adjustment_vars)==FALSE & adjustment_weighting=="post_strat"){
      
      #adjustment_vars<-purrr::map(paste0("~",adjustment_vars),as.formula)
      
      survey_design_post <- survey::postStratify(design= bootstrap_rep_design,
                                         strata=stats::reformulate(adjustment_vars),
                                         population=post_targets,
                                         partial = FALSE)
      

      
    }
    
    
    if(isTRUE(boot_all)){
      
      bench_design <- svrep::as_bootstrap_design(bench_design,
                                                         type = "Rao-Wu-Yue-Beaumont",
                                                         replicates = nboots)}
    
    }
 
  ###############################
  ### choose the right design ###
  ###############################
  
  if(nboots==0 & is.null(adjustment_vars)==TRUE) final_design<-df_design
  if(nboots>0 & is.null(adjustment_vars)==TRUE) final_design<-bootstrap_rep_design
  if(is.null(adjustment_vars)==FALSE & adjustment_weighting=="raking") final_design<-survey_design_raked
  if(is.null(adjustment_vars)==FALSE & adjustment_weighting=="post_strat") final_design<-survey_design_post
  
  
  ############################################
  ### calculate number of n for every pair ###
  ############################################
  
  n<-mapply(n_func,var1=row,var2=col,
            MoreArgs = list(design = df_design))
  
  #####################################
  ### calculate R and variance list ###
  #####################################
  

  if(parallel!=FALSE){future::plan(future::multisession,workers=parallel)}
  

  r_cor_list<-furrr::future_map2(row,col,~r_cor_func(final_design,.x,.y,corrtype=corrtype))
  r<-furrr::future_map_dbl(1:length(row),~r_cor_list[[.x]][[1]])
  var_list<-furrr::future_map(1:length(row),~r_cor_list[[.x]][[2]])
  
  if(isTRUE(boot_all)){
  r_cor_list_bench<-furrr::future_map2(row,col,~r_cor_func(bench_design,.x,.y,corrtype=corrtype))
  r_bench<-furrr::future_map_dbl(1:length(row),~r_cor_list_bench[[.x]][[1]])
  var_list_bench<-furrr::future_map(1:length(row),~r_cor_list_bench[[.x]][[2]])}
  
  
  if(nboots>0){
  boot_r_func<-function(variance){
    
    boot_r<-purrr::map_dbl(1:nrow(variance[[2]]),~suppressWarnings(cov2cor(matrix(variance[[2]][.x,],ncol=2)))[2])
    
  }
  boot_r<-purrr::map(var_list,~boot_r_func(.x))
  if(boot_all==TRUE) boot_r_bench<-purrr::map(var_list_bench,~boot_r_func(.x))
  
  }
  
  
  
  ##########################
  ### calculate p values ###
  ##########################
  
  if(nboots==0){
    
    t<-(abs(r)*sqrt(n-2))/suppressWarnings(sqrt(1-(r^2)))
    p<-2*stats::pt(t,(n-2),lower.tail = FALSE)}
  
  if(nboots>0){
    
    boot_pvalues<-function(boot_object,reference=0,r=NULL,row,col,iteration=NULL,
                           benchmark=FALSE,percentile_ci=TRUE,r1=NULL,r1_bench=NULL){
      
      
      if(is.null(r1_bench)==F){ 
        if(r1=="-1" & r1_bench=="-1") return(1)}
      
      boot_object[boot_object %in% "NaN"]<-NA
      if(!is.null(r)) r[r%in%"NaN"]<-NA
      in_interval <-TRUE
      alpha<-0
      
      ### check if boot_object contains na ###
      if(sum(is.na(boot_object)) > 0 & is.null(iteration)==FALSE){
        
        warning(paste(sum(is.na(boot_object)), "of the", length(boot_object),
                      "bootstraps contain not enough combined cases for the variable pair of:",
                      row[iteration],"and",col[iteration],".\n"))
      }
      if(benchmark==TRUE & sum(is.na(boot_object))==length(boot_object)) return("NaN")
      if(sum(is.na(boot_object))==length(boot_object)) return(NA)
      #if(!is.null(r)) if(is.na(r)) return(NA)
      if(is.null(r)==FALSE) {if(length(r)==1){if(is.na(r)) return(NA)}}
      if(is.null(r)==FALSE) {if(length(r)==1){if(all(is.na(r))) return(NA)}}
      
      
      
      
      ### get p_values up to 0.00001
      while(in_interval & alpha<1){
        alpha <- alpha + 0.001
        #if(iteration==69)browser()
        if(is.null(r) & percentile_ci==TRUE) cis<-c(stats::quantile(boot_object, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(boot_object, probs=(alpha/2),na.rm=TRUE))
        if(is.null(r) & percentile_ci==FALSE){
          SE=stats::sd(boot_object,na.rm = T)
          #if(alpha=="2") #browser()#return(1)
          cis<-c(r1 + stats::qnorm(1-alpha/2) * SE,
                 r1 - stats::qnorm(1-alpha/2) * SE)}

        
        if(is.null(r)==FALSE){
          if(percentile_ci==TRUE){
          diff<- boot_object - r
          cis<-c(stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(diff, probs=(alpha/2),na.rm=TRUE))}
          
          if(percentile_ci==FALSE){
            SE=stats::sd(boot_object-r,na.rm = T)
            if(alpha=="1") return(1)
            cis<-c(r1-r1_bench + stats::qnorm(1-alpha/2) * SE,
                   r1-r1_bench - stats::qnorm(1-alpha/2) * SE)
            }
          # lower_ci<- (r) - (mean(boot_object,na.rm=TRUE) - (r))-stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object))))
          # upper_ci<- (r) - (mean(boot_object,na.rm=TRUE) - (r))+stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object))))
          # cis<-c(lower_ci,upper_ci)
        }
        
        in_interval<- (reference > cis[1] & reference < cis[2])|(reference < cis[1] & reference > cis[2])

      }
      
      alpha<-alpha-0.001
      in_interval<-TRUE
      
      while(in_interval & alpha<1){
        alpha <- alpha + 0.00001
        if(is.null(r)==TRUE & percentile_ci==TRUE) cis<-c(stats::quantile(boot_object, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(boot_object, probs=(alpha/2),na.rm=TRUE))
        
        if(is.null(r)==TRUE & percentile_ci==FALSE){
          SE=stats::sd(boot_object,na.rm = T)
          if(alpha=="1") return(1)
          cis<-c(r1 + stats::qnorm(1-alpha/2) * SE,
                 r1 - stats::qnorm(1-alpha/2) * SE)}
        
        if(is.null(r)==FALSE){
          if(percentile_ci==TRUE){
          diff<- boot_object - r
          cis<-c(stats::quantile(diff, probs=(1-(alpha/2)),na.rm=TRUE),stats::quantile(diff, probs=(alpha/2),na.rm=TRUE))}
          
          if(percentile_ci==FALSE){
            SE=stats::sd(boot_object-r,na.rm = T)
            if(alpha=="1") return(1)
            cis<-c(r1-r1_bench + stats::qnorm(1-alpha/2) * SE,
                   r1-r1_bench - stats::qnorm(1-alpha/2) * SE)}
          # lower_ci<- (r) - (mean(boot_object) - (r))-stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object))))
          # upper_ci<- (r) - (mean(boot_object) - (r))+stats::qnorm(1-alpha/2) * (suppressWarnings(sqrt(var(boot_object))))
          # cis<-c(lower_ci,upper_ci)
        }
        in_interval<- (reference > cis[1] & reference < cis[2])|(reference < cis[1] & reference > cis[2])
      }
      alpha
    }
    
    
    p<-purrr::map_dbl(1:length(boot_r),~boot_pvalues(boot_r[[.x]],row=row,col=col,iteration=.x,benchmark=benchmark,percentile_ci=percentile_ci,r1=r[.x]))
    

    cor_matrix_bench[[1]][!lower.tri(cor_matrix_bench[[1]])]<-NA
    bench_cor<-c(t(cor_matrix_bench[[1]]))
    bench_cor<-bench_cor[is.na(bench_cor)==FALSE|is.nan(bench_cor)==TRUE]
    
    
    
    
    if(isFALSE(boot_all)) p_diff<-furrr::future_map(1:length(boot_r),~boot_pvalues(boot_r[[.x]],r=bench_cor[.x],benchmark=benchmark,percentile_ci=percentile_ci,r1=r[.x],r1_bench=bench_cor[.x]))
    if(isTRUE(boot_all)) p_diff<-furrr::future_map(1:length(boot_r),~boot_pvalues(boot_r[[.x]],r=boot_r_bench[[.x]],benchmark=benchmark,percentile_ci=percentile_ci, r1=as.numeric(r[.x]), r1_bench=as.numeric(r_bench[.x])))
    
    
  }
    
  
    
    r<- matrix(unlist(lapply(c(0:(length(variables)-1)),
                             na_func,values=r,vars=variables)),
               ncol=length(variables), byrow = TRUE,
               dimnames = (list(variables,variables)))
    
    n<- matrix(unlist(lapply(c(0:(length(variables)-1)),
                             na_func,values=n,vars=variables)),
               ncol=length(variables), byrow = TRUE,
               dimnames = (list(variables,variables)))
    
    p<- matrix(unlist(lapply(c(0:(length(variables)-1)),
                             na_func,values=p,vars=variables)),
               ncol=length(variables),
               nrow=length(variables), byrow = TRUE,
               dimnames = (list(variables,variables)))
    
    if(nboots==0) p_diff<-NULL
    if(nboots>0) p_diff<-matrix(unlist(lapply(c(0:(length(variables)-1)),
                                              na_func,values=p_diff,vars=variables)),
                                ncol=length(variables),
                                nrow=length(variables), byrow = TRUE,
                                dimnames = (list(variables,variables)))
    
    output<- list(r=r,n=n,P=p,p_diff=p_diff)
    
  rm(final_design)
  #if(nboots==0 ) return(output)
  #if(nboots>0 ) return(r)
  return(output)
}





# boot_pvalues2<-function(boot_object,variables){
#   
#   ps<-sapply(1:((length(variables)*(length(variables)-1))/2),subfunc_boot_pvalues2, boot_object=boot_object)
#   ps<-matrix(unlist(lapply(c(0:(length(variables)-1)),
#                            na_func,values=ps,vars=variables)),
#              ncol=length(variables),
#              nrow=length(variables), byrow = TRUE,
#              dimnames = (list(variables,variables)))
#   ps
# }
# 
# subfunc_boot_pvalues2<-function(boot_object,i){
#   
#   if(is.na(as.vector(boot_object$t0)[i])==FALSE){
#     alpha<-boot.pval::boot.pval(boot_object, type="perc",theta_null=0,index = i)}
#   
#   alpha
# }


weighted_correlation<-function(df, cor_matrix_bench,
                               weight = NULL, stratas = NULL, 
                               ids = NULL, 
                               bench=NULL,weight_bench=NULL,
                               stratas_bench=NULL,ids_bench=NULL,
                               variables = NULL, 
                               remove_nas = "pairwise", 
                               nboots = 0, parallel = FALSE, 
                               adjustment_weighting="raking",
                               adjustment_vars=NULL,
                               raking_targets=NULL,
                               post_targets=NULL,
                               corrtype="pearson",
                               benchmark=FALSE,boot_all=FALSE,
                               percentile_ci=TRUE){
  
  
  ### get only needed rows and collumns
  row<- varfunc(variables,2:(length(variables)))
  col<- unlist(sapply(c(1:(length(variables)-1)),varfunc2,vars=variables))
  
  
  if (nboots!=0 & nboots <=1) {
    stop("nboots must be 0 (for analytic p_values) or >1 for bootstrap p_values")}
  
  #if(nboots == 0){
    output<-wgt_cor(df = df, weight_var = weight, stratas = stratas, 
                    ids = ids, variables = variables, 
                    bench=bench,weight_var_bench=weight_bench,
                    stratas_bench=stratas_bench,ids_bench=ids_bench,
                    remove_nas = remove_nas, nboots=nboots,
                    row=row, col=col, 
                    adjustment_weighting=adjustment_weighting,
                    adjustment_vars=adjustment_vars,
                    raking_targets=raking_targets,
                    post_targets=post_targets,
                    cor_matrix_bench=cor_matrix_bench,
                    parallel=parallel,
                    corrtype=corrtype,
                    benchmark=benchmark,
                    boot_all=boot_all,
                    percentile_ci=percentile_ci)

    
  output
  
  
  
}


