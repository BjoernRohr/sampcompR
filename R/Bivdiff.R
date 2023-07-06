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
                               note=T,p_adjust=NULL, full=FALSE, grid="white",diff_perc=F,
                               diff_perc_size=4.5,perc_diff_transparance=0 ,gradient=F, 
                               breaks=breaks, colors=NULL, remove_nas="pairwise", nboots=0,
                               parallel = F) {


  ### Build title
  df1_label<-deparse(substitute(df))
  bench_label<-deparse(substitute(benchmark))
  plot_title<-ifelse(is.null(plot_title),paste("Compare ", df1_label," & ",bench_label, sep = "", collapse = NULL), plot_title)

  weighted_cor<-function(x,weight, ID, strata=NULL,return="r"){

    variables<-colnames(x)
    if(is.null(ID)==F) if(is.na(ID)==F)variables<-variables[!variables==ID]
    if(is.null(weight)==F) if(is.na(weight)==F) variables<-variables[!variables==weight]
    if(is.null(strata)==F) if(is.na(strata)==F) variables<-variables[!variables==strata]
    
    
    if(is.null(ID)==F) {
      if(is.na(ID)==F) ID<-x[,ID]
      else ID<-c(1:nrow(x))} 
    if(is.null(ID)==T) ID<-c(1:nrow(x))
    
    if(is.null(weight)==F) {
      if(is.na(weight)==F) weight<-x[,weight]
      else weight<-c(rep(1,nrow(df)))} 
    if(is.null(weight)==T) weight<-c(rep(1,nrow(df)))
    
    ### normalize the weight
    weight<- weight/(sum(weight)/nrow(x))
    
    if(is.null(strata)==F){
      if (is.na(strata)==F) strata<-x[,strata]
      else strata<-NULL
    }
    
    
    

    df_weighted<- survey::svydesign(id      = ID,
                                    strata  = strata,
                                    weights = weight,
                                    nest    = FALSE,
                                    data    = x)

    # insertform<-paste("~",colnames(x[1]))
    # for (i in 2:(length(x))) {
    #   insertform<-paste(insertform," + ",colnames(x[i]), collapse = "")
    # }


    matrix<-jtools::svycor(formula=stats::reformulate(variables), design = df_weighted, na.rm = TRUE,sig.stats=F)
    if(return=="p") matrix2<-jtools::svycor(formula=stats::reformulate(variables), design = df_weighted, na.rm = TRUE,sig.stats=T)
    matrix$cors[matrix$cors==1]<-NA

    if(return=="r") return(matrix$cors)
    if(return=="p") return(matrix2$p.values)
  }

  ############################
  ### equalize data frames ###
  ############################
  
  ###############################################
  ### When benchmark is a object of dataframe ###
  ###############################################
  if (inherits(benchmark,"data.frame")) {
  df2<-dataequalizer(benchmark,df,variables=variables,silence=T)
  benchmark2<-dataequalizer(df2,benchmark,variables=variables,silence=T)
  

  #### add weight variables again ifany ###
  if (is.null(weight)==F) df2<-df[,c(colnames(df2),weight)]
  if (is.null(ID)==F) df2<-df[,c(colnames(df2),ID)]
  if (is.null(strata)==F) df2<-df[,c(colnames(df2),strata)]

  if (is.null(weight_bench)==F) benchmark2<-benchmark[,c(colnames(benchmark2),weight_bench)]
  if (is.null(ID_bench)==F) benchmark2<-benchmark[,c(colnames(benchmark2),ID_bench)]
  if (is.null(strata_bench)==F) benchmark2<-benchmark[,c(colnames(benchmark2), strata_bench)]

  
  #colnames(df)<-c(as.character(1:(ncol(df))))
  ### Build correlation matrices
  if(remove_nas=="all") df2<-stats::na.omit(df2)
  df<-df2
  if (is.null(weight)==FALSE & is.null(ID)==FALSE) {
    df[,weight]<-NULL
    df[,ID]<-NULL
    #ID<-df2[,ID]
    #weight<-df2[,weight]
    if (is.null(strata)==F) {
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
    if (is.null(strata_bench)==F) {
      benchmark[,strata_bench]<-NULL
      #strata_bench<-benchmark2[,strata_bench]
      }
    }
  }
  
  ###########################################
  ### When benchmark is a object of rcorr ###
  ###########################################
  
  if(inherits(benchmark,"data.frame")==F){
    
    fit_cor<-function(rcorr_object,df=NULL,variables=NULL){
      
      ### check for variables in df and variables ###
      if (is.null(variables)==F) vars<-variables[variables %in% colnames(df)]
      if (is.null(variables)) vars<- colnames(df)
      
      
      rcorr_object[[1]]<-rcorr_object[[1]][rownames(rcorr_object[[1]]) %in% vars, colnames(rcorr_object[[1]]) %in% vars]
      rcorr_object[[2]]<-rcorr_object[[2]][rownames(rcorr_object[[2]]) %in% vars, colnames(rcorr_object[[2]]) %in% vars]
      rcorr_object[[3]]<-rcorr_object[[3]][rownames(rcorr_object[[3]]) %in% vars, colnames(rcorr_object[[3]]) %in% vars]
      
      rcorr_object
      
    }
    
    benchmark<-fit_cor(benchmark,df=df,variables = variables)
    df2<-df[,colnames(benchmark[[1]])]
    
    if (is.null(weight)==F) df2<-df[,c(colnames(df2),weight)]
    if (is.null(ID)==F) df2<-df[,c(colnames(df2),ID)]
    if (is.null(strata)==F) df2<-df[,c(colnames(df2),strata)]
    
    
    if(remove_nas=="all") df2<-stats::na.omit(df2)
    df<-df2
    if (is.null(weight)==FALSE & is.null(ID)==FALSE) {
      df[,weight]<-NULL
      df[,ID]<-NULL
      #ID<-df2[,ID]
      #weight<-df2[,weight]
      if (is.null(strata)==F) {
        df[,strata]<-NULL
        #strata<-df2[,strata]
        }
    }
    
  }

  if(corrtype=="r") corrtype<-"pearson"
  
  if (is.null(ID)==F ){
    if(is.na(ID)==T) if(corrtype=="rho") corrtype<-"spearman"
    if(is.na(ID)==F) if(corrtype=="rho") corrtype<-"pearson"
  }
  if (is.null(ID)==T){
    if(corrtype=="rho") corrtype<-"spearman"
  }
  
  
  
  cor_matrix_df <- Hmisc::rcorr(as.matrix(df), type = corrtype)
  if (is.null(weight)==FALSE & is.null(ID)==FALSE) cor_matrix_df<- weighted_correlation(df2,weight=weight,ids=ID,stratas=strata,
                                                                                        variables=colnames(df), remove_nas=remove_nas,
                                                                                        nboots = nboots)
  
  
  # if (is.null(weight)==FALSE & is.null(ID)==FALSE) cor_matrix_df$r<- weighted_cor(df2, weight , ID=ID, strata = strata, return="r")
  # if (is.null(weight)==FALSE & is.null(ID)==FALSE) cor_matrix_df[[3]]<- weighted_cor(df2, weight , ID=ID, strata = strata, return="p")
  cor_matrix_df$r[cor_matrix_df$r=="NaN"]<-NA
  cor_matrix_df[[3]][cor_matrix_df[[3]]=="NaN"]<-NA

  if (inherits(benchmark,"data.frame")) {
  cor_matrix_bench <- Hmisc::rcorr(as.matrix(benchmark), type = corrtype)
  if (is.null(weight_bench)==FALSE & is.null(ID_bench)==FALSE) cor_matrix_bench<- weighted_correlation(benchmark2,weight=weight_bench,
                                                                           ids=ID_bench,stratas=strata_bench,
                                                                           variables=colnames(benchmark),remove_nas = remove_nas ,
                                                                           nboots = 0, parallel = parallel)
  # if (is.null(weight_bench)==FALSE & is.null(ID_bench)==FALSE)cor_matrix_bench$r<-
  #   weighted_cor(benchmark2, weight_bench , ID=ID_bench, strata = strata_bench, return = "r")
  # if (is.null(weight_bench)==FALSE & is.null(ID_bench)==FALSE)cor_matrix_bench[[3]]<-
  #     weighted_cor(benchmark2, weight_bench , ID=ID_bench, strata = strata_bench, return = "p")
  cor_matrix_bench$r[cor_matrix_bench$r=="NaN"]<-NA
  cor_matrix_bench[[3]][cor_matrix_bench[[3]]=="NaN"]<-NA
  }
  
  
  if (inherits(benchmark,"data.frame")==F) cor_matrix_bench<-benchmark
  

  
  
  fischer_cor_df<- psych::fisherz(cor_matrix_df$r)
  fischer_cor_bench<- psych::fisherz(cor_matrix_bench$r)

  fischer_z_test<-psych::paired.r(cor_matrix_df$r,cor_matrix_bench$r,n=cor_matrix_df$n, n2=cor_matrix_bench$n)
  fischer_z_test$p[fischer_z_test$p=="NaN"]<-NA

  p_value= ifelse(is.null(p_value),0.05,p_value)

  ### implement p_adjustments if needed ###
  if (is.null(p_adjust)==F) {
    if (p_adjust!=F) fischer_z_test$p<- matrix(stats::p.adjust(p = fischer_z_test$p, method = p_adjust),
                                                      ncol = ncol(fischer_z_test$p))}




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

  if (is.null(colors)==T) colors=c('green','yellow','red')

  note_text<- paste("Note: ",breaks[1]," ", colors[1],") means that the Pearson's rs are not significant different. \n" ,breaks[2]," (", colors[2], ") means, at least one is significant >0 or <0 and both are
  significant different from each other. \n",breaks[3]," (", colors[3], ") means all conditions for Small Diff are true and the
  coeficients differ in direction or one is double the value of the other. \nLevel of Significance is p < 0.05.")



  ### Calculate percentage of difference ###

  if(diff_perc==T) {
    percental_difference_b1<-length(comp_matrix[comp_matrix == breaks[1] & is.na(comp_matrix)==F ])/ length(comp_matrix[is.na(comp_matrix)==F])
    percental_difference_b2<-length(comp_matrix[comp_matrix == breaks[2] & is.na(comp_matrix)==F ])/ length(comp_matrix[is.na(comp_matrix)==F])
    if (length(breaks)>2) percental_difference_b3<-length(comp_matrix[comp_matrix == breaks[3] & is.na(comp_matrix)==F ])/ length(comp_matrix[is.na(comp_matrix)==F])
    if (length(breaks)>3) percental_difference_b4<-length(comp_matrix[comp_matrix == breaks[4] & is.na(comp_matrix)==F ])/ length(comp_matrix[is.na(comp_matrix)==F])

    diff_summary<-paste(breaks[1]," :",(round((percental_difference_b1), digits = 3)*100),"% \n",
                        breaks[2]," :",(round(percental_difference_b2, digits = 3)*100),"%")
    if (length(breaks)>2) diff_summary<-paste (diff_summary, "\n",breaks[3], " :", (round(percental_difference_b3, digits = 3)*100),"%")
    if (length(breaks)>3) diff_summary<-paste (diff_summary, "\n",breaks[4], " :", (round(percental_difference_b4, digits = 3)*100),"%")
  }

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

  #### add difference to data frame

  difference_r<-(cor_matrix_df$r-cor_matrix_bench$r)
  difference_r<-reshape2::melt(difference_r)
  comp_matrix_df$difference_r<-difference_r$value

  ### change gradient ###

  alpha_matrix<-(abs(cor_matrix_df$r-cor_matrix_bench$r)/2/5)+0.8
  alpha_matrix<-reshape2::melt(alpha_matrix)
  colnames(alpha_matrix) <- c("x", "y", "gradient")
  comp_matrix_df$gradient<-alpha_matrix$gradient
  #}

  if (gradient==F) alpha_matrix$value<-1

  #if (matrix==TRUE) return(comp_matrix_df)

  ##############################
  ###     Label variables    ###
  ##############################

  if (is.null(varlabels)) varlabels<- unique(comp_matrix_df$x)


  ### build biger out matrix ###


  cor_matrix_df[[1]][upper.tri(cor_matrix_df[[1]], diag= T)]<-NA
  cor_matrix_df[[2]][upper.tri(cor_matrix_df[[2]], diag= T)]<-NA
  cor_matrix_df[[3]][upper.tri(cor_matrix_df[[3]], diag = T)]<-NA
  cor_matrix_bench[[1]][upper.tri(cor_matrix_bench[[1]], diag= T)]<-NA
  cor_matrix_bench[[2]][upper.tri(cor_matrix_bench[[2]], diag= T)]<-NA
  cor_matrix_bench[[3]][upper.tri(cor_matrix_bench[[3]], diag = T)]<-NA
  diff_table<-cor_matrix_df[[1]]-cor_matrix_bench[[1]]
  fischer_z_test$p[upper.tri(fischer_z_test$p,diag=T)]<-NA

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
    {if (gradient==T) ggplot2::aes(alpha= alpha_matrix$gradient)}+
    {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1)}+
    {if (grid == "none") ggplot2::geom_tile()}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_matrix, colour = "white", lwd=1,linetype=1)}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = edge_matrix, colour = grid, lwd=1,linetype=1)}+
    ggplot2::coord_fixed()+
    ggplot2::scale_fill_manual(values= colors, name="", na.translate = FALSE)+
    ggplot2::scale_y_discrete(name="", limits = rev(levels(comp_matrix_df$x)), labels= varlabels, breaks=unique(comp_matrix_df$x))+
    ggplot2::scale_x_discrete(name="", limits = levels(comp_matrix_df$y), labels= varlabels, breaks=unique(comp_matrix_df$y))+
    #{if(gradient==T) ggplot2::scale_alpha_continuous(values = alpha_matrix$values, na.translate=FALSE)}+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=0.9),
                   axis.text.y = ggplot2::element_text(vjust = 1, hjust=0.9),
                   axis.title.x= ggplot2::element_blank(),
                   axis.title.y= ggplot2::element_blank(),
                   plot.margin = grid::unit(mar, "cm"),
                   plot.caption=ggplot2::element_text(hjust = 0))+
    ggplot2::ggtitle(plot_title)+
    ggplot2::guides(alpha="none")

  if(note==T) comparison_plot<-comparison_plot + ggplot2::labs(caption = note_text)


  if (diff_perc==T) {
    comparison_plot <- comparison_plot + ggplot2::geom_label(ggplot2::aes( x = Inf, y = Inf, hjust = 1, vjust = 1), data = diff_summary,
                                                             label=diff_summary$label,
                                                             fill = ggplot2::alpha("white", perc_diff_transparance), 
                                                             color = ggplot2::alpha("black", 1), size= diff_perc_size)}










  #if (gradient==T) return(alpha_matrix)

  if (data == FALSE) return(comparison_plot)

}







#' Compare Multiple Data Frames on a Bivariate Level
#'
#' Compare multiple data frames on a bivariate level and plot them together.
#'
#' @param dfs A character vector containing the names of data frames to compare against the
#' benchmarks.
#' @param benchmarks A character vector containing the names of benchmarks to compare the
#' data frames against, or the names of a list. If it is a list, it has to be of the form,
#' as the output of \link[Hmisc]{rcorr}, with a pearson's r matrix in the first position, a 
#' n matrix (matrix of n for every correlation) in the second position and a P matrix in the 
#' third position. The vector must either to be the same length as \code{dfs}, or length 1. 
#' If it has length 1 every survey will be compared against the same benchmark. 
#' @param variables A character vector containing the names of the variables for the comparison.
#' If NULL, all variables named similar in both the dfs and the benchmarks will be compared.
#' Variables missing in one of the data frames or the benchmarks will be neglected for this comparison.
#' @param plot_title A character string containing the title of the plot.
#' @param plots_label A character string or vector of character strings containing the new
#' names of the data frames , also used in plot.
#' @param data If TRUE, a biv_compare object is returned, containing results of the
#' comparison.
#' @param id_bench,id A character vector determining id variables used to weight
#' the \code{dfs} or \code{benchmarks} with the help of the \code{survey} package. They have
#' to be part of the respective data frame. If less characters strings are provided,
#' than in \code{dfs}, the first input is used to weight every df or benchmark, where no input 
#' is provided.
#' @param weight_bench,weight A character vector determining variables to weight
#' the \code{dfs} of \code{benchmarks}. They have to be part of the respective data frame.
#' If less characters strings are provided, than in \code{dfs}, the first input is used 
#' to weight every df or benchmark, where no input is provided. If a weight variable is 
#' provided also an id variable is needed. For weighting, the \code{survey} package is used.
#' @param strata,strata_bench A character vector determining strata variables used to
#' weight the \code{dfs} or \code{benchmarks} with the help of the \code{survey} package.
#' They have to be part of the respective data frame. If less characters strings are provided,
#' than in \code{dfs}, the first input is used to weight every df or benchmark, where no input 
#' is provided.
#' @param p_value A number between zero and one to determine the maximum significance niveau.
#' @param varlabels A character string or vector of character strings containing the new
#' names of variables, also used in plot.
#' @param mar A vector that determines the margins of the plot.
#' @param note If note = True, a note will be displayed to describe the Plot.
#' @param p_adjust Can be either TRUE or a character string indicating a adjustment method.
#' If p_adjust=T the p_values will be adjusted with the Bonferroni adjustment method, by default,
#' to account for the problem of multiple comparisons. All adjustment methods available
#' in \code{\link{p.adjust}} can be used here, with the same character strings.
#' @param grid Grid determines the color of the lines between the tiles of the heatmap.
#' @param diff_perc If true a percental difference between surveys and benchmarks is
#' displayed in the plot.
#' @param diff_perc_size A number to determine the size of the displayed percental
#' difference between surveys in the plot.
#' @param perc_diff_transparance A number to determine the transparency of the displayed
#' percental difference between surveys in the plot.
#' @param gradient If gradient = TRUE, colors in the heatmap will be more or less
#' transparent, depending on the difference in Pearson's r of the data frames of comparison.
#' @param sum_weights A vector containing information for every variable to weight them in
#' the displayed percental difference calculation. It can be used if some variables are over-
#' or underrepresented in the analysis.
#' @param legend_show_x If true the X will be shown in the legend. At the moment, das does
#' not yet work correctly.
#' @param order A character vector to determine in which order the variables should be
#' displayed in the plot.
#' @param breaks A vector to label the color scheme in the in the legend.
#' @param colors A vector to determine the colors in the plot.
#' @param corrtype A character string, indicating the type of the bivariate correlation. 
#' It can either be "r" for Pearson's r or "rho" for Spearman's "rho". At the moment,
#' rho is only applicable to unweighted data.
#' @param missings_x If TRUE, missing pairs in the plot will be marked with an X.
#' @param remove_nas A character string, that indicates how missing should be removed, can either be
#' "all", two remove all cases that contain NA in any of the variables, or "pairwise", to remove nas
#' seperately for every variable pair when calculating Pearson's r
#' @param ncol_facet Number of colomns used in faced_wrap() for the plots.
#' @param nboots A numeric value indicating the number of bootstrap replications. 
#' If nboots = 0 no bootstrapping will be performed. Else nboots must be >2. Note, 
#' that bootstraping can be very computational heavy and can therefore take a while.
#' @param parallel If True, all detected cors will be used to in bootstrapping.
#'
#' @return A object generated with the help of [ggplot2::ggplot2()], used to visualize
#' the differences between the data frames and benchmarks. If data = T instead of the plot
#' a list will be returned containing information of the analyses. This biv_compare object
#' can be used in plot_biv_compare to build a plot later on.
#'
#' @details
#' The plot shows a heatmap of a correlation matrix, where the colors are determined by
#' the similarity of the Pearson's r value in both sets of respondents. Leaving 
#' default breaks and colors,
#' * \code{Same} (green) indicates, that the Pearson's r correlation is not significant > 0 in
#' the related data frame or benchmark or the Pearson's r correlations are not significant
#' different, between data frame and benchmark.
#' * \code{Small Diff} (yellow) indicates that the Pearson's r
#' correlation is significant > 0 in the related data frame or benchmark and the Pearson's r
#' correlations are significant different, between data frame and benchmark.
#' * \code{Large Diff} (red) indicates, that the same coditions of yellow are fulfilled, and
#' the correlations are either in opposite directions,or one is double the size of the other.
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
#' bivar_comp<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("south","black"),
#'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                    data=FALSE)
#' bivar_comp
#' 
# #' @importFrom jtools svycor
#' @export

biv_compare<-function (dfs, benchmarks, variables=NULL, corrtype="r", data = FALSE,
                       id=NULL,weight=NULL, strata=NULL,  id_bench=NULL,
                       weight_bench=NULL, strata_bench=NULL, p_value=NULL,
                       p_adjust=NULL, varlabels=NULL, plot_title=NULL, plots_label=NULL,
                       diff_perc=T, diff_perc_size=4.5, perc_diff_transparance=0,  
                       note=F, order=NULL,  breaks=NULL, colors=NULL,  
                       mar = c(0,0,0,0), grid="white", gradient=F,sum_weights= NULL ,missings_x=T, 
                       legend_show_x=F,remove_nas="pairwise", ncol_facet=3, nboots=0,
                       parallel = F){





  if(is.null(colors)==T) colors=c('green','yellow','red')
  if (is.null(breaks)) breaks<-c("Same","Small Diff", "Large Diff")

  plot_list<-NULL
  summary_df<-data.frame("samp"=NA,"label"=NA)

  if (is.null(plots_label)) plots_label<-dfs[1:length(dfs)]
  if (is.null(plots_label)==F) {
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
  
  ### if p_adjust==T get bonferroni as default ###
  if(is.null(p_adjust)==F) if(isTRUE(p_adjust)) if(is.character(p_adjust)==F) p_adjust<-"bonferroni"
  

  ### Get Dataframes ###
  help<-list()
  
  for (i in 1:length(dfs)){

    curr_df<-get(dfs[i])
    curr_bench<-get(benchmarks[i])

    if (is.null(weight)==F) {
    if (is.na(weight[i])==F) {
      curr_id <- id[i]
      curr_weight<- weight[i]}
      if (is.null(strata)==F) {if (is.na(strata[i])==F) curr_strata<- strata[i]}}

    if (is.null(weight_bench)==F){
      if (is.na(weight_bench[i])==F) {
        curr_id_bench <- id_bench[i]
        curr_weight_bench<- weight_bench[i]}
      if (is.null(strata_bench)==F) {if (is.na(strata_bench[i])==F) curr_strata_bench<- strata_bench[i]}}


    

    if (is.null(weight)==F) {
      if (is.na(weight[i])) {
        curr_id <- NULL
        curr_weight<- NULL
        if (is.null(strata)==F) {if(is.na(strata[i])) curr_strata<-NULL}}}

    if (is.null(weight)) {
      curr_id <- NULL
      curr_weight<- NULL}
    if (is.null(strata)) curr_strata<-NULL

    if (is.null(weight_bench)==F){
      if (is.na(weight_bench[i])) {
        curr_id <- NULL
        curr_weight<- NULL
        if (is.null(strata_bench)==F) {if(is.na(strata_bench[i])) curr_strata_bench<-NULL}}}

    if (is.null(weight_bench)){
        curr_id_bench <- NULL
        curr_weight_bench<- NULL}
    if (is.null(strata_bench)) curr_strata_bench<-NULL


    help[[i]]<-biv_comp_subfunction(df=curr_df,benchmark = curr_bench, corrtype=corrtype, variables = variables ,
                               plot_title=plot_title, ID_bench=curr_id_bench,
                               weight_bench= curr_weight_bench, ID=curr_id,weight=curr_weight,
                               strata = curr_strata, strata_bench = curr_strata_bench,
                               p_value=p_value, varlabels=varlabels, note=note,p_adjust=p_adjust, gradient=T,
                               data = T, breaks=breaks,remove_nas=remove_nas, nboots = nboots,
                               parallel = parallel)
    
    
   
    help[[i]][[1]]$samp<-i
    if (is.null(plots_label)) help[[i]][[1]]$samp_name<-dfs[i]
    if (is.null(plots_label)==F) help[[i]][[1]]$samp_name<-plots_label[i]


    if (is.null(plot_list)==F) {
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
 # if (is.null(plots_label)==F) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = plots_label)
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
      #if (is.null(k)==F) k=k+1
      #if (is.null(k)) k<-1
      plot_df2$value[plot_df2$x==names_var[i+1] & plot_df2$y==names_var[i]]<-NA
    }
    
    edge_df<- plot_df2[is.na(plot_df2$value),]
  }
  
  
  ##########################################
  ### Calculate percentage of difference ###
  ##########################################
  
  if(diff_perc==T) {
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
  if (is.null(order)==F) plot_list[[1]]$x<-factor(plot_list[[1]]$x, levels =order)
  if (is.null(order)==F) plot_list[[1]]$y<-factor(plot_list[[1]]$y, levels = order)

  ##############################
  ###     Label variables    ###
  ##############################
  
  variables_in<-unique(plot_list[[1]]$x)
  if (is.null(varlabels)) varlabels<- unique(plot_list[[1]]$x)
  if (length(varlabels)<length(variables_in)) varlabels<-c(varlabels,variables_in[(length(varlabels)+1):length(variables_in)])


  

  
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
  plot_list$plots_label<-as.character(unique(plot_list[[1]]$samp_name))
  
  if (is.null(plot_title)==F) plot_list$plot_title<-plot_title
  if (is.null(plot_title)) plot_list$plot_title<-NA
  
  if (is.null(varlabels)==F) plot_list$varlabels<-varlabels
  if (is.null(varlabels)) plot_list$varlabels<-NA
  
  if (is.null(p_value)==F) plot_list$p_value <-p_value 
  if (is.null(p_value)) plot_list$p_value <-NA  
  
  if (is.null(id)==F) plot_list$id  <-id  
  if (is.null(id)) plot_list$id  <-NA   
  
  if (is.null(weight)==F) plot_list$weight  <-weight  
  if (is.null(weight)) plot_list$weight  <-NA   
  
  if (is.null(strata)==F) plot_list$strata   <-strata    
  if (is.null(strata)) plot_list$strata   <-NA    
  
  if (is.null(id_bench)==F) plot_list$id_bench  <-id_bench  
  if (is.null(id_bench)) plot_list$id_bench  <-NA   
  
  if (is.null(weight_bench)==F) plot_list$weight_bench  <-weight_bench   
  if (is.null(weight_bench)) plot_list$weight_bench  <-NA   
  
  if (is.null(strata_bench)==F) plot_list$strata_bench <-strata_bench  
  if (is.null(strata_bench)) plot_list$strata_bench <-NA  
  
  if (is.null(p_adjust )==F) plot_list$p_adjust  <-p_adjust   
  if (is.null(p_adjust )) plot_list$p_adjust  <-NA
  
  plot_list$note_text  <-note_text
  
  
  for (i in 1:length(plot_list)){
    plot_list[i][is.na(plot_list[i])]<-list(NULL)}
  
  ###########################
  ### Return if data=TRUE ###
  ###########################
  
  if (data == T) return(plot_list)
  
  ######################
  ###     Plots      ###
  ######################


  comparison_plot<-
    ggplot2::ggplot(plot_list[[1]], ggplot2::aes(x = plot_list[[1]]$y, y = plot_list[[1]]$x)) +
    {if (gradient==T) ggplot2::aes(alpha= gradient)}+
    {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1,
                                            ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid == "none") ggplot2::geom_tile(ggplot2::aes( fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_df, colour = "white", lwd=1,linetype=1,
                                                              ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = edge_df, colour = grid, lwd=1,linetype=1,
                                                              ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    #ggplot2::geom_point(data= subset(plot_list[[1]], value=="X"), ggplot2::aes(x = y, y = x), show.legend = T)+
    {if (missings_x==T) 
      ggplot2::geom_point(show.legend = legend_show_x, na.rm = T, ggplot2::aes(shape= factor(plot_list[[1]]$shape, levels="X", labels=c("Missing"))))}+
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
    ggplot2::facet_wrap(~ samp, labeller = ggplot2::labeller(samp = labellist),ncol = ncol_facet)

  if(note==T) comparison_plot<-comparison_plot + ggplot2::labs(caption = plot_list[[1]]$note_text)


  if (diff_perc==T) {
    comparison_plot <- comparison_plot + ggplot2::geom_label(x=Inf, y=Inf,
                                                             ggplot2::aes( hjust = 1, vjust = 1), data=summary_df, 
                                                             label = summary_df$label,
                                                             fill = ggplot2::alpha("white", perc_diff_transparance),
                                                             color = ggplot2::alpha("black", 1), size= diff_perc_size, show.legend = F)}




  if (data == F) return (comparison_plot)

}






#' Plot Comparison of Multiple Data Frames on a Bivariate Level
#'
#' Plot a object generated by \link{biv_compare} function.
#' @param biv_data_object A object generated by \link{biv_compare} function.
#' @param plot_title A character string containing the title of the plot.
#' @param plots_label A character string or vector of character strings containing the
#' new names of the data frames , also used in plot.
#' @param p_value A number between 0 and one to determine the maximum significance niveau.
#' @param varlabels A character string or vector of character strings containing the new
#' names of variables, also used in plot.
#' @param mar A vector that determines the margins of the plot.
#' @param note If note = True, a note will be displayed to describe the Plot.
# #' @param p_adjust Can be either TRUE or a character string indicating a adjustment method.
# #' If p_adjust=T the p_values will be adjusted with the Bonferroni adjustment method, by default,
# #' to account for the problem of multiple comparisons. All adjustment methods available
# #' in \code{\link{p.adjust}} can be used here, with the same character strings.
#' @param grid Grid determines the color of the lines between the tiles of the heatmap.
#' @param diff_perc If TRUE a percental measure of difference between dfs and benchmarks is
#' displayed in the plot.
#' @param diff_perc_size A number to determine the size of the displayed percental
#' difference between surveys in the plot.
#' @param perc_diff_transparance A number to determine the transparancy of the displayed
#' percental difference between surveys in the plot.
#' @param gradient If gradient = TRUE, colors in the heatmap will be more or less transparent,
#' depending on the difference in Pearson's r of the data frames of comparison.
#' @param sum_weights A vector containing information for every variable to weight them in
#' the displayed percental difference calculation. It can be used if some variables are
#' over- or underrepresented in the analysis.
#' @param legend_show_x If true the X will be shown in the legend. At the moment, das
#' does not yet work correctly.
#' @param order A character vector to determine in which order the variables should be
#' displayed in the plot.
#' @param breaks A vector to label the color sheme in the in the legend.
#' @param colors A vector to determine the colors in the plot.
#' @param missings_x If TRUE, missing pairs in the plot will be marked with an X.
#' @param ncol_facet Number of colomns used in faced_wrap() for the plots.
#'
#' @return A object generated with the help of [ggplot2::ggplot2()], used to visualize
#' the differences between the data frames and benchmarks.
#' @details The plot shows a heatmap of a correlation matrix, where the colors are determined by
#' the similarity of the Pearson's r value in both sets of respondents. Leaving 
#' default breaks and colors,
#' * \code{Same} (green) indicates, that the Pearson's r correlation is not significant > 0 in
#' the related data frame or benchmark or the Pearson's r correlations are not significant
#' different, between data frame and benchmark.
#' * \code{Small Diff} (yellow) indicates that the Pearson's r
#' correlation is significant > 0 in the related data frame or benchmark and the Pearson's r
#' correlations are significant different, between data frame and benchmark.
#' * \code{Large Diff} (red) indicates, that the same coditions of yellow are fulfilled, and
#' the correlations are either in opposite directions,or one is double the size of the other.
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
#' bivar_data<-sampcompR::biv_compare(dfs = c("north","white"),
#'                                    benchmarks = c("south","black"),
#'                                    variables= c("age","educ","fatheduc","motheduc","wage","IQ"),
#'                                    data=TRUE)
#'                         
#' sampcompR::plot_biv_compare(bivar_data)
#'
#' @export


plot_biv_compare<-function (biv_data_object, plot_title=NULL, plots_label=NULL,
                            p_value=NULL, varlabels=NULL,
                            mar = c(0,0,0,0),note=F, grid="white",diff_perc=T,
                            diff_perc_size=4.5,perc_diff_transparance=0,gradient=F,sum_weights= NULL,
                            missings_x = T, legend_show_x=F, order=NULL, breaks=NULL,colors=NULL,
                            ncol_facet = 3){



  plot_list<- biv_data_object
  if(is.null(colors)==T) colors=plot_list$colors
  if (is.null(breaks)) breaks<-plot_list$breaks


  ##########################################
  ### Calculate percentage of difference ###
  ##########################################

  if(diff_perc==T) {

    summary_df<-difference_summary(plot_list[[1]],breaks=breaks, sum_weights=sum_weights)
  }

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
      plot_df2$value[plot_df2$x==names_var[i+1] & plot_df2$y==names_var[i]]<-NA
    }

    edge_df<- plot_df2[is.na(plot_df2$value),]
  }



  #######################################
  ### reorder plots to original order ###
  #######################################

  #if (is.null(plots_label)) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = unique(plot_list[[1]]$samp_name))
  #if (is.null(plots_label)==F) plot_list[[1]]$samp_name <- factor(plot_list[[1]]$samp_name, levels = plots_label)
  if (is.null(plots_label)) plots_label <- plot_list$plots_label
  if(length(plots_label)< length(plot_list$plots_label)) plots_label[(length(plots_label)+1):length(plot_list$plots_label)]<-
      plot_list$plots_label[(length(plots_label)+1):length(plot_list$plots_label)]
  

  breaks2<-c(breaks,"X")
  colors2<-c(colors, "white")

  plot_list[[1]]$shape<-NA
  plot_list[[1]]$shape[plot_list[[1]]$value=="X"]<-"X"
  
  
  labellist_biv<-function(lables,values){
    output<-lables
    names(output)<-as.character(values)
    output
  }
  
  
  labellist<-labellist_biv(plots_label,c(1:length(plots_label)))
  

  ##############################
  ###    order variables     ###
  ##############################
  if (is.null(order)==F) plot_list[[1]]$x<-factor(plot_list[[1]]$x, levels =order)
  if (is.null(order)==F) plot_list[[1]]$y<-factor(plot_list[[1]]$y, levels = order)

  ##############################
  ###     Label variables    ###
  ##############################

  variables_in<-unique(plot_list[[1]]$x)
  if (is.null(varlabels)) varlabels<- unique(plot_list[[1]]$x)
  if (length(varlabels)<length(variables_in)) varlables<-c(varlabels,variables_in[(length(varlabels)+1):length(variables_in)])
  
  # ########################
  # ### edit plots_label ###
  # ########################
  # 
  # if (is.null(plots_label)) plots_label<-"dfs"
  # if (length(plots_label)<length(unique(plot_list[[1]]$samp_name))) plots_label<-c(plots_label,unique(plot_list[[1]]$samp_name)[(length(plots_label)+1:length(unique(plot_list[[1]]$samp_name)))])
  # 

  ######################
  ###     Plots      ###
  ######################

  comparison_plot<-
    ggplot2::ggplot(plot_list[[1]], ggplot2::aes(x = plot_list[[1]]$y, y = plot_list[[1]]$x)) +
    {if (gradient==T) ggplot2::aes(alpha= gradient)}+
    {if (grid != "none") ggplot2::geom_tile(colour= grid, lwd =1,linetype=1,
                                            ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid == "none") ggplot2::geom_tile(ggplot2::aes( fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = na_df, colour = "white", lwd=1,linetype=1,
                                                              ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    {if (grid != "white" & grid != "none") ggplot2::geom_tile(data = edge_df, colour = grid, lwd=1,linetype=1,
                                                              ggplot2::aes(fill = factor(plot_list[[1]]$value, levels = breaks)))}+
    #ggplot2::geom_point(data= subset(plot_list[[1]], value=="X"), ggplot2::aes(x = y, y = x), show.legend = T)+
    {if(missings_x==T) 
      ggplot2::geom_point(show.legend = legend_show_x, na.rm = T, ggplot2::aes(shape= factor(plot_list$shape, levels="X", labels=c("Missing"))))}+
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
    ggplot2::facet_wrap(~ samp, labeller = ggplot2::labeller(samp = labellist),ncol = ncol_facet)

  if(note==T) comparison_plot<-comparison_plot + ggplot2::labs(caption = plot_list$note_text)


  if (diff_perc==T) {
    comparison_plot <- comparison_plot + ggplot2::geom_label(x=Inf, y=Inf,
                                                             ggplot2::aes(  hjust = 1, vjust = 1), data=summary_df,
                                                             label = summary_df$label,
                                                             fill = ggplot2::alpha("white", perc_diff_transparance),
                                                             color = ggplot2::alpha("black", 1), size= diff_perc_size, show.legend = F)}




  
    return (comparison_plot)

}




empty_finder <- function(df,samp_names){
  
  varnames<-as.character(unique(df$y))
  samps<-as.character(unique(df$samp))
  sampnames<-samp_names
  
  
  
  for (i in 1:length (varnames)) {
    for (j in 1:length (varnames)){
      for (k in 1:length (samps)) {
        
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df$samp==samps[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & is.na(df[,3]) & df$samp!=samps[k])))==F) df<-rbind(df, c(varnames[i],varnames[j],"X",NA,NA,samps[k],sampnames[k]))
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df$samp==samps[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & df$samp!=samps[k])))==T) df<-rbind(df, c(varnames[i],varnames[j],NA,NA,NA,samps[k],sampnames[k]))
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
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & is.na(df[,3]) & df["samp"]!=sampnames[k])))==F) df<-rbind(df, c(varnames[i],varnames[j],"X",NA,NA,sampnames[k]))
        if ((length(df$value[df[,1]==varnames[i] & df[,2]==varnames[j] & df["samp"]==sampnames[k]])==0) &
            (any((df[,1]==varnames[i] & df[,2]==varnames[j] & df["samp"]!=sampnames[k])))==T) df<-rbind(df, c(varnames[i],varnames[j],NA,NA,NA,sampnames[k]))
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
    
    percental_difference_b1<-sum(results_object$sum_weight[results_object$value == breaks[1] & is.na(results_object$value)==F
                                                           & results_object$samp==samps[i] & results_object$value != "X"])/
      sum(results_object$sum_weight[is.na(results_object$value)==F & results_object$samp==samps[i] & results_object$value != "X"])
    
    percental_difference_b2<-sum(results_object$sum_weight[results_object$value == breaks[2] & is.na(results_object$value)==F
                                                           & results_object$samp==samps[i] & results_object$value != "X"])/
      sum(results_object$sum_weight[is.na(results_object$value)==F & results_object$samp==samps[i] & results_object$value != "X"])
    if (length(breaks)>2) {
      percental_difference_b3<-sum(results_object$sum_weight[results_object$value == breaks[3] & is.na(results_object$value)==F
                                                             & results_object$samp==samps[i] & results_object$value != "X"])/
        sum(results_object$sum_weight[is.na(results_object$value)==F & results_object$samp==samps[i] & results_object$value != "X"])}
    
    diff_summary<-paste(breaks[1]," :",(round((percental_difference_b1), digits = 3)*100),"% \n",
                        breaks[2]," :",(round(percental_difference_b2, digits = 3)*100),"%")
    if (length(breaks)>2) diff_summary<-paste (diff_summary, "\n",breaks[3], " :", (round(percental_difference_b3, digits = 3)*100),"%")
    
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

r_cor_func<-function(design,var1,var2){
  variance <- survey::svyvar(stats::reformulate(var1,var2), design, na.rm = T)
  #as.matrix(variance)
  cormatrix<-stats::cov2cor(as.matrix(variance))
  cormatrix[2]
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
  
  n<-survey::svytotal(stats::as.formula(formula), design, na.rm = T)[1]
  names(n)<-NULL
  n
}

### Function, to weight correlations ###

wgt_cor<-function(df, row, col, i = NULL, weight_var = NULL, stratas = NULL, ids = NULL, 
                  variables = NULL, remove_nas = "pairwise", bootstrap = F){
  
  
  ### look if bootstrap or not ###
  if (bootstrap==T) df<-df[i,]
  
  ### prepare variables if needed ###  
  # if(is.null(variables)){
  #   variables<-colnames(df)
  #   if(is.null(ids)==F) if(is.na(ids) == F) variables<-variables[!variables == ids]
  #   if(is.null(weight_var)==F) if(is.na(weight_var) == F) variables<-variables[!variables == weight_var]
  #   if(is.null(stratas)==F) if(is.na(stratas) == F) variables<-variables[!variables == stratas]
  # }
  
  ### pret dataframe for the boot function
  if(remove_nas=="all") df<-stats::na.omit(df)
  df$rows<-1
  
  ### prepare weight variables ###
  ifelse(is.null(ids)==F,
         ifelse(is.na(ids)==F,ids<-df[,ids],c(1:nrow(df)))
         ,ids<-c(1:nrow(df)))
  
  ifelse(is.null(weight_var)==F,
         ifelse(is.na(weight_var)==F,weight_var<-df[,weight_var],c(rep(1,nrow(df))))
         ,weight_var<-c(rep(1,nrow(df))))
  
  # if(is.null(ids)==F) {
  #   if(is.na(ids)==F) ids<-df[,ids]
  #   else ids<-c(1:nrow(df))} 
  # if(is.null(ids)==T) ids<-c(1:nrow(df))
  
  # if(is.null(weight_var)==F) {
  #   if(is.na(weight_var)==F) weight_var<-df[,weight_var]
  #   else weight_var<-c(rep(1,nrow(df)))} 
  # if(is.null(weight_var)==T) weight_var<-c(rep(1,nrow(df)))
  
  ### normalize the weight_var
  weight_var<- weight_var/(sum(weight_var)/nrow(df))
  
  if(is.null(stratas)==F){
    ifelse(is.na(stratas)==F,stratas<-df[,stratas],stratas<-NULL)
    # if (is.na(stratas)==F) stratas<-df[,stratas]
    # else stratas<-NULL
  }
  
  ### Get surveydesign ###
  df_design<- survey::svydesign(id      = ids,
                                strata  = stratas,
                                weights = weight_var,
                                nest    = FALSE,
                                data    = df)
  

  r<-c()
  c<-1
  for (i in 1:length(row)){
      r[c]<-r_cor_func(df_design,row[i],col[i])
      c<-c+1
    }

  
  
  
  if(bootstrap==F) {n<-mapply(n_func,
                              var1=row,var2=col,
                              MoreArgs = list(design = df_design))}
  
  
  ### calculate analytical p-values ###
  if(bootstrap==F) {
    t<-(abs(r)*sqrt(n-2))/sqrt(1-(r^2))
    p<-2*stats::pt(t,(n-2),lower.tail = F)
    
    r<- matrix(unlist(lapply(c(0:(length(variables)-1)),
                             na_func,values=r,vars=variables)),
               ncol=length(variables), byrow = T,
               dimnames = (list(variables,variables)))
    
    n<- matrix(unlist(lapply(c(0:(length(variables)-1)),
                             na_func,values=n,vars=variables)),
               ncol=length(variables), byrow = T,
               dimnames = (list(variables,variables)))
    
    p<- matrix(unlist(lapply(c(0:(length(variables)-1)),
                             na_func,values=p,vars=variables)),
               ncol=length(variables),
               nrow=length(variables), byrow = T,
               dimnames = (list(variables,variables)))
    
    output<- list(r=r,n=n,P=p)}
  
  
  if(bootstrap ==F ) return(output)
  if(bootstrap ==T ) return(r)
}



bootstrap_correlations<-function(df, weight=NULL, stratas=NULL, 
                                 ids=NULL, variables=NULL, remove_nas="pairwise", 
                                 nboots = 2, parallel = F, row, col){
  
  # if(is.null(weight)==F) df[,weight]<-as.numeric(df[,weight])
  # if(is.null(stratas)==F) df[,stratas]<-as.numeric(df[stratas])
  # if(is.null(ids)==F) df[,ids]<-as.numeric(df[,ids])
  ### calculate bootstrap p_values ###
  
  if(is.null(variables)){
    variables<-colnames(df)
    if(is.null(ids)==F) if(is.na(ids) == F)variables<-variables[!variables == ids]
    if(is.null(weight)==F) if(is.na(weight) == F) variables<-variables[!variables == weight]
    if(is.null(stratas)==F) if(is.na(stratas) == F) variables<-variables[!variables == stratas]
  }

    ### perform bootstrap ###

  if (parallel==T) para<-"snow"
  if (parallel==F) para<-"no"
  
    boot_out<-boot(data = df, variables = variables, statistic = wgt_cor, R = nboots, 
                   weight_var = weight, stratas = stratas, ids = ids, bootstrap = T, 
                   ncpus = (parallel::detectCores()-1), parallel = para, row=row, col=col)
   

  # p_value_func<-function(i,boot_object){
  # 
  #   if(is.na(boot_object$t0[i])) return(NA)
  #   if(is.na(boot_object$t0[i])==F) return(
  #     mean(abs(boot_object$t[,i]) >= abs(boot_object$t0[i]))
  #   )
  # }
    #return(boot_out)
    
    # boot_pvalues<-function(boot_object,variables){
    #   
    #   subfunc_boot_pvalues<-function(boot_object,i){
    #     if(is.na(as.vector(boot_object$t0)[i])==F){
    #       
    #       r_null<-0
    #       in_interval<-TRUE
    #       alpha<-0
    #       
    #       ### get p_values up to 0.0001
    #       while(in_interval){
    #         
    #         alpha <- alpha + 0.0001
    #         
    #         ci<-suppressWarnings(boot::boot.ci(boot.out=boot_object,type = "perc",index = i, conf = (1 - alpha)))
    #         cis<-c(ci$percent[4],ci$percent[5])
    #         
    #         in_interval<-  (r_null > cis[1] & r_null < cis[2])|(r_null < cis[1] & r_null > cis[2])
    #       }
    #       
    #       alpha<-alpha-0.0001
    #       in_interval<-TRUE
    #       ### get more exact p_values
    #       
    #       while(in_interval){
    #         
    #         alpha <- alpha + 0.000001
    #         
    #         ci<-suppressWarnings(boot::boot.ci(boot.out=boot_object,type = "perc",index = i, conf = (1 - alpha)))
    #         cis<-c(ci$percent[4],ci$percent[5])
    #         
    #         in_interval<-  (r_null > cis[1] & r_null < cis[2])|(r_null < cis[1] & r_null > cis[2])
    #       }
    #       
    #       alpha
    #     }
    #     
    #     else {alpha<-NA}
    #     
    #     alpha
    #     
    #   } 
    #   
    #   ps<-sapply(1:(length(variables)*length(variables)),subfunc_boot_pvalues, boot_object=boot_object)
    #   ps<-matrix(ps,ncol=length(variables))
    #   colnames(ps)<-variables
    #   rownames(ps)<-variables
    #   ps
    #   }

    boot_pvalues2<-function(boot_object,variables){
      
      subfunc_boot_pvalues2<-function(boot_object,i){
        
        if(is.na(as.vector(boot_object$t0)[i])==F){
          alpha<-boot.pval::boot.pval(boot_object, type="perc",theta_null=0,index = i)}
        
        else {alpha<-c(NA)}
        
        alpha
        
      }

      ps<-sapply(1:(length(variables)*length(variables)),subfunc_boot_pvalues2, boot_object=boot_object)
      ps<-matrix(ps,ncol=length(variables))
      colnames(ps)<-variables
      rownames(ps)<-variables
      ps}
  
  
  p<-boot_pvalues2(boot_out,variables)
  
  
  output<-wgt_cor(df = df, weight_var = weight, stratas = stratas, 
          ids = ids, variables = variables, remove_nas = remove_nas, row = row,col=col)
  output$P<-p
  
  output
}




weighted_correlation<-function(df, weight = NULL, stratas = NULL, 
                                ids = NULL, variables = NULL, remove_nas = "pairwise", 
                                nboots = 0, parallel = F){
  
  
  ### get only needed rows and collumns
  row<- varfunc(variables,2:(length(variables)))
  col<- unlist(sapply(c(1:(length(variables)-1)),varfunc2,vars=variables))
  
  
  
  if(nboots == 0){
    output<-wgt_cor(df = df, weight_var = weight, stratas = stratas, 
                    ids = ids, variables = variables,
                    remove_nas = remove_nas, bootstrap=F,
                    row=row, col=col)
  }
  
  if (nboots!=0 & nboots <=1) {
    stop("nboots must be 0 (for analytic p_values) or >1 for bootstrap p_values")}
  
  if(nboots > 1) {
    output<- bootstrap_correlations(df = df, weight = weight, stratas = stratas, 
                                    ids = ids, variables = variables, 
                                    remove_nas= remove_nas, nboots = nboots, 
                                    parallel = parallel,row=row, col=col)
  }
  
  output
  
  
  
}


