###########################################################################
###
### 		Subject:	R-script zur Dataequalizer Funktion
### 		Date: 		November 2021
### 		Author: 	Bjoern Rohr
### 	Version:  	1.00
###
### 		Bugfix:   	/
###
###########################################################################

##########################################################
### Reduce Dataset 1 to the Variables inside Dataset 2 ###
##########################################################


### Documentation of the dataequalizer ###

#' Equalize dataframes
#'
#'
#' \code{dataequalizer} compares two data frames and looks if both data frames contain columns
#' with the same Name. A copy of source_df is returned, containing only columns named identical
#' in target_df and source_df data frames. The function is mainly used in the other functions of the package.
#'
#' @param target_df A data frame
#' @param source_df A data frame containing some column-names named equally in target_df
#' @param variables A vector to indicate variable names that should be in the copy of the source_df
#' if they are also in the target_df.
#' @param silence A logic value. If FALSE, warnings will be returned indicating, what variables where removed,
#' from the survey.
#' @return Returns a copy of source_df containing only variables with names contained
#' also in the target_df data frame.
#' 
#' @examples
#' ## Get Data to equalize 
#' card<-wooldridge::card
#' 
#' ##reduce data frame
#' card2<-card[c("id","age","educ","fatheduc","motheduc","IQ","wage")]
#' 
#' card_equalized<-sampcompR::dataequalizer(card2,card,variables=c("age","educ","IQ","wage"))
#' card_equalized[1:20,]
#'
#' @export
#'

dataequalizer <- function(target_df, source_df, variables=NULL, silence=F) {

  if (is.null(variables)) {
    inside<- colnames(source_df)[colnames(source_df) %in% colnames(target_df),drop=F]
  }

  if (is.null(variables)==F) {
    inside<- colnames(source_df)[colnames(source_df) %in% colnames(target_df),drop=F]
    inside<- inside[inside %in% variables]

  }


  missing<- colnames(source_df)[!(colnames(source_df) %in% inside),drop=F]
  #inside <- labelchecker(target_df, source_df, variables = variables)

  ### Put Missing variables in string together for a warning message ###
  if(!identical(missing, character(0))){
    missingvar <- paste(missing[1])

    for (i in 2:length(missing)) {
      missingvar <- paste(missingvar, "|", missing[i], sep = " ")
    }}
  else missingvar<-NULL


  if (length(missingvar)>0)
    if (is.null(variables)==T)
      if (isFALSE(silence)) {
        warning(paste(
          "target_df has less collumns than source_df",
          "\n   Only variables included in both datasets are used",
          "\n   Missing variables are:", missingvar))
      }

  if (length(missingvar)>0)
    if(is.null(variables)==F)
      if (isFALSE(silence)) {
        warning(paste(
          "Only chosen variables included in both datasets are used",
          "\n   Missing variables are:", missingvar))
      }

  help<- source_df[,colnames(source_df) %in% inside,drop=F]
  if(is.null(variables)==F) help<-help[,variables[variables %in% colnames(help)],drop=F]


  return(help)
}




