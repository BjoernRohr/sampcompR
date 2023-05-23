######################################################################
###                                                                ###
### 		Subject:	Turn Tables to a synthetic Data frame without    ###
###               usable correlations                              ###
### 		Date: 		May 2023                                         ###
### 		Author: 	Bjoern Rohr                                      ###
### 	Version:  	1.00                                             ###
###                                                                ###
### 		Bugfix:   	/                                              ###
###                                                                ###
######################################################################

##################
#### Functions ###
##################


### function to get a dataframe out of multiple tables ###

tables_to_df_unweighted <- function(tables, varnames, reduced = NULL, tablist=NULL) {
  ## integrate adapted base function ###

  table_to_df <- function(tab) {
    dftab <- as.data.frame(tab)
    help <- matrix(c(replicate((dftab$Freq[1] / reduce), as.character(dftab$Var1[1]))))


    for (i in 2:length(tab)) {
      help <- matrix(c(help, replicate((dftab$Freq[i] / reduce), as.character(dftab$Var1[i]))))
    }

    help <- (matrix(c(help, replicate((rowmax - length(help)), NA))))

    help <- as.data.frame(help)
    return(help)
  }

  ### for reducing the datasets
  reduce <- 1
  if (is.null(reduced) == FALSE) reduce <- reduced


  ### find maximum number of rows ###

  if (is.null(tablist)){
  rowmax <- (sum(get(tables[1])) / reduce)
  if (length(tables)>=2){
    for (i in 2:length(tables)) {
    rowmax2 <- (sum(get(tables[i])) / reduce)
    if (rowmax2 > rowmax) rowmax <- rowmax2
  }}}

  if(is.null(tablist)==F){
    rowmax <- (sum(get("tablist")[[1]]) / reduce)
    if (length(tables)>=2){
      for (i in 2:length(tables)) {
      rowmax2 <- (sum(get("tablist")[[i]]) / reduce)
      if (rowmax2 > rowmax) rowmax <- rowmax2
    }}

  }

  df <- as.data.frame(matrix(NA, nrow = rowmax))
  df[1:length(tables)] <- NA
  colnames(df) <- varnames

  ### loop, to build Dataset ###
  for (i in 1:length(tables)) {
    if(is.null(tablist))  df[i] <- table_to_df(get(tables[i]))
    if(is.null(tablist)==F) df[i] <- table_to_df(get("tablist")[[i]])
  }

  for (i in 1:length(colnames(df))) {
    df[, i] <- as.numeric(df[, i])
  }

  return(df)
}





#############################################################
### A Function, to turn a Dataframe into a weighted table ###
#############################################################

### Builds on Tables_to_DF_Function ###

weighted_uni_sample <- function(df, weights, ID, strata=NULL) {

  ### getting weights and ID
  id <- get(ID, df)
  w <- get(weights, df)
  ### normalize the weight 
  w<- w/(sum(w)/nrow(df))
  
  if(is.null(strata)==F) Strata<-get(strata, df)
  if(is.null(strata)==T) Strata<-NULL

  ### remove id & weights out of df ###

  if(is.null(strata)==T) df <- df[colnames(df) != ID & colnames(df) != weights]
  if(is.null(strata)==F) df <- df[colnames(df) != ID & colnames(df) != weights & colnames(df) != strata]

  df_weighted <- survey::svydesign(
    id = ~id,
    weights = ~w,
    strata=Strata,
    data = df
  )


  ### create an empty varnames and tables vector ###
  if(is.null(strata)==F){
    varnames <- c(rep(NA, ncol(df)))
    tables <- c(rep(NA, ncol(df)))
  }

  if(is.null(strata)==T){
    varnames <- c(rep(NA, ncol(df)))
    tables <- c(rep(NA, ncol(df)))
  }

  tablist<-list()
  ### return tables of every  ###
  for (j in 1:ncol(df)) {
    colnames(df)[j]
    insert_form <- stats::as.formula(paste("~", colnames(df)[j]))
    length <- nrow(df_weighted$variables[j])


    tablist[[j]] <- as.table(as.matrix(round(survey::svytable(insert_form, df_weighted, Ntotal = length))))


    ## fill varnames and tables vector ###
    varnames[j] <- paste(colnames(df)[j])
    tables[j] <- paste("tab[[", j,"]]", sep = "")
  }

  ### turn tables back to dataframe
  weighted_df <- tables_to_df_unweighted(tables = tables, varnames, tablist= tablist)
}






### Function for both ###

### Documentation of the tables_to_df ###

# #' Turns a list of tables to df or creates a weighted dataframe
# #'
# #' \code{tables_to_df} Turns a list of tables into a dataframe. It can only be
# #' used for univariate analysis. Additionally tables_to_df can turn a dataframe
# #' into a weighted Dataframe if weightes, ID and a dataframe are provided. Again
# #' the weighted dataframe can only be used in univariate analysis.
# #'
# #' @param tables A list of strings containing the names of the tables to turn into a dataframe
# #' @param varnames A list of strings containing the names of the variables in order of the tables list.
# #' @param reduced A numeric value. IF provided the number of cases will be devided
# #' by it, while the distribution stays nearly identical. Small rounding errors can be araise.
# #' @param df A dataframe that will be weighted. Will only be used if weights are provided.
# #' @param weigts A string containing the name of the variable used for weighting.
# #' The variable has to be part of the dataframe provided, but will be removed afterwards.
# #' If a weight is provided a ID variable and a dataframe are also required. For
# #' weighting the functions \code{\link[survey]{svydesign}} and \code{\link[survey]{svytable}}
# #' are used. Rounding errors can cause the size to vary a little.
# #' @param ID A \code{ID} A string containing the name of the variable used as ID.
# #' The variable is required to be part of the dataframe and will be used in the
# #' \code{\link[survey]{svydesign}} funktion. ID will be removed afterwards.
# #' @importFrom survey svydesign
# #' @importFrom survey svytable
# #' @importFrom stats as.formula
# #' 
# #' @keywords internal


tables_to_df <- function(tables = NULL, varnames = NULL, reduced = NULL, df = NULL, weights = NULL, ID = NULL, strata=NULL) {
  if (is.null(weights)) final_df<-tables_to_df_unweighted(tables, varnames, reduced = reduced)
  if (!is.null(weights)) final_df<-weighted_uni_sample(df, weights, ID, strata = strata)

  return(final_df)
}


### get varnames out of input to exclude weights and id from dataset


get_varname_of_input <- function(var) {
  input <- deparse(substitute(var)) ### get the names of the input for var
  pos <- which(strsplit(input, "")[[1]] == "$") ### Find $ position for var
  varname <- substr(input, pos + 1, nchar(input)) ### Find varname for var

  print(varname)
}


# test<-function(var){
#  help<-do.call("get_varname_of_input",list(var))
#  return(help)
# }

# test(ges$ID)

# idname<-deparse(substitute(ges$ID))
# get(idname)
## get_varname_of_input(var=eval(noquote(idname)))

# ges2 <- ges_w[rowSums(is.na(ges_w)) != ncol(ges_w), ] # Apply is.na function
# ges2                                            # Printing updated data

# noquote(idname)

# deparse(substitute(ges$ID))

# var<-ges$ID


# ID<-"ID"

# a<-get(ID,ges)

# df_weighted<- survey::svydesign(id      = ~a,
#                        weights = ~ges$gp_design_weight,
#                       data    = ges)



# sum<-sum(table(df_weighted$variables["ID"]))
# insert_form<- as.formula(paste("~", colnames(ges)["ID"]))


# tab<-as.table(as.matrix(round(svytable(~ ID,df_weighted, Ntotal=sum))))

# tab

# sum(tab48)
# sum(tab49)
# View(tab49)


# get("ID",ges)




