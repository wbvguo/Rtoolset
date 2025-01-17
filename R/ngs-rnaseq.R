#' perform log transformation with psuedo count
#'
#' @param df_mat data.frame or matrix
#' @param count psuedo count, default is 1
#' @param base base of the log transformation, default is 2
#'
#' @return a log transformed data.frame
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#'
#' log_transform(count_df)
log_transform = function(df_mat, count = 1, base = 2){
  df_log = df_mat %>% data.frame() %>%
    lapply(function(x){log(x+count, base = base)}) %>% data.frame() %>%
    magrittr::set_rownames(rownames(df_mat)) %>%
    magrittr::set_colnames(colnames(df_mat))
  return(df_log)
}




#' filter countData by gene expression, perform cpm and log2(cpm+1) transformation
#'
#' @param count_df a data.frame of count data, rows are genes and columns are samples
#' @param group a vector of group information, default is NULL, every sample is in the same group
#' @param min_count minimum count for a gene to be kept, default is 10
#' @param min_prop minimum proportion of samples in the smallest group to have the minimum count, default is 0.1
#'
#' @return a list of edgeR objects, including `dge_count`, `dge_cpm`, `dge_keep`, `dge_keep_cpm`, `dge_keep_cpm_log`
#' @export
#'
#' @examples
#' filter_calcpm_dge(count_df)
filter_calcpm_dge = function(count_df, group = NULL, min_count = 10, min_prop = 0.1){
  if(is.null(group)){group = rep(1, ncol(count_df))}
  dge_count = edgeR::DGEList(counts = count_df, group = group)
  dge_cpm   = edgeR::cpm(dge_count)

  keep_idx  = edgeR::filterByExpr(dge_count, min.count = min_count, min.prop = min_prop)
  dge_keep  = edgeR::calcNormFactors(dge_count[keep_idx, ,keep.lib.sizes=FALSE])
  dge_keep_cpm = edgeR::cpm(dge_keep)
  dge_keep_cpm_log = log_transform(dge_keep_cpm)

  return(list(dge_count= dge_count,
              dge_cpm  = dge_cpm,
              dge_keep = dge_keep,
              dge_keep_cpm = dge_keep_cpm,
              dge_keep_cpm_log = dge_keep_cpm_log))
}




#' get the the var_gene_list and topN (or top percent) most variable genes
#'
#' @param count_df a data.frame of count data, rows are genes and columns are samples
#' @param prop proportion of top genes to return, default is 0.2
#' @param topN number of top genes to return, default is NULL
#'
#' @return a list of variance (`var_genes_list`) and the expression matrix of most-variable genes (`topN_mat`)
#' @export
#'
#' @examples
#' get_top_var_mat(count_df)
get_top_var_mat = function(count_df, prop = 0.2, topN = NULL){
  var_genes_list = apply(count_df, MARGIN = 1, var)
  if(prop>1 | prop<=0){stop("prop should be in (0,1]")}
  if(is.null(topN)){topN = round(nrow(count_df)*prop)}
  topN_genes = names(sort(var_genes_list, decreasing=TRUE))[1:topN]
  return(list(var_genes = var_genes_list,
              topN_mat  = as.matrix(count_df[topN_genes,])))
}
