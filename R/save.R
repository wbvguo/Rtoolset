#' `save2pdf`
#'
#' save a plot to a pdf file
#'
#' @param file a character string of the file path
#' @param plot_code a language object of the plot code, usually use `quote` to wrap the code
#' @param overwrite a logical value, whether to overwrite the existing file, default is FALSE
#' @param append a logical value, whether to append the new plot to the existing file, default is TRUE
#' @param ... other parameters passed to `pdf`
#'
#' @return a pdf file
#' @export
#'
#' @examples save2pdf(file = "mytest_plot.pdf", width = 4, height = 4, overwrite = FALSE,
#'                    plot_code = quote({
#'                      t = seq(0, 100, 1)
#'                      plot(cos(t) + t*sin(t), sin(t) - t* cos(t), type = "l")
#'                    }))
save2pdf = function(file=NA, plot_code=NA, overwrite=FALSE, append=TRUE, ...){
  if(is.na(file)){stop("please specify the files to save the plot")}
  if(!is.language(plot_code)){stop("please specify the code to plot")}

  if((!file.exists(file)) | overwrite){
    # create new file with same name
    grDevices::pdf(file = file, ...)
    eval(plot_code)
    grDevices::dev.off()
  }else{
    # create new file with different name
    i = 0
    prefix = gsub(file, pattern=".pdf$", replacement="")
    new_file = paste0(prefix, sprintf("%02d", i), ".pdf")

    while (file.exists(new_file)){
      i = i+1
      new_file = paste0(prefix, sprintf("%02d", i), ".pdf")
    }

    # plot
    grDevices::pdf(file = new_file, ...)
    eval(plot_code)
    grDevices::dev.off()

    # append
    if(append){
      # combine
      combined_pdf = paste0(file, ".combined.pdf")
      qpdf::pdf_combine(c(file, new_file), output = combined_pdf)
      file.rename(combined_pdf, file)
      unlink(new_file)
    }
  }

  #################### USAGE ####################
  # save2pdf(file = "mytest_plot.pdf", width = 6, height = 6, overwrite = FALSE,
  #          plot_code = quote({
  #            t = seq(0, 100, 1)
  #            plot(cos(t) + t*sin(t), sin(t) - t* cos(t), type = "l", asp = 1)
  #          }))
}



