#' save a plot to a PDF file
#'
#' This function saves a plot to a PDF file. It can create a new PDF or append the plot to an existing file. If the file already exists and `overwrite` is set to `FALSE`, a new uniquely named file is created.
#'
#' @param file A character string specifying the file path for the PDF.
#' @param plot_code A language object representing the plot code to be executed. Typically, use `quote` to wrap the plot code.
#' @param overwrite Logical. If `TRUE`, overwrites the existing file. Default is `FALSE`.
#' @param append Logical. If `TRUE`, appends the new plot to the existing file by combining the old and new PDFs. Default is `TRUE`.
#' @param ... Additional arguments passed to the `pdf` function, such as `width` or `height`.
#'
#' @return A PDF file is created at the specified location.
#' @export
#'
#' @importFrom grDevices pdf dev.off
#' @importFrom qpdf pdf_combine
#'
#' @examples
#' \dontrun{
#' # Save a simple plot to a PDF
#' tmp_file <- tempfile(fileext = ".pdf")
#' save2pdf(file = tmp_file, width = 4, height = 4, overwrite = FALSE,
#'          plot_code = quote({
#'            t = seq(0, 100, 1)
#'            plot(cos(t) + t*sin(t), sin(t) - t*cos(t), type = "l")
#'          }))
#' }
save2pdf = function(file=NA, plot_code=NA, overwrite=FALSE, append=TRUE, ...){
  if(is.na(file)){stop("please specify the files to save the plot")}
  if(!is.language(plot_code)){stop("please specify the code to plot")}

  # Create directory if it doesn't exist
  dir_path <- dirname(file)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

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
}

