# save a plot to a PDF file

This function saves a plot to a PDF file. It can create a new PDF or
append the plot to an existing file. If the file already exists and
`overwrite` is set to `FALSE`, a new uniquely named file is created.

## Usage

``` r
save2pdf(file = NA, plot_code = NA, overwrite = FALSE, append = TRUE, ...)
```

## Arguments

- file:

  A character string specifying the file path for the PDF.

- plot_code:

  A language object representing the plot code to be executed.
  Typically, use `quote` to wrap the plot code.

- overwrite:

  Logical. If `TRUE`, overwrites the existing file. Default is `FALSE`.

- append:

  Logical. If `TRUE`, appends the new plot to the existing file by
  combining the old and new PDFs. Default is `TRUE`.

- ...:

  Additional arguments passed to the `pdf` function, such as `width` or
  `height`.

## Value

A PDF file is created at the specified location.

## Examples

``` r
if (FALSE) { # \dontrun{
# Save a simple plot to a PDF
tmp_file <- tempfile(fileext = ".pdf")
save2pdf(file = tmp_file, width = 4, height = 4, overwrite = FALSE,
         plot_code = quote({
           t = seq(0, 100, 1)
           plot(cos(t) + t*sin(t), sin(t) - t*cos(t), type = "l")
         }))
} # }
```
