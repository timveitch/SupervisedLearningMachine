MarkdownHeader <- function(header, level = 1) {
  paste(paste(rep("#",level),collapse=""), header)
}

RenderToHtml <- function(text, output.file) {
  write(text, output.file)
  
  library(rmarkdown)
  render(output.file,html_document(toc = T))
}

MDRCode <- function(rcode, options) {
  output = c("")
  
  output = append(output, paste("```{r",MDRCodeOptions(options), "}",sep=""))
  output = append(output, rcode)
  output = append(output, "```")
  output = append(output, "")
  output
}

MDRCodeOptions <- function(options) {
  options = c()
  if (!is.null(options$echo)) {
    options = append(options, MDOptionText(options, "echo"))
  }
  if (length(options) > 0) {
    paste(",", paste(options, collapse = ","))
  } else {
    ""
  }
  
}

MDOptionText <- function(options,option.name) {
  paste(option.name, options[[option.name]], sep = "=")
}