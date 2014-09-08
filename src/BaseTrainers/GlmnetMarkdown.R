
GlmnetMarkdownText <- function(output,level = 1) {
  output = MarkdownHeader("GlmnetOutput",level)
  output = append(output, GlmnetCoeffsText(output, level+1))
  output = append(output, GlmnetCVText(output,level+1))
  output = append(output, GlmnetPathText(output,level+1))
  output
}

GlmnetCoeffsText <- function(output, level) {
  output = MarkdownHeader("Non-zero coefficients", level)
  
  rcode = "nonzero = output$coefficients[output$coefficients != 0]"
  rcode = append(rcode, "length(nonzero)")
  rcode = append(rcode, "nonzero")
  rcode = append(rcode, "barplot(nonzero)")
  
  output = append(output, MDRCode(rcode))
  output = append(output, MarkdownHeader("Zero coefficients", level))
  
  rcode = "zero = output$coefficients[output$coefficients == 0]"
  rcode = append(rcode, "length(zero)")
  rcode = append(rcode, "names(zero)")
  
  output = append(output, MDRCode(rcode))
  output
}

GlmnetCVText <- function(output, level) {
  output = MarkdownHeader("Cross validation Error Vs Lambda",level)
  output = append(output, MDRCode("plot(output)", list(echo=FALSE)))
  output
}

GlmnetPathText <- function(output, level) {
  output = MarkdownHeader("Coefficient Profiles",level)
  output = append(output, MDRCode("plot(output$glmnet.fit,label = T)", list(echo = FALSE)))
  output
}