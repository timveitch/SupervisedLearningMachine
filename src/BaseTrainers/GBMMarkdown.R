
GBMMarkdownText <- function(output,level = 1) {
  output = MarkdownHeader("GBMOutput",level)
  #plot(cv.error)
  #output = append(output, GBMVariableImportance(output, level+1))
  
  #output = append(output, GlmnetCVText(output,level+1))
  #output = append(output, GlmnetPathText(output,level+1))
  output
}