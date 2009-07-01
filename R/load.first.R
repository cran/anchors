.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "anchors"))
  ver <- packageDescription("anchors", lib = mylib)$Version
  builddate <- packageDescription("anchors", lib = mylib)$Date
  cat("\n##  anchors (Version ", ver,", Build Date: ", builddate, ")\n",sep="")
  cat("##  See http://wand.stanford.edu/anchors for additional documentation and support.\n\n", sep="")
#  cat("## Contact Jonathan Wand <wand(at)stanford.edu> with comments about anchors.\n", sep="")
#  cat("##\n", sep="")
}
