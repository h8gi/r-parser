parseFile <- function(file) {
  exp <- parse(file)
  lapply(exp, parseStatement)
}

parseString <- function(str) {
  exp <- parse(text = str)
  lapply(exp, parseStatement)
}

parseStatement <- function(lang, inner = FALSE) {
  ## call
  if (is.call(lang)) {
    op <- operator(lang)
    if (inner) cat(" ")             # print
    cat("(")                        # print
    printOperator(op)               # print

    result <- append(op,
                     lapply(operands(lang),
                            function(opds){parseStatement(opds, TRUE)}))
    cat(")")                        # print
    if (!inner) cat(" \n")          # print
    result
  } else if (is.pairlist(lang)){
    cat("pairlist")
    lang
  } else {
    ## atomic
    printOperand(lang)              # print
    lang
  }
}

operator <- function(call) {
  call[[1]]
}

operands <- function(call) {
  call[2:length(call)]
}

printOperator <- function(op) {
  if (isGrammarSymbol(op)) cat("syntax:")
  cat( paste("\"", op, "\"", sep = "") )
}

printOperand <- function(opd, inner = FALSE) {
  cat(" ")
  cat(opd)
}

printPairlist <- function(pl) {
  
}
