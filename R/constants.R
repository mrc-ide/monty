INDEX <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")

SPECIAL_LHS <- c("dim")

RESERVED_MONTY <- c(
  INDEX, SPECIAL_LHS)

RESERVED_MONTY_PREFIX <- c("dim")
RESERVED_MONTY_PREFIX_RE <- 
  sprintf("^(%s)_.*", paste(RESERVED_MONTY_PREFIX, collapse = "|"))
