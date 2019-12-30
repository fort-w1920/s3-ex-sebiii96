### greate bb user-interface ###


### the constructor
new_bb <- function(text) {
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  output <- structure(
    gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text), 
    class = "bb"
  )
}


### this is just a placeholder for an actual validator
validate_bb <- function(text) {
  checkmate::assert_true(
    class(text)[1] == "bb"
  )
  # some hypothetical checks that the bb-tranformation worked #

  text
}

### the generic
bb <- function(text) {
  UseMethod("bb")
}


### bb.default:
## as the implicit classes of the vec_fail, matrix_fail and list_fail
# do not contain character, factor or list the application of the generic bb
# to those inputs results in an application of the default function.
# Therefore the default returns an error
bb.default <- function(text) {
  stop("invalid inputs.")
}

### bb.character
## vectors, matrices and arrays that consist of characters have the implicit
# classes c("character"), c("matrix", "character") and c(")
bb.character <- function(text) {
  checkmate::assert(
    checkmate::check_character(text)
  )
  text_bb <- new_bb(text)
  #class(text_bb) <- c("bb", sloop::s3_class(text))
  validate_bb(text_bb)
}



### bb.list ###
# here we make use of the fact that we simply apply the bb generic
# to the elements of the lists until we have no list anymore, i.e. we arrive
# at a factor/vector/matrix/array to which we apply the generic bb again
bb.list <- function(text) {
  checkmate::assert(
    checkmate::check_list(text)
  )
  text_bb <- lapply(
    X = text,
    FUN = bb
  )
  # on the one hand the character-elements need to have the class "bb" but
  # also the list itsself must have the correct class 
  class(text_bb) <- c("bb", sloop::s3_class(text))
  text_bb
}

### bb.factor ###
## input: a object with class factor or ordered, but due to method-dispatch
# the bb.factor method is also applied to ordered factors

# idea: in order to change factors we have to apply the new_bb function to the
# levels of the factors and change the class to c("bb", "factor") for factors
# and c("bb", "ordered", "factor") for ordered factors 


bb.factor <- function(text) {
  checkmate::assert(
    checkmate::check_factor(text, empty.levels.ok = FALSE)
  )
  
  # apply the new_bb 
  levels <- attr(text, "levels")
  levels_bb <- new_bb(levels)
  attributes(levels_bb) <- NULL # the constructor new_bb changes the class
  # to "bb" which we don't want as the levels should have class "character"
   

  underlying_integers <- unclass(text)
  attributes(underlying_integers) <- NULL

  if (is.ordered(text)) {
    new_class <- c("bb", "ordered", "factor")
  } else {
    new_class <- c("bb", "factor")
  }

  text_bb <- structure(
    underlying_integers,
    class = new_class,
    levels = levels_bb
  )
  
  validate_bb(text_bb)
}
