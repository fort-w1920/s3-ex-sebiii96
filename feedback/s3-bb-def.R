# constructor:
new_bb <- function(text) {
  checkmate::assert_character(text)
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  structure(
    gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text),
    class = c("bb", class(text))
  )
}
# validator:
validate_bb <- function(object) {
  checkmate::assert_class(object, "bb")
  # recursive validation for lists:
  if (is.list(object)) {
    rapply(object, validate_bb)
    return(object)
  }
  # check levels for factors, for all others get rid of dimension attributes
  data <- if (inherits(object, "factor")) {
    levels(object)
  } else {
    as.vector(object)
  }
  checkmate::assert_character(data)
  # chebeck vabalibid beebee-labanguabuagebe (very superficially):
  # regexp: some consonants - some vowels - "b" - same vowels as before the "b"
  match <- "([^aeiouäöüAEIOUÄÜÖ ]*)([aeiouäöü]{1,2})b\\2"
  looks_like_bb <- grepl(pattern = match, x = object)
  if (!all(looks_like_bb)) {
    stop("thebese enbentriebies dobon't loobook libikebe bb-labanguabuagebe: \n",
         object[!looks_like_bb])
  }
  object
}

# Turn character inputs <x> into bb-fied objects -- i.e, objects with an
# additional class entry "bb" in addition to its original class and all
# strings in the object turned into beebee-labanguabuagebe.
bb <- function(x, ...) UseMethod("bb")
# the constructor is definded as a generic, and we define methods for the generic
# in order to convert different data types to class "bb":

# default works for character vectors/matrices/arrays, else throws error
bb.default <- function(x) {
  if (mode(x) != "character") {
    stop("nobon-chabarabacteber obobjebects cabannobot bebe tuburnebed ibintobo bb-obobjebects!\n")
  }
  validate_bb(new_bb(x))
}

# also works for ordered (subclass of factor)
bb.factor <- function(x) {
  levels(x) <- unclass(bb.default(levels(x)))
  class(x) <- c("bb", class(x))
  x
}

bb.list <- function(x) {
  result <- lapply(x, bb)
  class(result) <- c("bb", class(result))
  result
}
