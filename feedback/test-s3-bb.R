## test data for bb():
texttest <- "Bedeutet nach jedem Vokal oder Diphtong die Konsonanten..."
test_vec <- strsplit(texttest, " ")[[1]]
test_matrix <- matrix(test_vec, nrow = 2, ncol = 4, byrow = TRUE)
test_array <- array(test_vec, dim = c(2, 2, 2))
test_list <- as.list(test_vec)
test_listoflists <- list(as.list(test_vec), list(test_vec))
test_factor <- factor(test_vec)
test_ordered <- ordered(test_vec)

str(bb(test_vec))
str(bb(test_matrix))
str(bb(test_list))
str(bb(test_listoflists))
str(bb(test_factor))
str(bb(test_ordered))

#-------------------------------------------------------------------------------
# tests:
library(testthat)

expect_bb <- function(x, value) {
  expect_is(x, "bb")
  expect_equivalent(unlist(unclass(x)), value)
}
bb_texttest <-
  "Bebedeubeutebet nabach jebedebem Vobokabal obodeber Dibiphtobong diebie Kobonsobonabanteben..."
bb_testvec <- strsplit(bb_texttest, " ")[[1]]

expect_bb(bb(texttest),
          bb_texttest)

expect_bb(bb(test_vec),
          bb_testvec)

expect_bb(bb(test_matrix),
          matrix(bb_testvec,
                 nrow = 2, ncol = 4, byrow = TRUE))
expect_bb(bb(test_array),
          array(bb_testvec,
                dim = c(2, 2, 2)))

expect_bb(bb(test_list),
          bb_testvec)
sapply(bb(test_list), expect_is, class = "bb") -> null

expect_bb(bb(test_listoflists),
          c(bb_testvec, bb_testvec))
rapply(bb(test_listoflists), expect_is, class = "bb", classes = "ANY") -> null
expect_length(bb(test_listoflists), 2)


expect_equivalent(levels(bb(test_factor)),
                  sort(bb_testvec)[c(1, 3, 2, 4:8)]) #alphabetic order changes!
expect_is(bb(test_factor), "bb")
expect_is(bb(test_factor), "factor")

expect_equivalent(levels(bb(test_ordered)),
                  sort(bb_testvec)[c(1, 3, 2, 4:8)]) #alphabetic order changes!
expect_is(bb(test_ordered), "bb")
expect_is(bb(test_ordered), "ordered")
