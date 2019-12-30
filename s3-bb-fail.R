## test data for which bb() should fail:
fail_vec <- 1:8
fail_matrix <- matrix(fail_vec, nrow = 2, ncol = 4)
fail_list <- list("bbficate this", fail_vec)

## failing tests:
bb(fail_vec)
bb(fail_matrix)
bb(fail_list)

