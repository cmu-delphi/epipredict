test_that("frosting validators / constructors work", {
  wf <- epi_workflow()
  expect_s3_class(new_frosting(), "frosting")
  expect_true(is_frosting(new_frosting()))
  expect_silent(epi_workflow(postprocessor = new_frosting()))
  expect_false(has_postprocessor(wf))
  expect_false(has_postprocessor_frosting(wf))
  expect_silent(wf %>% add_frosting(new_frosting()))
  expect_silent(wf %>% add_postprocessor(new_frosting()))
  expect_error(wf %>% add_postprocessor(list()))

  wf <- wf %>% add_frosting(new_frosting())
  expect_true(has_postprocessor(wf))
  expect_true(has_postprocessor_frosting(wf))
})


test_that("prediction works without any postprocessor", {

})
