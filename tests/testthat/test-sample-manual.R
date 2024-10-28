test_that("can sample manually", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, n_chains = 2)

  set.seed(1)
  expect_equal(
    monty_sample_manual_prepare(model, sampler, 100, path, n_chains = 2),
    path)

  r1 <- monty_sample_manual_run(1, path)
  r2 <- monty_sample_manual_run(2, path)

  res2 <- monty_sample_manual_collect(path)

  expect_true(is_directory(path))
  expect_true(file.exists(r1))
  expect_true(file.exists(r2))

  expect_equal(res2, res1)

  monty_sample_manual_cleanup(path)
  expect_false(file.exists(path))
})


test_that("can continue a manually sampled chain", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1a <- monty_sample(model, sampler, 100, n_chains = 2, restartable = TRUE)
  res1b <- monty_sample_continue(res1a, 50)

  set.seed(1)
  path_a <- withr::local_tempdir()
  monty_sample_manual_prepare(model, sampler, 100, path_a, n_chains = 2)
  monty_sample_manual_run(1, path_a)
  monty_sample_manual_run(2, path_a)
  res2a <- monty_sample_manual_collect(path_a, restartable = TRUE)
  expect_equal(res1a$pars, res2a$pars)

  path_b <- withr::local_tempdir()
  monty_sample_manual_prepare_continue(res2a, 50, path_b)
  monty_sample_manual_run(1, path_b)
  monty_sample_manual_run(2, path_b)
  res2b <- monty_sample_manual_collect(path_b, res2a)

  expect_equal(res2b$pars, res1b$pars)
})


test_that("path used for manual sampling must be empty", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "other"))
  expect_error(
    sample_manual_path_create(tmp, list()),
    "Can't use")
})


test_that("path used for manual sampling must have inputs", {
  tmp <- withr::local_tempdir()
  expect_error(
    sample_manual_path(tmp),
    "Did not find 'inputs.rds' within")
})


test_that("Requested chain id must be in range", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  monty_sample_manual_prepare(model, sampler, 100, path, n_chains = 2)
  expect_error(monty_sample_manual_run(3, path),
               "'chain_id' must be an integer in 1..2", fixed = TRUE)
})


test_that("can't collect results before they are complete", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  monty_sample_manual_prepare(model, sampler, 100, path, n_chains = 2)
  expect_error(monty_sample_manual_collect(path),
               "Results missing for chains 1 and 2", fixed = TRUE)
  monty_sample_manual_run(1, path)
  expect_error(monty_sample_manual_collect(path),
               "Results missing for chain 2", fixed = TRUE)
  monty_sample_manual_run(2, path)
  expect_s3_class(monty_sample_manual_collect(path), "monty_samples")
})


test_that("don't delete working directory if other files have been written", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  monty_sample_manual_prepare(model, sampler, 100, path, n_chains = 2)
  file.create(file.path(path, "other"))
  expect_message(
    monty_sample_manual_cleanup(path),
    "Not deleting")
  expect_true(file.exists(path))
  expect_equal(dir(path), "other")
})


test_that("can print information about a manual sample", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  monty_sample_manual_prepare(model, sampler, 100, path, n_chains = 2)
  expect_snapshot(
    monty_sample_manual_info(path),
    transform = scrub_manual_info)
})


test_that("can print information about chain completeness", {
  expect_message(sample_manual_info_chain(c(TRUE, TRUE)),
                 "All chains complete")
  expect_message(sample_manual_info_chain(c(FALSE, FALSE, FALSE)),
                 "No chains complete")
  expect_message(sample_manual_info_chain(c(FALSE, FALSE, FALSE)),
                 "No chains complete")

  res <- evaluate_promise(sample_manual_info_chain(c(TRUE, FALSE, FALSE)))
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "1 chain complete")
  expect_match(res$messages[[2]], "2 chains pending")
})


test_that("...", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  samples <- monty_sample(model, sampler, 100, n_chains = 2, restartable = TRUE)
  hash <- rlang::hash(samples)

  expect_equal(sample_manual_prepare_check_samples(samples, "value"),
               list(value = samples, hash = NULL))
  expect_equal(sample_manual_prepare_check_samples(samples, "hash"),
               list(value = NULL, hash = hash))
  expect_equal(sample_manual_prepare_check_samples(samples, "nothing"),
               list(value = NULL, hash = NULL))

  samples_fake <- list(restart = samples$restart)
  expect_equal(sample_manual_prepare_check_samples(samples_fake, "nothing"),
               list(value = NULL, hash = NULL))

  expect_error(sample_manual_prepare_check_samples(NULL, "nothing"),
               "Expected 'samples$restart' to be a list",
               fixed = TRUE)

  samples_err <- monty_sample(model, sampler, 100, n_chains = 2)
  expect_error(sample_manual_prepare_check_samples(samples_err, "hash"),
               "Your chains are not restartable",
               fixed = TRUE)
  expect_error(sample_manual_prepare_check_samples(NULL, "hash"),
               "Expected 'samples' to be a 'monty_samples' object",
               fixed = TRUE)
})


test_that("can validate samples on collect for non-restarts", {
  inputs <- NULL
  expect_null(sample_manual_collect_check_samples(inputs, NULL))
  expect_error(
    sample_manual_collect_check_samples(inputs, list()),
    "'samples' provided, but this was not a restarted sample")
})


test_that("continuation without passing samples allowed if samples saved", {
  inputs <- list(restart = list(1), samples = list(value = 2))
  expect_equal(sample_manual_collect_check_samples(inputs, NULL), 2)
})


test_that("continuation without passing samples not allowed otherwise", {
  inputs <- list(restart = list(1))
  expect_error(
    sample_manual_collect_check_samples(inputs, NULL),
    "Expected 'samples' to be provided, as this chain is a continuation")
})


test_that("samples, if provided, must match", {
  path <- withr::local_tempdir()
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  samples1 <- monty_sample(model, sampler, 10, n_chains = 2, restartable = TRUE)
  samples2 <- monty_sample(model, sampler, 10, n_chains = 2, restartable = TRUE)
  hash1 <- rlang::hash(samples1)
  hash2 <- rlang::hash(samples2)

  expect_equal(
    sample_manual_collect_check_samples(
      list(restart = list(), samples = list(value = samples1, hash = NULL)),
      samples1),
    samples1)
  expect_equal(
    sample_manual_collect_check_samples(
      list(restart = list(), samples = list(value = NULL, hash = hash1)),
      samples1),
    samples1)

  expect_error(
    sample_manual_collect_check_samples(
      list(restart = list(), samples = list(value = samples1, hash = NULL)),
      samples2),
    "Provided 'samples' does not match those at the start of the chain")
  expect_error(
    sample_manual_collect_check_samples(
      list(restart = list(), samples = list(value = NULL, hash = hash1)),
      samples2),
    "Provided 'samples' does not match those at the start of the chain")
})


test_that("can use burnin/thinning_factor in manual sampling", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, n_chains = 2,
                       burnin = 20, thinning_factor = 4)

  set.seed(1)
  path_a <- withr::local_tempdir()
  monty_sample_manual_prepare(model, sampler, 60, path_a, n_chains = 2,
                              burnin = 20, thinning_factor = 4)
  monty_sample_manual_run(1, path_a)
  monty_sample_manual_run(2, path_a)
  res2a <- monty_sample_manual_collect(path_a, restartable = TRUE)
  expect_equal(res2a$restart$thinning_factor, 4)

  path_b <- withr::local_tempdir()
  monty_sample_manual_prepare_continue(res2a, 40, path_b)
  monty_sample_manual_run(1, path_b)
  monty_sample_manual_run(2, path_b)
  res2b <- monty_sample_manual_collect(path_b, res2a)

  expect_equal(res2b$pars, res1$pars)
})
