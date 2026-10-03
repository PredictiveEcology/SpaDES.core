## `.wrap()`/`.unwrap()` methods take `filebackedPath`; the old `cachePath` still works, silently for now.

test_that(".wrap/.unwrap simList methods accept filebackedPath and the deprecated cachePath", {
  skip_if_not_installed("terra")
  testInit("terra", smcc = FALSE)

  sim <- simInit(objects = list(a = 1:3), paths = list(outputPath = tmpdir))

  expect_no_message(w <- .wrap(sim, filebackedPath = tmpdir))
  expect_no_message(u <- .unwrap(w, filebackedPath = tmpdir))
  expect_identical(u$a, 1:3)

  expect_no_message(w2 <- .wrap(sim, cachePath = tmpdir)) # old name accepted silently for now (reproducible#629)
  expect_no_message(u2 <- .unwrap(w2, cachePath = tmpdir))
  expect_identical(u2$a, 1:3)
})
