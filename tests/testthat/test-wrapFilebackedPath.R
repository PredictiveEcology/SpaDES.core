## `.wrap()`/`.unwrap()` methods take `filebackedPath`; the old `cachePath` still works, with a message.

test_that(".wrap/.unwrap simList methods accept filebackedPath and the deprecated cachePath", {
  skip_if_not_installed("terra")
  testInit("terra", smcc = FALSE)

  sim <- simInit(objects = list(a = 1:3), paths = list(outputPath = tmpdir))

  expect_no_message(w <- .wrap(sim, filebackedPath = tmpdir))
  expect_no_message(u <- .unwrap(w, filebackedPath = tmpdir))
  expect_identical(u$a, 1:3)

  expect_message(w2 <- .wrap(sim, cachePath = tmpdir), "filebackedPath")
  expect_message(u2 <- .unwrap(w2, cachePath = tmpdir), "filebackedPath")
  expect_identical(u2$a, 1:3)
})
