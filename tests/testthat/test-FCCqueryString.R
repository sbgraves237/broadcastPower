test_that("FCCqueryString", {
  FCCstr1 <- FCCqueryString()
  expect_equal(class(FCCstr1), 'character')
  
  FCCstr2 <- FCCqueryString(dlat2=38L,
                            mlat2=54L, slat2=12+1e-9, NS='N',
                            dlon2=77, mlon2=0, slon2=36+1e-9, EW='W')
  expect_equal(class(FCCstr2), 'character')
})
