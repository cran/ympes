expect_identical(
    cc(  dale, audrey, laura  , hawk, ),
    c("dale", "audrey", "laura", "hawk", "")
)

expect_identical(
    cc("  dale audrey laura   hawk "),
    c("dale", "audrey", "laura", "hawk")
)

