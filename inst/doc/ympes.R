library(ympes)

plot_palette(c("#5FE756", "red", "black"))

plot_palette(palette.colors(palette = "R4"), square = TRUE)

dat <- data.frame(
    first = letters,
    second = factor(rev(LETTERS)),
    third = "Q"
)
greprows(dat, "A|b")

grepvrows(dat, "A|b")
greprows(dat,  "A|b", value = TRUE)

greplrows(dat, "A|b", ignore.case = TRUE)

movements <- function(length) {
    x <- lapply(
        list(c("Bob", "Mary", "Rose"), c("Up", "Down", "Right", "Left"), 1:10),
        sample,
        size = length,
        replace = TRUE
    )
    do.call(paste, c(x, sep = "-"))
}

# just a small sample to begin with
(dat <- movements(3))
pattern <- "([[:alpha:]]+)-([[:alpha:]]+)-([[:digit:]]+)"
proto   <- data.frame(Name = "", Direction = "", Value = 1L)
strcapture(pattern, dat, proto = proto, perl = TRUE)

# Now a larger number of strings
dat <- movements(1e5)
(t  <- system.time(r <- strcapture(pattern, dat, proto = proto, perl = TRUE)))
(t2 <- system.time(r2 <- fstrcapture(dat, pattern, proto = proto)))
t[["elapsed"]] / t2[["elapsed"]]

cc(dale, audrey, laura, hawk)
cc("dale audrey laura hawk")

new_name(mtcars)
new_name(mtcars, 3L)

# Use in a user facing function
fun <- function(i, d, l, chr, b) {
    assert_scalar_int(i)
    TRUE
}
fun(i=1L)
try(fun(i="cat"))

# Use in an internal function
internal_fun <- function(a) {
    assert_string(
        a,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = "example_error"
    )
    TRUE
}
external_fun <- function(b) {
    internal_fun(a=b)
}

external_fun(b="cat")
try(external_fun(b = letters))
tryCatch(external_fun(b = letters), error = class)

