

# x: named list
# to_remove: named list
named_list_remove = function(x, to_remove) {
  sel = names(x) %in% names(to_remove)
  return(x[!sel])
}


# x: named list
add_s3_class = function(x) {
  y = lapply(x, function(k) {
    class(k) <- c("anS3Class", class(k))
    return(k)
  })

  names(y) <- paste0("anS3Class", names(y))
  return(y)
}


# x: list of characters
init_list = function(x) {
  y = unlist(x)
  z = lapply(x, function(k) {
    return(eval(str2lang(k)))
  })

  names(z) <- y
  return(z)
}


# x: list
unli_wn = function(x) {
  unlist(x, recursive = FALSE, use.names = TRUE)
}


# x: list
unli_won = function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE)
}


################################################################################
##                               fixtures                                     ##
################################################################################

r_ = list(
  `NULL` = NULL,
  `array()` = array(),
  `matrix()` = matrix(),
  `list()` = list(),
  `pairlist(NULL)` = pairlist(NULL),
  `data.frame()` = data.frame(),
  `table(NULL)` = table(NULL),
  `expression()` = expression(),
  `formula()` = formula(),
  `new.env()` = new.env()
)


# ------------------------------------------------------------------------------
r_lgl_vec = list(
  `logical(0L)` = logical(0L),
  `NA` = NA,
  `TRUE` = TRUE,
  `FALSE` = FALSE
)

r_lgl_vec_named = list(
  `structure(logical(0L), names = character(0L))` = structure(logical(0L), names = character(0L)),
  `c("NA" = NA)` = c("NA" = NA),
  `c("TRUE" = TRUE)` = c("TRUE" = TRUE),
  `c("FALSE" = FALSE)` = c("FALSE" = FALSE)
)

r_lgl_vec_wattr = list(
  `structure(logical(0L), foo = "logical(0L)")` = structure(logical(0L), foo = "logical(0L)"),
  `structure(NA, foo = "NA")` = structure(NA, foo = "NA"),
  `structure(TRUE, foo = "TRUE")` = structure(TRUE, foo = "TRUE"),
  `structure(FALSE, foo = "FALSE")` = structure(FALSE, foo = "FALSE")
)

r_lgl_vec_named_wattr = list(
  `structure(logical(0L), names = character(0L), foo = "logical(0L)")` = structure(logical(0L), names = character(0L), foo = "logical(0L)"),
  `structure(NA, names = "NA", foo = "NA")` = structure(NA, names = "NA", foo = "NA"),
  `structure(TRUE, names = "TRUE", foo = "TRUE")` = structure(TRUE, names = "TRUE", foo = "TRUE"),
  `structure(FALSE, names  = "FALSE", foo = "FALSE")` = structure(FALSE, names  = "FALSE", foo = "FALSE")
)

r_lgl_vec_ = c(r_lgl_vec, r_lgl_vec_named, r_lgl_vec_wattr,
               r_lgl_vec_named_wattr)

r_lgl_vec_ = c(r_lgl_vec_, add_s3_class(r_lgl_vec_))

fxt_lgl_vec_oan  = unli_won(r_lgl_vec)
fxt_lgl_vec_oanx = unli_won(r_lgl_vec[-2L])
fxt_lgl_vec_oan0 = r_lgl_vec[[1L]]
fxt_lgl_vec_oa   = unli_wn(r_lgl_vec_named)
fxt_lgl_vec_oax  = unli_wn(r_lgl_vec_named[-2L])
fxt_lgl_vec_oa0  = r_lgl_vec_named[[1L]]


# ------------------------------------------------------------------------------
r_int_vec = list(
  `integer(0L)` = integer(0L),
  `NA_integer_` = NA_integer_,
  `0L` = 0L
)

r_int_vec_named = list(
  `structure(integer(0L), names = character(0L))` = structure(integer(0L), names = character(0L)),
  `c("NA_integer_" = NA_integer_)` = c("NA_integer_" = NA_integer_),
  `c("0L" = 0L)` = c("0L" = 0L)
)

r_int_vec_wattr = list(
  `structure(integer(0L), foo = "integer(0L)")` = structure(integer(0L), foo = "integer(0L)"),
  `structure(NA_integer_, foo = "NA_integer_")` = structure(NA_integer_, foo = "NA_integer_"),
  `structure(0L, foo = "0L")` = structure(0L, foo = "0L")
)

r_int_vec_named_wattr = list(
  `structure(integer(0L), names = character(0L), foo = "integer(0L)")` = structure(integer(0L), names = character(0L), foo = "integer(0L)"),
  `structure(NA_integer_, names = "NA_integer_", foo = "NA_integer_")` = structure(NA_integer_, names = "NA_integer_", foo = "NA_integer_"),
  `structure(0L, names = "0L", foo = "0L")` = structure(0L, names = "0L", foo = "0L")
)

r_int_vec_ = c(r_int_vec, r_int_vec_named, r_int_vec_wattr,
               r_int_vec_named_wattr)

r_int_vec_ = c(r_int_vec_, add_s3_class(r_int_vec_))

fxt_int_vec_oan = unli_won(r_int_vec)
fxt_int_vec_oanx = unli_won(r_int_vec[-2L])
fxt_int_vec_oan0 = r_int_vec[[1L]]
fxt_int_vec_oa = unli_wn(r_int_vec_named)
fxt_int_vec_oax = unli_wn(r_int_vec_named[-2L])
fxt_int_vec_oa0 = r_int_vec_named[[1L]]


# ------------------------------------------------------------------------------
r_dbl_vec =  list(
  `double(0L)` = double(0L),
  `NA_real_` = NA_real_,
  `NaN` = NaN,
  `Inf` = Inf,
  `-Inf` = -Inf,
  `0.` = 0.
)

r_dbl_vec_named = list(
  `structure(double(0L), names = character(0L))` = structure(double(0L), names = character(0L)),
  `c("NA_real_" = NA_real_)` = c("NA_real_" = NA_real_),
  `c("NaN" = NaN)` = c("NaN" = NaN),
  `c("Inf" = Inf)` = c("Inf" = Inf),
  `c("-Inf" = -Inf)` = c("-Inf" = -Inf),
  `c("0." = 0.)` = c("0." = 0.)
)

r_dbl_vec_wattr = list(
  `structure(double(0L), foo = "bar")` = structure(double(0L), foo = "bar"),
  `structure(NA_real_, foo = "bar")` = structure(NA_real_, foo = "bar"),
  `structure(NaN, foo = "bar")` = structure(NaN, foo = "bar"),
  `structure(Inf, foo = "bar")` = structure(Inf, foo = "bar"),
  `structure(-Inf, foo = "bar")` = structure(-Inf, foo = "bar"),
  `structure(0., foo = "bar")` = structure(0., foo = "bar")
)

r_dbl_vec_named_wattr = list(
  `structure(double(0L), names = character(0L), foo = "double(0L)")` = structure(double(0L), names = character(0L), foo = "double(0L)"),
  `structure(NA_real_, names = "NA_real_", foo = "bar")` = structure(NA_real_, names = "NA_real_", foo = "bar"),
  `structure(NaN, names = "NaN", foo = "bar")` = structure(NaN, names = "NaN", foo = "bar"),
  `structure(Inf, names = "Inf", foo = "bar")` = structure(Inf, names = "Inf", foo = "bar"),
  `structure(-Inf, names = "-Inf", foo = "bar")` = structure(-Inf, names = "-Inf", foo = "bar"),
  `structure(0., names = "0.", foo = "bar")` = structure(0., names = "0.", foo = "bar")
)

r_dbl_vec_ = c(r_dbl_vec, r_dbl_vec_named, r_dbl_vec_wattr,
               r_dbl_vec_named_wattr)

r_dbl_vec_ = c(r_dbl_vec_, add_s3_class(r_dbl_vec_))

fxt_dbl_vec_oan = unli_won(r_dbl_vec)
fxt_dbl_vec_oanx = unli_won(r_dbl_vec[-2L])
fxt_dbl_vec_oan0 = r_dbl_vec[[1L]]
fxt_dbl_vec_oa = unli_wn(r_dbl_vec_named)
fxt_dbl_vec_oax = unli_wn(r_dbl_vec_named[-2L])
fxt_dbl_vec_oa0 = r_dbl_vec_named[[1L]]

fxt_dbl_vec_oayn = unli_won(r_dbl_vec[-3L])
fxt_dbl_vec_oay = unli_wn(r_dbl_vec_named[-3L])

fxt_dbl_vec_oazn = unli_won(r_dbl_vec[-(4:5)])
fxt_dbl_vec_oaz = unli_wn(r_dbl_vec_named[-(4:5)])

fxt_dbl_vec_oanxy = unli_won(r_dbl_vec[-(2:3)])
fxt_dbl_vec_oaxy = unli_wn(r_dbl_vec_named[-(2:3)])

fxt_dbl_vec_oanyz = unli_won(r_dbl_vec[-(3:5)])
fxt_dbl_vec_oayz = unli_wn(r_dbl_vec_named[-(3:5)])

fxt_dbl_vec_oanxyz = unli_won(r_dbl_vec[-(2:5)])
fxt_dbl_vec_oaxyz = unli_wn(r_dbl_vec_named[-(2:5)])


# ------------------------------------------------------------------------------
r_plx_vec = list(
  'complex(0L)',
  'NA_complex_',
  'complex(real = NaN, imaginary = NaN)',
  'complex(real = NaN, imaginary = Inf)',
  'complex(real = Inf, imaginary = Inf)',
  'complex(real = -Inf, imaginary = -Inf)',
  '0+0i'
)

r_plx_vec_named = list(
  'structure(complex(0L), names = character(0L))',
  'c("NA_complex_" = NA_complex_)',
  'c("complex(real = NaN, imaginary = NaN)" = complex(real = NaN, imaginary = NaN))',
  'c("complex(real = NaN, imaginary = Inf)" = complex(real = NaN, imaginary = Inf))',
  'c("complex(real = Inf, imaginary = Inf)" = complex(real = Inf, imaginary = Inf))',
  'c("complex(real = -Inf, imaginary = -Inf)" = complex(real = -Inf, imaginary = -Inf))',
  'c("0+0i" = 0+0i)'
)

r_plx_vec_wattr = list(
  'structure(complex(0L), foo = "complex(0L)")',
  'structure(NA_complex_, foo = "NA_complex_")',
  'structure(complex(real = NaN, imaginary = NaN), foo = "complex(real = NaN, imaginary = NaN)")',
  'structure(complex(real = NaN, imaginary = Inf), foo = "structure(complex(real = NaN, imaginary = Inf)")',
  'structure(complex(real = Inf, imaginary = Inf), foo = "complex(real = Inf, imaginary = Inf)")',
  'structure(complex(real = -Inf, imaginary = -Inf), foo = "complex(real = -Inf, imaginary = -Inf)")',
  'structure(0+0i, foo = "0+0i")'
)

r_plx_vec_named_wattr = list(
  'structure(complex(0L), names = character(0L), foo = "complex(0L)")',
  'structure(NA_complex_, names = "NA_complex_", foo = "NA_complex_")',
  'structure(complex(real = NaN, imaginary = NaN), names = "complex(real = NaN, imaginary = NaN)", foo = "complex(real = NaN, imaginary = NaN)")',
  'structure(complex(real = NaN, imaginary = Inf), names = "complex(real = NaN, imaginary = Inf)", foo = "structure(complex(real = NaN, imaginary = Inf)")',
  'structure(complex(real = Inf, imaginary = Inf), names = "complex(real = -Inf, imaginary = -Inf)", foo = "complex(real = Inf, imaginary = Inf)")',
  'structure(complex(real = -Inf, imaginary = -Inf), names = "complex(real = -Inf, imaginary = -Inf)", foo = "complex(real = -Inf, imaginary = -Inf)")',
  'structure(0+0i, names = "0+0i", foo = "0+0i")'
)

r_plx_vec = init_list(r_plx_vec)
r_plx_vec_named = init_list(r_plx_vec_named)
r_plx_vec_wattr = init_list(r_plx_vec_wattr)
r_plx_vec_named_wattr = init_list(r_plx_vec_named_wattr)

r_plx_ = c(r_plx_vec, r_plx_vec_named, r_plx_vec_wattr, r_plx_vec_named_wattr)
r_plx_ = c(r_plx_, add_s3_class(r_plx_))


# ------------------------------------------------------------------------------
r_raw_ = list(
  'raw(0L)',
  'raw(1L)',
  'structure(raw(0L), names = character(0L))',
  'c("raw(1L)" = raw(1L))',
  'structure(raw(0L), foo = "raw(0L)")',
  'structure(raw(1L), foo = "raw(1L)")',
  'structure(raw(0L), names = character(0L), foo = "raw(0L)")',
  'structure(raw(1L), names = "raw(1L)", foo = "raw(1L)")'
)

r_raw_ = init_list(r_raw_)
r_raw_ = c(r_raw_, add_s3_class(r_raw_))


# ------------------------------------------------------------------------------
r_chr_vec = list(
  `character(0L)` = character(0L),
  `NA_character_` = NA_character_,
  `""` = "",
  `"foo"` = "foo"
)

r_chr_vec_named = list(
  `structure(character(0L), names = character(0L))` = structure(character(0L), names = character(0L)),
  `c("NA_character_" = NA_character_)` = c("NA_character_" = NA_character_),
  `structure("", names = '""')` = structure("", names = '""'),
  `structure("foo", names = '"foo"')` = structure("foo", names = '"foo"')
)

r_chr_vec_wattr = list(
  `structure(character(0L), foo = "bar")` = structure(character(0L), foo = "bar"),
  `structure(NA_character_, foo = "bar")` = structure(NA_character_, foo = "bar"),
  `structure("", foo = "bar")` = structure("", foo = "bar"),
  `structure("foo", foo = "bar")` = structure("foo", foo = "bar")
)

r_chr_vec_named_wattr = list(
  `structure(character(0L), names = character(0L), foo = "character(0L)")` = structure(character(0L), names = character(0L), foo = "character(0L)"),
  `structure(NA_character_, names = "NA_character_", foo = "bar")` = structure(NA_character_, names = "NA_character_", foo = "bar"),
  `structure("", names = '""', foo = "bar")` = structure("", names = '""', foo = "bar"),
  `structure("foo", names = '"foo"', foo = "bar")` = structure("foo", names = '"foo"', foo = "bar")
)

r_chr_vec_ = c(r_chr_vec, r_chr_vec_named, r_chr_vec_wattr,
               r_chr_vec_named_wattr)

r_chr_vec_ = c(r_chr_vec_, add_s3_class(r_chr_vec_))

fxt_chr_vec_oan = unli_won(r_chr_vec)
fxt_chr_vec_oanx = unli_won(r_chr_vec[-2L])
fxt_chr_vec_oan0 = r_chr_vec[[1L]]
fxt_chr_vec_oa = unli_wn(r_chr_vec_named)
fxt_chr_vec_oax = unli_wn(r_chr_vec_named[-2L])
fxt_chr_vec_oa0 = r_chr_vec_named[[1L]]

fxt_chr_vec_oanb = unli_won(r_chr_vec[-3L])
fxt_chr_vec_oanxb = unli_won(r_chr_vec[-(2:3)])
fxt_chr_vec_oab = unli_wn(r_chr_vec_named[-3L])
fxt_chr_vec_oaxb = unli_wn(r_chr_vec_named[-c(2:3)])


# ------------------------------------------------------------------------------
r_arr_ = lapply(c(r_lgl_vec, r_int_vec, r_dbl_vec, r_chr_vec), function(val) {
  return(do.call(array, list(val)))
})

names(r_arr_) <- paste0("array(", names(r_arr_), ")")


# ------------------------------------------------------------------------------
r_mat_ = lapply(c(r_lgl_vec, r_int_vec, r_dbl_vec, r_chr_vec), function(val) {
  return(do.call(matrix, list(val)))
})

names(r_mat_) <- paste0("matrix(", names(r_mat_), ")")


# ------------------------------------------------------------------------------
r_df_ = lapply(c(r_lgl_vec, r_int_vec, r_dbl_vec, r_chr_vec), function(val) {
  return(do.call(data.frame, list(val)))
})

names(r_df_) <- paste0("data.frame(", names(r_df_), ")")


# ------------------------------------------------------------------------------
r_li_ = lapply(c(r_lgl_vec, r_int_vec, r_dbl_vec, r_chr_vec), function(val) {
  return(do.call(list, list(val)))
})

names(r_li_) <- paste0("list(", names(r_li_), ")")


# ------------------------------------------------------------------------------

r_dict = c(r_, r_lgl_vec_, r_int_vec_, r_dbl_vec_, r_chr_vec_, r_plx_, r_arr_,
           r_mat_, r_df_, r_li_, r_raw_)

expect_identical(length(unique(names(r_dict))), length(r_dict))
expect_false(any(names(r_dict) == "", na.rm = FALSE))
expect_false(anyNA(names(r_dict), recursive = FALSE))

# ------------------------------------------------------------------------------



################################################################################
##                    logical vectors                                         ##
################################################################################

# Test positive cases
expect_identical(is_lgl_vec(fxt_lgl_vec_oan), TRUE)
expect_identical(is_lgl_vec(fxt_lgl_vec_oan0), TRUE)
expect_identical(is_lgl_vec(fxt_lgl_vec_oa), TRUE)
expect_identical(is_lgl_vec(fxt_lgl_vec_oa0), TRUE)

positive = c(r_lgl_vec, r_lgl_vec_named)
for (x in positive) {
  expect_identical(is_lgl_vec(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_lgl_vec(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_lgl_vec_x(fxt_lgl_vec_oanx), TRUE)
expect_identical(is_lgl_vec_x(fxt_lgl_vec_oan0), TRUE)
expect_identical(is_lgl_vec_x(fxt_lgl_vec_oax), TRUE)
expect_identical(is_lgl_vec_x(fxt_lgl_vec_oa0), TRUE)

positive = c(r_lgl_vec[-2L], r_lgl_vec_named[-2L])
for (x in positive) {
  expect_identical(is_lgl_vec_x(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_lgl_vec_x(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_lgl_vec[-(1:2)], r_lgl_vec_named[-(1:2)])
for (x in positive) {
  expect_identical(is_lgl_vec_x1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_lgl_vec_x1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_lgl_vec_n(fxt_lgl_vec_oan), TRUE)
expect_identical(is_lgl_vec_n(fxt_lgl_vec_oan0), TRUE)

positive = r_lgl_vec
for (x in positive) {
  expect_identical(is_lgl_vec_n(x), TRUE)
}

# Test negative cases
expect_identical(is_lgl_vec_n(fxt_lgl_vec_oa), FALSE)
expect_identical(is_lgl_vec_n(fxt_lgl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_lgl_vec_n(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_lgl_vec_nx(fxt_lgl_vec_oanx), TRUE)
expect_identical(is_lgl_vec_nx(fxt_lgl_vec_oan0), TRUE)

positive = r_lgl_vec[-2L]
for (x in positive) {
  expect_identical(is_lgl_vec_nx(x), TRUE)
}

# Test negative cases
expect_identical(is_lgl_vec_nx(fxt_lgl_vec_oax), FALSE)
expect_identical(is_lgl_vec_nx(fxt_lgl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_lgl_vec_nx(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_lgl_vec[-(1:2)]
for (x in positive) {
  expect_identical(is_lgl_vec_nx1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_lgl_vec_nx1(x), FALSE)
}
rm(positive, negative)


################################################################################
##                       integer vector                                       ##
################################################################################

# Test positive cases
expect_identical(is_int_vec(fxt_int_vec_oan), TRUE)
expect_identical(is_int_vec(fxt_int_vec_oan0), TRUE)
expect_identical(is_int_vec(fxt_int_vec_oa), TRUE)
expect_identical(is_int_vec(fxt_int_vec_oa0), TRUE)

positive = c(r_int_vec, r_int_vec_named)
for (x in positive) {
  expect_identical(is_int_vec(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_int_vec(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_int_vec_x(fxt_int_vec_oanx), TRUE)
expect_identical(is_int_vec_x(fxt_int_vec_oan0), TRUE)
expect_identical(is_int_vec_x(fxt_int_vec_oax), TRUE)
expect_identical(is_int_vec_x(fxt_int_vec_oa0), TRUE)

positive = c(r_int_vec[-2L], r_int_vec_named[-2L])
for (x in positive) {
  expect_identical(is_int_vec_x(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_int_vec_x(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_int_vec[-(1:2)], r_int_vec_named[-(1:2)])
for (x in positive) {
  expect_identical(is_int_vec_x1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_int_vec_x1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_int_vec_n(fxt_int_vec_oan), TRUE)
expect_identical(is_int_vec_n(fxt_int_vec_oan0), TRUE)

positive = r_int_vec
for (x in positive) {
  expect_identical(is_int_vec_n(x), TRUE)
}

# Test negative cases
expect_identical(is_int_vec_n(fxt_int_vec_oa), FALSE)
expect_identical(is_int_vec_n(fxt_int_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_int_vec_n(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_int_vec_nx(fxt_int_vec_oanx), TRUE)
expect_identical(is_int_vec_nx(fxt_int_vec_oan0), TRUE)

positive = r_int_vec[-2L]
for (x in positive) {
  expect_identical(is_int_vec_nx(x), TRUE)
}

# Test negative cases
expect_identical(is_int_vec_nx(fxt_int_vec_oax), FALSE)
expect_identical(is_int_vec_nx(fxt_int_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_int_vec_nx(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_int_vec[-(1:2)]
for (x in positive) {
  expect_identical(is_int_vec_nx1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_int_vec_nx1(x), FALSE)
}
rm(positive, negative)


################################################################################
##                     character vector                                       ##
################################################################################

# Test positive cases
expect_identical(is_chr_vec(fxt_chr_vec_oan), TRUE)
expect_identical(is_chr_vec(fxt_chr_vec_oan0), TRUE)
expect_identical(is_chr_vec(fxt_chr_vec_oa), TRUE)
expect_identical(is_chr_vec(fxt_chr_vec_oa0), TRUE)

positive = c(r_chr_vec, r_chr_vec_named)
for (x in positive) {
  expect_identical(is_chr_vec(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_n(fxt_chr_vec_oan), TRUE)
expect_identical(is_chr_vec_n(fxt_chr_vec_oan0), TRUE)

positive = r_chr_vec
for (x in positive) {
  expect_identical(is_chr_vec_n(x), TRUE)
}

# Test negative cases
expect_identical(is_chr_vec_n(fxt_chr_vec_oa), FALSE)
expect_identical(is_chr_vec_n(fxt_chr_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_n(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_x(fxt_chr_vec_oanx), TRUE)
expect_identical(is_chr_vec_x(fxt_chr_vec_oan0), TRUE)
expect_identical(is_chr_vec_x(fxt_chr_vec_oax), TRUE)
expect_identical(is_chr_vec_x(fxt_chr_vec_oa0), TRUE)

positive = c(r_chr_vec[-2L], r_chr_vec_named[-2L])
for (x in positive) {
  expect_identical(is_chr_vec_x(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_x(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_nx(fxt_chr_vec_oanx), TRUE)
expect_identical(is_chr_vec_nx(fxt_chr_vec_oan0), TRUE)

positive = r_chr_vec[-2L]
for (x in positive) {
  expect_identical(is_chr_vec_nx(x), TRUE)
}

# Test negative cases
expect_identical(is_chr_vec_nx(fxt_chr_vec_oax), FALSE)
expect_identical(is_chr_vec_nx(fxt_chr_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_nx(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_b(fxt_chr_vec_oanb), TRUE)
expect_identical(is_chr_vec_b(fxt_chr_vec_oan0), TRUE)
expect_identical(is_chr_vec_b(fxt_chr_vec_oab), TRUE)
expect_identical(is_chr_vec_b(fxt_chr_vec_oa0), TRUE)

positive = c(r_chr_vec[-3L], r_chr_vec_named[-3L])
for (x in positive) {
  expect_identical(is_chr_vec_b(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_b(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_nb(fxt_chr_vec_oanb), TRUE)
expect_identical(is_chr_vec_nb(fxt_chr_vec_oan0), TRUE)

positive = r_chr_vec[-3L]
for (x in positive) {
  expect_identical(is_chr_vec_nb(x), TRUE)
}

# Test negative cases
expect_identical(is_chr_vec_nb(fxt_chr_vec_oab), FALSE)
expect_identical(is_chr_vec_nb(fxt_chr_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_nb(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_xb(fxt_chr_vec_oanxb), TRUE)
expect_identical(is_chr_vec_xb(fxt_chr_vec_oan0), TRUE)
expect_identical(is_chr_vec_xb(fxt_chr_vec_oaxb), TRUE)
expect_identical(is_chr_vec_xb(fxt_chr_vec_oa0), TRUE)

positive = c(r_chr_vec[-(2:3)], r_chr_vec_named[-(2:3)])
for (x in positive) {
  expect_identical(is_chr_vec_xb(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_xb(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_chr_vec_nxb(fxt_chr_vec_oanxb), TRUE)
expect_identical(is_chr_vec_nxb(fxt_chr_vec_oan0), TRUE)

positive = r_chr_vec[-(2:3)]
for (x in positive) {
  expect_identical(is_chr_vec_nxb(x), TRUE)
}

# Test negative cases
expect_identical(is_chr_vec_nxb(fxt_chr_vec_oaxb), FALSE)
expect_identical(is_chr_vec_nxb(fxt_chr_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_nxb(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_chr_vec[-(1:2)], r_chr_vec_named[-(1:2)])
for (x in positive) {
  expect_identical(is_chr_vec_x1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_x1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_chr_vec[-(1:2)]
for (x in positive) {
  expect_identical(is_chr_vec_nx1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_nx1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_chr_vec[-(1:3)], r_chr_vec_named[-(1:3)])
for (x in positive) {
  expect_identical(is_chr_vec_xb1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_xb1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_chr_vec[-(1:3)]
for (x in positive) {
  expect_identical(is_chr_vec_nxb1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_nxb1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_chr_vec[-c(1L, 3L)], r_chr_vec_named[-c(1L, 3L)])
for (x in positive) {
  expect_identical(is_chr_vec_b1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_b1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_chr_vec[-c(1L, 3L)]
for (x in positive) {
  expect_identical(is_chr_vec_nb1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_chr_vec_nb1(x), FALSE)
}
rm(positive, negative)


################################################################################
##                         double vector                                      ##
################################################################################

# Test positive cases
expect_identical(is_dbl_vec(fxt_dbl_vec_oan), TRUE)
expect_identical(is_dbl_vec(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec(fxt_dbl_vec_oa), TRUE)
expect_identical(is_dbl_vec(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec, r_dbl_vec_named)
for (x in positive) {
  expect_identical(is_dbl_vec(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec(fxt_dbl_vec_oan), TRUE)
expect_identical(is_dbl_vec(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec
for (x in positive) {
  expect_identical(is_dbl_vec_n(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_n(fxt_dbl_vec_oa), FALSE)
expect_identical(is_dbl_vec_n(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_n(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_x(fxt_dbl_vec_oanx), TRUE)
expect_identical(is_dbl_vec_x(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec_x(fxt_dbl_vec_oax), TRUE)
expect_identical(is_dbl_vec_x(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-2L], r_dbl_vec_named[-2L])
for (x in positive) {
  expect_identical(is_dbl_vec_x(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_x(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_nx(fxt_dbl_vec_oanx), TRUE)
expect_identical(is_dbl_vec_nx(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec[-2L]
for (x in positive) {
  expect_identical(is_dbl_vec_nx(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_nx(fxt_dbl_vec_oax), FALSE)
expect_identical(is_dbl_vec_nx(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nx(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_z(fxt_dbl_vec_oazn), TRUE)
expect_identical(is_dbl_vec_z(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec_z(fxt_dbl_vec_oaz), TRUE)
expect_identical(is_dbl_vec_z(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(4:5)], r_dbl_vec_named[-(4:5)])
for (x in positive) {
  expect_identical(is_dbl_vec_z(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_z(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_nz(fxt_dbl_vec_oazn), TRUE)
expect_identical(is_dbl_vec_nz(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec[-(4:5)]
for (x in positive) {
  expect_identical(is_dbl_vec_nz(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_nz(fxt_dbl_vec_oaz), FALSE)
expect_identical(is_dbl_vec_nz(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:2)], r_dbl_vec_named[-(1:2)])
for (x in positive) {
  expect_identical(is_dbl_vec_x1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_x1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_dbl_vec[-(1:2)]
for (x in positive) {
  expect_identical(is_dbl_vec_nx1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nx1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-c(1L, 3L)], r_dbl_vec_named[-c(1L, 3L)])
for (x in positive) {
  expect_identical(is_dbl_vec_y1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_y1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_dbl_vec[-c(1L, 3L)]
for (x in positive) {
  expect_identical(is_dbl_vec_ny1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_ny1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_y(fxt_dbl_vec_oayn), TRUE)
expect_identical(is_dbl_vec_y(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec_y(fxt_dbl_vec_oay), TRUE)
expect_identical(is_dbl_vec_y(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-3L], r_dbl_vec_named[-3L])
for (x in positive) {
  expect_identical(is_dbl_vec_y(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_y(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_ny(fxt_dbl_vec_oayn), TRUE)
expect_identical(is_dbl_vec_ny(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec[-3L]
for (x in positive) {
  expect_identical(is_dbl_vec_ny(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_ny(fxt_dbl_vec_oay), FALSE)
expect_identical(is_dbl_vec_ny(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_ny(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_xy(fxt_dbl_vec_oanxy), TRUE)
expect_identical(is_dbl_vec_xy(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec_xy(fxt_dbl_vec_oaxy), TRUE)
expect_identical(is_dbl_vec_xy(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(2:3)], r_dbl_vec_named[-(2:3)])
for (x in positive) {
  expect_identical(is_dbl_vec_xy(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_xy(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_nxy(fxt_dbl_vec_oanxy), TRUE)
expect_identical(is_dbl_vec_nxy(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec[-(2:3)]
for (x in positive) {
  expect_identical(is_dbl_vec_nxy(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_nxy(fxt_dbl_vec_oaxy), FALSE)
expect_identical(is_dbl_vec_nxy(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nxy(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_yz(fxt_dbl_vec_oanyz), TRUE)
expect_identical(is_dbl_vec_yz(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec_yz(fxt_dbl_vec_oayz), TRUE)
expect_identical(is_dbl_vec_yz(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(3:5)], r_dbl_vec_named[-(3:5)])
for (x in positive) {
  expect_identical(is_dbl_vec_yz(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_yz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_nyz(fxt_dbl_vec_oanyz), TRUE)
expect_identical(is_dbl_vec_nyz(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec[-(3:5)]
for (x in positive) {
  expect_identical(is_dbl_vec_nyz(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_nyz(fxt_dbl_vec_oayz), FALSE)
expect_identical(is_dbl_vec_nyz(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nyz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_xyz(fxt_dbl_vec_oanxyz), TRUE)
expect_identical(is_dbl_vec_xyz(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_dbl_vec_xyz(fxt_dbl_vec_oaxyz), TRUE)
expect_identical(is_dbl_vec_xyz(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(2:5)], r_dbl_vec_named[-(2:5)])
for (x in positive) {
  expect_identical(is_dbl_vec_xyz(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_xyz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
expect_identical(is_dbl_vec_nxyz(fxt_dbl_vec_oanxyz), TRUE)
expect_identical(is_dbl_vec_nxyz(fxt_dbl_vec_oan0), TRUE)

positive = r_dbl_vec[-(2:5)]
for (x in positive) {
  expect_identical(is_dbl_vec_nxyz(x), TRUE)
}

# Test negative cases
expect_identical(is_dbl_vec_nxyz(fxt_dbl_vec_oaxyz), FALSE)
expect_identical(is_dbl_vec_nxyz(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nxyz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:3)], r_dbl_vec_named[-(1:3)])
for (x in positive) {
  expect_identical(is_dbl_vec_xy1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_xy1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_dbl_vec[-(1:3)]
for (x in positive) {
  expect_identical(is_dbl_vec_nxy1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nxy1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:5)], r_dbl_vec_named[-(1:5)])
for (x in positive) {
  expect_identical(is_dbl_vec_xyz1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_xyz1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = r_dbl_vec[-(1:5)]
for (x in positive) {
  expect_identical(is_dbl_vec_nxyz1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_dbl_vec_nxyz1(x), FALSE)
}
rm(positive, negative)


################################################################################
##                         numeric vector                                     ##
################################################################################

# Test positive cases
# int
expect_identical(is_num_vec(fxt_int_vec_oan), TRUE)
expect_identical(is_num_vec(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec(fxt_int_vec_oa), TRUE)
expect_identical(is_num_vec(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec(fxt_dbl_vec_oan), TRUE)
expect_identical(is_num_vec(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec(fxt_dbl_vec_oa), TRUE)
expect_identical(is_num_vec(fxt_dbl_vec_oa0), TRUE)

positive = c(r_int_vec, r_int_vec_named, r_dbl_vec, r_dbl_vec_named)
for (x in positive) {
  expect_identical(is_num_vec(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_n(fxt_int_vec_oan), TRUE)
expect_identical(is_num_vec_n(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_n(fxt_dbl_vec_oan), TRUE)
expect_identical(is_num_vec_n(fxt_dbl_vec_oan0), TRUE)

positive = c(r_int_vec, r_dbl_vec)
for (x in positive) {
  expect_identical(is_num_vec_n(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_n(fxt_int_vec_oa), FALSE)
expect_identical(is_num_vec_n(fxt_int_vec_oa0), FALSE)
expect_identical(is_num_vec_n(fxt_dbl_vec_oa), FALSE)
expect_identical(is_num_vec_n(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_n(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_x(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_x(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec_x(fxt_int_vec_oax), TRUE)
expect_identical(is_num_vec_x(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec_x(fxt_dbl_vec_oanx), TRUE)
expect_identical(is_num_vec_x(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec_x(fxt_dbl_vec_oax), TRUE)
expect_identical(is_num_vec_x(fxt_dbl_vec_oa0), TRUE)

positive = c(r_int_vec[-2L], r_int_vec_named[-2L], r_dbl_vec[-2L],
             r_dbl_vec_named[-2L])

for (x in positive) {
  expect_identical(is_num_vec_x(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_x(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_nx(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_nx(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_nx(fxt_dbl_vec_oanx), TRUE)
expect_identical(is_num_vec_nx(fxt_dbl_vec_oan0), TRUE)

positive = c(r_int_vec[-2L], r_dbl_vec[-2L])
for (x in positive) {
  expect_identical(is_num_vec_nx(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_nx(fxt_int_vec_oax), FALSE)
expect_identical(is_num_vec_nx(fxt_int_vec_oa0), FALSE)
expect_identical(is_num_vec_nx(fxt_dbl_vec_oax), FALSE)
expect_identical(is_num_vec_nx(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nx(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_int_vec[-(1:2)], r_int_vec_named[-(1:2)], r_dbl_vec[-(1:2)],
             r_dbl_vec_named[-(1:2)])

for (x in positive) {
  expect_identical(is_num_vec_x1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_x1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_int_vec[-(1:2)], r_dbl_vec[-(1:2)])
for (x in positive) {
  expect_identical(is_num_vec_nx1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nx1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_int_vec[-1L], r_int_vec_named[-1L], r_dbl_vec[-c(1L, 3L)],
             r_dbl_vec_named[-c(1L, 3L)])

for (x in positive) {
  expect_identical(is_num_vec_y1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_y1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_int_vec[-1L], r_dbl_vec[-c(1L, 3L)])
for (x in positive) {
  expect_identical(is_num_vec_ny1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_ny1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_y(fxt_int_vec_oan), TRUE)
expect_identical(is_num_vec_y(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec_y(fxt_int_vec_oa), TRUE)
expect_identical(is_num_vec_y(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec_y(fxt_dbl_vec_oayn), TRUE)
expect_identical(is_num_vec_y(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec_y(fxt_dbl_vec_oay), TRUE)
expect_identical(is_num_vec_y(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-3L], r_dbl_vec_named[-3L], r_int_vec, r_int_vec_named)
for (x in positive) {
  expect_identical(is_num_vec_y(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_y(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_ny(fxt_int_vec_oan), TRUE)
expect_identical(is_num_vec_ny(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_ny(fxt_dbl_vec_oayn), TRUE)
expect_identical(is_num_vec_ny(fxt_dbl_vec_oan0), TRUE)

positive = c(r_dbl_vec[-3L], r_int_vec)
for (x in positive) {
  expect_identical(is_num_vec_ny(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_ny(fxt_int_vec_oa), FALSE)
expect_identical(is_num_vec_ny(fxt_int_vec_oa0), FALSE)
expect_identical(is_num_vec_ny(fxt_dbl_vec_oay), FALSE)
expect_identical(is_num_vec_ny(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_ny(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_z(fxt_int_vec_oan), TRUE)
expect_identical(is_num_vec_z(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec_z(fxt_int_vec_oa), TRUE)
expect_identical(is_num_vec_z(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec_z(fxt_dbl_vec_oazn), TRUE)
expect_identical(is_num_vec_z(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec_z(fxt_dbl_vec_oaz), TRUE)
expect_identical(is_num_vec_z(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(4:5)], r_dbl_vec_named[-(4:5)], r_int_vec,
             r_int_vec_named)

for (x in positive) {
  expect_identical(is_num_vec_z(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_z(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_nz(fxt_int_vec_oan), TRUE)
expect_identical(is_num_vec_nz(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_nz(fxt_dbl_vec_oazn), TRUE)
expect_identical(is_num_vec_nz(fxt_dbl_vec_oan0), TRUE)

positive = c(r_dbl_vec[-(4:5)], r_int_vec)
for (x in positive) {
  expect_identical(is_num_vec_nz(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_nz(fxt_int_vec_oa), FALSE)
expect_identical(is_num_vec_nz(fxt_int_vec_oa0), FALSE)
expect_identical(is_num_vec_nz(fxt_dbl_vec_oaz), FALSE)
expect_identical(is_num_vec_nz(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_xy(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_xy(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec_xy(fxt_int_vec_oax), TRUE)
expect_identical(is_num_vec_xy(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec_xy(fxt_dbl_vec_oanxy), TRUE)
expect_identical(is_num_vec_xy(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec_xy(fxt_dbl_vec_oaxy), TRUE)
expect_identical(is_num_vec_xy(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(2:3)], r_dbl_vec_named[-(2:3)], r_int_vec[-2L],
             r_int_vec_named[-2L])

for (x in positive) {
  expect_identical(is_num_vec_xy(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_xy(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_nxy(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_nxy(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_nxy(fxt_dbl_vec_oanxy), TRUE)
expect_identical(is_num_vec_nxy(fxt_dbl_vec_oan0), TRUE)

positive = c(r_dbl_vec[-(2:3)], r_int_vec[-2L])
for (x in positive) {
  expect_identical(is_num_vec_nxy(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_nxy(fxt_int_vec_oax), FALSE)
expect_identical(is_num_vec_nxy(fxt_int_vec_oa0), FALSE)
expect_identical(is_num_vec_nxy(fxt_dbl_vec_oaxy), FALSE)
expect_identical(is_num_vec_nxy(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nxy(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_yz(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_yz(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec_yz(fxt_int_vec_oax), TRUE)
expect_identical(is_num_vec_yz(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec_yz(fxt_dbl_vec_oanyz), TRUE)
expect_identical(is_num_vec_yz(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec_yz(fxt_dbl_vec_oayz), TRUE)
expect_identical(is_num_vec_yz(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(3:5)], r_dbl_vec_named[-(3:5)], r_int_vec,
             r_int_vec_named)

for (x in positive) {
  expect_identical(is_num_vec_yz(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_yz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_nyz(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_nyz(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_nyz(fxt_dbl_vec_oanyz), TRUE)
expect_identical(is_num_vec_nyz(fxt_dbl_vec_oan0), TRUE)

positive = c(r_dbl_vec[-(3:5)], r_int_vec)
for (x in positive) {
  expect_identical(is_num_vec_nyz(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_nyz(fxt_int_vec_oax), FALSE)
expect_identical(is_num_vec_nyz(fxt_int_vec_oa0), FALSE)
expect_identical(is_num_vec_nyz(fxt_dbl_vec_oayz), FALSE)
expect_identical(is_num_vec_nyz(fxt_dbl_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nyz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_xyz(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_xyz(fxt_int_vec_oan0), TRUE)
expect_identical(is_num_vec_xyz(fxt_int_vec_oax), TRUE)
expect_identical(is_num_vec_xyz(fxt_int_vec_oa0), TRUE)
# dbl
expect_identical(is_num_vec_xyz(fxt_dbl_vec_oanxyz), TRUE)
expect_identical(is_num_vec_xyz(fxt_dbl_vec_oan0), TRUE)
expect_identical(is_num_vec_xyz(fxt_dbl_vec_oaxyz), TRUE)
expect_identical(is_num_vec_xyz(fxt_dbl_vec_oa0), TRUE)

positive = c(r_dbl_vec[-(2:5)], r_dbl_vec_named[-(2:5)], r_int_vec[-2L],
             r_int_vec_named[-2L])

for (x in positive) {
  expect_identical(is_num_vec_xyz(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_xyz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
# int
expect_identical(is_num_vec_nxyz(fxt_int_vec_oanx), TRUE)
expect_identical(is_num_vec_nxyz(fxt_int_vec_oan0), TRUE)
# dbl
expect_identical(is_num_vec_nxyz(fxt_dbl_vec_oanxyz), TRUE)
expect_identical(is_num_vec_nxyz(fxt_dbl_vec_oan0), TRUE)

positive = c(r_dbl_vec[-(2:5)], r_int_vec[-2L])
for (x in positive) {
  expect_identical(is_num_vec_nxyz(x), TRUE)
}

# Test negative cases
expect_identical(is_num_vec_nxyz(fxt_dbl_vec_oaxyz), FALSE)
expect_identical(is_num_vec_nxyz(fxt_dbl_vec_oa0), FALSE)
expect_identical(is_num_vec_nxyz(fxt_int_vec_oax), FALSE)
expect_identical(is_num_vec_nxyz(fxt_int_vec_oa0), FALSE)

negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nxyz(x), FALSE)
}
rm(negative, positive)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:3)], r_dbl_vec_named[-(1:3)], r_int_vec[-(1:2)],
             r_int_vec_named[-c(1:2)])

for (x in positive) {
  expect_identical(is_num_vec_xy1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_xy1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:3)], r_int_vec[-(1:2)])
for (x in positive) {
  expect_identical(is_num_vec_nxy1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nxy1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-c(1L, 3:5)], r_dbl_vec_named[-c(1L, 3:5)],
             r_int_vec[-1L], r_int_vec_named[-1L])

for (x in positive) {
  expect_identical(is_num_vec_yz1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_yz1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-c(1L, 3:5)], r_int_vec[-1L])
for (x in positive) {
  expect_identical(is_num_vec_nyz1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nyz1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:5)], r_dbl_vec_named[-(1:5)], r_int_vec[-(1:2)],
             r_int_vec_named[-(1:2)])

for (x in positive) {
  expect_identical(is_num_vec_xyz1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_xyz1(x), FALSE)
}
rm(positive, negative)


# ------------------------------------------------------------------------------
# Test positive cases
positive = c(r_dbl_vec[-(1:5)], r_int_vec[-(1:2)])
for (x in positive) {
  expect_identical(is_num_vec_nxyz1(x), TRUE)
}

# Test negative cases
negative = named_list_remove(r_dict, positive)
for (x in negative) {
  expect_identical(is_num_vec_nxyz1(x), FALSE)
}
rm(positive, negative)

