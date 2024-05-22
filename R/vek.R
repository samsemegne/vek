
#' @title vek
#' @name vek
#' @description
#' Predicate helper functions for testing atomic vectors.
#'
#' All functions take a single argument `x` and check whether it's of the target
#' type of base-R atomic vector, returning `TRUE` or `FALSE`. Some additionally
#' check for value. Classes that extend any base-R atomic vector return `FALSE`.
#' Vectors that carry any attributes other than 'names' return `FALSE`.
#'
#' Function names may include a suffix that encodes what additional conditions
#' are evaluated. A select combination of these conditions is provided for each
#' type. Naming scheme:
#'
#' * n: no 'names' attribute
#' * x: no `NA` (type specific)
#' * y: no `NaN`
#' * z: no `Inf` or `-Inf`
#' * b: no blank characters, i.e. `""`
#' * 1: is of length 1
#'
#' For example:
#'
#' * `is_dbl_vec(x)` evaluates whether `x` is a base-R typeof double atomic
#' vector, of any length.
#' * `is_dbl_vec_xz(x)` will additionaly evaluate whether `x` contains no `NA`
#' nor `Inf` values. Note, `NaN` values are still allowed here, which is
#' dissimilar to behavior from base-R functions like `is.na(x)` or `anyNA(x)`,
#' wherein both `NA` and `NaN` values yield `TRUE`.
#' * `is_num_vec_xyz1(x)` effectively evaluates whether `x` is a single real
#' number.
#'
#' Supported types: logical, integer, double, numeric, and character.
#'
#' @param x `any`.
#' @returns `logical`. Returns `TRUE` or `FALSE`.
#' @seealso [vector()]
NULL


#' @rdname vek
#' @export
is_lgl_vec = function(x) {
  !is.object(x) && is.vector(x, mode = "logical")
}


#' @rdname vek
#' @export
is_lgl_vec_x = function(x) {
  is_lgl_vec(x) && !anyNA(x, recursive = FALSE)
}


#' @rdname vek
#' @export
is_lgl_vec_x1 = function(x) {
  is_lgl_vec_x(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_lgl_vec_n = function(x) {
  is_lgl_vec(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_lgl_vec_nx = function(x) {
  is_lgl_vec_x(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_lgl_vec_nx1 = function(x) {
  is_lgl_vec_x1(x) && is.null(names(x))
}


# ------------------------------------------------------------------------------
#' @rdname vek
#' @export
is_int_vec = function(x) {
  !is.object(x) && is.vector(x, mode = "integer")
}


#' @rdname vek
#' @export
is_int_vec_x = function(x) {
  is_int_vec(x) && !anyNA(x, recursive = FALSE)
}


#' @rdname vek
#' @export
is_int_vec_x1 = function(x) {
  is_int_vec_x(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_int_vec_n = function(x) {
  is_int_vec(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_int_vec_nx = function(x) {
  is_int_vec_x(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_int_vec_nx1 = function(x) {
  is_int_vec_x1(x) && is.null(names(x))
}


# ------------------------------------------------------------------------------
#' @rdname vek
#' @export
is_dbl_vec = function(x) {
  !is.object(x) && is.vector(x, mode = "double")
}


#' @rdname vek
#' @export
is_dbl_vec_x = function(x) {
  if (!is_dbl_vec(x))
    return(FALSE)
  return(!any(is.na(x) & !is.nan(x), na.rm = FALSE))
}


#' @rdname vek
#' @export
is_dbl_vec_y = function(x) {
  is_dbl_vec(x) && !any(is.nan(x), na.rm = FALSE)
}


#' @rdname vek
#' @export
is_dbl_vec_z = function(x) {
  is_dbl_vec(x) && !any(is.infinite(x), na.rm = TRUE)
}


#' @rdname vek
#' @export
is_dbl_vec_xy = function(x) {
  is_dbl_vec_x(x) && is_dbl_vec_y(x)
}


#' @rdname vek
#' @export
is_dbl_vec_yz = function(x) {
  is_dbl_vec_y(x) && is_dbl_vec_z(x)
}


#' @rdname vek
#' @export
is_dbl_vec_xyz = function(x) {
  is_dbl_vec(x) && all(is.finite(x), na.rm = FALSE)
}


#' @rdname vek
#' @export
is_dbl_vec_x1 = function(x) {
  is_dbl_vec_x(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_dbl_vec_y1 = function(x) {
  is_dbl_vec_y(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_dbl_vec_xy1 = function(x) {
  is_dbl_vec_xy(x) && length(x) == 1L
}


#
#' @rdname vek
#' @export
is_dbl_vec_yz1 = function(x) {
  is_dbl_vec_yz(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_dbl_vec_xyz1 = function(x) {
  is_dbl_vec_xyz(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_dbl_vec_n = function(x) {
  is_dbl_vec(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nx = function(x) {
  is_dbl_vec_x(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_ny = function(x) {
  is_dbl_vec_y(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nz = function(x) {
  is_dbl_vec_z(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nxy = function(x) {
  is_dbl_vec_xy(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nyz = function(x) {
  is_dbl_vec_yz(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nxyz = function(x) {
  is_dbl_vec_xyz(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nx1 = function(x) {
  is_dbl_vec_x1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_ny1 = function(x) {
  is_dbl_vec_y1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nxy1 = function(x) {
  is_dbl_vec_xy1(x) && is.null(names(x))
}


#
#' @rdname vek
#' @export
is_dbl_vec_nyz1 = function(x) {
  is_dbl_vec_yz1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_dbl_vec_nxyz1 = function(x) {
  is_dbl_vec_xyz1(x) && is.null(names(x))
}


# ------------------------------------------------------------------------------
#' @rdname vek
#' @export
is_chr_vec = function(x) {
  !is.object(x) && is.vector(x, mode = "character")
}


#' @rdname vek
#' @export
is_chr_vec_x = function(x) {
  is_chr_vec(x) && !anyNA(x, recursive = FALSE)
}


#' @rdname vek
#' @export
is_chr_vec_b = function(x) {
  is_chr_vec(x) && all(x != "", na.rm = TRUE)
}


#' @rdname vek
#' @export
is_chr_vec_xb = function(x) {
  is_chr_vec_x(x) && all(x != "", na.rm = TRUE)
}


#' @rdname vek
#' @export
is_chr_vec_x1 = function(x) {
  is_chr_vec_x(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_chr_vec_b1 = function(x) {
  is_chr_vec_b(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_chr_vec_xb1 = function(x) {
  is_chr_vec_xb(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_chr_vec_n = function(x) {
  is_chr_vec(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_chr_vec_nx = function(x) {
  is_chr_vec_x(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_chr_vec_nb = function(x) {
  is_chr_vec_b(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_chr_vec_nxb = function(x) {
  is_chr_vec_xb(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_chr_vec_nx1 = function(x) {
  is_chr_vec_x1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_chr_vec_nb1 = function(x) {
  is_chr_vec_b1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_chr_vec_nxb1 = function(x) {
  is_chr_vec_xb1(x) && is.null(names(x))
}


# ------------------------------------------------------------------------------
#' @rdname vek
#' @export
is_num_vec = function(x) {
  !is.object(x) && is.vector(x, mode = "numeric")
}


#' @rdname vek
#' @export
is_num_vec_x = function(x) {
  if (!is_num_vec(x))
    return(FALSE)
  if (is.integer(x))
    return(is_int_vec_x(x))
  else if (is.double(x))
    return(is_dbl_vec_x(x))
  else
    stop("invalid state")
}


#' @rdname vek
#' @export
is_num_vec_y = function(x) {
  if (!is_num_vec(x))
    return(FALSE)
  if (is.integer(x))
    return(TRUE)
  else if (is.double(x))
    return(is_dbl_vec_y(x))
  else
    stop("invalid state")
}


#' @rdname vek
#' @export
is_num_vec_z = function(x) {
  if (!is_num_vec(x))
    return(FALSE)
  if (is.integer(x))
    return(TRUE)
  else if (is.double(x))
    return(is_dbl_vec_z(x))
  else
    stop("invalid state")
}


#' @rdname vek
#' @export
is_num_vec_xy = function(x) {
  is_num_vec_x(x) && is_num_vec_y(x)
}


#' @rdname vek
#' @export
is_num_vec_yz = function(x) {
  is_num_vec_y(x) && is_num_vec_z(x)
}


#' @rdname vek
#' @export
is_num_vec_xyz = function(x) {
  if (!is_num_vec_x(x))
    return(FALSE)
  if (is.integer(x))
    return(TRUE)
  else if (is.double(x))
    return(all(is.finite(x), na.rm = FALSE))
  else
    stop("invalid state")
}


#' @rdname vek
#' @export
is_num_vec_x1 = function(x) {
  is_num_vec_x(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_num_vec_y1 = function(x) {
  is_num_vec_y(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_num_vec_yz1 = function(x) {
  is_num_vec_yz(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_num_vec_xy1 = function(x) {
  is_num_vec_xy(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_num_vec_xyz1 = function(x) {
  is_num_vec_xyz(x) && length(x) == 1L
}


#' @rdname vek
#' @export
is_num_vec_n = function(x) {
  is_num_vec(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nx = function(x) {
  is_num_vec_x(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_ny = function(x) {
  is_num_vec_y(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nz = function(x) {
  is_num_vec_z(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nxy = function(x) {
  is_num_vec_xy(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nyz = function(x) {
  is_num_vec_yz(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nxyz = function(x) {
  is_num_vec_xyz(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nx1 = function(x) {
  is_num_vec_x1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_ny1 = function(x) {
  is_num_vec_y1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nyz1 = function(x) {
  is_num_vec_yz1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nxy1 = function(x) {
  is_num_vec_xy1(x) && is.null(names(x))
}


#' @rdname vek
#' @export
is_num_vec_nxyz1 = function(x) {
  is_num_vec_xyz1(x) && is.null(names(x))
}
