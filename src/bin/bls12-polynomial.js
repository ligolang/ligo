// Provides: caml_bls12_381_hash_anemoi_allocate_ctxt_stubs_bytecode
function caml_bls12_381_hash_anemoi_allocate_ctxt_stubs_bytecode() {}
// Provides: caml_bls12_381_polynomial_dft_stubs
function caml_bls12_381_polynomial_dft_stubs() {}
// Provides: caml_bls12_381_polynomial_fft_inplace_on_stubs
function caml_bls12_381_polynomial_fft_inplace_on_stubs() {}
// Provides: caml_bls12_381_polynomial_ifft_inplace_on_stubs
function caml_bls12_381_polynomial_ifft_inplace_on_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_add_stubs
function caml_bls12_381_polynomial_polynomial_add_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_carray_get_stubs
function caml_bls12_381_polynomial_polynomial_carray_get_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_carray_set_stubs
function caml_bls12_381_polynomial_polynomial_carray_set_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_compute_domain_stubs
function caml_bls12_381_polynomial_polynomial_compute_domain_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_division_xn_stubs
function caml_bls12_381_polynomial_polynomial_division_xn_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_evaluate_stubs
function caml_bls12_381_polynomial_polynomial_evaluate_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_evaluations_add_stubs
function caml_bls12_381_polynomial_polynomial_evaluations_add_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_evaluations_linear_arrays_stubs
function caml_bls12_381_polynomial_polynomial_evaluations_linear_arrays_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_evaluations_mul_arrays_stubs
function caml_bls12_381_polynomial_polynomial_evaluations_mul_arrays_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_evaluations_rescale_stubs
function caml_bls12_381_polynomial_polynomial_evaluations_rescale_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_linear_stubs
function caml_bls12_381_polynomial_polynomial_linear_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_linear_with_powers_stubs
function caml_bls12_381_polynomial_polynomial_linear_with_powers_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_memset_zero_stubs
function caml_bls12_381_polynomial_polynomial_memset_zero_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_mul_by_scalar_stubs
function caml_bls12_381_polynomial_polynomial_mul_by_scalar_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_mul_stubs
function caml_bls12_381_polynomial_polynomial_mul_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_mul_xn_stubs
function caml_bls12_381_polynomial_polynomial_mul_xn_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_negate_stubs
function caml_bls12_381_polynomial_polynomial_negate_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_of_sparse_stubs
function caml_bls12_381_polynomial_polynomial_of_sparse_stubs() {}
// Provides: caml_bls12_381_polynomial_polynomial_sub_stubs
function caml_bls12_381_polynomial_polynomial_sub_stubs() {}
// Provides: caml_bls12_381_polynomial_prime_factor_algorithm_fft_stubs
function caml_bls12_381_polynomial_prime_factor_algorithm_fft_stubs() {}
// Provides: caml_bls12_381_polynomial_srs_g1_pippenger_stubs
function caml_bls12_381_polynomial_srs_g1_pippenger_stubs() {}

// Provides: caml_polynomial_erase_fr_array_stubs
function caml_polynomial_erase_fr_array_stubs(carray, idx, size) {}

// Provides: caml_polynomial_carray_get_stubs
function caml_polynomial_carray_get_stubs(carray, idx, size) {}

// Provides: caml_polynomial_carray_set_stubs
function caml_polynomial_carray_set_stubs(elt, idx, size) {}

// Provides: caml_polynomial_memset_zero_stubs
function caml_polynomial_memset_zero_stubs(n) {}

// Provides: caml_polynomial_compute_domain_stubs
function caml_polynomial_compute_domain_stubs(n, root_of_unity) {}

// Provides: caml_polynomial_of_sparse_stubs
function caml_polynomial_of_sparse_stubs(coefficients, nb_coefficients) {}

// Provides: caml_polynomial_add_stubs
function caml_polynomial_add_stubs(arg_1, arg_2, size_1, size_2) {}

// Provides: caml_polynomial_sub_stubs
function caml_polynomial_sub_stubs(arg_1, arg_2, size_1, size_2) {}

// Provides: caml_polynomial_mul_stubs
function caml_polynomial_mul_stubs(arg_1, arg_2, size_1, size_2) {}

// Provides: caml_polynomial_mul_by_scalar_stubs
function caml_polynomial_mul_by_scalar_stubs(scalar, arg, size) {}

// Provides: caml_polynomial_linear_stubs
function caml_polynomial_linear_stubs(poly_polylen_coeff, nb_polys) {}

// Provides: caml_polynomial_linear_with_powers_stubs
function caml_polynomial_linear_with_powers_stubs(
  coeff,
  poly_polylen,
  nb_polys
) {}

// Provides: caml_polynomial_negate_stubs
function caml_polynomial_negate_stubs(arg, size) {}

// Provides: caml_polynomial_evaluate_stubs
function caml_polynomial_evaluate_stubs(arg, size, scalar) {}

// Provides: caml_polynomial_division_xn_stubs
function caml_polynomial_division_xn_stubs(
  _q,
  res_r,
  poly,
  size,
  n_and_scalar
) {}

// Provides: caml_polynomial_mul_xn_stubs
function caml_polynomial_mul_xn_stubs(poly, size, n, scalar) {}

// Bindings for evaluations.ml

// Provides: caml_polynomial_evaluations_add_stubs
function caml_polynomial_evaluations_add_stubs(
  eval_1,
  eval_2,
  size_1,
  size_2
) {}

// Provides: caml_polynomial_evaluations_rescale_stubs
function caml_polynomial_evaluations_rescale_stubs(
  eval_1,
  size_res,
  size_eval
) {}

// Provides: caml_polynomial_evaluations_mul_arrays_stubs
function caml_polynomial_evaluations_mul_arrays_stubs(
  res,
  eval_evallen_comp_power_powlen,
  size_res,
  nb_evals
) {}

// Provides: caml_polynomial_evaluations_linear_arrays_stubs
function caml_polynomial_evaluations_linear_arrays_stubs(
  res,
  eval_evallen_coeff_comp,
  add_constant,
  size_res,
  nb_evals
) {}

// Provides: caml_polynomial_derivative_stubs
function caml_polynomial_derivative_stubs(poly, size) {}
