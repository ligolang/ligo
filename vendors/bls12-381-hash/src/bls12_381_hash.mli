(** Implementation of
    {{:https://eprint.iacr.org/2019/458.pdf} Poseidon} over the scalar field of
    BLS12-381 for a security with the permutation [x^5].

    {b The current implementation only provides the functions to run a
       permutation. The user is responsible to build a hash function on top of
       it. } *)
module Poseidon : sig
  (** Context of the permutation *)
  type ctxt

  (** [allocate_ctxt state_size nb_full_rounds nb_partial_rounds batch_size
      round_constants mds]. Allocate a context for a specific instance of Poseidon
  *)
  val allocate_ctxt :
    int ->
    int ->
    int ->
    int ->
    Bls12_381.Fr.t array ->
    Bls12_381.Fr.t array array ->
    ctxt

  (** Return the current state of the context *)
  val get_state : ctxt -> Bls12_381.Fr.t array

  (** Return the state size of the context *)
  val get_state_size : ctxt -> int

  (** [set_state ctxt state]. Set the context state to the given value. The
        value [state] must be of the same size than the expecting state *)
  val set_state : ctxt -> Bls12_381.Fr.t array -> unit

  (** Apply a permutation on the current state of the context *)
  val apply_permutation : ctxt -> unit

  module Parameters : sig
    (** Parameters for Poseidon with a state size of [3] for a security of
        128bits. The value is:
        - state size
        - number of full rounds
        - number of partial rounds
        - batch size
        - round constants
        - mds
    *)
    val state_size_128_3 :
      int * int * int * int * Bls12_381.Fr.t array * Bls12_381.Fr.t array array

    (** Parameters for Poseidon with a state size of [5] for a security of
        256bits. The value is:
        - state size
        - number of full rounds
        - number of partial rounds
        - batch size
        - round constants
        - mds
    *)
    val state_size_256_5 :
      int * int * int * int * Bls12_381.Fr.t array * Bls12_381.Fr.t array array
  end
end

(** Implementation of an instantiation of {{: https://eprint.iacr.org/2019/426 }
    Rescue } over the scalar field of BLS12-381 for a security of 128 bits and
    with the permutation [x^5]. The parameters of the instantiation are:
    - state size = 3
    - number of rounds = 14

    These parameters have been generated using {{:
    https://github.com/KULeuven-COSIC/Marvellous/blob/0969ce8a5ebaa0bf45696b44e276d3dd81d2e455/rescue_prime.sage}
    this script}.
*)
module Rescue : sig
  (** Context of the permutation *)
  type ctxt

  (** [allocate_ctxt mds round_constants nb_rounds state_size]. Allocate a
      context for a specific instance of Rescue *)
  val allocate_ctxt :
    Bls12_381.Fr.t array array -> Bls12_381.Fr.t array -> int -> int -> ctxt

  (** Return the current state of the context *)
  val get_state : ctxt -> Bls12_381.Fr.t array

  (** Return the state size of the context *)
  val get_state_size : ctxt -> int

  (** [set_state ctxt state]. Set the context state to the given value. The
      value [state] must be of the same size than the expecting state *)
  val set_state : ctxt -> Bls12_381.Fr.t array -> unit

  (** Apply a permutation on the current state of the context *)
  val apply_permutation : ctxt -> unit

  (** Set of parameters for BLS12-381, and parameters for specific
      instantiations given in the reference paper *)
  module Parameters : sig
    (** Parameters for Rescue with [state_size = 3] and 128 bits of security
        The parameters are:
        - number of rounds
        - state size
        - MDS matrix
        - round constants

        FIXME: The MDS and the round constants are not standard
    *)
    val state_size_3 :
      int * int * Bls12_381.Fr.t array array * Bls12_381.Fr.t array
  end
end

(** Implementation of {{: https://eprint.iacr.org/2022/840}
    the permutation Anemoi and the mode of operation Jive} over the scalar field
    of BLS12-381.

    The state of the permutation Anemoi is [m], where [m] is a multiple of [2].
    It is commonly refered by [l] such that [m = 2 l].
*)
module Anemoi : sig
  (** Set of parameters for BLS12-381, and parameters for specific
     instantiations given in the reference paper *)
  module Parameters : sig
    (** The type representing the set of parameters for a given instance *)
    type t

    (** [create security state_size linear_layer] creates a
        value of type {!t}. If the [state_size] is [2], [4], [6] or [8], an
        exception is raised. The library enforces the user to use the default
        security parameters and an optimised implementation is provided in these
        cases. Also, an exception is raised if the state size is not a multiple
        of [2].

        @deprecated It is highly recommended to follow the recommandation in the
        paper for the choice of security parameters. Please open an issue if you
        need support for other instances than the default parameters provided by
        the library.
    *)
    val create : int -> int -> Bls12_381.Fr.t array array -> t
      [@@deprecated
        "It is highly recommended to follow the recommandation in the paper \
         for the choice of security parameters. Please open an issue if you \
         need support for other instances than the default parameters provided \
         by the library."]

    (** Exponent for the substitution box. For BLS12-381, it is [5] *)
    val alpha : Bls12_381.Fr.t

    (** Inverse of the exponent for the substitution box. For BLS12-381, it is
        [20974350070050476191779096203274386335076221000211055129041463479975432473805] *)
    val alpha_inv : Bls12_381.Fr.t

    (** For BLS12-381, it is
        [14981678621464625851270783002338847382197300714436467949315331057125308909861]
    *)
    val delta : Bls12_381.Fr.t

    (** First generator of the scalar field of BLS12-381, i.e. [7] *)
    val g : Bls12_381.Fr.t

    (** Same than {!g} *)
    val beta : Bls12_381.Fr.t

    (** Set to [0] for BLS12-381 *)
    val gamma : Bls12_381.Fr.t

    (** [compute_number_of_rounds state_size security] computes the minimal
        number of rounds for an instance of Anemoi with a state size of [m =
        state_size] to reach a security level of [security] bits. The
        computation follows the formula given in section 5.2 *)
    val compute_number_of_rounds : int -> int -> int

    (** [generate_constants nb_rounds l] generates the constants for the
        instance of Anemoi for a state size of [m = 2 * l]. The output contains
        the C's followed by the D's as described in the paper in section 5.1 *)
    val generate_constants : int -> int -> Bls12_381.Fr.t array

    (** Parameters for the permutation Anemoi for a state size of [m = 2] (i.e.
        [l = 1]) and 128 bits of security given in the paper.
    *)
    val security_128_state_size_2 : t

    (** Parameters for the permutation Anemoi for a state size of [m = 4] (i.e.
        [l = 2]) and 128 bits of security given in the paper.
    *)
    val security_128_state_size_4 : t

    (** Parameters for the permutation Anemoi for a state size of [m = 6] (i.e.
        [l = 3]) and 128 bits of security given in the paper.
    *)
    val security_128_state_size_6 : t

    (** Parameters for the permutation Anemoi for a state size of [m = 8] (i.e.
        [l = 4]) and 128 bits of security given in the paper.
    *)
    val security_128_state_size_8 : t
  end

  (** A context contains the state and the instance parameters *)
  type ctxt

  (** [allocate_ctxt parameters]. Allocate a context for a specific instance of
      Anemoi specific by [parameters].
  *)
  val allocate_ctxt : Parameters.t -> ctxt

  (** Return the current state of the context *)
  val get_state : ctxt -> Bls12_381.Fr.t array

  (** Return the state size of the context *)
  val get_state_size : ctxt -> int

  (** [set_state ctxt state]. Set the context state to the given value. The
      value [state] must be of the same size than the expecting state *)
  val set_state : ctxt -> Bls12_381.Fr.t array -> unit

  (** [apply_one_round ctxt i_round_key] applies only one round of the permutation on the
      state. [i_round_key] is the index of the first round constant to use for the
      round. The context is modified. *)
  val apply_one_round : ctxt -> int -> unit

  (** Apply a permutation on the current state of the context *)
  val apply_permutation : ctxt -> unit

  (** [jive128_1 x y] calls the permutation Anemoi for [l = 1] with the state [S = (x, y)] and
      apply Jive on the output *)
  val jive128_1 : Bls12_381.Fr.t -> Bls12_381.Fr.t -> Bls12_381.Fr.t
end

(** {{: https://eprint.iacr.org/2022/403.pdf } Griffin } over the scalar field
    of BLS12-381 for a security of 128 bits and with the permutation [x^5].
*)
module Griffin : sig
  (** Context of the instance. It contains the states and the parameters such as
      the state size, the constants and the alpha/beta's *)
  type ctxt

  (** [allocate_ctxt nb_rounds state_size round_constants alpha_beta_s] allocates a new
      context for an instance of Griffin with a state of size [state_size], with
      round constants [round_constants] and alpha/beta values set to [alpha_beta_s].
  *)
  val allocate_ctxt :
    int -> int -> Bls12_381.Fr.t array -> Bls12_381.Fr.t array -> ctxt

  (** [apply_permutation ctxt] applies a permutation on the state. The context
      is modified. *)
  val apply_permutation : ctxt -> unit

  (** [apply_one_round ctxt i_round_key] applies only one round of the permutation on the
      state. [i_round_key] is the index of the first round constant to use for the
      round. The context is modified *)
  val apply_one_round : ctxt -> int -> unit

  (** [set_state ctxt state]. Set the context state to the given value. The
      value [state] must be of the same size than the expecting state *)
  val set_state : ctxt -> Bls12_381.Fr.t array -> unit

  (** Return the current state of the context *)
  val get_state : ctxt -> Bls12_381.Fr.t array

  (** Return the state size of the context *)
  val get_state_size : ctxt -> int

  module Parameters : sig
    (** Exponent for the substitution box. For BLS12-381, it is [5] *)
    val d : Bls12_381.Fr.t

    (** Inverse of the exponent for the substitution box. For BLS12-381, it is
        [20974350070050476191779096203274386335076221000211055129041463479975432473805] *)
    val d_inv : Bls12_381.Fr.t

    (** Parameters for Griffin with a state size of [3] for a security of
        128bits. The value is:
        - number of rounds
        - state size
        - round constants
        - alpha/beta's
    *)
    val state_size_3 : int * int * Bls12_381.Fr.t array * Bls12_381.Fr.t array

    (** Parameters for Griffin with a state size of [4] for a security of 128
        bits. The value is:
        - number of rounds
        - state size
        - round constants
        - alpha/beta's
    *)
    val state_size_4 : int * int * Bls12_381.Fr.t array * Bls12_381.Fr.t array
  end
end
