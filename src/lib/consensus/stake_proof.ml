open Core
open Coda_base

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type t =
      { delegator: Account.Index.Stable.V1.t
      ; delegator_pk: Signature_lib.Public_key.Compressed.Stable.V1.t
      ; ledger: Sparse_ledger.Stable.V1.t
      ; producer_private_key: Signature_lib.Private_key.Stable.V1.t
      ; producer_public_key: Signature_lib.Public_key.Stable.V1.t
      ; coinbase_receiver_pk: Signature_lib.Public_key.Compressed.Stable.V1.t
      }

    let to_latest = Fn.id
  end
end]

(* This is only the data that is necessary for creating the
   blockchain SNARK which is not otherwise available. So in
   particular it excludes the epoch and slot this stake proof
   is for.
*)
type t = Stable.Latest.t =
  { delegator: Account.Index.t
  ; delegator_pk: Signature_lib.Public_key.Compressed.t
  ; ledger: Sparse_ledger.t
  ; producer_private_key: Signature_lib.Private_key.t
  ; producer_public_key: Signature_lib.Public_key.t
  ; coinbase_receiver_pk: Signature_lib.Public_key.Compressed.t }
[@@deriving to_yojson, sexp]
