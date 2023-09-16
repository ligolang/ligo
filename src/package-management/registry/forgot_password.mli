type state =
  | Password_reset_emailed
  | Profile_creation_pending
  | Email_verification_pending

val main
  :  username:string
  -> ligo_registry:Uri.t
  -> ligorc_path:string
  -> (state, string) result
