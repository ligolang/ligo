module Handler = Handler

module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  include Prepare_rename
  include Rename
  include References
  include Definition
  include Type_definition
  include Hover
  include Formatting.Make (Ligo_api)
  include On_doc.Make (Ligo_api)
end
