module Handler = Handler

module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  include Definition
  include Document_link
  include Hover
  include Rename
  include Prepare_rename
  include References
  include Type_definition
  include Formatting.Make (Ligo_api)
  include On_doc.Make (Ligo_api)
end
