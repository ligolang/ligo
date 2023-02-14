module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  include Definition
  include Document_link
  include Formatting.Make (Ligo_api)
  include Hover
  include On_doc.Make (Ligo_api)
  include Prepare_rename
  include Rename
  include References
  include Type_definition
  module Handler = Handler
end
