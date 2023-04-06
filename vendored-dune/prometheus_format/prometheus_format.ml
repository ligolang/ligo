open Prometheus
let failf fmt =
  Fmt.kstr failwith fmt

module TextFormat_0_0_4 = struct
  let re_unquoted_escapes = Re.compile @@ Re.set "\\\n"
  let re_quoted_escapes = Re.compile @@ Re.set "\"\\\n"

  let quote g =
    match Re.Group.get g 0 with
    | "\\" -> "\\\\"
    | "\n" -> "\\n"
    | "\"" -> "\\\""
    | x -> failf "Unexpected match %S" x

  let output_metric_type f = function
    | Counter   -> Fmt.string f "counter"
    | Gauge     -> Fmt.string f "gauge"
    | Summary   -> Fmt.string f "summary"
    | Histogram -> Fmt.string f "histogram"

  let output_unquoted f s =
    Fmt.string f @@ Re.replace re_unquoted_escapes ~f:quote s

  let output_quoted f s =
    Fmt.string f @@ Re.replace re_quoted_escapes ~f:quote s

  (* Fmt.float by default prints floats using scientific exponential
   * notation, which loses significant data on e.g. timestamp:
   *   Fmt.str "%a" Fmt.float 1575363850.57 --> 1.57536e+09 *)
  let float_fmt f =
    Fmt.pf f "%f"

  let output_value f v =
    match classify_float v with
    | FP_normal | FP_subnormal | FP_zero -> float_fmt f v
    | FP_infinite when v > 0.0 -> Fmt.string f "+Inf"
    | FP_infinite -> Fmt.string f "-Inf"
    | FP_nan -> Fmt.string f "Nan"

  let output_pairs f (label_names, label_values) =
    let cont = ref false in
    let output_pair name value =
      if !cont then Fmt.string f ", "
      else cont := true;
      Fmt.pf f "%a=\"%a\"" LabelName.pp name output_quoted value
    in
    List.iter2 output_pair label_names label_values

  let output_labels ~label_names f = function
    | [] -> ()
    | label_values -> Fmt.pf f "{%a}" output_pairs (label_names, label_values)

  let output_sample ~base ~label_names ~label_values f { Sample_set.ext; value; bucket } =
    let label_names, label_values = match bucket with
      | None -> label_names, label_values
      | Some (label_name, label_value) ->
        let label_value_str = Fmt.str "%a" output_value label_value in
        label_name :: label_names, label_value_str :: label_values
    in
    Fmt.pf f "%a%s%a %a@."
      MetricName.pp base ext
      (output_labels ~label_names) label_values
      output_value value

  let output_metric ~name ~label_names f (label_values, samples) =
    List.iter (output_sample ~base:name ~label_names ~label_values f) samples

  let output f =
    MetricFamilyMap.iter (fun metric samples ->
        let {MetricInfo.name; metric_type; help; label_names} = metric in
        Fmt.pf f
          "# HELP %a %a@.\
           # TYPE %a %a@.\
           %a"
          MetricName.pp name output_unquoted help
          MetricName.pp name output_metric_type metric_type
          (LabelSetMap.pp ~sep:Fmt.nop (output_metric ~name ~label_names)) samples
      )
end
