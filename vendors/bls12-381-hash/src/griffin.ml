module Stubs = struct
  type ctxt

  external allocate_ctxt :
    int -> int -> Bls12_381.Fr.t array -> Bls12_381.Fr.t array -> ctxt
    = "caml_bls12_381_hash_griffin_allocate_ctxt_stubs"

  external apply_perm : ctxt -> unit
    = "caml_bls12_381_hash_griffin_apply_permutation_stubs"

  external apply_one_round : ctxt -> int -> unit
    = "caml_bls12_381_hash_griffin_apply_one_round_stubs"

  external get_state : Bls12_381.Fr.t array -> ctxt -> unit
    = "caml_bls12_381_hash_griffin_get_state_stubs"

  external set_state : ctxt -> Bls12_381.Fr.t array -> unit
    = "caml_bls12_381_hash_griffin_set_state_stubs"

  external get_state_size : ctxt -> int
    = "caml_bls12_381_hash_griffin_get_state_size_stubs"
end

type ctxt = Stubs.ctxt

let allocate_ctxt nb_rounds state_size constants alpha_beta_s =
  if state_size <> 3 && state_size <> 4 then
    failwith "Only state of 3 and 4 are implemented for the moment" ;
  let ctxt = Stubs.allocate_ctxt nb_rounds state_size constants alpha_beta_s in
  ctxt

let apply_permutation ctxt = Stubs.apply_perm ctxt

let apply_one_round ctxt i_round_key = Stubs.apply_one_round ctxt i_round_key

let get_state_size ctxt = Stubs.get_state_size ctxt

let set_state ctxt state =
  let exp_state_size = Stubs.get_state_size ctxt in
  let state_size = Array.length state in
  if state_size <> exp_state_size then
    failwith
      (Printf.sprintf
         "The given array contains %d elements but the expected state size is \
          %d"
         state_size
         exp_state_size) ;
  Stubs.set_state ctxt state

let get_state ctxt =
  let state_size = Stubs.get_state_size ctxt in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.(copy zero)) in
  Stubs.get_state state ctxt ;
  state

module Parameters = struct
  let d = Bls12_381.Fr.of_string "5"

  let d_inv =
    Bls12_381.Fr.of_string
      "20974350070050476191779096203274386335076221000211055129041463479975432473805"

  let state_size_3 =
    ( 12,
      3,
      Bls12_381.Fr.
        [| of_string
             "34128550609306794648855049790941029207260430992267281605932459023961690971527";
           of_string
             "3266841962043621564976329159669013270493836397698230880922871956509445295589";
           of_string
             "3788870321077812693845526449989981219720718862725565790483882416016813364971";
           of_string
             "27024891585303786115960806070333703216432373723439881013129899315637778629998";
           of_string
             "23010579400464938076910473636770832973813762864914735002621597484262695828622";
           of_string
             "27080836131190471291954411865555005238540949310090748858643349925944589708115";
           of_string
             "46636255352135029230912367971012168967444546775705654694396288342094186777525";
           of_string
             "47133164824570233448088166693080560815816882534409343236711996260986966467741";
           of_string
             "9606676225922904172932777687469921750434342743127013317371941417568076740659";
           of_string
             "18463500124224312515875567238728244985257614546546778836630989065496961733332";
           of_string
             "28472374405620684393423284799834488844243063495131136021015343369044447515728";
           of_string
             "35268741564671016498297025103057965141493419622674333513483247896565728148590";
           of_string
             "39539585386181717155638123485994910144245090227431714808608221839931008903493";
           of_string
             "52380088666150418943399934835814882444841247495734871947668472357102984318486";
           of_string
             "19167780327970696990366686602788009806143096819968263495753330062144751042611";
           of_string
             "29714551815676515766238911391444505405128697853355881959447373833299714778201";
           of_string
             "36137969496642078630737148698387538637943643635849903238148004453205535508419";
           of_string
             "43938585519152746565341473691290665049199746016959097846968019008987449858992";
           of_string
             "18358508122557898758794573769637081105826061265493116379680758931782412629326";
           of_string
             "19946718010959340809909598649820159018638951411545031530449894421301304601041";
           of_string
             "31715651356223023397409672475842491549537134647703758287414523885665812932453";
           of_string
             "28463446749720890206912367474344537583947071022035116560259967158902287016645";
           of_string
             "19352025581521776810257180011278905844311255975757730113854715532000370561248";
           of_string
             "23875742116255515995867064853865007839808574907101870889402929799973393348419";
           of_string
             "36283757151862211218715445654560253429530321310156361189848584389030038151884";
           of_string
             "32575606680762466027769466012933264462659120984156903566848815765316598735927";
           of_string
             "47888621950698322446966530449263122446161745917733573449037171624562096639523";
           of_string
             "21935782059159444261184801642627906900898586003107206203727898450011376823577";
           of_string
             "6658859162075582896934614132443368873283225807004093582364487335761684086353";
           of_string
             "25170381415908319602768131022135215671330810111735961551512738905773070633127";
           of_string
             "3856579408918104405775510647549098777335403755628729652620891849192253152741";
           of_string
             "51071656040623838816910331799254816013392914891920120152037239049697640303362";
           of_string
             "35780857593405893371916330187109234255729583537475686449856260508724718012888";
           of_string "0";
           of_string "0";
           of_string "0"
        |],
      Bls12_381.Fr.
        [| of_string
             "20950244155795017333954742965657628047481163604901233004908207073969011285354";
           of_string
             "3710185818436319233594998810848289882480745979515096857371562288200759554874"
        |] )

  let state_size_4 =
    ( 11,
      4,
      Bls12_381.Fr.
        [| of_string
             "34128550609306794648855049790941029207260430992267281605932459023961690971527";
           of_string
             "3266841962043621564976329159669013270493836397698230880922871956509445295589";
           of_string
             "3788870321077812693845526449989981219720718862725565790483882416016813364971";
           of_string
             "27024891585303786115960806070333703216432373723439881013129899315637778629998";
           of_string
             "23010579400464938076910473636770832973813762864914735002621597484262695828622";
           of_string
             "27080836131190471291954411865555005238540949310090748858643349925944589708115";
           of_string
             "46636255352135029230912367971012168967444546775705654694396288342094186777525";
           of_string
             "47133164824570233448088166693080560815816882534409343236711996260986966467741";
           of_string
             "9606676225922904172932777687469921750434342743127013317371941417568076740659";
           of_string
             "18463500124224312515875567238728244985257614546546778836630989065496961733332";
           of_string
             "28472374405620684393423284799834488844243063495131136021015343369044447515728";
           of_string
             "35268741564671016498297025103057965141493419622674333513483247896565728148590";
           of_string
             "39539585386181717155638123485994910144245090227431714808608221839931008903493";
           of_string
             "52380088666150418943399934835814882444841247495734871947668472357102984318486";
           of_string
             "19167780327970696990366686602788009806143096819968263495753330062144751042611";
           of_string
             "29714551815676515766238911391444505405128697853355881959447373833299714778201";
           of_string
             "36137969496642078630737148698387538637943643635849903238148004453205535508419";
           of_string
             "43938585519152746565341473691290665049199746016959097846968019008987449858992";
           of_string
             "18358508122557898758794573769637081105826061265493116379680758931782412629326";
           of_string
             "19946718010959340809909598649820159018638951411545031530449894421301304601041";
           of_string
             "31715651356223023397409672475842491549537134647703758287414523885665812932453";
           of_string
             "28463446749720890206912367474344537583947071022035116560259967158902287016645";
           of_string
             "19352025581521776810257180011278905844311255975757730113854715532000370561248";
           of_string
             "23875742116255515995867064853865007839808574907101870889402929799973393348419";
           of_string
             "36283757151862211218715445654560253429530321310156361189848584389030038151884";
           of_string
             "32575606680762466027769466012933264462659120984156903566848815765316598735927";
           of_string
             "47888621950698322446966530449263122446161745917733573449037171624562096639523";
           of_string
             "21935782059159444261184801642627906900898586003107206203727898450011376823577";
           of_string
             "6658859162075582896934614132443368873283225807004093582364487335761684086353";
           of_string
             "25170381415908319602768131022135215671330810111735961551512738905773070633127";
           of_string
             "3856579408918104405775510647549098777335403755628729652620891849192253152741";
           of_string
             "51071656040623838816910331799254816013392914891920120152037239049697640303362";
           of_string
             "35780857593405893371916330187109234255729583537475686449856260508724718012888";
           of_string
             "20950244155795017333954742965657628047481163604901233004908207073969011285354";
           of_string
             "3710185818436319233594998810848289882480745979515096857371562288200759554874";
           of_string
             "30288032286013295184960800141577929153144689575967301062487213512439157355972";
           of_string
             "481723775804382866461667527988591972945363824626223124260985488775304020142";
           of_string
             "24927320031052195697186780138061432837278685332539564079180626150831998848822";
           of_string
             "32998900640590419918379784983020973282594314960087846170738034630937198063643";
           of_string
             "8631511908740807956134897737056028248340473776882190692861787147243252572174";
           of_string "0";
           of_string "0";
           of_string "0";
           of_string "0"
        |],
      Bls12_381.Fr.
        [| of_string
             "11565587016645966220743989368574475058372014904683361799482031042545234168865";
           of_string
             "30671383810635628504609520714468855911284203461969920242344142791585782410006";
           of_string
             "23131174033291932441487978737148950116744029809366723598964062085090468337730";
           of_string
             "17813784892290133059542601841503491969755708846824405324169253766465967270998"
        |] )
end