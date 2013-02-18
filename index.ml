
module CrcMap = 
  Map.Make(struct 
    type t = string * Digest.t
    let compare (s1, d1) (s2, d2) = 
      let sc = String.compare s1 s2 in
      if sc = 0 then 
        Digest.compare d1 d2
      else sc
  end)

module StringMap = Map.Make(String)

type global = string CrcMap.t

type local = string StringMap.t

let create_global_from_files filenames = 
  let doFile acc fname = 
    let cmio, cmto = Cmt_format.read fname in
      match cmio, cmto with
      | Some cmi, _ -> begin
          match cmi.Cmi_format.cmi_crcs with
            (_, crc) :: _ -> 
              CrcMap.add (cmi.Cmi_format.cmi_name, crc) fname acc
          | _ -> acc
        end
      | _, Some cmt -> begin
          match cmt.Cmt_format.cmt_interface_digest with
            Some crc ->
              CrcMap.add (cmt.Cmt_format.cmt_modname, crc) fname acc
          | None -> acc
        end
      | None, None -> acc 
  (* TODO handle possible errors from Cmt_format.read *)
  in
    List.fold_left doFile CrcMap.empty filenames

let global_lookup global md = CrcMap.find md global

let create_local global mds = 
  let doMod acc ((name, _) as md) = 
    try
      StringMap.add name (CrcMap.find md global) acc
    with Not_found -> acc
  in
    List.fold_left doMod StringMap.empty mds

let local_lookup local md = StringMap.find md local
