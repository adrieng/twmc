let ratio = 0.9

let width = 80

let print ?(oc = stdout) d =
  PPrint.ToChannel.pretty ratio width oc PPrint.(d ^^ hardline)

let to_string d =
  let buf = Buffer.create 100 in
  PPrint.ToBuffer.pretty ratio width buf d;
  Bytes.to_string @@ Buffer.to_bytes buf

let to_fmt to_doc fmt x =
  Format.fprintf fmt "%s@?" (x |> to_doc |> to_string)
