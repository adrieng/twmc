let ratio = 1.

let width = 80

let print ?(oc = stdout) d =
  PPrint.ToChannel.pretty ratio width oc PPrint.(d ^^ hardline)

let to_string d =
  let buf = Buffer.create 100 in
  PPrint.ToBuffer.pretty ratio width buf d;
  Bytes.to_string @@ Buffer.to_bytes buf

let fmt ff d =
  PPrint.ToFormatter.pretty ratio width ff d

let to_fmt to_doc ff x =
  Format.fprintf ff "%a" fmt (to_doc x)
