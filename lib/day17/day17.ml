open Core

let is_example = false
let pass_code = if is_example then "ihgpwlah" else "lpvhkcbi"
let get_hash s = Md5.to_hex (Md5.digest_string s)
let result_p1 = ""
let result_p2 = ""
