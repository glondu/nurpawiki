open Digestif

let random_salt size =
  let mask = 255L in
  let salt = Bytes.create size in
  for i = 0 to size / 8 do
    let random_int = Random.int64 Int64.max_int in
    for j = 0 to min (size - (8 * i)) 8 - 1 do
      Bytes.set salt
        ((8 * i) + j)
        (Char.chr
           Int64.(to_int (logand mask (shift_right_logical random_int j))))
    done
  done;
  Bytes.to_string salt

(* Return Result.Ok (update, auth) where auth is true if the auth is successful
   and update is true if the hash need to be updated *)
let check stored_hash password =
  match String.split_on_char '$' stored_hash with
  | [ ""; "1"; password_hash ] -> (
      match MD5.of_hex_opt password_hash with
      | Some stored_digest ->
          let digest = MD5.digest_string password in
          Result.Ok (true, MD5.equal stored_digest digest)
      | None -> Result.Error "Failure reading stored hash" )
  | [ ""; "8"; salt; password_hash ] -> (
      match SHA3_512.of_hex_opt password_hash with
      | Some stored_digest ->
          let digest =
            SHA3_512.digest_string (password ^ Hex.to_string (`Hex salt))
          in
          Result.Ok (false, SHA3_512.equal stored_digest digest)
      | None -> Result.Error "Failure reading stored hash" )
  | _ -> Result.Error "Unknown storage format"

let salt password =
  let salt = random_salt 8 in
  Printf.sprintf "$8$%s$%s"
    (Hex.of_string salt |> Hex.show)
    (SHA3_512.digest_string (password ^ salt) |> SHA3_512.to_hex)
