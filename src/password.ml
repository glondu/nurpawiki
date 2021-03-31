open Cryptokit

let random_salt size = Random.(string secure_rng size)

(* Return Result.Ok (update, auth) where auth is true if the auth is successful
   and update is true if the hash need to be updated *)
let check stored_hash password =
  let hash_decode = Hexa.decode () in
  match String.split_on_char '$' stored_hash with
  | [ ""; "1"; password_hash ] -> (
      try
        let hash_function = Hash.md5 () in
        hash_decode#put_string password_hash;
        hash_decode#finish;
        hash_function#add_string password;
        Result.Ok (true, string_equal hash_decode#get_string hash_function#result)
      with Error _ -> Result.Error "Failure reading stored hash")
  | [ ""; "8"; salt; password_hash ] -> (
      try
        let salt_decode = Hexa.decode () in
        salt_decode#put_string salt;
        salt_decode#finish;
        let hash_function = Hash.sha3 512 in
        hash_decode#put_string password_hash;
        hash_decode#finish;
        hash_function#add_string (password^salt_decode#get_string);
        Result.Ok (false, string_equal hash_decode#get_string hash_function#result)
      with Error _ -> Result.Error "Failure reading stored hash")
  | _ -> Result.Error "Unknown storage format"

let salt password =
  let salt_encode = Hexa.encode () in
  let salt = random_salt 8 in
  salt_encode#put_string salt;
  salt_encode#finish;
  let hash_function = Hash.sha3 512 in
  let hash_encode = Hexa.encode () in
  hash_function#add_string (password ^ salt);
  hash_encode#put_string hash_function#result;
  hash_encode#finish;
  Printf.sprintf "$8$%s$%s" salt_encode#get_string hash_encode#get_string
