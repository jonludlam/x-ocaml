type t = Builtin of Client.t | Jtw of Jtw_client.t

let builtin ?extra_load url = Builtin (Client.make ?extra_load url)

let jtw ?findlib_requires ?findlib_index url =
  let c = Jtw_client.make url in
  Jtw_client.init ?findlib_requires ?findlib_index c;
  Jtw c

let on_message = function
  | Builtin c -> Client.on_message c
  | Jtw c -> Jtw_client.on_message c

let post = function Builtin c -> Client.post c | Jtw c -> Jtw_client.post c

let eval ?owner ~id ~line_number = function
  | Builtin c -> Client.eval ~id ~line_number c
  | Jtw c -> Jtw_client.eval ?owner ~id ~line_number c

let fmt ~id = function
  | Builtin c -> Client.fmt ~id c
  | Jtw c -> Jtw_client.fmt ~id c

let restart = function
  | Builtin c -> Client.restart c
  | Jtw c -> Jtw_client.restart c
