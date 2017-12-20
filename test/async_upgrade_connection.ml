open Core
open Async
open Cohttp_async
open Websocket
open Websocket_async

module Test_rpc = struct
  type query = int [@@deriving sexp, bin_io]
  type response = string [@@deriving sexp, bin_io]

  let rpc = Rpc.Rpc.create ~name:"test-rpc" ~version:0 ~bin_query ~bin_response
end

let start_server port () =
  Cohttp_async.Server.create_expert ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port) (fun ~body _ req ->
        let uri = Cohttp.Request.uri req in
        match Uri.path uri with
        | "/" ->
          eprintf "[PATH] /\n%!";
          let%bind response =
          Cohttp_async.Server.respond_string
            ~status:`OK
            {|
            <html>
            <head>
                <meta charset="utf-8">
                <script src="//code.jquery.com/jquery-1.11.3.min.js"></script>
                <script>
                    $(window).on('load', function(){
                        ws = new WebSocket('ws://localhost:7777/ws');
                        ws.onmessage = function(x) {
                            console.log(x.data);
                            var m = "<- Pong " + parseInt((x.data.substring(8)) - 1);
                            $('#msg').html("<p>" + x.data + "</p><p>" + m + "</p>");
                            ws.send(m);
                        };
                    });
            </script>
            </head>
            <body>
                <div id='msg'></div>
            </body>
            </html>
            |}
          in
          return (`Response response)
        | "/ws" ->
          eprintf "[PATH] /ws\n%!";
          let transport, upgrade =
            Websocket_async.upgrade_connection_transport req
          in
          don't_wait_for begin
            let%bind conn =
              Async_rpc_kernel.Rpc.Connection.create
                ~connection_state:Fn.id
                transport
            in
            match conn with
            | Error err ->
              Log.Global.error !"RPC con failed :( %{sexp:Exn.t}" err;
              Deferred.unit
            | Ok conn ->
              let%bind response = Rpc.Rpc.dispatch_exn Test_rpc.rpc conn 42 in
              Log.Global.error "Response! :D %s" response;
              Deferred.unit
          end;
          return upgrade
        | _ ->
          eprintf "[PATH] Catch-all\n%!";
          let%bind res =
            Server.respond_string
              ~status:`Not_found
              (Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
          in
          return (`Response res)
      )
  >>= fun _ -> Deferred.never ()

let main =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Runs a webserver that serves a page and a websocket for it to interact with"
    (* Let_syntax.Open_on_rhs.flag *)
    [%map_open
      let () = return ()
      and port = flag "port" (optional_with_default 7777 int) ~doc:"PORT Port to bind to (default 7777)"
      in
      fun () ->
        start_server port ()
    ]
;;

let () = Command.run main

