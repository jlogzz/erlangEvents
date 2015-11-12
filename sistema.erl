-module(sistema).
-export([start_server/0, server/1, registra_asistente/2, elimina_asistente/1, asistente/3]).

%% Variable para el nombre del servidor
server_node() ->
  super@IkerArbuluMac.

%% Casos de mensajes que puede recibir el servidor
server(Asistentes_List) ->
  receive
    {From, registra, Asistente, Nombre} ->
      New_Asistente_List = registra_asistente_servidor(From, Asistente, Nombre, Asistentes_List),
      server(New_Asistente_List);
    {From, logoff} ->
      New_Asistente_List  = server_logoff(From, Asistentes_List),
      server(New_Asistente_List);
    {From, message_to, To, Message} ->
      server_transfer(From, To, Message, Asistentes_List),
      io:format("list is now: ~p~n", [Asistentes_List]),
      server(Asistentes_List)
  end.

%% Arrancar el servidor
start_server() ->
  register(sistema, spawn(sistema, server, [[]])).

%% Se lleva a cabo el registro de Asistentes en el Servidor
registra_asistente_servidor(From, Asistente, Nombre, Asistentes_List) ->
  case lists:keymember(Asistente, 2, Asistentes_List) of
    true ->
      From ! {sistema, stop, user_exists_at_other_node},
      Asistentes_List;
    false ->
      From ! {sistema, registrado},
      [{From, Asistente, Nombre} | Asistentes_List]
  end.

server_logoff(From, Asistentes_List) ->
  lists:keydelete(From, 1, Asistentes_List).

server_transfer(From, To, Message, Asistentes_List) ->
  case lists:keysearch(From, 1, Asistentes_List) of
    false ->
      From ! {sistema, stop, you_are_not_logged_on};
    {value, {From, Name}} ->
      server_transfer(From, Name, To, Message, Asistentes_List)
  end.

server_transfer(From, Name, To, Message, Asistentes_List) ->
  case lists:keysearch(To, 2, Asistentes_List) of
    false ->
      From ! {sistema, receiver_not_found};
    {value, {ToPid, To}} ->
      ToPid ! {message_from, Name, Message},
      From ! {messenger, sent}
  end .

%% Metodos de Asistentes %%
registra_asistente(Asistente, Nombre) ->
  case whereis(asist_client) of
    undefined ->
      register(asist_client, spawn(sistema, asistente, [server_node(), Asistente, Nombre]));
    _ -> already_logged_on
  end.

elimina_asistente(Asistente)->
  asist_client ! {logoff, Asistente}.

asistente(Server_Node, Asistente, Nombre) ->
  {sistema, Server_Node} ! {self(), registra, Asistente, Nombre},
  await_result(),
  asistente(Server_Node).

asistente(Server_Node) ->
  receive
    {logoff, Asistente} -> %% Falta hacer la eliminacion de eventos por eso se incluye el id del asistente
      {sistema, Server_Node} ! {self(), logoff},
      exit(normal);
    {message_to, ToName, Message} ->
      {messenger, Server_Node} ! {self(), message_to, ToName, Message},
      await_result();
    {message_from, FromName, Message} ->
      io:format("Message from ~p: ~p~n", [FromName, Message])
  end,
  asistente(Server_Node).

await_result() ->
  receive
      {messenger, stop, Why} ->
        io:format("~p~n", [Why]),
        exit(normal);
      {messenger, What} ->
        io:format("~p~n", [What])
  end.
