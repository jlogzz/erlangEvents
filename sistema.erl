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
    {Asistente, logoff} ->
      New_Asistente_List  = server_logoff(Asistente, Asistentes_List),
      server(New_Asistente_List)
    %{From, desinscribe, Asistente, Conferencia}->
      % Falta incluir metodos para atender a la Conferencia
      %;
    %{From, inscribe, Asistente, Conferencia}->
      % Falta incluir metodos para desinscribir la Conferencia
      %;
    %{From, registra_conf, Conferencia, Titulo, Conferencista, Horario, Cupo}->
      % Falta incluir metodo para registrar la conferencia
      %;
    %{From, inscritos, Conferencia}->
      %Falta incluir metodo para sacar los registrados a la conferencia
      %;
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

server_logoff(Asistente, Asistentes_List) ->
  lists:keydelete(Asistente, 2, Asistentes_List).

%% Metodos de Asistentes %%
registra_asistente(Asistente, Nombre) ->
  case whereis(Asistente) of
    undefined ->
      register(Asistente, spawn(sistema, asistente, [server_node(), Asistente, Nombre]));
    _ -> already_logged_on
  end.

elimina_asistente(Asistente)->
  Asistente ! {logoff, Asistente}.

inscribe_conferencia(Asistente, Conferencia) ->
  Asistente ! {inscribe, Asistente, Conferencia}.

desinscribe_conferencia(Asistente, Conferencia) ->
  Asistente ! {desinscribe, Asistente, Conferencia}.

conferencias_inscritas(Asistente) ->
  Asistente ! {inscritas, Asistente}.

asistente(Server_Node, Asistente, Nombre) ->
  {sistema, Server_Node} ! {self(), registra, Asistente, Nombre},
  await_result(),
  asistente(Server_Node).

asistente(Server_Node) ->
  receive
    {logoff, Asistente} -> %% Falta hacer la eliminacion de eventos por eso se incluye el id del asistente
      {sistema, Server_Node} ! {Asistente, logoff},
      exit(normal);
    {inscribe, Asistente, Conferencia} ->
      {sistema, Server_Node} ! {self(), inscribe, Asistente, Conferencia},
      await_result();
    {desinscribe, Asistente, Conferencia} ->
      {sistema, Server_Node} ! {self(), desinscribe, Asistente, Conferencia},
      await_result()
  end,
  asistente(Server_Node).

await_result() ->
  receive
      {sistema, stop, Why} ->
        io:format("~p~n", [Why]),
        exit(normal);
      {sistema, What} ->
        io:format("~p~n", [What])
  end.

%% Metodos de Conferencias %%
registra_conferencia(Conferencia, Titulo, Conferencista, Horario, Cupo) ->
  case whereis(Conferencia) of
    undefined ->
      register(Conferencia, spawn(sistema, conferencia, [server_node(), Conferencia, Titulo, Conferencista, Horario, Cupo]));
    _ -> already_created
  end.

elimina_conferencia(Conferencia) ->
  Conferencia ! {elimina, Conferencia}.

conferencia(Server_Node, Conferencia, Titulo, Conferencista, Horario, Cupo) ->
  {sistema, Server_Node} ! {self(), registra_conf, Conferencia, Titulo, Conferencista, Horario, Cupo},
  await_result(),
  conferencia(Server_Node).

 asistentes_inscritos(Conferencia) ->
   Conferencia ! {inscritos, Conferencia}.

conferencia(Server_Node) ->
  receive
    {elimina, Conferencia} -> %% Falta hacer la eliminacion de eventos por eso se incluye el id del asistente
      {sistema, Server_Node} ! {Conferencia, elimina},
      exit(normal);
    {inscritos, Conferencia} ->
      {sistema, Server_Node} ! {self(), inscritos, Conferencia},
      await_result()
  end,
  conferencia(Server_Node).
