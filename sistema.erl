-module(sistema).
-export([start_server/0,
         server/2,
         lista_asistentes/0,
         lista_conferencias/0,
         registra_asistente/2,
         elimina_asistente/1,
         asistente/3,
         registra_conferencia/5,
         elimina_conferencia/1,
         conferencia/6,
         inscribe_conferencia/2,
         desinscribe_conferencia/2,
         conferencias_inscritas/1,
         asistentes_inscritos/1
        ]).

%% Variable para el nombre del servidor
server_node() ->
  super@JLO.

%% Casos de mensajes que puede recibir el servidor
server(Asistentes_List, Conferencia_List) ->
  receive
    {link, PID} ->
      monitor(process, PID),
      io:format("linkeado~n"),
      PID ! {monitor, self()},
      server(Asistentes_List, Conferencia_List);
    {asistentes, lista_asistentes} ->
      lista_asistentes_servidor(Asistentes_List),
      server(Asistentes_List, Conferencia_List);
    {conferencias, lista_conferencias} ->
      lista_conferencia_servidor(Conferencia_List),
      server(Asistentes_List, Conferencia_List);
    {From, registra, Asistente, Nombre} ->
      New_Asistente_List = registra_asistente_servidor(From, Asistente, Nombre, Asistentes_List),
      server(New_Asistente_List, Conferencia_List);
    {Asistente, logoff} ->
      New_Asistente_List  = server_logoff(Asistente, Asistentes_List),
      server(New_Asistente_List, Conferencia_List);
    {From, desinscribe, Asistente, Conferencia}->
      case lists:keymember(Asistente, 2, Asistentes_List) and lists:keymember(Conferencia, 2, Conferencia_List) of
        true ->
          New_Asistente_List   = desinscribe_conferencia_asistente(Asistente, Conferencia, Asistentes_List),
          New_Conferencia_List = desinscribe_conferencia_conferencia(Asistente, Conferencia, Conferencia_List),
          % New_Conferencia_List = Conferencia_List,
          From ! {sistema, eliminado_asistente_conferencia},
          server(New_Asistente_List, New_Conferencia_List);
        false ->
          From ! {sistema, asistente_o_conferencia_incorrecto},
          server(Asistentes_List, Conferencia_List)
      end;
    {From, inscribe, Asistente, Conferencia}->
      case lists:keymember(Asistente, 2, Asistentes_List) and puede_asistir(Asistente, Asistentes_List) and lists:keymember(Conferencia, 2, Conferencia_List) and hay_cupo(Conferencia, Conferencia_List) of
        true ->
          New_Asistente_List   = inscribe_conferencia_asistente(Asistente, Conferencia, Asistentes_List),
          New_Conferencia_List = inscribe_conferencia_conferencia(Asistente, Conferencia, Conferencia_List),
          % New_Conferencia_List = Conferencia_List,
          From ! {sistema, registrado_asistente_conferencia},
          server(New_Asistente_List, New_Conferencia_List);
        false ->
          From ! {sistema, asistente_o_conferencia_incorrecto},
          server(Asistentes_List, Conferencia_List)
      end;
    {From, registra_conferencia, Conferencia, Titulo, Conferencista, Horario, Cupo}->
      New_Conferencia_List =  registra_conferencia_servidor(From, Conferencia, Titulo, Conferencista, Horario, Cupo, Conferencia_List),
      server(Asistentes_List, New_Conferencia_List);
    {Conferencia, elimina_conferencia} ->
      New_Conferencia_List = elimina_conferencia_servidor(Conferencia, Conferencia_List),
      server(Asistentes_List, New_Conferencia_List);
    {From, inscritos, Conferencia}->
      asistentes_inscritos_servidor(From, Conferencia, Conferencia_List, Asistentes_List),
      server(Asistentes_List, Conferencia_List);
    {From, inscritas, Asistente}->
      conferencias_inscritas_servidor(From, Asistente, Asistentes_List, Conferencia_List),
      server(Asistentes_List, Conferencia_List);
    {'DOWN', Ref, process, Pid2, Reason} ->
            io:format("client exiting, got ~p~n", [{'DOWN', Ref, process, Pid2, Reason}]),
            New_Asistentes_List = server_killproc(Pid2, Asistentes_List),
            New_Conferencia_List = server_killproc(Pid2, Conferencia_List),
            server(New_Asistentes_List, New_Conferencia_List)

  end.

%% Arrancar el servidor
start_server() ->
  process_flag(trap_exit, true),
  register(sistema, spawn_link(sistema, server, [[], []])).

lista_asistentes() ->
  {sistema, server_node()} ! {asistentes, lista_asistentes}.

lista_conferencias() ->
  {sistema, server_node()} ! {conferencias, lista_conferencias}.

lista_asistentes_servidor(Asistentes_List) ->
  io:format("Asistentes: ~p~n", [Asistentes_List]).

lista_conferencia_servidor(Conferencia_List) ->
  io:format("Conferencias: ~p~n", [Conferencia_List]).

conferencias_inscritas_servidor(From, Asistente, List, OtherList) ->
  Tuple = lists:keyfind(Asistente, 2, List),
  Map =  element(3, Tuple),
  Conferencias = maps:get(conferencias, Map),
  lists:foreach(fun(X) -> display_map(From, X, OtherList) end, Conferencias).

asistentes_inscritos_servidor(From, Conferencia, List, OtherList) ->
  Tuple = lists:keyfind(Conferencia, 2, List),
  Map =  element(3, Tuple),
  Asistentes = maps:get(asistentes, Map),
  lists:foreach(fun(X) -> display_map(From, X, OtherList) end, Asistentes).

display_map(From, Key, List) ->
  Tuple = lists:keyfind(Key, 2, List),
  Map =  element(3, Tuple),
  From ! {sistema, Map}.

%% Se lleva a cabo el registro de Asistentes en el Servidor
registra_asistente_servidor(From, Asistente, Nombre, Asistentes_List) ->
  case lists:keymember(Asistente, 2, Asistentes_List) of
    true ->
      From ! {sistema, stop, user_exists_at_other_node},
      Asistentes_List;
    false ->
      From ! {sistema, registrado},
      [{From, Asistente, #{asistente=>Asistente, nombre=>Nombre, conferencias=>[]}} | Asistentes_List]
  end.

registra_conferencia_servidor(From, Conferencia, Titulo, Conferencista, Horario, Cupo, Conferencia_List) ->
    case lists:keymember(Conferencia, 2, Conferencia_List) of
      true ->
        From ! {sistema, stop, conferencia_exists_at_other_node},
        Conferencia_List;
      false ->
        From ! {sistema, registrado},
        [{From, Conferencia, #{conferencia=>Conferencia, titulo=>Titulo, conferencista=>Conferencista, horario=>Horario, cupo=>Cupo, asistentes=>[]}} | Conferencia_List]
    end.

hay_cupo(Conferencia, List) ->
  Tuple = lists:keyfind(Conferencia, 2, List),
  Map =  element(3, Tuple),
  maps:get(cupo, Map) > length(maps:get(asistentes, Map)).

puede_asistir(Asistente, List) ->
  Tuple = lists:keyfind(Asistente, 2, List),
  Map =  element(3, Tuple),
  3 > length(maps:get(conferencias, Map)).

inscribe_conferencia_asistente(Asistente, Conferencia, List) ->
  Tuple = lists:keyfind(Asistente, 2, List),
  Map =  element(3, Tuple),
  NewConferencias = [Conferencia | maps:get(conferencias, Map)],
  NewMap = maps:update(conferencias, NewConferencias, Map),
  NewTuple = setelement(3, Tuple, NewMap),
  lists:keyreplace(Asistente, 2, List, NewTuple).

inscribe_conferencia_conferencia(Asistente, Conferencia, List) ->
  Tuple = lists:keyfind(Conferencia, 2, List),
  Map =  element(3, Tuple),
  NewAsistentes = [Asistente | maps:get(asistentes, Map)],
  NewMap = maps:update(asistentes, NewAsistentes, Map),
  NewTuple = setelement(3, Tuple, NewMap),
  lists:keyreplace(Conferencia, 2, List, NewTuple).

desinscribe_conferencia_asistente(Asistente, Conferencia, List) ->
  Tuple = lists:keyfind(Asistente, 2, List),
  Map =  element(3, Tuple),
  NewConferencias = lists:delete(Conferencia, maps:get(conferencias, Map)),
  NewMap = maps:update(conferencias, NewConferencias, Map),
  NewTuple = setelement(3, Tuple, NewMap),
  lists:keyreplace(Asistente, 2, List, NewTuple).

desinscribe_conferencia_conferencia(Asistente, Conferencia, List) ->
  Tuple = lists:keyfind(Conferencia, 2, List),
  Map =  element(3, Tuple),
  NewAsistentes = lists:delete(Asistente, maps:get(asistentes, Map)),
  NewMap = maps:update(asistentes, NewAsistentes, Map),
  NewTuple = setelement(3, Tuple, NewMap),
  lists:keyreplace(Conferencia, 2, List, NewTuple).

server_logoff(Asistente, Asistentes_List) ->
  lists:keydelete(Asistente, 2, Asistentes_List).

server_killproc(PID, List) ->
    lists:keydelete(PID, 1, List).

elimina_conferencia_servidor(Conferencia, Conferencia_List) ->
  lists:keydelete(Conferencia, 2, Conferencia_List).

%% Metodos de Asistentes %%
registra_asistente(Asistente, Nombre) ->
  case whereis(Asistente) of
    undefined ->
      process_flag(trap_exit, true),
      PID = spawn_link(sistema, asistente, [server_node(), Asistente, Nombre]),
      register(Asistente, PID),
      {sistema, server_node()} ! {link, PID};
    _ -> already_logged_on
  end.

elimina_asistente(Asistente)->
  case whereis(Asistente) of
    undefined ->
      no_such_asistente;
    _ ->
      Asistente ! {logoff, Asistente}
  end.

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
    {monitor, PID} ->
      monitor(process, PID),
      io:format("link!~n");
    {inscritas, Asistente} ->
      {sistema, Server_Node} ! {self(), inscritas, Asistente},
      await_result();
    {'DOWN', Ref, process, Pid2, Reason} ->
            io:format("Server exiting, got ~p~n", [{'DOWN', Ref, process, Pid2, Reason}]),
            exit(self(), kill);
    {logoff, Asistente} -> %% Falta hacer la eliminacion de eventos por eso se incluye el id del asistente
      {sistema, Server_Node} ! {Asistente, logoff},
      unregister(Asistente),
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
      process_flag(trap_exit, true),
      PID = spawn_link(sistema, conferencia, [server_node(), Conferencia, Titulo, Conferencista, Horario, Cupo]),
      register(Conferencia, PID),
      {sistema, server_node()} ! {link, PID};
    _ -> already_created
  end.

elimina_conferencia(Conferencia) ->
  case whereis(Conferencia) of
    undefined ->
      no_such_conference;
    _ ->
      Conferencia ! {elimina, Conferencia}
  end.

conferencia(Server_Node, Conferencia, Titulo, Conferencista, Horario, Cupo) ->
  {sistema, Server_Node} ! {self(), registra_conferencia, Conferencia, Titulo, Conferencista, Horario, Cupo},
  await_result(),
  conferencia(Server_Node).

asistentes_inscritos(Conferencia) ->
  Conferencia ! {inscritos, Conferencia}.

conferencia(Server_Node) ->
  receive
    {monitor, PID} ->
      monitor(process, PID),
      io:format("link!~n");
    {'DOWN', Ref, process, Pid2, Reason} ->
            io:format("Server exiting, got ~p~n", [{'DOWN', Ref, process, Pid2, Reason}]),
            exit(self(), kill);
    {elimina, Conferencia} -> %% Falta hacer la eliminacion de eventos por eso se incluye el id del asistente
      {sistema, Server_Node} ! {Conferencia, elimina_conferencia},
      unregister(Conferencia),
      exit(normal);
    {inscritos, Conferencia} ->
      {sistema, Server_Node} ! {self(), inscritos, Conferencia},
      await_result()
  end,
  conferencia(Server_Node).
