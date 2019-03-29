-module(lager_logstash).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-type option() :: {level, lager:log_level()} |
  {host, inet:hostname()} |
  {port, inet:port_number()}.

-record(state, {
  address :: inet:ip_address() | inet:hostname(),
  port    :: inet:port_number(),
  level   :: {'mask', integer()},
  socket  :: gen_udp:socket(),
  type    :: string()
}).

-spec init([option(),...]) -> {ok, #state{}}.
init(Params) ->
  Level        = proplists:get_value(level, Params, debug),
  Host         = proplists:get_value(host, Params, undefined),
  Port         = proplists:get_value(port, Params, undefined),
  Type         = proplists:get_value(type, Params, lager_logstash),

  {ok, Address} = inet:getaddr(Host, inet),
  {ok, Socket} = gen_udp:open(0, [binary,{active,false}]),

  {ok, #state{
    address = Address,
    port    = Port,
    level   = lager_util:level_to_num(Level),
    socket  = Socket,
    type    = Type
  }}.

%% @private
handle_event({log, Message}, State) ->
  case is_loggable(Message, State#state.level) of
    true  ->
      Level = lager_msg:severity(Message),
      Timestamp = timestamp(lager_msg:datetime(Message)),
      Message1 = lager_msg:message(Message),
      Metadata = lager_msg:metadata(Message),
      Data = [
        {type, State#state.type},
        {level, Level},
        {'@timestamp', Timestamp},
        {message, Message1} | Metadata
      ],

      Msg = jiffy:encode({convert(Data)}),

      ok = gen_udp:send(State#state.socket, State#state.address, State#state.port, Msg),

      {ok, State};
    false -> {ok, State}
  end;
handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_call({set_loglevel, Level}, State) ->
  {ok, ok, State#state{level=lager_util:level_to_num(Level)}};
handle_call(get_loglevel, #state{level=Level} = State) ->
  {ok, Level, State};
handle_call(_Request, State) ->
  {ok, ok, State}.

%% @private
handle_info(_Info, State) ->
  {ok, State}.

%% @private
terminate(_, #state{socket=Socket}) ->
  ok = gen_udp:close(Socket);
terminate(_, _) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @private
timestamp({Date, Time}) -> [Date, $T, Time].

%% @private
convert(Data) -> lists:foldl(fun convert/2, [], Data).

%% @private
convert({_, undefined}, Acc) -> Acc;
convert({pid, Pid}, Acc) when is_pid(Pid) ->
  [{pid, list_to_binary(pid_to_list(Pid))} | Acc];
convert({K, List}, Acc) when is_list(List) ->
  [{K, unicode:characters_to_binary(List)} | Acc];
convert({K, Atom}, Acc) when is_atom(Atom) ->
  [{K, atom_to_binary(Atom, latin1)} | Acc];
convert(Else, Acc) -> [Else | Acc].

-spec is_loggable(lager_msg:lager_msg(), non_neg_integer()|{'mask', non_neg_integer()}) -> boolean().
is_loggable(Msg, {mask, Mask}) ->
  (lager_msg:severity_as_int(Msg) band Mask) /= 0;
is_loggable(Msg, SeverityThreshold) ->
  lager_msg:severity_as_int(Msg) =< SeverityThreshold.

