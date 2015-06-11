%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2015, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2015 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(lager_massive_test).

-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

%% API
-export([start_apps/0, start/2, start/3, start_link/3]).
-export([start_trace_main/0, start_trace_lager/0, analyze/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {msg, args, interval}).

%%%===================================================================
%%% API
%%%===================================================================
start_apps() ->
    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),
    lager_massive_test_sup:start_link().

start(Num, Interval) ->
    start(Num, Interval, sample_msg()).

start_trace_lager() ->
    fprof:trace([start, {procs, whereis(lager_event)}]).

analyze() ->
    fprof:trace([stop]),
    fprof:profile(),
    fprof:analyse([totals, no_details]),
    start(0, 100).

start(Num, Interval, Msg) ->
    {NMsg, NArgs} =
        case Msg of
            {M, Args} ->
                {M, Args};
            _ ->
                {Msg, []}
        end,
    case whereis(lager_massive_test_sup) of
        PId when is_pid(PId) ->
            ok;
        undefined ->
            case lager_massive_test_sup:start_link() of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    lager:error("start massive test sup failed ~p", [Reason])
            end
    end,
    fprof:trace([start, {procs, whereis(lager_massive_test_sup)}]),
    lists:foreach(
      fun({_Name, PId, _, _}) ->
              gen_server:cast(PId, stop)
      end, supervisor:which_children(lager_massive_test_sup)),
    lists:foldl(
      fun(_, Acc) ->
        case supervisor:start_child(lager_massive_test_sup, [NMsg, NArgs, Interval]) of
            {ok, _} ->
                Acc;
            {error, R} ->
                [R|Acc]
        end
      end, [], lists:seq(1, Num)).

sample_msg() ->
    {"order ~p failed reason ~p", [{action_order,4779144,{strategy,'strategy_agent@strategy.lk.com',<<"strategy_agent@strategy.lk.com:998f9718f4a25846aa900d548c4a62c9">>},<<"strategy_agent@strategy.lk.com:998f9718f4a25846aa900d548c4a62c9">>,undefined,1,100008,<<"CNY">>,<<"if1506">>,1,1,57530,4,4,2,undefined,undefined,0,undefined,undefined,undefined,3,{strategy,'strategy_agent@strategy.lk.com'},undefined,undefined,1433984657,1433984657}, no_enough_expendable_fund]}.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Msg, Args, Interval) ->
    gen_server:start_link(?MODULE, [Msg, Args, Interval], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Msg, Args, Interval]) ->
    self() ! log,
    {ok, #state{msg = Msg, args = Args, interval = Interval}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(log, #state{msg = Msg, args = Args, interval = Interval} = State) ->
    lager:info(Msg, Args),
    erlang:send_after(Interval, self(), log),
    {noreply, State};
handle_info(Info, State) ->
    lager:error("unexpected info msg ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
