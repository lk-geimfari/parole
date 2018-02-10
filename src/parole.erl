%%%-------------------------------------------------------------------
%%% @author Likid Geimfari
%%% @copyright (C) 2018, Likid Geimfari
%%% @doc
%%% Simple key-value server based on gen_server and gb_trees.
%%% @end
%%% Created : 08. Feb 2018 10:23 PM
%%%-------------------------------------------------------------------

-module(parole).

-behavior(gen_server).

-export([start/0, add/2, delete/1, lookup/1, get/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

add(Resource, Password) when is_binary(Password) ->
  gen_server:call({global, ?MODULE}, {add, Resource, Password}).

delete(Resource) ->
  gen_server:cast({global, ?MODULE}, {delete, Resource}).

lookup(Resource) ->
  gen_server:call({global, ?MODULE}, {lookup, Resource}).

get(Resource) ->
  gen_server:call({global, ?MODULE}, {get, Resource}).

%%====================================================================
%% Callback Functions
%%====================================================================

init(_Args) ->
  State = gb_trees:empty(),
  {ok, State}.

handle_call({add, Resource, Password}, _From, State) ->
  NewState = gb_trees:insert(Resource, Password, State),
  {reply, ok, NewState};
handle_call({lookup, Resource}, _From, State) ->
  Response = gb_trees:lookup(Resource, State),
  case Response of
    {value, _} -> true;
    _ -> not_found
  end,
  {reply, Response, State};
handle_call({get, Resource}, _From, State) ->
  Response = gb_trees:get(Resource, State),
  {reply, Response, State}.

handle_cast({delete, Resource}, State) ->
  NewState = gb_trees:delete(Resource, State),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.