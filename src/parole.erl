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

-export([start/0, add/2, delete/2, lookup/1, get/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

add(Resource, Password) ->
  gen_server:call({global, ?MODULE}, {add, Resource, Password}).

delete(Resource, Password) ->
  gen_server:call({global, ?MODULE}, {set, Resource, Password}).

lookup(Resource) ->
  gen_server:call({global, ?MODULE}, {lookup, Resource}).

get(Resource) ->
  gen_server:call({global, ?MODULE}, {get, Resource}).

raw_data() ->
  gen_server:call({global, ?MODULE}, {raw}).

%%====================================================================
%% Callback Functions
%%====================================================================

init(_Args) ->
  State = gb_trees:empty(),
  {ok, State}.

handle_call({add, Resource, Password}, _From, State) ->
  NewState = gb_trees:insert(Resource, Password, State),
  {reply, ok, NewState};
handle_call({delete, Resource}, _From, State) ->
  NewState = gb_trees:delete(Resource, State),
  {reply, ok, NewState};
handle_call({lookup, Resource}, _From, State) ->
  Response = gb_trees:lookup(Resource, State),
  {reply, Response, State};
handle_call({get, Resource}, _From, State) ->
  Response = gb_trees:get(Resource, State),
  {reply, Response, State};
handle_call({raw}, _From, State) ->
  Response = gb_trees:to_list(State),
  {reply, Response}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.