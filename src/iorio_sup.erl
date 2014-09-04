-module(iorio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { iorio_vnode_master,
                  {riak_core_vnode_master, start_link, [iorio_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    WriteFSMs = {iorio_write_fsm_sup,
                 {iorio_write_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [iorio_write_fsm_sup]},
         
    CoverageFSMs = {iorio_coverage_fsm_sup,
                 {iorio_coverage_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [iorio_coverage_fsm_sup]},
    { ok,
        { {one_for_one, 5, 10},
          [VMaster, WriteFSMs, CoverageFSMs]}}.
