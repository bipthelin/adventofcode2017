%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 5: A Maze of Twisty Trampolines, All Alike ---
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(five).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/1]).
-export([part2/1]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
part1(FileName) ->
  io:format("~p", [solve_part1(readfile(FileName))]).

part2(FileName) ->
  io:format("~p", [solve_part2(readfile(FileName))]).

%%%_* Private functions ================================================
solve_part1(MazePoints) ->
  solve(1, 0, fun(V) -> V+1 end, init_maze(MazePoints)).

solve_part2(MazePoints) ->
  solve(1, 0, fun(V) when V > 2 -> V-1; (V) -> V+1 end, init_maze(MazePoints)).

solve(N, P, _, Maze) when N > map_size(Maze); N < 1 -> P;
solve(N, P, F, Maze)                                  ->
    solve(N+maps:get(N, Maze), P+1, F, maps:put(N, F(maps:get(N, Maze)), Maze)).

init_maze(MazePoints) ->
  lists:foldl(
    fun(M, Maze) -> maps:put(maps:size(Maze)+1, M, Maze) end,
    maps:new(), MazePoints).

readfile(FileName) ->
  {ok, Data} = file:read_file(FileName),
  lists:map(
    fun(B) -> binary_to_integer(B) end,
      lists:droplast(binary:split(Data, [<<"\n">>], [global]))).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
