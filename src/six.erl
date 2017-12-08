%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 6: Memory Reallocation ---
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(six).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/0]).
-export([part2/0]).

%%%_* Macros ===========================================================
-define(input, [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
part1() ->
  io:format("~p", [solve_part1(?input)]).

part2() ->
  io:format("~p", [solve_part2([0,13,12,10,9,8,7,5,3,2,1,1,1,10,6,5])]).

%%%_* Private functions ================================================
solve_part1(MemBanks) ->
  solve([MemBanks], 1, MemBanks).

solve_part2(MemBanks) ->
  solve([MemBanks], 1, MemBanks).

solve(History, N, MemBanks) ->
    case lists:member(MemBanks, tl(History)) of
    true  -> {N-1, MemBanks};
    false ->
      {_, Pos, Val} = find_biggest(MemBanks),
      NewMemBanks   = distribute(Pos, Val, MemBanks),
      solve([NewMemBanks | History], N+1, NewMemBanks)
  end.

distribute(Pos, Val, MemBanks) ->
  lists:foldl(
    fun(P, Acc) ->
      PP = circular_pos(Pos+P, length(MemBanks)),
      list_set(Acc, PP, lists:nth(PP, Acc)+1)
    end, list_set(MemBanks, Pos, 0), lists:seq(1, Val)).

circular_pos(P, L) ->
  case P rem L of
      0  -> L;
      NP -> NP
  end.

list_set(List, Pos, Val) ->
  lists:sublist(List, Pos-1)++[Val]++lists:nthtail(Pos, List).

find_biggest(MemBanks) ->
  lists:foldl(
    fun(Mem, {N, _, Acc}) when Mem > Acc -> {N+1, N, Mem};
       (_,   {N, P, Acc})                -> {N+1, P, Acc}
    end,
    {2, 1, hd(MemBanks)}, tl(MemBanks)).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
