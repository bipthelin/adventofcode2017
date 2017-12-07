%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 3: Spiral Memory ---
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(three).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/0]).
-export([part2/0]).

%%%_* Macros ===========================================================
-define(input, 277678).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
part1() ->
  io:format("~p", [solve_part1(?input)]).

part2() ->
  io:format("~p", [solve_part2(?input)]).

%%%_* Private functions ================================================
solve_part1(Input) ->
  {N, DiagLo, DiagHi} = gen_diag(Input, 0),
  Circle              = DiagHi - DiagLo,
  InputCircle         = Input - DiagLo,
  (N - abs(hd([(Circle div 4)*X || X <- lists:seq(1, 4),
                        (Circle div 4)*X >= InputCircle])-InputCircle))+N.

gen_diag(Input, N) ->
  case level(N) > Input of
    true  -> {N, level(N-1), level(N)};
    false -> gen_diag(Input, N+1)
  end.

level(N) -> (2*N+1)*(2*N+1).

solve_part2(Input) ->
  solve_part2(maps:put({0,0}, 1, maps:new()), {1,0,2,1,1}, Input).

solve_part2(_, {_,_,_,_,V}, Input) when V > Input -> V;
solve_part2(Matrix, {X,Y,N,P,_}, Input)           ->
  {XX,YY,NN,PP} = direction(X, Y, N, P+1),
  Val           = sum_val(Matrix, {X,Y}),
  solve_part2(maps:put({X,Y}, Val, Matrix), {XX,YY,NN,PP,Val}, Input).

direction(X, Y, N, P) ->
  case length([C || C <- circle_points(circle_length(N)), P > C]) of
    1 -> {X,   Y+1, N,   P};
    2 -> {X-1, Y,   N,   P};
    3 -> {X,   Y-1, N,   P};
    4 -> {X+1, Y,   N,   P};
    5 -> {X+1, Y,   N+1, 1}
  end.

circle_length(N)        -> ((N+(N-1))-1)*4.
circle_points(Length)   -> [1 | [O * (Length div 4) || O <- lists:seq(1, 4)]].
sum_val(Matrix, {X, Y}) ->
  maps:get({X+1, Y},   Matrix, 0) +
  maps:get({X+1, Y+1}, Matrix, 0) +
  maps:get({X,   Y+1}, Matrix, 0) +
  maps:get({X-1, Y+1}, Matrix, 0) +
  maps:get({X-1, Y},   Matrix, 0) +
  maps:get({X-1, Y-1}, Matrix, 0) +
  maps:get({X,   Y-1}, Matrix, 0) +
  maps:get({X+1, Y-1}, Matrix, 0).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
