%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 2: Corruption Checksum ---
%%%
%%%      As you walk through the door, a glowing humanoid shape yells in
%%%      your direction. "You there! Your state appears to be idle. Come
%%%      help us repair the corruption in this spreadsheet - if we take
%%%      another millisecond, we'll have to display an hourglass cursor!"
%%%
%%%      The spreadsheet consists of rows of apparently-random numbers.
%%%      To make sure the recovery process is on the right track, they
%%%      need you to calculate the spreadsheet's checksum. For each row,
%%%       determine the difference between the largest value and the
%%%      smallest value; the checksum is the sum of all of these differences.
%%%
%%%      For example, given the following spreadsheet:
%%%
%%%      5 1 9 5
%%%      7 5 3
%%%      2 4 6 8
%%%      The first row's largest and smallest values are 9 and 1, and
%%%      their difference is 8.
%%%      The second row's largest and smallest values are 7 and 3, and their
%%%      difference is 4.
%%%      The third row's difference is 6.
%%%      In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(two).

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
solve_part1(Data) ->
  lists:sum(
    lists:map(
      fun(Line) ->
        L = lists:sort(
              lists:map(
                fun(B) -> binary_to_integer(B) end,
                binary:split(Line, [<<" ">>], [global]))),
        lists:last(L) - hd(L)
      end, Data)).

%% Ouch O(n2)
solve_part2(Data) ->
  lists:sum(
    lists:flatten(
      lists:map(
        fun(Line) ->
          Ints = binary:split(Line, [<<" ">>], [global]),
          [ [i(I) div i(Y) || Y <- Ints, I =/= Y, i(I) rem i(Y) =:= 0]
            || I <- Ints ]
        end, Data))).

readfile(FileName) ->
  {ok, Data} = file:read_file(FileName),
  lists:droplast(binary:split(Data, [<<"\n">>], [global])).

i(B) -> binary_to_integer(B).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
