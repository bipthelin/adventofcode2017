%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 1: Inverse Captcha ---
%%%
%%%      You're standing in a room with "digitization quarantine" written in
%%%      LEDs along one wall. The only door is locked, but it includes a small
%%%      interface. "Restricted Area - Strictly No Digitized Users Allowed."
%%%
%%%      It goes on to explain that you may only leave by solving a captcha
%%%      to prove you're not a human. Apparently, you only get one millisecond
%%%      to solve the captcha: too fast for a normal human, but it feels like
%%%      hours to you.
%%%
%%%      The captcha requires you to review a sequence of digits
%%%      (your puzzle input) and find the sum of all digits that match the
%%%      next digit in the list. The list is circular, so the digit after
%%%      the last digit is the first digit in the list.
%%%
%%%      For example:
%%%
%%%        - 1122 produces a sum of 3 (1 + 2) because the first digit (1)
%%%          matches the second digit and the third digit (2) matches
%%%          the fourth digit.
%%%        - 1111 produces 4 because each digit (all 1) matches the next.
%%%        - 1234 produces 0 because no digit matches the next.
%%%        - 91212129 produces 9 because the only digit that matches the
%%%          next one is the last digit, 9.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(one).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/1]).
-export([part2/1]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
part1(Input) ->
  io:format("~p", [solve_part1(Input)]).

part2(Input) ->
  io:format("~p", [solve_part2(Input)]).

%%%_* Private functions ================================================
solve_part1(Input) ->
  element(2, lists:foldl(
               fun(I, {I, S}) -> {I, i(I) + S};
                  (X, {_, S}) -> {X, S}
               end, {hd(lists:reverse(Input)), 0}, Input)).

solve_part2(Input) ->
  {Input1, Input2} = lists:split(length(Input) div 2, Input),
  lists:sum(
    lists:zipwith(
      fun(I, I) -> i(I) * 2;
         (_, _) -> 0
      end, Input1, Input2)).

i(LI) -> list_to_integer([LI]).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
