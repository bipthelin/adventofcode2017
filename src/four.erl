%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 4: High-Entropy Passphrases ---
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(four).

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
solve_part1(PassPhrases) ->
  lists:sum([1 || PP <- PassPhrases, unique_passphrase(PP)]).

solve_part2(PassPhrases) ->
  lists:sum([1 || PP <- PassPhrases, unique_passphrase(PP), no_anagram(PP)]).

no_anagram(PassPhrase) ->
  PP  = binary:split(PassPhrase, [<<" ">>], [global]),
  anagram =/=
    lists:foldl(
      fun(_, anagram) -> anagram;
         (P, PAcc)    ->
           PL = lists:sort(binary_to_list(P)),
           case lists:member(PL, PAcc) of
             true  -> anagram;
             false -> [PL | PAcc]
           end
      end, [], PP).

unique_passphrase(PassPhrase) ->
  PP  = binary:split(PassPhrase, [<<" ">>], [global]),
  PP2 =
    lists:foldl(
      fun(P, PAcc) ->
        case lists:member(P, PAcc) of
          true  -> PAcc;
          false -> [P | PAcc]
        end
      end, [], PP),
  PP =:= lists:reverse(PP2).

readfile(FileName) ->
  {ok, Data} = file:read_file(FileName),
  lists:droplast(binary:split(Data, [<<"\n">>], [global])).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
