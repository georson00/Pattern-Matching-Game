-module(match_game).
-export([start/0, loop/1]).

start() ->
    %Create a random pattern like [b, d, a]
    RandomPattern = generate_pattern(3), 
    io:format("Welcome to the Pattern Matching Game!~n"),
    loop(RandomPattern).

generate_pattern(N) ->
    % Generate a random pattern of length N using the symbols a, b, c, d, e
    List = [a, b, c, d, e],
    generate_random_list(List, N).

generate_random_list(_, 0) -> [];

generate_random_list(Options, N) ->
    % Select a random element from Options and recursively generate the rest
    % of the list until N reaches 0
    [lists:nth(random:uniform(length(Options)), Options) | generate_random_list(Options, N - 1)].

loop(SecretPattern) -> 
    % Loop until the user guesses the pattern or decides to quit
    io:format("Enter your guess (ex: a, b, c):~n"),
    Input = string:trim(io:get_line("")),
    Guess = parse_input(Input),
    case length(Guess) =:= length(SecretPattern) of 
        true ->
            give_feedback(Guess, SecretPattern),
            case Guess =:= SecretPattern of 
                % If the guess matches the secret pattern
                % congratulate the user and end the game
                true -> 
                    io:format("Congratulations! You've guessed the pattern!~n"),
                    %Ask if the user wants to play again
                    io:format("Do you want to play again? (y/n):~n"),
                    ReplayInput = string:trim(io:get_line("")),
                    case ReplayInput of 
                        "y" -> start();
                        _ -> io:format("Thanks for playing!~n")
                    end;

                false -> loop(SecretPattern)
            end;
        false ->
            io:format("❌ Invalid guess pattern length. Try again.~n"),
            loop(SecretPattern)
    end.
parse_input(Input) ->
    % Parse the input string into a list of atoms
    % Example: "a, b, c" -> [a, b, c]
    Tokens = string:tokens(Input, ", []\n"),
    lists:map(fun(Str) -> list_to_atom(string:trim(Str)) end, Tokens).


give_feedback(Guess, SecretPattern) ->
    % Provide feedback on the guess
    Correct = count_correct(Guess, SecretPattern),
    Misplaced = count_misplaced(Guess, SecretPattern),
    io:format("Correct in right place: ~p~n", [Correct]),
    io:format("Correct but misplaced: ~p~n", [Misplaced]).

count_correct([], []) -> 0;
count_correct([G|GT], [S|ST]) when G =:= S ->
    1 + count_correct(GT, ST);
count_correct([_|GT], [_|ST]) ->
    count_correct(GT, ST).

count_misplaced(Guess, SecretPattern) ->
    % Count how many symbols in the guess are correct but misplaced
    % This is done by filtering the guess against the secret pattern
    Matching = lists:filter(fun(E) -> lists:member(E, SecretPattern) end, Guess),
    count_duplicates(Matching) - count_correct(Guess, SecretPattern).

count_duplicates(L) ->
    % Count how many duplicates are in the list
    % This is done by folding over the list and counting each element
    lists:foldl(fun(_,Acc) -> Acc + 1 end, 0, L).


