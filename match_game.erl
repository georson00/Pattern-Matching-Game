-module(match_game).
-export([start/0, loop/3]).


%%Entry point for the game, starts the game with a random pattern
start() ->
    io:format("Welcome to the Pattern Matching Game!~n"),
    io:format("You will try to guess a hidden sequence of letters.~n"),
    io:format("Available letters: a, b, c, d, e~n"),
    io:format("Enter desired pattern length (e.g., 3, 4, or 5):~n"),
    {ok, [Length]} = io:fread("", "~d"),

    %% Dynamically create example guess format
    ExampleLetters = lists:sublist([a, b, c, d, e], Length),
    ExampleStr = string:join([atom_to_list(X) || X <- ExampleLetters], ","),
    io:format("Type your guesses like: ~s~n", [ExampleStr]),

    RandomPattern = generate_pattern(Length),
    loop(RandomPattern, 0, []).


%% Generate a random list of N atoms from a fixed pool
generate_pattern(N) ->
    List = [a, b, c, d, e],
    generate_random_list(List, N).



%% Recursively build a list of N random atoms
generate_random_list(_, 0) -> [];
generate_random_list(Options, N) ->
  
    [lists:nth(random:uniform(length(Options)), Options) | generate_random_list(Options, N - 1)].



%% Main game loop: read user input, check guess, give feedback, repeat
loop(SecretPattern, AttemptCount, GuessHistory) -> 
    io:format("Enter your guess: ~n "),
    Input = string:trim(io:get_line("")), %% Get user input as string
    Guess = parse_input(Input),           %% Parse string into list of atoms
    case length(Guess) =:= length(SecretPattern) of 
        true ->
            NewAttemptCount = AttemptCount + 1,
            UpdatedHistory = GuessHistory ++ [Guess],
            give_feedback(Guess, SecretPattern),
            %% Check if user guessed correctly
            case Guess =:= SecretPattern of 
                true -> 
                    io:format("🎉 Congratulations! You've guessed the pattern in ~p attempts!~n", [NewAttemptCount]),
                    io:format("Your guesses were: ~p~n", [UpdatedHistory]),
                    ask_replay();   %% Ask if user wants to play again
                false -> 
                    loop(SecretPattern, NewAttemptCount, UpdatedHistory)  %% Continue the game
            end;
        false ->
            io:format("❌ Invalid guess length. Please match the pattern length of ~p.~n", [length(SecretPattern)]),
            loop(SecretPattern, AttemptCount, GuessHistory)
    end.



%% Parse a string like "a,b,c" into [a,b,c]
parse_input(Input) ->
    Tokens = string:tokens(Input, ", []\n"),
    lists:map(fun(Str) -> list_to_atom(string:trim(Str)) end, Tokens).



%% Provide feedback to the user about their guess
give_feedback(Guess, SecretPattern) ->
    Correct = count_correct(Guess, SecretPattern),
    Misplaced = count_misplaced(Guess, SecretPattern),
    io:format("Correct in right place: ~p~n", [Correct]),
    io:format("Correct but misplaced: ~p~n", [Misplaced]).



%% Count how many elements are correct and in the right position
count_correct([], []) -> 0;
count_correct([G|GT], [S|ST]) when G =:= S ->
    1 + count_correct(GT, ST);
count_correct([_|GT], [_|ST]) ->
    count_correct(GT, ST).



%% Count how many elements are correct but in the wrong position
count_misplaced(Guess, SecretPattern) ->
    Matching = lists:filter(fun(E) -> lists:member(E, SecretPattern) end, Guess),
    count_duplicates(Matching) - count_correct(Guess, SecretPattern).



%% Count how many items are in a list (basic fold)
count_duplicates(L) ->
    lists:foldl(fun(_,Acc) -> Acc + 1 end, 0, L).


%% Ask the user if they want to play again
ask_replay() ->
    io:format("Do you want to play again? (y/n):~n"),
    Answer = string:trim(io:get_line("")),
    case Answer of 
        "y" -> start();
        _ -> io:format("Thanks for playing!~n")
    end.

