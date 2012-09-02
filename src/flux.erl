-module(flux).
-export([play/0, play/1]).
-export([test/0]).
-import(flux_cards).
-import(shuffle,[shuffle/1]).
-include("../include/flux.hrl").

-spec play() -> done.
play() -> play(2).

-spec play(pos_integer()) -> done.
play(N) -> play_(init_board(N)).

-spec play_(board()) -> done.
play_(Board) -> 
    case take_turn(Board) of 
	{ contignue,         Board2 } -> play_(Board2);
	{ win,       Winner, Board3 } -> show(Board3), win_game(Winner);
	{ draw,              Board4 } -> show(Board4), draw_game()
    end.

-spec draw_game() -> done.
draw_game()-> io:format("~n** It's a draw **~n~n"), done.

-spec win_game(player()) -> done.
win_game(Player) -> io:format("~n** ~s won **~n~n", [Player#player.name]), done.

-spec take_turn(board()) -> { draw, board() } | { contignue, board() } | { win, player(), board() }.
take_turn(Board = #board{deck=[]}) -> { draw, Board };    
take_turn(Board)                   -> 
    { Board2, NPlays } = apply_rules(Board),
    handle_plays(Board2, NPlays).

-spec apply_rules(board()) -> { board(), pos_integer() }.
apply_rules(Board) ->
    [ Player | Players ] = Board#board.players,
    D = draw_count(Board, Player),
    L = length(Board#board.deck),
    case L >= D of
        true  -> 
            { Cards, Deck } = lists:split(D, Board#board.deck);
        false -> 
            Cards1 = Board#board.deck,
            Deck1 = shuffle(Board#board.discard),
            { Cards2, Deck2 } = lists:split(D-L, Deck1),
            Cards = Cards1 ++ Cards2,
            Deck = Deck2
    end,
    NewHand = Cards ++ Player#player.hand,
    { Board#board{deck=Deck, players=[Player#player{hand=NewHand}|Players]}, play_count(Board, Player) }. 

-spec draw_count(board(), player()) -> integer().
draw_count(Board, Player) -> 
    Rules = lists:filter( fun(R) -> flux_cards:applies(R, Board, Player) end, Board#board.rules ),
    lists:foldl(fun flux_cards:draw_count/2, 0 ,Rules).

-spec play_count(board(), player()) -> integer().
play_count(Board, Player) -> 
    Rules = lists:filter( fun(R) -> flux_cards:applies(R, Board, Player) end, Board#board.rules ),
    lists:foldl(fun flux_cards:play_count/2, 0 ,Rules).


-spec handle_plays(board(), pos_integer()) -> { contignue, board() } | { win, player(), board() }.
handle_plays(Board, N) -> 
    Cards = pick_plays(Board, N),
    Board2 = lists:foldl(fun play_card/2, Board, Cards),
    [Player|Players] = Board2#board.players,
    Winners = winners(Board2),
    case Winners of
	[]    -> { contignue, Board2#board{players=Players ++ [Player]} }; % Opimize players ring
	[W]   -> { win, W, Board2 };
        _     -> error("More than one Winner")
    end.

% Implement for real, ask player
-spec pick_plays(board(),pos_integer()) -> [card()].
pick_plays(Board, N) ->
    [ Player | _ ] = Board#board.players,
    Hand = Player#player.hand,
    lists:sublist(shuffle(Hand),min(N,length(Hand))).


-spec play_card(card(),board()) -> board().
play_card(Card={keeper, _ }, Board) ->
    [Player|Others] = take_card(Board, Card),
    Keepers2 = [ Card | Player#player.keepers ],
    Board#board{players = [Player#player{keepers=Keepers2}|Others]};

play_card(Card={rule, _Id }, Board) ->
    Players = take_card(Board, Card),
    Rules2 = remove_overridden_rules(Card, Board#board.rules),
    Board#board{rules=Rules2, players=Players};
    
play_card(Card={goal, _Id }, Board) ->
    Players = take_card(Board, Card),
    Rules2 = remove_exess_goals([Card| Board#board.rules]), 
    Board#board{rules=Rules2, players=Players};

play_card(Card={action, _Id }, Board) ->
    Players = take_card(Board, Card),
    % todo implement action
    Discard2 = [ Card | Board#board.discard ],
    Board#board{discard=Discard2, players=Players}.

take_card(Board, Card) ->
    [Player | Others ] = Board#board.players,
    Hand = lists:delete(Card,Player#player.hand),
    [ Player#player{hand=Hand} | Others ].

-spec is_goal(card()) -> boolean().
is_goal({goal, _ }) -> true;
is_goal(_)          -> false.

-spec remove_exess_goals([rule()]) -> [rule()].
remove_exess_goals(Rules) ->
    { Goals, OtherRules } = lists:partition(fun is_goal/1, Rules),
    Max = case lists:any(fun(R) -> R == {rule, twoGoals} end, OtherRules) of 
	true  -> 2;
	false -> 1
    end,
    NewGoals = lists:sublist(Goals,Max),
    NewGoals ++ OtherRules.

-spec remove_overridden_rules(card(),[rule()]) -> [rule()].
remove_overridden_rules(NewRule, InPlay) ->
    NewRules = lists:filter( fun(R) -> not(is_covered_by(R,NewRule)) end, InPlay),
    [NewRule | NewRules].

-spec is_covered_by(rule(), rule()) -> boolean().
is_covered_by({goal,  _}, {goal,   _}) -> true; % not used
is_covered_by({rule,Id1}, {rule, Id2}) -> flux_cards:covers(Id2, Id1);
is_covered_by( _        , _          ) -> false. % goals don't cover rules and vice versa

-spec winners_for_goal(goal(),board()) -> [player()].
winners_for_goal({goal, Id}, Board) ->
    PlayerMeetsGoal = fun (P, Ws) -> case flux_cards:meets(Id, P, Board) of
					true  -> [P|Ws];
					false -> Ws	
				     end
		      end,
    lists:foldl(PlayerMeetsGoal, [], Board#board.players).

-spec winners(board()) -> [player()].
winners(Board) ->
    Goals = lists:filter(fun(R)-> is_goal(R) end, Board#board.rules),
    WhoMeetsGoals = fun(G, Ws) -> case winners_for_goal(G, Board) of
				      []  -> Ws;
				      Ws1 -> Ws1 ++ Ws
				  end
		    end,
    lists:foldl(WhoMeetsGoals, [], Goals).

-spec init_board(pos_integer()) -> board().
init_board(N) -> 
    Ps = [ new_player(I) || I <- lists:seq(1,N) ],
    Board = #board{rules=flux_cards:start_rules(), deck=flux_cards:deck(), players=Ps },
    draw_initial(Board).

-spec new_player(pos_integer()) -> player().
new_player(N) -> #player{name="Player " ++ [ $0 + N ], hand=[], keepers=[] }.

-spec draw_initial(board()) -> board().
draw_initial(Board) -> 
    { Players2, Deck2 } = draw_initial_(Board#board.players, [], Board#board.deck),
    Board#board{deck=Deck2,players=Players2}.
draw_initial_([], Done, Deck) -> 
    { lists:reverse(Done), Deck };
draw_initial_([Player|Players], Done, [C1|[C2|[C3|Deck]]]) ->
    draw_initial_(Players,[Player#player{hand=[C1,C2,C3]}|Done],Deck).

-spec show(board()) -> ok.
show(#board{rules=Rules, deck=Deck, discard=Discard, players=Players}) ->
    io:format("Cards in deck : ~s~n", [show_cards(Deck)]), 
    io:format("Dicard pile   : ~s~n", [show_cards(Discard)]),
    io:format("Rules in play : ~s~n", [show_cards(Rules)]),
    lists:foreach( fun(P) -> show_player(P) end, Players),
    io:nl(),
    io:nl(),
    ok.

-spec show_player(player()) -> ok.
show_player(Player) ->
    io:format("Player~s~n",       [Player#player.name]),
    io:format("  keepers : ~s~n", [show_cards(Player#player.keepers)] ),
    io:format("  hand    : ~s~n", [show_cards(Player#player.hand)] ),
    ok.
		   
-spec type_letter(card_type()) -> char().
type_letter(Type) -> hd(atom_to_list(Type)).

-spec show_cards([card()]) -> string().
show_cards([]) -> "-";     
show_cards(CS) ->
    ShowCard = fun({T,I}) -> lists:flatten(io_lib:format("~c:~p", [ type_letter(T), I ] )) end,
    SS = lists:map(ShowCard, CS),
    SepComma = fun(S,T) -> S ++ ", " ++ T end,
    lists:foldr(SepComma, "", SS).
		      
-spec debug(any()) -> ok.
debug(S) -> io:format("DEBUG: ~p~n", [S]), ok.

-spec press_enter() -> ok.
press_enter() -> io:get_line("Press Enter to contignue"), io:nl(), ok.

