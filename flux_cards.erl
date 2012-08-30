-module(flux_cards).
-export([covers/2, meets/3, deck/0, start_rules/0, draw_count/2, play_count/2, applies/3]).
-import(shuffle,[shuffle/1]).

-include("flux.hrl").

%% Cards from Fluxx 3.1

-spec start_rules() -> [card()].
start_rules() -> cards(rule, ['Draw 1', 'Play 1'] ).

-spec deck() -> [card()].
deck() -> 
    Keepers = cards( keeper, keepers() ),
    Goals   = cards( goal,   goals() ),
    Rules   = cards( rule,   rules() ), 
    Actions = cards( action, actions() ),
    shuffle(Keepers ++ Goals ++ Rules ++ Actions).

-spec cards(card_type(), [atom()]) -> [card()].
cards(Type, Ids) -> lists:map( fun(Id) -> card(Type, Id) end, Ids).

-spec card(card_type(), atom()) -> card().
card(Type, Id) -> {Type, Id}.

-spec keepers() -> [atom()].
keepers() -> [ 
    'The Brain', 'Bread', 'Chocolate Bar', 'Cookies', 'Death', 'Dreams', 'Love', 'Milk', 'Money', 
    'Moon', 'Peace', 'The Rocket', 'Sleep', 'The Sun', 'The Television', 'Time','The Toaster', 
    'War'
].
     
-spec goals() -> [atom()].
goals() -> [ 
    '10 Cards in Hand', '5 Keepers', 'All You Need is Love', 'The Appliances', 'Baked Goods',
    'Bed Time', 'The Brain (no TV)', 'Chocolate Cookies', 'Chocolate Milk', 'Death by Chocolate',
    'Dreamland', 'Hearts and Minds', 'Hippyism', 'Milk and Cookies', 'Night & Day', 
    'Peace (no War)', 'Rocket Science', 'Rocket to the Moon', 'Squishy Chocolate', 'Time is Money',
    'Toast', 'War = Death', 'Winning the Lottery'
].

-spec rules() -> [atom()].
rules() -> [ 
    'Draw 2', 'Draw 3', 'Draw 4', 'Draw 5', 'Play 2', 'Play 3', 'Play 4', 'Play All', 
    'Hand Limit 0', 'Hand Limit 1', 'Hand Limit 2', 'Keeper Limit 2', 'Keeper Limit 3', 
    'Keeper Limit 4', 'Double Agenda', 'Reverse Order', 'First Play Random', 'No-Hand Bonus',
    'Poor Bonus', 'Rich Bonus', 'X = X + 1'
].

-spec actions() -> [atom()].
actions() -> [ 
    'Discard & Draw', 'Draw 2 and Use \'em', 'Draw 3, play 2 of them', 'Empty the Trash', 
    'Everybody Gets 1', 'Exchange Keepers', 'I Need a Goal (3.0 only)', 'Jackpot', 
    'Let\'s Do That Again!', 'Let\'s Simplify', 'No Limits', 'Rotate Hands', 'Rules Reset',
    'Scramble Keepers', 'Steal a Keeper', 'Take Another Turn', 'Taxation!', 'Trade Hands', 
    'Trash a Keeper', 'Trash a New Rule', 'Use What You Take'
].

-spec meets(atom(), player(), board()) -> boolean().
meets('10 Cards in Hand', Player, Board)   -> most_cards_in_hand(Player, Board);
meets('5 Player', Player, Board)           -> most_keepers(Player, Board);
meets('All You Need is Love', ['Love'], _) -> true;
meets('The Appliances', Player, _)         -> has_both('The Toaster', 'The Television', Player);
meets('Baked Goods', Player, _)            -> has_both('Bread', 'Cookies', Player);
meets('Bed Time', Player, _)               -> has_both('Sleep', 'Time, Player', Player);
meets('The Brain (no TV)', Player, Board)  -> a_but_not_b('The Brain', 'The Television', Player, Board);
meets('Chocolate Cookies', Player, _)      -> has_both('Chocolate Bar', 'Cookies', Player);
meets('Chocolate Milk', Player, _ )        -> has_both('Chocolate Bar', 'Milk', Player);
meets('Death by Chocolate', Player, _)     -> has_both('Chocolate Bar', 'Death', Player);
meets('Dreamland', Player, _ )             -> has_both('Sleep', 'Dreams', Player);
meets('Hearts and Minds', Player, _)       -> has_both('Love', 'The Brain', Player);
meets('Hippyism', Player, _)               -> has_both('Peace', 'Love', Player);
meets('Milk and Cookies', Player, _)       -> has_both('Milk', 'Cookies', Player);
meets('Night & Day', Player, _)            -> has_both('Moon', 'Sun', Player);
meets('Peace (no War)', Player, Board)     -> a_but_not_b('Peace', 'War', Player, Board);
meets('Rocket Science', Player, _)         -> has_both('Rocket', 'Brain', Player);
meets('Rocket to the Moon', Player, _)     -> has_both('Rocket', 'Moon', Player);
meets('Squishy Chocolate', Player, _)      -> has_both('Sun', 'Chocolate Bar', Player);
meets('Time is Money', Player, _)          -> has_both('Time', 'Money', Player);
meets('Toast', Player, _)                  -> has_both('The Brain', 'The Toaster', Player);
meets('War = Death', Player, _)            -> has_both('War', 'Death', Player);
meets('Winning the Lottery', Player, _)    -> has_both('Dreams', 'Money', Player);
meets(_,_,_)                               -> false. 

-spec has_both(atom(), atom(), player()) -> boolean().
has_both(A,B, #player{keepers=Keepers}) -> lists:member(A, Keepers) andalso lists:member(B, Keepers).

% board should not be involved in this file
-spec a_but_not_b(atom(), atom(), player(), board()) -> boolean().
a_but_not_b(A,B, #player{keepers=Keepers}, Board) ->
    AllKeepers = lists:flatmap( fun(P) -> P#player.keepers end, Board#board.players),
    lists:member(A, Keepers) andalso not( lists:member(B,AllKeepers)).

-spec most_cards_in_hand(player(), board()) -> boolean().
most_cards_in_hand(Player, Board) -> is_maximal(Player, Board, fun(P) -> length(P#player.hand) end).

-spec most_keepers(player(), board()) -> boolean().
most_keepers(Player, Board) ->  is_maximal(Player, Board, fun(P) -> length(P#player.keepers) end).

-spec covers(atom(), atom()) -> boolean().
covers(X, X)           -> true;
covers(X, Y)           -> cover_type(X) =:= cover_type(Y) andalso cover_type(X) =/= other.

-spec cover_type(atom()) -> cover_type().
cover_type('Draw 1')   -> draw;
cover_type('Draw 2')   -> draw;
cover_type('Draw 3')   -> draw;
cover_type('Draw 4')   -> draw;
cover_type('Draw 5')   -> draw;

cover_type('Play 1')   -> play;
cover_type('Play 2')   -> play;
cover_type('Play 3')   -> play;
cover_type('Play 4')   -> play;
cover_type('Play All') -> play;

cover_type(governmentCoverUp) -> secret;
cover_type(secretData)        -> secret;
     
cover_type(_)       -> other.


-spec draw_count(rule(), non_neg_integer() ) -> non_neg_integer().
draw_count({rule, 'Draw 1'        }, _) -> 1;
draw_count({rule, 'Draw 2'        }, _) -> 2;
draw_count({rule, 'Draw 3'        }, _) -> 3;
draw_count({rule, 'Draw 4'        }, _) -> 4;
draw_count({rule, 'Draw 5'        }, _) -> 5;
draw_count({rule, 'X = X + 1'     }, N) -> N+1;
draw_count({rule, 'Poor Bonus'    }, N) -> N+1; % todo condition
draw_count({rule, 'No-Hand Bonus' }, N) -> 3+N; % todo condition
draw_count({rule, _               }, N) -> N;
draw_count({goal, _               }, N) -> N.


-spec play_count(rule(), non_neg_integer() ) -> non_neg_integer().
play_count({rule, 'Play 1'            }, _) -> 1;
play_count({rule, 'Play 2'            }, _) -> 2;
play_count({rule, 'Play 3'            }, _) -> 3;
play_count({rule, 'Play 4'            }, _) -> 4;
play_count({rule, 'Play 5'            }, _) -> 5;
play_count({rule, 'X = X + 1'         }, N) -> N+1;
play_count({rule, 'Rich Bonus'        }, N) -> N+1; 
play_count({rule, 'First Play Random' }, _) -> 2;
play_count({rule, _                   }, N) -> N.

-spec applies(rule(), board(), player()) -> boolean().
applies({rule, 'No-Hand Bonus'}, _     , Player) -> 0 == length(Player#player.hand);
applies({rule, 'Poor Bonus'   }, Board , Player) -> is_minimal(Player, Board, fun(P) -> length(P#player.keepers) end );
applies({rule, 'Rich Bonus'   }, Board , Player) -> is_maximal(Player, Board, fun(P) -> length(P#player.keepers) end );
applies( _                     ,  _    , _     ) -> true.
                            
-spec is_maximal(player(), board(), fun( ( player() ) -> integer() ) ) -> boolean().
is_maximal(Player, Board, Property) ->
    N = Property(Player),
    M = lists:foldl(fun (P,Mn) -> max(Mn, Property(P)) end, 0, Board#board.players),
    N > M. 

-spec is_minimal(player(), board(), fun( ( player() ) -> integer() ) ) -> boolean().
is_minimal(Player, Board, Property) ->
    N = Property(Player),
    M = lists:foldl(fun (P,Mn) -> min(Mn, Property(P)) end, 0, Board#board.players),
    N < M. 
