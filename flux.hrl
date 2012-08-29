

-type card_type()  :: keeper | goal | rule | action.
-type keeper()     :: { keeper, atom() }.
-type goal()       :: { goal,   atom() }.
-type rule()       :: { rule,   atom() } | goal().
-type action()     :: { action, atom() }.
-type card()       :: keeper() | goal() | rule() | action().
-type player()     :: { player, string(), [card()], [keeper()] }.
-type board()      :: { board, [rule()], [card()], [card()], [player()] }.
-type cover_type() :: draw | play | secret | other.

-record(board, {rules=[] :: [rule()], deck=[] :: [card()], discard=[] :: [card()], players=[] :: [player()]}).
-record(player,{name="Anonymous" :: string(), hand=[] :: [card()], keepers=[] :: [keeper()] }).
