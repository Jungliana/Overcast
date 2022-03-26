/* Overcast, by AAP. */

:- dynamic started/0.
:- dynamic i_am_at/1, at/2.
:- dynamic in_inventory/1.          % Status for objects in inventory.
:- dynamic wet/1, frozen/1, hot/1.  % Statuses from spells.

:- retractall(started).
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(in_inventory(_)).
:- retractall(wet(_)), retractall(frozen(_)), retractall(hot(_)).


/* ------- Paths --------- */
path(entrance, n, box_location).
path(box_location, s, entrance).

/* -------- Objects in locations -------- */
at(blue_bowl, entrance).
at(red_bowl, entrance).
at(white_bowl, entrance).
at(green_bowl, entrance).
at(golden_ring, entrance).
at(box, box_location).

/* ------- Immovable objects ------------ */
heavy(box).
heavy(blue_bowl).
heavy(red_bowl).
heavy(white_bowl).
heavy(green_bowl).

/* --------- Describing places and objects ----------- */
describe(entrance) :- write('You are standing before an entrance to the Gardens.'), !, nl.
describe(X) :- write('It looks like... a '), write(X), write('.'), !, nl.

/* ---------- Take item ---------- */
take(Item) :-
        in_inventory(Item),
        write('This item is already in your inventory!'),
        !, nl.

take(Item) :-
        heavy(Item),
        write('This object is too heavy!'),
        !, nl.

take(golden_ring) :-
        i_am_at(entrance),
        at(golden_ring, entrance),
        retractall(at(golden_ring, entrance)),
        assert(in_inventory(golden_ring)),
        write('Nice find!'), !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retractall(at(X, Place)),
        assert(in_inventory(X)),
        write('The '), write(X), write(' is now in your inventory.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* Inventory */
i :-
        in_inventory(Item),
        write(Item), write(','), fail.

i :-
        write(' *end of inventory*.').

inventory :-
        in_inventory(Item),
        write(Item), write(','), fail.

inventory :-
        write(' *end of inventory*.').


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

examine(X) :-
        i_am_at(Place),
        at(X, Place),
        describe(X),
        print_status(X), !.

print_status(X) :-
        wet(X), write('The '), write(X), write(' is wet.'), !, nl.

print_status(X) :-
        frozen(X), write('The '), write(X), write(' is frozen.'), !, nl.

print_status(X) :-
        hot(X), write('The '), write(X), write(' is hot.'), !, nl.

print_status(_) :-
        !, nl.

/* ------- All spell casts -------- */

% Not sure whether necessary
cast(_, X) :-
        in_inventory(X),
        write('You cannot use spells on items inside your inventory!'), !, nl.

/* rain spell section */
cast(rain, X) :-
        i_am_at(Place),
        at(X, Place),
        hot(X),
        retractall(hot(X)),
        write('The '), write(X), write(' is no longer hot.'), !, nl.

cast(rain, X) :-
        i_am_at(Place),
        at(X, Place),
        frozen(X),
        retractall(frozen(X)),
        write('The '), write(X), write(' is no longer frozen.'), !, nl.

cast(rain, X) :-
        i_am_at(Place),
        at(X, Place),
        assert(wet(X)),
        write('The '), write(X), write(' is wet because of rain.'), !, nl.

/* sunbeam spell section */
cast(sunbeam, X) :-
        i_am_at(Place),
        at(X, Place),
        wet(X),
        retractall(wet(X)),
        write('The '), write(X), write(' is no longer wet.'), !, nl.

cast(sunbeam, X) :-
        i_am_at(Place),
        at(X, Place),
        frozen(X),
        retractall(frozen(X)),
        write('The '), write(X), write(' is no longer frozen.'), !, nl.

cast(sunbeam, X) :-
        i_am_at(Place),
        at(X, Place),
        assert(hot(X)),
        write('The '), write(X), write(' is now hot.'), !, nl.

/* frost spell section */
cast(frost, X) :-
        i_am_at(Place),
        at(X, Place),
        hot(X),
        retractall(hot(X)),
        write('The '), write(X), write(' is no longer hot.'), !, nl.

cast(frost, X) :-
        i_am_at(Place),
        at(X, Place),
        assert(frozen(X)),
        retractall(wet(X)),
        write('The '), write(X), write(' is now frozen.'), !, nl.

/* wrong usage section */
cast(tp, _) :-
        write('Teleportation... coming soon in a DLC.'), !, nl.

cast(_, _) :-
        write('It doesn''t work!'), !, nl.



/* This rule tells how to die. */

die :-
        finish.

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.

/* This rule prints an introduction to the game. */
introduction :-
        nl,
        write('--- Welcome to Overcast! You are Vetero, the Weather Mage.'), nl,
        write('The time has come for your final trial - you must travel '), nl,
        write('through the Gardens of Bloom, full of dangers and puzzles.'), nl,
        write('You have three wands:'), nl,
        write('  --> the blue wand of rain'), nl,
        write('  --> the red wand of sunbeam'), nl,
        write('  --> the white wand of frost'), nl,
        write('But your magic is not unlimited... '), nl.

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax. '), nl,
        write('Rules of the adventure:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(item).        -- to pick up an item and place it in your inventory.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('use(item, other)   -- to use an item from your inventory on another object.'), nl,
        write('cast(rain/sunbeam/frost, object) -- to use one of your wands on the chosen object.'), nl,
        write('map.               -- to show a map.'), nl,
        write('mana.              -- to show your mana usage.'), nl, 
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.

start :-
        started,
        write('You''ve already started your trial, Vetero!'), !, nl.

start :-
        assert(started),
        introduction,
        instructions,
        assert(i_am_at(entrance)),
        look.


/* Map */
map :-
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /       :    |                 |'), nl,
        write('  (~~~~)      /        :    |             [_] |'), nl,
        write('  ( ~~~~)    [x]            |                 |'), nl,
        write('( ~~~~~~)     |             |--------  -------|'), nl,
        write('(~~~~~~~~)    |             |  \\_/       \\_/  |'), nl,
        write('  (~~~~~~~~)--|-----[ ]-----|                 |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |             |      START      |'), nl,
        write('|             |            [x]                |'), nl,
        write('|             |             |                 |'), nl,
        write('|             \\        :    |  \\_/       \\_/  |'), nl,
        write('|              \\_______:____|_________________|'), nl,
        write('|                    |                        |'), nl,
        write('|                    |       O                |'), nl,
        write('|                   [x]     /0\\               |'), nl,
        write('\\                    |      / \\               |'), nl,
        write(' \\___________________|____________[--]________|'), nl,
        write('                                  |  |'), nl,
        write('                                  |  |'), nl.
