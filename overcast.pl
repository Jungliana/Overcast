/* Overcast, by AAP. */

/* ------------ Dynamic section --------------- */
:- dynamic started/0.
:- dynamic i_am_at/1, at/2.
:- dynamic in_inventory/1, locked/1, shining/1, alive/1.
:- dynamic wet/1, frozen/1, hot/1.  % Statuses from spells.
:- discontiguous heavy/1, locked/1, alive/1. % To remove warnings.

:- retractall(started).
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(in_inventory(_)).
:- retractall(wet(_)), retractall(frozen(_)), retractall(hot(_)).


/* ----------------- Paths ----------------- */
path(entrance, n, box_location).
path(box_location, s, entrance).
path(entrance, w, fountain1).
path(fountain1, e, entrance).

/* -------- Objects in locations -------- */
/* Room Z1 (entrance) */
at(red_bowl, entrance).
at(white_bowl, entrance).
at(green_bowl, entrance).
at(blue_orb, red_bowl).
at(green_orb, green_bowl).
at(golden_ring, entrance).
at(gate, entrance).

/* Room Z1A (box_location) */
at(box, box_location).
at(blue_bowl, box_location).
at(red_orb, box).
at(curious_rat, box_location).

/* ------- Objects' starting statuses ------------ */
% heavy - the player cannot take this item.
% locked - it has to be unlocked somehow to proceed.
% shining - on a good way to solve a puzzle.
% alive - mostly rats.

/* Room Z1 (entrance) */
heavy(red_bowl).
heavy(white_bowl).
heavy(green_bowl).
heavy(gate).
locked(gate).
shining(green_bowl).
shining(white_bowl).
frozen(white_bowl).

/* Room Z1A (box_location) */
heavy(blue_bowl).
heavy(box).
alive(curious_rat).

/* --------- Puzzle mechanics --------- */
assert_shining(X) :-
        \+ shining(X),
        assert(shining(X)),
        check_solution, !.

check_solution :-
        locked(gate),
        (i_am_at(entrance); i_am_at(box_location)),
        shining(blue_bowl), shining(red_bowl),
        shining(green_bowl), shining(white_bowl),
        retractall(locked(gate)),
        nl, write('The '),
        ansi_format([bold,fg(magenta)], 'gate', [_]),
        write(' opens, inviting you to enter the Gardens.'), !, nl.

check_solution :-
        true.

/* --------- Describing places and objects ----------- */
describe(entrance) :- 
        write('You are standing before an entrance gate to the Gardens.'), nl,
        write('The birds are chirping merrily. The wind plays with your hair gently.'), nl,
        write('> Brusto says: "What a nice day to die, isn''t it?"'), !, nl.

describe(box_location) :- 
        write('You are in a small shed near the entrance.'), nl,
        write('There are some chests here, but you don''t care. You are not a chest mage.'), !, nl.

describe(X) :- write('It looks like... a '), write(X), write('.'), !, nl.

/* ---------- Take item ---------- */
take(red_orb) :-
        i_am_at(box_location),
        at(red_orb, box),
        retractall(at(red_orb, box)),
        assert(in_inventory(red_orb)),
        write('You take the fiery looking red orb from the box and put it to your bottomless flying chest.'), !, nl.

take(green_orb) :-
        i_am_at(entrance),
        at(green_orb, green_bowl),
        retractall(at(green_orb, green_bowl)),
        retractall(shining(green_bowl)),
        assert(in_inventory(green_orb)),
        write('You take the green orb from the green bowl and put it to your bottomless flying chest.'), nl,
        write('The bowl stops to shine. Maybe you did something wrong...'), !, nl.

take(red_orb) :-
        i_am_at(entrance),
        at(red_orb, red_bowl),
        \+ hot(red_bowl),
        retractall(at(red_orb, red_bowl)),
        retractall(shining(red_bowl)),
        assert(in_inventory(red_orb)),
        write('You take the red orb from the red bowl and put it to your bottomless flying chest.'), nl,
        write('The bowl stops to shine. Maybe you did something wrong...'), !, nl.

take(blue_orb) :-
        i_am_at(box_location),
        at(blue_orb, blue_bowl),
        \+ wet(blue_bowl),
        retractall(at(blue_orb, blue_bowl)),
        retractall(shining(blue_bowl)),
        assert(in_inventory(blue_orb)),
        write('You take the blue orb from the blue bowl and put it to your bottomless flying chest.'), nl,
        write('The bowl stops to shine. Maybe you did something wrong...'), !, nl.

take(Item) :-
        in_inventory(Item),
        write('> Brusto says: "This item is already in my stomach... I mean, storage!"'),
        !, nl.

take(Item) :-
        heavy(Item),
        i_am_at(Place),
        at(Item, Place),
        write('> Brusto says: "This object is too heavy for me!"'),
        !, nl.

take(Creature) :-
        alive(Creature),
        i_am_at(Place),
        at(Creature, Place),
        write('> Brusto says: "I don''t want to store anything alive inside me..."'),
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
        write('The '), write(X), write(' is now in your bottomless flying chest.'),
        !, nl.

take(Y) :-              % Take an item that is nested in another item.
        i_am_at(Place),
        at(X, Place),
        at(Y, X),
        retractall(at(Y, X)),
        assert(in_inventory(Y)),
        write('The '), write(Y), write(' is now in your bottomless flying chest.'),
        !, nl.

take(_) :-
        write('> Brusto says: "I don''t see it here."'),
        nl.



/* Inventory */
i :-
        write('Brusto shows you the items that you collected: '), nl, fail.

i :-
        in_inventory(Item),
        ansi_format([bold,fg(magenta)], '~w', [Item]), write(', '), fail.

i :-
        write('*end of inventory*.').

inventory :-
        i.

/* Use item on another object */

use(Item, Other) :-
        in_inventory(Item), heavy(Other),
        i_am_at(Place),
        at(Other, Place),
        retractall(in_inventory(Item)),
        assert(at(Item, Other)),
        write('You place a '), 
        ansi_format([bold,fg(magenta)], '~w', [Item]),
        write(' in a '),
        ansi_format([bold,fg(magenta)], '~w.', [Other]),
        nl, fail.

use(green_orb, green_bowl) :-
        i_am_at(entrance),
        at(green_orb, green_bowl),
        write('It '),
        ansi_format([bold,fg(yellow)], 'shines', [_]),
        write(' brightly!'), 
        assert_shining(green_bowl), !, nl.   % Assert shining must be in the last line in this case.

use(red_orb, red_bowl) :-
        i_am_at(entrance),
        at(red_orb, red_bowl),
        write('It '),
        ansi_format([bold,fg(yellow)], 'shines', [_]),
        write(' brightly!'), 
        assert_shining(red_bowl), !, nl.   % Assert shining must be in the last line in this case.

use(blue_orb, blue_bowl) :-
        i_am_at(box_location),
        at(blue_orb, blue_bowl),
        write('It '),
        ansi_format([bold,fg(yellow)], 'shines', [_]),
        write(' brightly!'), 
        assert_shining(blue_bowl), !, nl.    % Assert shining must be in the last line in this case.

use(Item, Other) :-
        i_am_at(Place),
        at(Other, Place),
        at(Item, Other),
        write('Nothing seems to happen.'), !, nl.

use(Item, Other) :-
        in_inventory(Item),
        i_am_at(Place),
        at(Other, Place),
        write('You can''t use it here!'), !, nl.

use(Item, _) :-
        \+ in_inventory(Item),
        write('You don''t have a '), write(Item), write(' in your inventory.'), !, nl.

use(_, Other) :-
        i_am_at(Place),
        \+ at(Other, Place),
        write('You don''t sense a '), write(Other), write(' here.'), !, nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

/* This rule tells how to move in a given direction. */

go(w)  :-
        i_am_at(entrance),
        locked(gate),
        write('The gate is locked. You have to find a way to open it!'), !, nl.

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retractall(i_am_at(Here)),
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
        write('You sense a '), 
        ansi_format([bold,fg(magenta)], '~w', [X]),
        write(' here.'), nl,
        fail.

notice_objects_at(_).

examine(curious_rat) :-
        i_am_at(box_location),
        locked(gate),
        write('> Curious rat says: "Squeak! I wonder what makes the stone bowls shine?"'), nl, fail.

examine(X) :-
        i_am_at(Place),
        at(X, Place),
        describe(X),
        notice_objects_at(X),
        print_status(X), !.

examine(X) :-
        in_inventory(X),
        write('You have this very magical artifact in your bottomless chest.'), !, nl.

examine(_) :-
        write('You don''t sense the magic presence of this object here.'), !, nl.


print_status(X) :-
        shining(X), write('The '),
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(yellow)], ' shining', [_]),
        write('! It gives you a good feeling.'), nl, fail.

print_status(X) :-
        locked(X), write('The '), write(X), write(' is locked.'), nl, fail.

print_status(X) :-
        wet(X), write('The '), 
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(blue)], ' wet', [_]), write('.'), !, nl.

print_status(X) :-
        frozen(X), write('The '), 
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(cyan)], ' frozen', [_]), write('.'), !, nl.

print_status(X) :-
        hot(X), write('The '), 
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(red)], ' hot', [_]), write('.'), !, nl.

print_status(_) :-
        !, nl.

/* ------- All spell casts -------- */

% Not sure whether necessary
cast(_, X) :-
        in_inventory(X),
        write('> Brusto says: "You cannot use spells on items inside me!"'), !, nl.

cast(_, Creature) :-
        alive(Creature),
        write('The '), write(Creature), write(' looks at you disapprovingly.'), nl, fail.

/* rain spell section */
cast(rain, blue_bowl) :-
        i_am_at(box_location),
        retractall(hot(blue_bowl)),
        retractall(frozen(blue_bowl)),
        retractall(shining(blue_bowl)),         % In case the same spell is used multiple times.
        assert(wet(blue_bowl)),
        write('You cast a rain spell on the blue bowl.'), nl,
        write('The bowl fills with rainwater and it starts to '),
        ansi_format([bold,fg(yellow)], 'shine', [_]),
        write(' brightly.'), 
        assert_shining(blue_bowl), !, nl.

cast(rain, white_bowl) :-
        i_am_at(entrance),
        shining(white_bowl),
        retractall(shining(white_bowl)),
        write('The white bowl stops to shine. Maybe you did something wrong...'), nl, fail.

cast(rain, red_bowl) :-
        i_am_at(entrance),
        shining(red_bowl),
        \+ at(red_orb, red_bowl),
        hot(red_bowl),
        retractall(shining(red_bowl)),
        retractall(hot(red_bowl)),
        assert(wet(red_bowl)),
        write('The water from the rain extinguishes the fire in the red bowl. It stops to shine.'), nl,
        write('Maybe you did something wrong...'), !, nl.

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
        write('The '), write(X), write(' is '),
        ansi_format([bold,fg(blue)], 'wet', [_]),
        write(' because of rain.'), !, nl.

/* sunbeam spell section */

cast(sunbeam, red_bowl) :-
        i_am_at(entrance),
        retractall(wet(red_bowl)),
        retractall(frozen(red_bowl)),
        retractall(shining(red_bowl)),
        assert(hot(red_bowl)),
        write('You cast a'),
        ansi_format([bold,fg(red)], ' sunbeam ', [_]),
        write('spell on the red bowl.'), nl,
        write('Great, now it''s on fire. But also it '),
        ansi_format([bold,fg(yellow)], 'shines', [_]),
        write(' with its own light.'), 
        assert_shining(red_bowl), !, nl.

cast(sunbeam, white_bowl) :-
        i_am_at(entrance),
        retractall(shining(white_bowl)),
        write('The white bowl stops to shine. Maybe you did something wrong...'), nl, fail.

cast(sunbeam, blue_bowl) :-
        i_am_at(box_location),
        shining(blue_bowl),
        wet(blue_bowl),
        \+ at(blue_orb, blue_bowl),
        retractall(shining(blue_bowl)),
        retractall(wet(blue_bowl)),
        write('The water evaporates and the blue bowl stops to shine.'), nl,
        write('Maybe you did something wrong...'), !, nl.

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
        write('The '), write(X), write(' is now'),
        ansi_format([bold,fg(red)], ' hot.', [_]), !, nl.

/* frost spell section */
cast(frost, white_bowl) :-
        i_am_at(entrance),
        retractall(shining(white_bowl)),        % In case the same spell is used multiple times.
        write('You use your white wand of frost on a white bowl. It '),
        ansi_format([bold,fg(yellow)], 'shines', [_]),
        write(' brightly!'), 
        assert_shining(white_bowl), nl, fail.

cast(frost, red_bowl) :-
        i_am_at(entrance),
        shining(red_bowl),
        hot(red_bowl),
        retractall(shining(red_bowl)),
        retractall(hot(red_bowl)),
        assert(frozen(red_bowl)),
        write('The freezing wind extinguishes the fire in the red bowl. It stops to shine.'), nl,
        write('Maybe you did something wrong...'), !, nl.

cast(frost, blue_bowl) :-
        i_am_at(box_location),
        shining(blue_bowl),
        wet(blue_bowl),
        retractall(shining(blue_bowl)),
        retractall(wet(blue_bowl)),
        assert(frozen(blue_bowl)),
        write('The water freezes and the blue bowl stops to shine.'), nl,
        write('Maybe you did something wrong...'), !, nl.

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
        write('The '), write(X), write(' is now '),
        ansi_format([bold,fg(cyan)], 'frozen.', [_]), !, nl.

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
        tty_clear,
        write('--- Welcome to '),
        ansi_format([bold,fg(magenta)], 'Overcast', [_]),
        write('! You are Vetero, the Weather Mage.'), nl,
        write('The time has come for your final trial - you must travel '), nl,
        write('through the '),
        ansi_format([bold,fg(green)], 'Gardens of Bloom', [_]),
        write(', full of dangers and puzzles.'), nl,
        write('You have three wands:'), nl,
        write('  --> the '),
        ansi_format([bold,fg(blue)], 'blue', [_]),
        write(' wand of rain'), nl,
        write('  --> the '),
        ansi_format([bold,fg(red)], 'red', [_]),
        write(' wand of sunbeam'), nl,
        write('  --> the '),
        ansi_format([underline,fg(white)], 'white', [_]),
        write(' wand of frost'), nl, nl,
        write('Your friend, Brusto the magical bottomless chest, looks at you expectantly. '), nl,
        write('For some reason, it says "true." every time you do something. Just ignore it.'), nl, nl,
        write('> Type `help.` for instructions. '), nl,
        write('> Type `look.` to sense magical objects around you. '), nl.

/* This rule just writes out game instructions. */

help :-
        instructions.

instructions :-
        tty_clear,
        write('Rules of the adventure:'), nl,
        write('> start.             -- to start the game.'), nl,
        write('> n.  s.  e.  w.     -- to go to another room in that direction.'), nl,
        write('> look.              -- to sense magical objects around you.'), nl,
        write('> examine(item).     -- to take a closer look at the item.'), nl,
        write('> take(item).        -- to pick up an item and place it in your bottomless flying chest.'), nl,
        write('> i. inventory.      -- to view the contents of your bottomless flying chest.'), nl,
        write('> use(item, other)   -- to use an item from your inventory on another object.'), nl,
        write('> cast(rain/sunbeam/frost, object) -- to use one of your wands on the chosen object.'), nl,
        write('> map.               -- to show a map.'), nl,
        write('> help.              -- to see this message again.'), nl,
        write('> halt.              -- to end the game and quit.'), nl,
        nl.

start :-
        started,
        write('You''ve already started your trial, Vetero!'), !, nl.

start :-
        assert(started),
        introduction,
        assert(i_am_at(entrance)).


/* Map */
map :-
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /       :    |                 |'), nl,
        write('  (~~~~)      /        :    |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]            |                 |'), nl,
        write('( ~~~~~~)     |             |--------  -------|'), nl,
        write('(~~~~~~~~)    |             |            \\_/  |'), nl,
        write('  (~~~~~~~~)--|-----[ ]-----|                 |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |             |      START      |'), nl,
        write('|             |            [x]                |'), nl,
        write('|             |             |                 |'), nl,
        write('|             \\        :    |  \\_/       \\_/  |'), nl,
        write('|              \\_______:____|_________________|'), nl,
        write('|                    |                        |'), nl,
        write('|                    |                        |'), nl,
        write('|                   [x]                       |'), nl,
        write('\\                    |                        |'), nl,
        write(' \\___________________|____________[--]________|'), nl,
        write('                                  |  |'), nl,
        write('      ~~ Gardens of Bloom ~~      |  |'), nl.
