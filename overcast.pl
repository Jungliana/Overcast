/* Overcast, by AAP. */

/* ------------ Dynamic section --------------- */
:- dynamic started/0.
:- dynamic i_am_at/1, at/2.
:- dynamic in_inventory/1, locked/1, shining/1, alive/1.
:- dynamic wet/1, frozen/1, hot/1.                       % Statuses from spells.
:- discontiguous locked/1, alive/1, immovable/1, wet/1.  % To remove warnings.

/* ----------------- Paths ----------------- */
path(entrance, n, shed).
path(shed, s, entrance).
path(entrance, w, room_south).
path(room_south, e, entrance).
path(room_south, n, room_north).
path(room_north, s, room_south).
path(room_north, w, pond_room).
path(pond_room, e, room_north).
path(pond_room, s, boss_room).
path(boss_room, n, pond_room).

/* -------- Objects in locations -------- */
/* Room Z1 (entrance) */
at(red_bowl, entrance).
at(white_bowl, entrance).
at(green_bowl, entrance).
at(blue_orb, red_bowl).
at(green_orb, green_bowl).
at(golden_ring, entrance).
at(gate, entrance).

/* Room Z1A (shed) */
at(box, shed).
at(blue_bowl, shed).
at(red_orb, box).
at(curious_rat, shed).

/* Room Z2 (south) */
at(fountain, room_south).
at(wooden_torch_holder, room_south).
at(wooden_torch, wooden_torch_holder).
at(cut_rope, room_south).
at(scissors, room_south).
at(confused_rat, room_south).

/* Room Z2 (north) */
at(fancy_fountain, room_north).
at(torch_holder, room_north).
at(torch, torch_holder).
at(rope, room_north).
at(long_plank, room_north).
at(valve, room_north).
at(gateway, room_north).

/* Room Z3 (pond) */
at(pond, pond_room).

/* Room Z4 (boss) */
at(pond, boss_room).
at(guardian, boss_room).
at(exit, boss_room).

/* ------- Objects' starting statuses ------------ */
% immovable - the player cannot take this item.
% locked - it has to be unlocked somehow to proceed.
% shining - on a good way to solve a puzzle.
% alive - mostly rats.

/* Room Z1 (entrance) */
immovable(red_bowl).
immovable(white_bowl).
immovable(green_bowl).
immovable(gate).
locked(gate).
shining(green_bowl).
shining(white_bowl).
frozen(white_bowl).

/* Room Z1A (shed) */
immovable(blue_bowl).
immovable(box).
alive(curious_rat).

/* Room Z2A */
wet(fountain).
immovable(fountain).
immovable(wooden_torch_holder).
immovable(cut_rope).
cut(cut_rope).
alive(confused_rat).

/* Room Z2B */
hot(torch).
immovable(fancy_fountain).
immovable(torch_holder).
immovable(rope).
immovable(valve).
immovable(gateway).
locked(gateway).

/* Room Z3 */
wet(pond).
locked(pond).
immovable(pond).

/* Room Z4 */
alive(guardian).
immovable(exit).

/* --------- Puzzle mechanics --------- */
assert_shining(X) :-
        \+ shining(X),
        assert(shining(X)),
        check_solution, !.

torches :-
        ((at(torch, torch_holder), at(wooden_torch, wooden_torch_holder)); 
        (at(wooden_torch, torch_holder), at(torch, wooden_torch_holder))),
        ((hot(torch), hot(wooden_torch));(not(hot(torch)), not(hot(wooden_torch)))).

fountains :-
        (wet(fountain), wet(fancy_fountain));
        (frozen(fountain), frozen(fancy_fountain));
        ((not(frozen(fountain)), not(wet(fountain))), 
        (not(frozen(fancy_fountain)), not(wet(fancy_fountain)))).

check_solution :-
        locked(gateway),
        (i_am_at(room_north); i_am_at(room_south)),
        cut(rope), cut(cut_rope),
        torches, fountains,
        retractall(locked(gateway)), nl,
        write('Now the two courtyards look like mirror images of each other. '),
        nl, write('The '),
        ansi_format([bold,fg(magenta)], 'gateway', [_]),
        write(' opens so you can visit next part of the Gardens.'), !, nl.

check_solution :-
        locked(gate),
        (i_am_at(entrance); i_am_at(shed)),
        shining(blue_bowl), shining(red_bowl),
        shining(green_bowl), shining(white_bowl),
        retractall(locked(gate)),
        nl, write('The '),
        ansi_format([bold,fg(magenta)], 'gate', [_]),
        write(' opens, inviting you to enter the Gardens.'), !, nl.

check_solution :-
        ((frozen(pond), write(' Great! Now you can walk on (frozen) water!')) ; 
        (at(long_plank, pond), write(' Great! Now you have a weakling-style makeshift bridge!'))),
        retractall(locked(pond)), !, nl.

check_solution :-  % pass quietly even if conditions not satisfied
        true.

/* ------------- Bossfight mechanics ---------------- */


/* --------- Describing places and objects ----------- */
describe(entrance) :- 
        write('You are standing before an entrance gate to the Gardens.'), nl,
        write('The birds are chirping merrily. The wind plays with your hair gently.'), nl,
        write('> Brusto says: "What a nice day to die, isn''t it?"'), !, nl.

describe(shed) :- 
        write('You are in a small shed near the entrance.'), nl,
        write('There are some chests here, but you don''t care. You are not a chest mage.'), !, nl.

describe(confused_rat) :-
        write('> Confused rat says: "Squeak! These two courtyards look nearly the same... It''s so confusing!"'), !, nl.

describe(curious_rat) :-
        write('> Curious rat says: "Squeak! I wonder what makes the stone bowls shine?"'), !, nl.

describe(valve) :-
        write('It''s obviously connected with the fancy fountain.'), !, nl.

describe(room_south) :- 
        write('You are in a small courtyard. You are surrounded by flower beds,'), nl,
        write('but the most eye-catching feature is the large fountain in the center.'), !, nl.

describe(room_north) :- 
        write('You are in another small courtyard. It looks strangely familiar.'), nl,
        write('The richly decorated fountain in the center seems to have been unused for a long time.'), !, nl.

describe(pond_room) :- 
        write('There is a large pond in front of you.'), nl,
        write('> Brusto says: "Bridges are for the weaklings!" and joyfully hovers over the water.'), nl,
        write('It seems that you are the weakling and cannot pass, unless you find another way...'), !, nl.

describe(pond) :- 
        write('> Brusto says: "It''s definitely not a time for a bath."'), !, nl.

describe(X) :- write('It looks like... a '), write(X), write('.'), !, nl.

/* ---------- Take item ---------- */
take(red_orb) :-
        i_am_at(shed),
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
        i_am_at(shed),
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
        immovable(Item),
        i_am_at(Place),
        at(Item, Place),
        write('> Brusto says: "This object is immovable, I can''t take it!"'),
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
inventory :-
        i.

i :-
        write('Brusto shows you the items that you collected: '), nl, fail.

i :-
        in_inventory(Item),
        ansi_format([bold,fg(magenta)], '~w', [Item]), write(', '), fail.

i :-
        write('*end of inventory*.').

/* Use one object */

use(curious_rat) :- describe(curious_rat), !.
use(confused_rat) :- describe(confused_rat), !.

use(valve) :-
        i_am_at(room_north),
        not(wet(fancy_fountain)), not(frozen(fancy_fountain)),
        retractall(hot(fancy_fountain)),
        assert(wet(fancy_fountain)),
        write('You turn the valve and you see the fancy fountain fill with water.'), 
        check_solution, !, nl.

use(valve) :-
        i_am_at(room_north),
        wet(fancy_fountain), not(frozen(fancy_fountain)),
        retractall(wet(fancy_fountain)),
        write('You turn the valve and you see all the water draining from the fancy fountain.'), 
        check_solution, !, nl.

use(valve) :-
        i_am_at(room_north),
        frozen(fancy_fountain),
        write('The water in the fancy fountain is frozen solid! Nothing happens.'), !, nl.

use(X) :-
        ((i_am_at(Place), at(X, Place)) ; in_inventory(X)),
        write('> Brusto says: "I don''t know what you''ve wanted to do with this '),
        ansi_format([bold,fg(magenta)], '~w', [X]), nl,
        write('  but it definitely does NOTHING... at least on our plane of existence."'), !, nl.

use(_) :-
        write('You cannot do that.').

/* Use item on another object */

use(scissors, rope) :-
        in_inventory(scissors),
        i_am_at(room_north),
        not(cut(rope)),
        assert(cut(rope)),
        write('You cut the rope using scissors.'), nl,
        write('Now it looks exactly like the rope in the south courtyard.'), 
        check_solution, !, nl.

use(scissors, rope) :-
        in_inventory(scissors),
        i_am_at(room_north),
        cut(rope),
        write('This rope is already cut.'), !, nl.

use(scissors, cut_rope) :-
        in_inventory(scissors),
        i_am_at(room_south),
        write('This rope is already cut.'), !, nl.

use(X, rope) :-
        in_inventory(X),
        i_am_at(room_north),
        write('Brusto says: "I don''t think we should hang the '),
        ansi_format([bold,fg(magenta)], '~w', [X]),
        write(' on the rope."'), !, nl.

use(X, cut_rope) :-
        in_inventory(X),
        i_am_at(room_south),
        write('Brusto says: "I don''t think we should hang the '),
        ansi_format([bold,fg(magenta)], '~w', [X]),
        write(' on the rope."'), !, nl.

% custom interactions for torches bcs i hate myself
use(torch, wooden_torch) :-
        in_inventory(torch),
        i_am_at(Place),
        at(X, Place),
        at(wooden_torch, X),
        (hot(wooden_torch); hot(torch)),
        assert(hot(wooden_torch)), assert(hot(torch)),
        write('Now both torches are burning.'), nl,
        write('You put the torch back in the magic chest.'), !, nl.

use(wooden_torch, torch) :-
        in_inventory(wooden_torch),
        i_am_at(Place),
        at(X, Place),
        at(torch, X),
        (hot(wooden_torch); hot(torch)),
        assert(hot(wooden_torch)), assert(hot(torch)),
        write('Now both torches are burning.'), nl,
        write('You put the wooden_torch back in the magic chest.'), !, nl.

use(Item, Wet) :-
        hot(Item), in_inventory(Item),
        i_am_at(Place),
        at(Wet, Place), wet(Wet),
        retractall(hot(Item)),
        write('The '),
        ansi_format([bold,fg(magenta)], '~w', [Item]),
        write(' is no longer burning.'), nl,
        write('You put it back in the magic chest.'), !, nl.

use(Item, Other) :-
        in_inventory(Item), immovable(Other),
        i_am_at(Place),
        at(Other, Place),
        retractall(in_inventory(Item)),
        assert(at(Item, Other)),
        write('You place a '), 
        ansi_format([bold,fg(magenta)], '~w', [Item]),
        write(' in a '),
        ansi_format([bold,fg(magenta)], '~w.', [Other]),
        nl, fail.

use(torch, torch_holder) :- i_am_at(room_north), check_solution, !.
use(torch, wooden_torch_holder) :- i_am_at(room_south), check_solution, !.
use(wooden_torch, torch_holder) :- i_am_at(room_north), check_solution, !.
use(wooden_torch, wooden_torch_holder) :- i_am_at(room_south), check_solution, !.

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
        i_am_at(shed),
        at(blue_orb, blue_bowl),
        write('It '),
        ansi_format([bold,fg(yellow)], 'shines', [_]),
        write(' brightly!'), 
        assert_shining(blue_bowl), !, nl.    % Assert shining must be in the last line in this case.

use(long_plank, pond) :-
        i_am_at(pond_room),
        at(long_plank, pond),
        check_solution, !, nl.

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

go(s)  :-
        i_am_at(pond_room),
        locked(pond),
        write('There''s no bridge here! You cannot fly (and probably'), nl, 
        write('swim, that would be too convenient), so you cannot pass.'), !, nl.

go(w)  :-
        i_am_at(entrance),
        locked(gate),
        write('The gate is locked. You have to find a way to open it!'), !, nl.

go(w)  :-
        i_am_at(room_north),
        locked(gateway),
        write('This gateway is closed. You have to unlock it somehow...'), !, nl.

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retractall(i_am_at(Here)),
        assert(i_am_at(There)),
        !, tty_clear, look.

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

examine(Y) :-
        i_am_at(Place),
        at(X, Place),
        at(Y, X),
        describe(Y),
        print_status(Y), !.

examine(X) :-
        i_am_at(Place),
        at(X, Place),
        describe(X),
        notice_objects_at(X),
        print_status(X), !.

examine(X) :-
        in_inventory(X),
        write('You have this very magical artifact in your bottomless chest.'), print_status(X), !, nl.

examine(_) :-
        write('You don''t sense the magic presence of this object here.'), !, nl.


print_status(X) :-
        shining(X), write('The '),
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(yellow)], ' shining', [_]),
        write('! It gives you a good feeling.'), nl, fail.

print_status(pond) :-
        write('The '),
        ansi_format([bold,fg(magenta)], 'pond', [_]), write(' is'),
        ansi_format([bold,fg(blue)], ' full of water', [_]), write('.'), nl, fail.

print_status(pond) :-
        locked(pond), write('It is impossible to cross. Unless you want to drown.'), nl, !.

print_status(X) :-
        locked(X), write('The '),
        ansi_format([bold,fg(magenta)], '~w', [X]),
        write(' is locked.'), nl, fail.

print_status(X) :-
        cut(X), write('The '),
        ansi_format([bold,fg(magenta)], '~w', [X]),
        write(' is cut.'), nl, fail.

print_status(blue_bowl) :-
        wet(blue_bowl), write('The '), 
        ansi_format([bold,fg(magenta)], 'blue bowl', [_]), write(' is'),
        ansi_format([bold,fg(blue)], ' full of water', [_]), write('.'), !, nl.

print_status(fountain) :-
        wet(fountain), write('The '), 
        ansi_format([bold,fg(magenta)], 'fountain', [_]), write(' is'),
        ansi_format([bold,fg(blue)], ' full of water', [_]), write('.'), !, nl.

print_status(fancy_fountain) :-
        wet(fancy_fountain), write('The '), 
        ansi_format([bold,fg(magenta)], 'fancy fountain', [_]), write(' is'),
        ansi_format([bold,fg(blue)], ' full of water', [_]), write('.'), !, nl.

print_status(X) :-
        wet(X), write('The '), 
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(blue)], ' wet', [_]), write('.'), !, nl.

print_status(X) :-
        frozen(X), write('The '), 
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(cyan)], ' frozen', [_]), write('.'), !, nl.

print_status(torch) :-
        hot(torch), write('The '), 
        ansi_format([bold,fg(magenta)], 'torch', [_]), write(' is'),
        ansi_format([bold,fg(red)], ' burning', [_]), write('.'), !, nl.

print_status(wooden_torch) :-
        hot(wooden_torch), write('The '), 
        ansi_format([bold,fg(magenta)], 'wooden torch', [_]), write(' is'),
        ansi_format([bold,fg(red)], ' burning', [_]), write('.'), !, nl.

print_status(red_bowl) :-
        hot(red_bowl), write('The '), 
        ansi_format([bold,fg(magenta)], 'red bowl', [_]), write(' is'),
        ansi_format([bold,fg(red)], ' burning', [_]), write('.'), !, nl.

print_status(X) :-
        hot(X), write('The '), 
        ansi_format([bold,fg(magenta)], '~w', [X]), write(' is'),
        ansi_format([bold,fg(red)], ' hot', [_]), write('.'), !, nl.

print_status(_) :-
        !, nl.

/* ------- All spell casts -------- */
rain(X) :- cast(rain, X).
sunbeam(X) :- cast(sunbeam, X).
frost(X) :- cast(frost, X).

% Not sure whether necessary
cast(_, X) :-
        in_inventory(X),
        write('> Brusto says: "You cannot use spells on items inside me!"'), !, nl.

cast(_, Creature) :-
        alive(Creature),
        write('The '), write(Creature), write(' looks at you disapprovingly.'), nl, fail.

/* rain spell section */
cast(rain, blue_bowl) :-
        i_am_at(shed),
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

cast(rain, torch) :-
        i_am_at(Place),
        at(X, Place),
        at(torch, X),
        hot(torch),
        retractall(hot(torch)),
        write('The torch is no longer lit.'), check_solution, !, nl.

cast(rain, wooden_torch) :-
        i_am_at(Place),
        at(X, Place),
        at(wooden_torch, X),
        hot(wooden_torch),
        retractall(hot(wooden_torch)),
        write('The wooden_torch is no longer lit.'), check_solution, !, nl.

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

cast(rain, fountain) :-
        i_am_at(Place),
        at(fountain, Place),
        assert(wet(fountain)),
        write('The fountain is '),
        ansi_format([bold,fg(blue)], 'full of water', [_]),
        write(' because of rain.'), check_solution, !, nl.

cast(rain, fancy_fountain) :-
        i_am_at(Place),
        at(fancy_fountain, Place),
        assert(wet(fancy_fountain)),
        write('The fancy fountain is '),
        ansi_format([bold,fg(blue)], 'full of water', [_]),
        write(' because of rain.'), check_solution, !, nl.

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

cast(sunbeam, rope) :-
        i_am_at(room_north),
        not(cut(rope)),
        assert(cut(rope)),
        assert(hot(rope)),
        write('The rope catches fire and after a while it is cut, or rather burned in two.'), nl,
        write('Now it looks exactly like the rope in the south courtyard.'),
        check_solution, !, nl.

cast(sunbeam, blue_bowl) :-
        i_am_at(shed),
        shining(blue_bowl),
        wet(blue_bowl),
        \+ at(blue_orb, blue_bowl),
        retractall(shining(blue_bowl)),
        retractall(wet(blue_bowl)),
        write('The water evaporates and the blue bowl stops to shine.'), nl,
        write('Maybe you did something wrong...'), !, nl.

cast(sunbeam, torch) :-
        i_am_at(Place),
        at(X, Place),
        at(torch, X),
        assert(hot(torch)),
        write('The torch is now'),
        ansi_format([bold,fg(red)], ' lit.', [_]), check_solution, !, nl.

cast(sunbeam, wooden_torch) :-
        i_am_at(Place),
        at(X, Place),
        at(wooden_torch, X),
        assert(hot(wooden_torch)),
        write('The wooden torch is now'),
        ansi_format([bold,fg(red)], ' lit.', [_]), check_solution, !, nl.

cast(sunbeam, X) :-
        i_am_at(Place),
        at(X, Place),
        wet(X),
        retractall(wet(X)),
        write('The '), write(X), write(' is no longer wet.'), check_solution, !, nl.

cast(sunbeam, X) :-
        i_am_at(Place),
        at(X, Place),
        frozen(X),
        retractall(frozen(X)),
        write('The '), write(X), write(' is no longer frozen.'), check_solution, !, nl.

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
        i_am_at(shed),
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
        write('The '), write(X), write(' is no longer hot.'), check_solution, !, nl.

cast(frost, X) :-
        i_am_at(Place),
        at(X, Place),
        assert(frozen(X)),
        retractall(wet(X)),
        write('The '), write(X), write(' is now '),
        ansi_format([bold,fg(cyan)], 'frozen.', [_]), check_solution, !, nl.


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
        write('> use(item, [other]) -- to use an object or use an item from your inventory on another object.'), nl,
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
        i_am_at(entrance),
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /        :   |                 |'), nl,
        write('  (~~~~)      /      _  :   |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]    / \\    *|                 |'), nl,
        write('( ~~~~~~)     |     \\_/     |-------   -------|'), nl,
        write('(~~~~~~~~)    |             |                 |'), nl,
        write('  (~~~~~~~~)--|-----   -----|            \\_/  |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |      _      |       '),
        ansi_format([bold,fg(red)], '-O-', [_]),
        write('       |'), nl,
        write('|             |*    / \\    [x]                |'), nl,
        write('|             |     \\_/     |                 |'), nl,
        write('|      ?      \\   :         |  \\_/       \\_/  |'), nl,
        write('|              \\__:_________|_________________|'), nl,
        write('|                                          |'), nl,
        write('|                                          |'), nl,
        write(' \\_____________________________[--]_______/'), nl,
        write('                               |  |'), nl,
        write('     ~~ Gardens of Bloom ~~    |  |'), !, nl.

map :-
        i_am_at(shed),
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /        :   |                 |'), nl,
        write('  (~~~~)      /      _  :   |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]    / \\    *|       '),
        ansi_format([bold,fg(red)], '-O-', [_]),
        write('       |'), nl,
        write('( ~~~~~~)     |     \\_/     |-------   -------|'), nl,
        write('(~~~~~~~~)    |             |                 |'), nl,
        write('  (~~~~~~~~)--|-----   -----|            \\_/  |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |      _      |                 |'), nl,
        write('|             |*    / \\    [x]                |'), nl,
        write('|      ?      |     \\_/     |                 |'), nl,
        write('|             \\   :         |  \\_/       \\_/  |'), nl,
        write('|              \\__:_________|_________________|'), nl,
        write('|                                          |'), nl,
        write('|                                          |'), nl,
        write(' \\_____________________________[--]_______/'), nl,
        write('                               |  |'), nl,
        write('     ~~ Gardens of Bloom ~~    |  |'), !, nl.

map :-
        i_am_at(room_south),
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /        :   |                 |'), nl,
        write('  (~~~~)      /      _  :   |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]    / \\    *|                 |'), nl,
        write('( ~~~~~~)     |     \\_/     |-------   -------|'), nl,
        write('(~~~~~~~~)    |             |                 |'), nl,
        write('  (~~~~~~~~)--|-----   -----|            \\_/  |'), nl,
        write(' / (~~~~~~)   |        '),
        ansi_format([bold,fg(red)], '-O-', [_]),        
        write('  |                 |'), nl,
        write('|     (~~~)   |      _      |                 |'), nl,
        write('|             |*    / \\    [x]                |'), nl,
        write('|             |     \\_/     |                 |'), nl,
        write('|      ?      \\   :         |  \\_/       \\_/  |'), nl,
        write('|              \\__:_________|_________________|'), nl,
        write('|                                          |'), nl,
        write('|                                          |'), nl,
        write(' \\_____________________________[--]_______/'), nl,
        write('                               |  |'), nl,
        write('     ~~ Gardens of Bloom ~~    |  |'), !, nl.

map :-
        i_am_at(room_north),
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /        :   |                 |'), nl,
        write('  (~~~~)      /      _  :   |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]    / \\    *|                 |'), nl,
        write('( ~~~~~~)     |     \\_/     |-------   -------|'), nl,
        write('(~~~~~~~~)    |  '),
        ansi_format([bold,fg(red)], '-O-', [_]),   
        write('        |                 |'), nl,
        write('  (~~~~~~~~)--|-----   -----|            \\_/  |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |      _      |                 |'), nl,
        write('|             |*    / \\    [x]                |'), nl,
        write('|             |     \\_/     |                 |'), nl,
        write('|      ?      \\   :         |  \\_/       \\_/  |'), nl,
        write('|              \\__:_________|_________________|'), nl,
        write('|                                          |'), nl,
        write('|                                          |'), nl,
        write(' \\_____________________________[--]_______/'), nl,
        write('                               |  |'), nl,
        write('     ~~ Gardens of Bloom ~~    |  |'), !, nl.

map :-
        i_am_at(pond_room),
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /        :   |                 |'), nl,
        write('  (~~~~)  '),
        ansi_format([bold,fg(red)], '-O-', [_]), 
        write(' /      _  :   |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]    / \\    *|                 |'), nl,
        write('( ~~~~~~)     |     \\_/     |-------   -------|'), nl,
        write('(~~~~~~~~)    |             |                 |'), nl,
        write('  (~~~~~~~~)--|-----   -----|            \\_/  |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |      _      |                 |'), nl,
        write('|             |*    / \\    [x]                |'), nl,
        write('|             |     \\_/     |                 |'), nl,
        write('|      ?      \\   :         |  \\_/       \\_/  |'), nl,
        write('|              \\__:_________|_________________|'), nl,
        write('|                                          |'), nl,
        write('|                                          |'), nl,
        write(' \\_____________________________[--]_______/'), nl,
        write('                               |  |'), nl,
        write('     ~~ Gardens of Bloom ~~    |  |'), !, nl.

map :-
        tty_clear,
        write('    _________________________^_^_^_^_^_^_^_^_^_'), nl,
        write('   (~~~)       /        :   |                 |'), nl,
        write('  (~~~~)      /      _  :   |  \\_/        [_] |'), nl,
        write('  ( ~~~~)    [x]    / \\    *|                 |'), nl,
        write('( ~~~~~~)     |     \\_/     |-------   -------|'), nl,
        write('(~~~~~~~~)    |             |                 |'), nl,
        write('  (~~~~~~~~)--|-----   -----|            \\_/  |'), nl,
        write(' / (~~~~~~)   |             |                 |'), nl,
        write('|     (~~~)   |      _      |                 |'), nl,
        write('|             |*    / \\    [x]                |'), nl,
        write('|     #__#    |     \\_/     |                 |'), nl,
        write('|    (o  o)   \\   :         |  \\_/       \\_/  |'), nl,
        write('|     |vv|     \\__:_________|_________________|'), nl,
        write('|    {    }                                |'), nl,
        write('|     [][]                                 |'), nl,
        write(' \\_____________________________[--]_______/'), nl,
        write('                               |  |'), nl,
        write('     ~~ Gardens of Bloom ~~    |  |'), !, nl.
