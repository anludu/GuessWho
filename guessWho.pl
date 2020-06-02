/**Guess Who! A Prolog gessing game.
*   Copyright (C) 2020  Ana Lucía Dueñas Chávez
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*
*  contact: analup250@gmail.com
*/


% Predicates for the guest who game.
% [alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan tom].

eyes(anita, blue).
eyes(robert, blue).
eyes(george, brown).
eyes(bernard, brown).
eyes(herman, brown).
eyes(philip, brown).
eyes(david, brown).
eyes(claire, brown).
eyes(richard, brown).
eyes(alfred, blue).
eyes(eric, brown).
eyes(max, brown).
eyes(joe, brown).
eyes(frans, brown).
eyes(charles, brown).
eyes(anne, brown).
eyes(peter, blue).
eyes(sam, brown).
eyes(tom, blue).
eyes(maria, brown).
eyes(paul, brown).
eyes(alex, brown).
eyes(susan, brown).
eyes(bill, brown).


hair(anita, blond).
hair(robert, brown).
hair(george, white).
hair(bernard, brown).
hair(herman,red).
hair(philip,black).
hair(david,blond).
hair(claire, red).
hair(richard, bald).
hair(alfred, red).
hair(eric, blond).
hair(max, black).
hair(joe, brown).
hair(frans, brown).
hair(charles, brown).
hair(anne, black).
hair(peter, white).
hair(sam, bald).
hair(tom, bald).
hair(maria, brown).
hair(paul, brown).
hair(alex, black).
hair(susan, white).
hair(bill, bald).

gender(anita, female).
gender(robert, male).
gender(george, male).
gender(bernard, male).
gender(herman, male).
gender(philip, male).
gender(david, male).
gender(claire, female).
gender(richard, male).
gender(alfred, male).
gender(eric, male).
gender(max, male).
gender(joe, male).
gender(frans, male).
gender(charles, male).
gender(anne, female).
gender(peter, male).
gender(sam, male).
gender(tom, male).
gender(maria, female).
gender(paul, male).
gender(alex, male).
gender(susan, female).
gender(bill, male).

hat(anita, no).
hat(robert, no).
hat(george, yes).
hat(bernard, yes).
hat(herman, no).
hat(philip, no).
hat(david, no).
hat(claire, yes).
hat(richard, no).
hat(alfred, no).
hat(eric,yes).
hat(max, no).
hat(joe, no).
hat(frans, no).
hat(charles, no).
hat(anne, no).
hat(peter, no).
hat(sam, no).
hat(tom, no).
hat(maria, yes).
hat(paul, no).
hat(alex, no).
hat(susan, no).
hat(bill, no).

smiling(anita, yes).
smiling(robert, no).
smiling(george, no).
smiling(bernard, no).
smiling(herman, no).
smiling(philip, yes).
smiling(david, yes).
smiling(claire, yes).
smiling(richard, no).
smiling(alfred, no).
smiling(eric, yes).
smiling(max, yes).
smiling(joe, yes).
smiling(frans, yes).
smiling(charles, yes).
smiling(anne, no).
smiling(peter, yes).
smiling(sam, yes).
smiling(tom, yes).
smiling(maria, yes).
smiling(paul, yes).
smiling(alex, yes).
smiling(susan, yes).
smiling(bill, yes).

nose(anita, small).
nose(robert, big).
nose(george, small).
nose(bernard, big).
nose(herman, big).
nose(philip, small).
nose(david, small).
nose(claire, small).
nose(richard, small).
nose(alfred, small).
nose(eric, small).
nose(max, big).
nose(joe, small).
nose(frans, small).
nose(charles, small).
nose(anne, big).
nose(peter, big).
nose(sam, small).
nose(tom, small).
nose(maria, small).
nose(paul, small).
nose(alex, small).
nose(susan, small).
nose(bill, small).

glasses(anita, no).
glasses(robert, no).
glasses(george, no).
glasses(bernard, no).
glasses(herman, no).
glasses(philip, no).
glasses(david, no).
glasses(claire, yes).
glasses(richard, no).
glasses(alfred, no).
glasses(eric, no).
glasses(max , no).
glasses(joe , yes).
glasses(frans , no).
glasses(charles , no).
glasses(anne , no).
glasses(peter , no).
glasses(sam , yes).
glasses(tom , yes).
glasses(maria , no).
glasses(paul , yes).
glasses(alex , no).
glasses(susan , no).
glasses(bill , no).

question(gender, male,  "Is it a Man?").
question(gender, female,  "Is it a Woman?").
question(eyes, blue,  "Does he/she have blue eyes?").
question(eyes, brown,  "Does he/she have brown eyes?").
question(eyes, black,  "Does he/she have black eyes?").
question(hair, red,  "Does he/she have red hair?").
question(hair, white,  "Does he/she have white hair?").
question(hair, brown,  "Does he/she have brown hair?").
question(hair, blond,  "Does he/she have blond hair?").
question(hair, black,  "Does he/she have black hair?").
question(hair, bald,  "Is he bald?").
question(hat, yes,  "Is he/she wearing hat?").
question(glasses, yes, "Is he/she wearing glasses?").
question(smiling, yes, "Is he/she smiling?").
question(nose, yes, "Does he/she have big nose" ).
%["Is it a Man?", "Is it a Woman?", "Does he/she have blue eyes?", "Does he/she have brown eyes?", "Does he/she have black eyes?", "Does he/she have red hair?", "Does he/she have white hair?", "Does he/she have brown hair?", "Does he/she have blond hair?", "Does he/she have black hair?", "Is he bald?", "Is he/she wearing hat?", "Is he/she wearing glasses?", "Is he/she smiling?", "Does he/she have big nose"].
%Pick the computer character to play the game from a list of all the characters.
%Ex: chooseRandomly([alex, alfred, anita, anne, bernard, bill, sam, susan, tom], Suspect).
%Ex: count([alex, alfred, anita, anne, bernard, bill, sam, susan, tom], Suspect).

chooseRandomly([H|T], Suspect):-
    length([H|T], ElementsInList),
    random_between(1, ElementsInList, X),
    nth1(X, [H|T], Suspect);
    nth1(X, [H|T], Suspect).

removeQuestion(_, [], []).
removeQuestion(X, [X|T], L):- removeQuestion(X, T, L), !.
removeQuestion(X, [H|T], [H|L]):- removeQuestion(X, T, L ).
% removeQuestion(alex, [alex, alfred, anita, anne, bernard, bill], X).

is_in_it(Item,[Item|_Rest]).
is_in_it(Item,[_DisregardHead|Tail]):-is_in_it(Item,Tail).
% is_in_it(anne, [alex, alfred, anita, bernard, bill]).

analize_characters_positive(Question, [], []).

%TO know of the characters fit the user answer, the result is a list with the suspects that match the answer of the question.
analize_characters_positive(Question, [H|T], [H|B]):-
%gets the predicate and the answer that belongs to the question.
  question(Category, Answer, Question),
%In the list of remaining characters search for suspects that match the answer of the question.
  call(Category, H, Answer),
%keeps doing it until the Character list
  analize_characters_positive(Question, T, B).

analize_characters_positive(Question, [H|T], B):- analize_characters_positive(Question, T, B).

%analize_characters_positive("Is it a Woman?", [alex, alfred, anita, anne, bernard, bill], X).

analize_characters_negative(Question, [], []).

%TO know of the characters fit the user answer, the result is a list with the suspects that match the answer of the question.
analize_characters_negative(Question, [H|T], B):-
%gets the predicate and the answer that belongs to the question.
  question(Category, Answer, Question),
%In the list of remaining characters search for suspects that match the answer of the question.
  call(Category, H, Answer),
%keeps doing it until the Character list
  analize_characters_negative(Question, T, B).

analize_characters_negative(Question, [H|T], [H|B]):-
  analize_characters_negative(Question, T, B).
%analize_characters_negative("Is it a Woman?", [alex, alfred, anita, anne, bernard, bill], X).
%ask("Is it a Woman?").
/* how to ask questions */
ask(Question) :-
    write(Question),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :-
   (yes(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

/* undo all yes/no assertions */
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.


let_them_ask(Question, Character):-
  question(Category, Answer, Question),
  (call(Category, Character, Answer)->
  true), write(yes), nl, !.

let_them_ask(Question, Character):-
  write(no), nl, fail, !.

guess_who_Computer():-
  chooseRandomly([alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom], MyCharacter),
  game(MyCharacter, [alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom]).

game(MyCharacter, CharactersLeft):-
  ask("Do you know who my Character is?"),
  write("Who is it then?"),
  read(Ans),
  verify_wining_condition(MyCharacter, Ans), !.

game(MyCharacter, CharactersLeft):-
  write("What is you question?"),
  read(UsersQuestion),
  helper_guess_who(MyCharacter, CharactersLeft, UsersQuestion).

helper_guess_who(MyCharacter, CharactersLeft, UsersQuestion):-
  let_them_ask(UsersQuestion, MyCharacter),
  analize_characters_positive(UsersQuestion, CharactersLeft, UpdateCharactersLeft),
  write(UpdateCharactersLeft),
  nl,
  game(MyCharacter, UpdateCharactersLeft);
  analize_characters_negative(UsersQuestion, CharactersLeft, UpdateCharactersLeft),
  write(UpdateCharactersLeft),
  nl,
  game(MyCharacter, UpdateCharactersLeft).

verify_wining_condition(MyCharacter, UserGuess):-
  MyCharacter == UserGuess,
  write("Yes, my character was "), write(MyCharacter);
  MyCharacter \= UserGuess,
  write("No, my character was "), write(MyCharacter);

characters([alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom]).
questions(["Is it a Man?", "Is it a Woman?", "Does he/she have blue eyes?", "Does he/she have brown eyes?", "Does he/she have black eyes?", "Does he/she have red hair?", "Does he/she have white hair?", "Does he/she have brown hair?", "Does he/she have blond hair?", "Does he/she have black hair?", "Is he bald?", "Is he/she wearing hat?", "Is he/she wearing glasses?", "Is he/she smiling?", "Does he/she have big nose"]).

guess_who(ToGuessCharacter):-
  guess_who2(ToGuessCharacter, ["Is it a Man?", "Is it a Woman?", "Does he/she have blue eyes?", "Does he/she have brown eyes?", "Does he/she have black eyes?", "Does he/she have red hair?", "Does he/she have white hair?", "Does he/she have brown hair?", "Does he/she have blond hair?", "Does he/she have black hair?", "Is he bald?", "Is he/she wearing hat?", "Is he/she wearing glasses?", "Is your he/she smiling?", "Does he/she have big nose"], [alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom], CurrentGuess).

guess_who2(ToGuessCharacter, [H|T], [A|B], CurrentGuess):-
  A == CurrentGuess, CurrentGuess == ToGuessCharacter, write("Your character is "), write(CurrentGuess), !.

guess_who2(ToGuessCharacter, [H|T], [A|B], CurrentGuess):-
  chooseRandomly([H|T], X),
  delete([H|T], X, NewQuestionList),
  tellThem(ToGuessCharacter, X, NewQuestionList, [A|B], CurrentGuess).

tellThem(ToGuessCharacter, X, NewQuestionList, [A|B], CurrentGuess):-
  ask(X),
  analize_characters_positive(X, [A|B], [H|T]),
  guess_who2(ToGuessCharacter,NewQuestionList, [H|T], H).

tellThem(ToGuessCharacter, X, NewQuestionList, [A|B], CurrentGuess):-
  analize_characters_negative(X, [A|B], [H|T]),
  guess_who2(ToGuessCharacter,NewQuestionList, [H|T], H).


game2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess):-
  write("YOUR TURN"),
  nl,
  ask("Do you know who my Character is?"),
  write("Who is it then?"),
  read(Ans),
  verify_wining_condition(MyCharacter, Ans), !.

game2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess):-
  write("What is you question?"),
  read(UsersQuestion),
  helper_guess_who2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess, UsersQuestion).

helper_guess_who2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess, UsersQuestion):-
  let_them_ask(UsersQuestion, MyCharacter),
  analize_characters_positive(UsersQuestion, UserCharactersLeft, UpdateCharactersLeft),
  write(UpdateCharactersLeft),
  nl,
  chooseRandomly([H|T], X),
  delete([H|T], X, NewQuestionList),
  tellThem2(MyCharacter, ToGuessCharacter, X, NewQuestionList, [A|B],  UpdateCharactersLeft,  CurrentGuess);
  analize_characters_negative(UsersQuestion, UserCharactersLeft, UpdateCharactersLeft),
  write(UpdateCharactersLeft),
  nl,
  chooseRandomly([H|T], X),
  delete([H|T], X, NewQuestionList),
  tellThem2(MyCharacter, ToGuessCharacter, X, NewQuestionList, [A|B],  UpdateCharactersLeft,  CurrentGuess).

tellThem2(MyCharacter, ToGuessCharacter, MyQuestion, NewQuestionList, [A|B], UsersList,  CurrentGuess):-
  write("MY TURN"),
  nl,
  ask(MyQuestion),
  analize_characters_positive(MyQuestion, [A|B], [H|T]),
  guess_who_final2(MyCharacter, ToGuessCharacter, MyQuestion, NewQuestionList, [H|T], UsersList, H).

tellThem2(MyCharacter, ToGuessCharacter, MyQuestion, NewQuestionList, [A|B], UsersList,  CurrentGuess):-
  analize_characters_negative(MyQuestion, [A|B], [H|T]),
  guess_who_final2(MyCharacter, ToGuessCharacter, MyQuestion, NewQuestionList, [H|T], UsersList, H).


guess_who_final(ToGuessCharacter):-
  chooseRandomly([alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom], MyCharacter),
  guess_who_final2(MyCharacter, ToGuessCharacter, MyQuestion, ["Is it a Man?", "Is it a Woman?", "Does he/she have blue eyes?", "Does he/she have brown eyes?", "Does he/she have black eyes?", "Does he/she have red hair?", "Does he/she have white hair?", "Does he/she have brown hair?", "Does he/she have blond hair?", "Does he/she have black hair?", "Is he bald?", "Is he/she wearing hat?", "Is he/she wearing glasses?", "Is your he/she smiling?", "Does he/she have big nose"], [alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom], [alex, alfred, anita, anne, bernard, bill, charles, claire, david, eric, frans, george, herman, joe, maria, max, paul, peter, philip, richard, robert, sam, susan, tom], CurrentGuess).

guess_who_final2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess):-
  A == CurrentGuess, CurrentGuess == ToGuessCharacter, write("Your character is "), write(CurrentGuess), !.

guess_who_final2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess):-
  game2(MyCharacter, ToGuessCharacter, MyQuestion, [H|T], [A|B], UserCharactersLeft, CurrentGuess).
