:- module(jsonselect, [json_select/3]).
    


/*
jyngine.pro

This is a small library that allows us to evaluate a selector expression
against a JSON structure and returns a selected JSON substructure.

The JSON structure isn't actual JSON but rather the Prolog structure that
the incoming JSON gets deserialized as by SWI's http/json library.

The selector syntax is currently a tad limited and a tad ugly, but can be used
to grab a good deal from JSON.

Selector Syntax:
selector      -> root_symbol, value_lookups
root_symbol   -> '@'
value_lookups -> value_lookup
value_lookups -> value_lookup value_lookups
value_lookup  -> object_lookup
value_lookup  -> array_lookup
object_lookup -> '.' string
array_lookup  -> '.' integer

Selector Examples:

{
    "name": "Miles Davis Quintet",

    "members": [
        {
            "name": "Miles Davis",
            "instr": "trumpet"
        },
        {
            "name": "Wayne Shorter",
            "instr": "tenor sax"
        },
        {
            "name": "Herbie Hancock",
            "instr": "piano"
        },
        {
            "name": "Ron Carter",
            "instr": "bass"
        },
        {
            "name": "Tony Williams",
            "instr": "drums"
        }
    ]
}

@.name -> "Miles Davis Quintet"
@.members.1.name -> "Wayne Shorter"
@.members.1.instr -> "tenor sax"

*/



%% json_select(+Selector, +JSONValue, -Selected)
%
% Our main-predicate.  This is the only exported predicate
% in this module.  Splits the selector and lets json_select_aux
% do all of the heavy lifting.
%
json_select(Selector, JSONValue, Selected) :-
    split_selector(Selector, [_|SelectorComponents]),
    json_select_aux(SelectorComponents, JSONValue, Selected).


%% json_select_aux(+SplitSelector, +JSONValue, -Selected)
%
% This is the driver: Recurses down the list of selector
% components, deriving a new value for each one.
%
% Terminates with a value when it runs out of selectors.
%
json_select_aux([], JSONValue, JSONValue).
json_select_aux([Selector|Selectors], JSONValue, Selected) :-
    json_type(JSONValue, json_object), json_object_select(Selector, JSONValue, CurrentlySelected),
    !,
    json_select_aux(Selectors, CurrentlySelected, Selected).
json_select_aux([Selector|Selectors], JSONValue, Selected) :-
    json_type(JSONValue, json_array), json_array_select(Selector, JSONValue, CurrentlySelected),
    !,
    json_select_aux(Selectors, CurrentlySelected, Selected).


%% json_object_select(+Selector, +JSONObject, -Selected)
%
% Given an object key and a JSON object select
% the object's value for the key.
%
json_object_select(Selector, JSONObject, Selected) :-
    json(NameValueList) = JSONObject,
    member(NameValuePair, NameValueList),
    NameValuePair = (Selector = Selected).
json_object_select(_, _, failure).


%% json_array_select(+Selector, +JSONArray, -Selected)
%
% Given a array index and a array select the array element
% at the given index.
%
json_array_select(ArrayIndexSelector, JSONArray, Selected) :-
    json_array_select_aux(ArrayIndexSelector, JSONArray, 0, Selected).
json_array_select(_, _, failure).

%% json_array_select_aux(+Selector, +JSONArray, +CurrentIndex, -Selected)
%
json_array_select_aux(ListIndexSelector, [Selected|_], CurrentIndex, Selected) :-
    ListIndexSelector = CurrentIndex.
json_array_select_aux(ListIndexSelector, [_|Rest], CurrentIndex, Selected) :-
    NextIndex is CurrentIndex + 1,
    json_array_select_aux(ListIndexSelector, Rest, NextIndex, Selected).


%% json_type(+JSONValue, -JSONType)
%
json_type(json(_), json_object).
json_type([], json_array).
json_type([_|_], json_array).


%% split_selector(+Selector, -Words)
%
split_selector(SelectorAtom, Words) :-
    atom_chars(SelectorAtom, SelectorChars),
    split_selector_aux(SelectorChars, 0, [], [], Words).

%% split_selector_aux(+Selector, +PrevChar, +CharStore, +WordStore, -Words)
%
%
split_selector_aux([CurrChar|Rest], PrevChar, CharStore, WordStore, Words) :-
    analyze_char(PrevChar, CurrChar, object_delim),
    !,
    reverse(CharStore, WordChars), name(Word, WordChars),
    split_selector_aux(Rest, CurrChar, [], [Word|WordStore], Words).
split_selector_aux([CurrChar|Rest], PrevChar, CharStore, WordStore, Words) :-
    analyze_char(PrevChar, CurrChar, array_beg_delim),
    !,
    reverse(CharStore, WordChars), name(Word, WordChars),
    split_selector_aux(Rest, CurrChar, [], [Word|WordStore], Words).
split_selector_aux([CurrChar|Rest], PrevChar, CharStore, WordStore, Words) :-
    analyze_char(PrevChar, CurrChar, array_end_delim),
    !,
    split_selector_aux(Rest, CurrChar, CharStore, WordStore, Words).
split_selector_aux([CurrChar|Rest], PrevChar, CharStore, WordStore, Words) :-
    analyze_char(PrevChar, CurrChar, non_delim),
    CurrChar \= '\\',
    !,
    split_selector_aux(Rest, CurrChar, [CurrChar|CharStore], WordStore, Words).
split_selector_aux([CurrChar|Rest], PrevChar, CharStore, WordStore, Words) :-
    analyze_char(PrevChar, CurrChar, non_delim),
    CurrChar = '\\',
    !,
    split_selector_aux(Rest, CurrChar, CharStore, WordStore, Words).
split_selector_aux([], _, CharStore, WordStore, Words) :-
    reverse(CharStore, WordChars), name(Word, WordChars),
    BackWords = [Word|WordStore],
    reverse(BackWords, Words).


analyze_char(PrevChar, '.', object_delim) :- PrevChar \= '\\'.
analyze_char(PrevChar, '[', array_beg_delim) :- PrevChar \= '\\'.
analyze_char(PrevChar, ']', array_end_delim) :- PrevChar \= '\\'.
analyze_char(_, _, non_delim).
