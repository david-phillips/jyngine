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

selector   -> key (~key)*
key        -> object_key | array_key
object_key -> [json_string ^~]
array_key  -> [0-9][0-9]*

Example Selectors:

"tokens~0~text",
"tokens~0~offset~0",
"tokens~0~offset~1",
"tokens~0~features"
*/



%% json_select(+Selector, +JSONValue, -Selected)
%
% Our main-predicate.  This is the only exported predicate
% in this module.  Splits the selector and lets json_select_aux
% do all of the heavy lifting.
%
json_select(Selector, JSONValue, Selected) :-
    split_selector(Selector, SelectorComponents),
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
    (
        json_type(JSONValue, json_object), json_object_select(Selector, JSONValue, CurrentlySelected)
        ;
        json_type(JSONValue, json_array), json_array_select(Selector, JSONValue, CurrentlySelected)
    ),
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



%% json_array_select(+Selector, +JSONArray, -Selected)
%
% Given a array index and a array select the array element
% at the given index.
%
json_array_select(_, [], null).
json_array_select(ArrayIndexSelector, JSONArray, Selected) :-
    json_array_select_aux(ArrayIndexSelector, JSONArray, 0, Selected).

%% json_array_select_aux(+Selector, +JSONArray, +CurrentIndex, -Selected)
%
json_array_select_aux(_, [], _, null).
json_array_select_aux(ListIndexSelector, [Selected|_], CurrentIndex, Selected) :-
    ListIndexSelector = CurrentIndex.
json_array_select_aux(ListIndexSelector, [_|Rest], CurrentIndex, Selected) :-
    NextIndex is CurrentIndex + 1,
    json_array_select_aux(ListIndexSelector, Rest, NextIndex, Selected).



%% json_type(+JSONValue, -JSONType)
%
json_type(JSONValue, JSONType) :-
    (is_list(JSONValue), JSONType = json_array)
    ;
    (json(_) = JSONValue, JSONType = json_object).

    

%% split_selector(+Selector, -Words)
%
% Given a selector, split the selector on the delimiter D such
% that selector_delim(D).
%
% Note that this idea of simply splitting selectors doesn't scale if
% we want a more sophisticated syntax.
%
% For example, if we wanted to index lists with the familiar square
% bracket syntax, we'd need a more sophisticated parser.
%
split_selector(Selector, Words) :-
    split_selector_aux(Selector, [], [], Words).

%% split_selector_aux(+Selector, +CharStorage, +WordStorage, -Words)
%
split_selector_aux([], CharStorage, WordStorage, Words) :-
    reverse(CharStorage, WordChars), name(Word, WordChars),
    BackWords = [Word|WordStorage],
    reverse(BackWords, Words).
split_selector_aux([CurrentChar|Rest], CharStorage, WordStorage, Words) :-
    selector_delim(Delim), CurrentChar \= Delim,
    !,
    split_selector_aux(Rest, [CurrentChar|CharStorage], WordStorage, Words).
split_selector_aux([CurrentChar|Rest], CharStorage, WordStorage, Words) :-
    selector_delim(Delim), 
    CurrentChar = Delim,
    !,
    reverse(CharStorage, WordChars), name(Word, WordChars),
    split_selector_aux(Rest, [], [Word|WordStorage], Words).

%% The colon character ':'
selector_delim(58).
