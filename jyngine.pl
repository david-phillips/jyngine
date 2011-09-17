:- module(jsonselect, [json_select/3]).
    


/*
jyngine.pro

A small library that allows for evaluation of a selector expression
against a JSON structure and returns a selected JSON substructure.

The JSON structure isnt actual JSON but rather the Prolog structure that
the incoming JSON gets deserialized as by SWIs http/json library.
*/



%% json_select(+Selector, +JSONValue, -Selected)
%
% Our main-predicate.  This is the only exported predicate
% in this module.  Splits the selector and lets json_select_aux
% do all of the heavy lifting.
%
json_select(Selector, JSONValue, Selected) :-
    analyze_selector(Selector, [_|SelectorComponents]),
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
    json_type(JSONValue, json_object), 
    json_object_select(Selector, JSONValue, CurrentlySelected),
    !,
    json_select_aux(Selectors, CurrentlySelected, Selected).
json_select_aux([Selector|Selectors], JSONValue, Selected) :-
    json_type(JSONValue, json_array),
    json_array_select(Selector, JSONValue, CurrentlySelected),
    !,
    json_select_aux(Selectors, CurrentlySelected, Selected).


%% json_object_select(+Selector, +JSONObject, -Selected)
%
% Given an object key and a JSON object select
% the object's value for the key.
%
% Arg1 is a list of form [Selector, SelectorType, SelectorSemantics]
%
json_object_select([Selector|_], JSONObject, Selected) :-
    json(NameValueList) = JSONObject,
    member(NameValuePair, NameValueList),
    NameValuePair = (Selector = Selected).
json_object_select(_, _, failure).


%% json_array_select(+Selector, +JSONArray, -Selected)
%
% Given a array index and a array select the array element
% at the given index.
%
% Arg1 is a list of form [Selector, SelectorType, SelectorSemantics]
%
json_array_select([ArrayIndexSelector, array_type, _], JSONArray, Selected) :-
    atom_number(ArrayIndexSelector, Index),
    json_array_select_aux(Index, JSONArray, 0, Selected).
json_array_select([_, array_slice_type, [SliceBeg,SliceEnd]], JSONArray, Selected) :-
    atom_number(SliceBeg, Beg), atom_number(SliceEnd, End),
    json_array_select_slice([Beg,End], JSONArray, Selected).
json_array_select(_, _, failure).

%% json_array_select_aux(+Selector, +JSONArray, +CurrentIndex, -Selected)
%
json_array_select_aux(ListIndexSelector, [Selected|_], CurrentIndex, Selected) :-
    ListIndexSelector = CurrentIndex.
json_array_select_aux(ListIndexSelector, [_|Rest], CurrentIndex, Selected) :-
    NextIndex is CurrentIndex + 1,
    json_array_select_aux(ListIndexSelector, Rest, NextIndex, Selected).

%% json_array_select_slice(+Slice, +JSONArray, -Range)
%
% Given slice [Beg,End], Range unifies with sublist whose
% first element is JSONArray[Beg] and whose last element is
% JSONArray[End-1].
%
json_array_select_slice(Slice, JSONArray, Range) :-
    json_array_select_slice_aux(Slice, JSONArray, 0, [], Range).
json_array_select_slice([SliceBeg,SliceEnd], Array, failure) :-
    length(Array, Length),
    (SliceBeg < 0 ; SliceEnd >= Length).

% Terminating condition: CurrentIndex = SliceEnd
json_array_select_slice_aux([_,SliceEnd], _, CurrentIndex, Storage, Matches) :-
    CurrentIndex = SliceEnd,
    !,
    reverse(Storage, Matches).
% Accumulating condition: SliceBeg =< CurrentIndex > SliceEnd
json_array_select_slice_aux([SliceBeg,SliceEnd], [CurrentVal|Rest], CurrentIndex, Storage, Matches) :-
    SliceBeg =< CurrentIndex, SliceEnd > CurrentIndex,
    NextIndex is CurrentIndex + 1,
    !,
    json_array_select_slice_aux([SliceBeg,SliceEnd], Rest, NextIndex, [CurrentVal|Storage], Matches).
% Skipping condition: else
json_array_select_slice_aux([SliceBeg,SliceEnd], [_|Rest], CurrentIndex, Storage, Matches) :-
    NextIndex is CurrentIndex + 1,
    json_array_select_slice_aux([SliceBeg,SliceEnd], Rest, NextIndex, Storage, Matches).
    

%% json_type(+JSONValue, -JSONType)
%
json_type(json(_), json_object).
json_type([], json_array).
json_type([_|_], json_array).


%% analyze_selector(+Selector, -Selectors)
%
analyze_selector(SelectorAtom, Selectors) :-
    atom_chars(SelectorAtom, SelectorChars),
    analyze_selector_aux(SelectorChars, 0, root_type, [], [], Selectors).

%% analyze_selector_aux(+Selector, +PrevChar, +CharStore, +SelectorStore, -Selectors)
%
%
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, CharStore, SelectorStore, Selectors) :-
    analyze_char(PrevChar, CurrChar, object_delim),
    !,
    reverse(CharStore, SelectorChars), atom_chars(Selector, SelectorChars),
    analyze_subselector(Selector, Type, AnalyzedSelector),
    analyze_selector_aux(Rest, CurrChar, object_type, [], [AnalyzedSelector|SelectorStore], Selectors).
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, CharStore, SelectorStore, Selectors) :-
    analyze_char(PrevChar, CurrChar, array_beg_delim),
    !,
    reverse(CharStore, SelectorChars), atom_chars(Selector, SelectorChars),
    analyze_subselector(Selector, Type, AnalyzedSelector),
    analyze_selector_aux(Rest, CurrChar, array_type, [], [AnalyzedSelector|SelectorStore], Selectors).
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, CharStore, SelectorStore, Selectors) :-
    analyze_char(PrevChar, CurrChar, array_end_delim),
    !,
    analyze_selector_aux(Rest, CurrChar, Type, CharStore, SelectorStore, Selectors).
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, CharStore, SelectorStore, Selectors) :-
    analyze_char(PrevChar, CurrChar, non_delim),
    CurrChar \= '\\',
    !,
    analyze_selector_aux(Rest, CurrChar, Type, [CurrChar|CharStore], SelectorStore, Selectors).
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, CharStore, SelectorStore, Selectors) :-
    analyze_char(PrevChar, CurrChar, non_delim),
    CurrChar = '\\',
    !,
    analyze_selector_aux(Rest, CurrChar, Type, CharStore, SelectorStore, Selectors).
analyze_selector_aux([], _, Type, CharStore, SelectorStore, Selectors) :-
    reverse(CharStore, SelectorChars), atom_chars(Selector, SelectorChars),
    analyze_subselector(Selector, Type, AnalyzedSelector),
    BackSelectors = [AnalyzedSelector|SelectorStore],
    reverse(BackSelectors, Selectors).


%% analyze_char(+PrevChar, +Char, -CharType)
%
analyze_char(PrevChar, '.', object_delim)    :- PrevChar \= '\\'.
analyze_char(PrevChar, '[', array_beg_delim) :- PrevChar \= '\\'.
analyze_char(PrevChar, ']', array_end_delim) :- PrevChar \= '\\'.
analyze_char(_, _, non_delim).


%% analyze_subselector(+Selector, +SelectorType, -AnalyzedSelector)
%
% Currently an analyzed selector is a three element list L:
%   L[0] -> Selector string
%   L[1] -> Selector type
%   L[2] -> Selector semantics
%
% The selector semantics is only given for cases where the selector
% has some internal structure.  For example, an array slice.
%
analyze_subselector(Selector, root_type, [Selector, root_type, null]).
analyze_subselector(Selector, object_type, [Selector, object_type, null]).
analyze_subselector(Selector, array_type, [Selector, array_type, null]) :-
    atom_chars(Selector, SelectorChars),
    not(member(':', SelectorChars)).
analyze_subselector(Selector, array_type, [Selector, array_slice_type, [SliceBeg, SliceEnd]]) :-
    sub_atom(Selector, Pos, 1, After, ':'),
    sub_atom(Selector, 0, Pos, _, SliceBeg),
    SliceEndPos is Pos + 1,
    sub_atom(Selector, SliceEndPos, After, 0, SliceEnd).
