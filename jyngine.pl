:- module(jsonselect, [json_select/3]).
    

/*
jyngine.pro

A small library that allows for evaluation of a selector expression
against a JSON structure and returns a selected JSON substructure.

*/


%% json_select(+Selector, +JSONValue, -Selected)
%
% Our main-predicate.  This is the only exported predicate
% in this module.  Splits the selector and lets json_select_aux
% do all of the heavy lifting.
%
json_select(Selector, JSONValue, Selected) :-
    analyze_selector(Selector, [_|SelectorComponents]),
    !,
    json_select_aux(SelectorComponents, JSONValue, Selected).


%% json_select_aux(+SplitSelector, +JSONValue, -Selected)
%
% This is the driver: Recurses down the list of selector
% components, deriving a new value for each one.
%
% Terminates with a value when it runs out of selectors.
%

% Base case.
json_select_aux([], JSONValue, JSONValue).
% Match fails if selector & json val are inconsistent types.
json_select_aux([[_,object_type,_]|_], JSONValue, nomatch) :- json_type(JSONValue, json_array).
json_select_aux([[_,array_type,_]|_], JSONValue, nomatch) :- json_type(JSONValue, json_object).
% Recursive Case: object selector
json_select_aux([Selector|Selectors], JSONValue, Selected) :-
    Selector = [_,object_type,_],
    json_object_select(Selector, JSONValue, CurrentlySelected),
    (CurrentlySelected = nomatch ->
        Selected = nomatch
        ;
        json_select_aux(Selectors, CurrentlySelected, Selected)).
% Recursive Case: array selector
json_select_aux([Selector|Selectors], JSONValue, Selected) :-
    (Selector = [_,array_type,_] ; Selector = [_,array_slice_type,_]),
    json_array_select(Selector, JSONValue, CurrentlySelected),
    (CurrentlySelected = nomatch ->
        Selected = nomatch
        ;
        json_select_aux(Selectors, CurrentlySelected, Selected)).


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
% Else, nomatch.
json_object_select(_, _, nomatch).


%% json_array_select(+Selector, +JSONArray, -Selected)
%
% Given a array index and a array select the array element
% at the given index.
%
% Arg1 is a list of form [Selector, SelectorType, SelectorSemantics]
%

% Bounds check for array index.
json_array_select([Index, array_type, _], JSONArray, nomatch) :-
    length(JSONArray, Length),
    Index >= Length. 
% Bounds check for array slice indexes.
json_array_select([_, array_slice_type, [Beg,End]], JSONArray, nomatch) :-
    length(JSONArray, Length),
    (Beg < 0 ; End >= Length).
% Array lookup.
json_array_select([Index, array_type, _], JSONArray, Selected) :-
    json_array_select_aux(Index, JSONArray, 0, Selected).
% Array slice lookup.
json_array_select([_, array_slice_type, Slice], JSONArray, Selected) :-
    json_array_select_slice(Slice, JSONArray, Selected).
% Else, nomatch.
json_array_select(_, _, nomatch).

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
    json_array_select_slice_aux(Slice, JSONArray, 0, Hole-Hole, Range).
% Terminating condition: CurrentIndex = SliceEnd
json_array_select_slice_aux([_,SliceEnd], _, SliceEnd, Storage-Hole, Storage) :-
    Hole = [].
% Accumulating condition: Beg =< CurrentIndex > End
json_array_select_slice_aux([Beg,End], [CurrentVal|Rest], CurrentIndex, Storage-Hole, Matches) :-
    Beg =< CurrentIndex, End > CurrentIndex,
    !,
    NextIndex is CurrentIndex + 1,
    Hole = [CurrentVal|NewHole],
    json_array_select_slice_aux([Beg,End], Rest, NextIndex, Storage-NewHole, Matches).
% Skipping condition: else
json_array_select_slice_aux([Beg,End], [_|Rest], CurrentIndex, Storage-Hole, Matches) :-
    NextIndex is CurrentIndex + 1,
    json_array_select_slice_aux([Beg,End], Rest, NextIndex, Storage-Hole, Matches).
    

%% json_type(+JSONValue, -JSONType)
%
json_type(json(_), json_object).
json_type([], json_array).
json_type([_|Rest], json_array) :- json_type(Rest, json_array).


%% analyze_selector(+Selector, -Selectors)
%
analyze_selector(SelectorAtom, Selectors) :-
    atom_chars(SelectorAtom, SelectorChars),
    analyze_selector_aux(SelectorChars, 0, root_type, CharsHole-CharsHole, SelectorsHole-SelectorsHole, Selectors).

%% analyze_selector_aux(+Selector, +PrevChar, +CharStore, +SelectorStore, -Selectors)
%
%

% Case: out of characters , Action: Push last subselector onto Selectors
analyze_selector_aux([], _, Type, Chars-CharsHole, Selectors-SelectorsHole, Selectors) :-
    CharsHole = [],
    analyze_subselector(Chars, Type, AnalyzedSelector),
    SelectorsHole = [AnalyzedSelector].
% Case: CurrentChar == '.' , Action: Push char store selector into selector store.
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, Chars-[], Selectors-SelectorsHole, Selectors) :-
    analyze_char(PrevChar, CurrChar, object_delim), !,
    analyze_subselector(Chars, Type, AnalyzedSelector),
    SelectorsHole = [AnalyzedSelector|NewSelectorsHole],
    analyze_selector_aux(Rest, CurrChar, object_type, NewCharsHole-NewCharsHole, Selectors-NewSelectorsHole, Selectors).
% CurrentChar == '[' , Action: Push char store selector into selector store.
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, Chars-[], Selectors-SelectorsHole, Selectors) :-
    analyze_char(PrevChar, CurrChar, array_beg_delim), !,
    analyze_subselector(Chars, Type, AnalyzedSelector),
    SelectorsHole = [AnalyzedSelector|NewSelectorsHole],
    analyze_selector_aux(Rest, CurrChar, array_type, NewCharsHole-NewCharsHole, Selectors-NewSelectorsHole, Selectors).
% CurrentChar == ']' or '\\' , Action: Don't push current char to charstore.
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, Chars-CharsHole, Selectors-SelectorsHole, Selectors) :-
    analyze_char(PrevChar, CurrChar, ignore_char), !,
    analyze_selector_aux(Rest, CurrChar, Type, Chars-CharsHole, Selectors-SelectorsHole, Selectors).
% CurrentChar == nonspecial char , Action: Push current char to charstore.
analyze_selector_aux([CurrChar|Rest], PrevChar, Type, Chars-[CurrChar|NewCharsHole], Selectors-SelectorsHole, Selectors) :-
    analyze_char(PrevChar, CurrChar, default_char), !,
    analyze_selector_aux(Rest, CurrChar, Type, Chars-NewCharsHole, Selectors-SelectorsHole, Selectors).


%% json_walk(+JSONData, +Callback)
%
% Depth-first traversal of JSONData, where
% Callback is called for every visited node.
%
json_walk(JSONData, Callback) :-
    json_type(JSONData, json_array),
    json_walk_array(JSONData, Callback).
json_walk(JSONData, Callback) :-
    json_type(JSONData, json_object),
    json_walk_object(JSONData, Callback).
json_walk(JSONData, Callback) :-
    Callable =.. [Callback,JSONData], call(Callable).
json_walk_array([], _).
json_walk_array([First|Rest], Callback) :-
    Callable =.. [Callback,First], call(Callable),
    json_walk(First, Callback),
    json_walk_array(Rest, Callback).
json_walk_object(JSONObject, Callback) :-
    json(Object) = JSONObject,
    json_walk_object_pairs(Object, Callback).
json_walk_object_pairs([], _).
json_walk_object_pairs([Pair|Rest], Callback) :-
    Callable =.. [Callback,Pair], call(Callable),
    (Name = Value) = Pair,
    json_walk(Value, Callback),
    json_walk_object_pairs(Rest, Callback).


%% analyze_char(+PrevChar, +Char, -CharType)
%
analyze_char(PrevChar, '.', object_delim)    :- PrevChar \= '\\'.
analyze_char(PrevChar, '[', array_beg_delim) :- PrevChar \= '\\'.
analyze_char(PrevChar, ']', ignore_char    ) :- PrevChar \= '\\'.
analyze_char(PrevChar, '\\', ignore_char   ) :- PrevChar \= '\\'.
analyze_char(_, _, default_char).


%% analyze_subselector(+Selector, +SelectorType, -AnalyzedSelector)
%
% Currently an analyzed selector is a three element list L:
%   L[0] -> Selector
%   L[1] -> Selector type
%   L[2] -> Selector semantics
%
% The selector semantics is only given for cases where the selector
% has some internal structure.  For example, an array slice.
%
analyze_subselector([Selector], root_type, [Selector, root_type, null]).
analyze_subselector(SelectorChars, object_type, [Selector, object_type, null]) :-
    atom_chars(Selector, SelectorChars).
analyze_subselector(SelectorChars, array_type, [Selector, array_type, null]) :-
    not(member(':', SelectorChars)),
    atom_chars(SelectorAtom, SelectorChars),
    atom_number(SelectorAtom, Selector).
analyze_subselector(SelectorChars, array_type, [Selector, array_slice_type, [SliceBeg, SliceEnd]]) :-
    atom_chars(Selector, SelectorChars),
    sub_atom(Selector, Pos, 1, After, ':'),
    sub_atom(Selector, 0, Pos, _, SliceBegAtom),
    SliceEndPos is Pos + 1,
    sub_atom(Selector, SliceEndPos, After, 0, SliceEndAtom),
    atom_number(SliceBegAtom, SliceBeg),
    atom_number(SliceEndAtom, SliceEnd).
