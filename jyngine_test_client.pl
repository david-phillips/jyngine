:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module('jyngine.pl').

:- style_check(-atom).


test_selector_module :-
    test_selectors(TestSelectors),
    test_json_data(TestJSONAtom),
    atom_json_term(TestJSONAtom, TestJSON, []),
    evaluate_selectors(TestSelectors, TestJSON).


%%
% List of test selectors.
%
test_selectors([
    '@.name',
    '@.members[1].name',
    '@.members[1].instr',
    '@.members[0].assoc[1:3]',
    '@.members[0].years\\.active.start'
]).


test_json_data('
{
    "name": "Miles Davis Quintet",

    "members": [
        { 
            "name": "Miles Davis",
            "instr": "trumpet",
            "assoc": [ "Billy Eckstine", "Charlie Parker", "John Coltrane", "Gil Evans" ],
            "years.active": {"start": 1944, "end": 1991}
        },
        { 
            "name": "Wayne Shorter",
            "instr": "tenor sax",
            "assoc": [ "Art Blakey", "Weather Report" ]
        },
        { 
            "name": "Herbie Hancock",
            "instr": "piano",
            "assoc": [ "Quincy Jones", "Chick Corea", "Harvey Mason" ]
        },
        { 
            "name": "Ron Carter",
            "instr": "bass",
            "assoc": [ "Jaki Byard", "Horace Silver", "McCoy Tyner" ]
        },
        { 
            "name": "Tony Williams",
            "instr": "drums",
            "assoc": [ "Sam Rivers", "Lifetime" ]
        },
    ]
}
').


%%
%
evaluate_selectors([], _).
evaluate_selectors([Selector|Selectors], JSONData) :-
    json_select(Selector, JSONData, Selected),
    format('Selector: ~w\n', [Selector]),
    format('Selected: ~w\n\n', [Selected]),
    evaluate_selectors(Selectors, JSONData).


%%
% Call the main method.
%
:- test_selector_module.
