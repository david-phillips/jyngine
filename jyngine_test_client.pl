:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module('jyngine.pl').



%%
% The main method.
%
fetch_and_process_data :-
    fetch_data(JSONData),
    test_selector_module(JSONData).



%%
% Open stream to resource at URL.
% Read JSON from stream into term.
%
fetch_data(FetchedData) :-
    url_to_process(URL),
    http_open(URL, DataStream, []),
    json_read(DataStream, FetchedData, []),
    close(DataStream).



%%
% Hardcoded World Bank URL
%
url_to_process('http://api.worldbank.org/countries/USA/indicators/AG.AGR.TRAC.NO?per_page=10&date=2005:2011&format=json').


%%
%
test_selector_module(JSONData) :-
    selectors(Selectors),
    evaluate_selectors(Selectors, JSONData).


%%
% List of test selectors.
%
selectors([
    '@.1.0.indicator.id',
    '@.1.0.indicator.value',
    '@.1.0.country.id',
    '@.1.0.country.value',
    '@.1.0.value',
    '@.1.0.da\\te'
]).


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
:- fetch_and_process_data.
