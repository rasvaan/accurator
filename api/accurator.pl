:- module(accurator, []).

/** <module> Accurator
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

http:location(html, cliopatria(html), []).
http:location(img, cliopatria(img), []).
user:file_search_path(html, web(html)).
user:file_search_path(img, web(img)).

:- http_handler(cliopatria('.'), serve_files_in_directory(html), [prefix]).
:- http_handler(img('.'), serve_files_in_directory(img), [prefix]).
