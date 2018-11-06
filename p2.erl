-module(p2).
-export([getFileContent/1]).
-export([getBinaryToList/1]).
-export([getSymbolsNumber/1]).
-export([countSymbol/2]).
-export([generateList/2]).

%https://stackoverflow.com/questions/14447575/reading-file-whole-flat-text-file-to-an-array

%Leer contenido de archivo a binario:
getFileContent(Filename) -> {ok, Text} = file:read_file(Filename), Text.

%Pasar archivo de binario a lista:
getBinaryToList(Filename) -> binary:bin_to_list(getFileContent(Filename)). %list_to_bin

%Obtener la tabla de simbolos de una lista:
generateList(_X,0)->[];
generateList(X,N)->[X]++generateList(X,N-1).
countSymbol(_X,[])->0;
countSymbol(X,[X|T])->1 + countSymbol(X,T);
countSymbol(X,[_H|T])->countSymbol(X,T).
getSymbolsNumber([])->[];
getSymbolsNumber([H|T])->Number = countSymbol(H,T) , [[1 + Number|H]] ++ 
								  getSymbolsNumber(T--generateList(H,Number)).

%Generar arbol a partir de lista de simbolos:
