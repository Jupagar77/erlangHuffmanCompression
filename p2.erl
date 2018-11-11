-module(p2).
-export([getFileContent/1]).
-export([getBinaryToList/1]).
-export([getSymbolsNumber/1]).
-export([countSymbol/2]).
-export([generateList/2]).

-export([sortAscending/1]).

-export([crearListaArboles/1]).
-export([sortAscendingArboles/1]).
-export([huffman/1]).
-export([tablaSimbolos/1]).

-export([compress/3]).
-export([findCode/2]).
-export([toCode/2]).
-export([group/1]).
-export([countValues/1]).
-export([completeByte/1]).
-export([leerArchivoComprimido/1]).
-export([removeRepeated/2]).

-export([crearArbolHuffman/1]).
-export([descomprimir/2]).
-export([decompress/2]).
-export([decompressLista/2]).

-export([hiloComprimir/3]).
-export([archivosListo/4]).
-export([huffmanServer/3]).

%COMPRESOR

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

removeRepeated([],_S)->[];
removeRepeated([H|T],H)->removeRepeated(T,H);
removeRepeated([H|T],S)->[H]++removeRepeated(T,S).

getSymbolsNumber([])->[];
getSymbolsNumber([H|T])->Number = countSymbol(H,T) ,
						 %io:fwrite("~9..0B |~10w| ~9..0B ~n", [trunc(N), H, Number]), 
						 %Old = T -- generateList(H,Number), bajo performance
						 New = removeRepeated(T,H),
						 [[1 + Number|H]] ++ getSymbolsNumber(New).

%Generar arbol a partir de lista de simbolos:
%Ordenar lista generada por getSymbolsNumber de mayor a menor segun la Frecuencia. Basado en Quicksort
sortAscending([]) -> [];
sortAscending([[Pivot|Char]|T]) -> sortAscending([[X|Y] || [X|Y] <- T, X < Pivot]) ++ [[Pivot|Char]] ++ sortAscending([[X|Y] || [X|Y] <- T, X >= Pivot]).

crearListaArboles([]) -> [];
crearListaArboles(L) -> crearListaArboles(L,[]).
crearListaArboles([],Aux) -> Aux;
crearListaArboles([[F|E]|T],Aux) -> Laux = Aux ++ [{[F|E],{},{}}], crearListaArboles(T,Laux).

sortAscendingArboles([]) -> [];
sortAscendingArboles([{[Pivot|Char],I,D}|T]) -> sortAscendingArboles([{[X|Y],I,D} || {[X|Y],I,D} <- T, X < Pivot]) ++ 
												[{[Pivot|Char],I,D}] ++ 
												sortAscendingArboles([{[X|Y],I,D} || {[X|Y],I,D} <- T, X >= Pivot]).
huffman([]) -> [];
huffman([Arb|[]]) -> Arb;
huffman([{[F1|E1],I1,D1},{[F2|E2],I2,D2}|[]]) 
	-> Suma = F1+F2, NuevaRaiz = [Suma|null], {NuevaRaiz,{[F1|E1],I1,D1},{[F2|E2],I2,D2}};
huffman([{[F1|E1],I1,D1},{[F2|E2],I2,D2}|T])
	-> Suma = F1+F2, NuevaRaiz = [Suma|null], Arb = {NuevaRaiz,{[F1|E1],I1,D1},{[F2|E2],I2,D2}},
	ListaActualizada = [Arb] ++ T, huffman(sortAscendingArboles(ListaActualizada)).

tablaSimbolos({})->[];
tablaSimbolos(Arb) -> tablaSimbolos(Arb,[]).
tablaSimbolos({[_F|E],{},{}},Path) -> [[E,Path]];
tablaSimbolos({_R,I,D},Path) -> tablaSimbolos(I,Path++[0])++tablaSimbolos(D,Path++[1]).

findCode(_S,[])->[];
findCode(S,[[Symbol,Code]|_T]) when Symbol == S -> Code;
findCode(S,[_H|T])-> findCode(S,T).

toCode([],_T)->[];
toCode([H|T],Table)-> findCode(H,Table) ++ toCode(T,Table).

countValues([])->0;
countValues([_H|T])->countValues(T)+1.
completeByte(L)->completeByte(L,countValues(L)).
completeByte(L,X) when X > 1->L;
completeByte(L,_X)-> completeByte(L ++ [0],countValues(L)+1).

group(L)-> group(L,2,[]).
group([],_N,Acum)->Acum;
group([H|T],1,Acum)-> [Acum ++ [H]] ++ group(T,2,[]);
group([H|T],N,Acum)-> group(T,N-1,Acum++[H]).

compress(F, Server, N) -> BinList = getBinaryToList(F), 
			   Tabla = tablaSimbolos(huffman(crearListaArboles(sortAscending(getSymbolsNumber(BinList))))),
			   Code = toCode(BinList,Tabla),
			   Compress = [Tabla,Code],
			   %file:write_file(filename:rootname(F),erlang:term_to_binary(Compress)),
			   Server ! {Compress,N}.


%--------------------------------------------------------

%DECOMPRESOR
leerArchivoComprimido(F)->erlang:binary_to_term(getFileContent(F)).

%Recibe tabla de simbolos y crea un arbol con eso
crearArbolHuffman([]) -> {};
crearArbolHuffman(L) -> crearArbolHuffman(L,{root,{},{}}).
crearArbolHuffman([],Arb) -> Arb;
crearArbolHuffman([[Char|[L|_]]|T],Arb) -> Arbol = crearArbolHuffmanInterno(Char,L,Arb), crearArbolHuffman(T,Arbol).

crearArbolHuffmanInterno(Char,[],_Arb) -> {Char,{},{}};
crearArbolHuffmanInterno(Char,[Val|T],{}) when Val =:= 0 -> {null,crearArbolHuffmanInterno(Char,T,{}),{}};
crearArbolHuffmanInterno(Char,[Val|T],{}) when Val =:= 1 -> {null,{},crearArbolHuffmanInterno(Char,T,{})};
crearArbolHuffmanInterno(Char,[Val|T],{R,I,D}) when Val =:= 0 -> {R,crearArbolHuffmanInterno(Char,T,I),D};
crearArbolHuffmanInterno(Char,[Val|T],{R,I,D}) when Val =:= 1 -> {R,I,crearArbolHuffmanInterno(Char,T,D)}.


%Recibe lista de binarios y arbol de huffman para interpretar. 
%Retorna lista de caracteres para interpetarlos.

descomprimir(_Arb,[]) -> [];
descomprimir(Arb,[H|_T]) -> descomprimir(Arb,Arb,H,[]).

descomprimir(Arb,{R,{},{}},L,Lchar) -> descomprimir(Arb,Arb,L, [Lchar|[R]] );
descomprimir(_,_,[],Lchar) -> Lchar;
descomprimir(Arb,{_R,I,_D},[H|T],Lchar) when H =:= 0 -> descomprimir(Arb,I,T,Lchar);
descomprimir(Arb,{_R,_I,D},[H|T],Lchar) when H =:= 1 -> descomprimir(Arb,D,T,Lchar).


decompressLista([],_F)->[];
decompressLista([[H|C]|T],Newfile)-> ArbolHuffman = crearArbolHuffman(H),
							 Decompresion = lists:flatten(descomprimir(ArbolHuffman,C)),
							 file:write_file(Newfile,Decompresion,[append]),
							 decompressLista(T,Newfile).

decompress(F,Newfile)-> ListaCompresion = group(leerArchivoComprimido(F)),
				decompressLista(ListaCompresion,Newfile).

hiloComprimir(F,Server,N)->spawn(fun()->compress(F,Server,N) end).

archivosListo(C,C,Comprimido,Filename)->io:format("Comprimiendo archivo!~n", []),
										file:write_file(Filename,erlang:term_to_binary(Comprimido)),
										huffmanServer(C,Comprimido,Filename);
archivosListo(C,_B,Comprimido,Filename)->huffmanServer(C,Comprimido,Filename).

huffmanServer(C,L,Filename)->
    receive
        {Comprimido,Cantidad} -> io:format("Recibiendo sub-archivo comprimido!~n", []), 
        						archivosListo(C+1,Cantidad,L++Comprimido,Filename);
        finalizar->io:format("Me muero~n", []);
        limpiar->huffmanServer(0,[],Filename);
        X -> io:format("recibo: ~p~n",[X]), huffmanServer(C,L,Filename)
    end.

% Prueba:
% HuffmanServer = spawn (fun()->p2:huffmanServer(0,[],"ejem.txt.huff")end).
% p2:hiloComprimir("ejem1.txt", HuffmanServer,2).
% p2:hiloComprimir("ejem2.txt", HuffmanServer,2).
% p2:decompress("ejem.txt.huff", "ejem.txt").