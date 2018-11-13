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

-export([compress/2]).
-export([findCode/2]).
-export([toCode/2]).
-export([group/1]).
-export([group8/1]).
-export([countValues/1]).
-export([completeByte/1]).
-export([completeByteRev/1]).
-export([leerArchivoComprimido/1]).
-export([removeRepeated/2]).

-export([crearArbolHuffman/1]).
-export([descomprimir/2]).
-export([decompress/2]).
-export([decompressLista/2]).

-export([hiloComprimir/2]).
-export([archivosListo/5]).
-export([huffmanServer/5]).

-export([bin2dec/1, bin2dec_/2]).
-export([dec2bin/1]).
-export([listToDec/1]).
-export([decToList/1]).
-export([tablaSymToTuple/1]).

-export([getFrecuenciaRepetidaSuma/2]).
-export([sumarFrecuencias/2]).

-export([mergeFrecuencias/2]).
-export([getFrecuenciaRepetida/2]).

bin2dec("") ->0;
bin2dec(List) -> bin2dec_(List, 0).

bin2dec_([], Sum) -> Sum;
bin2dec_([48 | Tail], Sum) -> bin2dec_(Tail, Sum * 2);
bin2dec_([49 | Tail], Sum) -> bin2dec_(Tail, Sum * 2 + 1).

dec2bin(B)->([N - $0 || N <- integer_to_list(B, 2)]).

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
getSymbolsNumber([H|T])->Number = countSymbol(H,T),
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
completeByte(L,X) when X > 7->L;
completeByte(L,_X)-> completeByte(L ++ [0],countValues(L)+1).

completeByteRev(L)->completeByteRev(L,countValues(L)).
completeByteRev(L,X) when X > 7->L;
completeByteRev(L,_X)-> completeByteRev([0|L],countValues(L)+1).

group(L)-> group(L,2,[]).
group([],_N,Acum)->Acum;
group([H|T],1,Acum)-> [Acum ++ [H]] ++ group(T,2,[]);
group([H|T],N,Acum)-> group(T,N-1,Acum++[H]).

group8(L)-> group8(L,8,[]).
group8([],_N,Acum)->[completeByte(Acum)];
group8([H|T],1,Acum)-> [Acum ++ [H]] ++ group8(T,8,[]);
group8([H|T],N,Acum)-> group8(T,N-1,Acum++[H]).

listToDec([])->[];
listToDec([H|T])-> [bin2dec(lists:concat(H))] ++ listToDec(T).

decToList([])->[];
decToList([H|T])-> [completeByteRev(dec2bin(H))] ++ decToList(T).

tablaSymToTuple([])->[];
tablaSymToTuple([[H|C]|T])-> [{H,lists:concat(C)}] ++ tablaSymToTuple(T).

compress(F, Server) -> 	BinList = getBinaryToList(F), 
			   			%Tabla = tablaSimbolos(huffman(crearListaArboles(sortAscending(getSymbolsNumber(BinList))))),
			   			%TablaTuple = tablaSymToTuple(Tabla),
			   			%DicTabla = dict:from_list(TablaTuple),

			   			%Code = toCode(BinList,Tabla),
			   			%CodeByn = group8(Code),
			   			%CodeDec = listToDec(CodeByn),
			   			Server ! {comprimir,sortAscending(getSymbolsNumber(BinList)),BinList}.


%--------------------------------------------------------

%DECOMPRESOR
leerArchivoComprimido(F)->binary:bin_to_list(getFileContent(F)).

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

decompress(F,Newfile)-> ListaCompresion = leerArchivoComprimido(F),
				decompressLista(ListaCompresion,Newfile).

hiloComprimir(F,Server)->spawn(fun()->compress(F,Server) end).

getFrecuenciaRepetidaSuma(L,[])->L;
getFrecuenciaRepetidaSuma([H|T],[[Cant|T]|_Cola]) -> [ H+Cant | T];
getFrecuenciaRepetidaSuma([H|T],[[_Cant|_Col]|Cola]) -> getFrecuenciaRepetidaSuma([H|T],Cola).
sumarFrecuencias([],_L)->[];
sumarFrecuencias([H|T],L)-> [getFrecuenciaRepetidaSuma(H,L)] ++ sumarFrecuencias(T,L).

getFrecuenciaRepetida(L,[])->L;
getFrecuenciaRepetida([_H|T],[[_Cant|T]|_Cola]) -> [];
getFrecuenciaRepetida([H|T],[[_Cant|_Col]|Cola]) -> getFrecuenciaRepetida([H|T],Cola).

mergeFrecuencias(L1,L2)->mergeFrecuencias(L1,L2,[]).
mergeFrecuencias([],L,Accum)->L ++ Accum;
mergeFrecuencias([H|T],L,Accum)->  mergeFrecuencias(T,L,Accum ++ [getFrecuenciaRepetida(H,L)]).

archivosListo(C,C,Sim,Filename,Bin)->	io:format("Comprimiendo archivo!~n", []),
										io:format("Generando tabla de simbolos!~n", []),
										Tabla = tablaSimbolos(huffman(crearListaArboles(Sim))),
										io:format("Generando codigo del archivo total!~n", []),
										Code = toCode(Bin,Tabla),
										io:format("Pasando a bytes!~n", []),
										CodeByn = group8(Code),
										io:format("Pasando a decimal!~n", []),
										CodeDec = listToDec(CodeByn),
										io:format("Escribiendo archivo!~n", []),
										file:write_file(Filename,(CodeDec)),
										io:format("Archivo comprimido!~n", []),
								  		huffmanServer(C,Tabla,Filename,C,Bin);

archivosListo(C,B,Sim,Filename,Bin)->huffmanServer(C,Sim,Filename,B,Bin).

huffmanServer(C,Symbol,Filename,N,Binaries)->
    receive

        {comprimir,Simbolos,Binary} -> io:format("Recibiendo tabla de frecuencias!~n", []),
        						Sumafrec = sumarFrecuencias(Symbol,Simbolos),
        						Mergefrec = sortAscending(mergeFrecuencias(Simbolos,Sumafrec)),
        					    archivosListo(C+1,N,Mergefrec,Filename,Binaries ++ Binary);

        {descomprimir,Filename,Newfile} -> io:format("Descomprimiendo archivo!~n", []),
        							ArbolHuffman = crearArbolHuffman(Symbol),
									
        						   	ListaCompresion = leerArchivoComprimido(Filename),
        						   	ListaBin = lists:flatten(decToList(ListaCompresion)),
        						
									Decompresion = lists:flatten(descomprimir(ArbolHuffman,[ListaBin])),
									file:write_file(Newfile,Decompresion);
		print -> io:format("Simbolos: ~p~n",[Symbol]), huffmanServer(C,Symbol,Filename,N,Binaries);
        finalizar->io:format("Me muero~n", []);
        X -> io:format("recibo: ~p~n",[X]), huffmanServer(C,Symbol,Filename,N,Binaries)
    end.

% Prueba:
% HuffmanServer = spawn (fun()->p2:huffmanServer(0,[],"mama.txt.huff",2,[])end).
% p2:hiloComprimir("mama01", HuffmanServer).
% HuffmanServer ! {descomprimir,"mama.txt.huff","mamaD.txt"}.