-module(p2Final).
%Proyecto #2 Sistemas Distribuidos
%Andres Dalolio Aguilar
%Pablo Gutierrez
%Claribel Bermudez

-export([huffmanServer/5,hiloComprimir/2,decompress/3]).

%-------------------------------Common

%Ordenar lista generada por getSymbolsNumber de mayor a menor segun la Frecuencia. Basado en Quicksort
sortAscending([]) -> [];
sortAscending([[Pivot|Char]|T]) -> sortAscending([[X|Y] || [X|Y] <- T, X < Pivot]) ++ [[Pivot|Char]] ++ sortAscending([[X|Y] || [X|Y] <- T, X >= Pivot]).

%-------------------------------Comprimir

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
	ListaActualizada = [Arb|T], huffman(sortAscendingArboles(ListaActualizada)).

tablaSimbolos({})->[];
tablaSimbolos(Arb) -> tablaSimbolos(Arb,[]).
tablaSimbolos({[_F|E],{},{}},Path) -> [{E,Path}];
tablaSimbolos({_R,I,D},Path) -> tablaSimbolos(I,Path++[0])++tablaSimbolos(D,Path++[1]).

countValues([])->0;
countValues([_H|T])->countValues(T)+1.

completeByte(L)->completeByte(L,countValues(L)).
completeByte(L,X) when X > 7->L;
completeByte(L,_X)-> completeByte(L ++ [0],countValues(L)+1).

group8(L)-> group8(L,8,[]).
group8([],_N,Acum)->[completeByte(Acum)];
group8([H|T],1,Acum)-> [Acum ++ [H]] ++ group8(T,8,[]);
group8([H|T],N,Acum)-> group8(T,N-1,Acum++[H]).

bin2dec_([], Sum) -> Sum;
bin2dec_([48 | Tail], Sum) -> bin2dec_(Tail, Sum * 2);
bin2dec_([49 | Tail], Sum) -> bin2dec_(Tail, Sum * 2 + 1).

bin2dec("") ->0;
bin2dec(List) -> bin2dec_(List, 0).

dec2bin(B)->([N - $0 || N <- integer_to_list(B, 2)]).

listToDec([])->[];
listToDec([H|T])-> [bin2dec(lists:concat(H))|listToDec(T)].

group8sincomplete(L)-> group8sincomplete(L,8,[]).
group8sincomplete([],_N,Acum)->[Acum];
group8sincomplete([H|T],1,Acum)-> [Acum ++ [H]] ++ group8sincomplete(T,8,[]);
group8sincomplete([H|T],N,Acum)-> group8sincomplete(T,N-1,Acum++[H]).

writeList([],_Filename,_Fd)->ok;
writeList([H|T],Filename,Fd)-> 
								io:format("Escribiendo: ~p~n",[H]), 
								file:write(Fd, [H]),
								writeList(T,Filename,Fd).

toCode(L,Dict,Filename)-> 
								{ok, Fd} = file:open(Filename, [append, delayed_write]),
								toCode(L,Dict,[],0,[],Filename,Fd),
								file:close(Fd),
								ok.
toCode([],_Dict,Acum,_Size,_Listafinal,Filename,Fd)-> 
								Final = group8(Acum), 
								FinalDec = listToDec(Final),
								writeList(FinalDec,Filename,Fd);
toCode([H|T],Dict,Acum,Size,_Listafinal,Filename,Fd) when Size >= 8 ->
								TotalAcumm = Acum ++ dict:fetch(H,Dict),
							    [E|R] = group8sincomplete(TotalAcumm),
								EDec = bin2dec(lists:concat(E)),
								file:write(Fd, [EDec]),
								FlattenR = lists:flatten(R),
								toCode(T,Dict,FlattenR,length(FlattenR),[],Filename,Fd);
toCode([H|T],Dict,Acum,_Size,Listafinal,Filename,Fd)->
								TotalAcumm = Acum ++ dict:fetch(H,Dict),
							  	toCode(T,Dict,TotalAcumm,length(TotalAcumm),Listafinal,Filename,Fd).

getFileContent(Filename) -> {ok, Text} = file:read_file(Filename), Text.

%Pasar archivo de binario a lista:
getBinaryToList(Filename) -> binary:bin_to_list(getFileContent(Filename)). %list_to_bin

countSymbol(_X,[])->0;
countSymbol(X,[X|T])->1 + countSymbol(X,T);
countSymbol(X,[_H|T])->countSymbol(X,T).

removeRepeated([],_S)->[];
removeRepeated([H|T],H)->removeRepeated(T,H);
removeRepeated([H|T],S)->[H|removeRepeated(T,S)].

getSymbolsNumber([])->[];
getSymbolsNumber([H|T])->Number = countSymbol(H,T),
						 New = removeRepeated(T,H),
						 [[1 + Number|H]|getSymbolsNumber(New)].
                         
compress(F, Server) -> 	BinList = getBinaryToList(F), 
			   			Server ! {comprimir,sortAscending(getSymbolsNumber(BinList)),BinList}.

%-------------------------------Descomprimir

%Recibe tabla de simbolos y crea un arbol con eso
crearArbolHuffman([]) -> {};
crearArbolHuffman(L) -> crearArbolHuffman(L,{root,{},{}}).
crearArbolHuffman([],Arb) -> Arb;
crearArbolHuffman([{Char,L}|T],Arb) -> Arbol = crearArbolHuffmanInterno(Char,L,Arb), crearArbolHuffman(T,Arbol).

crearArbolHuffmanInterno(Char,[],_Arb) -> {Char,{},{}};
crearArbolHuffmanInterno(Char,[Val|T],{}) when Val =:= 0 -> {null,crearArbolHuffmanInterno(Char,T,{}),{}};
crearArbolHuffmanInterno(Char,[Val|T],{}) when Val =:= 1 -> {null,{},crearArbolHuffmanInterno(Char,T,{})};
crearArbolHuffmanInterno(Char,[Val|T],{R,I,D}) when Val =:= 0 -> {R,crearArbolHuffmanInterno(Char,T,I),D};
crearArbolHuffmanInterno(Char,[Val|T],{R,I,D}) when Val =:= 1 -> {R,I,crearArbolHuffmanInterno(Char,T,D)}.

mayor(X,Y) when X > Y -> X;
mayor(_X,Y) -> Y.

caminoMasLargo(L) -> caminoMasLargo(L,0).
caminoMasLargo([],C) -> C;
caminoMasLargo([{_,List}|T],C) -> caminoMasLargo(T,mayor(length(List),C)).

writeListDescomprimir([],_Fd)->ok;
writeListDescomprimir([H|T],Fd)->
								io:format("Escribiendo: ~p~n",[H]), 
								file:write(Fd,H),
								writeListDescomprimir(T,Fd).

interpretar(_Arb,[]) -> [];
interpretar(Arb,L) -> interpretar(Arb,Arb,L,[],[]).

interpretar(Arb,{R,{},{}},L,Lchar,_Rest) -> interpretar(Arb,Arb,L, Lchar++[R],[]);
interpretar(_,_,[],Lchar,Rest) -> {Lchar,Rest};
interpretar(Arb,{_R,I,_D},[H|T],Lchar,Rest) when H =:= 0 -> interpretar(Arb,I,T,Lchar,Rest++[H]);
interpretar(Arb,{_R,_I,D},[H|T],Lchar,Rest) when H =:= 1 -> interpretar(Arb,D,T,Lchar,Rest++[H]).



interpretarFinal(_Arb,[]) -> [];
interpretarFinal(Arb,L) -> interpretarFinal(Arb,Arb,L,[]).

interpretarFinal(Arb,{R,{},{}},L,Lchar) -> interpretarFinal(Arb,Arb,L, [Lchar|[R]] );
interpretarFinal(_,_,[],Lchar) -> Lchar;
interpretarFinal(Arb,{_R,I,_D},[H|T],Lchar) when H =:= 0 -> interpretarFinal(Arb,I,T,Lchar);
interpretarFinal(Arb,{_R,_I,D},[H|T],Lchar) when H =:= 1 -> interpretarFinal(Arb,D,T,Lchar).

completeByteRev(L)->completeByteRev(L,length(L)).
completeByteRev(L,X) when X > 7->L;
completeByteRev(L,_X)-> completeByteRev([0|L],length(L)+1).

descomprimirData(L,CL,Arb,Filename) -> 
					{ok, Fd} = file:open(Filename, [append, delayed_write]),
					descomprimirData(L,CL,Arb,[],0,Fd),
					file:close(Fd),
					ok.

descomprimirData([],_CL,Arb,Acum,_Size,Fd) ->
					Final = lists:flatten(interpretarFinal(Arb,Acum)),
					writeListDescomprimir(Final,Fd);

descomprimirData([H|T],CL,Arb,Acum,Size,Fd) when Size >= CL ->
					TotalAcum = Acum ++ completeByteRev(dec2bin(H)),
					{Lelem,Lrestante} = interpretar(Arb,TotalAcum),
					file:write(Fd,Lelem),
					descomprimirData(T,CL,Arb,Lrestante,length(Lrestante),Fd);

descomprimirData([H|T],CL,Arb,Acum,_Size,Fd) -> 
					TotalAcum = Acum ++ completeByteRev(dec2bin(H)),			
					descomprimirData(T,CL,Arb,TotalAcum,length(TotalAcum),Fd).

decompress(Filename,Newfile,Symbolpath)-> 
						io:format("Descomprimiendo archivo!~n", []),
						io:format("Abriendo archivo de simbolos!~n", []),
						{ok, S} = file:read_file(Symbolpath), S,
						Symbol = erlang:binary_to_term(S),
        				io:format("Creando arbol de huffmann!~n", []),
        				ArbolHuffman = crearArbolHuffman(Symbol),
						io:format("Abriendo archivo comprimido!~n", []),
						{ok, F} = file:read_file(Filename), F,			
        				ListaCompresion = binary:bin_to_list(F),
        				io:format("Archivo comprimido de decimal a binario lista!~n", []),
        				CaminoLargo = caminoMasLargo(Symbol),
						io:format("CaminoLargo: ~p~n",[CaminoLargo]),
						descomprimirData(ListaCompresion,CaminoLargo,ArbolHuffman,Newfile).

%-------------------------------Servidor

hiloComprimir(F,Server)->spawn(fun()->compress(F,Server) end).

archivosListo(C,C,Sim,Filename,Bin)->	io:format("Comprimiendo archivo!~n", []),
										io:format("Generando tabla de simbolos!~n", []),
										Tabla = tablaSimbolos(huffman(crearListaArboles(Sim))),
										file:write_file(Filename ++ ".simbolos" ,[erlang:term_to_binary(Tabla)],[append]),
										io:format("Tabla de simbolos escrita!~n", []),
										io:format("Convirtiendo tabla en diccionario!~n", []),
										Diccionario = dict:from_list(Tabla),
										io:format("Generando archivo comprimido!~n", []),
										toCode(Bin,Diccionario,Filename),								
										io:format("Archivo comprimido!~n", []),
								  		huffmanServer(C,Tabla,Filename,C,Bin);

archivosListo(C,B,Sim,Filename,Bin)->huffmanServer(C,Sim,Filename,B,Bin).

getFrecuenciaRepetidaSuma(L,[])->L;
getFrecuenciaRepetidaSuma([H|T],[[Cant|T]|_Cola]) -> [ H+Cant | T];
getFrecuenciaRepetidaSuma([H|T],[[_Cant|_Col]|Cola]) -> getFrecuenciaRepetidaSuma([H|T],Cola).
sumarFrecuencias([],_L)->[];
sumarFrecuencias([H|T],L)-> [getFrecuenciaRepetidaSuma(H,L)|sumarFrecuencias(T,L)].

getFrecuenciaRepetida(L,[])->L;
getFrecuenciaRepetida([_H|T],[[_Cant|T]|_Cola]) -> [];
getFrecuenciaRepetida([H|T],[[_Cant|_Col]|Cola]) -> getFrecuenciaRepetida([H|T],Cola).
mergeFrecuencias(L1,L2)->mergeFrecuencias(L1,L2,[]).
mergeFrecuencias([],L,Accum)->L ++ Accum;
mergeFrecuencias([H|T],L,Accum)->  mergeFrecuencias(T,L,Accum ++ [getFrecuenciaRepetida(H,L)]).

huffmanServer(C,Symbol,Filename,N,Binaries)->
    receive

        {comprimir,Simbolos,Binary} -> 	io:format("Recibiendo tabla de frecuencias!~n", []),
        								Sumafrec = sumarFrecuencias(Symbol,Simbolos),
        								Mergefrec = sortAscending(mergeFrecuencias(Simbolos,Sumafrec)),
        					    		archivosListo(C+1,N,Mergefrec,Filename,Binaries ++ Binary);

		print ->	io:format("Simbolos: ~p~n",[Symbol]), huffmanServer(C,Symbol,Filename,N,Binaries);
        
        finalizar->	io:format("Me muero~n", []);

        X -> io:format("recibo: ~p~n",[X]), huffmanServer(C,Symbol,Filename,N,Binaries)
    end.

% Prueba:
% HuffmanServer = spawn (fun()->p2Final:huffmanServer(0,[],"b.huff",4,[])end).
% p2Final:hiloComprimir("file00", HuffmanServer).
% p2Final:decompress("b.huff", "bD.txt",  "b.huff.simbolos").