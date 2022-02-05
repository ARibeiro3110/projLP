% Afonso da Conceicao Ribeiro, 102763, Ano 1

% Projeto - "Solucionador de Puzzles Hashi (Parte I)" - 04/02/2022

% afonsodaconceicaoribeiro@tecnico.ulisboa.pt

% Unidade Curricular de Logica para Programacao

% Licenciatura em Engenharia Informatica e de Computadores - Alameda

% Instituto Superior Tecnico - Universidade de Lisboa

% Ano Letivo 2021/2022, Semestre 1, Periodo 2

:- [codigo_comum].





%  ============================================================
%  2.1. Predicado extrai_ilhas_linha/3
%
%  extrai_ilhas_linha(N_L, Linha, Ilhas), em que N_L e um
%  inteiro positivo, correspondente ao numero de uma linha e
%  Linha e uma lista correspondente a uma linha de um puzzle,
%  significa que Ilhas e a lista ordenada (ilhas da esquerda
%  para a direita) cujos elementos sao as ilhas da linha Linha.
%  ============================================================



extrai_ilhas_linha(N_L, Linha, Ilhas) :-

    findall( ilha(N_P, (N_L, N_C)) ,

             ( nth1(N_C, Linha, N_P),
               N_P =\= 0
             ),

             Ilhas).





%  ============================================================
%  2.2. Predicado ilhas/2
%
%  ilhas(Puz, Ilhas), em que Puz e um puzzle, significa que
%  Ilhas e a lista ordenada (ilhas da esquerda para a direita
%  e de cima para baixo) cujos elementos sao as ilhas de Puz.
%  ============================================================



ilhas(Puz, Ilhas) :-

    findall(Ilha,

            ( nth1(N_L, Puz, Linha),
              extrai_ilhas_linha(N_L, Linha, Ilhas_Linha),
              member(Ilha, Ilhas_Linha)
            ),

            Ilhas).





%  ============================================================
%  2.3. Predicados ilha_Pos/2, ilha_L/2, ilha_C/2,
%                  ordena_ilhas/2 e vizinhas/3
%
%  ilha_Pos(Ilha, Pos), ilha_L(Ilha, L) e ilha_C(Ilha, C), em
%  que Ilha e uma ilha, significam, respetivamente, que Pos e
%  a posicao de Ilha, L e a linha em que Ilha se encontra e
%  C e a coluna em que Ilha se encontra.
%
%  ordena_ilhas(Ilhas, Ilhas_ord), em que Ilhas e Ilhas_ord sao
%  listas de ilhas, significa que Ilhas_ord e a lista ordenada
%  das ilhas de Ilhas.
%
%  vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas e a lista de
%  ilhas de um puzzle e Ilha e uma dessas ilhas, significa que
%  Vizinhas e a lista ordenada (ilhas de cima para baixo e da
%  esquerda para a direita) cujos elementos sao as ilhas
%  vizinhas de Ilha.
%  ============================================================


ilha_Pos( ilha(_, Pos) , Pos).

ilha_L( ilha(_, (L,_)) , L).

ilha_C( ilha(_, (_,C)) , C).



ordena_ilhas(Ilhas, Ilhas_ord) :-

    findall(Pos,

            member( ilha(N, Pos) , Ilhas),

            Posicoes),

    sort(Posicoes, Posicoes_ord),

    findall( ilha(N, Pos) ,

             ( member(Pos, Posicoes_ord),
               member( ilha(N, Pos) , Ilhas)
             ),

             Ilhas_ord).



vizinhas(Ilhas, Ilha, Vizinhas) :-

    findall(Vizinha,

            ( member(Vizinha, Ilhas),
              ilha_L(Ilha, L),
              ilha_L(Vizinha, L)
            ),

            Linha),

    findall(Vizinha,

            ( member(Vizinha, Ilhas),
              ilha_C(Ilha, C),
              ilha_C(Vizinha, C)
            ),

            Coluna),

    % Linha e Coluna sao as listas de todas as ilhas da linha e
    % da coluna, respetivamente, onde se encontra a ilha Ilha.

    nth1(P_L_I, Linha, Ilha),

    nth1(P_C_I, Coluna, Ilha),

    findall(Vizinha,

            % Pretende-se encontrar as ilhas ...

            (   nth1(P_L_V, Linha, Vizinha),

                (   P_L_V is P_L_I - 1
                ;   P_L_V is P_L_I + 1
                )

                % ... que pertencem a lista Linha e se encontram
                % na posicao imediatamente antes ou depois da
                % posicao da ilha Ilha nessa mesma lista, ...

            ;   nth1(P_C_V, Coluna, Vizinha),

                (   P_C_V is P_C_I - 1
                ;   P_C_V is P_C_I + 1
                )

                % ... ou que pertencem a lista Coluna e se
                % encontram na posicao imediatamente antes ou
                % depois da posicao da ilha Ilha nessa mesma lista.

            ),

            Vizinhas_aux),

    ordena_ilhas(Vizinhas_aux, Vizinhas),

    !.




%  ============================================================
%  2.4. Predicado estado/2
%
%  estado(Ilhas, Estado), em que Ilhas e a lista de ilhas de
%  um puzzle, significa que Estado e a lista ordenada cujos
%  elementos sao as entradas referentes a cada uma das ilhas
%  de Ilhas.
%  ============================================================



estado(Ilhas, Estado) :-

    findall( [ Ilha, Vizinhas, [] ] ,

             ( member(Ilha, Ilhas),
               vizinhas(Ilhas, Ilha, Vizinhas)
             ),

             Estado).





%  ============================================================
%  2.5. Predicado posicoes_entre/2
%
%  posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao
%  posicoes, significa que Posicoes e a lista ordenada de
%  posicoes entre Pos1 e Pos2 (excluindo Pos1 e Pos2). Se Pos1
%  e Pos2 nao pertencerem a mesma linha ou a mesma coluna, o
%  resultado e false.
%  ============================================================



posicoes_entre((L, C1), (L, C2), Posicoes) :-

    C_m is min(C1, C2) + 1,

    C_M is max(C1, C2) - 1,

    % Sem somar e subtrair 1, as posicoes (L, C_m) e (L, C_M),
    % respetivamente, seriam incluidas.

    findall( (L, C),

             between(C_m, C_M, C),

             Posicoes),

    !.



posicoes_entre((L1, C), (L2, C), Posicoes) :-

    L_m is min(L1, L2) + 1,

    L_M is max(L1, L2) - 1,

    % Sem somar e subtrair 1, as posicoes (L_m, C) e (L_M, C),
    % respetivamente, seriam incluidas.

    findall( (L, C),

             between(L_m, L_M, L),

             Posicoes),

    !.





%  ============================================================
%  2.6. Predicado cria_ponte/3
%
%  cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao duas
%  posicoes, significa que Ponte e uma ponte entre essas duas
%  posicoes.
%  ============================================================



cria_ponte( Pos1, Pos2, ponte(Pos_m, Pos_M) ) :-

    sort([Pos1, Pos2], Pos),

    nth1(1, Pos, Pos_m),

    nth1(2, Pos, Pos_M).





%  ============================================================
%  2.7. Predicado caminho_livre/5
%
%  caminho_livre(Pos1, Pos2, Posicoes, I, Vz), em que Pos1 e
%  Pos2 sao posicoes, Posicoes e a lista ordenada de posicoes
%  entre Pos1 e Pos2, I e uma ilha, e Vz e uma das suas
%  vizinhas, significa que a adicao da ponte ponte(Pos1, Pos2)
%  nao faz com que I e Vz deixem de ser vizinhas.
%  ============================================================



caminho_livre(Pos1, Pos2, Posicoes, ilha(_, Pos1), ilha(_, Pos2)) :-

    posicoes_entre(Pos1, Pos2, Posicoes), !.



caminho_livre(Pos1, Pos2, Posicoes, ilha(_, Pos2), ilha(_, Pos1)) :-

    posicoes_entre(Pos1, Pos2, Posicoes), !.



caminho_livre(Pos1, Pos2, Pos_ponte, ilha(_, P_I), ilha(_, P_V)) :-

    posicoes_entre(Pos1, Pos2, Pos_ponte),

    posicoes_entre(P_I, P_V, Pos_entre_ilhas),

    intersection(Pos_ponte, Pos_entre_ilhas, []).

    % Se as listas nao tiverem elementos em comum, significa que
    % a ponte nao ocupa nenhuma posicao entre as duas ilhas.





%  ============================================================
%  2.8. Predicado actualiza_vizinhas_entrada/5
%
%  actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,
%  Nova_Entrada), em que Pos1 e Pos2 sao as posicoes entre as
%  quais ira ser adicionada uma ponte, Posicoes e a lista
%  ordenada de posicoes entre Pos1 e Pos2, e Entrada e uma
%  entrada, significa que Nova_entrada e igual a Entrada,
%  exceto no que diz respeito a lista de ilhas vizinhas; esta
%  deve ser atualizada, removendo as ilhas que deixaram de ser
%  vizinhas, apos a adicao da ponte.
%  ============================================================



actualiza_vizinhas_entrada( Pos1,
                            Pos2,
                            Posicoes,
                            [Ilha, Vizinhas_antes, Pontes],
                            [Ilha, Vizinhas_depois, Pontes] ) :-

    posicoes_entre(Pos1, Pos2, Posicoes),

    findall(Vizinha,

            ( member(Vizinha, Vizinhas_antes),
              caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha)
            ),

            Vizinhas_depois).





%  ============================================================
%  2.9. Predicado actualiza_vizinhas_apos_pontes/4
%
%  actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2,
%  Novo_estado), em que Estado e um estado, Pos1 e Pos2 sao as
%  posicoes entre as quais foi adicionada uma ponte, significa
%  que Novo_estado e o estado que se obtem de Estado apos a
%  atualizacao das ilhas vizinhas de cada uma das suas entradas.
%  ============================================================



actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-

    findall(Nova_entrada,

            ( member(Entrada, Estado),
              posicoes_entre(Pos1, Pos2, Posicoes),
              actualiza_vizinhas_entrada( Pos1,
                                          Pos2,
                                          Posicoes,
                                          Entrada,
                                          Nova_entrada )
            ),

            Novo_estado).





%  ============================================================
%  2.10. Predicado ilhas_terminadas/2
%
%  ilhas_terminadas(Estado, Ilhas_term), em que Estado e um
%  estado, significa que Ilhas_term e a lista de ilhas que ja
%  tem todas as pontes associadas, designadas por ilhas
%  terminadas. Se a entrada referente a uma ilha for
%  [ilha(N_pontes, Pos), Vizinhas, Pontes], esta ilha esta
%  terminada se N_pontes for diferente de 'X' e o comprimento
%  da lista Pontes for N_pontes.
%  ============================================================



ilhas_terminadas(Estado, Ilhas_term) :-

    findall( ilha(N_pontes, Pos) ,

             ( member( [ ilha(N_pontes, Pos) , _, Pontes] , Estado),
               N_pontes \== 'X',
               length(Pontes, N_pontes)
             ),

             Ilhas_term).





%  ============================================================
%  2.11. Predicado tira_ilhas_terminadas_entrada/3
%
%  tira_ilhas_terminadas_entrada(Ilhas_term, Entrada,
%  Nova_entrada), em que Ilhas_term e uma lista de ilhas
%  terminadas e Entrada e uma entrada, significa que
%  Nova_entrada e a entrada resultante de remover as ilhas de
%  Ilhas_term, da lista de ilhas vizinhas de entrada.
%  ============================================================



tira_ilhas_terminadas_entrada( Ilhas_term,
                               [Ilha, Vizinhas_antes, Pontes],
                               [Ilha, Vizinhas_depois, Pontes] ) :-

    subtract(Vizinhas_antes, Ilhas_term, Vizinhas_depois).





%  ============================================================
%  2.12. Predicado tira_ilhas_terminadas/3
%
%  tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em
%  que Estado e um estado e Ilhas_term e uma lista de ilhas
%  terminadas, significa que Novo_estado e o estado resultante
%  de aplicar o predicado tira_ilhas_terminadas_entrada a cada
%  uma das entradas de Estado.
%  ============================================================



tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-

    findall(Nova_entrada,

            ( member(Entrada, Estado),
              tira_ilhas_terminadas_entrada( Ilhas_term,
                                             Entrada,
                                             Nova_entrada )
            ),

            Novo_estado).





%  ============================================================
%  2.13. Predicado marca_ilhas_terminadas_entrada/3
%
%  marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,
%  Nova_entrada), em que Ilhas_term e uma lista de ilhas
%  terminadas e Entrada e uma entrada, significa que
%  Nova_entrada e a entrada obtida de Entrada da seguinte
%  forma: se a ilha de Entrada pertencer a Ilhas_term, o numero
%  de pontes desta e substituido por 'X'; em caso contrario
%  Nova_entrada e igual a Entrada.
%  ============================================================



marca_ilhas_terminadas_entrada( Ilhas_term,
                                [ ilha(N_pontes, Pos) , V, P ],
                                [ ilha(   'X'  , Pos) , V, P ] ) :-

    member( ilha(N_pontes, Pos) , Ilhas_term), !.



marca_ilhas_terminadas_entrada(_, Entrada, Entrada).





%  ============================================================
%  2.14. Predicado marca_ilhas_terminadas/3
%
%  marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em
%  que Estado e um estado e Ilhas_term e uma lista de ilhas
%  terminadas, significa que Novo_estado e o estado resultante
%  de aplicar o predicado marca_ilhas_terminadas_entrada a cada
%  uma das entradas de Estado.
%  ============================================================



marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-

    findall(Nova_entrada,

            ( member(Entrada, Estado),
              marca_ilhas_terminadas_entrada(Ilhas_term,
                                             Entrada,
                                             Nova_entrada)
            ),

            Novo_estado).





%  ============================================================
%  2.15. Predicado trata_ilhas_terminadas/3
%
%  trata_ilhas_terminadas(Estado, Novo_estado), em que Estado e
%  um estado, significa que Novo_estado e o estado resultante
%  de aplicar os predicados tira_ilhas_terminadas e
%  marca_ilhas_terminadas a Estado.
%  ============================================================



trata_ilhas_terminadas(Estado, Novo_estado) :-

    ilhas_terminadas(Estado, Ilhas_term),

    tira_ilhas_terminadas(Estado, Ilhas_term, Estado_aux),

    marca_ilhas_terminadas(Estado_aux, Ilhas_term, Novo_estado).





%  ============================================================
%  2.16. Predicados repete_el/3, junta_pontes_aux/5
%                   e junta_pontes/5
%
%  repete_el(El, N, L) significa que L e a lista constituida
%  por N ocorrencias do elemento El.
%  (predicado definido nos exercicios resolvidos da disciplina)
%
%  junta_pontes_aux(Estado, Num_pontes, Ilha1, Ilha2, Estado_aux)
%  cria a(s) ponte(s) entre Ilha1 e Ilha2 e adiciona-as as
%  entradas correspondentes a estas ilhas. Este predicado obtem
%  o estado Estado_aux, que, no predicado junta_pontes/5, sera
%  atualizado por aplicacao dos predicados
%  actualiza_vizinhas_apos_pontes/4 e trata_ilhas_terminadas/2.
%
%  junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado),
%  em que Estado e um estado e Ilha1 e Ilha2 sao duas ilhas,
%  significa que Novo_estado e o estado que se obtem de Estado
%  por adicao de Num_pontes pontes entre Ilha1 e Ilha2.
%  ============================================================



repete_el(E, 1, [E]).

repete_el(E, N, [E|R]) :-
    N > 1,
    N_1 is N - 1,
    repete_el(E, N_1, R).



junta_pontes_aux([], _, _, _, []).

junta_pontes_aux( [ [Ilha, Vizinhas, Pontes_inicial] | R ],
                  Num_pontes,
                  Ilha1,
                  Ilha2,
                  [ [Ilha, Vizinhas, Pontes_final] | R_aux ] ) :-

    (   (   Ilha = Ilha1
        ;   Ilha = Ilha2
        ),

        ilha_Pos(Ilha1, Pos1),

        ilha_Pos(Ilha2, Pos2),

        cria_ponte(Pos1, Pos2, Ponte),

        repete_el(Ponte, Num_pontes, Pontes_novas),

        append(Pontes_inicial, Pontes_novas, Pontes_final),

        !

        % Caso a ilha Ilha seja uma das ilhas entre as quais se
        % pretende criar ponte(s), Pontes_final sera a lista
        % correspondente a juncao das listas Pontes_inicial e
        % Pontes_novas, sendo esta ultima a lista da(s) ponte(s)
        % criadas.

    ;   Pontes_final = Pontes_inicial

        % Caso contrario, a lista Pontes_final e igual a lista
        % Pontes_inicial.

    ),

    junta_pontes_aux(R, Num_pontes, Ilha1, Ilha2, R_aux).



junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-

    junta_pontes_aux(Estado, Num_pontes, Ilha1, Ilha2, Estado_aux),

    ilha_Pos(Ilha1, Pos1),
    ilha_Pos(Ilha2, Pos2),

    actualiza_vizinhas_apos_pontes( Estado_aux,
                                    Pos1,
                                    Pos2,
                                    Estado_aux_2 ),

    trata_ilhas_terminadas(Estado_aux_2, Novo_estado).










