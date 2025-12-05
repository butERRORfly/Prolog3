% Поиск в глубину
path(X,Y,P) :- path1([X],Y,P).

prolong([X|T],[Y,X|T]) :-
    move(X,Y),
    not(member(Y, [X|T])).

path1([X|T],X,[X|T]).
path1(P,Y,R) :-
    prolong(P,P1), path1(P1,Y,R).

initial([w,w,w,e,b,b,b]).
goal([b,b,b,e,w,w,w]).

% Перемещение в соседнюю пустую лунку (вправо)
move(State, NewState) :-
    append(Left, [e, Ball|Right], State),
    member(Ball, [b,w]),
    append(Left, [Ball, e|Right], NewState).

% Перемещение в соседнюю пустую лунку (влево)
move(State, NewState) :-
    append(Left, [Ball, e|Right], State),
    member(Ball, [b,w]),
    append(Left, [e, Ball|Right], NewState).

% Прыжок через один шар вправо
move(State, NewState) :-
    append(Left, [e, Ball1, Ball2|Right], State),
    member(Ball1, [b,w]), member(Ball2, [b,w]),
    Ball1 \= e, Ball2 \= e,
    append(Left, [Ball2, Ball1, e|Right], NewState).

% Прыжок через один шар влево
move(State, NewState) :-
    append(Left, [Ball1, Ball2, e|Right], State),
    member(Ball1, [b,w]), member(Ball2, [b,w]),
    Ball1 \= e, Ball2 \= e,
    append(Left, [e, Ball2, Ball1|Right], NewState).

solve_deep_search :-
    initial(Start),
    goal(Goal),
    path(Start, Goal, Path),
    reverse(Path, ReversedPath),
    write('Решение найдено!'), nl, nl,
    print_solution(ReversedPath).

% Вывод решения с нумерацией шагов
print_solution(Path) :-
    print_steps(Path, 1).

print_steps([], _).
print_steps([State], N) :-
    format('~w (ЦЕЛЬ): ', [N]),
    write_state(State).

print_steps([State|Rest], N) :-
    (N =:= 1 ->
        format('~w (НАЧАЛО): ', [N])
    ;
        format('~w: ', [N])
    ),
    write_state(State),
    N1 is N + 1,
    print_steps(Rest, N1).

write_state(State) :-
    write('['),
    write_elements(State),
    write(']'), nl.

write_elements([]).
write_elements([X]) :- write(X).
write_elements([X|Rest]) :-
    write(X), write(','),
    write_elements(Rest).

% Поиск в ширину
path_breadth(X,Y,P) :- bdth([[X]],Y,P).
bdth([[X|T]|_], X,[X|T]).
bdth([P|QI],X,R) :-
    findall(Z,prolong(P,Z),T),
    append(QI,T,QO),!,
    bdth(QO,X,R).
bdth([_|T],Y,L) :- bdth(T,Y,L).

solve_bdth_search :-
    initial(Start),
    goal(Goal),
    path_breadth(Start, Goal, Solution),
    reverse(Solution, ReversedSolution),
    write('Решение найдено! Длина пути: '),
    length(ReversedSolution, Len),
    write(Len), nl, nl,
    write('Последовательность состояний:'), nl,
    print_solution(ReversedSolution).

% Поиск с погружением (ограничение глубины)
depth_id([Finish|T], Finish, [Finish|T], 0).
depth_id(Path, Finish, R, N) :- 
    N > 0,
    prolong(Path, NewPath), 
    N1 is N - 1, 
    depth_id(NewPath, Finish, R, N1).
    
% Решение с использованием поиска с ограничением глубины
solve_depth_limit(Limit) :-
    initial(Start),
    goal(Finish),
    depth_id([Start], Finish, Path, Limit),
    reverse(Path, ReversedPath),
    format('Решение найдено с ограничением глубины ~w!~n~n', [Limit]),
    print_solution(ReversedPath).

% Поиск с итерационным погружением
counter(1).
counter(M) :- counter(N), M is N + 1.

search_id(Start, Finish, Path) :-
    counter(Level),
    depth_id([Start], Finish, Path, Level).

% Решение с использованием поиска с итерационным погружением
solve_iterative_deepening :-
    initial(Start),
    goal(Finish),
    search_id(Start, Finish, Path),
    reverse(Path, ReversedPath),
    write('Решение найдено методом итерационного погружения!'), nl, nl,
    print_solution(ReversedPath).