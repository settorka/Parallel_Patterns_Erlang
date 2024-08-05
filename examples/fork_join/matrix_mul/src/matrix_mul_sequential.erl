-module(matrix_mul_sequential).
-export([multiply/2]).

% Function to multiply two matrices
multiply(Matrix1, Matrix2) ->
    % Validate matrix dimensions
    {_Rows1, Columns1} = matrix_dimensions(Matrix1),
    {Rows2, Columns2} = matrix_dimensions(Matrix2),
    validate_dimensions(Columns1, Rows2),

    % Perform matrix multiplication
    ResultMatrix = multiply_matrices(Matrix1, Matrix2, Columns2),

    % Return the resulting matrix
    ResultMatrix.

% Function to validate matrix dimensions
validate_dimensions(Columns1, Rows2) when Columns1 == Rows2 ->
    ok;
validate_dimensions(_, _) ->
    erlang:error(badarg).

% Function to extract matrix dimensions
matrix_dimensions(Matrix) ->
    {length(Matrix), length(hd(Matrix))}.

% Function to multiply two matrices
multiply_matrices(Matrix1, Matrix2, _Columns2) ->
    lists:map(fun(Row1) ->
                    lists:map(fun(Column2) ->
                                    dot_product(Row1, Column2)
                            end, transpose(Matrix2))
            end, Matrix1).

% Function to compute dot product of two lists
dot_product(List1, List2) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, List1, List2)).

% Function to transpose a matrix
transpose(Matrix) ->
    case Matrix of
        [] -> [];
        [FirstRow | RestRows] ->
            lists:foldl(fun(Elem, Acc) ->
                                lists:zipwith(fun(H, T) -> [H | T] end, Elem, Acc)
                end, lists:duplicate(length(FirstRow), []), [FirstRow | RestRows])
    end.