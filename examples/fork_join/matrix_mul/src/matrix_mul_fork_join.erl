-module(matrix_mul_fork_join).
-export([fork_join_multiply/2]).

-import(matrix_mul_sequential, [multiply/2, matrix_dimensions/1, validate_dimensions/2, dot_product/2, transpose/1]).

% Function to multiply two matrices using fork-join pattern
fork_join_multiply(Matrix1, Matrix2) ->
    % Determine matrix dimensions
    {_Rows1, Columns1} = matrix_dimensions(Matrix1),
    {Rows2, _Columns2} = matrix_dimensions(Matrix2),
    validate_dimensions(Columns1, Rows2),

    % Define the multiplication function for each chunk
    Fun = fun([Chunk]) -> 
                matrix_mul_sequential:multiply(Chunk, Matrix2) 
          end,

    % Process the function using fork-join pattern
    ResultMatrix = fork_join:process(Fun, chunk_matrix(Matrix1)),
    
    % Return the resulting matrix
    ResultMatrix.

% Function to chunk the matrix into rows
chunk_matrix(Matrix) ->
    {ChunkSize, Remainder} = {length(Matrix) div erlang:system_info(schedulers_online), length(Matrix) rem erlang:system_info(schedulers_online)},
    lists:map(fun(N) ->
                     lists:sublist(Matrix, (N - 1) * ChunkSize + 1,
                                   ChunkSize + if N =< Remainder -> 1; true -> 0 end)
              end, lists:seq(1, erlang:system_info(schedulers_online))).
