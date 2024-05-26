-module(imageFilter_parallel_custom).
-export([apply_filter/1]).

-include_lib("_build/default/lib/egd/src/egd.hrl").

% Entry function
apply_filter(InputPath) ->
    % Load the image
    {ok, Image} = egd:load(InputPath),
    % Get image dimensions
    {Width, Height} = egd:size(Image),
    % Create a new image for the output
    FilteredImage = egd:create(Width, Height),
    % Number of cores
    Cores = erlang:system_info(logical_processors_available),
    % Split image rows into chunks for each core
    ChunkSize = (Height + Cores - 1) div Cores,
    % Spawn processes to handle each chunk
    Pids = [spawn_link(fun() -> process_chunk(Image, FilteredImage, {Width, Height}, ChunkSize * (I - 1), ChunkSize) end) || I <- lists:seq(1, Cores)],
    % Wait for all processes to finish
    [receive done -> ok end || _ <- Pids],
    % Generate output file path
    OutputPath = generate_output_path(InputPath),
    % Save the filtered image
    egd:save(FilteredImage, OutputPath),
    % Cleanup
    egd:destroy(Image),
    egd:destroy(FilteredImage),
    ok.

% Function to process a chunk of image rows
process_chunk(Image, FilteredImage, {Width, Height}, StartRow, ChunkSize) ->
    EndRow = min(StartRow + ChunkSize, Height),
    lists:foreach(fun(Y) ->
        lists:foreach(fun(X) ->
            % Get the pixel color
            Color = egd:get_pixel(Image, X, Y),
            % Apply the filter (e.g., increase brightness)
            FilteredColor = adjust_brightness(Color, 50),
            % Set the new pixel color in the filtered image
            egd:put_pixel(FilteredImage, X, Y, FilteredColor)
        end, lists:seq(0, Width - 1))
    end, lists:seq(StartRow, EndRow - 1)),
    % Notify the main process that this chunk is done
    self() ! done.

% Function to adjust brightness of a pixel color
adjust_brightness({R, G, B, A}, Amount) ->
    % Ensure the new values are within the valid range 0-255
    NewR = min(max(R + Amount, 0), 255),
    NewG = min(max(G + Amount, 0), 255),
    NewB = min(max(B + Amount, 0), 255),
    {NewR, NewG, NewB, A}.

% Function to generate the output file path
generate_output_path(InputPath) ->
    % Extract filename from path
    Filename = filename:basename(InputPath),
    % Create the new filename
    "filtered_" ++ Filename.
