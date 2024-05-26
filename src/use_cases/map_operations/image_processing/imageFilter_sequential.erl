-module(imageFilter_sequential).
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
    % Apply the filter to each pixel
    apply_filter_to_pixels(Image, FilteredImage, Width, Height),
    % Generate output file path
    OutputPath = generate_output_path(InputPath),
    % Save the filtered image
    egd:save(FilteredImage, OutputPath),
    % Cleanup
    egd:destroy(Image),
    egd:destroy(FilteredImage),
    ok.

% Function to apply the filter to each pixel
apply_filter_to_pixels(Image, FilteredImage, Width, Height) ->
    lists:foreach(fun(Y) ->
        lists:foreach(fun(X) ->
            % Get the pixel color
            Color = egd:get_pixel(Image, X, Y),
            % Apply the filter (e.g., increase brightness)
            FilteredColor = adjust_brightness(Color, 50),
            % Set the new pixel color in the filtered image
            egd:put_pixel(FilteredImage, X, Y, FilteredColor)
        end, lists:seq(0, Width - 1))
    end, lists:seq(0, Height - 1)).

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
