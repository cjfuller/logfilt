defmodule LogFilt do

  @match_table [
          {~r/INFO/, :green},
          {~r/WARNING/, :yellow},
          {~r/ERROR/, :red},
          {~r/CRITICAL/, :magenta},
          {~r/^.*!!!.*$/, :cyan},
          {~r/\d{4}-\d{2}-\d{2}[^]]*]/, :grey},
      ]

  @color_map %{
                 red: "\x1b[31;1m",
                 green: "\x1b[32;1m",
                 yellow: "\x1b[33;1m",
                 magenta: "\x1b[35;1m",
                 cyan: "\x1b[36;1m",
                 grey: "\x1b[30;1m",
                 clear: "\x1b[0m",
             }


  @doc """
  Get color annotations for matches of a pattern in the provided string.

  A priority parameter controls how conflicting colorations are resolved.

  Returns: {priority, start_position, len, color}
  """
  def maybe_colorize_partial priority, color, pat, line do
    Regex.scan(pat, line, return: :index)
    |> Enum.map fn [{start, len}|_] -> {priority, start, len, color} end
  end

  @doc """
  Get the color from the color annotation prior to the specified position.

  Default to :clear if there's no annotation before the current position.
  """
  def prev_color(accumlated_colors, pos) do
    accumlated_colors
    |> Enum.sort
    |> Enum.take_while(fn {i, _} -> i <= pos end)
    |> List.last
    |> case do
         nil -> :clear
         {_, color} -> color
       end
  end

  @doc """
  Turn a list of colorization tuples into a list of positions where the color changes.

  The colorization tuples are {priority, start, len, color}.  Colorizations
  are applied according to start poistion, using the priority parameter to
  break any ties.

  Return a list of the format [{pos, color}...]
  """
  def prioritized_colorization_to_color_change_offsets colorization do
    colorization
    |> Enum.sort_by(fn {priority, start, len, color} ->
                      {start, -1 * priority, len, color} end)
    |> Enum.reduce([], fn {_, start, len, color}, acc ->
                     acc ++ [{start, color},
                             {start + len, prev_color(acc, start + len)}]
                   end)
  end

  @doc """
  Apply a list of positions where the color changes to a string.

  This will insert the appropriate escape codes into the string at those
  positions.
  """
  def apply_prioritized_colorization colorization, line do
    color_tup = (prioritized_colorization_to_color_change_offsets(colorization)
      |> Enum.sort
      |> Enum.reduce({0, ""}, fn {pos, color}, {startpos, str} ->
                       newstr = (str <> String.slice(line, startpos, pos - startpos)
                                 <> @color_map[color])
                       newpos = pos
                       {newpos, newstr}
                     end))

    {last_pos, color_str} = color_tup
    color_str <> String.slice(line, last_pos, String.length(line)) <> @color_map[:clear]
  end

  @doc """
  Colorize a string of text accorting to the rules in @match_table.
  """
  def colorize_line line do
    @match_table
    |> Enum.with_index
    |> Enum.map(fn {{pat, color}, i} -> maybe_colorize_partial(i, color, pat, line) end)
    |> List.flatten
    |> apply_prioritized_colorization(line)
  end

  @doc """
  Continually read from stdin, colorize the lines, and write to stdout.
  """
  def main do
    case IO.read :line do
      {:error, _} ->
        nil
      :eof ->
        nil
      s ->
        IO.puts colorize_line String.strip s
        main
    end
  end

end

LogFilt.main
