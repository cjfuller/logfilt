defmodule Logfilt do

  @match_table [
          {~r/INFO/, :green},
          {~r/WARNING/, :yellow},
          {~r/ERROR/, :red},
          {~r/CRITICAL/, :magenta},
          {~r/^.*!!!.*$/, :cyan},
          {~r/\d{4}-\d{2}-\d{2}[^]]*]/, :grey},
      ]

  @delete_lines [
    ~r/recording\.py/,
    ~r{GET /gae_mini_profiler},
    ~r/Stripped prohibited headers from URLFetch request: \['Host'\]/,
    ~r/Mismatch between XSRF header \(None\) and cookie/
  ]

  @delete_expressions [
    ~r/\d{4}-\d{2}-\d{2}[^]]*]/
  ]

  @color_map %{
                 red: "\x1b[31m",
                 green: "\x1b[32m",
                 yellow: "\x1b[33m",
                 magenta: "\x1b[35m",
                 cyan: "\x1b[36m",
                 grey: "\x1b[30m",
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

  def delete_expressions line do
    Enum.reduce(@delete_expressions, line, fn regex, repl -> Regex.replace(regex, repl, "") end)
  end

  @doc """
  Continually read from stdin, colorize the lines, and write to stdout.
  """
  def main(_args) do
    IO.stream(:stdio, :line)
    |> Stream.filter(fn line -> Enum.all?(@delete_lines, fn regex -> not Regex.match?(regex, line) end) end)
    |> Stream.map(&String.strip/1)
    |> Stream.map(&delete_expressions/1)
    |> Stream.map(&colorize_line/1)
    |> Enum.each(&IO.puts(&1))
  end

end
