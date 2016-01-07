defmodule Logfilt.Mixfile do
  use Mix.Project

  def project do
    [app: :logfilt,
     version: "0.1.0",
     escript: escript_config,
     deps: deps]
  end

  def application do
    []
  end

  defp deps do
    []
  end
  
  defp escript_config do
    [main_module: Logfilt]
  end
end
