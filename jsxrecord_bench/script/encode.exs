Code.eval_file("helper.exs", "./script")

jobs = %{
  "jsx" => &:jsxrecord.encode/1,
  "euneus" => &:jsxrecordx.encode/1,
}

data = [
  "Blockchain",
  # "Giphy",
  # "GitHub",
  # "GovTrack",
  # "Issue 90",
  # "JSON Generator (Pretty)",
  # "JSON Generator",
  # "Pokedex",
  # "UTF-8 escaped",
  # "UTF-8 unescaped"
]

inputs =
  for name <- data, into: %{} do
    name
    |> JSXRecordBench.Helper.read_data()
    |> Jason.decode!()
    |> (&{name, &1}).()
  end

JSXRecordBench.Helper.run(
  "encode",
  jobs,
  inputs,
  %{
    # graph: true,
    # parallel: 1,
    # warmup: 5,
    # time: 5,
    # memory_time: 1,
  }
)
