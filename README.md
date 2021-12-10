# AdventOfCode 2021

This repository contains my Haskell solutions to [Advent of Code](https://adventofcode.com/)

Solutions can be run by compiling main and running `./main [year] [?day] [?part]` (`main.exe [year] [?day] [?part]` for Windows), where just running it with a year will print all available solutions of that year without running tests, running it with a day will run both solutions of that day including tests, and running it with a part will run only that part and its test (alternatively `runhaskell main.hs [year] [?day] [?part]`)

In order for this to work all solutions should be in Year[year]/Solutions and formatted as Day[day].hs, there should be no other files in this folder (specifically with higher alphabetical name).

Note that the solutions get loaded dynamically and are thus compiled during runtime. Running solutions directly will get faster results.

Requires [hint](https://hackage.haskell.org/package/hint),
[MissingH](https://hackage.haskell.org/package/MissingH),
[multiset](https://hackage.haskell.org/package/multiset-0.3.4.3),
[Data-Filter](https://hackage.haskell.org/package/data-filter-0.1.0.0/docs/Data-Filter.html)
.

## License
MIT. See [LICENSE](LICENSE).
