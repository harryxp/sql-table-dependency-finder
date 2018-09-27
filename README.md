# sql-table-dependency-finder

Parse SQL and find table dependencies.

Hacked together in one evening for a task at hand.

    stack build
    stack exec -- sql-table-dependency-finder-exe < example.txt > example.out
    cat example.out
    stack exec -- sql-table-dependency-finder-exe --gv < example.out
