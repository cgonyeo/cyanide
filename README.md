# Cyanide: home bar management system

# This README isn't quite done yet. Come back later.

## What is cyanide?

Cyanide is an ncurses-style console application that tracks and manages
ingredients and recipes for a home bar. It can track current inventory and past
purchases for ingredients, show you what recipes you can make with your current
inventory, and filter recipes by spirit type, glass, and more.

## Acquiring cyanide

Cyanide can be downloaded from the [releases page on GitHub][releases]:

```
curl -o cyanide https://github.com/dgonyeo/cyanide/...
chmod +x cyanide
```

Cyanide can also be built from source with [stack][stack]:

```
git clone https://github.com/dgonyeo/cyanide
cd cyanide
stack install
```

## Running cyanide

Cyanide was developed, tested, and is actively used on Linux. It's possible it
will run without issue on MacOS or Windows.

Cyanide requires access to a working PostgreSQL database, as that's how it
stores information. Database credentials are stored at
`~/.config/cyanide/cyanide.conf`. If this file doesn't exist when cyanide is
run, cyanide will ask the user if cyanide should create the file with the
following contents:

```
[DATABASE]
host = localhost
port = 5432
user = cyanide
password = cyanide
database = cyanide
```

Running concurrent copies of cyanide is not recommended. It could be fine, but
if data is being modified the changes may not appear across all instances.

## License

GPLv3

[releases]: https://example.com
[stack]: https://docs.haskellstack.org/en/stable/README/
