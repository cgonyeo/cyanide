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
`~/.config/cyanide/cyanide.conf` in the following format:

```
TODO
```

If cyanide has not been run before, the database needs to be manually
initialized. This can be done with the `init_db.sql` file:

```
curl -o init_cyanide_db.sql https://github.com/dgonyeo/cyanide/...
psql -U DB_USER [MAYBE OTHER OPTIONS] -f init_db.sql
```

Cyanide will hopefully soon handle the database initialization step itself.

Running concurrent copies of cyanide is not recommended. It could be fine, but
if data is being modified the changes may not appear across all instances.

### Running cyanide on boot

TODO

## License

GPLv3

[releases]: https://example.com
[stack]: https://docs.haskellstack.org/en/stable/README/
