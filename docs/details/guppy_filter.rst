This subcommand will filter pqueries in one or more placefiles. Placements can
be filtered either by name or by tax_id.

By default, all pqueries present in any input file will be present in the
output file. In this case, adding :option:`-Er` or :option:`-Ex` flags will
cause pqueries to be excluded from the output if they match any of the provided
regexps or tax_ids. Adding :option:`-Ir` or :option:`-Ix` flags will re-add any
pqueries which would have otherwise been excluded.

If the options :option:`-Vr` or :option:`-Vx` are provided, the default will
instead be to exclude all pqueries, unless (respectively) :option:`-Ir` or
:option:`-Ix` are provided to include (respectively) specific placement names
or tax_ids. In this case, adding :option:`-Er` or :option:`-Ex` flags will
exclude pqueries which would have otherwise been included.

Whether or not :option:`-Vr` or :option:`-Vx` have been provided, a placement
will be included or excluded if *any* (as opposed to *all*) of the provided
inclusions or exclusions match.

When using the :option:`-Ix` or :option:`-Ex` options, it is required to also
pass the :option:`-c` option to provide a reference package for looking up
taxonomic data. The `--cutoff`` flag also controls whether :option:`-Ix` or
:option:`-Ex` will match for a particular placement: neither flag will match
unless the likelihood that the placement matches that tax_id is greater than
the value provided for the `--cutoff` flag.

Since pqueries can have multiple names, the :option:`-Ir` and :option:`-Er`
flags will filter the names a placement has. If a placement has had all of its
names filtered out, it won't be present in the output.
