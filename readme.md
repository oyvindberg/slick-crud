## slick-crud - Admin tool for Slick database tables

slick-crud exposes your database tables through a web editor interface, so you can free up your
time from writing mundane admin tools.

It provides the following features:
- Typeclass driven rendering and parsing of values
- Browsing data (table editors are composable and are linked, and can be presented according to a database query (sorting, filtering, projections, etc))
- Interface for creating new rows and updating individual values, both in a typesafe way
- Deleting rows
- Can mark types and/or tables as read-only
- Optional/Nullable values
- Notifications of (failed) updates
- (Optional) Changelog in a database with all updates (not creates and deletes for now)

slick-crud is available for slick 3.0.0 on scala 2.11.
 
Initial idea and implementation by [@teigen][teigen], originally written for penger.no, maintenance/rewrite by [@oyvindberg][oyvindberg]

[Demo application](http://olvind.com/slick-crud)

## Usage

slick-crud requires tables with a single (non-composite) primary key. It also requires instances of `SimpleCell`
 for every type which is used in a `Table`. All the individual cells are automatically collected in a tupled `CellRow`.
 If your table is projected to a type other than a Tuple, you also need to use `mappedCellRow()`
 to provide mapping back and forth to a tuple representation.

Furthermore, you need to connect the javascript generated from the scala.js frontend and make it available from your web application.

A complete, runnable example is available [here][demo].

Run it by cloning the git project and run `sbt run`


## Status
Slick doesn't exactly enable this use out of the box, which resulted in some [hairy][columnPicker] [code][astParser].
Although we have used this for a long while internally, we fully expect there to be issues as people use
it more creatively. Bug reports welcome :)

The Scala.js frontend mostly works, but could still need a lot of love.

We will try to keep the public API relatively constant, but no guarantees just yet.

[teigen]: https://github.com/teigen
[oyvindberg]: https://github.com/oyvindberg
[demo]: demo/src/main/scala/com/olvind/crud/demo/CrudDemoWebApp.scala
[columnPicker]: crud/jvm/src/main/scala/com/olvind/crud/server/columnPickers.scala
[astParser]: crud/jvm/src/main/scala/com/olvind/crud/server/astParsers.scala
