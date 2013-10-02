JSONWrench - build and manipulate JSON from the commandline

# Examples

## Building JSON from Strings

### convert text to a string

    $ echo -n "hello, JSONWrench" | jw string
    "hello, jsonwrench"
    $ echo 'hello, "JSONWrench"' | jw string
    "hello, \"jsonwrench\"\n"

### convert lines into an array

    $ cat groceries
    apple
    banana
    chocolate cookies
    $ jw lines < groceries
    ["apple", "banana", "chocolate cookies"]

### combine lists of keys and values into an object

    $ cat keys
    title
    author
    date
    $ cat values
    Introducing JSONWrench
    jekor
    2012-03-26
    $ cat keys values | jw ziplines
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}

## Transforming and Manipulating JSON

### concatenating lists

    $ cat array1
    ["line 1", "line 2", "line 3"]
    $ cat array2
    ["line 4", "line 5"]
    $ cat array1 array2 | jw concat
    ["line 1", "line 2", "line 3", "line 4", "line 5"]
    $ cat array3
    ["line 6"]
    $ cat array1 array2 array3 | jw concat
    ["line 1", "line 2", "line 3", "line 4", "line 5", "line 6"]

### merging objects

    $ cat object1
    {"title": "Introducing JSONWrench", "author": "jekor"}
    $ cat object2
    {"date": "2012-03-26"}
    $ cat object1 object2 | jw merge
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}
    $ cat object3
    {"title": "Revised Title"}
    $ cat object1 object2 object3 | jw merge
    {"title": "Revised Title", "author": "jekor", "date": "2012-03-26"}

### inserting new keys

    $ cat object1
    {"title": "Introducing JSONWrench", "author": "jekor"}
    $ cat object1 <(date -Idate) | jw insert date
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}

### combining arrays of keys and values into an object

    $ cat keys.json
    ["title", "author", "date"]
    $ cat values.json
    ["Introducing JSONWrench", "jekor", "2012-03-26"]
    $ cat keys.json values.json | jw zip
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}

### Normalizing JSON

    $ cat ugly.json
    [{"title":
    "Introducing JSONWrench", "author":
    "jekor", "date":
    "2012-03-26"}
    
    ]
    $ jw normalize < ugly.json
    [{"author": "jekor", "date": "2012-03-26", "title": "Introducing JSONWrench"}]
    $ cat greeting
    "hi
    there"
    $ jw normalize < greeting
    "hi\nthere"

Note that a side-effect of normalization is that the entire JSON object fits on 1 line. This is useful for more complex manipulation. Another useful side-effect is that keys are sorted alphabetically. This allows for normalized JSON objects to be compared for equality using string comparison.

## Building Nested Data Structures

### building arrays from JSON

    $ cat article1
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}
    $ cat article2
    {"title": "JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}
    $ cat article1 article2 | jw array
    [{"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}, {"title": "JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}]

Note that if we had used jw lines, the inputs would have been considered strings and the result would be as follows:

    $ cat article1 article2 | jw lines
    ["{\"title\": \"Introducing JSONWrench\", \"author\": \"jekor\", \"date\": \"2012-03-26\"}", "{\"title\": JSONWrench Examples\", \"author\": \"jekor\", \"date\": \"2012-03-27\"}"]

### building objects from JSON

    $ cat articles
    [{"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}, {"title": JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}]
    $ jw name "the articles" < articles
    {"the articles": [{"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}, {"title": JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}]}

<!-- Note that "articles" must first be converted into a JSON array. JSONWrench does not attempt to infer the type of its input, because doing so could lead to ambiguities. Instead, each operation requires inputs to be of a certain type. In this case, zip expects 2 arrays. -->

### nesting arrays

    $ cat array1 array2 array3 | jw merge
    ["line 1", "line 2", "line 3", "line 4", "line 5", "line 6"]
    $ cat array1 array2 array3 | jw array
    [["line 1", "line 2", "line 3"], ["line 4", "line 5"], ["line 6"]]
    $ cat array2 array3 | jw array
    [["line 4", "line 5"], ["line 6"]]
    $ echo `cat array1` `cat array2 array3 | jw array` | jw array
    [["line 1", "line 2", "line 3"], [["line 4", "line 5"], ["line 6"]]]

## Deconstructing JSON

Many unix tools are line-oriented, so it's useful to be able to convert a JSON array into lines.

### convert an array back into lines

    $ cat groceries.json
    ["apple", "banana", "chocolate cookies"]
    $ jw unlines < groceries.json
    apple
    banana
    chocolate cookies

Note, however, that a newline character in any of the strings will result in more lines than array elements. In that case, you want to keep the array elements as JSON strings so that they each fit on a single line.

    $ cat groceries-verbose.json
    ["apple", "banana\nmake sure they're ripe!", "chocolate cookies"]
    $ jw unlines < groceries-verbose.json
    apple
    banana
    make sure they're ripe!
    chocolate cookies
    $ jw unarray < groceries-verbose.json
    "apple"
    "banana\nmake sure they're ripe!"
    "chocolate cookies"

This also works for non-string elements.

    $ cat articles.json
    [{"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}, {"title": "JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}]
    $ jw unarray < articles.json
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}
    {"title": "JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}

### extract values from objects by key

    $ cat article.json
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}
    $ jw lookup date < article.json
    "2012-03-26"
    $ jw lookup missing < article.json
    null

### remove values from objects by key

    $ cat article.json
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}
    $ jw drop date < article.json
    {"title": "Introducing JSONWrench", "author": "jekor"}
    $ jw drop date author < article.json
    {"title": "Introducing JSONWrench"}

### remove values from objects by specifying the keys you'd like to keep

    $ cat article.json
    {"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}
    $ jw take date < article.json
    {"date": "2012-03-26"}
    $ jw take date author < article.json
    {"date": "2012-03-26", "author": "jekor"}

### extract keys from each object in an array

    $ cat articles.json
    [{"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}, {"title": JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}]
    $ jw unarray < articles.json | map jw lookup date
    "2012-03-26"
    "2012-03-27"
    $ jw unarray < articles.json | map jw lookup date | jw array
    ["2012-03-26","2012-03-27"]

Note that the map command might not be available on your system. It invokes the given command for each line of stdin (passing that line into the command's stdin)

### sorting on a key

First, using paste and cut. The idea here is to pull out the key we want to sort on, paste it to the front of the line, sort, and then cut it out of the line once sorted.

    $ cat articles.json
    [{"title": "Introducing JSONWrench", "author": "jekor", "date": "2012-03-26"}, {"title": "JSONWrench Examples", "author": "jekor", "date": "2012-03-27"}, {"title": "JSONWrench Design Notes", "author": "jekor", "date": "2012-03-23"}]
    $ jw unarray < articles.json | map jw lookup date > dates
    $ jw unarray < articles.json | paste dates -
    "2012-03-26"    {"author": "jekor", "date": "2012-03-26", "title": "Introducing JSONWrench"}
    "2012-03-27"    {"author": "jekor", "date": "2012-03-27", "title": "JSONWrench Examples"}
    "2012-03-23"    {"author": "jekor", "date": "2012-03-23", "title": "JSONWrench Design Notes"}
    $ jw unarray < articles.json | paste dates - | sort | cut -f2
    {"author": "jekor", "date": "2012-03-23", "title": "JSONWrench Design Notes"}
    {"author": "jekor", "date": "2012-03-26", "title": "Introducing JSONWrench"}
    {"author": "jekor", "date": "2012-03-27", "title": "JSONWrench Examples"}
    $ jw unarray < articles.json | paste dates - | sort | cut -f2 | jw array
    [{"title": "JSONWrench Design Notes", "author": "jekor", "date": "2012-03-23"}, {"author": "jekor", "date": "2012-03-26", "title": "Introducing JSONWrench"}, {"author": "jekor", "date": "2012-03-27", "title": "JSONWrench Examples"}]
    $ rm dates

## Common Problems

### `jw: <stdin>: hGetContents: invalid argument (invalid byte sequence)`

Make sure that your locale is set correctly.
