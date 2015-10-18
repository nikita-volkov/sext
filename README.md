#SExt
A small library that adds the missing useful functions to the standard Scala library. These include

* `unfold`, `unfold1`, `foldTo`, `foldFrom`
* `mapKeys`
* `zipBy`, `unzip4`
* `treeString`,`valueTreeString`
* `prependLines`, `splitBy`

... and some others you'll have to check in the [source `src/main/scala/sext/package.scala`]

#Using
Add the following Maven dependency

    <dependency>
      <groupId>com.github.nikita-volkov</groupId>
      <artifactId>sext</artifactId>
      <version>0.2.3</version>
    </dependency>

or SBT dependency
    
    libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.4"

(or the appropriate Gradle one)

Add the following import statement to your files:

    import sext._
