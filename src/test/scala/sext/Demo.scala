package sext

object Demo extends App {

  import sext.Sext._

  case class A(a: Int, b: String, c: Seq[A])
  val a = A(1, "adsf", A(5, "oiweur", Nil) :: A(4, "", A(6, "dsf", Nil) :: Nil) :: A(9, "sdlfkjw", Nil) :: Nil)
  a.treeStringValue.trace()

  Map(
    "repo/" -> Map(
      "org/" -> Map(
        "eclipse/" -> Map(
          "e4/" -> Map(
            "xwt/" -> Seq(
              "0.9.1-SNAPSHOT/" -> Seq(
                "maven-metadata-local.xml",
                "maven-metadata-local.xml.md5",
                "maven-metadata-local.xml.sha1",
                "xwt-0.9.1-SNAPSHOT-sources.jar",
                "xwt-0.9.1-SNAPSHOT-sources.jar.md5",
                "xwt-0.9.1-SNAPSHOT-sources.jar.sha1",
                "xwt-0.9.1-SNAPSHOT.jar",
                "xwt-0.9.1-SNAPSHOT.jar.md5",
                "xwt-0.9.1-SNAPSHOT.jar.sha1",
                "xwt-0.9.1-SNAPSHOT.pom",
                "xwt-0.9.1-SNAPSHOT.pom.md5",
                "xwt-0.9.1-SNAPSHOT.pom.sha1"
              ),
              "maven-metadata-local.xml",
              "maven-metadata-local.xml.md5",
              "maven-metadata-local.xml.sha1"
            )
          )
        )
      )
    )
  ).treeStringValue.trace()

}
