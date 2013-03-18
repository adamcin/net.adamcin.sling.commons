package net.adamcin.sling.commons.uri

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnitRunner])
class RFC6570Test extends FunSuite {
  final val LOGGER = LoggerFactory.getLogger(classOf[RFC6570Test])

  import RFC6570._
  test ("Level 1 examples") {
    def expectEx(input: String, reason: String, index: Int) {
      assert(RFC6570.construct(input) === Left(new URITemplateException(input, reason, index)))
    }

    expectEx("{}", "No variables specified for expression", 1)
    expectEx("a{}", "No variables specified for expression", 2)
    expectEx("ab{}", "No variables specified for expression", 3)
    expectEx("abc{}", "No variables specified for expression", 4)
    expectEx("abcd{}", "No variables specified for expression", 5)

    assert(RFC6570.construct("abcd{var}") ===
      Right(List(Literal("abcd"), Simple(List(Single("var"))))))
    assert(RFC6570.construct("abcd{+var}") ===
      Right(List(Literal("abcd"), Reserved(List(Single("var"))))))
    assert(RFC6570.construct("abcd{#var}") ===
      Right(List(Literal("abcd"), Fragment(List(Single("var"))))))
    assert(RFC6570.construct("abcd{.var}") ===
      Right(List(Literal("abcd"), Label(List(Single("var"))))))
    assert(RFC6570.construct("abcd{/var}") ===
      Right(List(Literal("abcd"), Slash(List(Single("var"))))))
    assert(RFC6570.construct("abcd{;var}") ===
      Right(List(Literal("abcd"), Semicolon(List(Single("var"))))))
    assert(RFC6570.construct("abcd{?var}") ===
      Right(List(Literal("abcd"), Query(List(Single("var"))))))
    assert(RFC6570.construct("abcd{&var}") ===
      Right(List(Literal("abcd"), Ampersand(List(Single("var"))))))

    assert(RFC6570.construct("abcd{vars*}") ===
      Right(List(Literal("abcd"), Simple(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{+vars*}") ===
      Right(List(Literal("abcd"), Reserved(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{#vars*}") ===
      Right(List(Literal("abcd"), Fragment(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{.vars*}") ===
      Right(List(Literal("abcd"), Label(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{/vars*}") ===
      Right(List(Literal("abcd"), Slash(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{;vars*}") ===
      Right(List(Literal("abcd"), Semicolon(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{?vars*}") ===
      Right(List(Literal("abcd"), Query(List(Explode("vars"))))))
    assert(RFC6570.construct("abcd{&vars*}") ===
      Right(List(Literal("abcd"), Ampersand(List(Explode("vars"))))))
  }


}
