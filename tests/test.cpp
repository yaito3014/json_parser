#define BOOST_TEST_MODULE yk_json_parser_test_module
#include <boost/test/unit_test.hpp>

#include <yk/json_parser.hpp>

BOOST_AUTO_TEST_SUITE(yk_json_parser)

BOOST_AUTO_TEST_CASE(AnyOf) {
  BOOST_TEST(bool(yk::anyOf("abc")("a")));
  BOOST_TEST(bool(yk::anyOf("abc")("b")));
  BOOST_TEST(bool(yk::anyOf("abc")("c")));

  BOOST_TEST(!bool(yk::anyOf("abc")("d")));
}

BOOST_AUTO_TEST_CASE(NoneOf) {
  BOOST_TEST(!bool(yk::noneOf("ab")("a")));
  BOOST_TEST(!bool(yk::noneOf("ab")("b")));
  BOOST_TEST(bool(yk::noneOf("ab")("c")));
}

BOOST_AUTO_TEST_CASE(Between) {
  const auto parser = yk::between(yk::anyOf("{"), yk::anyOf("}"), yk::anyOf("ab"));

  BOOST_TEST((parser("{a}").value() == yk::parse_result{'a', ""}));
  BOOST_TEST((parser("{b}").value() == yk::parse_result{'b', ""}));
  BOOST_TEST(!bool(parser("{c}")));

  BOOST_TEST(!bool(parser("a}")));
  BOOST_TEST(!bool(parser("{a")));
  BOOST_TEST(!bool(parser("a")));
}

BOOST_AUTO_TEST_CASE(Times) {
  const auto parser = yk::times(yk::anyOf("abc"), 3);

  BOOST_TEST((parser("abc").value() == yk::parse_result{std::vector{'a', 'b', 'c'}, ""}));

  BOOST_TEST(!bool(parser("ab")));
}

BOOST_AUTO_TEST_CASE(SepBy1) {
  const auto parser = yk::sepBy1(yk::anyOf("ab"), yk::anyOf(","));

  BOOST_TEST((parser("a").value() == yk::parse_result{std::vector{'a'}}, ""));
  BOOST_TEST((parser("a,b").value() == yk::parse_result{std::vector{'a', 'b'}}, ""));

  BOOST_TEST(!bool(parser("")));
  BOOST_TEST(!bool(parser("c")));
}

BOOST_AUTO_TEST_CASE(WhitespaceOr) {
  const auto parser = yk::whitespace_or(yk::anyOf("ab"), 'x');

  BOOST_TEST((parser("a").value() == yk::parse_result{'a', ""}));
  BOOST_TEST((parser("b").value() == yk::parse_result{'b', ""}));
  BOOST_TEST((parser(" ").value() == yk::parse_result{'x', ""}));
  BOOST_TEST((parser("  ").value() == yk::parse_result{'x', ""}));
}

BOOST_AUTO_TEST_CASE(Combinator) {
  const auto ab = yk::anyOf("ab");
  const auto parser = yk::between(yk::anyOf("["), yk::anyOf("]"), yk::whitespace_or(yk::sepBy1(ab, yk::anyOf(",")), std::vector<char>{}));
  BOOST_TEST((parser("[]").value() == yk::parse_result{std::vector<char>{}, ""}));
  BOOST_TEST((parser("[ ]").value() == yk::parse_result{std::vector<char>{}, ""}));
  BOOST_TEST((parser("[a]").value() == yk::parse_result{std::vector<char>{'a'}, ""}));
  BOOST_TEST((parser("[a,b]").value() == yk::parse_result{std::vector<char>{'a', 'b'}, ""}));
}

BOOST_AUTO_TEST_CASE(Number) {
  BOOST_TEST((yk::JNumber::parse("42") == yk::JNumber{42}));
  BOOST_TEST((yk::JNumber::parse("3.14") == yk::JNumber{3.14}));
  BOOST_TEST((yk::JNumber::parse("-1.234e5") == yk::JNumber{-1.234e5}));

  BOOST_REQUIRE_THROW(yk::JNumber::parse("foobar"), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JNumber::parse("3.14foo"), yk::parse_error);

  BOOST_REQUIRE_THROW(yk::JNumber::parse("foo3.14"), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JNumber::parse("foo3.14bar"), yk::parse_error);

  BOOST_TEST((yk::JNumber::try_parse("3.14foo").value() == yk::parse_result{yk::JNumber{3.14}, "foo"}));
}

BOOST_AUTO_TEST_CASE(String) {
  BOOST_TEST((yk::JString::parse("\"\"") == yk::JString{""}));
  BOOST_TEST((yk::JString::parse("\"foobar\"") == yk::JString{"foobar"}));

  BOOST_TEST((yk::JString::parse(R"("\r\n\t\b\f")") == yk::JString{"\r\n\t\b\f"}));
  BOOST_TEST((yk::JString::parse(R"("\\\"")") == yk::JString{"\\\""}));

  BOOST_TEST((yk::JString::parse(R"("\u0020")") == yk::JString{"\u0020"}));

  BOOST_REQUIRE_THROW(yk::JNumber::parse(""), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JNumber::parse("\""), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JNumber::parse("foobar"), yk::parse_error);
}

BOOST_AUTO_TEST_CASE(Bool) {
  BOOST_TEST((yk::JBool::parse("true") == yk::JBool{true}));
  BOOST_TEST((yk::JBool::parse("false") == yk::JBool{false}));

  BOOST_REQUIRE_THROW(yk::JBool::parse("ftarlusee"), yk::parse_error);
}

BOOST_AUTO_TEST_CASE(Null) {
  BOOST_TEST((yk::JNull::parse("null") == yk::JNull{}));

  BOOST_REQUIRE_THROW(yk::JNull::parse("nil"), yk::parse_error);
}

BOOST_AUTO_TEST_CASE(Array) {
  BOOST_TEST((yk::JArray::parse("[]") == yk::JArray{}));
  BOOST_TEST((yk::JArray::parse("[  ]") == yk::JArray{}));
  BOOST_TEST((yk::JArray::parse("[42]") == yk::JArray{yk::JNumber{42}}));
  BOOST_TEST((yk::JArray::parse("[33,4]") == yk::JArray{yk::JNumber{33}, yk::JNumber{4}}));

  BOOST_TEST((yk::JArray::parse("[  42  ]") == yk::JArray{yk::JNumber{42}}));
  BOOST_TEST((yk::JArray::parse("[  33  ,  4  ]") == yk::JArray{yk::JNumber{33}, yk::JNumber{4}}));

  BOOST_TEST((yk::JArray::parse("[ \"foo\", \"bar\" ]") == yk::JArray{yk::JString{"foo"}, yk::JString{"bar"}}));

  BOOST_TEST((yk::JArray::parse("[\",\",\",\"]") == yk::JArray{yk::JString{","}, yk::JString{","}}));

  BOOST_TEST((yk::JArray::parse("[[33],[4]]") == yk::JArray{yk::JArray{yk::JNumber{33}}, yk::JArray{yk::JNumber{4}}}));

  BOOST_REQUIRE_THROW(yk::JArray::parse("["), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JArray::parse("]"), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JArray::parse("[,]"), yk::parse_error);
  BOOST_REQUIRE_THROW(yk::JArray::parse("[42,]"), yk::parse_error);
}

BOOST_AUTO_TEST_CASE(Object) {
  BOOST_TEST((yk::JObject::parse("{}") == yk::JObject{}));
  BOOST_TEST((yk::JObject::parse("{  }") == yk::JObject{}));
  BOOST_TEST((yk::JObject::parse("{\"foo\":true}") == yk::JObject{{yk::JString{"foo"}, yk::JBool{true}}}));
  BOOST_TEST((yk::JObject::parse("{\"foo\":false}") == yk::JObject{{yk::JString{"foo"}, yk::JBool{false}}}));
}

BOOST_AUTO_TEST_CASE(Value) {
  BOOST_TEST((yk::JValue::parse("null") == yk::JNull{}));

  BOOST_TEST((yk::JValue::parse("true") == yk::JBool{true}));
  BOOST_TEST((yk::JValue::parse("false") == yk::JBool{false}));

  BOOST_TEST((yk::JValue::parse("\"\"") == yk::JString{""}));
  BOOST_TEST((yk::JValue::parse("\"foobar\"") == yk::JString{"foobar"}));

  BOOST_TEST((yk::JValue::parse("42") == yk::JNumber{42}));
  BOOST_TEST((yk::JValue::parse("3.14") == yk::JNumber{3.14}));

  BOOST_TEST((yk::JValue::parse(R"(
    {
      "str": "foobar",
      "number": 3.14,
      "boolean": [true, false],
      "null": null
    }
  )") == yk::JObject{
             {yk::JString{"str"}, yk::JString{"foobar"}},
             {yk::JString{"number"}, yk::JNumber{3.14}},
             {yk::JString{"boolean"}, yk::JArray{yk::JBool{true}, yk::JBool{false}}},
             {yk::JString{"null"}, yk::JNull{}},
         }));

  BOOST_TEST((yk::JValue::parse(R"( { "colon:here":"comma,here" } )") == yk::JObject{{yk::JString{"colon:here"}, yk::JString{"comma,here"}}}));
}

BOOST_AUTO_TEST_SUITE_END()  // yk_json_parser
