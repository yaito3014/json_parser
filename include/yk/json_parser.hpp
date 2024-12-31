#ifndef YK_JSON_PARSER_HPP
#define YK_JSON_PARSER_HPP

#include <exception>
#include <expected>
#include <filesystem>
#include <functional>
#include <initializer_list>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>

namespace yk {

template <class T, template <class...> class TT>
struct is_specialization_of : std::false_type {};

template <template <class...> class TT, class... Ts>
struct is_specialization_of<TT<Ts...>, TT> : std::true_type {};

template <class T, template <class...> class TT>
inline constexpr bool is_specialization_of_v = is_specialization_of<T, TT>::value;

template <class T, template <class...> class TT>
concept specialization_of = is_specialization_of_v<T, TT>;

class parse_error : std::runtime_error {
  using std::runtime_error::runtime_error;
};

template <class T>
struct parse_result {
  using value_type = T;

  T value;
  std::string_view remainder;

  constexpr bool operator==(const parse_result&) const noexcept = default;
};

template <class P>
concept Parser = std::invocable<P, std::string_view>;

template <class P>
concept TryParser = std::invocable<P, std::string_view> && specialization_of<typename std::invoke_result_t<P, std::string_view>::value_type, parse_result>;

class JValue;
class JObject;
class JNumber;
class JArray;
class JString;
class JBool;
class JNull;

template <class Derived>
struct ParserMixin {
  static constexpr Derived parse(std::string_view sv) {
    if (auto expect = Derived::try_parse(sv)) {
      if (expect->remainder.empty()) {
        return expect->value;
      } else {
        throw parse_error("not fully parsed");
      }
    } else {
      throw parse_error(expect.error().data());
    }
  }

  friend constexpr bool operator==(const ParserMixin&, const ParserMixin&) = default;
};

class JObject : public ParserMixin<JObject> {
public:
  constexpr JObject() = default;
  constexpr JObject(std::initializer_list<std::tuple<JString, JValue>> init_list) : entries_(init_list) {}

  static constexpr std::expected<parse_result<JObject>, std::string_view> try_parse(std::string_view) noexcept;

  constexpr bool operator==(const JObject&) const;

private:
  std::vector<std::tuple<JString, JValue>> entries_;
};

class JNumber : public ParserMixin<JNumber> {
public:
  constexpr JNumber(double value) noexcept : value_(value) {}

  constexpr bool operator==(const JNumber&) const noexcept = default;

  static constexpr std::expected<parse_result<JNumber>, std::string_view> try_parse(std::string_view) noexcept;

private:
  double value_;
};

class JArray : public ParserMixin<JArray> {
public:
  constexpr JArray() = default;

  constexpr JArray(std::initializer_list<JValue> init_list) : values_(init_list) {}

  constexpr bool operator==(const JArray&) const noexcept;

  static constexpr std::expected<parse_result<JArray>, std::string_view> try_parse(std::string_view) noexcept;

private:
  std::vector<JValue> values_;
};

class JString : public ParserMixin<JString> {
public:
  constexpr JString(std::string_view sv) : str_(sv) {}

  constexpr bool operator==(const JString&) const noexcept = default;

  static constexpr std::expected<parse_result<JString>, std::string_view> try_parse(std::string_view) noexcept;

private:
  std::string str_;
};

class JBool : public ParserMixin<JBool> {
public:
  constexpr JBool(bool value) noexcept : value_(value) {}

  constexpr bool operator==(const JBool&) const noexcept = default;

  static constexpr std::expected<parse_result<JBool>, std::string_view> try_parse(std::string_view) noexcept;

private:
  bool value_;
};

class JNull : public ParserMixin<JNull> {
public:
  constexpr bool operator==(const JNull&) const noexcept = default;

  static constexpr std::expected<parse_result<JNull>, std::string_view> try_parse(std::string_view) noexcept;
};

class JValue : public ParserMixin<JValue> {
public:
  enum class Kind {
    Null,
    Object,
    Number,
    Array,
    String,
    Bool,
  };

  constexpr JValue() = default;

  constexpr JValue(const JObject& object) : variant_(object) {}
  constexpr JValue(JObject&& object) : variant_(std::move(object)) {}

  constexpr JValue(const JNumber& number) : variant_(number) {}
  constexpr JValue(JNumber&& number) : variant_(std::move(number)) {}

  constexpr JValue(const JArray& array) : variant_(array) {}
  constexpr JValue(JArray&& array) : variant_(std::move(array)) {}

  constexpr JValue(const JString& string) : variant_(string) {}
  constexpr JValue(JString&& string) : variant_(std::move(string)) {}

  constexpr JValue(const JBool& boolean) : variant_(boolean) {}
  constexpr JValue(JBool&& boolean) : variant_(std::move(boolean)) {}

  constexpr JValue(const JNull& null) : variant_(null) {}
  constexpr JValue(JNull&& null) : variant_(std::move(null)) {}

  constexpr Kind which() const noexcept { return static_cast<Kind>(variant_.index()); }

  constexpr bool operator==(const JValue&) const = default;

  static constexpr std::expected<parse_result<JValue>, std::string_view> try_parse(std::string_view) noexcept;

private:
  std::variant<JNull, JObject, JNumber, JArray, JString, JBool> variant_;
};

constexpr bool JArray::operator==(const JArray&) const noexcept = default;

constexpr bool JObject::operator==(const JObject& other) const {
  // FIXME
  return entries_ == other.entries_;
}

constexpr auto anyChar(std::string_view sv) noexcept {
  return [=](std::string_view sv) -> std::expected<parse_result<char>, std::string_view> {
    if (sv.empty()) return std::unexpected{"empty input"};
    return parse_result{sv.front(), sv.substr(1)};
  };
}

constexpr auto anyOf(std::string_view chars) noexcept {
  return [=](std::string_view sv) -> std::expected<parse_result<char>, std::string_view> {
    if (sv.empty()) return std::unexpected{"empty input"};
    if (!chars.contains(sv.front())) return std::unexpected{"not found matching char"};
    return parse_result{sv.front(), sv.substr(1)};
  };
}

constexpr auto noneOf(std::string_view chars) noexcept {
  return [=](std::string_view sv) -> std::expected<parse_result<char>, std::string_view> {
    if (sv.empty()) return std::unexpected{"empty input"};
    if (chars.contains(sv.front())) return std::unexpected{"found matching char"};
    return parse_result{sv.front(), sv.substr(1)};
  };
}

template <TryParser Open, TryParser Close, TryParser P>
constexpr auto between(Open open, Close close, P parser) noexcept {
  return [=](std::string_view sv) -> std::expected<typename std::invoke_result_t<P, std::string_view>::value_type, std::string_view> {
    if (auto mopen = open(sv)) {
      if (auto mvalue = parser(mopen->remainder)) {
        if (auto mclose = close(mvalue->remainder)) {
          return parse_result{mvalue->value, mclose->remainder};
        } else {
          return std::unexpected{"missing close"};
        }
      } else {
        return std::unexpected{mvalue.error()};
      }
    } else {
      return std::unexpected{"missing open"};
    }
  };
}

template <TryParser P>
constexpr auto many(P parser) {
  return [=](std::string_view sv) -> std::expected<parse_result<std::vector<typename std::invoke_result_t<P, std::string_view>::value_type::value_type>>, std::string_view> {
    std::vector<typename std::invoke_result_t<P, std::string_view>::value_type::value_type> res;
    std::string_view remaining = sv;
    while (auto m = parser(remaining)) {
      res.push_back(m->value);
      remaining = m->remainder;
    }
    return parse_result{res, remaining};
  };
}

template <TryParser P>
constexpr auto times(P parser, unsigned n) {
  return [=](std::string_view sv) -> std::expected<parse_result<std::vector<typename std::invoke_result_t<P, std::string_view>::value_type::value_type>>, std::string_view> {
    std::vector<typename std::invoke_result_t<P, std::string_view>::value_type::value_type> res;
    std::string_view remaining = sv;
    for (auto i = 0u; i < n; ++i) {
      if (auto m = parser(remaining)) {
        res.push_back(m->value);
        remaining = m->remainder;
      } else {
        return std::unexpected{"not enough elements"};
      }
    }
    return parse_result{res, remaining};
  };
}

template <TryParser P, TryParser Sep>
constexpr auto sepBy1(P parser, Sep separator) {
  return [=](std::string_view sv) -> std::expected<parse_result<std::vector<typename std::invoke_result_t<P, std::string_view>::value_type::value_type>>, std::string_view> {
    std::vector<typename std::invoke_result_t<P, std::string_view>::value_type::value_type> res;
    std::string_view remaining = sv;
    if (auto head = parser(sv)) {
      res.push_back(head->value);
      remaining = head->remainder;
      while (true) {
        if (auto sep = separator(remaining)) {
          if (auto elem = parser(sep->remainder)) {
            res.push_back(elem->value);
            remaining = elem->remainder;
            continue;
          }
        }
        break;
      }
      return parse_result{res, remaining};
    } else {
      return std::unexpected{head.error()};
    }
  };
}

template <TryParser P, class T>
constexpr auto whitespace_or(P parser, const T& default_value) noexcept {
  return [=](std::string_view sv) -> std::invoke_result_t<P, std::string_view> {
    if (auto m = parser(sv)) {
      return m;
    } else {
      if (auto mws = many(anyOf(" \t\r\n"))(sv)) return parse_result{default_value, mws->remainder};
      return std::unexpected{m.error()};
    }
  };
}

constexpr std::expected<parse_result<JObject>, std::string_view> JObject::try_parse(std::string_view sv) noexcept {
  const auto item_parser = [](std::string_view s) -> std::expected<parse_result<std::tuple<JString, JValue>>, std::string_view> {
    const auto ws = many(anyOf(" \t\r\n"));
    if (auto key = between(ws, ws, JString::try_parse)(s)) {
      if (auto delim = anyOf(":")(key->remainder)) {
        if (auto value = JValue::try_parse(delim->remainder)) {
          return parse_result{std::tuple{key->value, value->value}, value->remainder};
        }
      }
    }
    return std::unexpected{"invalid item"};
  };

  if (auto m = between(anyOf("{"), anyOf("}"), whitespace_or(sepBy1(item_parser, anyOf(",")), std::vector<std::tuple<JString, JValue>>{}))(sv)) {
    JObject res;
    res.entries_ = m->value;
    return parse_result{res, m->remainder};
  } else {
    return std::unexpected("invalid object");
  }
}

constexpr std::expected<parse_result<JArray>, std::string_view> JArray::try_parse(std::string_view sv) noexcept {
  if (auto m = between(anyOf("["), anyOf("]"), whitespace_or(sepBy1(JValue::try_parse, anyOf(",")), std::vector<JValue>{}))(sv)) {
    JArray res;
    res.values_ = m->value;
    return parse_result{res, m->remainder};
  } else {
    return std::unexpected{m.error()};
  }
}

constexpr std::expected<parse_result<JNumber>, std::string_view> JNumber::try_parse(std::string_view sv) noexcept {
  double value;
  auto [out, errc] = std::from_chars(sv.begin(), sv.end(), value);
  if (errc != std::errc{}) return std::unexpected("invalid number");
  return parse_result<JNumber>{JNumber{value}, std::string_view(out, sv.end())};
}

constexpr std::expected<parse_result<JString>, std::string_view> JString::try_parse(std::string_view sv) noexcept {
  // TODO: implement escape sequence
  const auto charParser = [](std::string_view s) -> std::expected<parse_result<std::string>, std::string_view> {
    if (auto m = noneOf("\\\"")(s)) return parse_result{std::string(1, m->value), s.substr(1)};

    using namespace std::string_literals;

    if (s.starts_with('\\')) {
      const std::string_view trimmed = s.substr(1);
      if (trimmed.starts_with('\"')) return parse_result{"\""s, trimmed.substr(1)};
      if (trimmed.starts_with('\\')) return parse_result{"\\"s, trimmed.substr(1)};
      if (trimmed.starts_with('b')) return parse_result{"\b"s, trimmed.substr(1)};
      if (trimmed.starts_with('f')) return parse_result{"\f"s, trimmed.substr(1)};
      if (trimmed.starts_with('n')) return parse_result{"\n"s, trimmed.substr(1)};
      if (trimmed.starts_with('r')) return parse_result{"\r"s, trimmed.substr(1)};
      if (trimmed.starts_with('t')) return parse_result{"\t"s, trimmed.substr(1)};
      if (trimmed.starts_with('u')) {
        if (!times(anyOf("0123456789ABCDEF"), 4)(trimmed.substr(1))) return std::unexpected{"invalid hex code"};
        const std::string_view hex = trimmed.substr(1,4);

        unsigned n;
        std::from_chars(hex.begin(), hex.end(), n, 16);
        std::u16string u16str(1, n);
        std::u8string u8str = std::filesystem::path(u16str).u8string();

        return parse_result{std::string(u8str.begin(), u8str.end()), s.substr(6)};
      }
      return std::unexpected{"unknown escape sequence"};
    }

    return std::unexpected{"invalid string"};
  };

  if (auto m = between(anyOf("\""), anyOf("\""), many(charParser))(sv)) {
    const auto view = m->value | std::views::join;
    std::string res = std::string{view.begin(), view.end()};
    return parse_result{JString{res}, m->remainder};
  } else {
    return std::unexpected{m.error()};
  }
}

constexpr std::expected<parse_result<JBool>, std::string_view> JBool::try_parse(std::string_view sv) noexcept {
  if (sv.starts_with("true")) return parse_result{JBool{true}, sv.substr(4)};
  if (sv.starts_with("false")) return parse_result{JBool{false}, sv.substr(5)};
  return std::unexpected{"invalid boolean"};
}

constexpr std::expected<parse_result<JNull>, std::string_view> JNull::try_parse(std::string_view sv) noexcept {
  if (!sv.starts_with("null")) return std::unexpected{"invalid null"};
  return parse_result{JNull{}, sv.substr(4)};
}

constexpr std::expected<parse_result<JValue>, std::string_view> JValue::try_parse(std::string_view sv) noexcept {
  const auto ws = many(anyOf(" \t\r\n"));

  if (auto m = between(ws, ws, JObject::try_parse)(sv)) return parse_result{JValue{m->value}, m->remainder};
  if (auto m = between(ws, ws, JNumber::try_parse)(sv)) return parse_result{JValue{m->value}, m->remainder};
  if (auto m = between(ws, ws, JArray::try_parse)(sv)) return parse_result{JValue{m->value}, m->remainder};
  if (auto m = between(ws, ws, JString::try_parse)(sv)) return parse_result{JValue{m->value}, m->remainder};
  if (auto m = between(ws, ws, JBool::try_parse)(sv)) return parse_result{JValue{m->value}, m->remainder};
  if (auto m = between(ws, ws, JNull::try_parse)(sv)) return parse_result{JValue{m->value}, m->remainder};
  return std::unexpected{"invalid value"};
}

}  // namespace yk

#endif  // YK_JSON_PARSER_HPP
