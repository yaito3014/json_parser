#ifndef YK_JSON_PARSER_HPP
#define YK_JSON_PARSER_HPP

#include <exception>
#include <expected>
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

class JObject {
public:
  constexpr JObject() = default;
  constexpr JObject(std::initializer_list<std::tuple<JString, JValue>> init_list) : entries_(init_list) {}

  static constexpr JObject parse(std::string_view);
  static constexpr std::expected<parse_result<JObject>, std::string_view> try_parse(std::string_view) noexcept;

  constexpr bool operator==(const JObject&) const;

private:
  std::vector<std::tuple<JString, JValue>> entries_;
};

class JNumber {
public:
  constexpr JNumber(double value) noexcept : value_(value) {}

  constexpr bool operator==(const JNumber&) const noexcept = default;

  static constexpr JNumber parse(std::string_view);
  static constexpr std::expected<parse_result<JNumber>, std::string_view> try_parse(std::string_view) noexcept;

private:
  double value_;
};

class JArray {
public:
  constexpr JArray() = default;

  constexpr JArray(std::initializer_list<JValue> init_list) : values_(init_list) {}

  constexpr bool operator==(const JArray&) const noexcept;

  static constexpr JArray parse(std::string_view);
  static constexpr std::expected<parse_result<JArray>, std::string_view> try_parse(std::string_view) noexcept;

private:
  std::vector<JValue> values_;
};

class JString {
public:
  constexpr JString(std::string_view sv) : str_(sv) {}

  constexpr bool operator==(const JString&) const noexcept = default;

  static constexpr JString parse(std::string_view);
  static constexpr std::expected<parse_result<JString>, std::string_view> try_parse(std::string_view) noexcept;

private:
  std::string str_;
};

class JBool {
public:
  constexpr JBool(bool value) noexcept : value_(value) {}

  constexpr bool operator==(const JBool&) const noexcept = default;

  static constexpr JBool parse(std::string_view);
  static constexpr std::expected<parse_result<JBool>, std::string_view> try_parse(std::string_view) noexcept;

private:
  bool value_;
};

class JNull {
public:
  constexpr bool operator==(const JNull&) const noexcept = default;

  static constexpr JNull parse(std::string_view);
  static constexpr std::expected<parse_result<JNull>, std::string_view> try_parse(std::string_view) noexcept;
};

class JValue {
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

  static constexpr JValue parse(std::string_view sv);
  static constexpr std::expected<parse_result<JValue>, std::string_view> try_parse(std::string_view) noexcept;

private:
  std::variant<JNull, JObject, JNumber, JArray, JString, JBool> variant_;
};

constexpr bool JArray::operator==(const JArray&) const noexcept = default;

constexpr bool JObject::operator==(const JObject& other) const {
  // FIXME
  return entries_ == other.entries_;
}

constexpr std::string_view trim_start(std::string_view sv) noexcept {
  std::size_t pos = sv.find_first_not_of(" \n\t");
  if (pos == sv.npos) return sv.substr(sv.size(), 0);
  return sv.substr(pos);
}

constexpr std::string_view trim_end(std::string_view sv) noexcept {
  std::size_t pos = sv.find_last_not_of(" \n\t");
  if (pos == sv.npos) return sv.substr(0, 0);
  return sv.substr(0, pos + 1);
}

constexpr std::string_view trim(std::string_view sv) noexcept { return trim_start(trim_end(sv)); }

constexpr auto anyOf(std::string_view chars) noexcept {
  return [=](std::string_view sv) -> std::expected<parse_result<char>, std::string_view> {
    if (sv.empty()) return std::unexpected{"empty input"};
    if (!chars.contains(sv.front())) return std::unexpected{"not found matching char"};
    return parse_result{sv.front(), sv.substr(1)};
  };
}

template <TryParser P1, TryParser P2>
constexpr auto sequence(P1 p1, P2 p2) noexcept {
  return [=](std::string_view sv) -> std::expected<parse_result<std::tuple<typename std::invoke_result_t<P1, std::string_view>::value_type::value_type,
                                                                           typename std::invoke_result_t<P2, std::string_view>::value_type::value_type>>,
                                                   std::string_view> {
    if (auto m1 = p1(sv)) {
      if (auto m2 = p2(m1->remainder)) {
        return parse_result{std::tuple{m1->value, m2->value}, m2->remainder};
      } else {
        return std::unexpected{m2.error()};
      }
    } else {
      return std::unexpected{m1.error()};
    }
  };
}

constexpr JObject JObject::parse(std::string_view sv) {
  if (sv.size() < 2 || sv.front() != '{' || sv.back() != '}') throw parse_error("object is not surrounded by curly brackets");

  JObject object;
  // FIXME
  for (auto&& item : trim(sv.substr(1, sv.size() - 2)) | std::views::split(',')) {
    std::string_view key_value_pair{item};
    std::size_t pos = key_value_pair.find_first_of(':');
    std::string_view key = trim(key_value_pair.substr(0, pos));
    std::string_view value = trim(key_value_pair.substr(pos + 1));
    object.entries_.emplace_back(JString::parse(key), JValue::parse(value));
  }

  return object;
}

constexpr std::expected<parse_result<JObject>, std::string_view> JObject::try_parse(std::string_view) noexcept { return std::unexpected("not implemented"); }

constexpr JArray JArray::parse(std::string_view sv) {
  if (sv.size() < 2 || sv.front() != '[' || sv.back() != ']') throw parse_error("array is not surrounded by square brackets");

  JArray array;
  // FIXME
  for (auto&& item : trim(sv.substr(1, sv.size() - 2)) | std::views::split(',')) {
    array.values_.emplace_back(yk::JValue::parse(trim(std::string_view(item))));
  }

  return array;
}

constexpr std::expected<parse_result<JArray>, std::string_view> JArray::try_parse(std::string_view) noexcept { return std::unexpected("not implemented"); }

constexpr JNumber JNumber::parse(std::string_view sv) {
  if (auto expect = try_parse(sv))
    return expect->value;
  else
    throw parse_error(expect.error().data());
}

constexpr std::expected<parse_result<JNumber>, std::string_view> JNumber::try_parse(std::string_view sv) noexcept {
  double value;
  auto [out, errc] = std::from_chars(sv.begin(), sv.end(), value);
  if (errc != std::errc{} || out != sv.end()) return std::unexpected("invalid number");
  return parse_result<JNumber>{JNumber{value}, std::string(out, sv.end())};
}

constexpr JString JString::parse(std::string_view sv) {
  if (sv.size() < 2 || sv.front() != '"' || sv.back() != '"') throw parse_error("not quoted string");
  return JString{sv.substr(1, sv.size() - 2)};  // TODO: implement escape sequence
}

constexpr std::expected<parse_result<JString>, std::string_view> JString::try_parse(std::string_view) noexcept { return std::unexpected("not implemented"); }

constexpr JBool JBool::parse(std::string_view sv) {
  if (sv == "true") return JBool{true};
  if (sv == "false") return JBool{false};
  throw parse_error("invalid boolean");
}

constexpr std::expected<parse_result<JBool>, std::string_view> JBool::try_parse(std::string_view) noexcept { return std::unexpected("not implemented"); }

constexpr JNull JNull::parse(std::string_view sv) {
  if (sv != "null") throw parse_error("invalid null");
  return JNull{};
}

constexpr std::expected<parse_result<JNull>, std::string_view> JNull::try_parse(std::string_view) noexcept { return std::unexpected("not implemented"); }

constexpr JValue JValue::parse(std::string_view sv) {
  if (auto expect = try_parse(sv))
    return expect->value;
  else
    throw parse_error(expect.error().data());
}

constexpr std::expected<parse_result<JValue>, std::string_view> JValue::try_parse(std::string_view) noexcept { return std::unexpected("not implemented"); }

}  // namespace yk

#endif  // YK_JSON_PARSER_HPP
