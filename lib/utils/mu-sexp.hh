/*
** Copyright (C) 2020-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#ifndef MU_SEXP_HH__
#define MU_SEXP_HH__

#include "mu-utils.hh"

#include <stdexcept>
#include <vector>
#include <string>
#include <string_view>
#include <iostream>
#include <variant>
#include <cinttypes>
#include <ostream>
#include <cassert>

#include <utils/mu-result.hh>
#include <utils/mu-option.hh>

namespace Mu {

/**
 * A structure somewhat similar to a Lisp s-expression and which can be
 * constructed from/to an s-expressing string representation.
 *
 * A sexp is either an atom (String, Number, Symbol) or a List.
 */
struct Sexp {
	/**
	 * Types
	 *
	 */
	using	List   = std::vector<Sexp>;
	using	String = std::string;
	using   Number = int64_t;
	struct Symbol { // distinguish from String.
		Symbol(const std::string& s): name{s} {}
		Symbol(std::string&& s): name(std::move(s)) {}
		Symbol(const char* str): Symbol(std::string{str}) {}
		Symbol(std::string_view sv): Symbol(std::string{sv}) {}
		operator const std::string&() const {return name; }
		std::string name;

		bool operator==(const Symbol& rhs) const {
			return this == &rhs ? true : rhs.name == name;
		}
		bool operator!=(const Symbol& rhs) const { return *this == rhs ? false : true; }
	};
	enum struct Type { List, String, Number, Symbol };
	using ValueType = std::variant<List, String, Number, Symbol>;

	/**
	 * Is some Sexp of the given type?
	 *
	 * @return true or false
	 */
	constexpr bool stringp() const { return std::holds_alternative<String>(value); }
	constexpr bool numberp() const { return std::holds_alternative<Number>(value); }
	constexpr bool listp()   const { return std::holds_alternative<List>(value); }
	constexpr bool symbolp() const { return std::holds_alternative<Symbol>(value); }
	constexpr bool symbolp(const Sexp::Symbol& sym) const {return symbolp() && symbol() == sym; }
	constexpr bool nilp() const { return symbolp(nil_sym); }

	// Get the specific variant type.
	const List& list() const     { return std::get<List>(value); }
	List& list()                 { return std::get<List>(value); }
	const String& string() const { return std::get<String>(value); }
	String& string()             { return std::get<String>(value); }
	const Number& number() const { return std::get<Number>(value); }
	Number& number()             { return std::get<Number>(value); }
	const Symbol& symbol() const { return std::get<Symbol>(value); }
	Symbol& symbol()             { return std::get<Symbol>(value); }

	/**
	 * Constructors
	 */
	Sexp():value{List{}} {} // default: an empty list.
	// Copy & move ctors
	Sexp(const Sexp& other):value{other.value}{}
	Sexp(Sexp&& other):value{std::move(other.value)}{}
	// From various types
	Sexp(const List& lst): value{lst} {}
	Sexp(List&& lst): value{std::move(lst)} {}
	Sexp(const String& str): value{str} {}
	Sexp(String&& str): value{std::move(str)} {}
	Sexp(const char *str): Sexp{std::string{str}} {}
	Sexp(std::string_view sv): Sexp{std::string{sv}} {}

	template<typename N, typename = std::enable_if_t<std::is_integral_v<N>> >
	Sexp(N n):value{static_cast<Number>(n)} {}

	Sexp(const Symbol& sym): value{sym} {}
	Sexp(Symbol&& sym): value{std::move(sym)} {}

	template<typename S, typename T, typename... Args>
	Sexp(S&& s, T&& t, Args&&... args): value{List()} {
		auto& l{std::get<List>(value)};
		l.emplace_back(Sexp(std::forward<S>(s)));
		l.emplace_back(Sexp(std::forward<T>(t)));
		(l.emplace_back(Sexp(std::forward<Args>(args))), ...);
	}

	/**
	 * Copy-assignment
	 *
	 * @param rhs another sexp
	 *
	 * @return the sexp
	 */
	Sexp& operator=(const Sexp& rhs) {
		if (this != &rhs)
			value = rhs.value;
		return *this;
	}

	/**
	 * Move-assignment
	 *
	 * @param rhs another sexp
	 *
	 * @return the sexp
	 */
	Sexp& operator=(Sexp&& rhs) {
		if (this != &rhs)
			value = std::move(rhs.value);
		return *this;
	}

	/**
	 * Get the type of value
	 *
	 * @return type
	 */
	constexpr Type type() const { return static_cast<Type>(value.index()); }
	/**
	 * Get the name for some type
	 *
	 * @param t type
	 *
	 * @return name
	 */
	static constexpr std::string_view type_name(Type t) {
		switch(t) {
		case Type::String:
			return "string";
		case Type::Number:
			return "number";
		case Type::Symbol:
			return "symbol";
		case Type::List:
			return "list";
		default:
			return "<error>";
		}
	}

	/**
	 * Parse sexp from string
	 *
	 * @param str a string
	 *
	 * @return either an Sexp or an error
	 */
	static Result<Sexp> parse(const std::string& str);


	/**
	 * List specific functionality
	 *
	 */
	using	iterator       = List::iterator;
	using	const_iterator = List::const_iterator;

	iterator begin()        { return list().begin(); }
	const_iterator begin()  const { return list().begin(); }
	const_iterator cbegin() const { return list().cbegin(); }

	iterator       end()         { return list().end(); }
	const_iterator end()   const { return list().end(); }
	const_iterator cend()  const { return list().cend(); }

	bool empty() const { return list().empty(); }
	size_t size() const { return list().size(); }
	void clear() { list().clear(); }

	/// Adding to lists
	Sexp& add(const Sexp& s) { list().emplace_back(s); return *this; }
	Sexp& add(Sexp&& s)      { list().emplace_back(std::move(s)); return *this; }
	Sexp& add()              { return *this; }

	template <typename V1, typename V2, typename... Args>
	Sexp& add(V1&& v1, V2&& v2, Args... args) {
		return add(std::forward<V1>(v1))
			.add(std::forward<V2>(v2))
			.add(std::forward<Args>(args)...);
	}

	/// Adding list elements
	Sexp& add_list(Sexp&& l) { for (auto&& e: l) add(std::move(e)); return *this;};

	/// Some convenience for the query parser
	Sexp& front() { return list().front(); }
	const Sexp& front() const { return list().front(); }
	void pop_front() { list().erase(list().begin()); }

	Option<Sexp&> head() { if (listp()&&!empty()) return front(); else return Nothing; }
	Option<const Sexp&> head() const { if (listp()&&!empty()) return front(); else return Nothing; }

	bool head_symbolp() const {
		if (auto&& h{head()}; h) return h->symbolp(); else return false;
	}
	bool head_symbolp(const Symbol& sym) const {
		if (head_symbolp()) return head()->symbolp(sym); else return false;
	}

	/**
	 * Property lists (aka plists)
	 */

	bool plistp() const { return listp() && plistp(cbegin(), cend()); }
	Sexp& put_props() { return *this; } // Final case for template pack.
	template <class PropType, class SexpType, typename... Args>
	Sexp& put_props(PropType&& prop, SexpType&& sexp, Args... args) {
		auto&& propname{std::string(prop)};
		return del_prop(propname)
			.add(Symbol(std::move(propname)),
			     std::forward<SexpType>(sexp))
			.put_props(std::forward<Args>(args)...);
	}

	/**
	 * Find the property value for some property by name
	 *
	 * @param p property name
	 *
	 * @return the property if found, or nothing
	 */
	const Option<const Sexp&> get_prop(const std::string& p) const {
		if (auto&& it = find_prop(p, cbegin(), cend()); it != cend())
			return *(std::next(it));
		else
			return Nothing;
	}
	/// Output to string
	enum struct Format {
		Default	  = 0,	/**< Nothing in particular */
		SplitList = 1 << 0,	/**< Insert newline after list item */
		TypeInfo  = 1 << 1, /**< Show type-info */
	};

	/**
	 * Get a string representation of the sexp
	 *
	 * @return str
	 */
	std::string to_string(Format fopts=Format::Default) const;
	std::string to_json_string(Format fopts=Format::Default) const;

	Sexp& del_prop(const std::string& pname);

	/**
	 * Some useful constants
	 *
	 */
	static inline const auto nil_sym	= Sexp::Symbol{"nil"};
	static inline const auto t_sym	= Sexp::Symbol{"t"};

protected:
	const_iterator find_prop(const std::string& s, const_iterator b,
				       const_iterator e)  const;
	bool plistp(const_iterator b, const_iterator e) const;
private:
	iterator find_prop(const std::string& s,iterator b,
				 iterator e);
	ValueType value;


};

MU_ENABLE_BITOPS(Sexp::Format);

/**
 * String-literal; allow for ":foo"_sym to be a symbol
 */
static inline Sexp::Symbol
operator"" _sym(const char* str, std::size_t n)
{
	return Sexp::Symbol{str};
}

static inline std::ostream&
operator<<(std::ostream& os, const Sexp::Type& stype)
{
	os << Sexp::type_name(stype);
	return os;
}


static inline std::ostream&
operator<<(std::ostream& os, const Sexp& sexp)
{
	os << sexp.to_string();
	return os;
}

} // namespace Mu

#endif /* MU_SEXP_HH__ */
