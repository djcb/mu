/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <string>
#include <vector>
#include <type_traits>

#include "utils/mu-utils.hh"
#include "utils/mu-error.hh"

namespace Mu {
/// Simple s-expression parser & list that parses lists () and atoms (strings
/// ("-quoted), (positive) integers ([0..9]+) and symbol starting with alpha or
/// ':', then alphanum and '-')
///
/// (:foo (1234 "bar" nil) :quux (a b c))

/// Parse node
struct Sexp {
	/// Node type
	enum struct Type { Empty, List, String, Number, Symbol, Raw };

	/**
	 * Default CTOR
	 */
	Sexp() : type_{Type::Empty} {}

	// Underlying data type for list; we'd like to use std::dequeu here,
	// but that does not compile with libc++ (it does with libstdc++)
	using Seq = std::vector<Sexp>;

	/**
	 * Make a sexp out of an s-expression string.
	 *
	 * @param expr a string containing an s-expression
	 *
	 * @return the parsed s-expression, or throw Error.
	 */
	static Sexp make_parse(const std::string& expr);

	/**
	 * Make a node for a string/integer/symbol/list value
	 *
	 * @param val some value
	 * @param empty_is_nil turn empty string into a 'nil' symbol
	 *
	 * @return a node
	 */
	static Sexp make_string(std::string&& val, bool empty_is_nil=false)
	{
		if (empty_is_nil && val.empty())
			return make_symbol("nil");
		else
			return Sexp{Type::String, std::move(val)};
	}
	static Sexp make_string(const std::string& val, bool empty_is_nil=false)
	{
		if (empty_is_nil && val.empty())
			return make_symbol("nil");
		else
			return Sexp{Type::String, std::string(val)};
	}

	static Sexp make_number(int val) { return Sexp{Type::Number, format("%d", val)}; }
	static Sexp make_symbol(std::string&& val) {
		if (val.empty())
			throw Error(Error::Code::InvalidArgument,
				    "symbol must be non-empty");
		return Sexp{Type::Symbol, std::move(val)};
	}
	static Sexp make_symbol_sv(std::string_view val) {
		return make_symbol(std::string{val});
	}

	/**
	 * Add a raw string sexp.
	 *
	 * @param val value
	 *
	 * @return A sexp
	 */
	static Sexp make_raw(std::string&& val) {
		return Sexp{Type::Raw, std::string{val}};
	}
	static Sexp make_raw(const std::string& val) {
		return make_raw(std::string{val});
	}


	/**
	 *
	 *
	 * The value of this node; invalid for list nodes.
	 *
	 * @return
	 */
	const std::string& value() const {
		if (is_list())
			throw Error(Error::Code::InvalidArgument, "no value for list");
		if (is_empty())
			throw Error{Error::Code::InvalidArgument, "no value for empty"};
		return value_;
	}

	/**
	 * The underlying container of this list node; only valid for lists
	 *
	 * @return
	 */
	const Seq& list() const {
		if (!is_list())
			throw Error(Error::Code::InvalidArgument, "not a list");
		return seq_;
	}

	/**
	 * Convert a Sexp to its S-expression string representation
	 *
	 * @return the string representation
	 */
	std::string to_sexp_string() const;

	/**
	 * Convert a Sexp::Node to its JSON string representation
	 *
	 * @return the string representation
	 */
	std::string to_json_string() const;

	/**
	 * Return the type of this Node.
	 *
	 * @return the type
	 */
	Type type() const { return type_; }

	///
	/// Helper struct to build mutable lists.
	///
	struct List {
		List () = default;
		List (const Seq& seq): seq_{seq} {}

		/**
		 * Add a sexp to the list
		 *
		 * @param sexp a sexp
		 * @param args rest arguments
		 *
		 * @return a ref to this List (for chaining)
		 */
		List& add() { return *this; }
		List& add(Sexp&& sexp)
		{
			seq_.emplace_back(std::move(sexp));
			return *this;
		}
		template <typename... Args> List& add(Sexp&& sexp, Args... args)
		{
			seq_.emplace_back(std::move(sexp));
			seq_.emplace_back(std::forward<Args>(args)...);
			return *this;
		}

		/**
		 * Add a property (i.e., :key sexp ) to the list. Remove any
		 * prop with the same name
		 *
		 * @param name a property-name. Must start with ':', length > 1
		 * @param sexp a sexp
		 * @param args rest arguments
		 *
		 * @return a ref to this List (for chaining)
		 */
		List& add_prop(std::string&& name, Sexp&& sexp) {
			remove_prop(name);
			if (!is_prop_name(name))
				throw Error{Error::Code::InvalidArgument,
					    "invalid property name ('%s')",
					    name.c_str()};
			seq_.emplace_back(make_symbol(std::move(name)));
			seq_.emplace_back(std::move(sexp));
			return *this;
		}
		template <typename... Args>
		List& add_prop(std::string&& name, Sexp&& sexp, Args... args) {
			remove_prop(name);
			add_prop(std::move(name), std::move(sexp));
			add_prop(std::forward<Args>(args)...);
			return *this;
		}

		void remove_prop(const std::string& name) {
			if (!is_prop_name(name))
				throw Error{Error::Code::InvalidArgument,
					"invalid property name ('%s')", name.c_str()};
			auto it = std::find_if(seq_.begin(), seq_.end(), [&](auto&& elm) {
				return elm.type() == Sexp::Type::Symbol &&
					elm.value() == name;
			});
			if (it != seq_.cend() && it + 1 != seq_.cend()) {
				/* erase propname and value.*/
				seq_.erase(it, it + 2);
			}
		}

		/**
		 * Remove all elements from the list.
		 */
		void clear() { seq_.clear(); }

		/**
		 * Get the number of elements in the list
		 *
		 * @return number
		 */
		size_t size() const { return seq_.size(); }

		/**
		 * Is the list empty?
		 *
		 * @return true or false
		 */
		size_t empty() const { return seq_.empty(); }

	private:
		friend struct Sexp;
		Seq seq_;
	};

	/**
	 * Construct a list sexp from a List
	 *
	 * @param list a list-list
	 * @param sexp  a Sexp
	 * @param args rest arguments
	 *
	 * @return a sexp.
	 */
	static Sexp make_list(List&& list) { return Sexp{Type::List, std::move(list.seq_)}; }
	template <typename... Args> static Sexp make_list(Sexp&& sexp, Args... args)
	{
		List lst;
		lst.add(std::move(sexp)).add(std::forward<Args>(args)...);
		return make_list(std::move(lst));
	}

	/**
	 * Construct a property list sexp from a List
	 *
	 * @param name the property name; must start wtth ':'
	 * @param sexp  a Sexp
	 * @param args rest arguments (property list)
	 *
	 * @return a sexp.
	 */
	template <typename... Args>
	static Sexp make_prop_list(std::string&& name, Sexp&& sexp, Args... args)
	{
		List list;
		list.add_prop(std::move(name), std::move(sexp), std::forward<Args>(args)...);
		return make_list(std::move(list));
	}

	/**
	 * Construct a properrty list sexp from a List
	 *
	 * @param funcname function name for the call
	 * @param name the property name; must start wtth ':'
	 * @param sexp  a Sexp
	 * @param args rest arguments (property list)
	 *
	 * @return a sexp.
	 */
	template <typename... Args>
	static Sexp make_call(std::string&& funcname, std::string&& name, Sexp&& sexp, Args... args)
	{
		List list;
		list.add(make_symbol(std::move(funcname)));
		list.add_prop(std::move(name), std::move(sexp), std::forward<Args>(args)...);
		return make_list(std::move(list));
	}

	/// Some type helpers
	bool is_list() const { return type() == Type::List; }
	bool is_string() const { return type() == Type::String; }
	bool is_number() const { return type() == Type::Number; }
	bool is_symbol() const { return type() == Type::Symbol; }
	bool is_empty() const { return type() == Type::Empty; }

	operator bool() const { return !is_empty(); }

	static constexpr auto SymbolNil{"nil"};
	static constexpr auto SymbolT{"t"};
	bool                  is_nil() const { return is_symbol() && value() == SymbolNil; }
	bool                  is_t() const { return is_symbol() && value() == SymbolT; }

	/**
	 * Is this a prop-list? A prop list is a list sexp with alternating
	 * property / sexp
	 *
	 * @return
	 */
	bool is_prop_list() const
	{
		if (!is_list() || list().size() % 2 != 0)
			return false;
		else
			return is_prop_list(list().begin(), list().end());
	}

	/**
	 * Is this a call? A call is a list sexp with a symbol (function name),
	 * followed by a prop list
	 *
	 * @return
	 */
	bool is_call() const
	{
		if (!is_list() || list().size() % 2 != 1 || !list().at(0).is_symbol())
			return false;
		else
			return is_prop_list(list().begin() + 1, list().end());
	}

private:
	Sexp(Type typearg, std::string&& valuearg) : type_{typearg}, value_{std::move(valuearg)} {
		if (is_list())
			throw Error{Error::Code::InvalidArgument, "cannot be a list type"};
		if (is_empty())
			throw Error{Error::Code::InvalidArgument, "cannot be an empty type"};
	}
	Sexp(Type typearg, Seq&& seq) : type_{Type::List}, seq_{std::move(seq)} {
		if (!is_list())
			throw Error{Error::Code::InvalidArgument, "must be a list type"};
		if (is_empty())
			throw Error{Error::Code::InvalidArgument, "cannot be an empty type"};
	}
	/**
	 * Is the sexp a valid property name?
	 *
	 * @param sexp a Sexp.
	 *
	 * @return true or false.
	 */
	static bool is_prop_name(const std::string& str)
	{
		return str.size() > 1 && str.at(0) == ':';
	}
	static bool is_prop_name(const Sexp& sexp)
	{
		return sexp.is_symbol() && is_prop_name(sexp.value());
	}

	static bool is_prop_list(Seq::const_iterator b, Seq::const_iterator e)
	{
		while (b != e) {
			const Sexp& s{*b};
			if (!is_prop_name(s))
				return false;
			if (++b == e)
				return false;
			++b;
		}
		return b == e;
	}

	Type	    type_;  /**<  Type of node */
	std::string value_; /**< String value of node (only for
			       * non-Type::Lst)*/
	Seq seq_;	    /**< Children of node (only for
				   * Type::Lst) */
};

static inline std::ostream&
operator<<(std::ostream& os, Sexp::Type id)
{
	switch (id) {
	case Sexp::Type::List: os << "list"; break;
	case Sexp::Type::String: os << "string"; break;
	case Sexp::Type::Number: os << "number"; break;
	case Sexp::Type::Symbol: os << "symbol"; break;
	case Sexp::Type::Empty: os << "empty"; break;
	default: throw std::runtime_error("unknown node type");
	}

	return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const Sexp& sexp)
{
	os << sexp.to_sexp_string();
	return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const Sexp::List& sexp)
{
	os << Sexp::make_list(Sexp::List(sexp));
	return os;
}

} // namespace Mu

#endif /* MU_SEXP_HH__ */
