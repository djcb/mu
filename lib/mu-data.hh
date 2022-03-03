/*
**  Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public License
**  as published by the Free Software Foundation; either version 2.1
**  of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free
**  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
**  02110-1301, USA.
*/

#ifndef __DATA_HH__
#define __DATA_HH__

#include <string>
#include <iostream>
#include <regex>

#include <mu-message.hh>
#include <utils/mu-utils.hh>

namespace Mu {

// class representing some data item; either a Value or a Range a Value can still be a Regex (but
// that's not a separate type here)
struct Data {
	enum class Type { Value, Range };
	virtual ~Data() = default;

	Type               type;   /**< type of data */
	std::string        field;  /**< full name of the field */
	std::string        prefix; /**< Xapian prefix for thef field */
	Message::Field::Id id;     /**< Xapian value no for the field  */

      protected:
	Data(Type _type, const std::string& _field, const std::string& _prefix,
	     Message::Field::Id _id)
	    : type(_type), field(_field), prefix(_prefix), id(_id)
	{
	}
};

/**
 * operator<<
 *
 * @param os an output stream
 * @param t a data type
 *
 * @return the updated output stream
 */
inline std::ostream&
operator<<(std::ostream& os, Data::Type t)
{
	switch (t) {
	case Data::Type::Value: os << "value"; break;
	case Data::Type::Range: os << "range"; break;
	default: os << "bug"; break;
	}
	return os;
}

/**
 *  Range type -- [a..b]
 */
struct Range : public Data {
	/**
	 * Construct a range
	 *
	 * @param _field the field
	 * @param _prefix the xapian prefix
	 * @param _id xapian value number
	 * @param _lower lower bound
	 * @param _upper upper bound
	 */
	Range(const std::string& _field,
	      const std::string& _prefix,
	      Message::Field::Id _id,
	      const std::string& _lower,
	      const std::string& _upper)
	    :

	      Data(Data::Type::Range, _field, _prefix, _id), lower(_lower), upper(_upper)
	{
	}

	std::string lower; /**< lower bound */
	std::string upper; /**< upper bound */
};

/**
 * Basic value
 *
 */
struct Value : public Data {
	/**
	 * Construct a Value
	 *
	 * @param _field the field
	 * @param _prefix the xapian prefix
	 * @param _id field id
	 * @param _value the value
	 */
	Value(const std::string& _field,
	      const std::string& _prefix,
	      Message::Field::Id _id,
	      const std::string& _value,
	      bool               _phrase = false)
	    : Data(Value::Type::Value, _field, _prefix, _id), value(_value), phrase(_phrase)
	{
	}

	std::string value; /**< the value */
	bool        phrase;
};

/**
 * operator<<
 *
 * @param os an output stream
 * @param v a data ptr
 *
 * @return the updated output stream
 */
inline std::ostream&
operator<<(std::ostream& os, const std::unique_ptr<Data>& v)
{
	switch (v->type) {
	case Data::Type::Value: {
		const auto bval = dynamic_cast<Value*>(v.get());
		os << ' ' << quote(v->field) << ' ' << quote(utf8_flatten(bval->value));
		if (bval->phrase)
			os << " (ph)";

		break;
	}
	case Data::Type::Range: {
		const auto rval = dynamic_cast<Range*>(v.get());
		os << ' ' << quote(v->field) << ' ' << quote(rval->lower) << ' '
		   << quote(rval->upper);
		break;
	}
	default: os << "unexpected type"; break;
	}

	return os;
}

} // namespace Mu

#endif /* __DATA_HH__ */
