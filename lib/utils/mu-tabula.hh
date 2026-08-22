/*
** Copyright (C) 2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

// Quick & dirty text tables, i.e., a minimal version of tabular / tabulate.

#ifndef MU_TABULA_HH
#define MU_TABULA_HH

#include <vector>
#include <string>
#include <glib.h>
#include <fmt/format.h>
#include <fmt/color.h>

namespace Mu::Tabula {
namespace detail {
/**
 * Attempt to determine the _display length_ of some utf8 string;
 * do not count zero-width chars and count wide chars (such as emojis) for two.
 *
 * @param s some string
 *
 * @return the display length
 */
inline size_t display_length(const std::string& s) {

	size_t len{};
	for (const char *p{s.c_str()}; *p; p = g_utf8_next_char(p)) {
		const auto uc{g_utf8_get_char(p)};
		if (g_unichar_iszerowidth(uc))
			continue;
		len += g_unichar_iswide(uc) ? 2 : 1;
	}
	return len;
}
} // detail

/**
* One cell in a table (a string with some text-style)
*/
struct Cell {
	Cell(const char* str, fmt::text_style ts={}): data{str}, ts{ts} {}
	Cell(std::string_view sv, fmt::text_style ts={}): data{sv}, ts{ts} {}
	Cell(std::string d, fmt::text_style ts={}): data{std::move(d)}, ts{ts} {}
	// display width (in columns) of the (unstyled) data; count
	// double-width characters (e.g. emoji) as 2, zero-width ones as 0.
	size_t display_length() const { return detail::display_length(data); }
	std::string display_string() const { return fmt::format(ts, "{}", data); }
	std::string data;
	fmt::text_style ts{};
};

/**
* One row in a table (vector of Cells)
*/
struct Row {
	using ValueType = std::vector<Cell>;
	// Constructor that accepts exactly N cells
	template<typename... Cells>
	requires (std::is_convertible_v<Cells, Cell> && ...)
	explicit Row(Cells&&... cells)
	    : cells{Cell{std::forward<Cells>(cells)}...}
	    {}
	size_t size() const { return cells.size();}
	const Cell& column(size_t n) const { return cells.at(n); }
	ValueType cells;
};

/**
* A table (vector of Rows)
*/
struct Table {
	using ValueType = std::vector<Row>;
	/**
	 * Append a row to the table
	 *
	 * @param row a row
	 * @param ts apply (logical or) text-style to all cells in row
	 */
	void append(Row&& row, fmt::text_style ts={}) {
		auto& newrow{rows.emplace_back(std::move(row))};
		for (auto& cell: newrow.cells)
			cell.ts |= ts;
	}

	bool empty() const { return rows.empty(); }
	ValueType rows;
};

namespace detail { // implementation details
using MaxWidths=std::vector<size_t>;
inline MaxWidths max_widths (const Table& table) {
	MaxWidths max_widths{};
	for (const auto& row: table.rows) {
		if (max_widths.size() < row.size())
			max_widths.resize(row.size());
		for (size_t col{}; col != row.size(); ++col)
			max_widths[col] = std::max(max_widths[col],
						   row.column(col).display_length());
	}
	return max_widths;
}

inline std::string format_vline(const MaxWidths& max_widths) {
	std::string result;
	if (!max_widths.empty()) {
		result = "+";
		for (const auto& col: max_widths) // width + surrounding spaces
			fmt::format_to(std::back_inserter(result),
				       "{:-<{}}+", "", col + 2);
	}

	return result;
}

inline std::string format_row(const MaxWidths& max_widths,
			      const Row& cols) {
	std::string result{"|"};
	// iterate over the full table width; pad rows with fewer
	// cells with empty ones, so the right border stays aligned.
	for (size_t col{}; col != max_widths.size(); ++col) {
		static const Cell empty_cell{""};
		const auto& cell{col < cols.size() ? cols.column(col) : empty_cell};
		// pad manually, based on the cell's display width;
		// fmt would count the invisible ANSI style escapes.
		const auto pad{max_widths[col] - cell.display_length()};
		fmt::format_to(std::back_inserter(result),
			       " {}{:{}} |", cell.display_string(), "", pad);
	}
	return result;
}
} // detail


/**
 * Format a Table as a string; found through ADL by fmt, so a Table
 * can be passed directly to fmt::format() / mu_format() and friends.
 *
 * @param table a table
 *
 * @return the formatted table
 */
inline std::string
format_as(const Table& table) {

	std::string result;

	if (!table.empty()) {
		const auto max_widths{detail::max_widths(table)};
		const auto vline{detail::format_vline(max_widths)};
		result = vline;
		for (const auto& row: table.rows) {
			result += '\n';
			result += detail::format_row(max_widths, row);
			result += '\n';
			result += vline;
		}
	}

	return result;
}


}// Mu::Tabula
#endif /*MU_TABULA_HH*/
