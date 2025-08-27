/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_LABELS_HH
#define MU_LABELS_HH

#include <utils/mu-result.hh>

#include <string>
#include <vector>
#include <utility>

namespace Mu {
namespace Labels {

using LabelVec = std::vector<std::string>;

enum struct Delta { Add='+', Remove='-'};
using DeltaLabel = std::pair<Delta, std::string>;
using DeltaLabelVec = std::vector<DeltaLabel>;

/**
 * Parse a label expression, i.e., a label prefixed with '+' or '-'
 *
 * This also validates the label, as per valid_label()
 *
 * @param expr expression
 *
 * @return a result with either a DeltaLabel or an error
 */
Result<DeltaLabel> parse_delta_label(const std::string& expr);

/**
 * Parse a series of label expressions, with some separator
 *
 * This also validates the labels, as per valid_label()
 *
 * @param exprs label expressions
 *
 * @return a result with either a DeltaLabel or an error
 */
Result<DeltaLabelVec> parse_delta_labels(const std::string& exprs,
					 const std::string sepa);

/**
 * Is the label (without +/- prefix) valid?
 *
 * @param label some label
 *
 * @return either Ok or some error
 */
Result<void> validate_label(const std::string &label);

/**
 * Apply deltas to labels and return the result as well as the
 * effective changes.
 *
 * The deltas are handled in order; 'last one wins', hence:
 *      { +foo, -foo }    ==> no foo in the result
 *  and
 *      { -foo, +foo }   ==> foo in results
 *
 * The result labels do not contain duplicates. Order is not necessarily
 * maintained.
 *
 * The result is a pair, the first element is LabelVec with the results
 * as explained.
 *
 * The second is a DeltaVec with the _effective_ changes; this the input
 * DeltaVec but without any +<label> if label was already in labels; and without
 * -<label> if <label> was not in labels.
 *
 * @param labels existing labels
 * @param deltas deltas for these labels
 *
 * @return updated labels
 */
std::pair<LabelVec, DeltaLabelVec> updated_labels(const LabelVec& labels, const DeltaLabelVec& delta);

} // Labels
} // Mu


#endif /*MU_LABELS_HH*/
