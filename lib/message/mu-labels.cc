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


#include "mu-labels.hh"
#include <set>

using namespace Mu;
using namespace Mu::Labels;

Result<void>
Mu::Labels::validate_label(const std::string &label)
{
	if (label.empty())
		return Err(Error{Error::Code::InvalidArgument,
				 "labels cannot be empty"});
	else if (!g_utf8_validate(label.c_str(), label.size(), {})) // perhpps put hex in err str?
		return Err(Error{Error::Code::InvalidArgument,
				"labels must be valid UTF-8"});

	const auto cstr{label.c_str()};

	// labels must be at least two characters and not start with a
	// dash. these limitations are there to avoid confusion with
	// command-line parameters.
	if (cstr[0] == '-' || cstr[0] == '+')
		return Err(Error{Error::Code::InvalidArgument,
				 "labels cannot start with '+' or '-' ({})", label});

	for (auto cur = cstr; cur && *cur; cur = g_utf8_next_char(cur)) {

		const gunichar uc = g_utf8_get_char(cur);
		if (g_unichar_isalnum(uc))
			continue; // alphanum is okay

		// almost all non-ctrl ascii is allowed _except_ =,<,>,$,[]
		if (uc > ' ' &&  uc <= '~') {
			switch (uc) {
			case '"':
			case ',':
			case '/':
			case '\\':
			case '*':
			case '$':
				return Err(Error{Error::Code::InvalidArgument,
				 "illegal character '{}' in label '{}'", uc, label});
			default:
				break;
			}
		} else if (::isprint(uc))
			return Err(Error{Error::Code::InvalidArgument,
					"illegal non alpha-numeric character '{}' in label '{}'",
					static_cast<char>(uc), label});
		else
			return Err(Error{Error::Code::InvalidArgument,
					"illegal non alpha-numeric character {:#x} in label '{}'",
					uc, label});
	}

	return Ok();
}

Result<DeltaLabel>
Mu::Labels::parse_delta_label(const std::string &expr)
{
	if (expr.size() < 1)
		return Err(Error{Error::Code::InvalidArgument,
				 "empty labels are invalid"});
	const auto cstr{expr.c_str()};

	// first char; either '+' or '-'
	if (cstr[0] != '+' && cstr[0] != '-')
		return Err(Error{Error::Code::InvalidArgument,
				 "invalid label expression '{}'; "
				 "must start with '+' or '-'",
				 expr});
	Delta delta{cstr[0] == '+' ? Delta::Add : Delta::Remove};
	std::string label{expr.substr(1)};

	if (const auto res = validate_label(label); !res)
		return Err(res.error());

	return Ok(DeltaLabel{std::move(delta), std::move(label)});
}


Result<DeltaLabelVec>
Mu::Labels::parse_delta_labels(const std::string& exprs,
			       const std::string sepa)
{

	DeltaLabelVec deltas{};
	for (const auto& expr: split(exprs, sepa)) {
		if (auto delta = parse_delta_label(expr); !delta)
			return Err(std::move(delta.error()));
		else
			deltas.emplace_back(*delta);
	}

	return Ok(std::move(deltas));
}





struct cmp_delta_label { // can not yet be a λ in C++17
	bool operator()(const DeltaLabel& dl1, const DeltaLabel& dl2) const {
		return dl1.second < dl2.second;
	}
};
std::pair<LabelVec, DeltaLabelVec>
Mu::Labels::updated_labels(const LabelVec& labels, const DeltaLabelVec& deltas)
{
	// quite complicated!

	// First, the delta; put in a set for uniqueness; and use a special
	// comparison operator so "add" and "remove" deltas are considered "the same"
	// for the set; then fill the set from the end of the deltas vec to the begining,
	// so "the last one wins", as we want.

	// only one change per label, last one wins
	std::set<DeltaLabel, cmp_delta_label> working_deltas{
		deltas.rbegin(), deltas.rend()
	};

	// working set of lables; we start with _all_ (uniquified)
	std::set<std::string> working_labels{labels.begin(), labels.end()};

	// keep track of the deltas that actually changed something (ie.
	// removing a non-existing label or adding an already existing one is
	// not a change.)
	DeltaLabelVec effective_deltas;

	// now check each of our "workin deltas", apply on the working_labels, and
	// if they changed anything, add to 'effectivc_deltas
	for (auto&  delta: working_deltas) {
		switch (delta.first) {
		case Delta::Add:
			// add to the _effective_ deltas if the element wasn't
			// there before.
			if (working_labels.emplace(delta.second).second)
				effective_deltas.emplace_back(std::move(delta));
			break;
		case Delta::Remove:
			// add to the _effective_ deltas if the element was
			// actually removed.
			if (working_labels.erase(delta.second) > 0U)
				effective_deltas.emplace_back(std::move(delta));
			break;
		default:
			// can't have Neutral here.
			throw std::runtime_error("invalid delta");
		}
	}


	return {{ working_labels.begin(), working_labels.end()}, effective_deltas};
}



#ifdef BUILD_TESTS

#include "utils/mu-test-utils.hh"

static void
test_parse_delta_label()
{
	{
		const auto expr = parse_delta_label("+foo");
		assert_valid_result(expr);
		g_assert_true(expr->first == Delta::Add);
		assert_equal(expr->second, "foo");
	}

	{
		const auto expr = parse_delta_label("-bar@cuux");
		assert_valid_result(expr);
		g_assert_true(expr->first == Delta::Remove);
		assert_equal(expr->second, "bar@cuux");
	}

	g_assert_false(!!parse_delta_label("ravenking"));
	g_assert_false(!!parse_delta_label("+norrell strange"));
	g_assert_false(!!parse_delta_label("-😨"));
}


static void
test_parse_delta_labels()
{
	{
		const auto deltas = parse_delta_labels("+foo,-bar@cuux",",");
		assert_valid_result(deltas);

		g_assert_true(deltas->at(0).first == Delta::Add);
		assert_equal(deltas->at(0).second, "foo");

		g_assert_true(deltas->at(1).first == Delta::Remove);
		assert_equal(deltas->at(1).second, "bar@cuux");
	}

	{
		const auto expr = parse_delta_labels("+foo @boo🐂", " ");
		g_assert_false(!!expr);
	}
}



static void
test_validate_label()
{
	g_assert_true(!!validate_label("ravenking"));
	g_assert_true(!!validate_label("@raven+king"));
	g_assert_true(!!validate_label("operation:mindcrime"));

	g_assert_false(!!validate_label("norrell strange"));
	g_assert_false(!!validate_label("😨"));
	g_assert_false(!!validate_label(""));
	g_assert_false(!!validate_label("+"));
	g_assert_false(!!validate_label("-"));
}

static void
test_updated_labels()
{
	const auto assert_eq=[](const LabelVec& labels, const DeltaLabelVec& deltas,
				const LabelVec& exp_labels, const DeltaLabelVec& exp_deltas) {

		const auto& [res_labels, res_deltas] = updated_labels(labels, deltas);

		assert_equal_seq_str(res_labels, exp_labels);
		g_assert_cmpuint(res_deltas.size(), ==, exp_deltas.size());
		for (size_t i{}; i != res_deltas.size(); ++i) {
			g_assert_true(res_deltas[i].first == exp_deltas[i].first);
			assert_equal(res_deltas[i].second, exp_deltas[i].second);
		}
	};

	const auto delta_labels = [](std::initializer_list<std::string> strs)->DeltaLabelVec {
		DeltaLabelVec deltas;
		std::transform(strs.begin(), strs.end(), std::back_inserter(deltas),
			       [](auto str) {
				       const auto res = parse_delta_label(str);
				       assert_valid_result(res);
				       return *res;
			       });
		return deltas;
	};

	assert_eq({"foo", "bar", "cuux"}, delta_labels({"+fnorb", "+bar", "-bar", "+bar", "-cuux"}),
		  {"bar", "fnorb", "foo"}, delta_labels({"-cuux", "+fnorb"}));

	assert_eq({}, delta_labels({"-fnorb", "-fnorb", "+whiteward", "+altesia", "+fnorb"}),
		  {"altesia", "fnorb", "whiteward"}, delta_labels({"+altesia", "+fnorb", "+whiteward"}));

	assert_eq({"piranesi", "hyperion", "mordor", "piranesi"}, delta_labels({}),
		  {"hyperion", "mordor", "piranesi"}, delta_labels({}));
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/message/labels/parse-delta-label", test_parse_delta_label);
	g_test_add_func("/message/labels/parse-delta-labels", test_parse_delta_labels);
	g_test_add_func("/message/labels/validate-label", test_validate_label);
	g_test_add_func("/message/labels/updated-labels", test_updated_labels);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
