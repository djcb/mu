/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-utils.hh"
#include "mu-option.hh"
#include "mu-regex.hh"

#include <string>
#include <array>
#include <string_view>
#include <algorithm>

using namespace Mu;


static bool
starts_with(std::string_view haystack, std::string_view needle)
{
	if (needle.size() > haystack.size())
		return false;

	for (auto&& c = 0U; c != needle.size(); ++c)
		if (::tolower(haystack[c]) != ::tolower(needle[c]))
			return false;

	return true;
}

static bool
matches(std::string_view haystack, std::string_view needle)
{
	if (needle.size() != haystack.size())
		return false;
	else
		return starts_with(haystack, needle);
}



/**
 * HTML parsing context
 *
 */
class Context {
public:
	/**
	 * Construct a parsing context
	 *
	 * @param html some html to parse
	 */
	Context(const std::string& html): html_{html}, pos_{} {}

	/**
	 * Are we done with the html blob, i.e, has it been fully scraped?
	 *
	 * @return true or false
	 */
	bool done() const {
		return pos_ >= html_.size();
	}

	/**
	 * Get the current position
	 *
	 * @return position
	 */
	size_t position() const {
		return pos_;
	}

	/**
	 * Get the size of the HTML
	 *
	 * @return size
	 */
	size_t size() const {
		return html_.size();
	}

	/**
	 * Advance the position by _n_ characters.
	 *
	 * @param n number by which to advance.
	 */
	void advance(size_t n=1) {
		if (pos_ + n > html_.size())
			throw std::range_error("out of range");
		pos_ += n;
	}

	/**
	 * Are we looking at the given string?
	 *
	 * @param str string to match (case-insensitive)
	 *
	 * @return true or false
	 */
	bool looking_at(std::string_view str) const {
		if (pos_ >= html_.size() || pos_ + str.size() >= html_.size())
			return false;
		else
			return matches({html_.data()+pos_, str.size()}, str);
	}

	/**
	 * Grab a substring-view from the html
	 *
	 * @param fpos starting position
	 * @param len length
	 *
	 * @return string view
	 */
	std::string_view substr(size_t fpos, size_t len) const {
		if (fpos + len > html_.size())
			throw std::range_error(mu_format("{} + {} > {}",
							 fpos, len, html_.size()));
		else
			return { html_.data() + fpos, len };
	}

	/**
	 * Grab the string of alphabetic characters at the
	 * head (pos) of the context, and advance over it.
	 *
	 * @return the head-word or empty
	 */
	std::string_view eat_head_word() {
		size_t start_pos{pos_};
		while (!done()) {
			if (!::isalpha(html_.at(pos_)))
				break;
			++pos_;
		}
		return {html_.data() + start_pos, pos_ - start_pos};
	}


	/**
	 * Get the scraped data; only available when done()

	 * @return scraped data
	 */
	std::string scraped() {
		return cleanup(raw_scraped_);
	}

	/**
	 * Get the raw scrape buffer, where we can append
	 * scraped data.
	 *
	 * @return the buffer
	 */
	std::string& raw_scraped() {
		return raw_scraped_;
	}


	/**
	 * Get a reference to the HTML
	 *
	 * @return  html
	 */
	const std::string& html() const { return html_; }

private:

	/**
	 * Cleanup some raw scraped html: remove superfluous
	 * whitespace, avoid too long lines.
	 *
	 * @param unclean
	 *
	 * @return cleaned up string.
	 */
	std::string cleanup(const std::string unclean) const {
		// reduce whitespace and avoid too long lines;
		// makes it easier to debug.
		bool was_wspace{};
		size_t col{};
		std::string clean;
		clean.reserve(unclean.size()/2);
		for(auto&& c: unclean) {
			auto wspace = c == ' ' || c == '\t' || c == '\n';
			if (wspace) {
				was_wspace = true;
				continue;
			}
			++col;
			if (was_wspace) {
				if (col > 80) {
					clean += '\n';
					col = 0;
				} else if (!clean.empty())
					clean += ' ';
				was_wspace = false;
			}
			clean += c;
		}
		return clean;
	}


	const std::string&	html_; // no copy!
	size_t			pos_{};
	std::string		raw_scraped_;
};


G_GNUC_UNUSED static auto
format_as(const Context& ctx)
{
	return mu_format("<{}:{}: '{}'>",
			 ctx.position(), ctx.size(),
			 ctx.substr(ctx.position(),
				    std::min(static_cast<size_t>(8),
					     ctx.size() - ctx.position())));
}


static void
skip_quoted(Context& ctx, std::string_view quote)
{
	while(!ctx.done()) {
		if (ctx.looking_at(quote)) // closing quote
			return;
		ctx.advance();
	}
}


// attempt to skip over <script> / <style> blocks
static void
skip_script_style(Context& ctx, std::string_view tag)
{
	// <script> or <style> must be ignored

	bool escaped{};
	bool quoted{}, squoted{};
	bool inl_comment{};
	bool endl_comment{};

	auto end_tag_str = mu_format("</{}>", tag);
	auto end_tag = std::string_view(end_tag_str.data());

	while (!ctx.done()) {

		if (inl_comment) {
			if (ctx.looking_at("*/")) {
				inl_comment = false;
				ctx.advance(2);
			} else
				ctx.advance();
			continue;
		}

		if (endl_comment) {
			endl_comment = ctx.looking_at("\n");
			ctx.advance();
			continue;
		}

		if (ctx.looking_at("\\")) {
			escaped = !escaped;
			ctx.advance();
			continue;
		}

		if (ctx.looking_at("\"") && !escaped && squoted)  {
			quoted = !quoted;
			ctx.advance();
			continue;
		}

		if (ctx.looking_at("'") && !escaped && !quoted) {
			squoted = !squoted;
			ctx.advance();
			continue;
		}


		if (ctx.looking_at("/*")) {
			inl_comment = true;
			ctx.advance(2);
			continue;
		}

		if (ctx.looking_at("//")) {
			endl_comment = true;
			ctx.advance(2);
			continue;
		}

		if (!quoted && !squoted && ctx.looking_at(end_tag)) {
			ctx.advance(end_tag.size());
			break; /* we're done, finally! */
		}

		ctx.advance();
	}
}

// comment block; ignore completely
// pos will be immediately after the '<!--
static void
comment(Context& ctx)
{
	constexpr std::string_view comment_endtag{"-->"};
	while (!ctx.done()) {

		if (ctx.looking_at(comment_endtag)) {
			ctx.advance(comment_endtag.size());
			ctx.raw_scraped() += ' ';
			return;
		}
		ctx.advance();
	}
}

static bool // do we need a SPC separator for this tag?
needs_separator(std::string_view tagname)
{
	constexpr std::array<const char*, 7> nosep_tags = {
		"b", "em", "i", "s", "strike", "tt", "u"
	};
	return !seq_some(nosep_tags, [&](auto&& t){return matches(tagname, t);});
}

static bool // do we need to skip the element completely?
is_skip_element(std::string_view tagname)
{
	constexpr std::array<const char*, 4> skip_tags = {
		"script", "style", "head", "meta"
	};
	return seq_some(skip_tags, [&](auto&& t){return matches(tagname, t);});
}

// skip the end-tag
static void
end_tag(Context& ctx)
{
	while (!ctx.done()) {
		if (ctx.looking_at(">")) {
			ctx.advance();
			return;
		}
		ctx.advance();
	}
}

// skip the whole element
static void
skip_element(Context& ctx, std::string_view tagname)
{
	// do something special?
}


// the start of a tag, i.e., pos will be just after the '<'
static void
tag(Context& ctx)
{
	// some elements we want to skip completely,
	// for others just the tags.
	constexpr std::string_view comment_start {"!--"};
	if (ctx.looking_at(comment_start)) {
		ctx.advance(comment_start.size());
		comment(ctx);
		return;
	}

	if (ctx.looking_at("/")) {
		ctx.advance();
		end_tag(ctx);
		return;
	}

	auto tagname = ctx.eat_head_word();
	if (tagname == "script" ||tagname == "style") {
		skip_script_style(ctx, tagname);
		return;
	}
	else if (is_skip_element(tagname))
		skip_element(ctx, tagname);

	const auto needs_sepa = needs_separator(tagname);
	while (!ctx.done()) {

		if (ctx.looking_at("\""))
			skip_quoted(ctx, "\"");

		if (ctx.looking_at("'"))
			skip_quoted(ctx, "'");

		if (ctx.looking_at(">")) {
			ctx.advance();
			if (needs_sepa)
				ctx.raw_scraped() += ' ';
			return;
		}
		ctx.advance();
	}
}


static void
html_escape_char(Context& ctx)
{
	// we only care about a few accented chars, and add them unaccented, lowercase, since that's
	// we do for indexing anyway.
	constexpr std::array<const char*, 11> escs = {
		"breve",
		"caron",
		"circ",
		"cute",
		"grave",
		"horn"/*thorn*/,
		"macr",
		"slash",
		"strok",
		"tilde",
		"uml",
	};

	auto unescape=[escs](std::string_view esc)->char {
		if (esc.empty())
			return ' ';
		auto first{static_cast<char>(::tolower(esc.at(0)))};
		auto rest=esc.substr(1);
		if (seq_some(escs, [&](auto&& e){return starts_with(rest, e);}))
			return first;
		else
			return ' ';
	};

	size_t start_pos{ctx.position()};
	while (!ctx.done()) {
		if (ctx.looking_at(";")) {
			auto esc = ctx.substr(start_pos, ctx.position() - start_pos);
			ctx.raw_scraped() += unescape(esc);
			ctx.advance();
			return;
		}
		ctx.advance();
	}
}


// a block of text to be scraped
static void
text(Context& ctx)
{
	size_t start_pos{ctx.position()};
	while (!ctx.done()) {

		if (ctx.looking_at("&")) {

			ctx.raw_scraped() += ctx.substr(start_pos,
							ctx.position() - start_pos);
			ctx.advance();
			html_escape_char(ctx);
			start_pos = ctx.position();

		} else if (ctx.looking_at("<")) {
			ctx.raw_scraped() += ctx.substr(start_pos,
							ctx.position() - start_pos);
			ctx.advance();
			tag(ctx);
			start_pos = ctx.position();

		} else
			ctx.advance();
	}

	ctx.raw_scraped() += ctx.substr(start_pos, ctx.size() - start_pos);
}

static Context *CTX{};

std::string
Mu::html_to_text(const std::string& html)
{
	Context ctx{html};
	CTX = &ctx;

	text(ctx);

	CTX = {};
	return ctx.scraped();
}

#ifdef BUILD_TESTS
#include "mu-test-utils.hh"

static void
test_1()
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{ "<!-- Hello -->A",  "A"   },
			{ "A<!-- Test -->B", "A B"  },
			{ "A<i>a</i><b>p</b>", "Aap"},
			{ "N&ocute;&Ocirc;t", "Noot"},
			{
				"foo<!-- bar --><i>c</i>uu<bla>x</bla>"
				"<!--hello -->world<!--",
				"foo cuu x world"
			}
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}

static void
test_2()
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{ R"(<i>hello, <b bar="/b">world!</b>)",
			  "hello, world!"},
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}


static void
test_3()
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{R"(<i>hello, </i><script language="javascript">
				function foo() {
					alert("Stroopwafel!"); // test
				}
			    </script>world!)",
			  "hello, world!"},
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/html-to-text/test-1", test_1);
	g_test_add_func("/html-to-text/test-2", test_2);
	g_test_add_func("/html-to-text/test-3", test_3);

	return g_test_run();
}


#endif /*BUILD_TESTS*/


#ifdef BUILD_HTML_TO_TEXT

#include "mu-utils-file.hh"

// simple tool that reads html on stdin and outputs text on stdout
// e.g. curl --silent https://www.example.com | build/lib/utils/mu-html2text

int
main (int argc, char *argv[])
{
	auto res = read_from_stdin();
	if (!res) {
		mu_printerrln("error reading from stdin: {}", res.error().what());
		return 1;
	}

	mu_println("{}", html_to_text(*res));

	return 0;
}

#endif /*BUILD_HTML_TO_TEXT*/
