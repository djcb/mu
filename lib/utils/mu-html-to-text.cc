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

#include "mu-utils.hh"
#include "mu-option.hh"

#include <string>
#include <array>
#include <string_view>
#include <algorithm>
#include <charconv>

using namespace Mu;


static bool
starts_with(std::string_view haystack, std::string_view needle)
{
	if (needle.size() > haystack.size())
		return false;

	for (size_t c{}; c != needle.size(); ++c)
		if (to_ascii_lower(haystack[c]) != to_ascii_lower(needle[c]))
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
	Context(const std::string& html): html_{html}, pos_{} {
		raw_scraped_.reserve(html.size()/2);
	}

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
		if (pos_ >= html_.size() || pos_ + str.size() > html_.size())
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
			if (!is_ascii_alpha(html_.at(pos_)))
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
	std::string cleanup(const std::string& unclean) const {
		// reduce whitespace and avoid too long lines;
		// makes it easier to debug.
		bool was_wspace{};
		size_t col{};
		std::string clean;
		clean.reserve(unclean.size()/2);
		for(auto&& c: unclean) {
			if (is_ascii_space(c)) {
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


[[maybe_unused]] static auto
format_as(const Context& ctx)
{
	return mu_format("<{}:{}: '{}'>",
			 ctx.position(), ctx.size(),
			 ctx.substr(ctx.position(),
				    std::min(static_cast<size_t>(8),
					     ctx.size() - ctx.position())));
}


// skip until (and over) the closing quote; pos must be just after the
// opening quote.
static void
skip_quoted(Context& ctx, std::string_view quote)
{
	while(!ctx.done()) {
		if (ctx.looking_at(quote)) { // closing quote
			ctx.advance();
			return;
		}
		ctx.advance();
	}
}


// attempt to skip over <script> / <style> blocks
static void
skip_script_style(Context& ctx, std::string_view tag)
{
	// <script> or <style> must be ignored

	bool quoted{}, squoted{};
	bool inl_comment{};
	bool endl_comment{};

	// '//'-comments exist in javascript but not in css, where '//' may
	// occur in urls.
	const auto is_script{matches(tag, "script")};

	const auto end_tag_str = mu_format("</{}>", tag);
	const std::string_view end_tag{end_tag_str};

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
			if (ctx.looking_at("\n"))
				endl_comment = false;
			ctx.advance();
			continue;
		}

		if (!quoted && !squoted) {

			if (ctx.looking_at(end_tag)) {
				ctx.advance(end_tag.size());
				break; /* we're done, finally! */
			}

			if (ctx.looking_at("/*")) {
				inl_comment = true;
				ctx.advance(2);
				continue;
			}

			if (is_script && ctx.looking_at("//")) {
				endl_comment = true;
				ctx.advance(2);
				continue;
			}
		}

		if ((quoted || squoted) && ctx.looking_at("\\")) {
			// skip the escaped character as well
			ctx.advance();
			if (!ctx.done())
				ctx.advance();
			continue;
		}

		if (ctx.looking_at("\"") && !squoted) {
			quoted = !quoted;
			ctx.advance();
			continue;
		}

		if (ctx.looking_at("'") && !quoted) {
			squoted = !squoted;
			ctx.advance();
			continue;
		}

		if (ctx.looking_at("\n")) // strings don't span lines; this guards
			quoted = squoted = false; // against unterminated quotes

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
	constexpr auto nosep_tags = std::to_array<const char*>({
		"b", "em", "i", "s", "strike", "tt", "u"
	});
	return !seq_some(nosep_tags, [&](auto&& t){return matches(tagname, t);});
}

static bool // do we need to skip the element completely?
is_skip_element(std::string_view tagname)
{
	constexpr auto skip_tags = std::to_array<const char*>({
		"head", "title"
	});
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

// skip the whole element, until (and over) its matching end-tag. As a guard
// against a missing end-tag, stop before an opening <body>, so we cannot
// swallow the message text itself.
static void
skip_element(Context& ctx, std::string_view tagname)
{
	const auto end_tag_str = mu_format("</{}>", tagname);
	const std::string_view end_tag{end_tag_str};

	while (!ctx.done()) {
		if (ctx.looking_at(end_tag)) {
			ctx.advance(end_tag.size());
			return;
		}
		if (ctx.looking_at("<body"))
			return; // leave it to tag()
		ctx.advance();
	}
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
	if (matches(tagname, "script") || matches(tagname, "style")) {
		skip_script_style(ctx, tagname);
		ctx.raw_scraped() += ' ';
		return;
	} else if (is_skip_element(tagname)) {
		skip_element(ctx, tagname);
		ctx.raw_scraped() += ' ';
		return;
	}

	const auto needs_sepa = needs_separator(tagname);
	while (!ctx.done()) {

		if (ctx.looking_at("\"")) {
			ctx.advance();
			skip_quoted(ctx, "\"");
			continue;
		}

		if (ctx.looking_at("'")) {
			ctx.advance();
			skip_quoted(ctx, "'");
			continue;
		}

		if (ctx.looking_at(">")) {
			ctx.advance();
			if (needs_sepa)
				ctx.raw_scraped() += ' ';
			return;
		}
		ctx.advance();
	}
}

// handle an html-entity; pos must be just after the '&'. If it does not
// look like an entity, keep the '&' as literal text.
static void
html_escape_char(Context& ctx)
{
	// accented characters are added unaccented, lowercase, since that's
	// what we do for indexing anyway; this handles the common
	// "<base-char><accent-name>" entities, e.g. &aacute; and &Ocirc;
	constexpr auto accents = std::to_array<const char*>({
		"acute",
		"breve",
		"caron",
		"circ",
		"grave",
		"horn",
		"macr",
		"slash",
		"strok",
		"tilde",
		"uml",
	});

	struct Named { std::string_view name; std::string_view repl; };
	constexpr auto named = std::to_array<Named>({
		{"amp",	  "&"},
		{"apos",  "'"},
		{"gt",	  ">"},
		{"ldquo", "\""},
		{"lsquo", "'"},
		{"lt",	  "<"},
		{"mdash", "-"},
		{"nbsp",  " "},
		{"ndash", "-"},
		{"quot",  "\""},
		{"rdquo", "\""},
		{"rsquo", "'"},
	});

	const auto unescape=[&](std::string_view esc)->std::string {

		if (esc.front() == '#') { // numeric entity, e.g. &#233; or &#xe9;
			auto num{esc.substr(1)};
			int base{10};
			if (!num.empty() && to_ascii_lower(num.front()) == 'x') {
				num = num.substr(1);
				base = 16;
			}
			gunichar uc{};
			const auto [ptr, ec] = std::from_chars(
				num.data(), num.data() + num.size(), uc, base);
			if (ec == std::errc{} && ptr == num.data() + num.size() &&
			    uc >= 0x20 && g_unichar_validate(uc)) {
				char buf[6];
				return {buf, static_cast<size_t>(
						g_unichar_to_utf8(uc, buf))};
			}
			return " ";
		}

		for (auto&& n: named)
			if (matches(esc, n.name))
				return std::string{n.repl};

		if (seq_some(accents, [&](auto&& a){
			return starts_with(esc.substr(1), a);}))
			return std::string(1, to_ascii_lower(esc.front()));

		return " ";
	};

	// find the terminating ';'; entities are short and consist of
	// alphanumerics or '#'; anything else is a bare '&'.
	constexpr size_t max_entity_len{10};
	const auto start_pos{ctx.position()};
	Option<std::string_view> entity;
	for (size_t len{}; len <= max_entity_len &&
		     start_pos + len < ctx.size(); ++len) {
		const auto c{ctx.html()[start_pos + len]};
		if (c == ';') {
			if (len != 0)
				entity = ctx.substr(start_pos, len);
			break;
		}
		if (!is_ascii_alnum(c) && c != '#')
			break;
	}

	if (!entity) {
		ctx.raw_scraped() += '&';
		return;
	}

	ctx.raw_scraped() += unescape(*entity);
	ctx.advance(entity->size() + 1); // the entity and its ';'
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

std::string
Mu::html_to_text(const std::string& html)
{
	Context ctx{html};

	text(ctx);

	return ctx.scraped();
}

#ifdef BUILD_TESTS
#include "mu-test-utils.hh"

static void
test_basics()
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{ "<!-- Hello -->A",  "A"   },
			{ "A<!-- Test -->B", "A B"  },
			{ "A<i>a</i><b>p</b>", "Aap"},
			{ "N&oacute;&Ocirc;t", "Noot"},
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
test_quoted()
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{ R"(<i>hello, <b bar="/b">world!</b>)",
			  "hello, world!"},
			// '>' inside a quoted attribute value
			{ R"(<a href="http://example.com" title="x > y">link</a> text)",
			  "link text"},
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}


static void
test_script_style()
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{R"(<i>hello, </i><script language="javascript">
				function foo() {
					alert("Stroopwafel!"); // test
				}
			    </script>world!)",
			  "hello, world!"},
			// tags are case-insensitive
			{ "A<SCRIPT>var x = \"hello world\";</SCRIPT>B",
			  "A B"},
			// '//'-comment ending in the same line as the end-tag
			{ "A<script>// it's a comment\nvar x = 1;</script>B",
			  "A B"},
			// end-tag inside a string
			{ "A<script>var s = \"</script>\"; var t = 1;</script>B",
			  "A B"},
			{ "A<style>p::before { content: '</style>'; }</style>B",
			  "A B"},
			// '//' in css is not a comment
			{ "A<style>p { background: url(//cdn.example.com/i.png) }</style>B",
			  "A B"},
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}

static void
test_entities() // entities
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			// bare '&' is not an entity; keep the text
			{ "Tom & Jerry; forever", "Tom & Jerry; forever"},
			{ "AT&amp;T &lt;3", "AT&T <3"},
			// accents are dropped
			{ "p&aacute;gina", "pagina"},
			// numeric entities, decimal and hex
			{ "don&#39;t don&#x2019;t", "don't don’t"},
			{ "caf&#233;", "café"},
			// invalid numeric entities become a space
			{ "A&#xfffffff;B&#;C", "A B C"},
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}

static void
test_skipped() // skipped elements
{
	static std::vector<std::pair<std::string, std::string>>
		tests = {
			{ "<html><head><title>Title</title>"
			  "<meta charset=\"utf-8\"></head>"
			  "<body>Hello</body></html>",
			  "Hello"},
			// missing </head>: don't swallow the body
			{ "<html><head><meta charset=\"utf-8\">"
			  "<body>Hello</body></html>",
			  "Hello"},
		};

	for (auto&& test: tests)
		assert_equal(html_to_text(test.first), test.second);
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/html-to-text/test-basics", test_basics);
	g_test_add_func("/html-to-text/test-quoted", test_quoted);
	g_test_add_func("/html-to-text/test-script-style", test_script_style);
	g_test_add_func("/html-to-text/test-entities", test_entities);
	g_test_add_func("/html-to-text/test-skipped", test_skipped);

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
