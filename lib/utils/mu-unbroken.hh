// borrowed from Xapian; slightly adapted

/* Copyright (c) 2007, 2008 Yung-chung Lin (henearkrxern@gmail.com)
 * Copyright (c) 2011 Richard Boulton (richard@tartarus.org)
 * Copyright (c) 2011 Brandon Schaefer (brandontschaefer@gmail.com)
 * Copyright (c) 2011,2018,2019,2023 Olly Betts
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef MU_UNBROKEN_HH__
#define MU_UNBROKEN_HH__

#include <algorithm>
#include <iterator>

/**
 * Does unichar p belong to a script without explicit word separators?
 *
 * @param p
 *
 * @return true or false
 */
static inline bool
is_unbroken_script(unsigned p)
{
	// Array containing the last value in each range of codepoints which
	// are either all in scripts which are written without explicit word
	// breaks, or all not in such scripts.
	//
	// We only include scripts here which ICU has dictionaries for.	 The
	// same list is currently also used to decide which languages to do
	// ngrams for, though perhaps that should use a separate list.
	constexpr unsigned splits[] = {
		// 0E00..0E7F; Thai, Lanna Tai, Pali
		// 0E80..0EFF; Lao
		0x0E00 - 1, 0x0EFF,
		// 1000..109F; Myanmar (Burmese)
		0x1000 - 1, 0x109F,
		// 1100..11FF; Hangul Jamo
		0x1100 - 1, 0x11FF,
		// 1780..17FF; Khmer
		0x1780 - 1, 0x17FF,
		// 19E0..19FF; Khmer Symbols
		0x19E0 - 1, 0x19FF,
		// 2E80..2EFF; CJK Radicals Supplement
		// 2F00..2FDF; Kangxi Radicals
		// 2FE0..2FFF; Ideographic Description Characters
		// 3000..303F; CJK Symbols and Punctuation
		// 3040..309F; Hiragana
		// 30A0..30FF; Katakana
		// 3100..312F; Bopomofo
		// 3130..318F; Hangul Compatibility Jamo
		// 3190..319F; Kanbun
		// 31A0..31BF; Bopomofo Extended
		// 31C0..31EF; CJK Strokes
		// 31F0..31FF; Katakana Phonetic Extensions
		// 3200..32FF; Enclosed CJK Letters and Months
		// 3300..33FF; CJK Compatibility
		// 3400..4DBF; CJK Unified Ideographs Extension A
		// 4DC0..4DFF; Yijing Hexagram Symbols
		// 4E00..9FFF; CJK Unified Ideographs
		0x2E80 - 1, 0x9FFF,
		// A700..A71F; Modifier Tone Letters
		0xA700 - 1, 0xA71F,
		// A960..A97F; Hangul Jamo Extended-A
		0xA960 - 1, 0xA97F,
		// A9E0..A9FF; Myanmar Extended-B (Burmese)
		0xA9E0 - 1, 0xA9FF,
		// AA60..AA7F; Myanmar Extended-A (Burmese)
		0xAA60 - 1, 0xAA7F,
		// AC00..D7AF; Hangul Syllables
		// D7B0..D7FF; Hangul Jamo Extended-B
		0xAC00 - 1, 0xD7FF,
		// F900..FAFF; CJK Compatibility Ideographs
		0xF900 - 1, 0xFAFF,
		// FE30..FE4F; CJK Compatibility Forms
		0xFE30 - 1, 0xFE4F,
		// FF00..FFEF; Halfwidth and Fullwidth Forms
		0xFF00 - 1, 0xFFEF,
		// 1AFF0..1AFFF; Kana Extended-B
		// 1B000..1B0FF; Kana Supplement
		// 1B100..1B12F; Kana Extended-A
		// 1B130..1B16F; Small Kana Extension
		0x1AFF0 - 1, 0x1B16F,
		// 1F200..1F2FF; Enclosed Ideographic Supplement
		0x1F200 - 1, 0x1F2FF,
		// 20000..2A6DF; CJK Unified Ideographs Extension B
		0x20000 - 1, 0x2A6DF,
		// 2A700..2B73F; CJK Unified Ideographs Extension C
		// 2B740..2B81F; CJK Unified Ideographs Extension D
		// 2B820..2CEAF; CJK Unified Ideographs Extension E
		// 2CEB0..2EBEF; CJK Unified Ideographs Extension F
		0x2A700 - 1, 0x2EBEF,
		// 2F800..2FA1F; CJK Compatibility Ideographs Supplement
		0x2F800 - 1, 0x2FA1F,
		// 30000..3134F; CJK Unified Ideographs Extension G
		// 31350..323AF; CJK Unified Ideographs Extension H
		0x30000 - 1, 0x323AF
	};
	// Binary chop to find the first entry which is >= p.  If it's an odd
	// offset then the codepoint is in a script which needs splitting; if it's
	// an even offset then it's not.
	auto it = std::lower_bound(std::begin(splits),
				   std::end(splits), p);

	return ((it - splits) & 1);
}


#endif /* MU_UNBROKEN_HH__ */
