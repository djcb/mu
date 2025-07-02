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

#include "mu-scm-types.hh"
#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"
#include <mutex>
#include <cstdio>

using namespace Mu;
using namespace Mu::Scm;

namespace {
static SCM mime_part_type;
static scm_t_port_type *mime_stream_port_type;
}

static GMimeStream*
from_scm_port(SCM port)
{
	return GMIME_STREAM(reinterpret_cast<GMimeStream*>(SCM_STREAM(port)));
}

static size_t
mime_stream_read(SCM port, SCM dst, size_t start, size_t count)
{
	auto stream{from_scm_port(port)};
	const auto res = g_mime_stream_read(stream,
					    reinterpret_cast<char*>(SCM_BYTEVECTOR_CONTENTS (dst) + start),
					    count);
	if (res < 0)
		scm_misc_error("mime-object-read", "failed to read stream",
			       SCM_BOOL_F);

	return static_cast<size_t>(res);
}

static scm_t_off
mime_stream_seek(SCM port, scm_t_off offset, int whence)
{
	auto stream{from_scm_port(port)};
	const auto gwhence = [](int w) {
		switch(w) {
		case SEEK_SET: return GMIME_STREAM_SEEK_SET;
		case SEEK_CUR: return GMIME_STREAM_SEEK_CUR;
		case SEEK_END: return GMIME_STREAM_SEEK_END;
		default:
			scm_misc_error("mime-stream-seek", "invalid whence",
				       SCM_BOOL_F);
			return GMIME_STREAM_SEEK_SET; // never reached.
		}
	}(whence);

	const auto res = g_mime_stream_seek(stream, offset, gwhence);
	if (res < 0)
		scm_misc_error("mime-stream-seek", "invalid seek",
			       SCM_BOOL_F);

	return res;
}

static scm_t_port_type*
make_mime_stream_port_type()
{
	auto ptype = scm_make_port_type(const_cast<char*>("mime-stream"), mime_stream_read, {});

	scm_set_port_close(ptype, [](SCM port){g_mime_stream_close(from_scm_port(port));});
	scm_set_port_needs_close_on_gc(ptype, true);

	scm_set_port_seek(ptype, mime_stream_seek);

	return ptype;
}

static GMimePart*
part_from_scm(SCM scm, const char *func, int pos)
{
	if (!SCM_IS_A_P(scm, mime_part_type))
		throw ScmError{ScmError::Id::WrongType, func, pos, scm, "mime-part"};

	return GMIME_PART(reinterpret_cast<GMimePart*>(scm_foreign_object_ref(scm, 0)));
}

static GMimeStream*
get_decoded_stream(GMimePart *part)
{
	auto wrapper{g_mime_part_get_content(part)};
	if (!wrapper)
		throw ScmError{"make-make-mime-stream-port",
			"failed to create data-wrapper"};

	auto stream{g_mime_stream_mem_new()};
	if (!stream)
		throw ScmError{"make-make-mime-stream-port",
			"failed to create mem-stream"};

	const auto res{g_mime_data_wrapper_write_to_stream(wrapper, stream)};
	if (res < 0) {
		g_object_unref(stream);
		throw ScmError{"make-make-mime-stream-port",
			"failed to write to stream"};
	}

	return stream;
}

static GMimeStream*
get_stream(GMimePart *part, bool content_only)
{
	auto stream = g_mime_stream_mem_new();
	if (!stream)
		throw ScmError{"make-mime-stream-port",
			"failed to create mem-stream"};

	ssize_t res{};
	if (content_only)  // content-only
		res = g_mime_object_write_content_to_stream(
			GMIME_OBJECT(part), {}, stream);
	else // with headers
		res = g_mime_object_write_to_stream(
			GMIME_OBJECT(part), {}, stream);

	if (res < 0) {
		g_object_unref(stream);
		throw ScmError{"make-mime-stream-port",
			"failed to write to stream"};
	}

	return stream;
}

/**
 * Create a port for the mime-part
 *
 * @param mime_obj mime object (foreign object)
 * @param content_only_scm whether to not include headers
 * @param decode_scm whether to decode content
 * (must be false if decode_scm is true)
 *
 * @return SCM for mime stream port
 */
static SCM
subr_make_mime_stream_port(SCM mime_part_scm, SCM content_only_scm,
			   SCM decode_scm)
{

	constexpr auto func{"make-mime-stream-port"};
	GMimeStream *stream{};
	try {
		auto part = part_from_scm(mime_part_scm, func, 1);
		const auto decode{from_scm<bool>(decode_scm,
						 func, 2)};
		const auto content_only{from_scm<bool>(content_only_scm,
						       func, 3)};
		if (decode)
			stream = get_decoded_stream(part);
		else
			stream = get_stream(part, content_only);

		if (const auto res = g_mime_stream_reset(stream); res != 0)
			throw ScmError{func, "failed to reset stream"};

		return scm_c_make_port(mime_stream_port_type, SCM_RDNG,
				       reinterpret_cast<scm_t_bits>(stream));
	} catch (const ScmError& err) {
		if (stream)
			g_object_unref(stream);
		err.throw_scm();
	}

	return SCM_UNSPECIFIED;
}


SCM
Mu::Scm::to_scm(GMimePart *part)
{
	return scm_make_foreign_object_1(mime_part_type, g_object_ref(part));
}

SCM
Mu::Scm::to_scm(size_t idx, const MessagePart& part)
{
	static SCM sym_index{make_symbol("index")};
	static SCM sym_content_type{make_symbol("content-type")};
	static SCM sym_content_description{make_symbol("content-description")};
	static SCM sym_size{make_symbol("size")};
	static SCM sym_attachment{make_symbol("attachment?")};
	static SCM sym_filename{make_symbol("filename")};
	static SCM sym_signed{make_symbol("signed?")};
	static SCM sym_encrypted{make_symbol("encrypted?")};

	SCM alist = scm_acons(sym_index, to_scm(idx), SCM_EOL);

	if (const auto ctype{part.mime_type()}; ctype)
		alist = scm_acons(sym_content_type, to_scm(*ctype), alist);
	if (const auto cdesc{part.content_description()}; cdesc)
		alist = scm_acons(sym_content_description, to_scm(*cdesc), alist);

	if (part.is_attachment())
		alist = scm_acons(sym_attachment, SCM_BOOL_T, alist);

	alist = scm_acons(sym_size, to_scm(part.size()), alist);

	if (const auto fname{part.cooked_filename(true/*minmimal*/)}; fname)
		alist = scm_acons(sym_filename, to_scm(*fname), alist);

	if (part.is_signed())
		alist = scm_acons(sym_signed, SCM_BOOL_T, alist);
	if (part.is_encrypted())
		alist = scm_acons(sym_encrypted, SCM_BOOL_T, alist);

	return scm_reverse_x(alist, SCM_EOL); // slightly more convenient
}

static void
init_subrs()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	scm_c_define_gsubr("cc-mime-make-stream-port",3/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_make_mime_stream_port));
#pragma GCC diagnostic pop
}

void
Mu::Scm::init_mime()
{
	mime_part_type = scm_make_foreign_object_type(
		make_symbol("g-mime-part"),
		scm_list_1(make_symbol("data")),
		[](SCM scm) { // finalizer
			g_object_unref(
				GMIME_PART(reinterpret_cast<GMimePart*>(
						   scm_foreign_object_ref(scm, 0))));
		});

	mime_stream_port_type = make_mime_stream_port_type();

	init_subrs();
}
