/*
** Copyright (C) 2022-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_MESSAGE_HH__
#define MU_MESSAGE_HH__

#include <memory>
#include <string>
#include <vector>
#include <iostream>

#include "mu-xapian-db.hh"

#include "mu-contact.hh"
#include "mu-priority.hh"
#include "mu-flags.hh"
#include "mu-fields.hh"
#include "mu-document.hh"
#include "mu-message-part.hh"
#include "mu-message-file.hh"

#include "utils/mu-utils.hh"
#include "utils/mu-option.hh"
#include "utils/mu-result.hh"
#include "utils/mu-sexp.hh"

namespace Mu {

class Message {
public:
	enum struct Options {
		None		  = 0,		/**< Defaults */
		Decrypt		  = 1 << 0,	/**< Attempt to decrypt */
		RetrieveKeys	  = 1 << 1,	/**< Auto-retrieve crypto keys (implies network
						   * access) */
		AllowRelativePath = 1 << 2,	/**< Allow relative paths for filename
						   * in make_from_path */
		SupportNgrams     = 1 << 3,     /**< Support ngrams, as used in
						 * CJK and other languages. */
	};

	/**
	 * Default CTOR; not useful by itself, but can be moved into.
	 */
	Message() noexcept;


	/**
	 * Move CTOR
	 *
	 * @param some other message
	 */
	Message(Message&& other) noexcept;

	/**
	 * operator=
	 *
	 * @param other move some object object
	 *
	 * @return
	 */
	Message& operator=(Message&& other) noexcept;

	/**
	 * Construct a message based on a path
	 *
	 * @param path path to message
	 * @param opts options
	 *
	 * @return a message or an error
	 */
	static Result<Message> make_from_path(const std::string& path,
					      Options opts={}) try {
		return Ok(Message{path,opts});
	} catch (Error& err) {
		return Err(err);
	}
	/* LCOV_EXCL_START */
	catch (...) {
		return Err(Mu::Error(Error::Code::Message,
				     "failed to create message from path"));
	}
	/* LCOV_EXCL_STOP */

	/**
	 * Construct a message based on a Xapian::Document
	 *
	 * @param doc a Mu Document
	 *
	 * @return a message or an error
	 */
	static Result<Message> make_from_document(Xapian::Document&& doc) try {
		return Ok(Message{std::move(doc)});
	} catch (Error& err) {
		return Err(err);
	}
	/* LCOV_EXCL_START */
	catch (...) {
		return Err(Mu::Error(Error::Code::Message,
				     "failed to create message from document"));
	}
	/* LCOV_EXCL_STOP */

	/**
	 * Construct a message from a string. This is mostly useful for testing.
	 *
	 * @param text message text
	 * @param path path to message - optional; path does not have to exist.
	 * @param opts options
	 *
	 * @return a message or an error
	 */
	static Result<Message> make_from_text(const std::string& text,
					      const std::string& path={},
					      Options opts={}) try {
		return Ok(Message{text, path, opts});
	} catch (Error& err) {
		return Err(err);
	}
	/* LCOV_EXCL_START */
	catch (...) {
		return Err(Mu::Error(Error::Code::Message,
				     "failed to create message from text"));
	}
	/* LCOV_EXCL_STOP */

	/**
	 * DTOR
	 */
	~Message();

	/**
	 * Get the document.
	 *
	 *
	 * @return document
	 */
	const Document& document() const;


	/**
	 * The message options for this message
	 *
	 * @return message options
	 */
	Options options() const;


	/**
	 * Get the document-id, or 0 if non-existent.
	 *
	 * @return document id
	 */
	unsigned docid() const;

	/**
	 * Get the file system path of this message
	 *
	 * @return the path of this Message or NULL in case of error.
	 * the returned string should *not* be modified or freed.
	 */
	std::string path() const { return document().string_value(Field::Id::Path); }

	/**
	 * Get the sender (From:) of this message
	 *
	 * @return the sender(s) of this Message
	 */
	Contacts from() const { return document().contacts_value(Field::Id::From); }

	/**
	 * Get the recipient(s) (To:) for this message
	 *
	 * @return recipients
	 */
	Contacts to() const { return document().contacts_value(Field::Id::To); }

	/**
	 * Get the recipient(s) (Cc:) for this message
	 *
	 * @return recipients
	 */
	Contacts cc() const { return document().contacts_value(Field::Id::Cc); }

	/**
	 * Get the recipient(s) (Bcc:) for this message
	 *
	 * @return recipients
	 */
	Contacts bcc() const { return document().contacts_value(Field::Id::Bcc); }

	/**
	 * Get the maildir this message resides in; i.e., if the path is
	 * ~/Maildir/foo/bar/cur/msg, the maildir would typically be foo/bar
	 *
	 * This is determined when _storing_ the message (which uses
	 * set_maildir())
	 *
	 * @return the maildir requested or empty */
	std::string maildir() const  { return document().string_value(Field::Id::Maildir); }

	/**
	 * Set the maildir for this message. This is for use by the _store_ when
	 * it has determined the maildir for this message from the message's path and
	 * the root-maildir known by the store.
	 *
	 * @param maildir the maildir for this message
	 *
	 * @return Ok() or some error if the maildir is invalid
	 */
	Result<void> set_maildir(const std::string& maildir);

	/**
	 * Clean up the maildir. This is for internal use, but exposed for testing.
	 * For now cleaned-up means "stray trailing / removed".
	 *
	 * @param maildir some maildir
	 *
	 * @return a cleaned-up version
	 */
	static std::string sanitize_maildir(const std::string& maildir);

	/**
	 * Get the subject of this message
	 *
	 * @return the subject of this Message
	 */
	std::string subject() const  { return document().string_value(Field::Id::Subject); }

	/**
	 * Get the Message-Id of this message
	 *
	 * @return the Message-Id of this message (without the enclosing <>), or
	 * a fake message-id for messages that don't have them.
	 *
	 * For file-backed message, this fake message-id is based on a hash of the
	 * message contents. For non-file-backed (test) messages, some other value
	 * is concocted.
	 */
	std::string message_id() const { return document().string_value(Field::Id::MessageId);}

	/**
	 * get the mailing list for a message, i.e. the mailing-list
	 * identifier in the List-Id header.
	 *
	 * @return the mailing list id for this message (without the enclosing <>)
	 * or NULL in case of error or if there is none.
	 */
	std::string mailing_list() const { return document().string_value(Field::Id::MailingList);}

	/**
	 * get the message date/time (the Date: field) as time_t
	 *
	 * @return message date/time or 0 in case of error or if there
	 * is no such header.
	 */
	::time_t date() const {
		return static_cast<::time_t>(document().integer_value(Field::Id::Date));
	}

	/**
	 * get the last change-time this message. For path/document-based
	 * messages this corresponds with the ctime of the underlying file; for
	 * the text-based ones (as used for testing) it is the creation time.
	 *
	 * @return last-change time or 0 if unknown
	 */
	::time_t changed() const {
		return static_cast<::time_t>(document().integer_value(Field::Id::Changed));
	}

	/**
	 * get the flags for this message.
	 *
	 * @return the file/content flags
	 */
	Flags flags() const { return document().flags_value(); }


	/**
	 * Update the flags for this message. This is useful for flags
	 * that can only be determined after the message has been created already,
	 * such as the 'personal' flag.
	 *
	 * @param flags new flags.
	 */
	void set_flags(Flags flags);

	/**
	 * get the message priority for this message. The X-Priority, X-MSMailPriority,
	 * Importance and Precedence header are checked, in that order. if no known or
	 * explicit priority is set, Priority::Id::Normal is assumed
	 *
	 * @return the message priority
	 */
	Priority priority() const { return document().priority_value(); }

	/**
	 * get the file size in bytes of this message
	 *
	 * @return the filesize
	 */
	size_t size() const { return static_cast<size_t>(document().integer_value(Field::Id::Size)); }

	/**
	 * Get the (possibly empty) list of references (consisting of both the
	 * References and In-Reply-To fields), with the oldest first and the
	 * direct parent as the last one. Note, any reference (message-id) will
	 * appear at most once, duplicates and fake-message-id (see impls) are
	 * filtered out.
	 *
	 * @return a vec with the references for this msg.
	 */
	std::vector<std::string> references() const {
		return document().string_vec_value(Field::Id::References);
	}

	/**
	 * Get the thread-id for this message. This is the message-id of the
	 * oldest-known (grand) parent, or the message-id of this message if
	 * none.
	 *
	 * @return the thread id.
	 */
	std::string thread_id() const {
		return document().string_value(Field::Id::ThreadId);
	}

	/**
	 * get the list of tags (ie., X-Label)
	 *
	 * @param msg a valid MuMsg
	 *
	 * @return a list with the tags for this msg. Don't modify/free
	 */
	std::vector<std::string> tags() const {
		return document()
			.string_vec_value(Field::Id::Tags);
	}

	/*
	 * Convert to Sexp
	 */

	/**
	 * Get the s-expression for this message. Stays valid as long as this
	 * message is.
	 *
	 * @return an Sexp representing the message.
	 */
	const Sexp& sexp() const;

	/*
	 * And some non-const message, for updating an existing
	 * message after a file-system move.
	 *
	 * @return Ok or an error.
	 */
	Result<void> update_after_move(const std::string& new_path,
				       const std::string& new_maildir,
				       Flags new_flags);
	/*
	 * Below require a file-backed message, which is a relatively slow
	 * if there isn't one already; see load_mime_message()
	 */

	/**
	 * Get the text body
	 *
	 * @return text body
	 */
	Option<std::string> body_text() const;

	/**
	 * Get the HTML body
	 *
	 * @return text body
	 */
	Option<std::string> body_html() const;

	/**
	 * Get some message-header
	 *
	 * @param header_field name of the header
	 *
	 * @return the value (UTF-8), or Nothing.
	 */
	Option<std::string> header(const std::string& header_field) const;


	/**
	 * Get all contacts for this message.
	 *
	 * @return contacts
	 */
	Contacts all_contacts() const;

	/**
	 * Get information about MIME-parts in this message.
	 *
	 * @return mime-part info.
	 */
	using Part = MessagePart;
	const std::vector<Part>& parts() const;

	/**
	 * Get the path to a cache directory for this message, which is useful
	 * for temporarily saving attachments
	 *
	 * @param index optionally, create <cache-path>/<index> instead;
	 * this is useful for having part-specific subdirectories.
	 *
	 * @return path to a (created) cache directory, or an error.
	 */
	Result<std::string> cache_path(Option<size_t> index={}) const;


	/**
	 * Load the GMime (file) message (for a database-backed message),
	 * if not already (but see @param reload).
	 *
	 * Affects cached-state only, so we still mark this as 'const'
	 *
	 * @param reload whether to force reloading (even if already)
	 *
	 * @return true if loading worked; false otherwise.
	 */
	bool load_mime_message(bool reload=false) const;

	/**
	 * Clear the GMime message.
	 *
	 * Affects cached-state only, so we still mark this as 'const'
	 */
	void unload_mime_message() const;

	/**
	 * Has a (file-base) GMime message been loaded?
	 *
	 *
	 * @return true or false
	 */
	bool has_mime_message() const;

	struct Private;

	/*
	 * Usually the make_ builders are better to create a message, but in
	 * some special cases, we need a heap-allocated message... */

	Message(Xapian::Document&& xdoc);
	Message(const std::string& path, Options opts);

private:
	Message(const std::string& str, const std::string& path, Options opt);

	std::unique_ptr<Private> priv_;

}; // Message
MU_ENABLE_BITOPS(Message::Options);

static inline auto
format_as(const Message& msg) {
	return msg.path();
}

} // Mu
#endif /* MU_MESSAGE_HH__ */
