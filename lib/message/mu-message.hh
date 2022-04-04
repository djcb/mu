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

#ifndef MU_MESSAGE_HH__
#define MU_MESSAGE_HH__

#include <memory>
#include <string>
#include <vector>
#include "mu-contact.hh"
#include "mu-priority.hh"
#include "mu-flags.hh"
#include "mu-fields.hh"
#include "mu-document.hh"
#include "mu-message-part.hh"

#include "utils/mu-option.hh"
#include "utils/mu-result.hh"
#include "utils/mu-sexp.hh"

namespace Mu {

class Message {
public:
	/**
	 * Move CTOR
	 *
	 * @param some other message
	 */
	Message(Message&& msg);

	/**
	 * Construct a message based on a path. The maildir is optional; however
	 * messages without maildir cannot be stored in the database
	 *
	 * @param path path to message
	 * @param mdir the maildir for this message; i.e, if the path is
	 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar; you can
	 * pass NULL for this parameter, in which case some maildir-specific
	 * information is not available.
	 *
	 *
	 *
	 * @return a message or an error
	 */
	static Result<Message> make_from_path(const std::string& path,
					      Option<const std::string&> mdir={}) try {
		return Ok(Message{path, mdir});
	} catch (Error& err) {
		return Err(err);
	} catch (...) {
		return Err(Mu::Error(Error::Code::Message, "failed to create message"));
	}


	/**
	 * Construct a message based on a Message::Document
	 *
	 * @param doc
	 *
	 * @return a message or an error
	 */
	static Result<Message> make_from_document(Document& doc) try {
		return Ok(Message{doc});
	} catch (Error& err) {
		return Err(err);
	} catch (...) {
		return Err(Mu::Error(Error::Code::Message, "failed to create message"));
	}

	/**
	 * Construct a message from a string. This is mostly useful for testing.
	 *
	 * @param text message text
	 *
	 * @return a message or an error
	 */
	static Result<Message> make_from_string(const std::string& text) try {
		return Ok(Message{text});
	} catch (Error& err) {
		return Err(err);
	} catch (...) {
		return Err(Mu::Error(Error::Code::Message, "failed to create message"));
	}

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
	 * Get the maildir this message lives in; ie, if the path is
	 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar
	 *
	 * @return the maildir requested or empty */
	std::string maildir() const  { return document().string_value(Field::Id::Maildir); }

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
	 * a fake message-id for messages that don't have them
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
	 * get the message date/time (the Date: field) as time_t, using UTC
	 *
	 * @return message date/time or 0 in case of error or if there
	 * is no such header.
	 */
	::time_t date() const { return static_cast<time_t>(document().integer_value(Field::Id::Date)); }

	/**
	 * get the flags for this message
	 *
	 * @return the file/content flags
	 */
	Flags flags() const { return document().flags_value(); }

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
	 * get the list of references (consisting of both the References and
	 * In-Reply-To fields), with the oldest first and the direct parent as
	 * the last one. Note, any reference (message-id) will appear at most
	 * once, duplicates are filtered out.
	 *
	 * @return a vec with the references for this msg.
	 */
	std::vector<std::string> references() const {
		return document().string_vec_value(Field::Id::References);
	}

	/**
	 * get the list of tags (ie., X-Label)
	 *
	 * @param msg a valid MuMsg
	 *
	 * @return a list with the tags for this msg. Don't modify/free
	 */
	std::vector<std::string> tags() const {
		return document().string_vec_value(Field::Id::References);
	}


	/*
	 * Convert to Sexp
	 */

	/**
	 * convert the message to a Lisp symbolic expression (for further
	 * processing in e.g. emacs)
	 *
	 *
	 * @return a Mu::Sexp or a Mu::Sexp::List representing the message.
	 */
	Mu::Sexp::List to_sexp_list() const;
	Mu::Sexp       to_sexp() const;

	/*
	 * Below require a file-backed message, which is a relatively slow
	 * if there isn't one already; see load_mime_message()
	 *
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
private:
	Message(const std::string& path, Option<const std::string&> mdir);
	Message(const std::string& str);
	Message(Document& doc);


	std::unique_ptr<Private>        priv_;
}; // Message
} // Mu
#endif /* MU_MESSAGE_HH__ */
