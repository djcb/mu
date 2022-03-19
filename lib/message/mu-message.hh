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

struct _GMimeMessage;

namespace Mu {

class Message {
public:
	/**
	 * Construct a message based on a path
	 *
	 * @param path path to message
	 * @param mdir the maildir for this message; ie, if the path is
	 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar; you can
	 * pass NULL for this parameter, in which case some maildir-specific
	 * information is not available.
	 */
	Message(const std::string& path, const std::string& mdir);

	/**
	 * Construct a message based on a Message::Document
	 *
	 * @param doc
	 */
	Message(Document& doc): doc_{doc} {}

	/**
	 * Copy CTOR
	 *
	 * @param rhs a Message
	 */
	Message(const Message& rhs) {
		*this = rhs;
	}

	/**
	 * Move CTOR
	 *
	 * @param rhs a Message
	 */

	Message(Message&& rhs) {
		*this = std::move(rhs);
	}

	/**
	 * DTOR
	 *
	 */
	~Message();
/**
	 * Copy assignment operator
	 *
	 * @param rhs some message
	 *
	 * @return a message ref
	 */
	Message& operator=(const Message& rhs);

	/**
	 * Move assignment operator
	 *
	 * @param rhs some message
	 *
	 * @return a message ref
	 */
	Message& operator=(Message&& rhs);

	/**
	 * Get the document.
	 *
	 *
	 * @return document
	 */
	const Document& document() const { return doc_; }

	/**
	 * Get the file system path of this message
	 *
	 * @return the path of this Message or NULL in case of error.
	 * the returned string should *not* be modified or freed.
	 */
	std::string path() const { return doc_.string_value(Field::Id::Path); }

	/**
	 * Get the sender (From:) of this message
	 *
	 * @return the sender(s) of this Message
	 */
	Contacts from() const { return doc_.contacts_value(Field::Id::From); }

	/**
	 * Get the recipient(s) (To:) for this message
	 *
	 * @return recipients
	 */
	Contacts to() const { return doc_.contacts_value(Field::Id::To); }

	/**
	 * Get the recipient(s) (Cc:) for this message
	 *
	 * @return recipients
	 */
	Contacts cc() const { return doc_.contacts_value(Field::Id::Cc); }


	/**
	 * Get the recipient(s) (Bcc:) for this message
	 *
	 * @return recipients
	 */
	Contacts bcc() const { return doc_.contacts_value(Field::Id::Bcc); }

	/**
	 * Get the maildir this message lives in; ie, if the path is
	 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar
	 *
	 * @return the maildir requested or empty */
	std::string maildir() const  { return doc_.string_value(Field::Id::Maildir); }

	/**
	 * Get the subject of this message
	 *
	 * @return the subject of this Message
	 */
	std::string subject() const  { return doc_.string_value(Field::Id::Subject); }

	/**
	 * Get the Message-Id of this message
	 *
	 * @return the Message-Id of this message (without the enclosing <>), or
	 * a fake message-id for messages that don't have them
	 */
	std::string message_id() const { return doc_.string_value(Field::Id::MessageId);}

	/**
	 * get the mailing list for a message, i.e. the mailing-list
	 * identifier in the List-Id header.
	 *
	 * @return the mailing list id for this message (without the enclosing <>)
	 * or NULL in case of error or if there is none.
	 */
	std::string mailing_list() const { return doc_.string_value(Field::Id::MailingList);}

	/**
	 * get the message date/time (the Date: field) as time_t, using UTC
	 *
	 * @return message date/time or 0 in case of error or if there
	 * is no such header.
	 */
	time_t date() const { return static_cast<time_t>(doc_.integer_value(Field::Id::Date)); }

	/**
	 * get the flags for this message
	 *
	 * @return the file/content flags
	 */
	Flags flags() const { return doc_.flags_value(); }

	/**
	 * get the message priority for this message. The X-Priority, X-MSMailPriority,
	 * Importance and Precedence header are checked, in that order. if no known or
	 * explicit priority is set, Priority::Id::Normal is assumed
	 *
	 * @return the message priority
	 */
	Priority priority() const { return doc_.priority_value(); }

	/**
	 * get the file size in bytes of this message
	 *
	 * @return the filesize
	 */
	size_t size() const { return static_cast<size_t>(doc_.integer_value(Field::Id::Size)); }

	/**
	 * get the list of references (consisting of both the References and
	 * In-Reply-To fields), with the oldest first and the direct parent as
	 * the last one. Note, any reference (message-id) will appear at most
	 * once, duplicates are filtered out.
	 *
	 * @return a vec with the references for this msg.
	 */
	std::vector<std::string> references() const {
		return doc_.string_vec_value(Field::Id::References);
	}

	/**
	 * get the list of tags (ie., X-Label)
	 *
	 * @param msg a valid MuMsg
	 *
	 * @return a list with the tags for this msg. Don't modify/free
	 */
	std::vector<std::string> tags() const {
		return doc_.string_vec_value(Field::Id::References);
	}

	/**
	 * Get some message-header
	 *
	 * @param header_field name of the header
	 *
	 * @return the value
	 */
	std::string header(const std::string& header_field) const;

private:
	Document doc_;
	mutable struct _GMimeMessage *mime_msg_{};

}; // Message
} // Mu
#endif /* MU_MESSAGE_HH__ */
