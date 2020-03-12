/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_STORE_HH__
#define __MU_STORE_HH__

#include <mu-msg.h>

#ifdef __cplusplus

#include "mu-contacts.hh"

#include <xapian.h>

#include <string>
#include <vector>
#include <ctime>

#include <utils/mu-utils.hh>

namespace Mu {

class Store {
public:
        /**
         * Construct a store for an existing document database
         *
         * @param path path to the database
         * @param readonly whether to open the database in read-only mode
         */
        Store (const std::string& path, bool readonly=true);

        /**
         * Construct a store for a not-yet-existing document database
         *
         * @param path path to the database
         * @param maildir maildir to use for this store
         * @param personal_addressesaddresses that should be recognized as
         * 'personal' for identifying personal messages.
         */
        Store (const std::string& path, const std::string& maildir,
               const StringVec& personal_addresses);

        /**
         * DTOR
         */
        ~Store();

        /**
         * Is the store read-only?
         *
         * @return true or false
         */
        bool read_only() const;

        /**
         * Path to the database; this is some subdirectory of the path
         * passed to the constructor.
         *
         * @return the database path
         */
        const std::string& database_path() const;

        /**
         * Path to the top-level Maildir
         *
         * @return the maildir
         */
        const std::string& root_maildir() const;

        /**
         * Version of the database-schema
         *
         * @return the maildir
         */
        const std::string& schema_version() const;


        /**
         * Time of creation of the store
         *
         * @return creation time
         */
        std::time_t created() const;

        /**
         * Get a vec with the personal addresses
         *
         * @return personal addresses
         */
        const StringVec& personal_addresses() const;

        /**
         * Get the Contacts object for this store
         *
         * @return the Contacts object
         */
        const Contacts& contacts() const;

        /**
         * Add a message to the store.
         *
         * @param path the message path.
         *
         * @return the doc id of the added message
         */
        unsigned add_message (const std::string& path);

        /**
         * Add a message to the store.
         *
         * @param path the message path.
         *
         * @return true if removing happened; false otherwise.
         */
        bool remove_message (const std::string& path);

        /**
         * Fina  message in the store.
         *
         * @param docid doc id for the message to find
         *
         * @return a message (owned by caller), or nullptr
         */
        MuMsg* find_message (unsigned docid) const;

        /**
         * does a certain message exist in the store already?
         *
         * @param path the message path
         *
         * @return true if the message exists in the store, false otherwise
         */
        bool contains_message (const std::string& path) const;

        /**
         * Get the timestamp for some directory
         *
         * @param path the path
         *
         * @return the timestamp, or 0 if not found
         */
        time_t dirstamp (const std::string& path) const;

        /**
         * Set the timestamp for some directory
         *
         * @param path a filesystem path
         * @param tstamp the timestamp for that path
         */
        void set_dirstamp (const std::string& path, time_t tstamp);

        /**
         * Get the number of documents in the document database
         *
         * @return the number
         */
        std::size_t size() const;

        /**
         * Is the database empty?
         *
         * @return true or false
         */
        bool empty() const;

        /**
         * Begin a database transaction
         */
        void begin_transaction();

        /**
         * Commit a database transaction
         *
         */
        void commit_transaction();

        /**
         * Are we in a transaction?
         *
         * @return true or false
         */
        bool in_transaction() const;


        /**
         * Get a reference to the private data. For internal use.
         *
         * @return private reference.
         */
        struct                   Private;
        std::unique_ptr<Private>&       priv()       { return priv_; }
        const std::unique_ptr<Private>& priv() const { return priv_; }

private:
        std::unique_ptr<Private> priv_;
};

} // namespace Mu


#endif /*__cplusplus*/

#include <glib.h>
#include <inttypes.h>
#include <utils/mu-util.h>
#include <mu-contacts.hh>

G_BEGIN_DECLS

struct MuStore_;
typedef struct MuStore_ MuStore;

/* http://article.gmane.org/gmane.comp.search.xapian.general/3656 */
#define MU_STORE_MAX_TERM_LENGTH (240)


/**
 * create a new read-only Xapian store, for querying documents
 *
 * @param path the path to the database
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return a new MuStore object with ref count == 1, or NULL in case of error;
 * free with mu_store_unref
 */
MuStore* mu_store_new_readable (const char* xpath, GError **err)
		   G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;
/**
 * create a new writable Xapian store, a place to store documents
 *
 * @param path the path to the database
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return a new MuStore object with ref count == 1, or NULL in case
 * of error; free with mu_store_unref
 */
MuStore*  mu_store_new_writable (const char *xpath, GError **err)
        G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * create a new writable Xapian store, a place to store documents, and
 * create/overwrite the existing database.
 *
 * @param path the path to the database
 * @param path to the maildir
 * @param personal_addressesaddresses that should be recognized as
 * 'personal' for identifying personal messages.
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return a new MuStore object with ref count == 1, or NULL in case
 * of error; free with mu_store_unref
 */
MuStore*  mu_store_new_create (const char *xpath, const char *maildir,
                               const char **personal_addresses, GError **err)
        G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * increase the reference count for this store with 1
 *
 * @param store a valid store object
 *
 * @return the same store with increased ref count, or NULL in case of
 * error
 */
MuStore* mu_store_ref (MuStore *store);

/**
 * decrease the reference count for this store with 1
 *
 * @param store a valid store object
 *
 * @return NULL
 */
MuStore* mu_store_unref (MuStore *store);


/**
 * we need this when using Xapian::(Writable)Database* from C
 */
typedef gpointer XapianWritableDatabase;
typedef gpointer XapianDatabase;


/**
 * get the underlying writable database object for this store; not
 * that this pointer becomes in valid after mu_store_destroy
 *
 * @param store a valid store
 *
 * @return a Xapian::WritableDatabase (you'll need to cast in C++), or
 * NULL in case of error.
 */
XapianWritableDatabase* mu_store_get_writable_database (MuStore *store);


/**
 * get the underlying read-only database object for this store; not that this
 * pointer becomes in valid after mu_store_destroy
 *
 * @param store a valid store
 *
 * @return a Xapian::Database (you'll need to cast in C++), or
 * NULL in case of error.
 */
XapianDatabase* mu_store_get_read_only_database (MuStore *store);

/**
 * get the version of the xapian database (ie., the version of the
 * 'schema' we are using). If this version != MU_STORE_SCHEMA_VERSION,
 * it's means we need to a full reindex.
 *
 * @param store the store to inspect
 *
 * @return the version of the database as a newly allocated string
 * (free with g_free); if there is no version yet, it will return NULL
 */
const char* mu_store_schema_version (const MuStore* store);


/**
 * Get the database-path for this message store
 *
 * @param store the store to inspetc
 *
 * @return the database-path
 */
const char *mu_store_database_path (const MuStore *store);


/**
 * Get the root-maildir for this message store.
 *
 * @param store the store
 *
 * @return the maildir.
 */
const char *mu_store_root_maildir(const MuStore *store);


/**
 * Get the time this database was created
 *
 * @param store the store
 *
 * @return the maildir.
 */
time_t mu_store_created(const MuStore *store);

/**
 * Get the list of personal addresses from the store
 *
 * @param store the message store
 *
 * @return the list of personal addresses, or NULL in case of error.
 *
 * Free with g_strfreev().
 */
char** mu_store_personal_addresses (const MuStore *store);

/**
 * Get the a MuContacts* ptr for this store.
 *
 * @param store a store
 *
 * @return the contacts ptr
 */
const MuContacts* mu_store_contacts (MuStore *store);


/**
 * get the numbers of documents in the database
 *
 * @param index a valid MuStore instance
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return the number of documents in the database; (unsigned)-1 in
 * case of error
 */
unsigned mu_store_count (const MuStore *store, GError **err);


/**
 * try to flush/commit all outstanding work to the database and the contacts
 * cache.
 *
 * @param store a valid xapian store
 */
void mu_store_flush (MuStore *store);

#define MU_STORE_INVALID_DOCID 0

/**
 * store an email message in the XapianStore
 *
 * @param store a valid store
 * @param msg a valid message
 * @param err receives error information, if any, or NULL
 *
 * @return the docid of the stored message, or 0
 * (MU_STORE_INVALID_DOCID) in case of error
 */
unsigned mu_store_add_msg   (MuStore *store, MuMsg *msg, GError **err);


/**
 * update an email message in the XapianStore
 *
 * @param store a valid store
 * @param the docid for the message
 * @param msg a valid message
 * @param err receives error information, if any, or NULL
 *
 * @return the docid of the stored message, or 0
 * (MU_STORE_INVALID_DOCID) in case of error
 */
unsigned mu_store_update_msg (MuStore *store, unsigned docid, MuMsg *msg,
			      GError **err);

/**
 * store an email message in the XapianStore; similar to
 * mu_store_store, but instead takes a path as parameter instead of a
 * MuMsg*
 *
 * @param store a valid store
 * @param path full filesystem path to a valid message
 * @param err receives error information, if any, or NULL
 *
 * @return the docid of the stored message, or 0
 * (MU_STORE_INVALID_DOCID) in case of error
 */
unsigned mu_store_add_path (MuStore *store, const char *path, GError **err);

/**
 * remove a message from the database based on its path
 *
 * @param store a valid store
 * @param msgpath path of the message (note, this is only used to
 * *identify* the message; a common use of this function is to remove
 * a message from the database, for which there is no message anymore
 * in the filesystem.
 *
 * @return TRUE if it succeeded, FALSE otherwise
 */
gboolean mu_store_remove_path (MuStore *store, const char* msgpath);

/**
 * does a certain message exist in the database already?
 *
 * @param store a store
 * @param path the message path
 *
 * @return TRUE if the message exists, FALSE otherwise
 */
gboolean mu_store_contains_message (const MuStore *store,  const char* path);

/**
 * get the docid for message at path
 *
 * @param store a store
 * @param path the message path
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return the docid if the message was found, MU_STORE_INVALID_DOCID (0) otherwise
 * */
unsigned mu_store_get_docid_for_path (const MuStore *store, const char* path,
                                      GError **err);

/**
 * store a timestamp for a directory
 *
 * @param store a valid store
 * @param dirpath path to some directory
 * @param stamp a timestamp
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return TRUE if setting the timestamp succeeded, FALSE otherwise
 */
gboolean mu_store_set_dirstamp (MuStore *store, const char* dirpath,
				 time_t stamp, GError **err);

/**
 * get the timestamp for a directory
 *
 * @param store a valid store
 * @param msgpath path to some directory
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return the timestamp, or 0 in case of error
 */
time_t mu_store_get_dirstamp (const MuStore *store, const char* dirpath,
			       GError **err);

/**
 * check whether this store is read-only
 *
 * @param store a store
 *
 * @return TRUE if the store is read-only, FALSE otherwise (and in
 * case of error)
 */
gboolean mu_store_is_read_only (const MuStore *store);

/**
 * call a function for each document in the database
 *
 * @param self a valid store
 * @param func a callback function to to call for each document
 * @param user_data a user pointer passed to the callback function
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return MU_OK if all went well, MU_STOP if the foreach was interrupted,
 * MU_ERROR in case of error
 */
typedef MuError (*MuStoreForeachFunc) (const char* path, gpointer user_data);
MuError  mu_store_foreach (MuStore *self, MuStoreForeachFunc func,
			   void *user_data, GError **err);

/**
 * check if the database is locked for writing
 *
 * @param xpath path to a xapian database
 *
 * @return TRUE if it is locked, FALSE otherwise (or in case of error)
 */
gboolean mu_store_database_is_locked (const gchar *xpath);

/**
 * get a specific message, based on its Xapian docid
 *
 * @param self a valid MuQuery instance
 * @param docid the Xapian docid for the wanted message
 * @param err receives error information, or NULL
 *
 * @return a MuMsg instance (use mu_msg_unref when done with it), or
 * NULL in case of error
 */
MuMsg* mu_store_get_msg (const MuStore *self, unsigned docid, GError **err)
	G_GNUC_WARN_UNUSED_RESULT;

/**
 * Print some information about the store
 *
 * @param store a store
 * @param nocolor whether to _not_ show color
 */
void mu_store_print_info  (const MuStore *store, gboolean nocolor);


G_END_DECLS

#endif /* __MU_STORE_HH__ */
