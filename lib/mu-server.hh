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

#ifndef MU_SERVER_HH__
#define MU_SERVER_HH__

#include <memory>
#include <functional>

#include <utils/mu-sexp.hh>
#include <utils/mu-utils.hh>
#include <mu-store.hh>

namespace Mu {

/**
 * @brief Implements the mu	server, as used by mu4e.
 *
 */
class Server {
public:
	enum struct OutputFlags {
		None	  = 0,
		SplitList = 1 << 0,
		/**< insert newlines between list items */
		Flush	  = 1 << 1,
		/**< flush output buffer after */
	};

	/**
	 * Prototype for output function
	 *
	 * @param sexp an s-expression
	 * @param flags flags that influence the behavior
	 */
	using Output = std::function<void(const Sexp& sexp, OutputFlags flags)>;

	/**
	 * Construct a new server
	 *
	 * @param store a message store object
	 * @param output callable for the server responses.
	 */
	Server(Store& store, Output output);

	/**
	 * DTOR
	 */
	~Server();

	/**
	 * Invoke a call on the server.
	 *
	 * @param expr the s-expression to call
	 *
	 * @return true if we the server is still ready for more
	 * calls, false when it should quit.
	 */
	bool invoke(const std::string& expr) noexcept;

private:
	struct Private;
	std::unique_ptr<Private> priv_;
};
MU_ENABLE_BITOPS(Server::OutputFlags);

} // namespace Mu


#endif /* MU_SERVER_HH__ */
