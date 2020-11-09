/*
** Copyright (C) 2019-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef MU_ERROR_HH__
#define MU_ERROR_HH__

#include <stdexcept>
#include "mu-utils.hh"
#include <glib.h>

namespace Mu {

struct Error final: public std::exception {

        enum struct Code {
                AccessDenied = 100, // don't overlap with MuError
                Command,
                File,
                Index,
                Internal,
                InvalidArgument,
                Message,
                NotFound,
                Parsing,
                Query,
                SchemaMismatch,
                Store,
        };

        /**
         * Construct an error
         *
         * @param codearg error-code
         * #param msgarg the error diecription
         */
        Error(Code codearg, const std::string& msgarg):
                code_{codearg}, what_{msgarg}
                {}

        /**
         * Build an error from an error-code and a format string
         *
         * @param code error-code
         * @param frm format string
         * @param ... format parameters
         *
         * @return an Error object
         */
        __attribute__((format(printf, 3, 0)))
        Error(Code codearg, const char *frm, ...): code_{codearg} {
                va_list args;
                va_start(args, frm);
                what_ = format(frm, args);
                va_end(args);
        }

        Error(Error&& rhs)      = default;
        Error(const Error& rhs) = delete;

        /**
         * Build an error from a GError an error-code and a format string
         *
         * @param code error-code
         * @param gerr a GError or {}, which is consumed
         * @param frm format string
         * @param ... format parameters
         *
         * @return an Error object
         */
        __attribute__((format(printf, 4, 0)))
        Error(Code codearg, GError **err, const char *frm, ...): code_{codearg} {

                va_list args;
                va_start(args, frm);
                what_ = format(frm, args);
                va_end(args);

                if (err && *err)
                        what_ += format (": %s", (*err)->message);
                else
                        what_ += ": something went wrong";

                g_clear_error(err);
        }

        /**
         * DTOR
         *
         */
        virtual ~Error() = default;

        /**
         * Get the descriptiove message.
         *
         * @return
         */
        virtual const char* what() const noexcept override {
                return what_.c_str();
        }

        /**
         * Get the error-code for this error
         *
         * @return the error-code
         */
        Code code() const { return code_; }



private:
        const Code   code_;
        std::string  what_;
};

} // namespace Mu

#endif /* MU_ERROR_HH__ */
