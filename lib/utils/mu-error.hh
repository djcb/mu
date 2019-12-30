/*
** Copyright (C) 2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

namespace Mu {

struct Error final: public std::runtime_error {
        enum struct Code {
                AccessDenied,
                Internal,
                InvalidArgument,
                NotFound,
                SchemaMismatch,
        };

        /**
         * Construct an error
         *
         * @param codearg error-code
         * #param msgarg the error diecription
         */
        Error(Code codearg, const std::string& msgarg):
                std::runtime_error(msgarg), code_{codearg}
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
        __attribute__((format(printf, 2, 0)))
        static Error make(Code codearg, const char *frm, ...) {
                va_list args;
                va_start(args, frm);
                auto msg = format(frm, args);
                va_end(args);
                return Error(codearg, msg);
        }

        /**
         * Get the error-code for this error
         *
         * @return the error-code
         */
        Code code() const { return code_; }

private:
        const Code   code_;
};


} // namespace Mu


#endif /* MU_ERROR_HH__ */
