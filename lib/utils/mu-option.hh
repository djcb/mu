/*
 * Created on 2020-11-08 by Dirk-Jan C. Binnema <dbinnema@logitech.com>
 *
 * Copyright (c) 2020 Logitech, Inc.  All Rights Reserved
 * This program is a trade secret of LOGITECH, and it is not to be reproduced,
 * published, disclosed to others, copied, adapted, distributed or displayed
 * without the prior authorization of LOGITECH.
 *
 * Licensee agrees to attach or embed this notice on all copies of the program,
 * including partial copies or modified versions thereof.
 *
 */

#ifndef MU_OPTION__
#define MU_OPTION__

#include "optional.hpp"


namespace Mu {

/// Either a value of type T, or None
template <typename T> using Option=tl::optional<T>;

template <typename T> Option<T> Some(T&& t) { return t; }
constexpr auto Nothing = tl::nullopt; // 'None' is take already

}
#endif /*MU_OPTION__*/
