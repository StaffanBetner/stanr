// Precompiled-header seed for stanli's translation units.
//
// stan/math.hpp is the single most expensive header every stanli source
// file pays for -- ~1.9 GB and several seconds to parse even in a file
// with almost no code of its own, on top of whatever the file's own
// density/kernel instantiations cost. Narrowing to stan/math/rev.hpp
// (dropping fwd/mix, which nothing here instantiates) made no
// measurable difference: the cost is prim+rev's own size, not fwd/mix
// riding along unused. So stan/math.hpp itself is what's worth
// precompiling, and there's nothing narrower to reach for instead.
//
// This starts with the printf override rather than sitting after it in
// PKG_CPPFLAGS: GCC's automatic .gch pickup for a header silently does
// nothing if any other -include precedes it on the command line
// (measured -- falls back to a full reparse, no warning even under
// -Winvalid-pch), so src/Makevars replaces PKG_CPPFLAGS's own -include
// with this file for stanli's translation units rather than adding a
// second one after it.
//
// recorder.hpp before stan/math.hpp, not after: it specializes
// is_fvar<rvar> and friends, and stan-math's own templates only see
// those specializations if they're declared before the templates are
// parsed (see densities_impl.hpp, which established this order first).
// Baking stan/math.hpp into the PCH without it first breaks overload
// resolution for every stanli density -- caught by actually building
// through this Makevars rule rather than compiling files by hand.
#include "stan_sundials_printf_override.hpp"
#include <stanli/recorder.hpp>
#include <stan/math.hpp>
