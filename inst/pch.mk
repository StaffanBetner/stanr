# Reusable recipe for building the stanr precompiled model header.
#
# Invoked from R with `make -f <this file> <target>` while the `MAKEFILES`
# environment variable points at R's `Makeconf`, so $(CXX20), $(CXX20STD),
# $(ALL_CPPFLAGS), $(CXX20FLAGS) and $(CXX20PICFLAGS) are all defined.
# HEADER, PCH, PKG_CPPFLAGS and EXTRA_CXXFLAGS are passed on the command
# line by the caller.

.PHONY: pch

pch:
	$(CXX20) $(CXX20STD) $(ALL_CPPFLAGS) $(CXX20FLAGS) $(CXX20PICFLAGS) -x c++-header "$(HEADER)" -o "$(PCH)" $(EXTRA_CXXFLAGS)
