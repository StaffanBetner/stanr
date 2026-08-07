# Reusable recipe for building the stanr precompiled model header.
.PHONY: pch

pch:
	$(CXX20) $(CXX20STD) $(ALL_CPPFLAGS) $(CXX20FLAGS) $(CXX20PICFLAGS) -x c++-header "$(HEADER)" -o "$(PCH)" $(EXTRA_CXXFLAGS)
