# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

REBAR?=rebar


.PHONY: all
# target: all - Makes everything
all: build


.PHONY: build
# target: build - Builds the project
build:
	$(REBAR) compile


.PHONY: check
# target: check - Checks if project builds and passes all the tests
check: build eunit


.PHONY: clean
# target: clean - Removes build artifacts
clean:
	$(REBAR) clean
	rm -f test/*.beam


.PHONY: distclean
# target: distclean - Removes all unversioned files
distclean: clean
	git clean -fxd


.PHONY: eunit
# target: eunit - Runs eunit test suite
eunit:
	$(REBAR) eunit


.PHONY: help
# target: help - Prints this help
help:
	@egrep "^# target:" Makefile | sed -e 's/^# target: //g' | sort


%.beam: %.erl
	erlc -o test/ $<
