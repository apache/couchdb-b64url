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

REBAR?=rebar3

VERSION = $(shell git describe --tags --always --first-parent)

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


.PHONY: dist
# target: dist - Generate source release
# A little bit of explanation for the untar + sed + tar dance below.
# During development we dynamically generate the vsn in the .app file
# using {vsn, git}. We can't do that in the actual source release
# because it's not a git repo, so we inject the version string into the
# archived app.src file and then repackage the archive for shipping.
dist:
	@git archive --prefix=b64url-${VERSION}/ HEAD | tar xf -
	@sed --in-place -e "s/{vsn, git}/{vsn, \"${VERSION}\"}/" b64url-${VERSION}/src/b64url.app.src
	@tar czf b64url-${VERSION}.tar.gz b64url-${VERSION}


.PHONY: clean
# target: clean - Removes build artifacts
clean:
	$(REBAR) clean


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
