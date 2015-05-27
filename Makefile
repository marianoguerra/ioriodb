REBAR = $(shell pwd)/rebar
.PHONY: deps

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

refetchdeps: distclean deps

clean:
	$(REBAR) clean

distclean: relclean devclean
	$(REBAR) delete-deps

rel: all
	$(REBAR) generate

relclean: clean
	rm -rf rel/iorio

devrelclean:
	rm -rf dev

dumpbuild:
	echo "build by $(USER) at $(shell date -u +%Y-%m-%dT%H-%M-%S) on $(shell uname -a).\nCommit Hash: $(shell git log -n 1 --pretty=format:"%H")" >> ./rel/iorio/build.txt

newrel: relclean rel dumpbuild

console:
	./rel/iorio/bin/iorio console

newdevrel: devrelclean devrel

xref: all
	$(REBAR) skip_deps=true xref

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/iorio/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/iorio/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/iorio/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/iorio/lib;)


##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev
