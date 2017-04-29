
EMACS ?= emacs
EMACS_FLAGS ?= --batch -Q --directory .

DISTRIB_FILES = twittering-mode.el \
		README README.ja \
		NEWS NEWS.ja \
		INSTALL INSTALL.ja \
		win-curl

SRC := $(wildcard *.el)
OUT := $(SRC:.el=.elc)

.PHONY: all check clean compile update-po release release-upload upload

all: clean compile check

update-po:
	$(MAKE) -C doc update-po

check:
	$(EMACS) $(EMACS_FLAGS) \
	         --load test/vendor/test.el \
	         --load test/el-test-runner.el

clean:
	rm -f twittering-mode.elc README *.zip *.tar.gz

%.elc: %.el
	$(EMACS) $(EMACS_FLAGS) -f batch-byte-compile $<

compile: $(OUT)

VERSION = $$(cat VERSION)
DISTRIB_DIR = twittering-mode-$(VERSION)

README: README.markdown
	cp $< $@
release: $(DISTRIB_FILES)
	@(! [ -z "$${SF_USERNAME}" ] || (echo "Environmental variable 'SF_USERNAME', which is a username of sf.net, is required."; false))
	@echo -n "wrote NEWS file? [y or n]: "; read ans; [ "$${ans}" = "y" ]
	@echo -n "What is next version number?: " && \
	  read version && \
	  (if [ "$${version}" != "$$(cat VERSION)" ]; then \
	     mv VERSION LAST-VERSION; \
	     echo $${version} > VERSION; \
	 fi) && \
	ruby misc/vernum-updater.rb \
	  --prev-version=$$(cat LAST-VERSION) --next-version=$$(cat VERSION) \
	  doc/web/index.text
	ruby misc/vernum-updater.rb \
	  --prev-version=HEAD --next-version=$$(cat VERSION) \
	  twittering-mode.el NEWS NEWS.ja
	@([ -d $(DISTRIB_DIR) ] && rm -rf $(DISTRIB_DIR)) || true
	mkdir $(DISTRIB_DIR)
	cp -r -t $(DISTRIB_DIR)/ $(DISTRIB_FILES)
	zip -r $(DISTRIB_DIR).zip $(addprefix $(DISTRIB_DIR)/,$(DISTRIB_FILES))
	tar czvf $(DISTRIB_DIR).tar.gz $(addprefix $(DISTRIB_DIR)/,$(DISTRIB_FILES))
	rm -rf $(DISTRIB_DIR)

release-upload: release
	make upload

upload:
	(echo "cd /home/frs/project/t/tw/twmode/"; \
	 echo "-rm $(DISTRIB_DIR)/*"; \
	 echo "-rmdir $(DISTRIB_DIR)"; \
	 echo "mkdir $(DISTRIB_DIR)"; \
	 echo "cd $(DISTRIB_DIR)"; \
	 echo "put $(DISTRIB_DIR).zip"; \
	 echo "put $(DISTRIB_DIR).tar.gz") > upload.bat
	sftp -b upload.bat $${SF_USERNAME},twmode@web.sourceforge.net
	rm -f upload.bat
