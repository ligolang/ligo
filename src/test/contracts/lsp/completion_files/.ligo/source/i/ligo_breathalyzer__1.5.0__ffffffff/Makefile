.PHONY: test test-list-ext

LIGO_CC := ligo

define test_ligo
  $(LIGO_CC) run test $(1)
endef

test: test-list-ext test-auction test-ticket-factory
	$(call test_ligo,test/test.mligo)

test-list-ext:
	$(call test_ligo,examples/simple/test_list_ext.mligo)

test-auction:
	$(call test_ligo,examples/auction/test/test_auction_sc.mligo)

test-ticket-factory:
	$(call test_ligo,examples/ticket_factory/test/test_ticket_factory.mligo)
