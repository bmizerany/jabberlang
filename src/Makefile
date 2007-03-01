#DEBUG = -DDEBUG=true
#TEST = -DNOTEST=true

EMULATOR=beam
EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import
ERL_COMPILE_FLAGS += -I../../exmpp/include

SOURCES= \
	gen_xmpp_client.erl \
	xmpp.erl \
	xmpp_callbacks.erl \
	xmpp_echo.erl \
	xmpp_echo_behaviour.erl \
	xmpp_echo_behaviour2.erl \
	xmpp_erlang_example.erl \
	xmpp_iq.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/xmpp.hrl

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN)  $(DEBUG) $(TEST) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
