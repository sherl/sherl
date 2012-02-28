.PHONY: all clean

all:
	./rebar compile

clean:
	@rm -rf ebin
