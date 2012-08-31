flux: src/flux.erl src/flux_cards.erl include/flux.hrl
	erlc -I include -o ebin src/*.erl

go: flux
	erl -pa ebin -run flux play

clean: 
	rm ebin/*
