public: README.edoc src/json.erl
	erl -noshell -run edoc_run files '["src/json.erl"]' \
		'[{dir, "$@"}, {overview, "README.edoc"}, {sort_functions, false}]'

%.edoc: %.md
	(echo @doc; pandoc $<) > $@

.INTERMEDIATE: README.edoc
