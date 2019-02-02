public: README.edoc src/rfc8259.erl
	erl -noshell -run edoc_run files '["src/rfc8259.erl"]' \
		'[{dir, "$@"}, {overview, "README.edoc"}, {sort_functions, false}]'

%.edoc: %.md
	(echo @doc; pandoc $<) > $@

.INTERMEDIATE: README.edoc
